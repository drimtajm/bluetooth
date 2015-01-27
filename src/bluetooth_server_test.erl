-module(bluetooth_server_test).
-compile([export_all]).

-include("../include/client_server_info.hrl").

socket_acceptor(Caller, Socket) ->
    case bluetooth_interface:bt_socket_accept(Socket) of
	{ok, Socket2, RemoteAddress} ->
	    io:format("Accept ok, remote address: ~p~n", [RemoteAddress]),
	    receive_loop(Socket2, <<>>);
	{error, ErrorCode} -> print_error('accept', ErrorCode)
    end,
    Caller ! {self(), done},
    ok.

receive_loop(Socket, Pending) ->
    case myreceive(Socket, Pending) of
	{ok, <<"Bye!">>, _NewPending} ->
	    io:format("Received 'Bye!' message from client~n");
	{error, ErrorCode}  ->
	    print_error('receive', ErrorCode);
	{ok, Bin, NewPending} ->
	    io:format("received ~p bytes from client. data: ~p~n",
		      [byte_size(Bin), Bin]),
	    io:format("Received: ~p~n", [binary_to_term(Bin)]),
	    receive_loop(Socket, NewPending)
    end.

print_error(Operation, ErrorCode) ->
    io:format("~p failed with error code: ~p~n", [Operation, ErrorCode]).

    
go() ->
    bluetooth_interface:set_local_name(atom_to_list(get_localhost())),
    LocalMac = proplists:get_value(get_localhost(), ?HOSTS),
    io:format("Local MAC address: ~p~n", [LocalMac]),
    {ok, Socket} = bluetooth_interface:create_rfcomm_socket(),
    {ok, SecuritySetting} = bluetooth_interface:get_bt_security_setting(Socket),
    io:format("Create ok, Socket handle: ~p, security setting: ~p~n",
	      [Socket, SecuritySetting]),
    case bluetooth_interface:bind_bt_socket_any_device(Socket, ?PORT) of
        ok -> {ok, Name} = bluetooth_interface:get_local_name(), 
	      io:format("Bind ok, device: ~p~n", [Name]),
	      case bluetooth_interface:bt_socket_listen(Socket) of
		  ok                 -> io:format("Listen ok~n"),
					continue(Socket);
		  {error, ErrorCode} -> print_error('listen', ErrorCode)
	      end;
	{error, ErrorCode} -> print_error('bind', ErrorCode)
    end,
    bluetooth_interface:close_socket(Socket).

continue(Socket) ->
    Pid = spawn_link(?MODULE, socket_acceptor, [self(), Socket]),
    receive
	{Pid, done} -> ok
    after 60000 ->
	    io:format("timeout!~n")
    end.

myreceive(Socket, Pending) ->
    case erlang:decode_packet(2, Pending, []) of
	{ok, Packet, Rest} ->
	    {ok, Packet, Rest};
	{more, _} ->
	    {ok, _Count, Data} =
		bluetooth_interface:bt_socket_receive(Socket),
	    myreceive(Socket, <<Pending/binary, Data/binary>>)
    end.
