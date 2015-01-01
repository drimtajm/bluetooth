-module(bluetooth_server_test).
-compile([export_all]).

-define(HOSTS, {{pepparkakehus, "00:02:72:c0:64:11"},
%%                {datorbebis,    "00:02:72:C0:63:F4"},
	        {hogwarts,      "44:33:4C:1B:CC:F0"}}).
-define(PORT, 13).

get_localhost() ->
    list_to_atom([X || X <- string:to_lower(os:cmd("hostname")),
		       (((X>=$a) and (X=<$z)) orelse ((X>=$0) and (X=<$9)))]).

get_mac_addresses() ->
    Localhost = get_localhost(),
    case ?HOSTS of
	{{Localhost, LocalMac1}, {_RemoteHost1, RemoteMac1}} ->
	    {{local, LocalMac1}, {remote, RemoteMac1}};
	{{_RemoteHost2, RemoteMac2}, {Localhost, LocalMac2}} ->
	    {{local, LocalMac2}, {remote, RemoteMac2}};
	_Else -> undefined
    end.

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
%%	    Array = extract_data(Count, Bin, []),
%%	    lists:foreach(
%%	      fun (Term) -> io:format("Received: ~p~n", [Term]) end, Array),
%%	    case (lists:member("Bye!", Array)) of
%%		true  -> receive_loop(Socket);
%%		false -> ok
%%	    end
    end.

%%extract_data(Count, Bin, Result) ->
%%    Term = binary_to_term(Bin),
%%    NewBin = term_to_binary(Term),
%%    case byte_size(NewBin) of
%%	Count ->
%%	    lists:reverse([Term | Result]);
%%	LCount when LCount < Count ->
%%	    RestBinary = binary_part(Bin, Count, LCount - Count),
%%	    extract_data(Count - LCount, RestBinary, [Term | Result])
%%    end.    

print_error(Operation, ErrorCode) ->
    io:format("~p failed with error code: ~p~n", [Operation, ErrorCode]).

    
go() ->
    {{local, LocalMac}, {remote, _RemoteMac}} = get_mac_addresses(),
    {ok, Socket} = bluetooth_interface:create_rfcomm_socket(),
    io:format("Create ok, Socket handle: ~w~n", [Socket]),
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
