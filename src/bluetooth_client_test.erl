-module(bluetooth_client_test).
-compile([export_all]).

-include("../include/client_server_info.hrl").

socket_connector(Caller, Socket, RemoteMac) ->
    case bluetooth_interface:bt_socket_connect(Socket, ?PORT, RemoteMac) of
        ok -> socket_is_connected(Socket, Caller);
        {error, ErrorCode} -> print_error('connect', ErrorCode),
			      cleanup(Caller)
    end.

socket_is_connected(Socket, Caller) ->
    Data = erlang:term_to_binary({self(), greetings}),
    case send(Socket, Data) of
	ok ->
	    io:format("message sent to server. data: ~p~n",
		      [binary_to_term(Data)]),
	    socket_sending_successful(Socket, Caller);
	{error, ErrorCode} -> print_error('send', ErrorCode),
			      cleanup(Caller)
    end.

socket_sending_successful(Socket, Caller) ->
    send(Socket, term_to_binary("hej")),
    send(Socket, term_to_binary("och")),
    send(Socket, term_to_binary("ho")),
    Data2 = term_to_binary("Bye!"),
    case send(Socket, Data2) of
	ok ->
	    io:format("message sent to server. data: ~p~n", [Data2]);
	{error, ErrorCode2} -> print_error('send', ErrorCode2)
    end,
    %% delay for 10 seconds
    timer:sleep(10000),
    cleanup(Caller).

cleanup(Caller) ->
    Caller ! {self(), done},
    ok.

print_error(Operation, ErrorCode) ->
    io:format("~p failed with error code: ~p~n", [Operation, ErrorCode]).

go() ->
    go(?DEFAULTSERVER).

go(Server) ->
    bluetooth_interface:set_local_name(atom_to_list(get_localhost())),
    RemoteMac = proplists:get_value(RemoteHost, ?HOSTS),
    if (RemoteMac == undefined) ->           %% verify MAC address is found
	    error(unknown_host);
       true -> ok
    end,
    io:format("Remote MAC address: ~p~n", [RemoteMac]),    
    {ok, Socket} = bluetooth_interface:create_rfcomm_socket(),
    {ok, SecuritySetting} = bluetooth_interface:get_bt_security_setting(Socket),
    io:format("Create ok, Socket handle: ~p, security setting: ~p~n",
	      [Socket, SecuritySetting]),
    continue(Socket, RemoteMac),
    bluetooth_interface:close_socket(Socket).

continue(Socket, RemoteMac) ->
    Pid = spawn(?MODULE, socket_connector, [self(), Socket, RemoteMac]),
    receive
        {Pid, done} -> ok
    after 60000 ->
            io:format("timeout!~n")
    end.

send(Socket, Data) ->
    bluetooth_interface:bt_socket_send(Socket,
				       [<<(byte_size(Data)):16>>, Data]).
