-module(bluetooth_client_test).
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
%%    bluetooth_interface:set_local_name(atom_to_list(get_localhost())),
    {{local, _LocalMac}, {remote, RemoteMac}} = get_mac_addresses(),
    {ok, Socket} = bluetooth_interface:create_rfcomm_socket(),
    {ok, SecuritySetting} = bluetooth_interface:get_bt_security_setting(Socket),
    io:format("Create ok, Socket handle: ~p, security setting: ~p~n",
	      [Socket, SecuritySetting]),
    continue(Socket, RemoteMac),
%%    case bluetooth_interface:bind_bt_socket(Socket, ?PORT, LocalMac) of
%%        ok                 -> continue(Socket, RemoteMac);
%%        {error, ErrorCode} -> print_error('bind', ErrorCode)
%%    end,
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
