-module(bluetooth_client_test).
-compile([export_all]).

-define(HOSTS, {{gizmo,     "7C:C7:08:61:19:56"},
                {kalleanka, "7C:C7:08:DD:8F:19"}}).
-define(PORT, 13).

get_localhost() ->
    list_to_atom([X || X <- string:to_lower(os:cmd("hostname")),
		       (((X>=$a) and (X=<$z)) orelse ((X>=$0) and (X=<$9)))]).

get_mac_addresses() ->
    Localhost = get_localhost(),
    %%proplists:get_value(Localhost, ?HOSTS)
    case ?HOSTS of
        {{Localhost, LocalMac1}, {_RemoteHost1, RemoteMac1}} ->
	    {{local, LocalMac1}, {remote, RemoteMac1}};
	{{_RemoteHost2, RemoteMac2}, {Localhost, LocalMac2}} ->
            {{local, LocalMac2}, {remote, RemoteMac2}};
        _Else -> undefined
    end.

socket_connector(Caller, Socket, RemoteMac) ->
    case bluetooth_interface:bt_socket_connect(Socket, ?PORT, RemoteMac) of
        ok ->
	    Data = erlang:term_to_binary({self(), greetings}),
	    case bluetooth_interface:bt_socket_send(Socket, Data) of
                  ok ->
                      io:format("message sent to server. data: ~p~n",
				[binary_to_term(Data)]);
		{error, ErrorCode} -> print_error('send', ErrorCode)
              end;
        {error, ErrorCode} -> print_error('connect', ErrorCode)
    end,
    bluetooth_interface:bt_socket_send(Socket, term_to_binary("hej")),
    bluetooth_interface:bt_socket_send(Socket, term_to_binary("och")),
    bluetooth_interface:bt_socket_send(Socket, term_to_binary("ho")),
    Data2 = term_to_binary("Bye!"),
    case bluetooth_interface:bt_socket_send(Socket, Data2) of
	ok ->
	    io:format("message sent to server. data: ~p~n", [Data2]);
	{error, ErrorCode2} -> print_error('send', ErrorCode2)
    end,
    %% delay for 10 seconds
    timer:sleep(10000),
    Caller ! {self(), done},
    ok.

print_error(Operation, ErrorCode) ->
    io:format("~p failed with error code: ~p~n", [Operation, ErrorCode]).

go() ->
    {{local, _LocalMac}, {remote, RemoteMac}} = get_mac_addresses(),
    {ok, Socket} = bluetooth_interface:create_rfcomm_socket(),
    continue(Socket, RemoteMac),
%%    case bluetooth_interface:bind_bt_socket(Socket, ?PORT, LocalMac) of
%%        ok                 -> continue(Socket, RemoteMac);
%%        {error, ErrorCode} -> print_error('bind', ErrorCode)
%%    end,
    bluetooth_interface:close_bt_socket(Socket).

continue(Socket, RemoteMac) ->
    Pid = spawn(?MODULE, socket_connector, [self(), Socket, RemoteMac]),
    receive
        {Pid, done} -> ok
    after 60000 ->
            io:format("timeout!~n")
    end.
