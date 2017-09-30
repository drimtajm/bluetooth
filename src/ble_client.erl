-module(ble_client).
-compile([export_all]).

get_remote_mac() ->
   "F7:63:C1:32:22:F6".

get_uart_rx_handle() ->
   16#0021.

list_to_term(List) ->
    {ok,Tokens,_EndLine} = erl_scan:string(List),
    case  erl_parse:parse_exprs(Tokens) of
        {ok,AbsForm} ->
            {value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
            Value;
	_ ->
	    invalid_term
    end.

read_loop(_Socket, 0, _Delay) ->
    ok;
read_loop(Socket, Count, Delay) ->
    case bluetooth_interface:read_ble_value(Socket, get_uart_rx_handle()) of
	        {error, ErrorCode}  ->
	            print_error('receive', ErrorCode);
	        {ok, ByteCount, Bin}    ->
%%	            io:format("received ~p bytes from client. data: ~p~n",
%%		              [ByteCount, Bin])%%,
	            List = binary_to_list(Bin),
		    Term = case lists:last(List) of
		    	       $.    -> list_to_term(List);
			       _Else -> list_to_term(lists:append(List, "."))
			   end,
	            case Term of
		       invalid_term ->
		           ok;
		       X when element(2, X) > 12; element(2, X) < 8 ->
		           io:format("Gotcha! ~p~n", [X]);
		       %%Y when is_tuple(Y) ->
		       %%    io:format("~p~n", [Y]);
		       _ ->
		           ok
	            end
%%	            io:format("term is: ~p~n", [Term])
    end,
    %%timer:sleep(Delay),
    read_loop(Socket, Count-1, Delay).

socket_connector(Caller, Socket, RemoteMac) ->
    case bluetooth_interface:connect_ble_socket(Socket, RemoteMac) of
        ok ->
%%	    Data = erlang:term_to_binary({self(), greetings}),
%%	    case bluetooth_interface:bt_socket_send(Socket, Data) of
%%                  ok ->
%%                      io:format("message sent to server. data: ~p~n",
%%				[binary_to_term(Data)]);
            read_loop(Socket, 150, 200);
        {error, ErrorCode} -> print_error('connect', ErrorCode)
    end,
    Caller ! {self(), done},
    ok.

print_error(Operation, ErrorCode) ->
    io:format("~p failed with error code: ~p~n", [Operation, ErrorCode]).

go() ->
    {ok, Socket} = bluetooth_interface:create_ble_socket(),
    io:format("Got socket: ~p~n", [Socket]),
    ok = bluetooth_interface:bind_ble_socket_any_device(Socket),
    io:format("Socket bound~n"),
    continue(Socket, get_remote_mac()),
    bluetooth_interface:close_socket(Socket).

continue(Socket, RemoteMac) ->
    Pid = spawn(?MODULE, socket_connector, [self(), Socket, RemoteMac]),
    receive
        {Pid, done} -> ok
    after 60000 ->
            io:format("timeout!~n")
    end.
