%%%-------------------------------------------------------------------
%%% @author  drimtajm@github
%%% @copyright (C) 2013, Angela Johansson
%%% @doc
%%% An Erlang NIF for bluetooth communication
%%%
%%% This interface uses the Bluez library to access the bluetooth stack
%%% in Linux.
%%% A precondition to using this software is that Bluez is installed.
%%%
%%% This bluetooth interface is free software:  you can redistribute it
%%% and/or modify it under the terms of the GNU Lesser General Public License
%%% as published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% The bluetooth interface is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with this software.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%% @end
%%% Created : 20 Oct 2013 by drimtajm
%%%-------------------------------------------------------------------
-module(bluetooth_interface).

-export([create_rfcomm_socket/0, close_bt_socket/1]).
-export([bind_bt_socket_any_device/2, bind_bt_socket/3]).
-export([bt_socket_listen/1, bt_socket_accept/1, bt_socket_connect/3]).
-export([bt_socket_send/2, bt_socket_receive/1]).
-export([mac_address_to_string/1]).

-define(nif_stub,
        erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

-ifndef(test).
-on_load(on_load/0).
-else.
-export([on_load/0]).
-endif.

-type error_code()  :: atom() | {other, integer()}.
-type socket()      :: pos_integer().
-type bt_channel()  :: 1..30.
%%-type mac_address() :: {byte(), byte(), byte(), byte(), byte(), byte()}.

on_load() ->
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, bad_name} ->
		      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    Filename = filename:join(PrivDir, ?MODULE),
    ok = erlang:load_nif(Filename, 0).

-spec(create_rfcomm_socket() -> {ok, socket()} | {error, error_code()}).
%%% @doc This creates an RFCOMM bluetooth socket and returns the socket handle
create_rfcomm_socket() ->
    create_rfcomm_socket_nif().

-spec(close_bt_socket(socket()) -> ok | {error, error_code()}).
%%% @doc This closes a bluetooth socket
close_bt_socket(Socket) ->
    close_bt_socket_nif(Socket).

-spec(bind_bt_socket_any_device(socket(), bt_channel()) -> 
	     ok | {error, error_code()}).
%%% @doc This binds a bluetooth socket to an RFCOMM port,
%%%      using the first available bluetooth device
bind_bt_socket_any_device(Socket, Channel) ->
    bind_bt_socket_any_nif(Socket, Channel).

-spec(bind_bt_socket(socket(), bt_channel(), string()) -> 
	     ok | {error, error_code()}).
%%% @doc This binds a bluetooth socket to an RFCOMM port,
%%%      using the device with the specified mac address
bind_bt_socket(Socket, Channel, MacAddressString) ->
%%    MacAddressString = mac_address_to_string(MacAddress),
    bind_bt_socket_nif(Socket, Channel, MacAddressString).


mac_address_to_string(MacAddress) ->
    {A, B, C, D, E, F} = MacAddress,
    lists:flatten(
      io_lib:format("~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B",
		    [A, B, C, D, E, F])).

bt_socket_listen(Socket) ->
    bt_socket_listen_nif(Socket).

bt_socket_accept(Socket) ->
    bt_socket_accept_nif(Socket).

bt_socket_connect(Socket, Port, MacAddressString) ->
    %%MacAddressString = mac_address_to_string(RemoteMac),
    bt_socket_connect_nif(Socket, Port, MacAddressString).

bt_socket_send(Socket, Data) ->
    bt_socket_send_nif(Socket, Data).

bt_socket_receive(Socket) ->
    bt_socket_receive_nif(Socket).

%%%%%%%%%%%%%%%
%% Define stubs for NIF functions

create_rfcomm_socket_nif()                        -> ?nif_stub.
bind_bt_socket_any_nif(_Socket, _Channel)         -> ?nif_stub.
bind_bt_socket_nif(_Socket, _Channel, _Mac)       -> ?nif_stub.
bt_socket_listen_nif(_Socket)                     -> ?nif_stub.
bt_socket_accept_nif(_Socket)                     -> ?nif_stub.
bt_socket_connect_nif(_Socket, _Port, _RemoteMac) -> ?nif_stub.
bt_socket_send_nif(_Socket, _Data)                -> ?nif_stub.
bt_socket_receive_nif(_Socket)                    -> ?nif_stub.
close_bt_socket_nif(_Socket)                      -> ?nif_stub.

%%
%%%%%%%%%%%%%%%
