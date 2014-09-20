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

-export([create_rfcomm_socket/0, close_socket/1]).
-export([create_hci_socket/0]).
-export([bind_bt_socket_any_device/2, bind_bt_socket/3]).
-export([bt_socket_listen/1, bt_socket_accept/1, bt_socket_connect/3]).
-export([bt_socket_send/2, bt_socket_receive/1]).
-export([mac_address_to_string/1]).
-export([discover/0, discover/1]).
-export([get_remote_name/1, get_local_name/0, set_local_name/1]).

-define(nif_stub,
	erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

%% The following default values are recommended by Bluez.
%% It gives a discovery interval of 10.24 seconds.
-define(DEFAULT_NUM_DISCOVERY_CYCLES, 8).
-define(DEFAULT_MAX_RSP, 255).

-ifndef(test).
-on_load(on_load/0).
-else.
%% Need to export on_load/0 to suppress warning about otherwise unused function
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

create_hci_socket() ->
    create_hci_socket_nif().

-spec(close_socket(socket()) -> ok | {error, error_code()}).
%%% @doc This closes a bluetooth socket
close_socket(Socket) ->
    close_socket_nif(Socket).

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

discover() ->
    discover([]).

discover(Options) ->
    NumCycles = proplists:get_value(num_discovery_cycles, Options,
				    ?DEFAULT_NUM_DISCOVERY_CYCLES),
    MaxRsp = proplists:get_value(max_rsp, Options, ?DEFAULT_MAX_RSP),
    discover_potential_peers_nif(NumCycles, MaxRsp).

get_remote_name(MacAddress) when is_tuple(MacAddress) ->
    get_remote_name(mac_address_to_string(MacAddress));
get_remote_name(MacAddress) ->
    {ok, Socket} = create_hci_socket(),
    {ok, Name} = get_remote_name_nif(Socket, MacAddress),
    close_socket(Socket),
    {ok, Name}.

get_local_name() ->
    {ok, Socket} = create_hci_socket(),
    Result = get_local_name_nif(Socket),
    close_socket(Socket),
    Result.

set_local_name(Name) when is_list(Name) ->
    {ok, Socket} = create_hci_socket(),
    Result = set_local_name_nif(Socket, Name),
    close_socket(Socket),
    Result.


%%%%%%%%%%%%%%%
%% Define stubs for NIF functions

create_rfcomm_socket_nif()                        -> ?nif_stub.
create_hci_socket_nif()                           -> ?nif_stub.
discover_potential_peers_nif(_NumCycles, _MaxRsp) -> ?nif_stub.
get_remote_name_nif(_Socket, _MacAddress)         -> ?nif_stub.
get_local_name_nif(_Socket)                       -> ?nif_stub.
set_local_name_nif(_Socket, _Name)                -> ?nif_stub.
bind_bt_socket_any_nif(_Socket, _Channel)         -> ?nif_stub.
bind_bt_socket_nif(_Socket, _Channel, _Mac)       -> ?nif_stub.
bt_socket_listen_nif(_Socket)                     -> ?nif_stub.
bt_socket_accept_nif(_Socket)                     -> ?nif_stub.
bt_socket_connect_nif(_Socket, _Port, _RemoteMac) -> ?nif_stub.
bt_socket_send_nif(_Socket, _Data)                -> ?nif_stub.
bt_socket_receive_nif(_Socket)                    -> ?nif_stub.
close_socket_nif(_Socket)                         -> ?nif_stub.

%%
%%%%%%%%%%%%%%%
