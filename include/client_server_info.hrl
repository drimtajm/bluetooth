-ifndef(CLIENT_SERVER_INFO_HRL).
-define(CLIENT_SERVER_INFO_HRL, true).

-define(HOSTS, [{pepparkakehus, "00:02:72:c0:64:11"},
                {datorbebis,    "00:1A:7D:DA:71:02"},
	        {hogwarts,      "44:33:4C:1B:CC:F0"}]).
-define(DEFAULTSERVER, pepparkakehus).
-define(PORT, 13).

get_localhost() ->
    list_to_atom([X || X <- string:to_lower(os:cmd("hostname")),
		       (((X>=$a) and (X=<$z)) orelse ((X>=$0) and (X=<$9)))]).

-endif.
