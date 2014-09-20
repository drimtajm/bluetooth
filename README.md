bluetooth
=========

An embryo for an Erlang bluetooth application.<br><br>

Prerequisites
-------------
1. Install Bluez
2. To enable bluetooth adapter and put it in scan mode, do:

   sudo hciconfig hci0 up
   sudo hciconfig hci0 piscan

Example run - handling names
----------------------------
	1> bluetooth_interface:get_local_name().
	{ok,"Azure:Dec  3 2010,00:24:03"}

	2> bluetooth_interface:set_local_name("pepparkakehus").
	ok
	3> bluetooth_interface:get_local_name().
	{ok,"pepparkakehus"}

	4> {ok, Addrs} = bluetooth_interface:discover().
	{ok,["00:02:72:C0:63:F4"]}
	5> bluetooth_interface:get_remote_name(hd(Addrs)).
	{ok,"datorbebis"}
