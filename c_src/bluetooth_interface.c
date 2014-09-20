// An Erlang NIF which interfaces the bluez library (bluetooth stack for Linux).
// Copyright (C) 2013 Angela Johansson
//
// This file is part of free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This bluetooth interface is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this code.  If not, see <http://www.gnu.org/licenses/>.

#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <sys/socket.h>
#include <string.h>

#ifdef __ARM_EABI__
// ARM architecture, we're probably on a Raspberry Pi. Include "real" Bluez headers
#include <bluetooth/bluetooth.h>
#include <bluetooth/rfcomm.h>
#include <bluetooth/hci.h>
#include <bluetooth/hci_lib.h>
#else
// Not on ARM, probably compiling on a PC. Include stub headers
#include "bluetooth-stub.h"
#endif

#include "erl_nif.h"

#define DEBUG 0

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  atom_ok    = enif_make_atom(env, "ok");
  atom_error = enif_make_atom(env, "error");
  return 0;
}

//----------------------------------------------
// Internal functions
//----------------------------------------------

static ERL_NIF_TERM errno2atom(ErlNifEnv *env, const int error_code) {
  switch (error_code) {
  case EACCES:       return enif_make_atom(env, "eacces");
  case EINVAL:       return enif_make_atom(env, "einval");
  case ENOENT:       return enif_make_atom(env, "enoent");
  case EBADF:        return enif_make_atom(env, "ebadf");
  case EBADFD:       return enif_make_atom(env, "ebadfd");
  case EADDRINUSE:   return enif_make_atom(env, "eaddrinuse");
  case ECONNREFUSED: return enif_make_atom(env, "econnrefused");
  case ECONNRESET:   return enif_make_atom(env, "econnreset");
  case ENOTCONN:     return enif_make_atom(env, "enotconn");
  case EHOSTUNREACH: return enif_make_atom(env, "ehostunreach");
  case EHOSTDOWN:    return enif_make_atom(env, "ehostdown");
    // TODO: implement other error codes
  default:           return enif_make_tuple(env, 2,
					    enif_make_atom(env, "other"),
					    enif_make_int(env, error_code));
  }
}

static ERL_NIF_TERM make_error(ErlNifEnv *env, const int error_code) {
  return enif_make_tuple(env, 2, atom_error, errno2atom(env, error_code));
}

static ERL_NIF_TERM create_rfcomm_socket_nif(ErlNifEnv *env, int argc,
					     const ERL_NIF_TERM argv[]) {
  int optval;
  int result = socket(AF_BLUETOOTH, SOCK_STREAM, BTPROTO_RFCOMM);
  if (result < 0) {
    result = errno;
    return make_error(env, result);
  }
  optval = 1;
  setsockopt(result, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval));
  return enif_make_tuple(env, 2, atom_ok, enif_make_int(env, result));
}

static ERL_NIF_TERM bdaddr2erlnifterm(ErlNifEnv *env, bdaddr_t *bdaddr) {
  return enif_make_tuple(env, 6,
			 enif_make_int(env, bdaddr->b[0]),
			 enif_make_int(env, bdaddr->b[1]),
			 enif_make_int(env, bdaddr->b[2]),
			 enif_make_int(env, bdaddr->b[3]),
			 enif_make_int(env, bdaddr->b[4]),
			 enif_make_int(env, bdaddr->b[5]));
}

//static bdaddr_t erlnifterm2bdaddr(ErlNifEnv *env,
//				  const ERL_NIF_TERM erlbdaddr) {
//  int arity, i, temp;
//  const ERL_NIF_TERM* array;
//  bdaddr_t bdaddr;
//  if (!enif_get_tuple(env, erlbdaddr, &arity, &array) || (arity != 6)) {
//    exit(1);
//  }
//  for (i; i < arity; ++i) {
//    if (!enif_get_int(env, array[i], &temp)) {
//      exit(1);
//    }
//    bdaddr.b[i] = (uint8_t)temp;
//  }
//  return bdaddr;
//}

//----------------------------------------------
// End: Internal functions
//----------------------------------------------

static ERL_NIF_TERM create_hci_socket_nif(ErlNifEnv *env, int argc,
					  const ERL_NIF_TERM argv[]) {
  int device_id, result;
  device_id = hci_get_route(NULL);
  if (device_id < 0) {
    result = errno;
    return make_error(env, result);
  }
  result = hci_open_dev(device_id);
  if (result < 0) {
    result = errno;
    return make_error(env, result);
  }
  return enif_make_tuple(env, 2, atom_ok, enif_make_int(env, result));
}

static ERL_NIF_TERM discover_potential_peers_nif(ErlNifEnv *env, int argc,
						 const ERL_NIF_TERM argv[]) {
  int device_id, result, num_cycles, max_rsp, i;
  int flags = IREQ_CACHE_FLUSH;
  if (!enif_get_int(env, argv[0], &num_cycles) || (num_cycles <= 0)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[1], &max_rsp) || (max_rsp <= 0)) {
    return enif_make_badarg(env);
  }
  device_id = hci_get_route(NULL);
  if (device_id < 0) {
    result = errno;
    return make_error(env, result);
  }
  inquiry_info *info = (inquiry_info*)malloc(max_rsp * sizeof(inquiry_info));
  result = hci_inquiry(device_id, num_cycles, max_rsp, NULL, &info, flags);
  if (result < 0) {
    result = errno;
    return make_error(env, result);
  }
  ERL_NIF_TERM address_array[result];
  char addr[19] = { 0 };
  for (i=0; i < result; ++i) {
    ba2str(&(info+i)->bdaddr, addr);
    address_array[i] = enif_make_string(env, addr, ERL_NIF_LATIN1);
      //bdaddr2erlnifterm(env, &(info+i)->bdaddr);
  }
  free(info);
  return enif_make_tuple2(env, atom_ok,
			  enif_make_list_from_array(env, address_array, result));
}

static ERL_NIF_TERM get_remote_name_nif(ErlNifEnv *env, int argc,
					const ERL_NIF_TERM argv[]) {
  int sock, result;
  char mac_address[18], name[248];
  bdaddr_t bdaddr;
  if (!enif_get_int(env, argv[0], &sock) || (sock < 0)) {
    return enif_make_badarg(env);
  }
  if (enif_get_string(env, argv[1], mac_address, 18, ERL_NIF_LATIN1) <= 0) {
    return enif_make_badarg(env);
  }
  str2ba(mac_address, &bdaddr);
  result = hci_read_remote_name(sock, &bdaddr, sizeof(name), name, 0);
  if (result < 0) {
    return enif_make_tuple2(env, atom_ok, enif_make_atom(env, "unknown"));
  }
  return enif_make_tuple2(env, atom_ok,
			  enif_make_string(env, name, ERL_NIF_LATIN1));
}

static ERL_NIF_TERM get_local_name_nif(ErlNifEnv *env, int argc,
				       const ERL_NIF_TERM argv[]) {
  int sock, result;
  char name[248];
  if (!enif_get_int(env, argv[0], &sock) || (sock < 0)) {
    return enif_make_badarg(env);
  }
  result = hci_read_local_name(sock, sizeof(name), name, 0);
  if (result < 0) {
    return enif_make_tuple2(env, atom_ok, enif_make_atom(env, "unknown"));
  }
  return enif_make_tuple2(env, atom_ok,
			  enif_make_string(env, name, ERL_NIF_LATIN1));
}

static ERL_NIF_TERM set_local_name_nif(ErlNifEnv *env, int argc,
				       const ERL_NIF_TERM argv[]) {
  int sock, result;
  char name[248];
  if (!enif_get_int(env, argv[0], &sock) || (sock < 0)) {
    return enif_make_badarg(env);
  }
  if (enif_get_string(env, argv[1], name, 248, ERL_NIF_LATIN1) <= 0) {
    return enif_make_badarg(env);
  }
  result = hci_write_local_name(sock, name, 0);
  if (result < 0) {
    result = errno;
    return make_error(env, result);
  }
  return atom_ok;
}

static ERL_NIF_TERM bind_socket_nif(ErlNifEnv *env, int argc,
				    const ERL_NIF_TERM argv[]) {
  int sock, port, result;
  struct sockaddr_rc local_address = { 0 };
  char mac_address[18];
  if (!enif_get_int(env, argv[0], &sock) || (sock < 0)) {
    return enif_make_badarg(env);
  }
//  char *device = "hci0"; // 4 bytes long, so 4, below:
//  result = setsockopt(sock, SOL_SOCKET, SO_BINDTODEVICE, device, 4);
  if (!enif_get_int(env, argv[1], &port) || (port < 1) || (port > 30)) {
    return enif_make_badarg(env);
  }
  if (enif_get_string(env, argv[2], mac_address, 18, ERL_NIF_LATIN1) <= 0) {
    return enif_make_badarg(env);
  }
  local_address.rc_family = AF_BLUETOOTH;
  str2ba(mac_address, &local_address.rc_bdaddr);
  local_address.rc_channel = (uint8_t)port;
  result =
    bind(sock, (struct sockaddr *)&local_address, sizeof(local_address));
  if (result < 0) {
    result = errno;
    return make_error(env, result);
  }
  return atom_ok;
}

static ERL_NIF_TERM bind_socket_any_nif(ErlNifEnv *env, int argc,
					const ERL_NIF_TERM argv[]) {
  int sock, port, result;
  struct sockaddr_rc local_address = { 0 };
  if (!enif_get_int(env, argv[0], &sock) || (sock < 0)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[1], &port) || (port < 1) || (port > 30)) {
    return enif_make_badarg(env);
  }
  local_address.rc_family = AF_BLUETOOTH;
  local_address.rc_bdaddr = *BDADDR_ANY;
  local_address.rc_channel = (uint8_t)port;
  result =
    bind(sock, (struct sockaddr *)&local_address, sizeof(local_address));
  if (result < 0) {
    result = errno;
    return make_error(env, result);
  }
  return atom_ok;
}

static ERL_NIF_TERM socket_listen_nif(ErlNifEnv *env, int argc,
				      const ERL_NIF_TERM argv[]) {
  int sock, result;
  if (!enif_get_int(env, argv[0], &sock) || (sock < 0)) {
    return enif_make_badarg(env);
  }
  result = listen(sock, 1);
  if (result < 0) {
    result = errno;
    return make_error(env, result);
  }
  return atom_ok;
}

static ERL_NIF_TERM socket_accept_nif(ErlNifEnv *env, int argc,
				      const ERL_NIF_TERM argv[]) {
  int sock, result;
  struct sockaddr_rc remote_address = { 0 };
  socklen_t socket_length = sizeof(remote_address);
  char buffer[18] = { 0 };
  if (!enif_get_int(env, argv[0], &sock) || (sock < 0)) {
    return enif_make_badarg(env);
  }
  result = accept(sock, (struct sockaddr *)&remote_address, &socket_length);
  if (result < 0) {
    result = errno;
    return make_error(env, result);
  }
  ba2str(&remote_address.rc_bdaddr, buffer);
  return enif_make_tuple(env, 3, atom_ok, enif_make_int(env, result),
			 enif_make_string(env, buffer, ERL_NIF_LATIN1));
}

static ERL_NIF_TERM socket_connect_nif(ErlNifEnv *env, int argc,
				       const ERL_NIF_TERM argv[]) {
  int sock, result, port;
  struct sockaddr_rc remote_address = { 0 };
  char mac_address[18];
  if (!enif_get_int(env, argv[0], &sock) || (sock < 0)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[1], &port) || (port < 1) || (port > 30)) {
    return enif_make_badarg(env);
  }
  if (enif_get_string(env, argv[2], mac_address, 18, ERL_NIF_LATIN1) <= 0) {
    return enif_make_badarg(env);
  }
  remote_address.rc_family = AF_BLUETOOTH;
  str2ba(mac_address, &remote_address.rc_bdaddr);
  remote_address.rc_channel = (uint8_t)port;
  result =
    connect(sock, (struct sockaddr *)&remote_address, sizeof(remote_address));
  if (result < 0) {
    result = errno;
    return make_error(env, result);
  }
  return atom_ok;
}

static ERL_NIF_TERM socket_send_nif(ErlNifEnv *env, int argc,
				    const ERL_NIF_TERM argv[]) {
  int sock, result;
  if (!enif_get_int(env, argv[0], &sock) || (sock < 0)) {
    return enif_make_badarg(env);
  }
  if (enif_is_binary(env, argv[1])) {
    ErlNifBinary p;
    enif_inspect_binary(env, argv[1], &p);
    #if DEBUG
    int i;
    for (i = 0; i < p.size; ++i) {
      printf("%d ", p.data[i]);
    }
    printf("\n");
    #endif
    result = send(sock, p.data, p.size, 0);
   } else {
    return enif_make_badarg(env);
  }
  if (result < 0) {
    result = errno;
    return make_error(env, result);
  }
  return atom_ok;
}

static ERL_NIF_TERM socket_receive_nif(ErlNifEnv *env, int argc,
				       const ERL_NIF_TERM argv[]) {
  int sock;
  ssize_t result;
  unsigned char buffer[1024] = { 0 };
  ErlNifBinary bin;
  if (!enif_get_int(env, argv[0], &sock) || (sock < 0)) {
    return enif_make_badarg(env);
  }
  result = recv(sock, buffer, sizeof(buffer), 0);
  if (result <= 0) {
    result = errno;
    return make_error(env, result);
  }
  if (!enif_alloc_binary(result, &bin)) {
    return enif_make_tuple(env, 2, atom_error,
			   enif_make_atom(env, "allocation_failed"));
  }
  memcpy(bin.data, buffer, result);
  return enif_make_tuple(env, 3, atom_ok, enif_make_int(env, result),
			 enif_make_binary(env, &bin));
}

static ERL_NIF_TERM close_socket_nif(ErlNifEnv *env, int argc,
				     const ERL_NIF_TERM argv[]) {
  int sock, result;
  if (!enif_get_int(env, argv[0], &sock) || (sock < 0)) {
    return enif_make_badarg(env);
  }
  result = close(sock);
  if (result < 0) {
    result = errno;
    return make_error(env, result);
  }
  return atom_ok;
}



static ErlNifFunc nif_funcs[] =
  {
    {"create_rfcomm_socket_nif",     0, create_rfcomm_socket_nif},
    {"create_hci_socket_nif",        0, create_hci_socket_nif},
    {"discover_potential_peers_nif", 2, discover_potential_peers_nif},
    {"get_remote_name_nif",          2, get_remote_name_nif},
    {"get_local_name_nif",           1, get_local_name_nif},
    {"set_local_name_nif",           2, set_local_name_nif},
    {"bind_bt_socket_any_nif",       2, bind_socket_any_nif},
    {"bind_bt_socket_nif",           3, bind_socket_nif},
    {"bt_socket_listen_nif",         1, socket_listen_nif},
    {"bt_socket_accept_nif",         1, socket_accept_nif},
    {"bt_socket_connect_nif",        3, socket_connect_nif},
    {"bt_socket_receive_nif",        1, socket_receive_nif},
    {"bt_socket_send_nif",           2, socket_send_nif},
    {"close_socket_nif",             1, close_socket_nif}
  };

ERL_NIF_INIT(bluetooth_interface, nif_funcs, load, NULL, NULL, NULL)

