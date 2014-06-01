#include <stdint.h>

#ifndef AF_BLUETOOTH
#define AF_BLUETOOTH      31
#endif
#define BTPROTO_RFCOMM     3

/* BD Address */
typedef struct {
  uint8_t b[6];
} __attribute__((packed)) bdaddr_t;

/* ---- RFCOMM sockets ---- */
struct sockaddr_rc {
  sa_family_t     rc_family;
  bdaddr_t        rc_bdaddr;
  uint8_t         rc_channel;
};
#define BDADDR_ANY   (&(bdaddr_t) {{0, 0, 0, 0, 0, 0}})

int ba2str(const bdaddr_t *ba, char *str);
int str2ba(const char *str, bdaddr_t *ba);
