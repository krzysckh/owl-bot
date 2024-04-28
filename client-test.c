/* -*- mode: c; compile-command: "cc -Wall client-test.c -o client -ltls && ./client" -*- */

#include <stdio.h>
#include <unistd.h>
#include <tls.h>
#include <string.h>
#include <errno.h>

int
main(void)
{
  tls_init();
  struct tls_config *tc = tls_config_new();
  struct tls *t = tls_client();
  tls_configure(t, tc);
  tls_connect(t, "bije.zone", "443");

  char *s = "GET / HTTP/1.1\r\nHost: bije.zone\r\n\r\n";
  tls_write(t, s, strlen(s));

  char buf[4096] = {0};
  int n;
  while ((n = tls_read(t, buf, 4095))) {
    if (n > 0) {
      buf[n] = 0;
      printf("%s", buf);
    }
  }

  tls_close(t);
  return 0;
}
