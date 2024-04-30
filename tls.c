#include <stdint.h>
#include <tls.h>

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#include "ovm.h"

#define PTR(t) onum((intptr_t)t, 0)
#define cptr(v) ((void*)(intptr_t)cnum(v))
#define car(l) G(l, 1)
#define cdr(l) G(l, 2)

#define getctx(a) (is_type(a,TPORT)?(void*)ptrmap[cnum(a)]:cptr(a))

uintptr_t ptrmap[4096];

void
list2data(word l, unsigned char *u, int N)
{
  int i;
  for (i = 0; i < N; ++i) {
    u[i] = cnum(car(l));
    l = cdr(l);
  }
}

word
prim_custom(int op, word a, word b, word c)
{
  switch (op) {
  case 1000:
    tls_init();
    return ITRUE;
    break;
  case 1001: { /* tls-conect fd server-name → ptr | #f */
    struct tls_config *tc = tls_config_new();
    struct tls *t = tls_client();
    tls_configure(t, tc);

    int fd = cnum(a);

    if (tls_connect_fds(t, fd, fd, cstr(b)) < 0) {
      fprintf(stderr, "tls error: %s\n", tls_error(t));
      return IFALSE;
    }

    ptrmap[fd] = (uintptr_t)t;

    tls_handshake(t);
    return PTR(t);
  }
  case 1002: { /* write ptr l → #t */
    uint N = llen((word*)b);
    uint8_t *d = malloc(N);
    list2data(b, d, N);
    uint n = 0;
    do {
      int temp = tls_write(getctx(a), d, N);
      if (temp > 0) n += temp;
    } while (n < N);
    printf("written %d / %d\n", n, N);
    free(d);
    return ITRUE;
  }
  case 1003: { /* read ptr|fd n → str */
    uint N = cnum(b);
    int n;
    unsigned char *buf = malloc(N+1);

    n = tls_read(getctx(a), buf, N);
    if (n <= 0) {
      free(buf);
      return IFALSE;
    }

    word bv = mkbvec(buf, n);
    free(buf);
    return bv;

    /* buf[n] = 0; */
    /* word l = INULL; */
    /* for (n = n-1; n >= 0; --n) { */
    /*   word v = onum(buf[n], 1); */
    /*   l = cons(v, l); */
    /* } */

    /* free(buf); */
    /* return l; */
  }
  case 1004: { /* close ptr */
    tls_close(getctx(a));
  }
  }
  return IFALSE;
}
