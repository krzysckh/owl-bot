OWL_SOURCE_PATH=/home/kpm/nmojeprogramy/owl
CFLAGS=-ggdb -DPRIM_CUSTOM -I$(OWL_SOURCE_PATH)/c -Wall -Wextra #-fsanitize=address

run:
	ol -i lib/robusta/ -C $(OWL_SOURCE_PATH)/c/ovm.c -x c ws-client.scm \
		| clang $(CFLAGS) tls.c -x c - -ltls -o ws-client
	./ws-client
