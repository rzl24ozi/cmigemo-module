ROOT = ../..
EMACS = $(ROOT)/src/emacs
CMIGEMO = /usr/local
CC      = gcc
LD      = gcc
LDFLAGS =
SO      = so
ifeq ($(SO),dll)
CFLAGS  = -std=gnu99 -ggdb3 -Wall
LDFLAGS += -static-libgcc
else
CFLAGS  = -std=gnu99 -ggdb3 -Wall -fPIC
endif
USE_LIBICONV =
LIBICONV =
ifeq ($(USE_LIBICONV),yes)
CFLAGS += -DUSE_LIBICONV
LIBICONV = -liconv
endif

all: cmigemo-module.$(SO)

%.$(SO): %.o
	$(LD) -shared $(LDFLAGS) -o $@ $< -L$(CMIGEMO)/lib -lmigemo $(LIBICONV)

%.o: %.c
	$(CC) $(CFLAGS) -I. -I$(ROOT)/src -I$(CMIGEMO)/include -c $<
