SHELL = /bin/sh
#
.c.o :
	$(CC) -c $(CCCHK) $(CFLAGS) $*.c
#
CFLAGS  = $(SDSS_CFLAGS) -DCHECK_LEAKS -I../include

SUBDIRS = include lib src



all :	include/export.h
	@ for f in $(SUBDIRS); do \
		(cd $$f ; echo In $$f; $(MAKE) $(MFLAGS) all ); \
	done

clean :
	- /bin/rm -f *~ core
	@ for f in $(SUBDIRS); do \
		(cd $$f ; echo In $$f; $(MAKE) $(MFLAGS) clean ); \
	done

# Some versions of IDL have non-backward compatible export.h files. So use whatever is current.
include/export.h:
	@if test -z "$$IDL_DIR"; then \
		echo "IDL_DIR environment variable is not set -- it must point to the top of the IDL product directory, "; \
		echo "   e.g. /usr/local/itt/idl"; \
		exit 1; \
	else \
		echo "Linking in IDL export.h, using IDL_DIR=$$IDL_DIR"; \
		test -r "$$IDL_DIR/external/export.h" || (echo "no valid $$IDL_DIR/external/export.h"; exit 1); \
		( cd include; ln -s $$IDL_DIR/external/export.h ); \
	fi
