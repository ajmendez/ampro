SHELL = /bin/sh
#
# .c.o :
# 	$(CC) -c $(CCCHK) $(CFLAGS) $(X_CFLAGS) $*.c
#
INC = ../../include
CFLAGS = $(SDSS_CFLAGS) -DCHECK_LEAKS -I$(INC)

LIB = $(AMPRO_DIR)/lib

RECIPES = nrecipes.o nrutil.o
OBJECTS = \
	arrmedian.o \
	$(RECIPES)

all : $(LIB)/libtest.$(SO_EXT)


$(LIB)/libtest.$(SO_EXT): $(OBJECTS)
	$(LD) $(X_LD_FLAGS) -o $(LIB)/libtest.$(SO_EXT) $(OBJECTS)

#
# Install things in their proper places in $(AMPRO_DIR)
#
install : 
	-@ cp *.c $(AMPRO_DIR)/src/math
	-@ cp *.h $(AMPRO_DIR)/src/math
	-@ cp Makefile $(AMPRO_DIR)/src/math

clean :
	- /bin/rm -f *~ core *.o so_locations *.dylib
