#
# Makefile for compiler project files. For use under Linux/Unix.
#
# Targets:
#
#       make libcomp.a		generate the library libcomp.a
#	make clean		delete all object files (but NOT the library
#				file) created by this Makefile
#	make veryclean		delete all object and library files created
#				by this Makefile
#
# The versions of C compiler and archiver being used are set by the macros
# CC and AR. The version of MAKE being used is set by MAKE
#
# Name of your local gcc (or other ANSI C compiler).
CC=gcc
# Flags to pass to compiler.
CFLAGS=-ansi -pedantic -I../headers

# Name of your code library maintainer (in this case "ar").
AR=ar 
# Flags to pass to "ar" to create a new library. "rv" on Sun4 and Linux.
ARFLAGS=rv

# Name of your local delete program. Usually "/bin/rm -rf".
RM=/bin/rm -rf

# Name of library of object files.
CODELIB=libcomp.a

# List of objects to be placed in the archive.
OBJS=$(CODELIB)(code.o) $(CODELIB)(line.o) \
     $(CODELIB)(scanner.o) $(CODELIB)(sets.o) \
     $(CODELIB)(strtab.o) $(CODELIB)(symbol.o) \
     $(CODELIB)(debug.o)

# Build rules follow.

$(CODELIB) : $(OBJS)
	@ echo "$(CODELIB) is now up to date"

clean:
	$(RM) *.o

veryclean:
	$(RM) $(CODELIB) *.o 
