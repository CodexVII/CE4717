#
# Makefile for compiler project files. For use under Linux/Unix.
#
# Targets:
#
#       make libcomp.a		generate the library libcomp.a
#	make expression	        generate expression compiler.
#	make opprec	        generate operator-precedence expression compiler.
#       make sim                build the machine code simulator program
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
CFLAGS=-ansi -pedantic -Wall -Iheaders

# Name of your code library maintainer (in this case "ar").
AR=ar 
# Flags to pass to "ar" to create a new library. "rv" on Sun4 and Linux.
ARFLAGS=rv

# Name of your local make utility. Needed for recursive invocation of
# make on the "simulator" directory
MAKE=make

# Name of your local delete program. Usually "/bin/rm -rf".
RM=/bin/rm -rf

# Name of library of object files.
CODELIB=libcomp.a

# Build rules follow.

$(CODELIB) :
	$(MAKE) -C libsrc $(CODELIB)
	mv libsrc/$(CODELIB) .
	$(MAKE) -C libsrc veryclean

expression: expression.o $(CODELIB)
	$(CC) -o $@ expression.o $(CODELIB)

opprec: opprec.o $(CODELIB)
	$(CC) -o $@ opprec.o $(CODELIB)

sim  : 
	$(MAKE) -C simulator sim
	mv simulator/sim .
	$(MAKE) -C simulator veryclean

clean:
	$(RM) *.o

veryclean:
	$(RM) $(CODELIB) *.o expression opprec
