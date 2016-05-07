##
## Makefile for first/follow/predict-set generator under Unix/Linux/cygwin.
##
## Targets:
##
##        make ff             Make the ff program.
##        make clean          Remove ff and any intermediate .o files, also 
##                            scanner.c (which is build automatically from
##                            scanner.l).
##        make veryclean      Same as make clean but also removes executable ff.
##
## Assumes the presence of the flex scanner generator and the flex library (usually
## there on Linux).
##
CC=gcc                                                   ## C compiler to use.
CFLAGS=-ansi -pedantic -Wall                             ## Flags to the C compiler.
LEX=flex                                                 ## Gnu flex.
RM=/bin/rm -f                                            ## Delete command.
##
##
OBJS=ff.o parser.o strtab.o symbol.o useless.o scanner.o  ## Objects for linking.

##
## Build rules follow.
##
ff: $(OBJS)
	$(CC) -o $@ $(OBJS) -lfl

scanner.c: scanner.l
	$(LEX) -o $@ $?

clean:
	$(RM) $(OBJS) scanner.c

veryclean:
	$(RM) ff $(OBJS) scanner.c
