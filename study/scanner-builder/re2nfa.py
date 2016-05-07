#!/usr/bin/python
##------------------------------------------------------------------------------
##
## re2nfa.py  -- Regular expression to NFA converter.
##
## Use:
##
##     From the command-line
##
##       $ ./re2nfa.py [<option>] <re1> [<re2> ... <reN>]
##
##     where <option> is a one of the following command-line options
##
##       -h or -help  Help
##       -tab         Output table representing the NFA of the RE(s).
##       -ttab        Output the table in LaTeX format.
##       -graph       Display the NFA graphically using Tkinter.
##       -psgraph     Output the NFA as PostScript to standard output.
##       -dot         Output a Dot description of the NFA.
##
##     In the absence of a command-line option, a simple textual
##     representation of the NFA is output to stdout.
##
## N.B.  If a single regular-expression is entered on the command-line,
## a Thompson NFA is generated.  If more than one RE is specified, a
## non-Thompson NFA is generated, consisting of N Thompson NFAs (one
## for each RE on the command-line) joined by an N-way multi-branch initial
## state.  The property of this NFA is that it will have multiple final
## states (unlike a Thompson NFA), the first RE in the input will
## correspond to the lowest-numbered final state, the next to the
## next-highest numbred final state, etc.  Thus, an NFA to DFA
## converter can decide which RE has been recognised by examining the
## state-set of any halting state that it generates, and returning
## the RE associated with the lowest-numbered NFA final state (this
## technique automatically prioritises the REs, with the first one
## having highest priority, etc.).
##
##
##
## To use this code in an interactive Python session, see
## "parseREs / parseRE" below, and "TkDrawing", "Postscript" etc.
##
##
## 
from nfa import *
from drawingsurface import *

def processArgs(argList):
    if argList[0][0] == '-': option = argList.pop(0)
    else: option = "-plain"

    nfa = None
    if option.startswith("-h") or len(argList) == 0: print_help_text()
    else:
        if len(argList) == 1:
            nfa = parseRE(argList[0])
        else:
            nfa = parseREs(argList)
        if   option == "-tab":     nfa.output_table()
        elif option == "-ttab":    nfa.output_table(latex=True)
        elif option == "-graph":   TkDrawing(nfa)
        elif option == "-psgraph": PostScript(nfa)
        elif option == "-dot":     nfa.output_dot()
        elif option == "-plain":   nfa.output_plain()
        else:
            print "Unknown option: %s" % option
            print_help_text()
    ##return nfa


def print_help_text():
    print """    Use:

         From the command-line

           $ ./re2nfa.py [<option>] <re1> [<re2> ... <reN>]

         where <option> is a one of the following command-line options

           -h        Help
           -tab      Output table representing the NFA of the RE(s).
           -ttab     Output the table in LaTeX format.
           -graph    Display the NFA graphically using Tkinter.
           -psgraph  Output the NFA as PostScript to standard output.
           -dot      Output a Dot description of the NFA.
           -plain    Output a plain-text description of the NFA (default).

         In the absence of a command-line option, a simple textual
         representation of the NFA is output to stdout.
    """
    
##------------------------------------------------------------------------------
##
## The Regular-Expression parser.  Parses strings representing simple
## regular expressions (i.e., those containing *only* alternation,
## concatenation and Kleene-closure operators) and returns NFA objects
## representing them (as NFA graphs).
##
## There are two main interfaces:
##
##      parseREs   - This accepts a string containing *multiple*
##                   REs (separated by whitespace) and returns an
##                   OuterChoiceNFA to represent them.  Alternatively,
##                   it accepts a list of strings, each representing
##                   a single RE, and returns an OuterChoiceNFA
##                   to represent them.  This option is useful where
##                   the REs are in separate strings, as when reading
##                   the command-line via sys.argv, for example.
##
##      parseRE    - This accepts a string containing a *single*
##                   RE and returns a ThompsonNFA object to
##                   represent it.
##
## The other routines in the parser are not designed to be called
## directly by the user.  They implement a recursive-descent parser
## for REs.  The EBNF grammar is:
##
##      <OptionsRE>   :== <ConcatRE> { '|' <ConcatRE> }
##      <ConcatRE>    :== <ClosureRE> { <ClosureRE> }
##      <ClosureRE>   :== <PrimitiveRE> { '*' }
##      <PrimitiveRE> :== "(" <OptionsRE> ")" | CHAR
##
## Note that the grammar collapses multiple Kleene closure operators
## in a row into a single such operator (i.e., a** and a*** etc. parse
## as a*).  This is because a* == a** == a*** etc.
##

def parseREs(regExpressions):
    "Parse a number of (space-separated) REs and return an OuterChoiceNFA."
    if isinstance(regExpressions,str):
        return OuterChoiceNFA([parseRE(re) for re in regExpressions.split()])
    elif isinstance(regExpressions,list):
        return OuterChoiceNFA([parseRE(re) for re in regExpressions])
    else:
        print "Input format is not correct, should be a string or list of strings."
        return None

def parseRE(regExprStr):
    "Parse a regular expression and return a (Thompson) NFA for it."
    return parseOptionsRE(StringBuffer(regExprStr))

def parseOptionsRE(sbuf):
    "Outer-level RE, parsing Option ('|') operators."
    nfa1 = parseConcatRE(sbuf)
    nfa = nfa1
    while nfa1 and sbuf.peek() == '|':
        sbuf.next()
        nfa1 = parseConcatRE(sbuf)
        if nfa1: nfa = ChoiceNFA(nfa,nfa1)
    return nfa

def parseConcatRE(sbuf):
    "Mid-level RE, parsing concatenations of lower-lvel REs."
    nfa1 = parseClosureRE(sbuf)
    nfa = nfa1
    while nfa1 and sbuf.peek() and sbuf.peek() not in "|)":
        nfa1 = parseClosureRE(sbuf)
        if nfa1: nfa = CompositeNFA(nfa,nfa1)
    return nfa

def parseClosureRE(sbuf):
    "Low-level RE, parsing optional Kleene Closures."
    nfa = parsePrimitiveRE(sbuf)
    if sbuf.peek() == '*':
        sbuf.next()
        while sbuf.peek() == '*': sbuf.next()
        nfa = ClosureNFA(nfa)
    return nfa

def parsePrimitiveRE(sbuf):
    "Lowest-level RE, either a single char or a parenthesised grouping."
    ch = sbuf.peek()
    if ch == '(':
        sbuf.next()
        nfa = parseOptionsRE(sbuf)
        if sbuf.peek() == ')': sbuf.next()
        else:
            print "Syntax Error: ')' expected."
            nfa = None
    elif ch:
        nfa = PrimitiveNFA(ch)
        sbuf.next()
    else:
        print "Syntax Error: end of input encountered"
        nfa = None
    return nfa


##------------------------------------------------------------------------------
##
## StringBuffer is a convenience class for the regular-expression parser. 
## It allows a string to be "walked-over" one character at a time, with
## lookahead via method "peek" and advance via method "next".  Once the
## buffer has been completely examined, calls to "peek" return None.
##
## Not for general use, only useful for the parser.
##

class StringBuffer(object):
    def __init__(this,string):
        this.string = string
        this.index = 0

    def peek(this):
        if this.index < len(this.string):
            return this.string[this.index]
        else:
            return None

    def next(this):
        this.index += 1


##------------------------------------------------------------------------------
##
## Startup code, run when this is a command-line operation.
##
##
import sys

##if __name__ == "__main__":
##    print len(sys.argv)
##    for (i,s) in enumerate(sys.argv):
##        print i,s
if __name__ == "__main__":
    if len(sys.argv) > 1:
        nfa = processArgs(sys.argv[1:])
    else:
        print_help_text()
