#!/usr/bin/python
##------------------------------------------------------------------------------
##
## nfa2dfa.py  -- NFA to DFA converter.
##
## Use:
##
##     From the command-line:
##
##       $ ./nfa2dfa.py [<option(s)>] [<string>]
##
##     where <option(s)> is a one of the following command-line options
##
##       -h or -help  Help
##       -tab         Output table representing the DFA.
##       -ttab        Output the table in LaTeX format.
##       -dot         Output a Dot description of the DFA.
##       -scan        If parameter <string> is supplied on the
##                    command line, scan it according to the DFA built
##                    from the NFA description on stdin.
##       -min         This is an "option-modifier", it may be
##                    supplied with one of the other options
##                    (or by itself) to specify that the resulting
##                    DFA should be minimised.
##
##     In the absence of a command-line option (or if only "-min" is
##     specified), a verbose record of the operation of the subset
##     algorithm in converting the input NFA to a DFA is output to 
##     stdout.
##
##     This script, if invoked from the command-line, expects to
##     read, from stdin, a description of an NFA.  This is expected
##     to be in the "plain" output format as generated by "re2nfa.py".
##     Note that this input doesn't have to be generated by "re2nfa.py",
##     it just has to use this format.  Non-Thomspn NFAs can be 
##     described in this format, so "nfa2dfa.py" isn't limited merely
##     to converting Thompson machines.
##
##
##     If this script is invoked interactively (in an Idle session,
##     for example), use direct calls to "parsePlain" to convert
##     textual representations of NFAs to NFA objects, then use
##     "subset" (see dfa.py) to convert to a DFA, and the "scan"
##     method of DFA to scan strings according to the generated
##     DFA.  Most documentation can be found in "dfa.py", this
##     script just contains the plain-format NFA parser and a
##     command-line options handler.
##
##
from fa import FA
from nfa import NFA, ThompsonNFA
from state import State,DFAState
from dfa import DFA, subset, DFAScanner
from dfamin import minimiseDFA
import os


def processArgs(argList):
    if "-min" in argList:
        minimise = True
        argList.remove("-min")
    else:
        minimise = False
    if len(argList) > 0 and argList[0][0] == '-': option = argList[0]
    else: option = "-plain"

    nfa = None
    if option.startswith("-h"): print_help_text()
    else:
        nfaString = sys.stdin.read()
        nfa = parsePlain(nfaString)
        if nfa == None: return
        if   option == "-plain":
            dfa = subset(nfa, verbose=True)
            print "\nDFA construction complete. DFA is\n"
            dfa.output_table()
            if minimise:
                print "\nDFA state minimisation requested.\n"
                dfa = minimiseDFA(dfa,verbose=True,reorder=False)
                print "\nMinimised DFA is\n"
                dfa.output_table()
        else:
            dfa = subset(nfa, verbose=False)
            if minimise: dfa = minimiseDFA(dfa,verbose=False,reorder=True)
            if   option == "-tab":     dfa.output_table()
            elif option == "-ttab":    dfa.output_table(latex=True)
            elif option == "-dot":     dfa.output_dot("DFA")
            elif option == "-scan":
                if len(argList) > 1:   dfa.scan(argList[1],verbose=True)
                else:
                    print "No string to scan"
                    print_help_text()
            else:
                print "Unknown option: %s" % option
                print_help_text()


def print_help_text():
    print """    Use:

         From the command-line

           $ ./nfa2dfa.py [<option(s)>] [<string>]

         where <option(s)> is a one of the following command-line options

           -h        Help
           -tab      Output table representing the DFA.
           -ttab     Output the table in LaTeX format.
           -dot      Output the DFA in GraphViz dot format.
           -scan     If parameter <string> is supplied on the
                     command line, scan it according to the DFA built
                     from the NFA description on stdin.
           -min      This is an "option-modifier", it may be
                     supplied with one of the other options
                     (or by itself) to specify that the resulting
                     DFA should be minimised.

         In the absence of a command-line option (or if only "-min" is
         specified), a verbose record of the operation of the subset
         algorithm in converting the input NFA to a DFA is output to 
         stdout.

         A textual description of the NFA to be converted is expected
         on stdin.  This should be in the "plain" output format
         generated by "re2nfa".
    """


##------------------------------------------------------------------------------
##            
## "parsePlain" is a parser for a plain-format NFA held as a multi-line string 
## passed in as parameter "plainNFA"
##
def parsePlain(plainNFA):
    """Parse a plain-format NFA description from a multi-line string
       and generate an NFA object from it, either a ThomsponNFA or
       a plain NFA (for non-Thompson machines)."""
    scanner = PlainScanner(plainNFA)
    try:
        accept("NFA",scanner)
        stateCount = parseStateCount(scanner)
        stateList = []
        stateNameMap = {}
        for i in range(stateCount):              ## Create a list of states with names from
            state = State(str(i),(-1,-1),list()) ## 0 up, dummy positions and no links.
            stateList.append(state)
            stateNameMap[str(i)] = state
            
        startStateName = parseStartState(scanner)

        nfa, nfaType = parseNFAtype(scanner)
    
        nfa.startState = stateNameMap[startStateName]
        nfa.stateCount = stateCount
        nfa.alphabet = set([])
    
        nfa.width = 0           ## These fields are not defined for an NFA read from a simple
        nfa.height = 0          ## textual description.
        nfa.rePrecedence = 0

        acceptingStateName,regExpr = parseAcceptingState(scanner)
        nfa.finalStates.append(stateNameMap[acceptingStateName])
        nfa.regExprs.append(regExpr)
        if nfaType == "Non-Thompson":
            while scanner.currentToken == "Accepting":
                acceptingStateName,regExpr = parseAcceptingState(scanner)
                nfa.finalStates.append(stateNameMap[acceptingStateName])
                nfa.regExprs.append(regExpr)

        while scanner.currentToken == "State":
            parseStateTransitions(scanner,nfa,stateNameMap)
            
    except SyntaxError, e:
        print "Syntax Error:", e
        nfa = None
        stateList = []
    return nfa


def accept(expected,scanner):
    if scanner.currentToken == expected: scanner.nextToken()
    else:
        raise SyntaxError, ("'%s' expected, got '%s'" %\
                            (expected,scanner.currentToken))

def parseStateCount(scanner):
    accept("States:",scanner)
    return int(scanner.getToken())

def parseStartState(scanner):
    accept("Start",scanner)
    accept("state:",scanner)
    return scanner.getToken()
    

def parseNFAtype(scanner):
    nfaType = scanner.getToken()
    accept("NFA",scanner)
    if nfaType == "Thompson": nfa = ThompsonNFA()
    elif nfaType == "Non-Thompson": nfa = NFA()
    else:
        raise SyntaxError, ("Unknown NFA type: '%s'" % nfaType)
    return nfa, nfaType

def parseAcceptingState(scanner):
    accept("Accepting",scanner)
    accept("state:",scanner)
    stateName = scanner.getToken()
    regExpr = scanner.getToken()
    return stateName,regExpr

def parseStateTransitions(scanner,nfa,stateNameMap):
    accept("State",scanner)
    sourceState = stateNameMap[scanner.getToken()]
    while len(scanner.currentToken) > 0 and scanner.currentToken[0] == '(':
        transitionChar = scanner.getToken().strip("()")
        nfa.alphabet.add(transitionChar)
        accept("-->",scanner)
        targetState = stateNameMap[scanner.getToken()]
        sourceState.successors.append((transitionChar,targetState,None))


class PlainScanner(object):
    def __init__(self,charbuffer):
        self.index = 0
        self.charbuffer = charbuffer
        self.nextToken()

    def nextToken(self):
        self.currentToken = ""
        ch = ""
        while self.index < len(self.charbuffer):
            ch = self.charbuffer[self.index]
            if not ch in " \t\n\r": break
            self.index += 1
        if ch == "'":
            self.index += 1
            while self.index < len(self.charbuffer):
                ch = self.charbuffer[self.index]
                if ch in "\n\r": break
                self.index +=1
                if ch == "'": break
                self.currentToken += ch
        elif ch == '(':
            self.currentToken += ch
            self.index += 1
            while self.index < len(self.charbuffer):
                ch = self.charbuffer[self.index]
                if ch in "\n\r": break
                self.index +=1
                self.currentToken += ch
                if ch == ")": break
        else:
            while self.index < len(self.charbuffer):
                ch = self.charbuffer[self.index]
                ## print "token", self.currentToken, self.index, ch
                if ch in " \t\r\n": break
                self.currentToken += ch
                self.index += 1

    def getToken(self):
        s = self.currentToken
        self.nextToken()
        return s



        
#### Non-Thompson NFA for (a|b)*ab
####
##ntNFA=parsePlain("""NFA
##States: 3
##Start state: 0
##Non-Thompson NFA
##  Accepting state:  2 '(a|b)*ab'
##State 0
##    (a) --> 0
##    (a) --> 1
##    (b) --> 0
##State 1
##    (b) --> 2
##""")
##
#### Thompson NFA for (a|bc)*
####    
##tNFA1=parsePlain("""NFA
##States: 9
##Start state: 0
##Thompson NFA
##  Accepting state:      2  '(a|bc)*'
##State 0
##    (eps) --> 1
##    (eps) --> 2
##State 1
##    (eps) --> 3
##    (eps) --> 4
##State 3
##    (a) --> 5
##State 4
##    (b) --> 6
##State 5
##    (eps) --> 7
##State 6
##    (c) --> 8
##State 7
##    (eps) --> 2
##    (eps) --> 1
##State 8
##    (eps) --> 7
##""")
##
#### Non-Thompson NFA for abc or (a|b|c)*
####    
##tNFA2=parsePlain("""NFA
##States: 17
##Start state: 0
##Non-Thompson NFA
##  Accepting state:  4  'abc'
##  Accepting state:  7  '(a|b|c)*'
##State 0
##    (eps) --> 1
##    (eps) --> 5
##State 1
##    (a) --> 2
##State 2
##    (b) --> 3
##State 3
##    (c) --> 4
##State 5
##    (eps) --> 6
##    (eps) --> 7
##State 6
##    (eps) --> 8
##    (eps) --> 9
##State 8
##    (eps) --> 10
##    (eps) --> 11
##State 9
##    (c) --> 12
##State 10
##    (a) --> 13
##State 11
##    (b) --> 14
##State 12
##    (eps) --> 15
##State 13
##    (eps) --> 16
##State 14
##    (eps) --> 16
##State 15
##    (eps) --> 7
##    (eps) --> 6
##State 16
##    (eps) --> 15
##""")

##------------------------------------------------------------------------------
##
## Startup code, run when this is a command-line operation.  Only run this
## code if (1) this is a __main__ module and (2), it's not inside IDLE.
##
import sys

if __name__ == "__main__":
    if not str(sys.stdin).startswith("<idle"): processArgs(sys.argv[1:])

