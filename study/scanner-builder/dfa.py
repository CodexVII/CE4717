##------------------------------------------------------------------------------
##
## dfa.py -- Implements a DFA class and the subset algorithm for building
##           DFAs from NFAs.
##
##
## Classes: DFA --        A specialization of the FA class from fa.py for
##                        deterministic automata.
##             method: scan -- return all matches found by this DFA when
##                             matching an argument string.
##
##          StateSet   -- A set of NFA State objects.  Method "toString"
##                        is used to print it out in a "pretty" way.
##
##          DFAScanner -- A class designed to scan tokens from a string
##                        using a DFA.  Mainly invoked via the DFA
##                        "scan" method.
##
## Functions:
##
##          subset: Accept an NFA object and generate a corresponding DFA
##                  by applying the subset algorithm.
##
##          e_closure: Generate the epsilon-closure of a set of NFA states
##                     (represented as a StateSet object).
##
##
from fa import FA
from nfa import NFA, ThompsonNFA
from state import State,DFAState


##------------------------------------------------------------------------------
##
## DFA -- Class representing a Deterministic Finite Automaton.  Inherits
## from FA (fa.py) and adds a method for scanning a string.  A DFA also
## differs from an FA in that it is assumed to be build (using subset, below)
## from DFAState objects, which contain a StateSet field that holds the
## NFA StateSet associated with a DFA state that has been build using the
## subset algorithm.
##
##
class DFA(FA):
    ## scan: return a list of all the matches found by this DFA when scanning
    ## the argument "astring".  Each element of the returned list is a tuple
    ## (matching-re,matching-substring).  If argument "verbose" is passed
    ## as True, output detailed infomation about the operation of the DFA.
    ##
    ## This method is basically a wrapper around a DFAScanner object (see
    ## below).
    ##
    def scan(this,astring,verbose=False):
        scanner = DFAScanner(this,astring)
        matches = []
        sepstr = 70*"="
        if verbose: print sepstr
        while True:
            (regExpr,matchedStr)=scanner.scanNext(verbose)
            if verbose:
                if regExpr: print "\nMatched RE %s, string '%s'\n" % (regExpr,matchedStr)
                else: print "\nNo match\n"
                print sepstr
            if not regExpr: break
            matches.append((regExpr,matchedStr))
            if len(matchedStr) == 0: break
        return matches
        

##------------------------------------------------------------------------------
##
## "subset and" "doSubset" implement the subset algorithm that converts an NFA
## passed as an argument, to a DFA.  Parameter "verbose" controls whether the
## automaton outputs lots of operational information during a run.
##
##
def subset(nfa, verbose=True):
    ## Note: may need to do the subset construction potentially *twice*.  The first 
    ## time counts the number of states in the resulting DFA.  If this is 14 or less, 
    ## then do it again, using state names 'A', 'B', etc.  If it is greater than
    ## 14 (which would result in 'O' appearing as a DFA state name, something that
    ## might cause confusion with NFA state 0), use state names 'S0', 'S1', 'S2',
    ## etc.  Note that if the state count exceeds 14 and the verbose flag is False
    ## the subset construction only happens once because the (quiet) first time
    ## around uses the 'S0', 'S1' etc. sequence for naming.
    ##
    dfa = doSubset(nfa,"S0",False)
    if dfa.stateCount > 14: dfaStartStateName = "S0"
    else: dfaStartStateName = "A"    
    if dfa.stateCount <= 14 or verbose:
        dfa = doSubset(nfa,dfaStartStateName,verbose)
    return dfa


def doSubset(nfa,stateName,verbose):
    dfa = DFA()
    dfa.alphabet = set(nfa.alphabet)
    dfa.alphabet.discard(FA.EPS)
    sortedAlphabet=sorted(dfa.alphabet)
    dfaStateList = [DFAState(stateName)] ; stateName = nextDFAStateName(stateName)
    nfaStateSetQueue = [e_closure(StateSet(nfa.startState))]
    index = 0
    while index < len(nfaStateSetQueue):
        aStateSet = nfaStateSetQueue[index]
        aDFAState = dfaStateList[index] ; index += 1
        aDFAState.stateSet = aStateSet
        if verbose:
            print "\n---------------------------------------------------------------------"
            print "Working with DFA state %s (state set: %s)." % \
                  (aDFAState.name,aStateSet),
        for i,nfaFinal in enumerate(nfa.finalStates):
            if nfaFinal in aStateSet:
                dfa.finalStates.append(aDFAState)
                dfa.regExprs.append(nfa.regExprs[i])
                if verbose:
                    print "\nAccepting state: Associated RE is '%s'." % nfa.regExprs[i],
                break
        if verbose: print "\n"
        for ch in sortedAlphabet:
            targetStateSet = e_closure(findTargetSet(aStateSet,ch,nfa))
            if len(targetStateSet) > 0:
                if verbose: print "    Transition on '%s' to %s.  " % (ch, targetStateSet),
                if targetStateSet in nfaStateSetQueue:
                    targetDFAState = dfaStateList[nfaStateSetQueue.index(targetStateSet)]
                    if verbose: print "Not new: %s." % targetDFAState.name 
                else:
                    targetDFAState = DFAState(stateName) ; stateName = nextDFAStateName(stateName)
                    nfaStateSetQueue.append(targetStateSet)
                    dfaStateList.append(targetDFAState)
                    if verbose:
                        print "New: %s. " % targetDFAState.name
                if verbose:
                    print "      Noting transition %s --%s-> %s\n" %\
                          (aDFAState.name,ch,targetDFAState.name)
                aDFAState.successors.append((ch,targetDFAState,None))
            else:
                if verbose: print "    No transition on '%s'\n" % ch
    dfa.startState = dfaStateList[0]
    dfa.stateCount = len(dfaStateList)
    return dfa


##------------------------------------------------------------------------------
##
## findTargetSet: Given an NFA StateSet, a transition character and an nfa as
## arguments, return the state set containing all the states reachable from
## the argument state set under the given argument character.
##
##
def findTargetSet(stateSet,ch,nfa):
    "Find all states reachable from a state set on a given char for an nfa."
    target = StateSet([])
    for state in stateSet:
        for successor in state.successors:
            if successor[0] == ch: target.add(successor[1])
    return target


##------------------------------------------------------------------------------
##
## nextDFAStatename: Generate a new DFA state name from the current one.
##
##
def nextDFAStateName(stateName):
    "Return the next state name in the sequence being used, either S0,S1,... or A,B,C...."
    if stateName[0] == 'S':   ## Name is "Sxxx"
        val = int(stateName[1:])
        return "S%d" % (val+1) 
    else:                     ## Name is single char alphabetic, A, B, C, ...
        return chr(ord(stateName)+1)

            
##------------------------------------------------------------------------------
##
## e_closure: Calculate the epsilon closure of an NFA StateSet.
##
##
def e_closure(stateSet):
    assert isinstance(stateSet,StateSet)
    stateQueue = list(stateSet)
    while stateQueue != []:
        state = stateQueue.pop(0)
        for successor in state.successors:
            if successor[0] == NFA.EPS and not successor[1] in stateSet:
                stateSet.add(successor[1])
                stateQueue.append(successor[1])
    return stateSet


##------------------------------------------------------------------------------
##
## StateSet is a set of NFA states, this class if built on top of the standard
## Python "set" class, but assumes that the contents are State objects, and
## that the names of the states are integers, or strings that can be
## converted to integers.
##
## Note the "abbrev" flag.  If this is True (the default) and the number of
## states in the set exceeds 10, only print out the first two and last two.
##
##

class StateSet(set):
    def __init__(this,stateList):
        if isinstance(stateList,State): stateList = [stateList]
        set.__init__(this,stateList)

    def __str__(this):
        return this.toString()

    def toString(this,laTeX=False,abbrev=True):
        nameList = [state.name for state in this]
        if len(nameList) > 0:
            if isinstance(nameList[0],int) or nameList[0].isdigit():   # Names look like '0', '1', etc.
                nameList.sort(lambda x,y: int(x) - int(y))
            else:
                name = nameList[0]
                if name.startswith("S") and name[1:].isdigit(): # looks like 'S0', 'S1', etc.
                    nameList.sort(lambda x,y: int(x[1:]) - int(y[1:]))
                else:   # Something else, just sort alphabetically
                    nameList.sort(lambda x,y: cmp(x,y))
        if laTeX: s = "\\{"
        else: s = "{"
        if len(nameList) > 10 and abbrev:
            s += "%s, %s, ... %s, %s" % \
                 (nameList[0], nameList[1], nameList[-2], nameList[-1])
        else:
            start = True
            for name in nameList:
                if start:
                    s += str(name)
                    start = False
                else:
                    s += ", " + str(name)
        if laTeX: s += "\\}"
        else: s += "}"
        return s



##------------------------------------------------------------------------------
##
## DFAScanner is a DFA scanner class.  It takes a DFA and a string to
## be scanned as initialisers and returns recognised REs defined
## by the NFA on calls to "scanNext".
##
##
    
class DFAScanner(object):
    def __init__(this,dfa,string):
        this.dfa = dfa
        this.string = string
        this.startIndex = 0
        this.endIndex = 0
        this.currentIndex = 0

    def scanNext(this,verbose=True):
        this.regExpr = None
        this.state = this.dfa.startState
        if verbose:
            print "  Current   | Next |"
            print " DFA State  |  Ch  | (Current NFA State Set)"
            print "------------+------+--------------------------------------"
        while True:
            stateIsFinal = False
            if this.state in this.dfa.finalStates:
                stateIsFinal = True
                this.endIndex = this.currentIndex
                this.regExpr = this.dfa.regExprs[this.dfa.finalStates.index(this.state)]
            ch = this.getChar()
            if verbose:
                
                if len(this.state.name) == 1:  s = "   %s " % this.state.name
                else: s = "  %3s" % this.state.name
                if stateIsFinal: s += " (Acc) |"
                else: s += "       |"
                s += " '%s'  | (%s)" % (ch, this.state.stateSet)
                print s
            newState = this.findTransition(ch)
            if not newState: break
            this.currentIndex += 1
            this.state = newState
        if verbose and this.currentIndex > this.endIndex:
            print "Halted in nonaccepting state"
            if this.regExpr != None:
                print "    Rewinding to last accepting state (rollback by %d chars)" % \
                      (this.currentIndex - this.endIndex)
            else:
                print "    Can't rewind, no previous accepting state"
        recognisedString = this.string[this.startIndex:this.endIndex]
        this.startIndex = this.endIndex
        this.currentIndex = this.endIndex
        return this.regExpr, recognisedString

    def getChar(this):
        if this.currentIndex < len(this.string):
            return this.string[this.currentIndex]
        else:
            return '$'

    def findTransition(this,ch):
        for succ in this.state.successors:
            if ch == succ[0]: return succ[1]
        return None
