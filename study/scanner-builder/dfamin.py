##------------------------------------------------------------------------------
##
## dfamin.py -- Routines for DFA minimisation.
##
## Main interface is via function "minimiseDFA".  Everything else is private 
## to this source file.  These routines are essentially a Python implementation
## of the algorithm for DFA minimisation presented in Aho, Compilers, 2nd ed.,
## pp 180-184.
##
## Function:
##
##          minimiseDFA(dfa,verbose=False,reorder=False)
##
##              Arguments:
##
##                  dfa - A DFA object representing the DFA to be minimised.
##
##                  verbose - A boolean.  If True, means print out a verbose
##                            report of the operation of the minimisation
##                            algorithm.  If False, operate quietly.
##                            Defaults to False: operate quietly.
##
##                  reorder - A boolean.  If True, the states of the final,
##                         minimised DFA will be renamed in "ascending"
##                            order (A, B, C, etc., without gaps).  If
##                            False no re-ordering will occur (leading to
##                         possible gaps in the state sequence of the
##                         final DFA, showing where redundant states
##                            have been removed).
##                            Defaults to False: do not reorder.
##             
##              Returns: minimum-state DFA (as a DFA object).
##
##
## Internal functions:
##
##     splitStateSet
##     targetStateSet
##     countUniques
##     showPartition
##     makeInitialPartition
##     buildMinDFA
##
##

from state import State,DFAState
from dfa import DFA,StateSet, nextDFAStateName


##------------------------------------------------------------------------------
##
## The "dead state" is an explicit target for non-existant transitions out
## of states in the current DFA (the one to be minimised).  The idea is to
## convert the current DFA to a "strict" one, where there is a transition from
## every state on every character of the DFA alphabet.
##
## deadSS is the "dummy" state set that includes only the dead state.
##
## The dead state and dead state set are really just useful fictions used to
## make the operation of this minimisation algorithm simpler.  The dead state
## is not included in the final, minimised DFA (which may not be a "strict"
## DFA because of this.  Since the action on an unrecognised character is to
## wind-back to the last recognition point, this doesn't matter.)
##

deadState = DFAState("phi")      ## The "dead" state, we use 'phi'.
deadSS    = StateSet(deadState)


##------------------------------------------------------------------------------
##
## minimiseDFA: Generate the minimal (up to renaming) DFA derivable from the
## argument dfa.  Returns the minimised DFA as a DFA object.
##
##

def minimiseDFA(dfa,verbose=False,reorder=False):
    "Return the minimal-state DFA equivalent to the argument."
    partition = makeInitialPartition(dfa) + [deadSS]
    changes = True
    while changes:
        if verbose:
            print "Current partition is", ; showPartition(partition)
        newPartition = []
        changes = False
        for stateSet in partition:
            if len(stateSet) > 1:
                didSplit,newSSes = splitStateSet(stateSet,partition,dfa,verbose)
                newPartition.extend(newSSes)
                changes |= didSplit
            else:
                newPartition.append(stateSet)
        if changes: partition = newPartition[:]
    ## We are done, no changes to the partitions on the last round
    if verbose:
        print "Partitioning complete.  Partition for minimum-state DFA is:",
        showPartition(partition)
    ## Generate the new DFA by selecting one DFA state from each state set in the
    ## current partition (omitting the state set containing the "dead state").
    minDFA = buildMinDFA(dfa,partition)
    ## May want to re-order the states in the new, minimised DFA.
    if reorder: reorderStates(minDFA)
    return minDFA
        

##------------------------------------------------------------------------------
##
## splitStateSet: Given a state set with at least two states in it, check to
## see if it needs to be split, and if so, split it.
##
## The idea is to loop over each character in the DFA alphabet, examining
## transitions out of the states of the set on that character.  If transitions
## go to more than one target state set, the current state ste needs splitting.
##
## Note that this routine immediately exits when it has done a split on a
## particular character (that is, it doesn't go back and examine all the
## remaining characters in the input after doing a split, it relies on the
## caller to ensure that all options are investigated).  If it doesn't find
## a split on a particular character, on the other hand, it loops around
## to examine the next character in the alphabet, until either a split is
## found, and an exit forced, or no split is found.
##
## Returns a tuple (<Boolean>,<List>). The boolean indicates whether or not
## a split occurred.  The list is a list of state sets resulting from the
## operation of this routine.  If a split occurred, it will be a list of
## the split state sets that result from the argument state set, splitting
## on some character of the DFA alphabet, if no split occurred it will simply
## be the original, argument state set wrapped in a list.
##
##

def splitStateSet(stateSet,partition,dfa,verbose=False):
    "Check to see if a state set needs splitting, and if it does, split it."
    ssl = list(stateSet)
    for ch in dfa.alphabet:
        ## targets is a list containing the target state sets of all the
        ## states in this set under transitions on 'ch'.
        targets = [targetStateSet(ch,state,partition) for state in ssl]
        ## countUniques counts the number of unique target state sets, if this
        ## is > 1, a split in the current state set is needed.
        uniqueTargetCount,indexMap = countUniques(targets)
        if uniqueTargetCount > 1:
            if verbose:
                print "  Splitting state group %s on '%s' into" % \
                      (stateSet.toString(),ch),
            splitSets = []
            for i in range(uniqueTargetCount): splitSets.append(StateSet([]))
            for i,state in enumerate(ssl): splitSets[indexMap[i]].add(state)
            if verbose:
                print [ss.toString() for ss in splitSets]
            return True,splitSets
    return False,[stateSet]        

        
##------------------------------------------------------------------------------
##
## targetStateSet: Given a DFA state, a transition character, and the current
## partition, return the target state set that the transition character will
## "go to" from this state.  If there is no transition out of the state on
## a particular character, return the state set associated with the "dead
## state".
##
##
        
def targetStateSet(ch,state,partition):
    "Find the target state set of a state under a transition on a char."
    targetState = deadState
    for successor in state.successors:
        if successor[0] == ch:
            targetState = successor[1] ; break
    for stateSet in partition:
        if targetState in stateSet:
            return stateSet
    print "targetStateSet, no target for (%s,%s) found in" % (ch,state.name),
    showPartition(partition)
    return None

    
##------------------------------------------------------------------------------
##
## countUniques: Given a list of DFA state sets, which represent the state set
## targets of a state set under a transition on a character, count the number
## of unique targets there are in this list.
##
## Also generate a mapping vector (a list the same length as the argument
## "targetStateSets").  Each entry in this list will be a number representing
## the (unique) "label" of the target state set in the argument list.
##
## Example.  Say the argument state set list looks like this:
##
##     [<ss A>, <ss Z>, <ss A>, <ss Z>, <ss Z>, <ss X>]
##
## (where <ss A>, etc., are StateSet objects), then the routine will return
## a count of 3, because there are three unique target state sets in the list,
## and will return a "mapping vector" that looks like this:
##
##     [0, 1, 0, 1, 1, 2]
##
## where the first unique state set to be encountered in the list gets index
## 0, the next, index 1, etc.
##
## What's the point of this?  The idea is that the calling routine will need
## to divide the original state set into N groups (if this routine returns
## a target count of N), so will need to create a list of N StateSets as
## targets.  The mapping vector can be used to assign states from the
## original state set to the appropriate target.
##
## Example:
##
## Say the original state set is {A, B, C, D} (these are State objects), and
## the mapping returns [<ss 1>, <ss 4>, <ss 2>, <ss 1>] under a certain
## character.  Then this routine will return (3,[0,1,2,0]), and the caller
## can split {A,B,C,D} into the 3-element list [{A,D}, {B}, {C}].
##
##                               

def countUniques(targetStateSets):
    """Count the number of unique states sets in the argument and also
       generate a mapping vector, mapping state sets onto unique 0-based
       indices (which can be used to generate a split os a state set."""
    count = 0
    found = []
    indexMap = []
    for stateSet in targetStateSets:
        if not stateSet in found:
            indexMap.append(count)
            count += 1
            found.append(stateSet)
        else:
            indexMap.append(found.index(stateSet))
    return count,indexMap


##------------------------------------------------------------------------------
##
## showPartition: Print to standard output a prettily-formatted version of this
## partition, with the state sets neatly printed by name (rather than as
## Python objects).
##
##

def showPartition(partition):
    print ["%s" % s.toString() for s in partition]



##------------------------------------------------------------------------------
##
## makeInitialPartition: divide the dfa states into two or more initial
## sets.  Make one set for each unique type of accepting state, and a
## set for what is left over.
##
    
def makeInitialPartition(dfa):
    "Generate the initial partition of the dfa."
    allstates = StateSet(dfa.listStates())
    if len(dfa.regExprs) == 1:
        accepting = StateSet(dfa.finalStates)
        allstates -= accepting
        if len(allstates) > 0:
            partition = [allstates-accepting,accepting]
        else:
            partition = [accepting]
    else:
        acceptingSets = {}
        for i,re in enumerate(dfa.regExprs):
            if acceptingSets.has_key(re):
                acceptingSets[re].add(dfa.finalStates[i])
            else:
                acceptingSets[re] = StateSet(dfa.finalStates[i])
        partition=[]
        for accepting in acceptingSets.itervalues():
            partition.append(accepting)
            allstates -= accepting
        if len(allstates) > 0:  partition.insert(0,allstates)
    return partition


##------------------------------------------------------------------------------
##
## buildMinDFA: Take a dfa and a partition representing the minimal-state
## version of the dfa and return a DFA object representing the minimal-state
## DFA.  Note that any state set containing the dead state is ignored.
##
## N.B.  The original DFA is corrupted by this routine, and shouldn't be
## accessed after it is called.
##
##

def buildMinDFA(dfa, partition):
    ## showPartition(partition)
    ## print [s.name for s in dfa.listStates()]
    ## First make a new DFA and ensure that its start state is the same
    ## as that of the original DFA.  For all the other states in the
    ## new DFA, just select a state from each of the state sets in the
    ## partition (expect for any dead-state state sets, which are
    ## ignored).
    newDFA = DFA()
    newDFA.startState = dfa.startState
    newDFA.alphabet = dfa.alphabet
    selectedStates = []
    for stateSet in partition:
        if newDFA.startState in stateSet:
            selectedStates.append((newDFA.startState,stateSet))
        elif stateSet != deadSS:
            selectedStates.append((list(stateSet)[0],stateSet))
    ## Now construct new links in the new DFA.  Note that we are
    ## modifiying states *shared* with the original DFA, so this
    ## action *corrupts* the original DFA.
    newDFA.finalStates = []
    newDFA.regExprs = []
    for state,stateSet in selectedStates:
        for i,successor in enumerate(state.successors):
            for targetState,targetStateSet in selectedStates:
                if successor[1] in targetStateSet:
                    state.successors[i] = (successor[0],targetState)
                    break
        if state in dfa.finalStates:
            newDFA.finalStates.append(state)
            newDFA.regExprs.append(dfa.regExprs[dfa.finalStates.index(state)])
    newDFA.stateCount = len(selectedStates)
    return newDFA


##------------------------------------------------------------------------------
##
## reorderStates: Re-order the states in a DFA so that any gaps in the ordering
## (maybe left by the minimisation algorithm) are removed.
##
##

def reorderStates(dfa):
    name = nextDFAStateName(dfa.startState.name) ## Keep the same "type"" of name as originally.
    for state in dfa.listStates():
        if state != dfa.startState:
            state.name = name
            name = nextDFAStateName(name)
            

##------------------------------------------------------------------------------
##
## Debugging stuff
##
##
##from re2nfa import parseRE
##from dfa import subset
##
##nfa=parseRE("(a|b)*abb")
##dfa=subset(nfa,False)
##
#### DFA from F&LeBm p 126.
##dfastate1=DFAState('1')
##dfastate2=DFAState('2')
##dfastate3=DFAState('3')
##dfastate4=DFAState('4')
##dfastate5=DFAState('5')
##dfastate6=DFAState('6')
##dfastate7=DFAState('7')
##
##dfastate1.successors=[('a',dfastate2,None),('d',dfastate5,None)]
##dfastate2.successors=[('b',dfastate3,None)]
##dfastate3.successors=[('c',dfastate4,None)]
##dfastate4.successors=[]
##dfastate5.successors=[('b',dfastate6,None)]
##dfastate6.successors=[('c',dfastate7,None)]
##dfastate7.successors=[]
##
##dfaflb126=DFA()
##dfaflb126.startState=dfastate1
##dfaflb126.finalStates=[dfastate4,dfastate7]
####dfaflb126.regExprs=['abc','dbc']     ## Two different REs
##dfaflb126.regExprs=['acc','acc']       ## Dummy REs allow the final states to merge.
##dfaflb126.alphabet=set(['a','b','c','d'])
##dfaflb126.stateCount=7
##
##
##p126=makeInitialPartition(dfaflb126)
##
##showPartition(p126)
##

    
