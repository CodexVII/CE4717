##--------------------------------------------------------------------------------------
##
## State represents a finite-automaton state (either an an NFA or a DFA state). 
##
## Fields are:
##
##    name:        The name of the state (string or integer).
##    position:    The (x,y) co-ordinate position of the centre of the state's circle
##                 representation.  The implicit assumption is that we are using a 
##                 millimetre-based co-ordinate system (x across, y up), with states
##                 represented graphically as circles with diameter 10mm.
##                 The Python representation of a position is as a 2-tuple of 
##                 integers, (x,y).
##    successors:  A list of the successors of this state.  Although not explicitly
##                 employed in this method, the assumed representation of a 
##                 successor link is as a 3-tuple, (char,nextstate,connector), where
##                 'char' (element[0] of the tuple) is the character upon which the
##                 transition is allowed to happen, 
##                 'nextstate' (element[1]) is another State object, the target of 
##                 the link, and;
##                 'connector' (element[2]) is a Connector object that describes how
##                 the link between these two states is to be drawn in the graphical
##                 representation, e.g., as a straight-line Arrow (class Straight), 
##                 or as a curved link with an arrow (class CurvedNFA).
##

class State(object):
    "Represents a finite-automaton state, i.e., name, position, list of linked states."
    RADIUS = 5   ## State circle's diameter is 10, hence radius = 5.
    def __init__(this,name,position=None,successors=None):
        this.name = str(name)
        this.position = position
        if successors: this.successors = successors
        else: this.successors = []

    def __repr__(this):
        s = "<State %s, " % this.name
        if this.position and this.position != (-1,-1): s += ("pos %s" % str(this.position))
        s += ": links ["
        first = True
        for succ in this.successors:
            if first: first = False
            else: s += ", "    ## char,   target-state name 
            s += "(%s) --> %s" % (succ[0],succ[1].name)
        s += "]>"
        return s

    def draw(this,paper):
        if not this.position or this.position == (-1,-1): return
        x = this.position[0] ; y = this.position[1]
        paper.drawCircle(x,y,State.RADIUS)
        if len(this.successors) == 0:  ## i.e., final state  (=> doubled circle).
            paper.drawCircle(x,y,State.RADIUS-1)
        paper.drawText(x,y,this.name)


##--------------------------------------------------------------------------------------
##
## DFAState specialises State to add a stateSet field (which is supposed to hold a
## StateSet of NFA states.  For a DFA that is generated by the DFA subset algorithm,
## this field can hold the set of NFA states that is associated with each DFA state
## generated by it.


class DFAState(State):
    "Represents a state in a DFA."
    def __init__(this,name):
        State.__init__(this,name,None,None)
        this.stateSet = None
    
