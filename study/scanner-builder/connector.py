##--------------------------------------------------------------------------------------
##
## Connector represents a (graphical) connection between two State objects. 
##
## It's an abstract base class for the two concrete instances, Straight, which is a
## straight-line arrow from the first state to the second, labeled by the transition
## character if this is not "epsilon", and CurvedNFA, which is a curved connection 
## used by NFAs with Kleene closures.
##
import math
from state import State

class Connector(object):
    "Represents a connection between two states."
    pass

##
## Set up a straight-line connector between two states.  Co-ordinates are
## (x,y) tuples, using an assumed millimetre grid and are relative to the
## position of the centre of the start-state.  Note that it is important
## for the co-ordinates in Line objects to be relative to the centre of
## the start-state of the connection, this allows translation of subgraphs
## (when bigger NFAs are being built from smaller ones) without having to
## worry about translation of the co-ordinates within Connector objects, as
## long as the centre-points of all State objects are translated correctly,
## the connectors will move with them because of their relative co-ordinates.
##
class Straight(Connector):
    "A straight-line connection between two states."
    def __init__(this,startState,endState,label=None):
        startPoint = startState.position
        endPoint = endState.position
        dx = endPoint[0] - startPoint[0]
        dy = endPoint[1] - startPoint[1]
        angle = math.atan2(dy, dx)
        this.startPoint = (State.RADIUS * math.cos(angle),
                           State.RADIUS * math.sin(angle))
        this.endPoint = (dx - State.RADIUS * math.cos(angle),
                         dy - State.RADIUS * math.sin(angle))
        this.label = label

    def __repr__(this):
        return "Straight from %s to %s (relative to base point)." %\
               (this.startPoint,this.endPoint)

    ## draw allows this connector to be drawn to a range of output devices,
    ## the specific device being used (Tk canvas, PostScript, etc., is determined
    ## by the "paper" object passed in here).
    def draw(this,state,paper):
        x = state.position[0]
        y = state.position[1]
        dx1 = this.startPoint[0]
        dy1 = this.startPoint[1]
        dx2 = this.endPoint[0]
        dy2 = this.endPoint[1]
        paper.drawArrow(x+dx1,y+dy1,x+dx2,y+dy2)
        if this.label:
            paper.drawText(x+(dx2+dx1)/2.0, y+(dy2+dy1)/2.0, this.label, "above")

##
## CurvedNFA is a curved (spline-based) connector between two states, designed to represent
## the sort of link needed for Kleene-closure NFAs with their long-range forward and
## backward jumps that would cross over exisiting states if draw using Line.  As with
## Line, and for the same reasons, the co-ordinates held by this object are displacements
## relative to a start-state.
##
## A CurvedNFA object contains four points, two end-points, and two spline control points.
## The spline control points define a polygon which bounds the connecting curve. Two
## types of curve are allowed by this class, curves representing "forward jumps"
## go above the NFA they "enclose", and curves representing "backward jumps" go
## below, as in standard NFA closure state diagrams. Parameter "y" passed to this
## classes constructor is the limit in the y-direction that the curve cannot
## exceed.  If this is a forward curve, y should be above the centre-line of the
## enclosed NFA, if a backward curve, y should be beneath it.  The constructor
## checks these conditions using assertions.
##
## At the moment CurvedNFA isn't very sophisticated.  It just constructs a
## bounding-box rectangle with the start and end points as two corners and
## control points made from (start.x,y) and (end.x,y).  It's up to the
## device-specific drawing methods (invoked via "draw", below) to interpret
## these co-ordinates suitably.
##
## Don't change the representation in here to absolute co-ordinates!
##
##
class CurvedNFA(Connector):
    "A curved connection between two NFA states."
    def __init__(this,startState,endState,y):
        ## print "CurvedNFA constructor, building curve from %s to %s, y bound is %d" %\
        ##       (startState.position,endState.position,y)
        startPoint = startState.position
        endPoint = endState.position
        dx = endPoint[0] - startPoint[0]
        dy = y - startPoint[1]
        assert startPoint[1] == endPoint[1]     ## y-coords of start and end state must be the same
        if dx > 0:                              ## forward link
            assert dy > 0                       ## forward link goes up in y-coords
            this.startPoint = (0,State.RADIUS)  ## All movement relative, initally to startPoint.
            this.cp1 = (0,dy-State.RADIUS)      ## relative to this.startPoint move.
            this.cp2 = (dx,0)
            this.endPoint = (0,State.RADIUS-dy)
        else:                                   ## backward link
            assert dy < 0                       ## should go down in y
            this.startPoint = (0,-State.RADIUS) 
            this.cp1 = (0,dy+State.RADIUS)
            this.cp2 = (dx,0)
            this.endPoint = (0,-(dy+State.RADIUS))
        ## print this.startPoint, this.cp1, this.cp2, this.endPoint

    def __repr__(this):
        return "CurvedNFA from %s to %s (control points %s, %s)" % (this.startPoint,
                                                                    this.endPoint,
                                                                    this.cp1,
                                                                    this.cp2)

    ## Draw the curve onthe particular output device passed in as 'paper'.
    def draw(this,state,paper):
        sx = state.position[0] + this.startPoint[0]
        sy = state.position[1] + this.startPoint[1]
        c1x = sx + this.cp1[0]
        c1y = sy + this.cp1[1]
        c2x = c1x + this.cp2[0]
        c2y = c1y + this.cp2[1]
        ex = c2x + this.endPoint[0]
        ey = c2y + this.endPoint[1]
        paper.drawCurve(sx,sy,c1x,c1y,c2x,c2y,ex,ey)
