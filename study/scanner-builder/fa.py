##--------------------------------------------------------------------------------------
##
## Class FA represents a finite automaton, either an NFA or a DFA. The idea is that 
## this class acts as an abstract base class implementing common functionality, and 
## that derived classes (nondeterministic like PrimitiveNFA, ChoiceNFA, CompositeNFA,
## ClosureNFA and OuterChoiceNFA) or DFA (deterministic) do the actual work of
## generating state machines.
##
## Internally an automaton is represented as a graph of State objects, accessable
## from its head via the "startState" instance variable.
## 
## Instance variables:
##
##    startState:  The first state in the automaton, a State object.
##
##    finalStates: The final (i.e., accepting) state(s) in the automaton, a list of 
##                 State objects.  N.B. this list will always be of length 1 for
##                 Thompson NFAs, it may have more than one element in a non-
##                 Thompson NFA or a DFA.
##
##    regExprs:    The RE(s) recognised by this NFA, a list of strings (in one-to-
##                 one correspondence with the States in finalStates) that
##                 represent the regular expressions corresponding with each
##                 accepting state in the finalStates list
##
##     width:      The width of the graphical representation of this automaton
##                 in the internal millimetre-based co-ordinate system.  The
##                 (implicit) assumption is that we are using a millimetre-based
##                 co-ordinate system (x across, y up), with states represented
##                 graphically as circles with diameter 10mm.
##                 The Python representation of a position is as a 2-tuple of 
##                 integers, (x,y).  If this is 0, it means that the automaton
##                 has no graphical representation
##
##     height:     The height of this automaton.  If 0, the automaton has no
##                 graphical representation.
##
##     alphabet:   A set of characters: the input alphabet of this automaton
##
##     stateCount: The number of states in this automaton.
##
##
## Methods:
##
##     listStates:   Return a list of States in this automaton.
##
##     output_plain: Generate a plain representation of this automaton on
##                   standard output.  The grammar for the representation is
##
##                   <plain-output>         :== <title-line>
##                                              <state-count-line>
##                                              <start-state-line>
##                                              [<nfa-type-line>]
##                                              <accepting-state-line>
##                                              {<accepting-state-line>}
##                                              <state> {<state>}
##                   <title-line>           :== ("NFA" | "DFA") "\n"
##                   <state-count-line>     :== "States:" <integer> "\n"
##                   <start-state-line>     :== "Start State:" <name> "\n"
##                   <nfa-type-line>        :== ("Thompson NFA" | "Non-Thompson NFA") "\n"
##                   <accepting-state-line> :== "  Accepting State:" <name> <regExpr> "\n"
##                   <state>                :== "State" <name> "\n"
##                                              <transition> {<transition>}
##                   <transition>           :== "    (" <char> ") --> " <name>
##                   <integer>              :== unsigned integer
##                   <name>                 :== a printable string
##                   <char>                 :== "eps" | a single character
##
##     output_dot:   Generate a representation of the automaton in the Graphviz
##                   graph-description language.  Outputs to sys.stdout.
##
##     output_table: Generate a tabular-representation of the automaton. Takes a
##                   single optional parameter "latex", if False (the default)
##                   this method just generates a plain-text table, if True, it
##                   generates a table in LaTeX source form.
##
##
##
## Support methods (don't call these directly from the user level):
##
##     draw:         Display an FA on an output device (passed in as the single
##                   parameter "paper"), this can be a Tk display, a PostScript
##                   output file, etc.  Don't call this directly from the user
##                   level, it is normally called by a TkDrawing obejct or a
##                   PostScript object, these do the setting up of the display
##                   surface before calling this routine.  (N.B., in terms of
##                   design patterns, "draw" is a Bridge pattern -- with the
##                   other side of the Bridge being provided by the device-
##                   specific methods in the various display surface objects
##                   that call draw.  The idea is that "draw" provides a
##                   device-independent way to traverse the FA graph, and
##                   the device-specific objects provide implementations
##                   of drawing methods called by the "abstract" drawing
##                   methods called through "draw").
##
##     showREcolumn: Instructs the table-drawing routines whether or not to
##                   include a column containing the regular expressions
##                   associated with accepting states.  Defaults to True.
##
##     re2tex:       Converts a regular-expression string to TeX-format.
##
## 
from state import State
from connector import *

class FA(object):
    "Base class for all automata, deterministic and non-deterministic."

    EPS = 'eps' ## Representing null characters.

    def __init__(this):
        "N.B. Usually not a good idea to instance this directly!"
        this.startState = None
        this.finalStates = []
        this.regExprs = []
        this.width = 0
        this.height = 0
        this.stateCount = 0
        this.alphabet = set([])

        
    def listStates(this):
        "Return a list of all the unique states in this automaton."
        stateList = [this.startState]
        index = 0
        namesAreIntegers = True
        namesArePrefixedIntegers = True
        while index < len(stateList):
            state = stateList[index] ; index += 1
            if namesAreIntegers:
                namesAreIntegers = isinstance(state.name,int) or state.name.isdigit()
            elif namesArePrefixedIntegers:
                namesArePrefixedIntegers = state.name[1:].isdigit()
            for successor in state.successors: 
                nextState = successor[1]  ## Field 1 of the successor tuple is the next State object.
                if not nextState in stateList: stateList.append(nextState)
        ## Sort the list of states by name (if it is longer than 1).
        if len(stateList) > 1:
            if namesAreIntegers: 
                stateList.sort(lambda s1,s2: int(s1.name) - int(s2.name))
            elif namesArePrefixedIntegers:
                stateList.sort(lambda s1,s2: int(s1.name[1:]) - int(s2.name[1:]))
            else:
                stateList.sort(lambda s1,s2: cmp(s1.name,s2.name))
        return stateList


    def output_plain(this,title,nfa_type=None):
        "Print out the automaton in plain textual format."
        print title              ## "title" should be "NFA" or "DFA" 
        print "States:", this.stateCount
        print "Start state:", this.startState.name
        if nfa_type: print nfa_type ## Should be "Thompson NFA", "Non-Thompson NFA" for an NFA.
        for i in range(len(this.finalStates)):
            print "  Accepting state: %2s  '%s'" % (this.finalStates[i].name,this.regExprs[i])
        for state in this.listStates():
            if len(state.successors) > 0:
                print "State", state.name
                for transition in state.successors:
                    print "    (%s) --> %s" % (transition[0],transition[1].name)


    ##
    ## "output_dot" generates a DOT-language description of the graph.  Note that it uses
    ## "edge-coalescing", i.e., if there are two or more arcs with differing labels between
    ## a pair of nodes, this routine collapses them into a single arc with a label that
    ## is the "or" of all the individual arc labels.  For example, if there are arcs
    ## between nodes A and B with labels "a", "b" and "c", this routine will generate
    ## a single DOT-arc with label "a|b|c".  This really helps to clear up clutter on the
    ## generated graph.
    ##
    ## The output can be processed using "dot".  The "-Tpdf" flag is used to generate
    ## pdf output ("dot -Tpdf graph.dot > graph.pdf").  Unfortunately, "dot" doesn't
    ## always generate very good looking graphical representations of the graph's
    ## structure (it will be correct, but "messy").  Consider using "twopi" as an
    ## alternative, this sometimes works well.  "circo" is another option, but it
    ## doesn't do a good job of laying out the arcs.
    ##
    def output_dot(this,title):
        """Print out the automaton in the format of the GraphViz graph language, in a.
           form intended for processing by the dot, twopi or circo programs."""
        print "digraph", title, "{"
        print "    rankdir=LR"
        print "    node [shape = doublecircle];",
        for finalState in this.finalStates: print finalState.name,
        print ";\n    node [shape = circle];"
        for state in this.listStates():
            if len(state.successors) > 0:
                found = set([])  ## Target states from current one seen so far.
                for i in range(len(state.successors)):
                    transition = state.successors[i]
                    if not transition[1].name in found:  ## If new, issue dot arc command.
                        found.add(transition[1].name)
                        s = "    %s -> %s" % (state.name,transition[1].name)
                        if transition[0] != FA.EPS:
                            s += ' [ label = "%s' % transition[0]
                            ## Check over remaining transitions to see if any coalescing
                            ## can happen here.
                            for j in range(i+1,len(state.successors)):
                                t2 = state.successors[j]
                                if t2[1].name == transition[1].name:  s += '|%s' % t2[0]
                            s += '" ];'
                        else: s += ";"
                        print s
        print "}"


    def output_table(this,latex=False):
        """Output the automaton in tabular format, if latex is True, output LaTeX code,
           otherwise just plain text. if showREcolumn is True, display a column at the
           right-hand-edge with the regular expressions recognised by the automaton."""
        (row_labels,table,col_labels) = this.__format_table__()
        if latex:  this.__output_tex_formatted_table__(row_labels,table,col_labels)
        else:      this.__output_plain_formatted_table__(row_labels,table,col_labels)

    ##
    ## "__format_table__" generates a 2-d table (a list of lists) representing the next 
    ## states associated with each state/transition-char pair.  Rows represent a specific 
    ## state, columns within the rows a specific state/transition-char entry.  These 
    ## entries are formatted as strings of successor state names (comma-separated).  Note 
    ## that if there are no-transitions out of a state on a particular character, then 
    ## the entry in the table at the intersection of this row and character is the empty 
    ## string.
    ##
    ## If the FA being analysed is an "OuterChoiceNFA", i.e., a composite NFA that is
    ## designed to recognise a number of different regular expressions, then the table
    ## will have an extra column (at the end).  This column will contain, for the
    ## accepting states of the NFA, the regular expressions associated with each such
    ## accepting state.  If the current NFA state is not accepting, this column will
    ## contain the empty string.  Note that if the NFA is *not* an OuterChoice type,
    ## there will not be a column containing regular expression strings (since this
    ## sort of NFA has only a single accepting state and recognises just one regular
    ## expression.  If the FA is a DFA, the regular expression column is always
    ## included.  This is controlled by the method  "showREcolumn()".  The default
    ## implementation of "showREcolumn" in this class returns True, so displaying
    ## a column of REs by default, override this method in subclasses to change this
    ## behavour.
    ##
    ## "__format_table__" also generates a list of all the states (state name strings)
    ## in this FA, and a list of the alpahbet of transition characters (if epsilon is
    ## a valid transition character, it appears at the front of this list, the list
    ## is otherwise sorted alphabetically.
    ##
    ## Thus, if the FA alpahbet has N entries, an OuterChoiceNFA will have rows
    ## with (N+1) columns, any other NFA will have rows with N columns.  In either
    ## case, the list of column labels will have N entries.
    ##
    ## "__format_table__" returns a triple: (row_labels,table,col_labels).
    ##
    ## 
    def __format_table__(this):
        ## First get the FA alphabet into and sort it.  If the alphabet contains
        ## epsilon, ensure that it appears first in the list.
        alphaList = list(this.alphabet)
        alphaList.sort()
        if FA.EPS in alphaList:        ## Ensure that if there is an epsilon
            alphaList.remove(FA.EPS)   ## in the FA alphabet, it appears at
            alphaList.insert(0,FA.EPS) ## the front of the alphaList.
        stateList = this.listStates()
        table = []  ## Table will be a row-column structure (a list of lists) holding targets on state/alpha.
        row_labels = [ str(state.name) for state in stateList ] ## State numbers as strings, one per row.
        ## Iterate over all the states in the NFA to build the 2-d table structure.
        for state in stateList:
            row = []                    ## Current row being built, for this row:
            for alpha in alphaList:     ## Iterate over all the possible transiton chars (i.e., the alphabet).
                slist = []              ## slist will be a list of targets on this state/alpha combination.
                for succ in state.successors: ## We have to look at the successors list to see if there is a
                    if succ[0] == alpha: slist.append(succ[1].name) ## transition out on state/alpha.
                slist.sort()            ## Sort the target list for pretty-looking results.
                s = "" ; first = True   ## Now convert it to a comma-separated string.
                for target in slist:
                    if first: s += str(target); first = False
                    else: s += ", %s" % target
                row.append(s)           ## Put the string in the table indexed by state and alpha.
            if this.showREcolumn():     ## If this is an OuterChoice NFA or a DFA we have to add the RE
                isFinal = False         ## associated with this state if it is an accepting state.
                for i,finalState in enumerate(this.finalStates):
                    if state == finalState:
                        row.append(this.regExprs[i])
                        isFinal = True
                        break
                if not isFinal: row.append("")
            table.append(row)           ## Add the assembled row (all columns for this state) to the table.
        return (row_labels,table,alphaList)


    def showREcolumn(this):
        """Default implementation of showREcolumn: defaults to True.  Override
           this in subclasses to get different table-display behaviour.  If this
           method returns True, it is indicating that the table being generated
           should display a column at its right-hand-side with regular expressions
           associated with accepting states."""
        return True


    def __output_plain_formatted_table__(this,row_labels,table,col_labels):
        ## First calculate the minimum required width for each table column.
        col_labels = [''] + col_labels
        col_widths = [len(s) for s in col_labels]  ## Initial widths from widths of column headers.
        for i,row in enumerate(table):             ## Work over each row.
            row = [row_labels[i]] + row 
            for j in range(len(col_widths)):
                col_widths[j] = max(len(row[j]),col_widths[j])
        header = "      "                                 ## Generate a string for the column
        for i,label in enumerate(col_labels):             ## headers and print it.
            header += label.rjust(col_widths[i]+1) + ' |'
        print header
        linesep = "      "                                        ## Linesep is a divided between
        for widths in col_widths: linesep += (widths+2)*'-' + '+' ## the table header and body.
        print linesep
        for i,row in enumerate(table):             ## Now do the main body of the table
            line = "      "
            row = [row_labels[i]] + row            ## Prepend state name as first column.
            for j in range(len(col_widths)):
                line += row[j].rjust(col_widths[j]+1) + ' |'
            if this.showREcolumn() and row[-1] != '':  ## If this is a DFA or OuterChoice NFA
                line += ' "%s"' % row[-1]              ## may need to add the RE.
            print line
        print linesep
        

    def __output_tex_formatted_table__(this,row_labels,table,col_labels):
        ## Output header for a complete LaTeX2e document.
        print "\\documentclass[12pt,a4paper]{article}"
        print "\\usepackage{times}"
        print "\\begin{document}"
        print "\\thispagestyle{empty}"
        print "\\begin{center}"
        ## Generate and output the TeX table format instructions.
        formatInstrs="\\begin{tabular}{r|"
        for i in range(len(col_labels)): formatInstrs += "|c"
        formatInstrs += "|l}"
        lineSep = "\cline{1-%d}" % (1+len(col_labels))
        print formatInstrs
        ## Convert the formatting of the column headers to TeX style.
        for i in range(len(col_labels)):
            if col_labels[i] == FA.EPS: col_labels[i] = "$\epsilon$"
            else: col_labels[i] = "$%s$" % col_labels[i]
        ## Calculate the minimum required width for each table column.
        col_labels = [''] + col_labels
        col_widths = [len(s) for s in col_labels]  ## Initial widths from widths of column headers.
        for i,row in enumerate(table):             ## Work over each row.
            row = [row_labels[i]] + row 
            for j in range(len(col_widths)):
                col_widths[j] = max(len(row[j]),col_widths[j])
        ## Print the header, with the transition characters
        header = ""
        for i,label in enumerate(col_labels): header += label.rjust(col_widths[i]+1) + ' &'
        header += " \\\\ " + lineSep
        print header
        for i,row in enumerate(table):             ## Now do the main body of the table
            line = ""
            row = [row_labels[i]] + row            ## Prepend state name as first column.
            for j in range(len(col_widths)):
                line += row[j].rjust(col_widths[j]+1) + ' &'
            if this.showREcolumn() and row[-1] != '':          ## If this is a DFA or OuterChoice NFA
                line += " %s" % this.re2tex(row[-1])           ## may need to add the RE.
            line += " \\rule{0pt}{2.4ex}\\\\ %s" % lineSep     ## Note the 2.4ex struct, needed to
            print line                                         ## get decent separation between rows.
        print "\\end{tabular}"
        print "\\end{center}"
        print "\\end{document}"


    def __repr__(this):
        "Return a string representation of this automaton (uses listStates)."
        return str(this.listStates())


    ## display, a PostScript output file, etc.  Don't call this directly from the
    ## user lavel, it is normally called by a TkDrawing obejct or a PostScript
    ## object, these do the setting up of the display device first before calling
    ## this routine.
    def draw(this,paper):
        states = this.listStates()
        for state in states:
            for successors in state.successors:
                connector = successors[2]
                connector.draw(state,paper)
        for state in states: state.draw(paper)


    def re2tex(this,regExpr):
        "Convenience method: return an RE string in TeX format."
        ## 1.  Surround the string in TeX math delimiters ("$")
        re = "$%s$" % regExpr
        ## 2.  Surround all parentheses in TeX grouping braces
        re = re.replace('(','{(').replace(')',')}')
        ## 3.  Replace all *'s with superscripted TeX \ast markers.
        return re.replace('*','^\\ast ')

##-----------------------------------------------------------------------------
##  End of class RE
##-----------------------------------------------------------------------------
##


