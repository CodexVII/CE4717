import re
##--------------------------------------------------------------------------
##
## opprec.py:  Simple operator-precedence based expression parsing in 
##             Python.  Uses the "recursive operator precedence" technique
##             from the Kaleidoscope tutorial.  This combines operator
##             precedence with recursive descent.
##
## Use:
##
##         $ idle opprec.py
##
##           >>> p = Parser()
##           >>> ast = p.parse("10 + 20*30**2 - 4")
##           >>> print ast
##           ('-', ('+', '10', ('*', '20', ('**', '30', '2'))), '4')
##           >>> evalAST(ast)
##           18006
##           >>> 10 + 20 * 30**2 -4
##           18006
##           >>> print compileAST(ast)
##            0: Load #10
##            1: Load #20
##            2: Load #30
##            3: Load #2
##            4: Exp
##            5: Mult
##            6: Add
##            7: Load #4
##            8: Sub
##            9: Halt
##
##
##           >>> 
##
##
## Note that '**' is a right-associative operator, all the others
## are left-associative.
##
##           >>> print p.parse("2**2**3")
##           ('**', '2', ('**', '2', '3'))
##           >>> print p.parse("2**(2**3)")
##           ('**', '2', ('**', '2', '3'))
##           >>> print p.parse("(2**2)**3")
##           ('**', ('**', '2', '2'), '3')
##           >>> evalAST(p.parse("2**2**3"))
##           256
##           >>> print 2**2**3, 2**(2**3), (2**2)**3
##           256 256 64
##
##
## Implementation:  the scanner code comes first, followed by the 
## parser, finally the AST evaluator.
##
###
## The AST representation is simple, just a tuple or string representing an
## integer.
##
## If the AST is a tuple, it is either a 2-tuple or a 3-tuple.
## A 2-tuple holds a unary operator, the first argument is the operator 
## (as a string), the second is an AST.  
##
## If the AST is a 3-tuple, this is the AST of a binary operator,
## operator first, as argument [0], a string, and the two operands as
## arguments [1] and [2], both ASTs.
##
##          Input      AST
## E.g.     10         '10'
##          -2         ('-', '2')
##          2+3        ('+', '2', '3')
##          2+3*4      ('+', '2', ('*', '3', '4'))
##
##
## The parser has three routines.  Routine "parse" is designed to get
## the parse going at the top-level (and within any nested parentheses).
## Routine "parsePrimary" is designed to parse nonterminals.  Finally
## routine "opPrecParse" handles the details of applying operators
## in correct precedence order.
##
## "parse" is always called first.  It expects to see the left-hand
## side of an expression appearing in the input.  It calls 
## "parsePrimary" to handle this recursively, returning an AST, then
## calls "opPrecParse" to handle operators.  "opPrecParse" is passed
## the AST just parsed, along with a precedence for the expression
## parsed so far.
##
##
##--------------------------------------------------------------------------
## A simple scanner.  Splits input into identifiers, integers and
## operators +, -, *, / and ** using the re module.
## Ignores whitespace.
##
class Scanner(object):
    regexpr = r'[A-Za-z][A-Za-z0-9]*|[0-9]+|[-+/]|[*]+|\(|\)'
    
    def __init__(this,string):
        this.tlist = re.findall(Scanner.regexpr,string)
        this.pos = 0

    def currTok(this):
        if this.pos < len(this.tlist): return this.tlist[this.pos]
        else: return '<eof>'

    def nextTok(this):
        this.pos += 1

    def getTok(this):
        token = this.currTok()
        this.nextTok()
        return token

    def reset(this):
        this.pos = 0
        

##--------------------------------------------------------------------------
## An operator-precedence parser.
##
class Parser(object):
    ## Precedence table for operators.
    prectab = {'**': 30, '*': 20, '/': 20, '+': 10, '-': 10}

    ## The set of right-associative operators.
    r_assoc_set = { '**' }
    

    ##def __init__(this,string):
    ##    this.scanner = Scanner(string)

    ## Note: return -1 for anything that isn't an operator.       
    def prec(this,op):
        return Parser.prectab.get(op,-1)

    ## Routine to check if an operator is right-associaitve.
    def rassoc(this,op):
        return op in this.r_assoc_set
    
    ## Main parser kicks off from this routine.
    def parse(this,string=None):
        if string != None: this.scanner = Scanner(string)
        return this.opPrecParse(this.parsePrimary(),0)

    ## Parse a "primary expression", the thing on the left-
    ## or right-hand side of a binary operator.  This can be
    ## either: (a) A parenthesized subexpression;
    ##         (b) A unary minus in front of a primary expression, or;
    ##         (c) A simple name or number (e.g., a, 10);
    ##
    def parsePrimary(this):
        tok = this.scanner.currTok()
        this.scanner.nextTok()
        if tok == '(':          ## (a), parenthesized subxpression
            ast = this.parse()
            if this.scanner.getTok() != ')': raise RuntimeError, "')' expected" 
            else: return ast
        elif tok == '-':        ## (b), unary minus
            return ('-',this.parsePrimary())
        else:                   ## (c), anything else (name or number)
            return tok

    ## The operator-precedence part of the parser. 'op1' is 'binop' and
    ## 'op2' is implicit (this.scanner.currTok).  Note the handling
    ## of right-associative operators.  For right-association, 'op1'
    ## and 'op2' have to be the same, and 'op1' has to be right-
    ## associative.  This is quite a restrictive verion of right-
    ## associativity.
    ##
    ## Note in particular the precedence associated with the recursive
    ## call for a higher-precedence op2 (prec(op1)+1) and for a
    ## right-associative op1/op2 (prec(op1)).  This is important!
    ##
    def opPrecParse(this,lhs,exprPrec):
        while this.prec(this.scanner.currTok()) >= exprPrec:
            binop = this.scanner.getTok()
            rhs = this.parsePrimary()
            if this.prec(binop) < this.prec(this.scanner.currTok()):
                rhs = this.opPrecParse(rhs,this.prec(binop)+1)
            elif binop == this.scanner.currTok() and this.rassoc(binop):
                rhs = this.opPrecParse(rhs,this.prec(binop))
            lhs = (binop,lhs,rhs)
        return lhs
            


##--------------------------------------------------------------------------
## An AST evaluator for expressions.  Assumes all the terminal nodes of
## the AST are integers.  Non-terminal nodes are either 2-tuples or
## 3-tuples.  2-tuples correspond to unary operators (<op-str> <ast>),
## 3-tuples to binary operators (<op-str> <left-ast> <right-ast>).
##
def evalAST(ast):
    if isinstance(ast,tuple):
        if len(ast) == 3:   ## Binary operator tuple
            binop = ast[0]
            lval = evalAST(ast[1])
            rval = evalAST(ast[2])
            if binop == '*':     return lval*rval
            elif binop == '/':   return lval/rval
            elif binop == '+':   return lval+rval
            elif binop == '-':   return lval-rval
            elif binop == '**':  return lval**rval ## math.pow(lval,rval)
            else:
                raise RuntimeError, "Unknown binary operator %s" % binop
        elif len(ast) == 2:  ## Unary operator tuple, only unary minus.
            if ast[0] == '-':
                return -evalAST(ast[1])
            else:
                raise RuntimeError, "Unknown unary operator %s" % ast[0]
    else:
        return int(ast)
        

def compileAST(ast,toplevel=True,codeGen=None):
    """Compile an AST represented as a tuple of nested tuples into stack-
       machine assembly code and return as a string suitable for loading
       by the stack-machine simulator."""
       
    class CodeGenerator(object):
        """Keeps track of instruction addresses."""
        def __init__(self):
            self.instructions = ""
            self.addr = 0
        def emit(self,instruction):
            self.instructions += "%2d: %s\n" % (self.addr, instruction)
            self.addr += 1
            
    if toplevel and codeGen == None: codeGen = CodeGenerator()      
    if isinstance(ast,tuple):
        if len(ast) == 3:   ## Binary operator tuple
            binop = ast[0]
            compileAST(ast[1],toplevel=False,codeGen=codeGen)
            compileAST(ast[2],toplevel=False,codeGen=codeGen)
            if binop == '*':     instruction = "Mult"
            elif binop == '/':   instruction = "Div"
            elif binop == '+':   instruction = "Add"
            elif binop == '-':   instruction = "Sub"
            elif binop == '**':  instruction = "Exp"
            else:
                raise RuntimeError, "Unknown binary operator %s" % binop
            codeGen.emit(instruction)
        elif len(ast) == 2:  ## Unary operator tuple, only unary minus.
            if ast[0] == '-':
                codeGen.emit("Neg")
            else:
                raise RuntimeError, "Unknown unary operator %s" % ast[0]
    else:
        codeGen.emit("Load #%d" % int(ast))
    if toplevel:
        codeGen.emit("Halt")
        return codeGen.instructions
