#Eduardo Antonio Beitia Tristan A00829121
import ply.lex as lex
import ply.yacc as yacc

#-------------------------- LEXER -------------------------
# List of token names.   This is always required
tokens = (
   'ISNOT',
   'CTEINT',
   'CTEFLOAT',
   'CTESTRING',
   'EQUAL',
   'OPENP',
   'CLOSEP',
   'PLUS',
   'MINUS',
   'MULTIPLY',
   'DIVIDE',
   'GREATER',
   'LESS',
   'OPENK',
   'CLOSEK',
   'ENDL',
   'COMA',
   'COLON',
   'IF',
   'ELSE',
   'WHILE',
   'PROGRAM',
   'ID',
   'VAR',
   'END',
   'INT',
   'FLOAT',
   'COUT',
   'DO'
   
)

# Regular expression rules for simple tokens
t_ISNOT = r'\!='
t_CTEINT = r'\d+'
t_CTEFLOAT = r'(-)?\d+.\d+'
t_EQUAL = r'\='
t_OPENP = r'\('
t_CLOSEP = r'\)'
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'/'
t_GREATER = r'\>'
t_LESS = r'\<'
t_OPENK = r'\{'
t_CLOSEK = r'\}'
t_ENDL = r'\;'
t_COMA = r'\,'
t_COLON = r'\:'
t_IF = r'if'
t_ELSE = r'else'
t_WHILE = r'while'
t_PROGRAM = r'program'
t_VAR = r'var'
t_END = r'end'
t_INT = r'int'
t_FLOAT = r'float'
t_COUT = r'cout'
t_DO = r'do'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

#Definir cuando es un ID
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    reserved_keywords = ['program', 'if', 'else', 'then', 'cout', 'do', 'while', 'var', 'int', 
                         'float',',', ':', '+', '-', '*', '/', '=', '<', '>', '!=', '"', 'end']
    if t.value in reserved_keywords:
        t.type = t.value.upper()
    return t

def t_CTESTRING( t):
    r'"[^"\n]*"'
    t.value = t.value[1:-1]  # Remove the double quotes from the value
    return t

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

#------------------- PARSER --------------------------
def p_program(p):
    '''program : PROGRAM ID ENDL programPrima'''
    #If everything is accepted - VALID INPUT PARSE IT ALL BABY
    print("\n-----------------------PARSER---------------------------------------\n")
    print("Parser valid input")

    
def p_programPrima(p):
    '''programPrima : body END 
                     | vars body END'''

def p_vars(p):
    '''vars : VAR ID idPrima'''
    addToVarTable(p[2]) #adding id to variable table is a key

def p_idPrima(p):
    '''idPrima : COMA ID idPrima
               | COLON type'''
    addToVarTable(p[2]) #adding id to variable table is a key
    
    
def p_idPrima2(p):
    '''idPrima2 : ID idPrima'''
    addToVarTable(p[1]) #adding id to variable table is a key
    

def p_varsPrima(p):
    '''varsPrima : 
               | idPrima2'''
     
def p_type(p):
    '''type : INT ENDL varsPrima
            | FLOAT ENDL varsPrima'''
    global idTypeValue
    idTypeValue = p[1] #next id value

    
def p_body(p):
    '''body : OPENK bodyPrima CLOSEK'''

def p_bodyPrima(p):
    '''bodyPrima : 
               | statement bodyPrima'''
    
def p_statement(p):
    '''statement : assign
               | condition
               | cycle
               | print'''
    
def p_print(p):
    '''print : COUT OPENP printPrima '''

def p_printPrima(p):
    '''printPrima : expresion expresionPrima
               | CTESTRING expresionPrima'''
    
def p_expresionPrima(p):
    '''expresionPrima : COMA printPrima 
               | CLOSEP ENDL'''
    
def p_assign(p):
    '''assign : ID EQUAL expresion ENDL'''
    global elementStack
    elementStack.append(p[1]) 
    print(p[1])
    global operatorStack
    operatorStack.append(p[2])
    print(p[2])
    addToQuadrupleTable() #llamar funcion quadruple
    
def p_cycle(p):
    '''cycle : DO body WHILE OPENP expresion CLOSEP ENDL'''

def p_expresion(p):
    '''expresion : exp  expresionPrima2'''

def p_expresionPrima2(p):
    '''expresionPrima2 : 
               | GREATER exp
               | LESS exp
               | ISNOT exp'''

def p_condition(p):
    '''condition : IF OPENP expresion CLOSEP body conditionPrima'''

def p_conditionPrima(p):
    '''conditionPrima : ENDL
               | ELSE body ENDL'''
    
def p_factor(p):
    '''factor : factorPrima'''

def p_factorPrima(p):
    '''factorPrima : OPENP expresion CLOSEP
               | factorPrima2
               | PLUS factorPrima2
               | MINUS factorPrima2'''
    if len(p)>2:
        global operatorStack
        operatorStack.append(p[1])
        print(p[1])
        addToQuadrupleTable() #llamar funcion quadruple
        
    
def p_factorPrima2(p):
    '''factorPrima2 : ID
               | cte'''
    if p[1] is not None:
        global elementStack
        elementStack.append(p[1])
        print(p[1])

def p_cte(p):
    '''cte : CTEINT
           | CTEFLOAT'''
    global elementStack
    elementStack.append(p[1])
    print(p[1])

    
def p_exp(p):
    '''exp : termino expPrima'''

def p_expPrima(p):
    '''expPrima : 
           | PLUS exp
           | MINUS exp'''
    if len(p) > 1:
        global operatorStack
        operatorStack.append(p[1])
        print(p[1])
        addToQuadrupleTable() #llamar funcion quadruple

def p_termino(p):
    '''termino : factor terminoPrima'''

def p_terminoPrima(p):
    '''terminoPrima : 
           | MULTIPLY termino
           | DIVIDE termino'''
    if len(p) > 1:
        global operatorStack
        operatorStack.append(p[1])
        addToQuadrupleTable() #llamar funcion quadruple
        print(p[1])

# Error rule for syntax errors
def p_eror(p):
    print("Syntax error in input!")

print("\n--------------------------------------------------------------------\n")

parser = yacc.yacc()

#-------------------- variables table ----------------------
def addToVarTable(p):
    global idTypeValue, variableTable
    if p is None :
        return
    
    if p in variableTable.keys():
        print("\n-------------ERROR---------------")
        print(p,"- ya existe o es una palabra reservada")
        print("-------------ERROR---------------\n")
        exit()
    else:
        variableTable[p] = variableTable.get(p,idTypeValue)

#-------------------- semantic table -----------------------
def getSementicTable():
    dict = {int:["+","-","/","*","<",">","!="],float:[["+","-","/","*"]]}
    print(dict)

#-------------------- quadruple table -----------------------
def addToQuadrupleTable():
    global operationCounter

    operand = operatorStack.pop()

    if operand == '(':
        return

    operationCounter += 1 

    #operand = operatorStack.pop()
    rightValue = elementStack.pop()
    leftValue = elementStack.pop()
    newTempVar = "T" + str(operationCounter)

    tempQuadruple=[operand,leftValue,rightValue,newTempVar]

    elementStack.append(newTempVar)
    quadrupleTable.append(tempQuadruple)

#To create variable table 
idTypeValue=""
variableTable={}

#To create quadruple table
elementStack=[]
operatorStack=[]
operationCounter=0
#gotoJumpStack=[]
quadrupleTable=[]

data1 = '''
    program buenCaso; var a, b: int; c, d: float; {
        b = c/a+d+t*(y+k)/m;
    }end
'''

lexer.input(data1)

# Tokenize
while True:
    tok = lexer.token()
    if not tok: 
        break      # No more input
    print(tok)

# Build the parser
parser.parse(data1)

#print variable Table
print("\n-----------------------VARIABLE TABLE-------------------------------\n")
print(variableTable)

#print semantic table
print("\n-----------------------SEMANTIC TABLE-------------------------------\n")
getSementicTable()

#print quadruple table
print("\n-----------------------QUADRUPLE TABLE------------------------------\n")
print(quadrupleTable)
print("\n--------------------------------------------------------------------\n")

# idTypeValue=""
# variableTable={}

# data2 = '''
# program buenCaso; var  a: int; b: float; { } end
# '''
# lexer.input(data2)

# # Tokenize
# while True:
#     tok = lexer.token()
#     if not tok: 
#         break      # No more input
#     print(tok)

# # Build the parser
# parser.parse(data2)

# #print variableTable
# print("\n-----------------------VARIABLE TABLE-------------------------------\n")
# print(variableTable)

# #print semantic table
# print("\n-----------------------SEMANTIC TABLE-------------------------------\n")
# getSementicTable()
# print("\n--------------------------------------------------------------------\n")

# idTypeValue=""
# variableTable={}

# data3 = '''
# program buenCaso; var  a: int; a: float; { } end
# '''
# lexer.input(data2)

# # Tokenize
# while True:
#     tok = lexer.token()
#     if not tok: 
#         break      # No more input
#     print(tok)

# # Build the parser
# parser.parse(data3)

# #print variableTable
# print("\n-----------------------VARIABLE TABLE-------------------------------\n")
# print(variableTable)

# #print semantic table
# print("\n-----------------------SEMANTIC TABLE-------------------------------\n")
# getSementicTable()
# print("\n--------------------------------------------------------------------\n")