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
#t_OPENK = r'\{'
t_CLOSEK = r'\}'
t_ENDL = r'\;'
t_COMA = r'\,'
t_COLON = r'\:'
#t_IF = r'if'
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
#def t_ID(t):
    #r'[a-zA-Z_][a-zA-Z0-9_]*'
    #reserved_keywords = ['program', 'if', 'else', 'then', 'cout', 'do', 'while', 'var', 'int', 
                         #'float',',', ':', '+', '-', '*', '/', '=', '<', '>', '!=', '"', 'end']
    #if t.value in reserved_keywords:
        #t.type = t.value.upper()
    #return t

def t_CTESTRING( t):
    r'"[^"\n]*"'
    t.value = t.value[1:-1]  # Remove the double quotes from the value
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    global tokens
    if t.value.upper() in tokens:
        tok = t.value.upper()
        if tok == 'IF':
            createJump.append(['GOTOF', '', ''])
        elif tok == 'ELSE':
            quadrupleTable.append(['GOTO', '', ''])
            addJumpToQuadruple()
            jumpStack.append(['GOTO', len(quadrupleTable), ''])
        elif tok == 'DO':
            createJump.append("DO")
        elif tok == 'WHILE':
            createJump.append(['GOTOT', '', ''])
        t.type = t.value.upper()
    return t

def t_OPENK(t):
    r'\{'
    if len(createJump) and createJump[0] == "DO":
        createJump.pop()
        gotoJumpStack.append(len(quadrupleTable)+1)
    elif len(createJump) and len(elementStack):
        nextJump = createJump.pop()
        quadrupleTable.append([nextJump[0], elementStack.pop(), ''])
        nextJump[1] = len(quadrupleTable)
        jumpStack.append(nextJump)
    return t

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE'),
)

#-------------------- quadruple table -----------------------
def addToQuadrupleTable():
    global operationCounter, quadrupleTable, newTempVar

    operand = operatorStack.pop()
    if operand == '(':
        return
    elif operand == 'cout':
        operationCounter += 1 

        value = elementStack.pop()

        newTempVar = "T" + str(operationCounter)
        tempQuadruple=[operand,value,newTempVar]

        elementStack.append(newTempVar)
        quadrupleTable.append(tempQuadruple)
        return

    operationCounter += 1 

    #operand = operatorStack.pop()
    rightValue = elementStack.pop()
    leftValue = elementStack.pop()
    newTempVar = "T" + str(operationCounter)

    tempQuadruple=[operand,leftValue,rightValue,newTempVar]

    elementStack.append(newTempVar)
    quadrupleTable.append(tempQuadruple)

#------------------- PREPARING NEXT JUMP -------------
def addJumpToQuadruple():
    nextJump = jumpStack.pop()
    if len(gotoJumpStack) and nextJump[0] == 'GOTOT':
        quadrupleTable[nextJump[1] - 1][2] = gotoJumpStack.pop()
    else:
        quadrupleTable[nextJump[1] - 1][2] = len(quadrupleTable) + 1

#-------------------- variables table ----------------------
def addToVarTable(p):
    global idTypeValue, variableTable
    if p is None :
        return
    
    if p in variableTable.keys():
        print("\n-------------ERROR---------------")
        print(p,"- is already created")
        print("-------------ERROR---------------\n")
        exit()
    else:
        variableTable[p] = variableTable.get(p,idTypeValue)

#-------------------- semantic table -----------------------
def getSementicTable():
    dict = {int:["+","-","/","*","<",">","!="],float:[["+","-","/","*"]]}
    print(dict)

#-------------------- create memory -----------------------
def createMemory():
    inicialDirection=1000
    for key in variableTable.keys():
        variableDirection[key] = inicialDirection
        memory[inicialDirection] = 0
        inicialDirection += 1
    

#-------------------- VM process function -----------------
def processProgram(quadTable):
    steps = 0
    for quadArray in quadTable:
        if quadArray[0] == '=':
            if quadArray[1] in variableDirection.keys():
                value = memory[variableDirection[quadArray[1]]]
            else:
                value = quadArray[1]
            memory[variableDirection[quadArray[2]]] = value
        elif quadArray[0] == '+':
            firstValue = 0
            secondValue = 0
            if quadArray[1] in variableDirection.keys():
                firstValue = memory[variableDirection[quadArray[1]]]
            else:
                firstValue = quadArray[1]

            if quadArray[2] in variableDirection.keys():
                secondValue = memory[variableDirection[quadArray[2]]]
            else:
                secondValue = quadArray[2]
            
            #print("values: ",firstValue, secondValue)
            result = int(firstValue) + int(secondValue)

            variableDirection[quadArray[3]] = str(1000 + len(variableDirection))
            memory[variableDirection[quadArray[3]]] = result
        elif quadArray[0] == '-':
            firstValue = 0
            secondValue = 0
            if quadArray[1] in variableDirection.keys():
                firstValue = memory[variableDirection[quadArray[1]]]
            else:
                firstValue = quadArray[1]

            if quadArray[2] in variableDirection.keys():
                secondValue = memory[variableDirection[quadArray[2]]]
            else:
                secondValue = quadArray[2]
            
            #print("values: ",firstValue, secondValue)
            result = int(firstValue) - int(secondValue)

            variableDirection[quadArray[3]] = str(1000 + len(variableDirection))
            memory[variableDirection[quadArray[3]]] = result
        elif quadArray[0] == '/':
            firstValue = 0
            secondValue = 0
            if quadArray[1] in variableDirection.keys():
                firstValue = memory[variableDirection[quadArray[1]]]
            else:
                firstValue = quadArray[1]

            if quadArray[2] in variableDirection.keys():
                secondValue = memory[variableDirection[quadArray[2]]]
            else:
                secondValue = quadArray[2]
            
            #print("values: ",firstValue, secondValue)
            result = int(firstValue) / int(secondValue)

            variableDirection[quadArray[3]] = str(1000 + len(variableDirection))
            memory[variableDirection[quadArray[3]]] = result
        elif quadArray[0] == '*':
            firstValue = 0
            secondValue = 0
            if quadArray[1] in variableDirection.keys():
                firstValue = memory[variableDirection[quadArray[1]]]
            else:
                firstValue = quadArray[1]

            if quadArray[2] in variableDirection.keys():
                secondValue = memory[variableDirection[quadArray[2]]]
            else:
                secondValue = quadArray[2]
            
            #print("values: ",firstValue, secondValue)
            result = int(firstValue) * int(secondValue)

            variableDirection[quadArray[3]] = str(1000 + len(variableDirection))
            memory[variableDirection[quadArray[3]]] = result
        elif quadArray[0] == '>':
            firstValue = 0
            secondValue = 0
            if quadArray[1] in variableDirection.keys():
                firstValue = memory[variableDirection[quadArray[1]]]
            else:
                firstValue = quadArray[1]

            if quadArray[2] in variableDirection.keys():
                secondValue = memory[variableDirection[quadArray[2]]]
            else:
                secondValue = quadArray[2]
            
            result = int(firstValue) > int(secondValue)
            
            if result == True:
                variableDirection[quadArray[3]] = str(1000 + len(variableDirection))
                memory[variableDirection[quadArray[3]]] = 1
            else:
                variableDirection[quadArray[3]] = str(1000 + len(variableDirection))
                memory[variableDirection[quadArray[3]]] = 0
        elif quadArray[0] == '<':
            firstValue = 0
            secondValue = 0
            if quadArray[1] in variableDirection.keys():
                firstValue = memory[variableDirection[quadArray[1]]]
            else:
                firstValue = quadArray[1]

            if quadArray[2] in variableDirection.keys():
                secondValue = memory[variableDirection[quadArray[2]]]
            else:
                secondValue = quadArray[2]
            
            result = int(firstValue) < int(secondValue)
            
            if result == True:
                variableDirection[quadArray[3]] = str(1000 + len(variableDirection))
                memory[variableDirection[quadArray[3]]] = 1
            else:
                variableDirection[quadArray[3]] = str(1000 + len(variableDirection))
                memory[variableDirection[quadArray[3]]] = 0
        elif quadArray[0] == '!=':
            firstValue = 0
            secondValue = 0
            if quadArray[1] in variableDirection.keys():
                firstValue = memory[variableDirection[quadArray[1]]]
            else:
                firstValue = quadArray[1]

            if quadArray[2] in variableDirection.keys():
                secondValue = memory[variableDirection[quadArray[2]]]
            else:
                secondValue = quadArray[2]
            
            result = int(firstValue) != int(secondValue)
            
            if result == True:
                variableDirection[quadArray[3]] = str(1000 + len(variableDirection))
                memory[variableDirection[quadArray[3]]] = 1
            else:
                variableDirection[quadArray[3]] = str(1000 + len(variableDirection))
                memory[variableDirection[quadArray[3]]] = 0
        elif quadArray[0] == 'GOTOF':
            value = 0
            if quadArray[1] in variableDirection.keys():
                value = memory[variableDirection[quadArray[1]]]
            else:
                value = quadArray[1]

            if int(value) == 0:
                processProgram(quadTable[int(quadArray[2]-1):])
                return 
        elif quadArray[0] == 'GOTO':
            processProgram(quadTable[int(quadArray[2]-1):])
            return
        elif quadArray[0] == 'cout':
            print(memory[variableDirection[quadArray[1]]])
        steps += 1

#------------------- PARSER -------------------------------
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
    if len(p) == 4 :
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
    operatorStack.append(p[1])
    addToQuadrupleTable()

def p_printPrima(p):
    '''printPrima : expresion expresionPrima
               | CTESTRING expresionPrima'''
    
def p_expresionPrima(p):
    '''expresionPrima : COMA printPrima 
               | CLOSEP ENDL'''
    
def p_assign(p):
    '''assign : ID EQUAL expresion ENDL'''
    elementStack.append(p[1]) 
    operatorStack.append(p[2])
    addToQuadrupleTable() #llamar funcion quadruple
    
def p_cycle(p):
    '''cycle : DO body WHILE OPENP expresion CLOSEP ENDL'''
    nextJump = createJump.pop()
    nextJump[1] = elementStack.pop()
    quadrupleTable.append(nextJump)
    jumpStack.append([nextJump[0], len(quadrupleTable), ''])
    addJumpToQuadruple()

def p_expresion(p):
    '''expresion : exp  expresionPrima2'''

def p_expresionPrima2(p):
    '''expresionPrima2 : 
               | GREATER exp
               | LESS exp
               | ISNOT exp'''
    if len(p) > 2:
        operatorStack.append(p[1])
        addToQuadrupleTable() #llamar funcion quadruple

def p_condition(p):
    '''condition : IF OPENP expresion CLOSEP body conditionPrima'''
    if len(jumpStack):
        addJumpToQuadruple()

def p_conditionPrima(p):             
    '''conditionPrima : ELSE body ENDL
               | ENDL '''
    
def p_factor(p):
    '''factor : OPENP expresion CLOSEP
               | factorPrima2
               | PLUS factorPrima2
               | MINUS factorPrima2'''
    if len(p)>2:
        operatorStack.append(p[1])
        addToQuadrupleTable() #llamar funcion quadruple

#def p_factorPrima(p):
    
def p_factorPrima2(p):
    '''factorPrima2 : ID
               | cte'''
    if p[1] is not None:
        elementStack.append(p[1])

def p_cte(p):
    '''cte : CTEINT
           | CTEFLOAT'''
    elementStack.append(p[1])

    
def p_exp(p):
    '''exp : expPrima'''

def p_expPrima(p):
    '''expPrima : termino PLUS exp
                | termino MINUS exp
                | termino'''
    if len(p) > 2:
        operatorStack.append(p[2])
        addToQuadrupleTable() #llamar funcion quadruple

def p_termino(p):
    '''termino : terminoPrima'''

def p_terminoPrima(p):
    '''terminoPrima : factor MULTIPLY termino
           | factor DIVIDE termino
           | factor '''
    if len(p) > 2:
        operatorStack.append(p[2])
        addToQuadrupleTable() #llamar funcion quadruple

# Error rule for syntax errors
def p_eror(p):
    print("Syntax error in input!")

print("\n--------------------------------------------------------------------\n")

parser = yacc.yacc()

#---------------------------CASO 1--------------------------------------

#--------------------------VARIABLES------------------------------------
#To create variable table 
idTypeValue=""
variableTable={}

#To create quadruple table including jumps
elementStack=[]
operatorStack=[]
operationCounter=0
gotoJumpStack=[]
quadrupleTable=[]
newTempVar=""
createJump=[]
jumpStack=[]

#To create memory and VM process
memory = {}
variableDirection = {}

#---------------------------INPUT--------------------------------------
data1 = '''
    program buenCaso; var a, b: int; {
        a = 6;
        b = 7;
        if(a>b){
            cout(b-a);
        }else{
            cout(a);
        };
        cout(b);
    }end
'''

lexer.input(data1)

# Tokenize
# while True:
#     tok = lexer.token()
#     if not tok: 
#         break      # No more input
#     print(tok)

# Build the parser
parser.parse(data1)

#print variable Table
#print("\n-----------------------VARIABLE TABLE-------------------------------\n")
#print(variableTable)

#print semantic table
#print("\n-----------------------SEMANTIC TABLE-------------------------------\n")
#getSementicTable()

#print quadruple table
print("\n-----------------------QUADRUPLE TABLE------------------------------\n")
for quadruple in quadrupleTable:
    print(quadruple, "\n")
print("\n--------------------------------------------------------------------\n")

createMemory()

print("-------------------------VM-RESULT----------------------------------")
processProgram(quadrupleTable)
print(variableDirection)
print(memory)




