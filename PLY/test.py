import ply.lex as lex
import ply.yacc as yacc
import sys

tokens = [
    'PROGRAM',
    'ID',
    'INT',
    'FLOAT',
    'PLUS',
    'MINUS',
    'DIVIDE',
    'MULTIPLY',
    'LPAREN',
    'RPAREN',
    'EQUAL',
    'LESS',
    'GREATER',
    'COMMA',
    'CTE_INT',
    'CTE_FLOAT',
    'CTE_STRING',
    'LBRACKET',
    'RBRACKET',
    'DO',
    'WHILE',
    'IF',
    'ELSE',
    'COUT',
    'NOTEQUAL',
    'END',
    'VAR',
    'COLON',
    'EOL'
]

t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_EQUAL = r'\='
t_LESS = r'\<'
t_GREATER = r'\>'
t_COMMA = r'\,'
t_RBRACKET = r'\}'
t_NOTEQUAL = r'\!='
t_COLON = r'\:'
t_EOL = r'\;'

t_ignore = r' '

def t_CTE_FLOAT(t):
    r'-?\d+\.\d+'
    t.value = float(t.value)
    return t


def t_CTE_INT(t):
    r'-?\d+'
    t.value = int(t.value)
    return t

# JUMPS STRUCTURE

# IN PENDING JUMPS
# TYPE OF JUMP | EMPTY | EMPTY

# IN JUMPS
# TYPE OF JUMP | POSITION IN QUADRUPLES | EMPTY

# IN QUADRUPLES
# TYPE OF JUMP | CONDITION (OPTIONAL) | WHERE SHOULD IT JUMP

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    global tokens
    if t.value.upper() in tokens:
        tok = t.value.upper()
        if tok == 'IF':
            pending_jumps.append(['goToF', '', ''])
        elif tok == 'ELSE':
            quadruple_array.append(['goTo', '', ''])
            print(operands_stack)
            process_jump()
            jumps.append(['goTo', len(quadruple_array), ''])
        elif tok == 'DO':
            breadcrumb_stack.append(len(quadruple_array) + 1)
        elif tok == 'WHILE':
            pending_jumps.append(['goToT', '', ''])
        t.type = t.value.upper()
    return t

def t_LBRACKET(t):
    r'\{'
    if len(pending_jumps) and len(operands_stack):
        jump = pending_jumps.pop()
        quadruple_array.append([jump[0], operands_stack.pop(), ''])
        jump[1] = len(quadruple_array)
        jumps.append(jump)
    return t

def t_CTE_STRING(t):
    r'\"(.+?)\"'
    return t

def t_error(t):
    # print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


var_table = {}
var_stack = []
var_line = []
rel_ops = {'>', '<', '!=', '='}
ar_ops = {'+', '-', '*', '/'}
hierarchy_table = {'*': 1, '/': 1, '+': 2, '-': 2, '<': 3, '>': 3, '!=': 3, '=': 4}
temp_count = 0
env = {}
quadruple_array = []
operator_stack = []
operands_stack = []
pending_jumps = []
jumps = []
token_list = []
breadcrumb_stack = []

counter = 1

def process_jump():
    jump = jumps.pop()
    if len(breadcrumb_stack) and jump[0] == 'goToT':
        quadruple_array[jump[1] - 1][2] = breadcrumb_stack.pop()
    else:
        quadruple_array[jump[1] - 1][2] = len(quadruple_array) + 1


def get_operation_result_type(a_type, b_type, operation):
    if operation in rel_ops:
        return 'int'
    return a_type if a_type == b_type else 'float'


def add_to_var_stack(step):
    if step is None or step.upper in tokens:
        return
    if step in var_stack:
        print("ERROR: VARIABLE '" + step + "' HAS MULTIPLE DECLARATIONS")
        exit()
    var_stack.append(step)


def process_var_stack():
    # if not correct_syntax:
    #     return False
    types = {'int', 'float'}
    while len(var_stack):
        step = var_stack.pop()
        if step in types:
            var_type = step
            for i in var_line:
                var_table[i] = var_type
            var_line.clear()
            continue
        else:
            var_line.append(step)
    print(var_table)
    return True


def solve_operation():
    global temp_count
    right_operand = operands_stack.pop()
    left_operand = operands_stack.pop()
    operator = operator_stack.pop()
    temp_count = temp_count + 1
    new_temp_var = 'T' + str(temp_count)
    quadruple_array.append([operator, left_operand, right_operand, new_temp_var])
    operands_stack.append(new_temp_var)

def print_quadruples():
    print('----- QUADRUPLES -----')
    for quadruple in quadruple_array:
        print(quadruple)
    print('----------------------')



def flatten_list(items):
    flattened_list = []
    for item in items:
        if isinstance(item, list):
            flattened_list.extend(item)
        else:
            flattened_list.append(item)
    return flattened_list

def print_quadruples():
    print('----- QUADRUPLE LIST -----')
    for quadruple in quadruple_array:
        print(quadruple)
    print('--------------------------')

def p_programa(p):
    '''
    programa : PROGRAM ID EOL programa1
    '''



def p_programa1(p):
    '''
    programa1 : body END
                | vars body END
    '''


def p_vars(p):
    '''
    vars : VAR ID vars1
    '''
    add_to_var_stack(p[2])


def p_vars1(p):
    '''
    vars1 : COMMA ID vars1
            | COLON type
    '''
    if len(p) == 4:
        add_to_var_stack(p[2])

def p_vars2(p):
    '''
    vars2 : ID vars1
    '''
    add_to_var_stack(p[1])


def p_vars3(p):
    '''
    vars3 : vars2
        |
    '''

def p_type(p):
    '''
    type : INT EOL vars3
        | FLOAT EOL vars3
    '''
    var_stack.append(p[1])

def p_body(p):
    '''
    body : LBRACKET body1 RBRACKET
    '''

def p_body1(p):
    '''
    body1 : statement body1
        |
    '''


def p_statement(p):
    '''
    statement : assign
                | condition
                | cycle
                | print
    '''

def p_print(p):
    '''
    print : COUT LPAREN print1
    '''

def p_print1(p):
    '''
    print1 : expression print2
        | CTE_STRING print2
    '''

def p_print2(p):
    '''
    print2 : COMMA print1
        | RPAREN EOL
    '''

def p_assign(p):
    '''
    assign : ID EQUAL expression EOL
    '''
    operands_stack.append(p[1])
    operator_stack.append(p[2])
    if p[1] not in var_stack:
        print("ERROR: UNDECLARED VARIABLE '" + p[1] + "' USED")
        exit()
    solve_operation()

def p_expression(p):
    '''
    expression : exp expression1
    '''


# JUMPS STRUCTURE

# IN PENDING JUMPS
# TYPE OF JUMP | EMPTY | EMPTY

# IN JUMPS
# TYPE OF JUMP | POSITION IN QUADRUPLES | EMPTY

# IN QUADRUPLES
# TYPE OF JUMP | CONDITION (OPTIONAL) | WHERE SHOULD IT JUMP

def p_expression1(p):
    '''
    expression1 : GREATER exp
                | LESS exp
                | NOTEQUAL exp
                |
    '''
    if len(p) > 2:
        operator_stack.append(p[1])
        solve_operation()

def p_cycle(p):
    '''
    cycle : DO body WHILE LPAREN expression RPAREN EOL
    '''
    jump = pending_jumps.pop()
    jump[1] = operands_stack.pop()
    quadruple_array.append(jump)
    jumps.append([jump[0], len(quadruple_array), ''])
    process_jump()

def p_condition(p):
    '''
    condition : IF LPAREN expression RPAREN body condition1
    '''
    if len(jumps):
        process_jump()


def p_condition1(p):
    '''
    condition1 : ELSE body EOL
                | EOL
    '''


def p_factor(p):
    '''
    factor : LPAREN expression RPAREN
            | PLUS factor1
            | MINUS factor1
            | factor1
    '''
    if len(p) == 3:
        operator_stack(p[1])
        solve_operation()

def p_factor1(p):
    '''
    factor1 : ID
            | cte
    '''
    if p[1] is not None:
        if p[1] not in var_stack:
            print("ERROR: UNDECLARED VARIABLE '" + p[1] + "' USED")
            exit()
        operands_stack.append(p[1])


def p_exp(p):
    '''
    exp : exp1
    '''

def p_exp1(p):
    '''
    exp1 : term PLUS exp1
        | term MINUS exp1
        | term
    '''
    if len(p) == 4:
        operator_stack.append(p[2])
        solve_operation()

def p_term(p):
    '''
    term : term1
    '''

def p_term1(p):
    '''
    term1 : factor MULTIPLY term1
        | factor DIVIDE term1
        | factor
    '''
    if len(p) == 4:
        operator_stack.append(p[2])
        solve_operation()

def p_cte(p):
    '''
    cte : CTE_INT
        | CTE_FLOAT
    '''

def p_error(p):
    print("SYNTAX ERROR FOUND")
    print(p)
    exit()



lexer = lex.lex()
parser = yacc.yacc()



program_string = '''program pt2; var a, b, c, d, e: int; { 
                        if (a > b) {
                            a=b+c;
                        }else{
                            c=b;
                        };
                    } end'''

lexer.input(program_string)

while True:
    tok = lexer.token()
    if not tok: 
        break      # No more input
    print(tok)

parser.parse(program_string)

print_quadruples()


