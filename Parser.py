import ply.yacc as yacc
from scanner import tokens, lexer

# AST node creation helpers (updated naming)
def create_func_def(name, params, body):
    return {'type': 'func', 'name': name, 'params': params, 'stmt': body}  # 'func' not 'func_def'

def create_val_def(name, expr):
    return {'type': 'val', 'name': name, 'stmt': expr}

def create_let(decls, body):
    return {'type': 'let', 'bindings': decls, 'body': body}  # 'let' not 'stm_let', 'bindings' not 'facts'

def create_binop(left, op, right):
    return {'type': 'op', 'op': op, 'left': left, 'right': right}  # 'op' not 'binop'

# Precedence rules (lowest to highest)
precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'EQUAL', 'LESS_THAN', 'GREATER_THAN'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('left', 'DOT'),
)

# Grammar rules
def p_program(p):
    '''program : statements'''
    p[0] = {'type': 'program', 'statements': p[1]}

def p_statements(p):
    '''statements : statement
                  | statements statement'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

def p_statement(p):
    '''statement : function_def
                 | exec_statement
                 | let_statement'''
    p[0] = p[1]

def p_function_def(p):
    '''function_def : FUNC ID LBRACE params RBRACE ASSIGN expression END'''
    p[0] = create_func_def(p[2], p[4], p[7])

def p_let_statement(p):
    '''let_statement : LET declarations END IN expression END'''
    p[0] = create_let(p[2], p[5])

def p_exec_statement(p):
    '''exec_statement : EXEC expression'''
    p[0] = {'type': 'exec', 'expression': p[2]}  # 'exec' not 'exec_stmt'

def p_params(p):
    '''params : ID
              | params COMMA ID
              | '''
    if len(p) == 1:
        p[0] = []
    elif len(p) == 2:
        p[0] = [{'type': 'param', 'name': p[1]}]  # 'name' not 'id'
    else:
        p[0] = p[1] + [{'type': 'param', 'name': p[3]}]

def p_declarations(p):
    '''declarations : declaration
                    | declarations declaration
                    | '''
    if len(p) == 1:
        p[0] = {}
    elif len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = {**p[1], **p[2]}

def p_declaration(p):
    '''declaration : VAL ID ASSIGN expression'''
    p[0] = {p[2]: create_val_def(p[2], p[4])}

def p_expression(p):
    '''expression : term
                  | expression PLUS term
                  | expression MINUS term
                  | expression LESS_THAN term
                  | expression GREATER_THAN term
                  | expression EQUAL term
                  | expression AND term
                  | expression OR term
                  | expression DOT term
                  | let_expression
                  | if_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = create_binop(p[1], p[2], p[3])

def p_if_expression(p):
    '''if_expression : IF expression THEN expression ELSE expression END'''
    p[0] = {'type': 'if', 'cond': p[2], 'then': p[4], 'else': p[6]}  # 'if' not 'if_expr'

def p_let_expression(p):
    '''let_expression : LET declarations END IN expression END'''
    p[0] = create_let(p[2], p[5])

def p_term(p):
    '''term : factor
            | term TIMES factor
            | term DIVIDE factor'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = create_binop(p[1], p[2], p[3])

def p_factor_unary(p):
    '''factor : MINUS factor'''
    p[0] = {'type': 'unary_minus', 'expr': p[2]}

def p_factor(p):
    '''factor : ID
              | NUMBER
              | STRING
              | TRUE
              | FALSE
              | NIL
              | LPAREN expression RPAREN
              | ID LBRACE args RBRACE'''
    if len(p) == 2:
        token_type = p.slice[1].type
        
        if token_type == 'NUMBER':
            p[0] = {'type': 'value', 'kind': 'number', 'value': p[1]}  # 'kind' not 'type_value'
        elif token_type == 'STRING':
            p[0] = {'type': 'value', 'kind': 'string', 'value': p[1]}
        elif token_type == 'TRUE':
            p[0] = {'type': 'value', 'kind': 'boolean', 'value': True}
        elif token_type == 'FALSE':
            p[0] = {'type': 'value', 'kind': 'boolean', 'value': False}
        elif token_type == 'NIL':
            p[0] = {'type': 'value', 'kind': 'nil', 'value': None}
        elif token_type == 'ID':
            p[0] = {'type': 'id', 'name': p[1]}  # 'name' not 'value'
        else:
            p[0] = {'type': 'id', 'name': p[1]}
    elif len(p) == 4:
        p[0] = p[2]
    else:  # function call: ID LBRACE args RBRACE
        p[0] = {'type': 'call', 'func': p[1], 'args': p[3]}  # 'func' not 'id'

def p_args(p):
    '''args : expression
            | args COMMA expression
            | '''
    if len(p) == 1:
        p[0] = []
    elif len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}', line {p.lineno}")
    else:
        print("Syntax error at EOF")

# Create parser
parser = yacc.yacc()

# Function to parse a file and return AST
def parse_file(filename):
    with open(filename, 'r') as f:
        data = f.read()
    result = parser.parse(data, lexer=lexer)
    return result
