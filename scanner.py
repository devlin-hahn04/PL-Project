import ply.lex as lex

# BEGIN LEXICAL ANALYZER DEFINITION

# Creating the token list
tokens = (
    # Basic tokens
    'ID', 'ID_FUNC', 'NUMBER', 'STRING',
    
    # Keywords
    'IF', 'THEN', 'ELSE', 'LET', 'VAL', 'FUNC', 'END',
    'IN', 'NIL', 'TRUE', 'FALSE', 'EXEC',
    
    # Delimiters
    'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE', 'COMMA', 'ASSIGN',
    
    # Operators
    'EQUAL', 'LESS_THAN', 'GREATER_THAN', 'PLUS', 'MINUS', 'TIMES',
    'DIVIDE', 'DOT', 'AND', 'OR'
)

last_line = 1

# Delimiters
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\['
t_RBRACE = r'\]'
t_COMMA = r','

# Operators
t_EQUAL = r'='
t_LESS_THAN = r'<'
t_GREATER_THAN = r'>'
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_DOT = r'\.'
t_AND = r'&'
t_OR = r'\|'

# Complex tokens as functions
def t_ASSIGN(t):
    r':='
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r'\"[^\"]*\"'
    t.value = t.value[1:-1]
    return t

def t_ID(t):
    r'[a-z][a-zA-Z0-9_\']*'
    
    keywords = {
        'if': 'IF',
        'then': 'THEN',
        'else': 'ELSE',
        'let': 'LET',
        'val': 'VAL',
        'func': 'FUNC',
        'end': 'END',
        'in': 'IN',
        'nil': 'NIL',
        'true': 'TRUE',
        'false': 'FALSE',
        'exec': 'EXEC'
    }
    
    if t.value in keywords:
        t.type = keywords[t.value]
    else:
        t.type = 'ID'
    
    return t

def t_ID_FUNC(t):
    r'[A-Z][a-zA-Z0-9_\']*'
    return t

def t_newline(t):
    r'\n+'
    global last_line
    t.lexer.lineno += len(t.value)
    last_line = t.lexer.lineno

t_ignore = ' \t\r'

def t_COMMENT(t):
    r'//.*'
    pass

def t_error(t):
    print(f"Illegal character used '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

# END LEXICAL ANALYZER DEFINITION

if __name__ == "__main__":
    # Test the lexer
    test_file = open('Program_Test.txt', 'r')
    data = test_file.read()
    test_file.close()
    lexer.input(data)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)
