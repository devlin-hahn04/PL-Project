import ply.lex as lex

# BEGIN LEXICAL ANALYZER DEFINITION

#Creating the token list
tokens = (
    # Basic tokens
    'IDENTIFIER', 'NUMBER', 'STRING',
    
    # Keywords
    'IF', 'THEN', 'ELSE', 'LET', 'VAL', 'FUNC', 'END',
    'IN', 'NIL', 'TRUE', 'FALSE', 'EXEC',
    
    # Delimiters
    'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE', 'COMMA', 'ASSIGN',
    
    # Operators
    'EQUAL', 'LESS_THAN', 'GREATER_THAN', 'PLUS', 'MINUS', 'TIMES',
    'DIVIDE', 'DOT', 'AND', 'OR'
)

#Single character tokens


#Delimiters
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\['
t_RBRACE = r'\]'
t_COMMA = r','

#Operators
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

#Complex tokens as functions
def t_ASSIGN(t):
    r':='
    return t

def t_NUMBER(t):
    r'\d+'  #\d matches any digit (0-9) and + is one or more of the pattern
    t.value= int(t.value) #converting string to int for parser to use later
    return t

def t_STRING(t):
    r'\"[^\"]*\"' #any character except double quote repeated 0 or more times
    t.value= t.value[1:-1] #removing the quotes 
    return t

def t_IDENTIFIER(t):
    r'[a-zA-Z][a-zA-Z0-9_\']*'
    
    #keywords being matched to token type
    keywords= {
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

    #checking whether identifier is a keyword
    t.type= keywords.get(t.value, 'IDENTIFIER')
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t\r'  #ignoring whitespace 


def t_COMMENT(t):
    r'//.*'
    pass #not returning anything means ignoring 

def t_error(t):
    print(f"Illegal character used '{t.value[0]}'")
    t.lexer.skip(1)

lexer= lex.lex()

# END LEXICAL ANALYZER DEFINITION


#Testing Lexical Analysis

textFile = open('Program_Test.txt', 'r')

data = textFile.read()




lexer.input(data)

while True:
    tok = lexer.token()
    if not tok:
        break
    print(tok)

