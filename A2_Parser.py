import ply.lex as lex
import ply.yacc as yacc


# BEGIN LEXICAL ANALYZER DEFINITION


#Creating the token list
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

def t_ID(t):
    r'[a-z][a-zA-Z0-9_\']*'
    
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

    #checking whether token is a keyword
    if t.value in keywords: 
        t.type= keywords[t.value]

    else:
        t.type= 'ID'
    
    return t

def t_ID_FUNC(t):
    r'[A-Z][a-zA-Z0-9_\']*'
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


# END LEXICAL ANALYZER DEFINITION

###########################

# BEGIN PARSING DEFINITION

precedence= (          #precedence rules for operators
        ('left', 'OR'),
        ('left', 'AND'),
        ('left', 'EQUAL', 'LESS_THAN', 'GREATER_THAN'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIVIDE'),
        ('left', 'DOT'),
    )
#Defining grammar rules

#Top-level rule! program consists of facts followed by exec line
def p_global_facts(p):
    '''global_facts : facts exec_line'''
    p[0] = ('global_facts', p[1], p[2])  # Combines facts and exec_line

#Facts can be function definitions or assignments and can have multiple
def p_facts_multiple(p):
    '''facts : func_def facts
             | assign facts'''
    p[0] = ('facts', p[1], p[2])  #Joins facts together

#Facts can be empty (no more facts)
def p_facts_empty(p):
    '''facts : empty'''
    p[0] = ('facts_empty',)  #End of facts list

#Empty production 
def p_empty(p):
    '''empty :'''
    p[0] = None  #Represents nothing

#Function definition: func name[params] := body end
def p_func_def(p):
    '''func_def : FUNC ID_FUNC LBRACE params RBRACE ASSIGN stm END'''
    p[0] = ('func_def', p[2], p[4], p[7])  #Stores name, params, body

#Parameters with comma (function name)
def p_params_func_comma(p):
    '''params : ID_FUNC COMMA params'''
    p[0] = ('params', p[1], p[3])  #Builds parameter list

#Parameters with comma (regular identifier)
def p_params_id_comma(p):
    '''params : ID COMMA params'''
    p[0] = ('params', p[1], p[3])  #Builds parameter list

#Single parameter (function name)
def p_params_func(p):
    '''params : ID_FUNC'''
    p[0] = ('params', p[1])  #Single parameter

#Single parameter (regular identifier)
def p_params_id(p):
    '''params : ID'''
    p[0] = ('params', p[1])  #Single parameter

#Variable assignment: val name := value end
def p_assign(p):
    '''assign : VAL ID ASSIGN stm END'''
    p[0] = ('assign', p[2], p[4])  #Stores name and value

#Function call: name[args]
def p_stm_func_call(p):
    '''stm : ID_FUNC LBRACE args RBRACE'''
    p[0] = ('func_call', p[1], p[3])  #Stores function name and args

#Arguments with comma (function name)
def p_args_func_comma(p):
    '''args : ID_FUNC COMMA args'''
    p[0] = ('args', p[1], p[3])  #Builds argument list

#Arguments with comma (expression)
def p_args_stm_comma(p):
    '''args : stm COMMA args'''
    p[0] = ('args', p[1], p[3])  #Builds argument list

#Single argument (function name)
def p_args_func(p):
    '''args : ID_FUNC'''
    p[0] = ('args', p[1])  #Single argument

#Single argument (expression)
def p_args_stm(p):
    '''args : stm'''
    p[0] = ('args', p[1])  #Single argument


#Binary Operators
def p_stm_binop(p):
    '''stm : stm PLUS stm
           | stm MINUS stm
           | stm TIMES stm
           | stm DIVIDE stm
           | stm DOT stm
           | stm LESS_THAN stm
           | stm GREATER_THAN stm
           | stm EQUAL stm
           | stm AND stm
           | stm OR stm'''
    p[0] = (p[2], p[1], p[3])

#Basic values
def p_stm_string(p):
    '''stm : STRING'''
    p[0] = ('STRING', p[1])

def p_stm_number(p):
    '''stm : NUMBER'''
    p[0] = ('NUMBER', p[1])

def p_stm_true(p):
    '''stm : TRUE'''
    p[0] = ('TRUE',)

def p_stm_false(p):
    '''stm : FALSE'''
    p[0] = ('FALSE',)

def p_stm_nil(p):
    '''stm : NIL'''
    p[0] = ('NIL',)

def p_stm_id(p):
    '''stm : ID'''
    p[0] = ('ID', p[1])

def p_stm_paren(p):
    '''stm : LPAREN stm RPAREN'''
    p[0] = ('paren', p[2])

def p_stm_if(p):
    '''stm : IF stm THEN stm ELSE stm END'''
    p[0] = ('if', p[2], p[4], p[6])

def p_stm_let(p):
    '''stm : LET facts IN stm END'''
    p[0] = ('let', p[2], p[4])

def p_exec_line(p):
    '''exec_line : EXEC stm'''
    p[0] = ('exec', p[2])

#Error rule for syntax errors
def p_error(p):
    if p:
        print(f"Syntax error in input: line {p.lineno}")
    else:
        print("Syntax error in input: none")

# END PARSING DEFINITION


# CALL PARSING 

def main():
  print("Initiating Parsing")

  # Build the lexer and parser
  lexer = lex.lex()
  parser = yacc.yacc()

  # Read the file
  textFile = open('Program_Test.txt', 'r')
  data = textFile.read()

  # Parse the file
  parser.parse(data, lexer=lexer)
  
  print("Finalizing Parsing")

if __name__ == '__main__':
  main()

