import ply.lex as lex

# BEGIN LEXICAL ANALYZER DEFINITION
#
#
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

