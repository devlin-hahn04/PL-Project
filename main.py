import sys
import os
from Parser import parser
from interpreter import Interpreter
import pprint

def main():
    if len(sys.argv) != 2:
        print("Usage: python main.py <source_file>")
        return 1
    
    filename = sys.argv[1]
    
    if not os.path.exists(filename):
        print(f"Error: File not found - {filename}")
        return 1
    
    print(f"=== Processing file: {filename} ===")
    
    print("Parsing source code...")
    try:
        with open(filename, 'r') as f:
            source_code = f.read()
        ast = parser.parse(source_code)
        if ast is None:
            print("Parsing failed due to syntax errors.")
            return 1
        print("Parsing successful!")
        
        # Print the AST
        print("\n" + "=" * 70)
        print("ABSTRACT SYNTAX TREE (AST):")
        print("=" * 70)
        pprint.pprint(ast, indent=2, width=100)
        print("=" * 70 + "\n")
        
    except Exception as e:
        print(f"Parsing error: {e}")
        import traceback
        traceback.print_exc()
        return 1
    
    print("Executing program...")
    try:
        interpreter = Interpreter()
        result = interpreter.eval(ast)
        
        print("\n=== PROGRAM OUTPUT ===")
        if result is None:
            print(f"Result: None")
        else:
            print(f"Result: {result}")
        print("======================")
        
        return 0
    except Exception as e:
        print(f"\nRuntime error: {e}")
        import traceback
        traceback.print_exc()
        return 1

if __name__ == '__main__':
    sys.exit(main())
