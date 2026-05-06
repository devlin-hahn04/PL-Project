class Interpreter:
    def __init__(self):
        self.facts = {}
        self.environment = {}

    def eval(self, node):
        if node is None:
            return None
            
        if node['type'] == 'program':
            result = None
            for stmt in node['statements']:
                if stmt['type'] == 'func':
                    self.facts[stmt['name']] = stmt
            
            for stmt in node['statements']:
                if stmt['type'] == 'func':
                    continue
                elif stmt['type'] == 'exec':
                    result = self.eval(stmt['expression'])
                elif stmt['type'] == 'let':
                    result = self.eval(stmt)
                else:
                    result = self.eval(stmt)
            return result
            
        elif node['type'] == 'let':
            saved_env = self.environment.copy()
            for binding_name in node['bindings']:
                binding = node['bindings'][binding_name]
                if binding['type'] == 'val':
                    self.environment[binding['name']] = self.eval(binding['stmt'])
            result = self.eval(node['body'])
            self.environment = saved_env
            return result

        elif node['type'] == 'if':
            condition = self.eval(node['cond'])
            if condition:
                return self.eval(node['then'])
            else:
                return self.eval(node['else'])

        elif node['type'] == 'unary_minus':
            return -self.eval(node['expr'])

        elif node['type'] == 'call':
            func_name = node['func']
            if func_name not in self.facts:
                raise NameError(f"Function '{func_name}' is not defined")
                
            func_def = self.facts[func_name]
            saved_env = self.environment.copy()
            
            args = [self.eval(arg) for arg in node['args']]
            params = func_def.get('params', [])
            
            if len(args) != len(params):
                raise Exception(f"Argument count mismatch: expected {len(params)}, got {len(args)}")
            
            for i, param in enumerate(params):
                self.environment[param['name']] = args[i]
            
            result = self.eval(func_def['stmt'])
            self.environment = saved_env
            return result

        elif node['type'] == 'op':
            left = self.eval(node['left'])
            right = self.eval(node['right'])
            op = node['op']
            
            if op == '+':
                return left + right
            elif op == '-':
                return left - right
            elif op == '*':
                return left * right
            elif op == '/':
                if right == 0:
                    raise ZeroDivisionError("Division by zero")
                return left / right
            elif op == '<':
                return left < right
            elif op == '>':
                return left > right
            elif op == '=':
                return left == right
            elif op == '&':
                return left and right
            elif op == '|':
                return left or right
            elif op == '.':
                return str(left) + str(right)
            else:
                raise ValueError(f"Unknown operator: {op}")

        elif node['type'] == 'value':
            return node['value']
            
        elif node['type'] == 'id':
            if node['name'] not in self.environment:
                raise NameError(f"Variable '{node['name']}' is not defined")
            return self.environment[node['name']]
            
        elif node['type'] == 'func':
            self.facts[node['name']] = node
            return None
            
        else:
            raise TypeError(f"Unknown node type: {node['type']}")
