import re

# ================================
# LEXER
# ================================
TOKEN_SPEC = [
    ("NUMBER",   r'\d+(\.\d+)?'),
    ("ID",       r'[A-Za-z_]\w*'),
    ("OP",       r'==|!=|>=|<=|>|<|\+|\-|\*|/|%'),
    ("AND",      r'and'),
    ("OR",       r'or'),
    ("NOT",      r'not'),
    ("ASSIGN",   r'='),
    ("LPAREN",   r'\('),
    ("RPAREN",   r'\)'),
    ("SEMICOL",  r';'),
    ("SKIP",     r'[ \t]+'),
    ("MISMATCH", r'.')
]

TOKEN_REGEX = "|".join(f"(?P<{name}>{pattern})" for name, pattern in TOKEN_SPEC)

def tokenize(code):
    tokens = []
    for match in re.finditer(TOKEN_REGEX, code):
        kind = match.lastgroup
        value = match.group()

        if kind == "NUMBER":
            value = float(value) if '.' in value else int(value)
        elif kind == "ID":
            pass
        elif kind == "SKIP":
            continue
        elif kind == "MISMATCH":
            raise SyntaxError(f"Unexpected token: {value}")

        tokens.append((kind, value))
    tokens.append(("EOF", None))
    return tokens

# ================================
# PARSER (RECURSIVE DESCENT)
# ================================
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def peek(self):
        return self.tokens[self.pos]

    def advance(self):
        self.pos += 1
        return self.tokens[self.pos - 1]

    # Grammar:
    # program     → stmt (';' stmt)*
    # stmt        → ID '=' expr | expr
    # expr        → logic_or
    # logic_or    → logic_and ('or' logic_and)*
    # logic_and   → equality ('and' equality)*
    # equality    → comparison ( (== | != ) comparison )*
    # comparison  → term ( > | < | >= | <= term )*
    # term        → factor ( ('+'|'-') factor )*
    # factor      → unary ( ('*'|'/'|'%') unary )*
    # unary       → 'not' unary | primary
    # primary     → NUMBER | ID | '(' expr ')'

    def parse(self):
        statements = [self.statement()]
        while self.peek()[0] == "SEMICOL":
            self.advance()
            statements.append(self.statement())
        return statements

    def statement(self):
        tok_type, tok_val = self.peek()
        if tok_type == "ID":
            # Lookahead for assignment
            next_tok = self.tokens[self.pos + 1]
            if next_tok[0] == "ASSIGN":
                var = self.advance()[1]    # ID
                self.advance()             # =
                expr = self.expr()
                return ("assign", var, expr)
        return self.expr()

    def expr(self):
        return self.logic_or()

    def logic_or(self):
        node = self.logic_and()
        while self.peek()[0] == "OR":
            op = self.advance()[1]
            right = self.logic_and()
            node = ("binop", op, node, right)
        return node

    def logic_and(self):
        node = self.equality()
        while self.peek()[0] == "AND":
            op = self.advance()[1]
            right = self.equality()
            node = ("binop", op, node, right)
        return node

    def equality(self):
        node = self.comparison()
        while self.peek()[0] == "OP" and self.peek()[1] in ("==", "!="):
            op = self.advance()[1]
            right = self.comparison()
            node = ("binop", op, node, right)
        return node

    def comparison(self):
        node = self.term()
        while self.peek()[0] == "OP" and self.peek()[1] in (">", "<", ">=", "<="):
            op = self.advance()[1]
            right = self.term()
            node = ("binop", op, node, right)
        return node

    def term(self):
        node = self.factor()
        while self.peek()[0] == "OP" and self.peek()[1] in ("+", "-"):
            op = self.advance()[1]
            right = self.factor()
            node = ("binop", op, node, right)
        return node

    def factor(self):
        node = self.unary()
        while self.peek()[0] == "OP" and self.peek()[1] in ("*", "/", "%"):
            op = self.advance()[1]
            right = self.unary()
            node = ("binop", op, node, right)
        return node

    def unary(self):
        tok_type, tok_val = self.peek()
        if tok_type == "NOT":
            self.advance()
            return ("unop", "not", self.unary())
        return self.primary()

    def primary(self):
        tok_type, tok_val = self.peek()

        if tok_type == "NUMBER":
            self.advance()
            return ("num", tok_val)

        if tok_type == "ID":
            self.advance()
            return ("var", tok_val)

        if tok_type == "LPAREN":
            self.advance()
            expr = self.expr()
            if self.peek()[0] != "RPAREN":
                raise SyntaxError("Missing closing parenthesis")
            self.advance()
            return expr

        raise SyntaxError(f"Unexpected token: {tok_val}")

# ================================
# INTERPRETER (EVALUATOR)
# ================================
class Interpreter:
    def __init__(self):
        self.env = {}  # symbol table

    def eval(self, node):
        ntype = node[0]

        if ntype == "num":
            return node[1]

        if ntype == "var":
            name = node[1]
            if name not in self.env:
                raise NameError(f"Undefined variable '{name}'")
            return self.env[name]

        if ntype == "assign":
            _, var, expr = node
            value = self.eval(expr)
            self.env[var] = value
            return value

        if ntype == "unop":
            _, op, expr = node
            val = self.eval(expr)
            if op == "not":
                return not val

        if ntype == "binop":
            _, op, left, right = node
            l = self.eval(left)
            r = self.eval(right)

            return {
                "+": l + r,
                "-": l - r,
                "*": l * r,
                "/": l / r,
                "%": l % r,
                "==": l == r,
                "!=": l != r,
                ">": l > r,
                "<": l < r,
                ">=": l >= r,
                "<=": l <= r,
                "and": l and r,
                "or": l or r
            }[op]

        raise RuntimeError("Unknown AST node")

# ================================
# MAIN REPL LOOP
# ================================
print("=== Advanced Expression Interpreter ===")
print("Type expressions or assignments. Use ';' to chain statements.")
print("Type 'exit' to quit.\n")

interpreter = Interpreter()

while True:
    code = input(">>> ")

    if code.strip() == "exit":
        break

    try:
        tokens = tokenize(code)
        parser = Parser(tokens)
        statements = parser.parse()

        last_value = None
        for stmt in statements:
            last_value = interpreter.eval(stmt)

        print(last_value)

    except Exception as e:
        print("Error:", e)
