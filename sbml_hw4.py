
# Vincent Bonafede
# 111918988


class Node:
    def __init__(self):
        print("init node")

    def evaluate(self):
        return 0


class NumberNode(Node):         # TODO differenciate between reals and int
    def __init__(self, v):
        if ('.' in v):
            self.value = float(v)
        else:
            self.value = int(v)

    def evaluate(self):
        return self.value


class StringNode(Node):
    def __init__(self, value):
        self.value = value
        if('"' in value):
            self.value = value.strip('"')
        elif("'" in value):
            self.value = value.strip("'")

    def evaluate(self):
        return self.value


class BooleanNode(Node):
    def __init__(self, value):
        if(value == 'True'):
            self.value = True
        elif(value == 'False'):
            self.value = False

    def evaluate(self):
        return self.value


class VariableNode(Node):
    def __init__(self, value):
        self.value = value
        # print("in variable node")

    def evaluate(self):
        # print('In Variable evaluate -> ' + self.value)
        return var_table[self.value]


class AssignNode(Node):
    def __init__(self, var, expr):
        self.var = var
        self.expr = expr
        # print("in assign node")

    def evaluate(self):
        # print('evaluating assignment')
        var_table[self.var.value] = self.expr.evaluate()


class AssignToListNode(Node):
    def __init__(self, var, index, expr):
        self.var = var
        self.index = index
        self.expr = expr

    def evaluate(self):
        var_table[self.var.value][self.index.evaluate()] = self.expr.evaluate()


class AssignmentNode(Node):
    def __init__(self, symbol, value):
        self.symbol = symbol
        self.value = value

    def evaluate(self):
        var_table[self.symbol.v] = self.value.evaluate()


class IfNode(Node):
    def __init__(self, condition, block):
        self.condition = condition
        self.block = block
        # print("in if node")

    def evaluate(self):
        # print('Evaluating If Before')
        if (self.condition.evaluate()):
            # print('Evaluating If')
            self.block.evaluate()


class IfElseNode(Node):
    def __init__(self, condition, if_block, else_block):
        self.condition = condition
        self.if_block = if_block
        self.else_block = else_block

    def evaluate(self):
        if (self.condition.evaluate()):
            self.if_block.evaluate()
        else:
            self.else_block.evaluate()


class WhileNode(Node):
    def __init__(self, condition, block):
        self.condition = condition
        self.block = block

    def evaluate(self):
        while(self.condition.evaluate()):
            self.block.evaluate()


class PrintNode(Node):
    def __init__(self, el):
        self.el = el
        # print("in print node")

    def evaluate(self):
        print(self.el.evaluate())


class ListNode(Node):
    def __init__(self, v1, appended):
        self.v1 = v1
        self.appended = appended

    def evaluate(self):
        if (self.v1 is None):
            return []
        if (self.appended is True):
            return self.v1.evaluate()
        else:
            return [self.v1.evaluate()]


class AppendListNode(Node):
    def __init__(self, v1, v2, alreadyList):
        self.v1 = v1
        self.v2 = v2
        self.alreadyList = alreadyList

    def evaluate(self):
        if (self.alreadyList is True):
            return self.v1.evaluate() + [self.v2.evaluate()]
        else:
            return [self.v1.evaluate()] + [self.v2.evaluate()]


class TupleNode(Node):
    def __init__(self, v1, isSingleton):
        self.v1 = v1
        self.isSingleton = isSingleton

    def evaluate(self):
        if self.isSingleton is True:
            return tuple((self.v1.evaluate(),))
        else:
            return tuple(self.v1.evaluate())


class TupleIndexNode(Node):
    def __init__(self, index, tuple):
        self.index = index
        self.tuple = tuple

    def evaluate(self):
        if(self.index.evaluate() == 0):
            print("SYNTAX ERROR")
            exit()
        else:
            return self.tuple.evaluate()[self.index.evaluate() - 1]


class IndexNode(Node):
    def __init__(self, v1, v2, v3):
        self.v1 = v1
        self.v2 = v2
        self.v3 = v3

    def evaluate(self):
        if self.v3 is None:
            return self.v1.evaluate()[self.v2.evaluate()]
        else:
            return self.v1.evaluate()[self.v2.evaluate()][self.v3.evaluate()]


class BlockNode(Node):
    def __init__(self, sl):
        self.statementList = sl
        # print("in block node")

    def evaluate(self):
        for statement in self.statementList:
            statement.evaluate()


class Unop(Node):
    def __init__(self, op, v1):
        self.v1 = v1
        self.op = op

    def evaluate(self):
        return - self.v1.evaluate()


class Unnot(Node):
    def __init__(self, op, v1):
        self.v1 = v1
        self.op = op

    def evaluate(self):
        return not self.v1.evaluate()


class BopNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1
        self.v2 = v2
        self.op = op

    def evaluate(self):
        if (self.op == '+'):                                    # +
            return self.v1.evaluate() + self.v2.evaluate()
        elif (self.op == '-'):                                  # -
            return self.v1.evaluate() - self.v2.evaluate()
        elif (self.op == '*'):                                  # *
            return self.v1.evaluate() * self.v2.evaluate()
        elif (self.op == '/'):                                  # /
            return self.v1.evaluate() / self.v2.evaluate()
        elif (self.op == '<'):                                  # <
            return self.v1.evaluate() < self.v2.evaluate()
        elif (self.op == '<='):                                 # <=
            return self.v1.evaluate() <= self.v2.evaluate()
        elif (self.op == '=='):                                 # ==
            return self.v1.evaluate() == self.v2.evaluate()
        elif (self.op == '<>'):                                 # <>
            return self.v1.evaluate() != self.v2.evaluate()
        elif (self.op == '>'):                                  # >
            return self.v1.evaluate() > self.v2.evaluate()
        elif (self.op == '>='):                                 # >=
            return self.v1.evaluate() >= self.v2.evaluate()
        elif (self.op == 'div'):                                # div
            return self.v1.evaluate() // self.v2.evaluate()
        elif (self.op == 'mod'):                                # mod
            return self.v1.evaluate() % self.v2.evaluate()
        elif (self.op == '**'):                                 # **
            return self.v1.evaluate() ** self.v2.evaluate()
        elif (self.op == 'andalso'):                            # andalso
            return self.v1.evaluate() and self.v2.evaluate()
        elif (self.op == 'orelse'):                             # orelse
            return self.v1.evaluate() or self.v2.evaluate()
        elif (self.op == "::"):                                 # concat
            return [self.v1.evaluate()] + self.v2.evaluate()
        elif (self.op == "in"):                                 # in
            return self.v1.evaluate() in self.v2.evaluate()
        elif (self.op == 'e' or 'E'):                           # real number
            return self.v1.evaluate() * (10 ** self.v2.evaluate())


var_table = dict()
func_dic = dict()


reserved = {
    'in' : 'IN',
    'not' : 'NOT',
    'andalso' : 'ANDALSO',
    'orelse' : 'ORELSE',
    'div' : 'DIV',
    'mod' : 'MOD',
    'e' : 'E',               # TODO only works for lower case e not uppercase E
    'print' : 'PRINT',
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
}


tokens = [
    'NUMBER', 'BOOLEAN', 'STRING',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'LPAREN', 'RPAREN', 'EXPONENT', 'LESSTHEN', 'GREATERTHEN', 'LSQRBRACKET',
    'RSQRBRACKET', 'TUPLEINDEX', 'CONCAT', 'LESSTHENEQUAL',
    'GREATERTHENEQUAL', 'EQUALEQUAL', 'NOTEQUAL', 'COMMA',
    'SEMICOLON', 'L_CURLY', 'R_CURLY', 'VARIABLE', 'EQUALS'     # hw 4 stuff
]
tokens += list(reserved.values())


t_LPAREN = r'\('
t_RPAREN = r'\)'
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EXPONENT = r'\*\*'
t_LESSTHEN = r'\<'
t_GREATERTHEN = r'\>'
t_LSQRBRACKET = r'\['
t_RSQRBRACKET = r'\]'
t_TUPLEINDEX = r'\#'
t_CONCAT = r'\::'
t_LESSTHENEQUAL = r'\<='
t_GREATERTHENEQUAL = r'\>='
t_EQUALEQUAL = r'\=='
t_NOTEQUAL = r'\<>'
t_E = r'e|E'
t_COMMA = r'\,'
t_SEMICOLON = r'\;'
t_L_CURLY = r'\{'
t_R_CURLY = r'\}'
t_EQUALS = r'\='


def t_NUMBER(t):
    r'-?\d*(\d\.|\.\d)\d* | \d+'
    try:
        t.value = NumberNode(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t


def t_STRING(t):
    r'\'(.+?)\'|\"(.+?)\"'
    t.value = StringNode(t.value)
    return t


def t_BOOLEAN(t):
    r'True|False'
    t.value = BooleanNode(t.value)
    return t


def t_VARIABLE(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    if t.value in reserved.keys():
        t.type = reserved.get(t.value)
        # print("variable: " + t.value)
    else:
        t.value = VariableNode(t.value)
    # print('Value: ' + str(t.value))
    return t


# Ignored characters
t_ignore = " \t"


def t_error(t):
    print('SYNTAX ERROR')
    # print("Syntax error at (t_error) '%s'" % t.value)
    exit()


# Build the lexer
import ply.lex as lex

lex.lex()

# Parsing rules
precedence = (
    ('left', 'ORELSE'),
    ('left', 'ANDALSO'),
    ('left', 'NOT'),
    ('left', 'LESSTHEN', 'LESSTHENEQUAL', 'EQUALEQUAL', 'NOTEQUAL', 'GREATERTHEN', 'GREATERTHENEQUAL'),
    ('right', 'CONCAT'),
    ('left', 'IN'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE', 'DIV', 'MOD'),
    ('left', 'EXPONENT'),
    # ('left', ), # list indexing
    ('left', 'TUPLEINDEX'),
    # ('left',), # tuple creation
    # ('left',), # parentetical expression
    ('left', 'E'),
    ('right', 'UMINUS'),
)


def p_block(p):
    '''block : L_CURLY statement_list R_CURLY'''
    p[0] = BlockNode(p[2])


def p_block1(p):
    '''block : L_CURLY R_CURLY'''
    p[0] = BlockNode([])


def p_smt(t):
    '''statement : print_statement
                 | if_statement
                 | if_else_statement
                 | while_statement
                 | assign
                 | assign_statement
                 | index_assign_statement'''
    t[0] = t[1]


def p_print_statement(p):
    '''print_statement : PRINT LPAREN expression RPAREN SEMICOLON'''
    # print("in print")
    p[0] = PrintNode(p[3])


def p_if_statement(t):
    '''if_statement : IF LPAREN expression RPAREN block'''
    t[0] = IfNode(t[3], t[5])


def p_if_else_statement(t):
    '''if_else_statement : IF LPAREN expression RPAREN block ELSE block'''
    t[0] = IfElseNode(t[3], t[5], t[7])


def p_while_statement(t):
    '''while_statement : WHILE LPAREN expression RPAREN block'''
    # print("in while statement")
    t[0] = WhileNode(t[3], t[5])


def p_statement_list(p):
    '''statement_list : statement_list statement'''
    p[0] = p[1] + [p[2]]


def p_statement_list_val(p):
    '''statement_list : statement'''
    p[0] = [p[1]]


def p_expression_unop(t):
    '''expression : MINUS expression %prec UMINUS'''
    t[0] = Unop(t[1], t[2])


def p_expression_not(t):
    # '''expression : NOT expression'''
    'expression : NOT BOOLEAN'
    t[0] = Unnot(t[1], t[2])


def p_expression_binop(t):                                                      # TODO not using the correct types
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression LESSTHEN expression
                  | expression LESSTHENEQUAL expression
                  | expression EQUALEQUAL expression
                  | expression NOTEQUAL expression
                  | expression GREATERTHEN expression
                  | expression GREATERTHENEQUAL expression
                  | expression DIV expression
                  | expression MOD expression
                  | expression EXPONENT expression
                  | expression ANDALSO expression
                  | expression ORELSE expression
                  | expression CONCAT expression
                  | expression IN expression
                  | NUMBER E expression'''
    t[0] = BopNode(t[2], t[1], t[3])


def p_expression_factor(t):
    '''expression : factor
                  | list
                  | index'''
    t[0] = t[1]


def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = t[2]


def p_factor_number(t):
    '''factor : NUMBER'''
    t[0] = t[1]


def p_factor_variable(t):
    '''factor : VARIABLE'''
    # print("in factor variable")
    t[0] = t[1]


def p_factor_string(t):
    'factor : STRING'
    t[0] = t[1]


def p_assign_variable(t):
    'assign : VARIABLE EQUALS expression SEMICOLON'
    # print('In Assign')
    t[0] = AssignNode(t[1], t[3])


def p_assign_statement(t):
    '''assign_statement : VARIABLE EQUALS expression SEMICOLON'''
    t[0] = AssignmentNode(t[1], t[3])


def p_index_assign_statement(t):
    '''index_assign_statement : VARIABLE LSQRBRACKET expression RSQRBRACKET EQUALS expression SEMICOLON'''
    t[0] = AssignToListNode(t[1], t[3], t[6])


def p_factor_boolean(t):
    'factor : BOOLEAN'
    t[0] = t[1]


def p_empty_list(t):
    'list : LSQRBRACKET RSQRBRACKET'
    t[0] = ListNode(None, None)


def p_list(t):
    'list : LSQRBRACKET in_list RSQRBRACKET'
    t[0] = ListNode(t[2], False)


def p_list_appended(t):
    'list : LSQRBRACKET in_list_appended RSQRBRACKET'
    t[0] = ListNode(t[2], True)


def p_in_list(t):
    '''in_list : expression'''
    t[0] = t[1]


def p_in_list_comma(t):
    '''in_list_appended : expression COMMA expression'''
    t[0] = AppendListNode(t[1], t[3], False)


def p_in_list_list_comma(t):
    '''in_list_appended : in_list_appended COMMA expression'''
    t[0] = AppendListNode(t[1], t[3], True)


def p_indexing(t):
    'index : list_head LSQRBRACKET expression RSQRBRACKET'
    t[0] = IndexNode(t[1], t[3], None)


def p_multi_indexing(t):
    'index : list_head LSQRBRACKET expression RSQRBRACKET LSQRBRACKET expression RSQRBRACKET'
    t[0] = IndexNode(t[1], t[3], t[6])


def p_list_head(t):
    '''list_head : list
                 | STRING
                 | VARIABLE'''
    t[0] = t[1]


def p_tuple(t):
    'expression : LPAREN in_list_appended RPAREN'
    t[0] = TupleNode(t[2], False)


def p_tuple_index(t):
    'expression : TUPLEINDEX NUMBER expression'
    t[0] = TupleIndexNode(t[2], t[3])


def p_singleton_tuple(t):
    'expression : LPAREN expression COMMA RPAREN'
    t[0] = TupleNode(t[2], True)


def p_error(t):
    # print("Syntax error at %d %d: %s (%s)" %(t.lineno, t.lexpos, t.value, t.type))
    print("SYNTAX ERROR")
    exit()

import ply.yacc as yacc

yacc.yacc()

lex.lex(debug=0)
yacc.yacc(debug=0)

import sys

# take input via a txt file

if (len(sys.argv) != 2):
    sys.exit("invalid arguments")
code = open(sys.argv[1], 'r').read().replace('\n', '')

lex.input(code)
while True:
    token = lex.token()
    if not token: break
ast = yacc.parse(code)
try:
    if ast is not None:
        ast.evaluate()
        code = ""
except Exception as e:
    # print("SEMANTIC ERROR " + str(e))
    print("SEMANTIC ERROR")                     # + str(e)
    exit()

# HW 3 file input
#
# if (len(sys.argv) != 2):
#     sys.exit("invalid arguments")
# code = open(sys.argv[1], 'r')
#
# for line in code:
#     ast = yacc.parse(line.strip())
#     try:
#         if ast is not None:
#             print(ast.evaluate())
#     except Exception as e:
#         # print("SEMANTIC ERROR: " + str(e))
#         print("SEMANTIC ERROR")
#         exit()

