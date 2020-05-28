
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


class TupleNode(Node):
    def __init__(self, v1, isSingleton):
        self.v1 = v1
        self.isSingleton = isSingleton

    def evaluate(self):
        if self.isSingleton is True:
            return tuple((self.v1.evaluate(),))
        else:
            return tuple(self.v1.evaluate())


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


class TupleIndexNode(Node):
    def __init__(self, index, tuple):
        self.index = index
        self.tuple = tuple

    def evaluate(self):
        if(self.index.evaluate() == 0):
            print("SYNTAX ERROR")
            # exit()
        else:
            return self.tuple.evaluate()[self.index.evaluate() - 1]


class IndexNode(Node):
    def __init__(self, v1, v2):
        self.v1 = v1
        self.v2 = v2

    def evaluate(self):
        return self.v1.evaluate()[self.v2.evaluate()]


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


reserved = {
    'in' : 'IN',
    'not' : 'NOT',
    'andalso' : 'ANDALSO',
    'orelse' : 'ORELSE',
    'div' : 'DIV',
    'mod' : 'MOD',
    'e' : 'E'               # TODO only works for lower case e not uppercase E
}


tokens = [
    'NUMBER', 'BOOLEAN', 'STRING',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'LPAREN', 'RPAREN', 'EXPONENT', 'LESSTHEN', 'GREATERTHEN', 'LSQRBRACKET',
    'RSQRBRACKET', 'TUPLEINDEX', 'CONCAT', 'LESSTHENEQUAL',
    'GREATERTHENEQUAL', 'EQUALEQUAL', 'NOTEQUAL', 'COMMA'
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
    r'[A-Za-z][A-Za-z]*'
    if t.value in reserved.keys():
        t.type = reserved.get(t.value)
    else:
        print("SYNTAX ERROR")
    return t


# Ignored characters
t_ignore = " \t"


def t_error(t):
    print('SYNTAX ERROR')
    # print("Syntax error at (t_error) '%s'" % t.value)
    # exit()


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


def p_expression_unop(t):
    '''expression : MINUS expression %prec UMINUS'''
    t[0] = Unop(t[1], t[2])


def p_expression_not(t):
    # '''expression : NOT expression'''
    'expression : NOT BOOLEAN'
    t[0] = Unnot(t[1], t[2])


def p_expression_binop(t):                 # TODO correct data types for each, differenciate between syntax and semantic
    '''expression : NUMBER PLUS NUMBER
                  | STRING PLUS STRING
                  | list PLUS list
                  | expression MINUS expression
                  | NUMBER TIMES NUMBER
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
                  | BOOLEAN ANDALSO BOOLEAN
                  | BOOLEAN ORELSE BOOLEAN
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


def p_factor_string(t):
    'factor : STRING'
    t[0] = t[1]


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
    'in_list : expression'
    t[0] = t[1]


def p_in_list_comma(t):
    '''in_list_appended : expression COMMA expression'''
    t[0] = AppendListNode(t[1], t[3], False)


def p_in_list_list_comma(t):
    '''in_list_appended : in_list_appended COMMA expression'''
    t[0] = AppendListNode(t[1], t[3], True)


def p_indexing(t):                                                  # TODO multiple indexing
    'index : list_head LSQRBRACKET expression RSQRBRACKET'
    t[0] = IndexNode(t[1], t[3])


def p_list_head(t):
    '''list_head : list
                 | STRING'''
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
    # self.type, self.value, self.lineno, self.lexpos)
    print("SYNTAX ERROR")
    # exit()

import ply.yacc as yacc

yacc.yacc()

lex.lex(debug=0)
yacc.yacc(debug=0)

import sys

# take input via a txt file

if (len(sys.argv) != 2):
    sys.exit("invalid arguments")
code = open(sys.argv[1], 'r')

for line in code:
    ast = yacc.parse(line.strip())
    try:
        if ast is not None:
            print(ast.evaluate())
    except Exception as e:
        # print("SEMANTIC ERROR: " + str(e))
        print("SEMANTIC ERROR")
        # exit()

