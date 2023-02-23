import ply.lex as lex

resultado_lexema = []

tokens = ['NUMERO', 'DECIMAL', 'TXT', 'ID', 'SUMA', 'RESTA', 
          'DIVISION', 'MULTI', 'EQUALS', 'SEMI', 'MENORQ', 'MAYORQ',
          'MENORIGUAL', 'MAYORIGUAL']

reservadas = {
    'int' : 'INT',
    'double' : 'DOUBLE',
    'bool' : 'BOOL',
    'string' : 'STRING',
    'float' : 'FLOAT'
}

tokens = tokens + list(reservadas.values())

t_SUMA          = r'\+'
t_RESTA         = r'\-'
t_MULTI         = r'\*'
t_DIVISION      = r'/'
t_EQUALS        = r'='
t_MENORQ        = r'<'
t_MAYORQ        = r'>'
t_MENORIGUAL    = r'<='
t_MAYORIGUAL    = r'>='
t_SEMI          = r';'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Caracter ilegal '%s'"%t.value[0])
    t.lexer.skip(1)

def t_DECIMAL(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_NUMERO(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_TXT(t) :
    r'\".*?\"'
    return t
    
def t_espacio(t):
    r"\s"
    pass

def t_DOUBLE(t):
    r'double'
    return t

def t_FLOAT(t):
    r'float'
    return t

def t_INT(t):
    r'int'
    return t


def t_BOOL(t):
    r'bool'
    return t

def t_STRING(t):
    r'string'
    return t

def t_ID(t):
    r'\w+\=|''\w+'
    return t

lexer = lex.lex()

# Prueba de ingreso

def prueba(data):
    global resultado_lexema

    analizador = lex.lex()
    analizador.input(data)
    while True:
        tok = analizador.token()
        if not tok:
            break
        estado = "Linea {:4} Tipo {:4} >>>>> {:4}".format(
            str(tok.lineno), str(tok.type), str(tok.value))
        resultado_lexema.append(estado)

    return resultado_lexema


# abrir archivo
analizador = lex.lex()
# path = "prueba.java"

# try:
#     archivo = open(path, 'r')
# except:
#     print("el archivo no se encontro")
#     quit()

# text = ""
# for linea in archivo:
#     text += linea
# prueba(text)
# # AL IMPRIMIR LOS DATOS, ESTO LO ORDENA DE MANERA ESTRUCTURADA
# print('\n'.join(list(map(''.join, resultado_lexema))))
