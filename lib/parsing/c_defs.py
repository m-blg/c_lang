from enum import Enum, auto

KEYWORDS = [
    'auto',
    'break',
    'case',
    'char',
    'const',
    'continue',
    'default',
    'do',
    'double',
    'else',
    'enum',
    'extern',
    'float',
    'for',
    'goto',
    'if',
    'inline',
    'int',
    'long',
    'register',
    'restrict',
    'return',
    'short',
    'signed',
    'sizeof',
    'static',
    'struct',
    'switch',
    'typedef',
    'union',
    'unsigned',
    'void',
    'volatile',
    'while',
    '_Bool',
    '_Complex',
    '_Imaginary']


PUNCTUATORS = ['[',
     ']',
     '(',
     ')',
     '{',
     '}',
     '.',
     '->',
     '++',
     '--',
     '&',
     '*',
     '+',
     '-',
     '˜',
     '!',
     '/',
     '%',
     '<<',
     '>>',
     '<',
     '>',
     '<=',
     '>=',
     '==',
     '!=',
     'ˆ',
     '|',
     '&&',
     '||',
     '?',
     ':',
     ';',
     '...',
     '=',
     '*=',
     '/=',
     '%=',
     '+=',
     '-=',
     '<<=',
     '>>=',
     '&=',
     'ˆ=',
     '|=',
     ',',
     '#',
     '##',
     '<:',
     ':>',
     '<%',
     '%>',
     '%:',
     '%:%:']


class IntegerConstantType(Enum):
    INT = ""
    UINT = "u"
    LONG = "l"
    ULONG = "lu"
    LLONG = "ll"
    ULLONG = "llu"


class IntegerConstantBase(Enum):
    DEC = ""
    HEX = "0x"
    OCT = "0"
