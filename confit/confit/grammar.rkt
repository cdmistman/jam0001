#lang brag

begin: expr*
@expr: apply | atom | quote | quasiquote | unquote | unquote-splicing | line-comment | block-comment
apply: /OPEN expr* /CLOSE
@atom: symbol | string | number
quote: QUOTE expr
quasiquote: QUASIQUOTE expr
unquote-splicing: UNQUOTE-SPLICE expr
unquote: UNQUOTE expr
line-comment: LINE-COMMENT
block-comment: BLOCK-COMMENT
symbol: SYMBOL
string: STRING
number: NUMBER
