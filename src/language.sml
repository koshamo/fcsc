type name = string
type isRec = bool

val recursive = true : isRec
val nonRecursive = false : isRec
datatype 'a expr 
    = EVar of name
    | ENum of int
    | EConstr of int * int
    | EAp of 'a expr * 'a expr
    | ELet of isRec * ('a * 'a expr) list * 'a expr
    | ECase of 'a expr * (int * 'a list * 'a expr)
    | ELam of 'a list * 'a expr

type 'a alter = int * 'a list * 'a expr

fun bindersOf (defns : ('a * 'b) list) = map #1 defns
fun rhssOf (defns : ('a * 'b) list) = map #2 defns
fun isAtomicExpr (EVar v) = true
  | isAtomicExpr (ENum n) = true
  | isAtomicExpr _        = false

type 'a scDefn = name * 'a list * 'a expr
type 'a program = 'a scDefn list

type coreExpr = name expr 
type coreAlt = name alter 
type coreProgram = name program
type coreScDefn = name scDefn

