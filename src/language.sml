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
    | ECase of 'a expr * 'a alter list
    | ELam of 'a list * 'a expr

type coreExpr = name expr 

fun bindersOf (defns : ('a * 'b) list) = map #1 defns
