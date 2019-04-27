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

(*  bindersOf : ('a * 'b) list -> 'a list  *)
fun bindersOf (defns : ('a * 'b) list) = map #1 defns

(** rhssOf : ('a * 'b) list -> 'b list  *)
fun rhssOf (defns : ('a * 'b) list) = map #2 defns

(*  isAtomicExpr : 'a expr -> bool  *)
fun isAtomicExpr (EVar v) = true
  | isAtomicExpr (ENum n) = true
  | isAtomicExpr _        = false

type 'a scDefn = name * 'a list * 'a expr
type 'a program = 'a scDefn list

type coreExpr = name expr 
type coreAlt = name alter 
type coreProgram = name program
type coreScDefn = name scDefn

val preludeDefs = 
  [("I",  ["x"], EVar "x"),
   ("K",  ["x","y"], EVar "x"),
   ("K1", ["x","y"], EVar "y"),
   ("S",  ["f","g","x"], EAp (EAp (EVar "f", EVar "x"),
                             EAp (EVar "g", EVar "x"))),
   ("compose", ["f","g","x"], EAp (EVar "f", 
                                  (EAp (EVar "g", EVar "x")))),
   ("twice", ["f"], EAp (EAp (EVar "compose", EVar "f"), EVar "f"))
  ]

(*  pprint : coreProgram -> string  *)

