type Name = string
type IsRec = bool

val recursive = true : IsRec
val nonRecursive = false : IsRec

datatype 'a Expr 
    = EVar of Name
    | ENum of int
    | EConstr of int * int
    | EAp of 'a Expr * 'a Expr
    | ELet of IsRec * ('a * 'a Expr) list * 'a Expr
    | ECase of 'a Expr * (int * 'a list * 'a Expr)
    | ELam of 'a list * 'a Expr

type 'a Alter = int * 'a list * 'a Expr

(*  bindersOf : ('a * 'b) list -> 'a list  *)
fun bindersOf (defns : ('a * 'b) list) = map #1 defns

(*  rhssOf : ('a * 'b) list -> 'b list  *)
fun rhssOf (defns : ('a * 'b) list) = map #2 defns

(*  isAtomicExpr : 'a Expr -> bool  *)
fun isAtomicExpr (EVar v) = true
  | isAtomicExpr (ENum n) = true
  | isAtomicExpr _        = false

type 'a ScDefn = Name * 'a list * 'a Expr
type 'a Program = 'a ScDefn list

type CoreExpr = Name Expr 
type CoreAlt = Name Alter 
type CoreProgram = Name Program
type CoreScDefn = Name ScDefn

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


(* -- PrettyPrinter -- *)

(*  pprExpr : CoreExpr -> string  *)
fun pprExpr (ENum n) = Int.toString n
  | pprExpr (EVar v) = v
  | pprExpr (EAp (e1,e2)) = pprExpr e1 ^ " " ^ pprAExpr e2

(*  pprAExpr : CoreExpr -> String  *)
and pprAExpr e = if isAtomicExpr e 
                 then pprExpr e
                 else "(" ^ pprExpr e ^ ")"

(*  mkExprs : CoreExpr -> CoreExpr Seq Lazy  *)
fun mkExprs e = Cons (e, fn () => mkExprs e)

(*  mkMultiAp : int -> CoreExpr -> CoreExpr -> CoreExpr  *)
fun mkMultiAp n e1 e2 = foldl EAp e1 (take n (mkExprs e2))

(*  pprint : CoreProgram -> string  *)

