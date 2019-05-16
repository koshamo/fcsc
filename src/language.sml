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

datatype Iseq = INil
              | IStr of string
              | IAppend of Iseq * Iseq

(*  iNil : Iseq  *)
val iNil = INil

(*  iStr : string -> Iseq  *)
fun iStr str = IStr str

(*  iAppend : Iseq -> Iseq -> ISeq  *)
fun iAppend seq1 seq2 = IAppend (seq1, seq2)

(*  iNewline : Iseq  *)
val iNewline = IStr "\n"

(*  iIndent : Iseq -> Iseq  *)
fun iIndent seq = seq

(*  iDisplay : Iseq -> string  *)
(*  iConcat : Iseq list -> Iseq  *)
fun iConcat = rfold iAppend iNil

(*  iInterleave : Iseq -> Iseq list -> Iseq  *)
fun iInterleave s [] = iNil
  | iInterleave s [seq] = seq
  | iInterleave s (seq::seqs) = iAppend seq (iAppend s (iInterleave s seqs))

(*  pprDefn : Name * CoreExpr -> Iseq  *)
fun pprDefn (name,expr) = 
            iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

(*  pprDefns : (Name * CoreExpr) list -> Iseq  *)
and pprDefns defns = let val sep = iConcat [ iStr ";", iNewline ]
                     in iInterleave sep (map pprDefn defns)
                     end

(*  pprExpr : CoreExpr -> Iseq  *)
and pprExpr (ENum n) = iStr Int.toString n
  | pprExpr (EVar v) = iStr v
  | pprExpr (EAp (e1,e2)) = iAppend (iAppend pprExpr e1 iStr " ") 
                                    pprAExpr e2
  | pprExpr (ELet isrec defns expr) = 
                let 
                  val keyword = if isrec then "letrec" else "let" 
                in
                  iConcat [ iStr keyword, iNewline,
                            iStr "  ", iIndent (pprDefns defns), iNewline,
                            iStr "in ", pprExpr expr ]
                end

(*  pprAExpr : CoreExpr -> String  *)
and pprAExpr e = if isAtomicExpr e 
                 then pprExpr e
                 else "(" ^ pprExpr e ^ ")"

(*  mkExprs : CoreExpr -> CoreExpr Seq Lazy  *)
fun mkExprs e = Cons (e, fn () => mkExprs e)

(*  mkMultiAp : int -> CoreExpr -> CoreExpr -> CoreExpr  *)
fun mkMultiAp n e1 e2 = foldl EAp e1 (take n (mkExprs e2))

(*  pprint : CoreProgram -> string  *)
fun pprint prog = iDisplay (pprProgram prog)

