type Name = string
type IsRec = bool

val recursive = true : IsRec
val nonRecursive = false : IsRec

datatype 'a Expr 
    = EVar of Name
    | ENum of int
(*    | EConstr of int * int   *)
    | EAp of 'a Expr * 'a Expr
    | ELet of IsRec * ('a * 'a Expr) list * 'a Expr
    | ECase of 'a Expr * (int * 'a list * 'a Expr) list
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

(*  iNum : int -> Iseq  *)
fun iNum n = IStr (Int.toString n)

(*  iStr : string -> Iseq  *)
fun iStr str = IStr str

(*  iAppend : Iseq -> Iseq -> Iseq  *)
fun iAppend INil seq2 = seq2
  | iAppend seq1 INil = seq1
  | iAppend seq1 seq2 = IAppend (seq1, seq2)

(*  iNewline : Iseq  *)
val iNewline = IStr "\n"

(*  iSpace : Iseq  *)
val iSpace = IStr " "

(*  iIndent : Iseq -> Iseq  *)
fun iIndent seq = seq

(*  flatten : Iseq list -> string  *)
fun flatten [] = ""
  | flatten (INil :: seqs) = flatten seqs
  | flatten ((IStr (s)) :: seqs) = s ^ (flatten seqs)
  | flatten ((IAppend (seq1,seq2)) :: seqs) = flatten (seq1 :: seq2 :: seqs)

(*  iDisplay : Iseq -> string  *)
fun iDisplay seq = flatten [seq]

(*  iConcat : Iseq list -> Iseq  *)
val iConcat = rfold iAppend iNil

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
and pprExpr (ENum n) = iNum n
  | pprExpr (EVar v) = iStr v
  | pprExpr (EAp (e1,e2)) = iConcat [pprExpr e1, iSpace, pprAExpr e2 ]
  | pprExpr (ELet (isrec,defns,expr)) = 
                let 
                  val keyword = if isrec then "letrec" else "let" 
                in
                  iConcat [ iStr keyword, iNewline,
                            iStr "  ", iIndent (pprDefns defns), iNewline,
                            iStr "in ", pprExpr expr ]
                end
  | pprExpr (ECase (expr,alter)) = 
                    let 
                      val iNl = iConcat [ iStr ";", iNewline ]
                      fun pprAlt (tag, args, rhs) =
                          iConcat [ iStr "<", iNum tag, iStr "> ",
                                    pprArgs args, iStr " -> ",
                                    iIndent (pprExpr rhs) ]
                    in
                        iConcat [ iStr "case ", pprExpr expr, 
                                  iStr " of", iNewline, iStr "  ",
                                  iIndent (iInterleave iNl 
                                    (map pprAlt alter)) ]
                    end
  | pprExpr (ELam (args,body)) =
                    iConcat [ iStr "(\\", pprArgs args, iStr ". ",
                              iIndent (pprExpr body), iStr ")" ]

(*  pprArgs : 'a list -> iSeq  *)
and pprArgs args = iInterleave iSpace (map iStr args)

(*  pprAExpr : CoreExpr -> Iseq  *)
and pprAExpr e = if isAtomicExpr e 
                 then pprExpr e
                 else iConcat [ IStr "(", pprExpr e, IStr ")" ]

(*  pprProgram : CoreProgram -> Iseq  *)
and pprProgram prog = iInterleave (iAppend (iStr " ;") iNewline) 
                                  (map pprSc prog)

(*  pprSc : CoreScDefn -> Iseq  *)
and pprSc (name, args, body) = 
                     iConcat [ iStr name, iSpace, pprArgs args,
                     iStr " = ", iIndent (pprExpr body) ]

(*  mkExprs : CoreExpr -> CoreExpr Seq Lazy  *)
fun mkExprs e = Cons (e, fn () => mkExprs e)

(*  mkMultiAp : int -> CoreExpr -> CoreExpr -> CoreExpr  *)
fun mkMultiAp n e1 e2 = foldl EAp e1 (take n (mkExprs e2))

(*  pprint : CoreProgram -> iSeq  *)
fun pprint prog = iDisplay (pprProgram prog)

