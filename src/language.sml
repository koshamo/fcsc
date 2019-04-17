signature LANGUAGE =
  sig
    datatype 'a expr 
        = EVar of string
        | ENum of int
        | EConstr of int * int
        | EAp of 'a expr * 'a expr
        | ELet of bool * ['a * 'a expr] * 'a expr
        | ECase of 'a expr * ['a alter]
        | ELam of ['a] * 'a expr
  end
