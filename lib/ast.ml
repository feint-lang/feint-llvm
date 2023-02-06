type expr = Nil | Bool of bool | Int of int | String of string
type statement = expr
type program = statement list
