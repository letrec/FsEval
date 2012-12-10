open System
open Microsoft.FSharp.Text.Lexing

open Ast
open Lexer
open Parser

let inline quote_string (s : string) = "\"" + s + "\""

let inline unquote_string (s : string) = s.Replace("\"", String.Empty)

let value_to_string v =
  match v with
  | VBoolean b -> b.ToString()
  | VInteger i -> i.ToString()
  | VDouble d -> d.ToString()
  | VString s -> quote_string s
  | VError e -> e

let value_to_type v =
  match v with
  | VBoolean b -> TBoolean
  | VInteger i -> TInteger
  | VDouble d -> TDouble
  | VString s -> TString
  | VError e -> TError

let type_to_string t =
  match t with
  | TBoolean -> "Boolean"
  | TInteger -> "Integer"
  | TDouble -> "Double"
  | TString -> "String"
  | TError -> "Error"

let rec expr_to_string (expr : Expr) : string =
  match expr with
  | Identifier x -> x
  | Boolean x -> x.ToString()
  | Integer x -> x.ToString()
  | Double x -> x.ToString()
  | String x -> x
  | Paren x -> "(" + expr_to_string x + ")"
  | Or (x, y) -> expr_to_string x + " || " + expr_to_string y
  | And (x, y) -> expr_to_string x + " && " + expr_to_string y
  | Plus (x, y) -> expr_to_string x + " + " + expr_to_string y
  | Minus (x, y) -> expr_to_string x + " - " + expr_to_string y
  | Aster (x, y) -> expr_to_string x + " * " + expr_to_string y
  | Slash (x, y) -> expr_to_string x + " / " + expr_to_string y
  | Greater (x, y) -> expr_to_string x + " > " + expr_to_string y
  | Less (x, y) -> expr_to_string x + " < " + expr_to_string y
  | Negate x -> "-" + expr_to_string x

let rec expr_to_type expr env =
  match expr with
  | Identifier x -> (env x ) |> value_to_type
  | Boolean b -> TBoolean
  | Integer i -> TInteger
  | Double d -> TDouble
  | String s -> TString
  | Paren x -> expr_to_type x env
  | Or (x, y) -> TBoolean
  | And (x, y) -> TBoolean
  | Plus (Integer x, Integer y) -> TInteger
  | Plus (Double x, Double y) -> TDouble
  | Plus (String x, String y) -> TString
  | Plus (x, y) -> TInteger
  | Minus (Integer x, Integer y) -> TInteger
  | Minus (Double x, Double y) -> TDouble
  | Minus (String x, String y) -> TString
  | Minus (x, y) -> TInteger
  | Aster (Integer x, Integer y) -> TInteger
  | Aster (Double x, Double y) -> TDouble
  | Aster (String x, String y) -> TString
  | Aster (x, y) -> TInteger
  | Slash (Integer x, Integer y) -> TInteger
  | Slash (Double x, Double y) -> TDouble
  | Slash (String x, String y) -> TString
  | Slash (x, y) -> TInteger
  | Greater (x, y) -> TBoolean
  | Less (x, y) -> TBoolean
  | Negate (Integer x) -> TInteger
  | Negate (Double x) -> TDouble
  | Negate (x) -> TInteger

let error expr expected (env : string -> Value) =
  let actual = expr_to_type expr env |> type_to_string
  VError ("Typing error: expression " + expr_to_string expr + " is expected to be of type " + expected + ", but has a type " + actual)

let rec eval expr (env : string -> Value) =
  match expr with
  | Identifier x -> env x
  | Boolean x -> VBoolean x
  | Integer x -> VInteger x
  | Double x -> VDouble x
  | String x -> VString (x.Replace("\"", String.Empty))
  | Paren x -> eval x env
  | Or (x, y) ->
    match (eval x env, y) with
    | VBoolean true, y -> VBoolean true
    | VBoolean false, y ->
      match eval y env with
      | VBoolean y' -> VBoolean y'
      | _ -> error y "Boolean" env
    | _ -> error y "Boolean" env
  | And (x, y) ->
    match (eval x env, y) with
    | VBoolean false, y -> VBoolean false
    | VBoolean true, y ->
      match eval y env with
      | VBoolean y' -> VBoolean y'
      | _ -> error y "Boolean" env
    | _ -> error y "Boolean" env
  | Greater (x, y) ->
    match (eval x env, eval y env) with
    | VInteger x', VInteger y' -> VBoolean (x' > y')
    | VInteger x', y' -> error y "Integer" env
    | VDouble x', VDouble y' -> VBoolean (x' > y')
    | VDouble x', y' -> error y "Double" env
    | _ -> error y "Integer or Double" env
  | Less (x, y) ->
    match (eval x env, eval y env) with
    | VInteger x', VInteger y' -> VBoolean (x' < y')
    | VInteger _, _ -> error y "Integer" env
    | VDouble x', VDouble y' -> VBoolean (x' < y')
    | VDouble _, _ -> error y "Double" env
    | _ -> error y "Integer or Double" env
  | Plus (x, y) ->
    match (eval x env, eval y env) with
    | VInteger x', VInteger y' -> VInteger (x' + y')
    | VInteger _, _ -> error y "Integer" env
    | VDouble x', VDouble y' -> VDouble (x' + y')
    | VDouble _, _ -> error y "Double" env
    | VString x', VString y' -> VString (unquote_string x' + unquote_string y')
    | VString _, _ -> error y "String" env
    | _ -> error y "Integer or Double or String" env
  | Minus (x, y) ->
    match (eval x env, eval y env) with
    | VInteger x', VInteger y' -> VInteger (x' - y')
    | VInteger _, _ -> error y "Integer" env
    | VDouble x', VDouble y' -> VDouble (x' - y')
    | VDouble _, _ -> error y "Double" env
    | _ -> error y "Integer or Double" env
  | Aster (x, y) ->
    match (eval x env, eval y env) with
    | VInteger x', VInteger y' -> VInteger (x' * y')
    | VInteger _, _ -> error y "Integer" env
    | VDouble x', VDouble y' -> VDouble (x' * y')
    | VDouble _, _ -> error y "Double" env
    | _ -> error y "Integer or Double" env
  | Slash (x, y) ->
    match (eval x env, eval y env) with
    | VInteger x', VInteger y' -> VInteger (x' / y')
    | VInteger _, _ -> error y "Integer" env
    | VDouble x', VDouble y' -> VDouble (x' / y')
    | VDouble _, _ -> error y "Double" env
    | _ -> error y "Integer or Double" env
  | Negate x ->
    match eval x env with
    | VInteger x' -> VInteger -x'
    | VDouble x' -> VDouble -x'
    | _ -> error x "Integer or Double" env

type QExpr<'a> = Microsoft.FSharp.Quotations.Expr<'a>

let rec to_expr (expr : Expr) : QExpr<(string -> Value) -> Value> =
  match expr with
  | Identifier x -> <@ fun (env : string -> Value) -> env x @>
  | Boolean x -> <@ fun (env : string -> Value) -> VBoolean x @>
  | Integer x -> <@ fun (env : string -> Value) -> VInteger x @>
  | Double x -> <@ fun (env : string -> Value) -> VDouble x @>
  | String x -> <@ fun (env : string -> Value) -> VString (x.Replace("\"", String.Empty)) @>
  | Paren x -> <@ %(to_expr x) @>
  | Or (x, y) ->
    match (x, y) with
    | Boolean true, _ -> <@ fun (env : string -> Value) -> VBoolean true @>
    | Boolean false, y -> <@ %(to_expr y) @>
    | _ ->
      let x' = to_expr x
      let y' = to_expr y
      <@
        fun (env : string -> Value) -> 
        match (%x')(env) with
        | VBoolean true -> VBoolean true
        | VBoolean false ->
          match (%y')(env) with
          | VBoolean y'' -> VBoolean y''
          | _ -> error y "Boolean" env
        | _ -> error y "Boolean" env
      @>
  | And (x, y) ->
    match (x, y) with
    | Boolean false, _ -> <@ fun (env : string -> Value) -> VBoolean false @>
    | Boolean true, y -> <@ %(to_expr y) @>
    | _ ->
      let x' = to_expr x
      let y' = to_expr y
      <@
        fun (env : string -> Value) -> 
        match (%x')(env) with
        | VBoolean false -> VBoolean false
        | VBoolean true ->
          match (%y')(env) with
          | VBoolean y'' -> VBoolean y''
          | _ -> error y "Boolean" env
        | _ -> error y "Boolean" env
      @>
  | Greater (x, y) ->
      let x' = to_expr x
      let y' = to_expr y
      <@
        fun (env : string -> Value) -> 
        match ((%x') (env), (%y') (env)) with
        | VInteger x'', VInteger y'' -> VBoolean (x'' > y'')
        | VInteger x'', y'' -> error y "Integer" env
        | VDouble x'', VDouble y'' -> VBoolean (x'' > y'')
        | VDouble x'', y'' -> error y "Double" env
        | _ -> error y "Integer or Double" env
      @>
  | _ -> <@ failwith "" @>


let vars = Map.ofList ["x", VInteger 1; "y", VDouble 2.0; "z", VBoolean true]
let env (x : string) : Value =
  let x' = x.Replace("[", String.Empty).Replace("]", String.Empty) //Crap
  match vars.TryFind x' with
  | None -> VError ("Unbound variable " + x)
  | Some v -> v

let rec repl () =
  printf ">"
  match Console.ReadLine() with
  | "q" -> ()
  | s ->
    let lexbuf = LexBuffer<_>.FromString(s)
    try
      let expr = Parser.start Lexer.tokenize lexbuf
      let value = eval expr env
      printfn "%s = %s" (value |> value_to_type |> type_to_string) (value |> value_to_string)

      let qexpr = to_expr expr
      printfn "%s" (qexpr.ToString())
      //printfn "%s" env |> qexpr |> value_to_string
    with ex ->
      let pos = lexbuf.EndPos
      printfn "Error at line %d, character %d: %s" pos.Line pos.Column ex.Message

    repl ()

repl ()