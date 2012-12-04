namespace Ast

type Expr =
  | Identifier of string
  | Boolean of bool
  | Integer of int
  | Double of double
  | String of string
  | Paren of Expr
  | Or of Expr * Expr
  | And of Expr * Expr
  | Plus of Expr * Expr
  | Minus of Expr * Expr
  | Aster of Expr * Expr
  | Slash of Expr * Expr
  | Greater of Expr * Expr
  | Less of Expr * Expr
  | Negate of Expr

type Value =
  | VBoolean of bool
  | VInteger of int
  | VDouble of double
  | VString of strinf
  | VError of string

type Type =
  | TBoolean
  | TInteger
  | TDouble
  | TString
  | TError