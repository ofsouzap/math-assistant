%{
  open! Core
  open Math_assistant
  open Hole_expr.Multihole_expr
%}

%token <int> INT
%token <string> VAR
%token HOLE
%token E PI
%token PLUS MINUS TIMES DIV POW LN
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS
%left TIMES DIV
%right POW

%start <t> main

%type <t> hole_expr
%type <t> hole_expr_self_contained
%type <Constant.t> constant

%%

main:
  | e = hole_expr EOF { e }
  ;

hole_expr:
  | hole_expr_self_contained { $1 }
  | hole_expr PLUS hole_expr { Add ($1, $3) }
  | hole_expr MINUS hole_expr { Sub ($1, $3) }
  | hole_expr TIMES hole_expr { Mul ($1, $3) }
  | hole_expr DIV hole_expr { Frac ($1, $3) }
  | hole_expr POW hole_expr { Pow { base = $1; exponent = $3 } }
  | LN hole_expr_self_contained { Ln $2 }
;

hole_expr_self_contained:
  | LPAREN hole_expr RPAREN { $2 }
  | HOLE { Hole }
  | constant { Constant $1 }
  | VAR { Var (Variable.of_string_exn $1) }
;

constant:
  | MINUS INT { Constant.Int_lit (-$2) }
  | INT { Constant.Int_lit $1 }
  | E { Constant.E }
  | PI { Constant.Pi }
;
