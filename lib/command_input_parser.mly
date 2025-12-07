%{
  open! Core
  open Command_input
%}

%token <string> LNAME
%token SPACE
%token EOF

%start <Command.t> command
%%

command:
  | command_name args EOF { { Command.name = $1; args = $2 } }
  ;

command_name:
  | LNAME { Command_name.Command_name ($1) }
  ;

arg:
  | LNAME { Arg.String ($1) }
  ;

args:
  | arg SPACE args { $1 :: $3 }
  | arg { [$1] }
  ;
