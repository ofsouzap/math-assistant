{
  open Command_input_parser
}

let lowercase = ['a'-'z']
let lowercases = ['a'-'z']+
let uppercases = ['A'-'Z']+
let letters = lowercases | uppercases
let digits = ['0'-'9']+
let alphanums = letters | digits
let space = [' ' '\t']+

rule token = parse
  | space { SPACE }
  | (lowercase alphanums) as word { LNAME word }
  | eof { EOF }
  | _ { failwith "Unexpected character" }
