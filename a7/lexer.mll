{
  open Parser
}

let dot = '.'
let rule_def = ":-"
let comma = ','
let open_brace = '('
let close_brace = ')'
let open_sq = '['
let close_sq = ']'
let eol = '\n'
let space = [' ''\t']+
let var = (['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*)
let constructor = (['a'-'z']['a'-'z''A'-'Z''0'-'9''_']*)
let constants = "\""[^'\"']+"\""

let pipe = '|'
let underscore = '_'

rule read =
parse
    | dot   { EORF }
    | rule_def   { Rule_def }
    | var as v { V v}
    | constructor as const   { Constructor const }
    | constants as c { Const c}
    | comma   { Comma }
    | open_brace   { Open_Brace }
    | close_brace   { Close_Brace }
    | open_sq   { Open_Sq }
    | close_sq   { Close_Sq }
    | space    { read lexbuf }
    | eol   { read lexbuf }
    | eof   { EOF }

    | pipe  { Pipe }
    | underscore  { Underscore }
