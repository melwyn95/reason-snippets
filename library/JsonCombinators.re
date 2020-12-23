open Angstrom;

type t =
  | Number(float)
  | Boolean(bool)
  | String(string)
  | Null
  | Object(list((string, t)))
  | Array(list(t));

let comma = char(',');
let colon = char(':');

/* Primitive Parsers */
let nullParser = string("null") *> return(Null);
let trueParser = string("true") *> return(Boolean(true));
let falseParser = string("false") *> return(Boolean(false));
let boolParser = trueParser <|> falseParser;

let numberParser =
  take_while1(
    fun
    | '0' .. '9'
    | '.' => true
    | _ => false,
  )
  >>| (n => Number(float_of_string(n)));

let stringParser =
  char('"') *> take_while1(c => c != '"') <* char('"') >>| (s => String(s));

let keyParser = char('"') *> take_while1(c => c != '"') <* char('"');

let pair = (a, b) => (a, b);

let parser =
  fix(parse => {
    let arrayParser =
      char('[') *> sep_by(comma, parse) <* char(']') >>| (a => Array(a));
    let member = lift2(pair, keyParser <* colon, parse);
    let objectParser =
      char('{') *> sep_by(comma, member) <* char('}') >>| (o => Object(o));

    peek_char_fail
    >>= (
      c =>
        switch (c) {
        | '"' => stringParser
        | 't'
        | 'f' => boolParser
        | 'n' => nullParser
        | '[' => arrayParser
        | '{' => objectParser
        | _ => numberParser
        }
    );
  });

let parse = jsonString =>
  switch (parse_string(~consume=All, parser, jsonString)) {
  | Ok(t) => t
  | Error(msg) => failwith(msg)
  };

let rec stringify = jsont => {
  let memberString = ((key, value)) =>
    "\"" ++ key ++ "\"" ++ ":" ++ stringify(value);
  switch (jsont) {
  | Number(n) => string_of_float(n)
  | Boolean(b) => string_of_bool(b)
  | String(s) => "\"" ++ s ++ "\""
  | Null => "null"
  | Object(xys) =>
    "{" ++ (List.map(memberString, xys) |> String.concat(",")) ++ "}"
  | Array(xs) =>
    "[" ++ (List.map(stringify, xs) |> String.concat(",")) ++ "]"
  };
};