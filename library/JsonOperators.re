open Angstrom;
open Angstrom.Let_syntax;

type t =
  | Number(float)
  | Boolean(bool)
  | String(string)
  | Null
  | Object(list((string, t)))
  | Array(list(t));

let comma = char(',');
let colon = char(':');

let nullParser = {
  let+ _ = string("null");
  Null;
};
let trueParser = {
  let+ _ = string("true");
  Boolean(true);
};
let falseParser = {
  let+ _ = string("false");
  Boolean(false);
};
let boolParser = choice([trueParser, falseParser]);

let digits =
  take_while1(
    fun
    | '0' .. '9' => true
    | _ => false,
  );

let numberParser = {
  let+ whole = digits
  and+ _ = option('.', char('.'))
  and+ frac = option("0", digits);
  Number(float_of_string(whole ++ "." ++ frac));
};

let stringParser = {
  let+ _ = char('"')
  and+ s = take_while1(c => c != '"')
  and+ _ = char('"');
  String(s);
};

let keyParser = {
  let+ _ = char('"')
  and+ key = take_while1(c => c != '"')
  and+ _ = char('"');
  key;
};

let pair = (a, b) => (a, b);

let parser =
  fix(parse => {
    let arrayParser = {
      let+ _ = char('[')
      and+ t = sep_by(comma, parse)
      and+ _ = char(']');
      Array(t);
    };

    let member = {
      let+ k = keyParser
      and+ _ = colon
      and+ v = parse;
      pair(k, v);
    };

    let objectParser = {
      let+ _ = char('{')
      and+ members = sep_by(comma, member)
      and+ _ = char('}');
      Object(members);
    };

    let* c = peek_char_fail;

    switch (c) {
    | '"' => stringParser
    | 't'
    | 'f' => boolParser
    | 'n' => nullParser
    | '[' => arrayParser
    | '{' => objectParser
    | _ => numberParser
    };
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