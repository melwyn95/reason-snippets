open Angstrom;

module EvalExpr = {
  let parens = p => char('(') *> p <* char(')');
  let add = char('+') *> return((a, b) => a + b);
  let sub = char('-') *> return((a, b) => a - b);
  let mul = char('*') *> return((a, b) => a * b);
  let div = char('/') *> return((a, b) => a / b);
  let integer =
    take_while1(
      fun
      | '0' .. '9' => true
      | _ => false,
    )
    >>| int_of_string;

  let chainl1 = (e, op) => {
    let rec go = acc =>
      lift2((f, x) => f(acc, x), op, e) >>= go <|> return(acc);
    e >>= (init => go(init));
  };

  let expr =
    fix(expr => {
      let factor = parens(expr) <|> integer;
      let term = chainl1(factor, mul <|> div);
      chainl1(term, add <|> sub);
    });

  let eval = (str: string): int =>
    switch (parse_string(~consume=All, expr, str)) {
    | Ok(v) => v
    | Error(msg) => failwith(msg)
    };
};

let expr = "((1+2)*(3+4))";
print_endline(
  "Eval: " ++ expr ++ " = " ++ (EvalExpr.eval(expr) |> string_of_int),
);

module JSON = {
  type t =
    | Boolean(bool, string)
    | Number(float, string)
    | String(string)
    | Null
    | Array(list(t))
    | Object(list((t, t)));

  let pair = (x, y) => (x, y);

  let lSquareBracket = char('[');
  let rSquareBracket = char(']');
  let lCurlyBracket = char('{');
  let rCurlyBracket = char('}');
  let colon = char(':');
  let comma = char(',');

  let parse = {
    fix(json => {
      let arrayParser =
        lSquareBracket
        *> sep_by(comma, json)
        >>| (vs => Array(vs))
        <* rSquareBracket;

      let stringParser =
        char('"')
        *> take_while1(c => c != '"')
        >>| (s => String(s))
        <* char('"');

      let booleanParser = {
        let trueParser = string("true") *> return(Boolean(true, "true"));
        let falseParser =
          string("false") *> return(Boolean(false, "false"));
        trueParser <|> falseParser;
      };

      let numberParser = {
        take_while1(
          fun
          | '0' .. '9'
          | '.' => true
          | _ => false,
        )
        >>| (s => Number(float_of_string(s), s));
      };

      let nullParser = string("null") *> return(Null);

      let mem = lift2(pair, stringParser <* colon, json);

      let objectParser =
        lCurlyBracket
        *> sep_by(comma, mem)
        >>| (kvs => Object(kvs))
        <* rCurlyBracket;

      peek_char_fail
      >>= (
        c => {
          switch (c) {
          | '[' => arrayParser
          | '{' => objectParser
          | '"' => stringParser
          | 't'
          | 'f' => booleanParser
          | 'n' => nullParser
          | _ => numberParser
          };
        }
      );
    });
  };

  let rec stringify = json => {
    switch (json) {
    | Boolean(_, b) => b
    | Number(_, n) => n
    | String(s) => "\"" ++ s ++ "\""
    | Null => "null"
    | Array(xs) =>
      "[" ++ (List.map(x => stringify(x), xs) |> String.concat(",")) ++ "]"
    | Object(xys) =>
      "{"
      ++ (
        xys
        |> List.map(((x: t, y: t)) => stringify(x) ++ ":" ++ stringify(y))
        |> String.concat(",")
      )
      ++ "}"
    };
  };
};

let jsonString = "{\"foo\":true,\"bar\":[1.2,false,{},[],null],\"baz\":{\"one\":[1],\"two\":null}}";

switch (parse_string(~consume=All, JSON.parse, jsonString)) {
| Ok(v) =>
  print_endline(JSON.stringify(v) == jsonString ? "PASSED" : "FAILED")
| Error(msg) => failwith(msg)
};