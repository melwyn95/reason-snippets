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
      chainl1(factor, choice([mul, div, add, sub]));
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
    char('"')
    *> take_while1(c => c != '"')
    <* char('"')
    >>| (s => String(s));

  let keyParser = char('"') *> take_while1(c => c != '"') <* char('"');

  let pair = (a, b) => (a, b);

  let parse =
    fix(parse => {
      let arrayParser =
        char('[') *> sep_by(comma, parse) <* char(']') >>| (a => Array(a));
      let member = lift2(pair, keyParser <* colon, parse);
      let objectParser =
        char('{')
        *> sep_by(comma, member)
        <* char('}')
        >>| (o => Object(o));

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
};

let jsonString = "{\"foo\":true,\"bar\":[1.2,false,{},[],null,\"here\"],\"baz\":{\"one\":[2.4],\"two\":null}}";

switch (parse_string(~consume=All, JSON.parse, jsonString)) {
| Ok(v) =>
  print_endline(
    JSON.stringify(v) == jsonString ? jsonString ++ " PASSED" : "FAILED",
  )
| Error(msg) => failwith(msg)
};

module Expr = {
  type t =
    | Int(int)
    | Add(t, t)
    | Sub(t, t)
    | Mul(t, t)
    | Div(t, t);

  let integer =
    take_while1(
      fun
      | '0' .. '9' => true
      | _ => false,
    )
    >>| (n => Int(int_of_string(n)));

  let parens = p => char('(') *> p <* char(')');

  let add = char('+');
  let sub = char('-');
  let mul = char('*');
  let div = char('/');

  let combine = (left, operator, right) => {
    switch (operator) {
    | '+' => Add(left, right)
    | '-' => Sub(left, right)
    | '*' => Mul(left, right)
    | '/' => Div(left, right)
    | ' ' => left
    | _ => failwith("ivalid operator: " ++ Char.escaped(operator))
    };
  };

  let parser =
    fix(parser => {
      let leftTerm = parens(parser) <|> integer;
      let operator = add <|> sub <|> mul <|> div <|> return(' ');
      let rightTerm = parens(parser) <|> integer <|> return(Int(0));
      lift3(combine, leftTerm, operator, rightTerm);
    });

  let parse = expr =>
    switch (parse_string(~consume=All, parser, expr)) {
    | Ok(v) => v
    | Error(msg) => failwith(msg)
    };

  let rec eval = t =>
    switch (t) {
    | Int(n) => n
    | Add(e1, e2) => eval(e1) + eval(e2)
    | Sub(e1, e2) => eval(e1) - eval(e2)
    | Div(e1, e2) => eval(e1) / eval(e2)
    | Mul(e1, e2) => eval(e1) * eval(e2)
    };
};

let expr = "((1+2)*(4+3))";
let answer = Expr.parse(expr) |> Expr.eval;

print_int(answer);
print_newline();