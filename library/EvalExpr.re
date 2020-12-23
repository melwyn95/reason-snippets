open Angstrom;

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