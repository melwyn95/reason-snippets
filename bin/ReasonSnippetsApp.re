open Angstrom;

module EvalExpr = Library.EvalExpr;
module JSONC = Library.JsonCombinators;

let expr = "((1+2)*(3+4))";
print_endline(
  "Eval: " ++ expr ++ " = " ++ (EvalExpr.eval(expr) |> string_of_int),
);

let jsonString = "{\"foo\":true,\"bar\":[1.2,false,{},[],null,\"here\"],\"baz\":{\"one\":[2.4],\"two\":null}}";
let json = JSONC.parse(jsonString);
if (JSONC.stringify(json) == jsonString) {
  print_endline("Passed!!");
};

module JSONO = Library.JsonOperators;
let jsonString = "{\"foo\":true,\"bar\":[1.2,false,{},[],null,\"here\"],\"baz\":{\"one\":[2.4],\"two\":null}}";
let json = JSONO.parse(jsonString);
if (JSONO.stringify(json) == jsonString) {
  print_endline("Passed!!");
};