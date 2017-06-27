namespace PLC.Core

open FParsec

module Calculator =
  type Oper =
    | Plus
    | Minus
    | Mul
    | Div
    | Unknow of char

  type Expr =
    | Atom of string
    | Num of float
    | Def of string * Expr
    | App of Oper * Expr * Expr

  type Env = (string * float) list

  type ExprParser = Parser<Expr, unit>

  let parseExpr, parseExprRef: ExprParser * ExprParser ref = createParserForwardedToRef<Expr, unit>() 

  let parseOper = parse {
    let! ch = anyChar
    return match ch with
           | '+' -> Plus
           | '-' -> Minus
           | '*' -> Mul
           | '/' -> Div
           | _ -> Unknow ch
  }

  let parseName = regex "[a-zA-Z0-9]+"

  let parseAtom = parse {
    let! name = parseName
    return Atom name
  }

  let parseNum = parse {
    let! num = pfloat
    return Num num
  }

  let parseDef = parse {
    do! pstring "(define" >>. spaces
    let! atom = parseName
    do! spaces
    let! num = parseExpr
    do! spaces .>> pstring ")"
    return Def (atom, num)
  }

  do parseExprRef := parseDef
                 <|> parseNum
                 <|> parseAtom
                 <|> parse {
                       do! pchar '(' >>. spaces
                       let! oper = parseOper
                       do! spaces
                       let! leftExpr = parseExpr
                       do! spaces
                       let! rightExpr = parseExpr
                       do! spaces .>> pchar ')'
                       return App (oper, leftExpr, rightExpr)
                     }

  let rec eval (env:Env) (expr:Expr) =
    match expr with
    | Atom name ->
      let (name, num) = List.find (fun (x,y) -> x = name) env
      (num, env)
    | Num num ->
      (num, env)
    | Def (name, expr) ->
      let (num, newEnv) = eval env expr
      let newEnv = (name, num) :: newEnv
      (num, newEnv)
    | App (oper, leftExpr, rightExpr) ->
      let (operandLeft, newEnv) = eval env leftExpr
      let (operandRight, newEnv) = eval newEnv leftExpr
      match oper with
      | Plus -> (operandLeft + operandRight, newEnv)
      | Minus -> (operandRight - operandRight, newEnv)
      | Mul -> (operandRight - operandRight, newEnv)
      | Div -> (operandRight - operandRight, newEnv)
      | Unknow char -> failwith "unknow operator"

  let exec (res, env) expr =
    match run parseExpr expr with
    | Success(expr, _, _) ->
      eval env expr
    | Failure(errorMsg, _, _) ->
      failwith errorMsg

  let test =
    let source = "(define x 1.0)\n(+ x 1.0)"
    let stmts = source.Split '\n' |> List.ofArray
    let (res, env) = List.fold exec (0.0, List.empty) stmts
    printfn "result: %f" res
