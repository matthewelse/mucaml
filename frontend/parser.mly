%{
  open! Ox 
  module Expr = Ast.Expr
  module Toplevel = Ast.Toplevel
  module Location = Ast.Location

  let mkloc start_pos end_pos = Location.of_lex start_pos end_pos
%}

%token <int> INT32
%token <int> INT64
%token <bool> BOOL
%token <string> VAR
%token <string> STRING
%token UNARY_MINUS
%token PLUS MINUS
%token TIMES DIV
%token LPAREN RPAREN
%token EQ NE LT GT LE GE
%token LET REC IN
%token IF THEN ELSE
%token FUN
%token ARROW DARROW
%token COLON COMMA
%token EOF
%token EXTERNAL

%nonassoc IN
%nonassoc LET
%nonassoc FUN
%nonassoc IF
%right ARROW
%left EQ NE
%left LT GT LE GE
%left PLUS MINUS
%left TIMES DIV
%right UNARY_MINUS
%nonassoc INT32 INT64 BOOL VAR LPAREN
%nonassoc APP

%type <Ast.t> prog
%type <Ast.Expr.t> expr
%type <string * Type.t> param
%type <(string * Type.t) list> param_list
%type <Type.t> type_annot type_name

%start prog

%%

let prog :=
  ~ = toplevel*; EOF; <>

let toplevel :=
  | LET; name = VAR; ~ = param_list; type_annot?; EQ; body = expr;
    { let location = mkloc $startpos $endpos in
      Toplevel.Function { name; params = param_list; body; location } }
  | EXTERNAL; name = VAR; type_ = type_annot; EQ; c_name = STRING;
    { let location = mkloc $startpos $endpos in
      Toplevel.External { name; type_; c_name; location } }

let expr :=
  | n = INT32;                                
    { Expr.Literal (Int32 (I32.of_int_exn n), mkloc $startpos $endpos) }
  | n = INT64;                                
    { Expr.Literal (Int64 (I64.of_int n), mkloc $startpos $endpos) }
  | b = BOOL;                                 
    { Expr.Literal (Bool b, mkloc $startpos $endpos) }
  | v = VAR;                                  
    { Expr.Var (v, mkloc $startpos $endpos) }
  | LPAREN; RPAREN;                           
    { Expr.Literal (Unit, mkloc $startpos $endpos) }
  | UNARY_MINUS; ~ = expr;                    
    { let zero_lit = Expr.Literal (Int32 #0l, mkloc $startpos $startpos) in
      Expr.App (Expr.Var ("-", mkloc $startpos $startpos), [ zero_lit; expr ], mkloc $startpos $endpos) }
  | l = expr; op = binop; r = expr;            
    { Expr.App (Expr.Var (op, mkloc $startpos $endpos), [l; r], mkloc $startpos $endpos) }
  | LET; var = VAR; type_ = type_annot?; EQ; value = expr; IN; body = expr;
    { Expr.Let ((var, type_), value, body, mkloc $startpos $endpos) }
  | LET; REC; var = VAR; type_ = type_annot; EQ; value = expr; IN; body = expr;
    { Expr.Letrec ((var, type_), value, body, mkloc $startpos $endpos) }
  | IF; cond = expr; THEN; if_true = expr; ELSE; if_false = expr; %prec IF
    { Expr.If (cond, if_true, if_false, mkloc $startpos $endpos) }
  | FUN; ~ = param_list; DARROW; ~ = expr; %prec FUN   
    { Expr.Fun (param_list, expr, mkloc $startpos $endpos) }
  | f = expr; arg = expr; %prec APP           
    { Expr.App (f, [ arg ], mkloc $startpos $endpos) }
  | LPAREN; ~ = expr; RPAREN;                     
    <> 

let param_list :=
  | ~ = separated_list(COMMA, param); <>

let param :=
  | var = VAR; ~ = type_annot; { (var, type_annot) }

let type_annot :=
  | ~ = preceded(COLON, type_name); <>

let type_name:=
  | var = VAR; { Type.(Base (Base.of_string var)) }
  | l = type_name; ARROW; r = type_name; { Type.Fun (l, r) }

let binop == 
  | PLUS;  { "+" }
  | MINUS; { "-" }
  | TIMES; { "*" }
  | DIV;   { "/" }
  | EQ;    { "=" }
  | NE;    { "<>" }
  | LT;    { "<" }
  | GT;    { ">" }
  | LE;    { "<=" }
  | GE;    { ">=" }

