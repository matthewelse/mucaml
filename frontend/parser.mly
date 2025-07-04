%{
  open Ast
%}

%token <int32> INT32
%token <int64> INT64
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
%type <Ast.expr> expr
%type <string * Type.t> param
%type <(string * Type.t) list> param_list
%type <Type.t> type_annot type_name

%start prog

%%

let prog :=
  ~ = toplevel*; EOF; <>

let toplevel :=
  | LET; name = VAR; ~ = param_list; type_annot?; EQ; body = expr;
    { Function { name; params = param_list; body } }
  | EXTERNAL; name = VAR; type_ = type_annot; EQ; c_name = STRING;
    { External { name; type_; c_name } }

let expr :=
  | ~ = INT32;                                <Int32>
  | ~ = INT64;                                <Int64>
  | ~ = BOOL;                                 <Bool>
  | ~ = VAR;                                  <Var>
  | LPAREN; RPAREN;                           { Unit }
  | UNARY_MINUS; ~ = expr;                    { App (Var "-", [ Int32 0l; expr ]) }
  | l = expr; ~ = binop; r = expr;            { App (Var binop, [l; r]) }
  | LET; var = VAR; type_ = type_annot?; EQ; value = expr; IN; body = expr;
    { Let ((var, type_), value, body) }
  | LET; REC; var = VAR; type_ = type_annot; EQ; value = expr; IN; body = expr;
    { Letrec ((var, type_), value, body) }
  | IF; cond = expr; THEN; if_true = expr; ELSE; if_false = expr; %prec IF
    { If (cond, if_true, if_false) }
  | FUN; ~ = param_list; DARROW; ~ = expr; %prec FUN   
    { Fun (param_list, expr)  }
  | f = expr; arg = expr; %prec APP           { App (f, [ arg ]) }
  | LPAREN; ~ = expr; RPAREN;                     <> 

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

