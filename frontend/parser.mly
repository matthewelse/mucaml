%{
  open! Ox 
  open! Import
  module Expr = Ast.Expr
  module Toplevel = Ast.Toplevel

  let mkloc start_pos end_pos = Location.of_lex start_pos end_pos
  let mkid s = Identifier.of_string s
%}

%token <int> INT32
%token <int> INT64
%token <bool> BOOL
%token <string> VAR
%token <string> TYPE_VAR
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
%token COLON COMMA DOT
%token EOF
%token EXTERNAL

%nonassoc IN
%nonassoc LET
%nonassoc FUN
%nonassoc IF
%left EQ NE
%left LT GT LE GE
%left PLUS MINUS
%left TIMES DIV
%right UNARY_MINUS
%nonassoc INT32 INT64 BOOL VAR LPAREN
%nonassoc APP

%type <Ast.t> prog
%type <Ast.Expr.t> expr
%type <Identifier.t Located.t * Type.t option> param
%type <(Identifier.t Located.t * Type.t option) Nonempty_list.t> param_list
%type <Type.t> type_annot type_name constrained_type atomic_type
%type <Type.t Located.t> type_annot_loc
%type <Type.Constraint.t> type_constraint
%type <Type.Constraint.t list> type_constraint_list 

%start prog

%%

let prog :=
  ~ = toplevel*; EOF; <>

let toplevel :=
  | LET; name = VAR; ~ = param_list; return_type = type_annot?; EQ; body = expr;
    { let loc = mkloc $startpos $endpos in
      let return_type = Option.map return_type ~f:(fun t -> ({ txt = t; loc = mkloc $startpos(return_type) $endpos(return_type) } : Type.t Located.t)) in
      Toplevel.Function { name = { txt = mkid name; loc = mkloc $startpos(name) $endpos(name) }; params = param_list; return_type; body; loc } }
  | EXTERNAL; LPAREN; op = binop; RPAREN; type_ = type_annot; EQ; c_name = STRING;
    { let loc = mkloc $startpos $endpos in
  Toplevel.External { name = { txt = mkid op; loc = mkloc $startpos(op) $endpos(op) }; type_ = ({ txt = type_; loc = mkloc $startpos(type_) $endpos(type_) } : Type.t Located.t); c_name = { txt = c_name; loc = mkloc $startpos(c_name) $endpos(c_name) }; loc } 
    }
  | EXTERNAL; name = VAR; type_ = type_annot; EQ; c_name = STRING;
    { let loc = mkloc $startpos $endpos in
  Toplevel.External { name = { txt = mkid name; loc = mkloc $startpos(name) $endpos(name) }; type_ = ({ txt = type_; loc = mkloc $startpos(type_) $endpos(type_) } : Type.t Located.t); c_name = { txt = c_name; loc = mkloc $startpos(c_name) $endpos(c_name) }; loc } 
    }

let expr :=
  | n = INT32;                                
    { { Expr.desc = Literal (Int32 (I32.of_int_exn n)); loc = mkloc $startpos $endpos } }
  | n = INT64;                                
    { { Expr.desc = Literal (Int64 (I64.of_int n)); loc = mkloc $startpos $endpos } }
  | b = BOOL;                                 
    { { Expr.desc = Literal (Bool b); loc = mkloc $startpos $endpos } }
  | s = STRING;                               
    { { Expr.desc = Literal (String s); loc = mkloc $startpos $endpos } }
  | v = VAR;                                  
    { { Expr.desc = Var (mkid v); loc = mkloc $startpos $endpos } }
  | LPAREN; RPAREN;                           
    { { Expr.desc = Literal Unit; loc = mkloc $startpos $endpos } }
  | UNARY_MINUS; ~ = expr;                    
    { let zero_lit = { Expr.desc = Literal (Int32 #0l); loc = mkloc $startpos $startpos } in
      { Expr.desc = App { func = { Expr.desc = Var (mkid "-"); loc = mkloc $startpos $startpos }; args = [ zero_lit; expr ] }; loc = mkloc $startpos $endpos } }
  | l = expr; op = binop; r = expr;            
    { { Expr.desc = App { func = { Expr.desc = Var (mkid op); loc = mkloc $startpos(op) $endpos(op) }; args = [l; r] }; loc = mkloc $startpos $endpos } }
  | LET; var = VAR; type_ = type_annot?; EQ; value = expr; IN; body = expr;
            { { Expr.desc = Let { var = { txt = mkid var; loc = mkloc $startpos(var) $endpos(var) }; type_ = Option.map type_ ~f:(fun t -> ({ txt = t; loc = mkloc $startpos(type_) $endpos(type_) } : Type.t Located.t)); value; body }; loc = mkloc $startpos $endpos } }
  | LET; REC; var = VAR; type_ = option(type_annot_loc); EQ; value = expr; IN; body = expr;
          { { Expr.desc = Letrec { var = { txt = mkid var; loc = mkloc $startpos(var) $endpos(var) }; type_; value; body }; loc = mkloc $startpos $endpos } }
  | IF; cond = expr; THEN; if_true = expr; ELSE; if_false = expr; %prec IF
    { { Expr.desc = If { condition = cond; if_true; if_false }; loc = mkloc $startpos $endpos } }
  | FUN; ~ = param_list; DARROW; ~ = expr; %prec FUN   
    { { Expr.desc = Fun { params = param_list; body = expr }; loc = mkloc $startpos $endpos } }
  | f = expr; arg = expr; %prec APP           
    { { Expr.desc = App { func = f; args = [ arg ] }; loc = mkloc $startpos $endpos } }
  | LPAREN; ~ = expr; RPAREN;                     
    <> 

let param_list :=
  | ~ = nonempty_list(param); <Nonempty_list.of_list_exn>

let param :=
  | var = VAR; 
    { (({ txt = mkid var; loc = mkloc $startpos(var) $endpos(var) } : _ Located.t), None) }
  | LPAREN; var = VAR; type_annot = option(type_annot); RPAREN;
    { (({ txt = mkid var; loc = mkloc $startpos(var) $endpos(var) } : _ Located.t), type_annot) }

let type_annot_loc :=
  | ty = type_annot; {
    { txt = ty; loc = mkloc $startpos(ty) $endpos(ty) }
  }

let type_annot :=
  | ~ = preceded(COLON, type_name); <>

let type_name:=
  | ~ = atomic_type; <>
  | left = atomic_type; ARROW; right = type_name;
    { 
      match right with
      | Type.Fun (args, ret_type) -> Type.Fun (Nonempty_list.cons left args, ret_type)
        | _ -> Type.Fun ([ left ], right)
    }

let atomic_type :=
  | var = VAR; { Type.(Base (Base.of_string var)) }
  | type_var = TYPE_VAR; { Type.Var (String.drop_prefix type_var 1) }
  | LPAREN; ~ = type_name; RPAREN; <>
  | ~ = constrained_type; <>

let constrained_type :=
  | LPAREN; constraints = type_constraint_list; RPAREN; DOT; ty = type_name;
    { Type.Constrained (constraints, ty) }

let type_constraint :=
  | var = TYPE_VAR; COLON; type_class = VAR;
    { 
      Type.Constraint.{ var; type_class } 
    }

let type_constraint_list :=
  | ~ = separated_nonempty_list(COMMA, type_constraint); <>

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

