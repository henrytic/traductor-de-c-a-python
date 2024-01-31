%{
    #include "node.h"
    NBlock *programBlock; 

    extern int yylex();
    void yyerror(const char *s) { fprintf(stderr, "ERROR: %sn", s); }

    int lineNumber = 1;
%}

%define parse.error verbose


%union {
    Node *node;
    NBlock *block;
    NExpression *expr;
    NStatement *stmt;
    NIdentifier *ident;
    NVariableDeclaration *var_decl;
    std::vector<NVariableDeclaration*> *varvec;
    std::vector<NExpression*> *exprvec;
    std::string *string;
    int token;
    ExprType type;
}


%token <string> TIDENTIFIER TINTEGER TDOUBLE TCHAR TSTRING
%token <token> TINTTYPE TDOUBLETYPE TCHARTYPE TVOIDTYPE
%token <token> TIF TELSE TFOR TWHILE TRETURN
%token <token> TLPAREN TRPAREN TLBRACE TRBRACE TLBRACK TRBRACK
%token <token> TOR TAND TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL TPLUS TMINUS TMUL TDIV
%token <token> TAMPERSAND
%token <token> TCOMMA TDOT TSEMICOLON


%type <type> type
%type <ident> ident ptr   //! ptr: ONLY FOR scanf
%type <expr> numeric expr logic_expr 
%type <varvec> func_decl_args
%type <exprvec> call_args
%type <block> program stmts block
%type <stmt> stmt var_decl func_decl condition loop return array_decl
%type <token> comparison

/* Operator precedence for mathematical operators */
%right TEQUAL
%left TAND TOR
%left TCEQ TCNE TCLT TCLE TCGT TCGE
%left TPLUS TMINUS
%left TMUL TDIV

// [Lex & Yacc | Yacc If-Else Ambiguity]
%nonassoc TIFX
%nonassoc TELSE

%start program

%%

program : stmts { programBlock = $1; }
        ;
        
stmts : stmt { fprintf(stderr, "stmts->stmt\n"); $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
      | stmts stmt { fprintf(stderr, "stmts->stmts stmt\n"); $1->statements.push_back($<stmt>2); }
      ;

stmt : func_decl { fprintf(stderr, "stmt->func_decl\n"); }
     | var_decl TSEMICOLON { fprintf(stderr, "stmt->var_decl PUNTO Y COMA\n"); }
     | expr TSEMICOLON { fprintf(stderr, "stmt->expr PUNTO Y COMA\n"); $$ = new NExpressionStatement(*$1); }
     | condition { fprintf(stderr, "stmt->condición\n"); }
     | loop { fprintf(stderr, "stmt->bucle\n"); }
     | return { fprintf(stderr, "stmt->retorno\n"); }
     | array_decl TSEMICOLON { fprintf(stderr, "stmt->array_decl PUNTO Y COMA\n"); }
     ;

block : TLBRACE stmts TRBRACE { fprintf(stderr, "bloque->LLAVE IZQUIERDA stmts LLAVE DERECHA\n"); $$ = $2; }
      | TLBRACE TRBRACE { $$ = new NBlock(); }
      ;

var_decl : type ident { fprintf(stderr, "var_decl->ident ident\n"); $$ = new NVariableDeclaration($1, *$2); }
         | type ident TEQUAL expr { fprintf(stderr, "var_decl->ident ident IGUAL expr\n"); $$ = new NVariableDeclaration($1, *$2, $4); }
         ;
        
func_decl : type ident TLPAREN func_decl_args TRPAREN block 
            { $$ = new NFunctionDeclaration($1, *$2, *$4, *$6); delete $4; }
          ;
    
func_decl_args : /*blank*/  { $$ = new VariableList(); }
          | var_decl { fprintf(stderr, "func_decl_args->var_decl\n"); $$ = new VariableList(); $$->push_back($<var_decl>1); }
          | func_decl_args TCOMMA var_decl { $1->push_back($<var_decl>3); }
          ;

ident : TIDENTIFIER { std::cerr<<"ident->"<<*$1<<std::endl; $$ = new NIdentifier(*$1); delete $1; }
      ;

numeric: TINTEGER     { fprintf(stderr, "numérico->TINTEGER %s\n", $1->c_str()); $$ = new NInteger(atol($1->c_str())); delete $1; }
       | TDOUBLE      { fprintf(stderr, "numérico->TDOUBLE %s\n", $1->c_str()); $$ = new NDouble(atof($1->c_str())); delete $1; }
       | TSTRING      { fprintf(stderr, "numérico->TSTRING %s\n", $1->c_str()); $$ = new NString($1->substr(1, $1->length()-2)); delete $1; }
       | TCHAR        { fprintf(stderr, "numérico->TCHAR %s\n", $1->c_str()); $$ = new NChar((*$1)[1]); delete $1; }
       ;

type: TINTTYPE        { $$ = INT; }
    | TDOUBLETYPE     { $$ = DOUBLE; }
    | TCHARTYPE       { $$ = CHAR; }
    | TVOIDTYPE       { $$ = VOID; }
    ;

ptr : TAMPERSAND TIDENTIFIER { fprintf(stderr, "ptr->AMPERSAND ident\n"); $$ = new NIdentifier(*$2); delete $2; }

expr : logic_expr { fprintf(stderr, "expr->expr_lógica\n"); }
     | ident TEQUAL expr { fprintf(stderr, "expr->ident IGUAL expr\n"); $$ = new NAssignment(*$<ident>1, *$3); }
     | ident TLBRACK expr TRBRACK TEQUAL expr { fprintf(stderr, "array[expr] = expr\n"); $$ = new NAssignment(*$<ident>1, *$6, *$3); }
     | ident TLPAREN call_args TRPAREN { fprintf(stderr, "expr->ident PARENTESIS IZQUIERDO args_llamada PARENTESIS DERECHO\n"); $$ = new NMethodCall(*$1, *$3); delete $3; }
     | ident { fprintf(stderr, "expr->ident\n"); $<ident>$ = $1;}
     | ptr { fprintf(stderr, "expr->ptr\n"); $<ident>$ = $1; }
     | numeric { fprintf(stderr, "expr->numérico\n"); }
     | expr TPLUS expr { fprintf(stderr, "expr->expr MÁS expr\n"); $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TMINUS expr { fprintf(stderr, "expr->expr MENOS expr\n"); $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TMUL expr { fprintf(stderr, "expr->expr POR expr\n"); $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TDIV expr { fprintf(stderr, "expr->expr DIVIDIDO POR expr\n"); $$ = new NBinaryOperator(*$1, $2, *$3); }
     | TLPAREN expr TRPAREN { fprintf(stderr, "expr->PARENTESIS IZQUIERDO expr PARENTESIS DERECHO\n"); $$ = $2; }
     | ident TLBRACK expr TRBRACK { fprintf(stderr, "array ident[expr]\n"); $$ = new NArrayVariable(*$1, *$3); }
     ;

logic_expr: logic_expr TAND logic_expr { fprintf(stderr, "expr_lógica->expr_lógica Y expr_lógica\n"); $$ = new NBinaryOperator(*$1, $2, *$3); }
          | logic_expr TOR logic_expr { fprintf(stderr, "expr_lógica->expr_lógica O expr_lógica\n"); $$ = new NBinaryOperator(*$1, $2, *$3); }
          | expr comparison expr { fprintf(stderr, "expr_lógica->expr comparación expr\n"); $$ = new NBinaryOperator(*$1, $2, *$3); }
          | TLPAREN logic_expr TRPAREN { fprintf(stderr, "expr_lógica->PARENTESIS IZQUIERDO expr_lógica PARENTESIS DERECHO\n"); $$ = $2; }
          ;
    
call_args : /*blank*/  { $$ = new ExpressionList(); }
          | expr { fprintf(stderr, "args_llamada->expr\n"); $$ = new ExpressionList(); $$->push_back($1); }
          | call_args TCOMMA expr  { fprintf(stderr, "args_llamada->args_llamada COMA expr\n"); $1->push_back($3); }
          ;

comparison : TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE
           ;

condition: TIF TLPAREN logic_expr TRPAREN block %prec TIFX   { $$ = new NIfStatement(*$3, $5); }
         | TIF TLPAREN logic_expr TRPAREN block TELSE block { $$ = new NIfStatement(*$3, $5, $7); }
         ;

loop : TFOR TLPAREN expr TSEMICOLON logic_expr TSEMICOLON expr TRPAREN block { fprintf(stderr, "bucle->TFOR PARENTESIS IZQUIERDO expr PUNTO Y COMA expr_lógica PUNTO Y COMA expr PARENTESIS DERECHO bloque\n"); $$ = new NForStatement($3, $5, $7, $9); }
     | TFOR TLPAREN var_decl TSEMICOLON logic_expr TSEMICOLON expr TRPAREN block { fprintf(stderr, "bucle->TFOR PARENTESIS IZQUIERDO var_decl PUNTO Y COMA expr_lógica PUNTO Y COMA expr PARENTESIS DERECHO bloque\n"); $$ = new NForStatement($3, $5, $7, $9); }
     | TWHILE TLPAREN logic_expr TRPAREN block { fprintf(stderr, "bucle->TWHILE PARENTESIS IZQUIERDO expr_lógica PARENTESIS DERECHO bloque\n"); $$ = new NWhileStatement($3, $5); }
     ;


return : TRETURN expr TSEMICOLON { fprintf(stderr, "retorno->TRETURN expr PUNTO Y COMA\n"); $$ = new NReturnStatement($2); }
       | TRETURN TSEMICOLON { fprintf(stderr, "retorno->TRETURN PUNTO Y COMA\n"); $$ = new NReturnStatement(); }
       ;


array_decl : type ident TLBRACK expr TRBRACK { fprintf(stderr, "array_decl->type ident CORCHETE IZQUIERDO expr CORCHETE DERECHO\n"); $$ = new NArrayDeclaration($1, *$2, $4); }
           ;

%%