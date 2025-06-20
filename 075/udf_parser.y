%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!

#define NIL (new cdk::nil_node(LINE))

%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;          /* integer value */
  double                d;
  std::string          *s;          /* symbol name or string literal */
  cdk::basic_node      *node;       /* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;
  udf::block_node      *block;
  std::vector<size_t>  *sizes;
};

%token tAND tOR tNE tLE tGE tOBJECTS tSIZEOF tCONTRACT tCAPACITY tRANK tDIMS tDIM tRESHAPE
%token tPUBLIC tPRIVATE tFORWARD
%token tTYPE_STRING tTYPE_INT tTYPE_REAL tTYPE_POINTER tTYPE_AUTO tTYPE_TENSOR tTYPE_VOID
%token tFOR tIF tWRITE tWRITELN tINPUT tELIF tELSE
%token tBREAK tCONTINUE tRETURN

%nonassoc tIFX
%nonassoc tIF 
%nonassoc tELSE tELIF
%right '='
%left tOR
%left tAND
%right '~'
%left tEQ tNE
%left tGE tLE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%left tCONTRACT
%left '@'
%left '.'
%nonassoc tUNARY '?'
%nonassoc '[' '('

%type<type> data_type void_type

%token<i> tINTEGER
%token<d> tREAL
%token<s> tID tSTRING
%token<expression> tNULLPTR

%type<node> stmt return iffalse
%type<sequence> file stmts opt_stms
%type<sequence> exprs opt_exprs
%type<expression> expr integer real opt_initializer
%type<lvalue> lval
%type<block> block

%type<node> dec argdec fordec vardec fundec fundef
%type<sequence> decls argdecs fordecs vardecs opt_vardecs
%type<sequence> opt_forinit

%type<s> string
%type<sizes> vector_dims

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : /* empty */   { compiler->ast($$ = new cdk::sequence_node(LINE)); }
     | decls         { compiler->ast($$ = $1); }

decls :       dec { $$ = new cdk::sequence_node(LINE, $1);     }
      | decls dec { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

dec  : vardec ';' { $$ = $1; }
     | fundec     { $$ = $1; }
     | fundef     { $$ = $1; }
     ;

vardec : tFORWARD data_type  tID                         { $$ = new udf::variable_declaration_node(LINE, tPUBLIC,  $2, *$3, nullptr); }
       | tPUBLIC  data_type  tID opt_initializer         { $$ = new udf::variable_declaration_node(LINE, tPUBLIC,  $2, *$3, $4); }
       |          data_type  tID opt_initializer         { $$ = new udf::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $3); }
       | tPUBLIC  tTYPE_AUTO tID '=' expr                { $$ = new udf::variable_declaration_node(LINE, tPUBLIC, nullptr, *$3, $5); }
       |          tTYPE_AUTO tID '=' expr                { $$ = new udf::variable_declaration_node(LINE, tPRIVATE, nullptr, *$2, $4); }
       ;

fundec   :          data_type  tID '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPRIVATE, $1, *$2, $4); }
         | tFORWARD data_type  tID '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPUBLIC,  $2, *$3, $5); }
         | tPUBLIC  data_type  tID '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPUBLIC,  $2, *$3, $5); }
         |          tTYPE_AUTO tID '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPRIVATE, nullptr, *$2, $4); }
         | tFORWARD tTYPE_AUTO tID '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPUBLIC,  nullptr, *$3, $5); }
         | tPUBLIC  tTYPE_AUTO tID '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPUBLIC,  nullptr, *$3, $5); }
         |          void_type  tID '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPRIVATE, $1, *$2, $4); }
         | tFORWARD void_type  tID '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPUBLIC,  $2, *$3, $5); }
         | tPUBLIC  void_type  tID '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPUBLIC,  $2, *$3, $5); }
         ;

fundef   :         data_type  tID '(' argdecs ')' block { $$ = new udf::function_definition_node(LINE, tPRIVATE, $1, *$2, $4, $6); }
         | tPUBLIC data_type  tID '(' argdecs ')' block { $$ = new udf::function_definition_node(LINE, tPUBLIC,  $2, *$3, $5, $7); }
         |         tTYPE_AUTO tID '(' argdecs ')' block { $$ = new udf::function_definition_node(LINE, tPRIVATE, nullptr, *$2, $4, $6); }
         | tPUBLIC tTYPE_AUTO tID '(' argdecs ')' block { $$ = new udf::function_definition_node(LINE, tPUBLIC,  nullptr, *$3, $5, $7); }
         |         void_type  tID '(' argdecs ')' block { $$ = new udf::function_definition_node(LINE, tPRIVATE, $1, *$2, $4, $6); }
         | tPUBLIC void_type  tID '(' argdecs ')' block { $$ = new udf::function_definition_node(LINE, tPUBLIC,  $2, *$3, $5, $7); }
         ;

argdecs  : /* empty */         { $$ = new cdk::sequence_node(LINE);  }
         |             argdec  { $$ = new cdk::sequence_node(LINE, $1);     }
         | argdecs ',' argdec  { $$ = new cdk::sequence_node(LINE, $3, $1); }
         ;

argdec   : data_type tID { $$ = new udf::variable_declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); }
         ;

stmts : stmt       { $$ = new cdk::sequence_node(LINE, $1); }
      | stmts stmt { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

stmt : expr ';'                         { $$ = new udf::evaluation_node(LINE, $1); }
     | tWRITE exprs ';'                  { $$ = new udf::print_node(LINE, $2, false); }
     | tWRITELN exprs ';'                  { $$ = new udf::print_node(LINE, $2, true); }
     | tBREAK                           { $$ = new udf::break_node(LINE);  }
     | tCONTINUE                        { $$ = new udf::continue_node(LINE); }
     | tIF '(' expr ')' stmt %prec tIFX   { $$ = new udf::if_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt iffalse    { $$ = new udf::if_else_node(LINE, $3, $5, $6); }
     | tFOR '(' opt_forinit ';' opt_exprs ';' opt_exprs ')' stmt  { $$ = new udf::for_node(LINE, $3, $5, $7, $9); }
     | return                           { $$ = $1; }
     | block                            { $$ = $1; }
     ;

iffalse   : tELSE stmt                          { $$ = $2; }
          | tELIF '(' expr ')' stmt %prec tIFX  { $$ = new udf::if_node(LINE, $3, $5); }
          | tELIF '(' expr ')' stmt iffalse     { $$ = new udf::if_else_node(LINE, $3, $5, $6); }
          ;

expr : integer               { $$ = $1; }
     | real                  { $$ = $1; }
     | string                { $$ = new cdk::string_node(LINE, $1); }
     | tNULLPTR              { $$ = new udf::nullptr_node(LINE); }
     | '-' expr %prec tUNARY { $$ = new cdk::unary_minus_node(LINE, $2); }
     | '+' expr %prec tUNARY { $$ = new cdk::unary_plus_node(LINE, $2); }
     | expr '+' expr         { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr         { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr         { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr         { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr         { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr         { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr         { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr         { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr         { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr         { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr         { $$ = new cdk::eq_node(LINE, $1, $3); }
     | expr tAND expr        { $$ = new cdk::and_node(LINE, $1, $3); }
     | expr tOR  expr        { $$ = new cdk::or_node (LINE, $1, $3); }
     /* TENSOR */
     | expr tCONTRACT expr   { $$ = new udf::tensor_contract_node(LINE,$1,$3);}
     | expr '.' tCAPACITY    { $$ = new udf::tensor_capacity_node(LINE,$1);}
     | expr '.' tRANK        { $$ = new udf::tensor_rank_node(LINE,$1);}
     | expr '.' tDIMS        { $$ = new udf::tensor_dims_node(LINE,$1);}
     | expr '.' tDIM '(' expr ')' { $$ = new udf::tensor_dim_node(LINE,$1,$5);}
     | expr '.' tRESHAPE '(' exprs ')' { $$ = new udf::tensor_reshape_node(LINE,$1,$5);}
     | '~' expr              { $$ = new cdk::not_node(LINE, $2); }
     | tINPUT                { $$ = new udf::input_node(LINE); }
     | tID '(' opt_exprs ')' { $$ = new udf::function_call_node(LINE, *$1, $3);}
     | tSIZEOF '(' expr ')'  { $$ = new udf::sizeof_node(LINE, $3); }
     | '(' expr ')'          { $$ = $2; }
     | '[' opt_exprs ']'     { $$ = new udf::tensor_node(LINE, $2); }
     | tOBJECTS '(' expr ')' { $$ = new udf::mem_alloc_node(LINE, $3); }
     | lval                  { $$ = new cdk::rvalue_node(LINE, $1); }
     | lval '=' expr         { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | lval '?'              { $$ = new udf::address_of_node(LINE, $1); }
     ;

exprs : expr             { $$ = new cdk::sequence_node(LINE, $1);     }
      | exprs ',' expr   { $$ = new cdk::sequence_node(LINE, $3, $1); }
      ;

lval : tID                                { $$ = new cdk::variable_node(LINE, $1); delete $1;}
     | tID '@' '(' exprs ')'
     {
     $$ = new udf::tensor_index_node(LINE, new cdk::rvalue_node(LINE, new cdk::variable_node(LINE, *$1)), $4);
     delete $1;
     }
     | '(' expr ')' '@' '(' exprs ')'
     {
     $$ = new udf::tensor_index_node(LINE, $2, $6);
     }
     | tID '(' opt_exprs ')' '@' '(' exprs ')'
     { 
     $$ = new udf::tensor_index_node(LINE, new udf::function_call_node(LINE, *$1, $3), $7); 
     }
     | lval '[' expr ']'                  { $$ = new udf::pointer_index_node(LINE, new cdk::rvalue_node(LINE, $1), $3); }
     | '(' expr ')' '[' expr ']'          { $$ = new udf::pointer_index_node(LINE, $2, $5); }
     | tID '(' opt_exprs ')' '[' expr ']' { $$ = new udf::pointer_index_node(LINE, new udf::function_call_node(LINE, *$1, $3), $6); }
     ;

block    : '{' opt_vardecs opt_stms '}'  { $$ = new udf::block_node(LINE, $2, $3); }
         ;

vardecs      : vardec ';'          { $$ = new cdk::sequence_node(LINE, $1);     }
             | vardecs vardec ';' { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

opt_vardecs  : /* empty */ { $$ = NULL; }
             | vardecs     { $$ = $1; }
             ;

fordec          : data_type tID '=' expr { $$ = new udf::variable_declaration_node(LINE, tPRIVATE,  $1, *$2, $4); }
                ;
              
fordecs         :             fordec { $$ = new cdk::sequence_node(LINE, $1);     }
                | fordecs ',' fordec { $$ = new cdk::sequence_node(LINE, $3, $1); }
                ;

opt_forinit     :          { $$ = new cdk::sequence_node(LINE, NIL); }
                | tTYPE_AUTO tID '=' expr {
                   $$ = new cdk::sequence_node(LINE, new udf::variable_declaration_node(LINE, tPRIVATE,nullptr,*$2,$4));
                   delete $2;
                }
                | exprs    { $$ = new cdk::sequence_node(LINE, $1); }
                | fordecs  { $$ = $1; }
                ;

data_type    : tTYPE_STRING                     { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING);  }
             | tTYPE_INT                        { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT);     }
             | tTYPE_REAL                       { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE);  }
             | tTYPE_POINTER '<' data_type '>'  { $$ = cdk::reference_type::create(4, $3); }
             | tTYPE_POINTER '<' tTYPE_AUTO '>' { $$ = cdk::reference_type::create(4, nullptr); }
             | tTYPE_TENSOR '<' vector_dims '>' { $$ = cdk::tensor_type::create(*$3); delete $3; }
             ;

void_type : tTYPE_VOID                       { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
          ;

vector_dims: tINTEGER                 { $$ = new std::vector<size_t>(); $$->push_back($1); }
          | vector_dims ',' tINTEGER  { $$ = $1; $$->push_back($3); }
          ;

opt_initializer  : /* empty */         { $$ = nullptr; /* must be nullptr, not NIL */ }
                 | '=' expr            { $$ = $2; }
                 ;

opt_exprs : /* empty */         { $$ = new cdk::sequence_node(LINE); }
          | exprs                   { $$ = $1; }
          ;

opt_stms : /* empty */  { $$ = new cdk::sequence_node(LINE); }
         | stmts        { $$ = $1; }
         ;

return : tRETURN      ';'   { $$ = new udf::return_node(LINE, nullptr); }
       | tRETURN expr ';'   { $$ = new udf::return_node(LINE, $2); }
       ;

integer         : tINTEGER                      { $$ = new cdk::integer_node(LINE, $1); };
real            : tREAL                         { $$ = new cdk::double_node(LINE, $1); };
string          : tSTRING                       { $$ = $1; }
                | string tSTRING                { $$ = $1; $$->append(*$2); delete $2; }
                ;
%%
