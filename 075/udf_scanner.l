%option c++ prefix="udf_scanner_" outfile="udf_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <cstring>
#include <vector>
#include <sstream>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "udf_parser.tab.h"

// output stream for building string literals
static std::ostringstream strlit;

// don't change this
#define yyerror LexerError

#define CHECKED_STOI(base) { \
  try { \
    yylval.i = std::stoi(yytext, nullptr, base); \
  } catch (const std::out_of_range&) { \
    yyerror("integer overflow"); \
  } \
}

#define CHECKED_STOD { \
  try { \
    yylval.d = std::stod(yytext); \
    return tREAL; \
  } catch (const std::out_of_range&) { \
    yyerror("real overflow"); \
  } \
}

%}

SPACE     ([ \t\r]|\n)
INT       [[:digit:]]+

LETTER  [A-Za-z_]
IDENTIFIER   {LETTER}({LETTER}|[[:digit:]])*

%x X_COMMENT
%x X_STRING X_BACKSLASH X_IGNORE_STRING
%x X_HEX_INT
%%
  yydebug=1; set_debug(1);

  /* - COMMENTS - */

"//".*$

"/*"                  yy_push_state(X_COMMENT);
<X_COMMENT>"/*"       yy_push_state(X_COMMENT);
<X_COMMENT>"*/"       yy_pop_state();
<X_COMMENT>.|\n       ; 

  /* - TYPES - */

"auto"                   return tTYPE_AUTO;
"int"                    return tTYPE_INT;
"real"                   return tTYPE_REAL;
"string"                 return tTYPE_STRING;
"ptr"                    return tTYPE_POINTER;
"void"                   return tTYPE_VOID;
"tensor"                 return tTYPE_TENSOR;
[<>]                     return *yytext; 

  /* - TENSOR OPERATIONS - */
"capacity"               return tCAPACITY;
"rank"                   return tRANK;
"dims"                   return tDIMS;
"dim"                    return tDIM;
"reshape"                return tRESHAPE;

  /* - SOME EXPRESSIONS - */

"nullptr"                return tNULLPTR;
"sizeof"                 return tSIZEOF;
"objects"                 return tOBJECTS;

  /* - GLOBAL SYMBOLS - */

"public"                 return tPUBLIC;
"forward"                return tFORWARD;

  /* - CONDITIONALS - */

"if"                   return tIF;
"elif"                 return tELIF;
"else"                 return tELSE;

  /* - LOOPS - */

"for"                   return tFOR; 
"break"                 return tBREAK;
"continue"              return tCONTINUE;

  /* - EXPRESSION OPERATORS - */

[\[\](),?.]             return *yytext; 
[-+*/%~<>@=]          return *yytext;
"=="                   return tEQ;
"!="                   return tNE;
"<="                   return tLE;
">="                   return tGE;
"&&"                   return tAND;
"||"                   return tOR;
"**"                   return tCONTRACT;
"input"                return tINPUT;

  /* - SEPARATORS, RETURN, WRITE - */

[#\[\](){};:!,]        return *yytext;
"return"               return tRETURN;
"write"                return tWRITE;
"writeln"              return tWRITELN;

  /* - INTEGERS - */

"0x"                   yy_push_state(X_HEX_INT);

<X_HEX_INT>[[:xdigit:]]+      CHECKED_STOI(16); yy_pop_state(); return tINTEGER;
<X_HEX_INT>.|\n               std::cerr << "WARNING: line " << lineno() << ": bad hexadecimal data!" << std::endl; exit(1);

{INT}                         CHECKED_STOI(10); return tINTEGER;

  /* - REALS - */

([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)?           CHECKED_STOD;

  /* - IDENTIFIERS (NAMES) - */

{IDENTIFIER}  yylval.s = new std::string(yytext); return tID;

  /* - STRINGS - */

"\""                           yy_push_state(X_STRING);
<X_STRING>\\                   yy_push_state(X_BACKSLASH);
<X_STRING>"\""                 {
                                 yylval.s = new std::string(strlit.str());
                                 strlit.str("");
                                 yy_pop_state();
                                 return tSTRING;
                               }
<X_STRING>.                    strlit << *yytext;

<X_BACKSLASH>0                 yy_push_state(X_IGNORE_STRING);                           
<X_BACKSLASH>n                 strlit << '\n'; yy_pop_state();
<X_BACKSLASH>r                 strlit << '\r'; yy_pop_state();
<X_BACKSLASH>t                 strlit << '\t'; yy_pop_state();
<X_BACKSLASH>\\                strlit << '\\'; yy_pop_state();
<X_BACKSLASH>\"                strlit << '\"'; yy_pop_state();
<X_BACKSLASH>[[:xdigit:]]{1,2} {
                                 strlit << (char)(unsigned char)strtoul(yytext, NULL, 16);
                                 yy_pop_state();
                               }
<X_BACKSLASH>.                 strlit << *yytext; yy_pop_state();

<X_IGNORE_STRING>\"       yy_pop_state(); yy_pop_state(); yy_pop_state(); return tSTRING;
<X_IGNORE_STRING>\\\"     ;
<X_IGNORE_STRING>\\\\     ;
<X_IGNORE_STRING>\n       yyerror("newline in string");
<X_IGNORE_STRING>\0       yyerror("null byte in string");
<X_IGNORE_STRING>.        ;

  /* - OTHERS - */

[[:space:]]         
.                      yyerror(yytext);   

%%
