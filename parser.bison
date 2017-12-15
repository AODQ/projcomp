%{
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <type_traits>
#include <variant>
#include "expression.hpp"


enum class RunningState {
  TestCaseDebug, // tester.d test cases
  OutputDebug, //outputdebug+bison
  Bison // bison only
};
const RunningState Running_state = RunningState::Bison;

using FrDataType = std::variant<std::string, int, float>;

template <typename... Args>
void Bison_Output(Args&&... args) {
  if ( Running_state != RunningState::TestCaseDebug )
      (std::cerr << ... << args) << '\n';
}

extern "C" int yylex();
extern "C" int yyparse();
void rerror(std::string s);
void yyerror(const char* str);
void cperror(std::string s) {
  yyerror(s.c_str());
}
%}

%union {
  int ival;
  float fval;
  const char* sval;
}

%token <ival> TINTEGER
%token <fval> TFLOAT
%token <sval> TSTRING
%token <sval> TVARIABLE
%token <sval> KEYWORD_VOID
%token <sval> KEYWORD_FLOAT
%token <sval> KEYWORD_RETURN
%token <sval> KEYWORD_INT
%token <sval> KEYWORD_WHILE
%token <sval> KEYWORD_IF
%token <sval> KEYWORD_ELSE
%token <sval> SYNTAX_ERROR

%type<sval> keyword_thing

%%

funcroll:
    funcunroll {}
  | funcroll funcunroll {}

funcunroll:
    funcdropdecl {}
  | funcdropdef  {}

funcdropdef:
  funcdropt '{' line '}' {
    in_func = false;
    Emit_ASM_Func();
  }

funcdropdecl:
  funcdropf ';' {}

funcdropt:
  keyword_thing TVARIABLE '(' ')' {
    in_func = true;
    finf_label = std::string($2);
    Reduce_Expression(BTS::Func{String_To_VarType($1), "", VarType::_unknown,
                      std::string($2)});
  }
  | keyword_thing TVARIABLE '(' keyword_thing TVARIABLE ')' {
    finf_label = std::string($2);
    Reduce_Expression(BTS::Func{String_To_VarType($1), $5,
                      String_To_VarType($4), std::string($2)});
    Add_Expr_Stack(std::string($5), ExprTime::RunTime);
    Reduce_Expression(BTS::Sensible{BTS::Declaration{String_To_VarType($4)}});
    Finish_Expr();
  }
funcdropf:
  keyword_thing TVARIABLE '(' ')' {
    finf_label = std::string($2);
    Reduce_Expression(BTS::FuncProto{String_To_VarType($1), "",
                      VarType::_unknown, std::string($2)});
  }
  | keyword_thing TVARIABLE '(' keyword_thing TVARIABLE ')' {
    finf_label = std::string($2);
    Reduce_Expression(BTS::FuncProto{String_To_VarType($1), "tempparam",
                      String_To_VarType($4), std::string($2)});
  }

keyword_thing:
    KEYWORD_VOID  { $$ = "void";   }
  | KEYWORD_INT   { $$ = "int";    }
  | KEYWORD_FLOAT { $$ = "float"; }

line:
    single_line {}
  | line single_line {}

single_line:
    func_call ';'      { Finish_Expr();  }
  | var_declaration ';'{ Finish_Expr();  }
  | var_assignment ';' { Finish_Expr();  }
  | KEYWORD_RETURN expression ';' {
    Reduce_Expression(BTS::Sensible{BTS::Return{}});
    Finish_Expr();
  }
  | error ';' { yyerrok; }
  | block {}

block:
    whileblock tblock { Finish_Block(); }
  | ifblock    tblock { Finish_Block(); }
  | elseblock  tblock { Finish_Block(); }

whileblock:
  KEYWORD_WHILE '(' expression ')' {
    Reduce_Expression(BTS::Sensible{BTS::Block{BlockType::_while}});
    Finish_Expr();
  }

ifblock:
  KEYWORD_IF '(' expression ')' {
    Reduce_Expression(BTS::Sensible{BTS::Block{BlockType::_if}});
    Finish_Expr();
  }
elseblock:
  KEYWORD_ELSE {
    Reduce_Expression(BTS::Sensible{BTS::Block{BlockType::_else}});
    Finish_Expr();
  }

tblock:
    '{' line '}' {}
  | single_line {}

var_declaration:
    KEYWORD_INT expression {
      Reduce_Expression(BTS::Sensible{BTS::Declaration{VarType::_int, !in_func}});
    }
  | KEYWORD_FLOAT expression {
      Reduce_Expression(BTS::Sensible{BTS::Declaration{VarType::_float, !in_func}});
    }

var_assignment:
    TVARIABLE '=' expression {
      Reduce_Expression(BTS::Assignment{std::string($1)});
    }

func_call:
    TVARIABLE '(' expression ')' {
      Reduce_Expression(BTS::FuncCall{std::string($1)});
    }
  | TVARIABLE '(' ')' {
      Reduce_Expression(BTS::FuncCall{std::string($1)});
    }

expression:
    expr_HO { }
  | expression ',' expr_HO {}

expr_HO:
    expr_HO '&' '&' expr_LO { Reduce_Expression(BTS::FuncCall {"&&"}); }
  | expr_HO '|' '|' expr_LO { Reduce_Expression(BTS::FuncCall {"||"}); }
  | expr_LO {}

expr_LO:
      expr_LO '<' expr_AS     { Reduce_Expression(BTS::FuncCall {"<"}); }
    | expr_LO '>' expr_AS     { Reduce_Expression(BTS::FuncCall {">"}); }
    | expr_LO '<' '=' expr_AS { Reduce_Expression(BTS::FuncCall {"<="}); }
    | expr_LO '>' '=' expr_AS { Reduce_Expression(BTS::FuncCall {">="}); }
    | expr_LO '=' '=' expr_AS { Reduce_Expression(BTS::FuncCall {"=="}); }
    | expr_LO '!' '=' expr_AS { Reduce_Expression(BTS::FuncCall {"!="}); }
    | expr_AS {}

expr_AS:
      expr_AS '+' expr_MD { Reduce_Expression(BTS::FuncCall{"+"}); }
    | expr_AS '-' expr_MD { Reduce_Expression(BTS::FuncCall{"-"}); }
    | expr_MD {}

expr_MD:
      expr_MD '*' expr_PU { Reduce_Expression(BTS::FuncCall{"*"}); }
    | expr_MD '/' expr_PU { Reduce_Expression(BTS::FuncCall{"/"}); }
    | expr_MD '%' expr_PU { Reduce_Expression(BTS::FuncCall{"%"}); }
    | expr_PU {}

expr_PU:
      '(' expr_HO ')' {}
      | '-' expr_PU { Reduce_Expression(BTS::FuncCall{"-u"}); }
      | '+' expr_PU {}
      | '!' expr_PU { Reduce_Expression(BTS::FuncCall{"!"}); }
      | expr_single {}

expr_single:
    TINTEGER  { Add_Expr_Stack($1              , ExprTime::CompileTime); }
  | TFLOAT    { Add_Expr_Stack($1              , ExprTime::CompileTime); }
  | TSTRING   { Add_Expr_Stack(std::string($1) , ExprTime::CompileTime); }
  | TVARIABLE { Add_Expr_Stack(std::string($1) , ExprTime::RunTime    ); }
  | func_call {}

%%

int main ( int argc, char** argv ) {
  if ( argc > 1 ) {
    Assert(argc == 3, "Need to pass in two arguments");
    filename.push_back(std::string(argv[1]));
    std::string toformat = Preprocess(filename.back());
    yyin = fopen(toformat.c_str(), "r");
    Assert(yyin, "Could not open ", argv[1]);
    yypush_buffer_state(yy_create_buffer(yyin, YY_BUF_SIZE));
    freopen(argv[2], "w", stdout);
  }
  do { yychar = tolower(yychar); yyparse(); } while (!feof(yyin));
}

void yyerror(const char* s) {
  if ( Running_state != RunningState::TestCaseDebug ) {
    std::string str = std::string(s);
    // filter \n
    auto it = std::remove_if(str.begin(), str.end(),
                    [&](auto&& t){ return t == '\n'; });
    str.erase(it, str.end());
    writeln(line_number, ": error [", str, "] in '", filename.back(), "' on '",
            yytext, "\n");
  }
}

void rerror(std::string str) {
  auto it = std::remove_if(str.begin(), str.end(),
                  [&](auto&& t){ return t == '\n'; });
  str.erase(it, str.end());
  writeln(line_number, ": error [", str, "] in '", filename.back(), "' on '",
          yytext, "\n");
}