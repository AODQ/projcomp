%{
#include <iostream>
#include <map>
#include <algorithm>
#include <string>
#include <functional>
#include <vector>
#include <variant>
#include <type_traits>
#include "parser.tab.bison"
#include "expression.hpp"

enum class Token {
  Float, Integer, String, Variable, Operator,
  Comment, SyntaxError, Unknown, Nil
};

auto Token_Info ( Token t ) {
  struct Info {
    std::string name;
    int bison;
    Info(std::string name, int bison):name(name),bison(bison){}
  }; 

  static const std::map<Token, Info> Token_to_info = {
    {Token::Float,       Info("Float",       TFLOAT)},
    {Token::Integer,     Info("Integer",     TINTEGER)},
    {Token::String,      Info("String",      TSTRING)},
    {Token::Variable,    Info("Variable",    TVARIABLE)},
    {Token::Operator,    Info("Operator",    TSTRING)},
    {Token::SyntaxError, Info("SyntaxError", TSTRING)},
    {Token::Comment,     Info("Comment",     TSTRING)},
    {Token::Unknown,     Info("Unknown",     TSTRING)},
    {Token::Nil,         Info("Nil",         TSTRING)},
  };
  if ( auto it = Token_to_info.find(t); it != Token_to_info.end() )
    return (*it).second;
  std::cout << "Could not find token " << (int)t << '\n';
  return Info("Invalid", -1);
}

std::string Token_To_String ( Token t ) { return Token_Info(t).name ; }
int         Token_To_Bison  ( Token t ) { return Token_Info(t).bison; }

using TokenVariant = std::variant<int, float, const char*>;
using TokenFunction = std::function<TokenVariant(const char*)>;
template <typename T>
void TokenSet(T& l, const char* r, TokenFunction f) {
  // l = std::visit([](auto&& arg){return arg;}, std::apply(f, r));
  l = std::get<T>(f(r));
}

static const std::map<Token, TokenFunction> Token_func {
  {Token::Float,   [](auto&& t){ return TokenVariant(std::stof(t)); }},
  {Token::Integer, [](auto&& t){ return TokenVariant(std::stoi(t)); }},
  {Token::String,  [](auto&& t){ return TokenVariant(strdup(t));    }},
  {Token::Variable,[](auto&& t){ return TokenVariant(strdup(t));    }},
};

template <typename T>
auto Evaluate_Token(Token a, const char* text, T& uval) {
  if ( auto it = Token_func.find(a); it != Token_func.end() )
    TokenSet(uval, text, (*it).second);
  return a;
}

void Line_Count(const char* text) {
  // count line numbers
  std::string line_str = std::string(text);
  line_number += std::count(line_str.begin(), line_str.end(), '\n');
}

template <typename T>
auto Out(Token a, const char* text, T& uval) {
  Line_Count(text);
  // ---
  switch ( Running_state ) {
    case RunningState::TestCaseDebug:
      std::cout << Token_To_String(a) << '\n' << text << '\n';
    break;
    case RunningState::OutputDebug:
      std::cout << "'" << Token_To_String(a) << "' @ " << line_number << ": "
                << text << '\n';
    case RunningState::Bison:
      Evaluate_Token(a, text, uval);
    break;
    default:
      std::cout << "UNKNOWN STATE\n";
      std::exit(-1);
    break;
  }
  return Token_To_Bison(a);
}
%}

Alpha [a-zA-Z]
Digit [0-9]
Integer {Digit}+
Float_EComp ("E"|"e")"-"?{Integer}
Float_DComp "."{Digit}*
Float {Integer}+({Float_DComp}|{Float_EComp})({Float_EComp}|{Float_DComp})?
String \"[^\r\n\"]*\"
Variable {Alpha}({Alpha}|{Digit})*
Operator ("+"|"%"|"-"|"*"|"/"|">"|"<"|"!"|"&"|"|")
CommSt ([^*/]|[\t\n\r])(([^*]|[\t\r\n])*)
MultiLineComment "/*"\**({CommSt}\*+)*(["/"]|[\r\n])
Comment {MultiLineComment}|"//"[^\r\n]+
String_Not \"[^\"\r\n]+(\r|\n)

/* vim macro to generate: vyi("l"|"")hP~ll */
Keyword_Void   (("r"|"R")("i"|"I")("e"|"E")("n"|"N"))
Keyword_RET "retournez"
Keyword_Float  (("r"|"R")("e"|"E")("e"|"E")("l"|"L"))
Keyword_Int    (("e"|"E")("n"|"N")("t"|"T")("i"|"I")("e"|"E")("r"|"R"))
Keyword_While  (("p"|"P")("e"|"E")("n"|"N")("d"|"D")("a"|"A")("n"|"N")("t"|"T"))
Keyword_If     (("s"|"S")("i"|"I"))
Keyword_Else   ("s"|"S")("i"|"I")("n"|"N")("o"|"O")("n"|"N")

SyntaxError {String_Not}
%%
{Comment} {
  std::string t = std::string(yytext);
  Out(Token::Comment, &t[0], yylval.sval);
}
(\n\r|\r\n|\r|\n) {
  ++line_number;
}
([ ]|\t|\r) { }

{Keyword_Void}  {return KEYWORD_VOID; }
{Keyword_Float} {return KEYWORD_FLOAT;}
{Keyword_Int}   {return KEYWORD_INT;  }
{Keyword_While} {return KEYWORD_WHILE;}
{Keyword_If}    {return KEYWORD_IF;   }
{Keyword_RET}   {return KEYWORD_RETURN;}
{Keyword_Else}  {return KEYWORD_ELSE; }
{Integer}       {return Out(Token::Integer    , yytext, yylval.ival); }
{Float}         {return Out(Token::Float      , yytext, yylval.fval); }
{String}        {return Out(Token::String     , yytext, yylval.sval); }
{Variable}      {return Out(Token::Variable   , yytext, yylval.sval); }
{Operator}      {return yytext[0];}
{SyntaxError} {
  Line_Count(yytext);
  yylval.sval = yytext;
  yyerror(yytext);
  return SYNTAX_ERROR;
}
<<EOF>> {
  yypop_buffer_state();
  filename.pop_back();
  if ( !YY_CURRENT_BUFFER ) {
    Emit_ASM_Entry();
    yyterminate();
  }
}
(";"|"("|","|")"|"{"|"}"|"=") {return yytext[0];}
[^ ]          {Out(Token::Unknown    , yytext, yylval.sval); yyerror(yytext); }


%%
