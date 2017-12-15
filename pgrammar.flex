%{
#include <iostream>
#include <map>
#include <algorithm>
#include <string>
#include <functional>
#include <vector>
#include <variant>
#include <type_traits>
#include "pparser.tab.bison"
// preprocessor
%}

Alpha [a-zA-Z]
Digit [0-9]
Variable {Alpha}({Alpha}|{Digit})*
String \"[^\r\n\"]*\"
CommSt ([^*/]|[\t\n\r])(([^*]|[\t\r\n])*)
MultiLineComment "/*"\**({CommSt}\*+)*(["/"]|[\r\n])
Comment {MultiLineComment}|"//"[^\r\n]+

/* vim macro to generate: vyi("l"|"")hP~ll */
Meta_Include "#"("i"|"I")("n"|"N")("c"|"C")("l"|"L")("u"|"U")("s"|"S")("e"|"E")("z"|"Z")
Meta_Define "#"("d"|"D")("e"|"E")("f"|"F")("i"|"I")("n"|"N")("i"|"I")("s"|"S")("s"|"S")("e"|"E")("z"|"Z")
Meta_Undef  "#"("u"|"U")("n"|"N")("d"|"D")("e"|"E")("f"|"F")
Meta_Ifdef "#"("s"|"S")("i"|"I")("d"|"D")("e"|"E")("f"|"F")
Meta_Ifndef "#"("s"|"S")("i"|"I")("p"|"P")("d"|"D")("e"|"E")("f"|"F")
Meta_Else "#"("S"|"s")("i"|"I")("n"|"N")("o"|"O")("n"|"N")
Meta_End "#"("f"|"F")("i"|"I")("n"|"N")

SyntaxError {String_Not}
%%
{Meta_Define}   {return META_DEFINE;}
{Meta_Undef}    {return META_UNDEF;}
{Meta_Ifdef}    {return META_IFDEF;}
{Meta_Ifndef}   {return META_IFNDEF;}
{Meta_Else}     {return META_ELSE;}
{Meta_End}      {return META_END;}
{Meta_Include}  {return META_INCLUDE;}
{Variable}      {
  std::string t = std::string(yytext);
  std::transform(t.begin(), t.end(), t.begin(), ::tolower);
  yylval.sval = strdup(t.c_str());
  return TVARIABLE;
}
{String}        {yylval.sval = strdup(yytext); return TSTRING;}
{Comment}       {yylval.sval = strdup(yytext); return TCOMMENT;}
[\n] { ++line_number; yylval.sval = yytext; return '\n'; }
[^ ]          {
  std::string t = std::string(yytext);
  std::transform(t.begin(), t.end(), t.begin(), ::tolower);
  yylval.sval = strdup(t.c_str());
  yylval.sval = strdup(yytext); return ANY;
}
[ ]          { yylval.sval = strdup(yytext);  return ' '; }
<<EOF>> {
  yypop_buffer_state();
  meta_layers.pop_back();
  if ( !YY_CURRENT_BUFFER ) yyterminate();
}
%%