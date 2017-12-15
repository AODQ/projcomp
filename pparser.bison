%{
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <type_traits>
#include <variant>

int line_number;

extern "C" int yylex();
extern "C" int yyparse();
void yyerror(const char* str);
void cperror(std::string s) {
  yyerror(s.c_str());
}

std::vector<std::string> garbage;
std::vector<std::string> includes;
std::vector<std::string> metan;
std::vector<bool> metal;
bool meta_ifdef;
std::vector<int> meta_layers;

void Preprocess ( std::string t ) {
  meta_layers.push_back(0);
  t = std::string { &t[1], &t[t.size()-1] };
  yyin = fopen(t.c_str(), "r");
  if ( !yyin ) {
    std::cerr << "Could not open file " << t << "\n";
  }
  yypush_buffer_state(yy_create_buffer(yyin, YY_BUF_SIZE));
}


%}

%union {
  const char* sval;
}

%token <sval> TVARIABLE
%token <sval> TSTRING
%token <sval> TCOMMENT
%token <sval> META_DEFINE
%token <sval> META_INCLUDE
%token <sval> META_UNDEF 
%token <sval> META_IFDEF
%token <sval> META_IFNDEF
%token <sval> META_ELSE
%token <sval> META_END
%token <sval> ANY
%%

funcroll:
    funcunroll {}
  | funcroll funcunroll {}

funcunroll:
    meta {}
    | ANY { std::cout << $1; }
    | ' ' { std::cout << ' '; }
    | '\n' { std::cout << '\n'; }
    | TVARIABLE {std::cout << $1; }
    | TSTRING   {std::cout << $1; }
    | TCOMMENT  {std::cout << $1; }

metaroll:
    metaunroll {}
  | metaroll metaunroll {}

metaunroll:
  meta {}
  | ANY { garbage.back() += std::string($1); }
  | ' ' { garbage.back() += " "; }
  | '\n'{ garbage.back() += "\n"; }
  | TVARIABLE { garbage.back() += std::string($1); }
  | TSTRING   { garbage.back() += std::string($1); }
  | TCOMMENT  { garbage.back() += std::string($1); }

meta:
      metaline  {}
    | metablock {}

metaline:
  META_DEFINE ' ' TVARIABLE {
    metan.push_back(std::string($3));
  }
  | META_UNDEF ' ' TVARIABLE {
    auto str = std::string($3);
    auto& m = metan;
    if ( auto it = std::find(m.begin(), m.end(), str); it != m.end() ) {
      metan.erase(it);
    }
  }
  | META_INCLUDE ' ' TSTRING {
    if ( meta_layers.back() > 0 )
      includes.push_back(std::string($3));
    else
      Preprocess(std::string($3));
  }

metablock:
    metaif ' ' TVARIABLE metaroll {
      std::string t = $3;
      auto& m = metan;
      if ( (std::find(m.begin(), m.end(), t) != m.end()) ^ meta_ifdef ) {
        garbage.pop_back();
      }
      if ( -- meta_layers.back() == 0 ) {
        for ( auto c : garbage )
          std::cout << c;
        for ( auto c : includes )
          Preprocess(c);
        garbage.clear();
        includes.clear();
      }
    } META_END |
    metaif ' ' TVARIABLE metaroll {
      std::string t = $3;
      bool mle = false;
      auto& m = metan;
      if ( (std::find(m.begin(), m.end(), t) != m.end()) ^ meta_ifdef ) {
        garbage.pop_back();
        mle = true;
      }
      metal.push_back(mle);
    } metaelse metaroll {
      auto mle = metal.back();
      metal.pop_back();
      if ( !mle ) {
        garbage.pop_back();
        includes.clear();
      }
      if ( -- meta_layers.back() == 0 ) {
        for ( auto c : garbage )
          std::cout << c;
        for ( auto c : includes )
          Preprocess(c);
        garbage.clear();
        includes.clear();
      }
    } META_END

metaif:
  META_IFDEF {
    ++meta_layers.back();
    meta_ifdef = true;
    garbage.push_back("");
  }
  | META_IFNDEF {
    ++meta_layers.back();
    garbage.push_back("");
    meta_ifdef = false;
  }
metaelse:
  META_ELSE {
    garbage.push_back("");
  }
%%

int main ( int argc, char** argv ) {
  meta_layers.push_back(0);
  if ( argc > 1 ) {
    if ( argc != 3 ) {
      std::cout << "Need to pass in 2 args\n";
      return 0;
    }
    yyin = fopen(argv[1], "r");
    yypush_buffer_state(yy_create_buffer(yyin, YY_BUF_SIZE));
    freopen(argv[2], "w", stdout);
  }
  do { yychar = tolower(yychar); yyparse(); } while (!feof(yyin));
}

void yyerror(const char* s) {
  std::string str = std::string(s);
}
