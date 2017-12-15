#ifndef __EXPRESSION_H__
#define __EXPRESSION_H__

#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>
#include "ASM.hpp"


// holds_alternative does the exact opposite of its name, so alias it to a
// better name using perfect forwarding
template <typename T, typename Arg>
inline auto VariantType(Arg&& arg) {
  return std::holds_alternative<T>(std::forward<Arg>(arg));
}

template <bool>
struct CTAssert{};
template <>
struct CTAssert<true>{};

enum class  ExprTime {
  CompileTime, RunTime
};

struct Expression;
enum class BlockType { _while, _if, _else };
inline std::string BlockType_String ( BlockType t ) {
  switch ( t ) {
    case BlockType::_while:   return "while";
    case BlockType::_if:      return "if";
    case BlockType::_else:    return "else";
  }
}

using ExprList = std::vector<Expression>;

struct Container {
  VarType type;
  int offset; // for compile-time integers, this is the value
};

namespace EH {
  struct MetaData {
    std::string value;
    MetaData ( std::string _value ):value(_value) {
      // Can't wait for Alexandrescu/DLang-inspired Ranges TS in C++20 ...
      std::transform(value.begin(), value.end(), value.begin(), tolower);
    }
    inline int Emit_ASM ( ) const { return 0; }
  };
  struct Declaration {
    ExprList expr_list;
    int Emit_ASM ( ) const;
  };
  struct Return {
    std::shared_ptr<Expression> ret;
    Return(std::shared_ptr<Expression>&&);
    int Emit_ASM ( ) const;
  };
  struct Block {
    BlockType type;
    std::shared_ptr<Expression> condit;
    std::string label;
    Block(const Block&);
    Block(BlockType, std::shared_ptr<Expression>&&);
    int Emit_ASM   ( ) const;
  };
  struct BlockPromise {
    BlockType type;
    std::string label;
    bool emit_asm = true;
    int Emit_ASM ( ) const;
  };
  struct ParamList {
    ExprList param_list;
    inline int Emit_ASM ( ) const { return 0; }
  };
  struct Assignment {
    Container lhand;
    std::shared_ptr<Expression> rhand;
    inline int Emit_ASM ( ) const;
  };
  struct Variable {
    Container var;
    // Variable ( const Container&  var ) : var (var) {}
    // Variable (       Container&& var ) : var (var) {}
    // Variable ( const Variable&  v )    : var (v.var) {}
    // Variable (       Variable&& v )    : var (std::move (v.var)) {}
    inline int Emit_ASM ( ) const { return 0; }
  };
  struct DataCT {
    Container ct_data;
    DataCT ( const Container&  ct_data ) ;
    DataCT (       Container&& ct_data ) ;
    DataCT ( const DataCT&  d ) ;
    DataCT (       DataCT&& d ) ;
    int Emit_ASM ( ) const;
  };
  struct DataRT {
    Container rt_data;
    DataRT ( const Container&  rt_data ) ;
    DataRT (       Container&& rt_data ) ;
    DataRT ( const DataRT&  d ) ;
    DataRT (       DataRT&& d ) ;
    int Emit_ASM ( ) const;
  };
  struct Function {
    ParamList param_list;
    MetaData function;
    Function ( const ParamList&  , const MetaData&  );
    Function (       ParamList&& ,       MetaData&& );
    Function ( const Function&  f );
    int Emit_ASM ( ) const;
  };
}

namespace DataSection {
  extern std::vector<std::string> asciiz;
  extern std::vector<float>       floatz;
};

namespace Environment {
  struct Env {
    std::map<std::string, Container> var_list;
    int frame_ptr=0;
  };
  extern Env head_env;
  inline int Allocate_Stack ( int amt, Env& e = head_env ) {
    e.frame_ptr += amt;
    return e.frame_ptr;
  }
  inline void New_Container ( VarType vtype, const std::string& lbl,
                              Env& e = head_env ) {
    int bytes = Environment::Allocate_Stack(VarType_Size(vtype), e);
    e.var_list[lbl] = Container{vtype, bytes};
  }
  inline Container RContainer ( const std::string& s, Env& e) {
    std::string ns = s;
    std::transform(ns.begin(), ns.end(), ns.begin(), tolower);
    if ( auto vl = e.var_list.find(ns); vl != e.var_list.end() )
      return vl->second;
    if ( auto vl = head_env.var_list.find(ns); vl != head_env.var_list.end() )
      return vl->second;
    return Container{VarType::_unknown, 0};
  }
  inline Container* RContainer_Ptr ( const std::string& s, Env& e = head_env ) {
    std::string ns = s;
    std::transform(ns.begin(), ns.end(), ns.begin(), tolower);
    if ( auto vl = e.var_list.find(ns); vl != e.var_list.end() )
      return &vl->second;
  }
};

using ExpressionVariant = std::variant<
  EH::Declaration, EH::ParamList, EH::Assignment, EH::Variable, EH::DataCT,
  EH::DataRT, EH::Function, EH::MetaData, EH::Block, EH::BlockPromise,
  EH::Return
>;

struct Expression {
  ExpressionVariant EH_value;

  Expression ( const Expression&  expr );
  Expression (       Expression&& expr );
  template <typename T, typename = std::enable_if_t<
                        std::is_convertible<T, ExpressionVariant>::value>>
  Expression ( const T& t  ):EH_value (t){}
  template <typename T, typename = std::enable_if_t<
                        std::is_convertible<T, ExpressionVariant>::value>>
  Expression (       T&& t ):EH_value (std::move(t)){}
  Expression ( ) : EH_value(EH::MetaData("Nil")){}

  VarType RVar_Type ( ) const;

  // HAS to be a EH::DataCT, returns offset in case of string [for now]
  std::variant<int, float> RCT_Result ( ) const;

  int Emit_ASM ( ) const;
};

struct FuncInfo {
  std::string label;
  VarType ret_type;
  Environment::Env env;
  VarType paramtype = VarType::_unknown;
  bool defined = false;

  ExprList expr_program;
  ExprList expr_stack;
  std::vector<EH::BlockPromise> block_stack;
  EH::BlockPromise* last_block;
};

extern std::map<std::string, FuncInfo> finf;
extern std::string finf_label;
extern bool in_func;
inline FuncInfo& finfc ( ) { return finf[finf_label]; }

extern std::vector<std::string> filename;

EH::ParamList Make_List_From_Stack ( int amt );

namespace BTS {
  struct FuncCall    {std::string function; };
  struct Declaration {VarType type;bool global_hack;};
  struct Assignment  {std::string variable; };
  struct Block       {BlockType type;       };
  struct FuncProto   {VarType type; std::string param;
                      VarType paramtype; std::string label;};
  struct Func        {VarType type; std::string param;
                      VarType paramtype; std::string label;};
  struct Return      {};
  using BTSVariant = std::variant<FuncCall, Declaration, Assignment, Block,
                                  FuncProto, Func, Return>;
  struct Sensible {
    BTSVariant bison;
    template <typename T, typename = std::enable_if_t<
                std::is_convertible<T, BTSVariant>::value>>
    Sensible ( const T& t  ):bison (t){}
    template <typename T, typename = std::enable_if_t<
                std::is_convertible<T, BTSVariant>::value>>
    Sensible (       T&& t ):bison (std::move(t)){}
  };
}

void Reduce_Expression ( const BTS::Sensible& expr );
template <typename T, typename = std::enable_if_t<
          std::is_convertible<T, BTS::BTSVariant>::value>>
void Reduce_Expression ( T&& t ) {
  Reduce_Expression(BTS::Sensible(std::move(t)));
}

void Emit_ASM_Func ( );
void Emit_ASM_Entry ( );
void Finish_Expr();
void Finish_Block();


template <typename T>
  inline Container Register_Constant_Container ( const T& val ) {
  if        constexpr ( std::is_same<T, std::string>::value ) {
      DataSection::asciiz.push_back(val);
      return Container{VarType::_str, (int)DataSection::asciiz.size()};
    } else if constexpr ( std::is_same<T, float>::value ) {
      DataSection::floatz.push_back(val);
      return Container{VarType::_float, (int)DataSection::floatz.size()};
    } else if constexpr ( std::is_same<T, int>::value ) {
      return Container{VarType::_int, val};
    }
}

inline Container Register_Runtime_Container ( const std::string& value ) {
  auto&& results = Environment::RContainer(value, finfc().env);
  if ( results.type == VarType::_unknown ) {
    // Not yet defined, so push a MetaData on the stack
    finfc().expr_stack.push_back(Expression(EH::MetaData(value)));
  }
  return results;
}

template <typename T>
inline void Add_Expr_Stack ( const T& data, ExprTime expr_time ) {
  switch ( expr_time ) {
    case ExprTime::RunTime:
      if constexpr ( std::is_same<T, std::string>::value ) {
        auto&& datart = EH::DataRT(Register_Runtime_Container(data));
        finfc().expr_stack.push_back(Expression(datart));
      } else {
        Assert(false, "Expected string for a variable");
      }
    break;
    case ExprTime::CompileTime:
      auto&& datact = EH::DataCT(Register_Constant_Container(data));
      finfc().expr_stack.push_back(Expression(datact));
    break;
  }
}

inline std::string EHVariant_Str(ExpressionVariant v) {
  std::string res;
  std::visit(Match{
    [&]( const EH::Declaration&  val ) { res = "EH::Declaration"; },
    [&]( const EH::ParamList&    val ) { res = "EH::ParamList"; },
    [&]( const EH::Assignment&   val ) { res = "EH::Assignment"; },
    [&]( const EH::Variable&     val ) { res = "EH::Variable"; },
    [&]( const EH::DataCT&       val ) { res = "EH::DataCT"; },
    [&]( const EH::DataRT&       val ) { res = "EH::DataRT"; },
    [&]( const EH::Function&     val )
      { res = "EH::Function [ " + val.function.value + " ]"; },
    [&]( const EH::MetaData    & val )
      { res = "EH::MetaData [ " + val.value + " ]"; },
    [&]( const EH::Block& val ) { res = "EH::Block"; },
    [&]( const EH::BlockPromise& val ) { res = "EH::BlockPromise"; },
    [&]( const EH::Return& val) { res = "EH::Return"; } // 
  }, v);
  return res;
}

inline std::string EHVariant_Str_All(const ExprList& e) {
  std::string accum = "{ ";
  for ( const auto& r : e ) {
    accum += Accum(EHVariant_Str(r.EH_value), ", ");
  }
  return accum + "}";
}

std::string Preprocess ( std::string filename );

#endif
