#include "expression.hpp"
#include <iostream>
#include <variant>
#include <iomanip>

bool Is_Op1 ( std::string t ) {
  return (t == "-u" || t == "!");
}
bool Is_Op2 ( std::string t ) {
  return (t == "+"  || t == "-"  || t == "*"  || t == "/"  || t == "%"  ||
          t == "<"  || t == ">"  || t == "&&" || t == "<=" || t == ">=" ||
          t == "==" || t == "!=" || t == "||");
}

// -------------- asm emit --------------
static int callstackcount=0;
int EH::Function::Emit_ASM ( ) const {
  ++callstackcount;
  const ExprList& expr_list = param_list.param_list;
  std::string flabel  = function.value;
  if ( flabel == "ecrivez" ) {
    for ( const auto& expr : expr_list ) {
      auto var_type = expr.RVar_Type();
      if ( var_type == VarType::_str ) { // string
        ASM::Print_Str(std::get<int>(expr.RCT_Result()));
        continue;
      }// run time/gets put on stack
      int stack_offset = expr.Emit_ASM();
      ASM::Print(var_type, stack_offset);
      ASM::Pop_Stack(4);
    }
    return 0; // print stack 0
  } else if ( Is_Op2(flabel) ) {
    Assert(expr_list.size() == 2, "; need size 2 got ", expr_list.size());
    int loff = expr_list[0].Emit_ASM();
    ++callstackcount;
    int roff = expr_list[1].Emit_ASM();
    --callstackcount;
    auto lhand = expr_list[0].RVar_Type(),
         rhand = expr_list[1].RVar_Type();
    ASM::Prepare_Registers_From_Stack(lhand, rhand);
    switch ( flabel[0] ) {
      case '+': ASM::Add(lhand, rhand); break;
      case '-': ASM::Sub(lhand, rhand); break;
      case '*': ASM::Mul(lhand, rhand); break;
      case '/': ASM::Div(lhand, rhand); break;
      case '%': ASM::Mod(lhand, rhand); break;
      case '&': ASM::And(lhand, rhand); break;
      case '|': ASM::Or (lhand, rhand); break;
      case '>':
        if ( flabel == ">=" ) ASM::GTE(lhand, rhand);
        else                  ASM::GT (lhand, rhand);
      break;
      case '<':
        if ( flabel == "<=" ) ASM::LTE(lhand, rhand);
        else                  ASM::LT (lhand, rhand);
      break;
    }
    if ( flabel == "==" ) ASM::EQ (rhand, lhand);
    if ( flabel == "!=" ) ASM::NEQ(rhand, lhand);
  } else if ( Is_Op1(flabel) ) {
    Assert(expr_list.size() == 1, "; need size 1 got ", expr_list.size());
    int off = expr_list[0].Emit_ASM();
    auto type = expr_list[0].RVar_Type();
    ASM::Pop_Stack(4);
    if ( flabel == "-u" ) {
      ASM::Neg(type);
    } else if ( flabel == "!" ) {
      ASM::Not(type);
    }
  } else {
    if ( auto it = finf.find(flabel); it != finf.end() ) {
      auto& f = std::get<1>(*it);
      // load up parameter
      int amt = f.paramtype == VarType::_unknown ? 0 : expr_list[0].Emit_ASM();
      auto type = amt>0?expr_list[0].RVar_Type():VarType::_unknown;
      ASM::Call(flabel, f.paramtype, type, f.ret_type, callstackcount>6);
    } else
      Assert(false, "Unknown function call '", flabel, "'");
  }
  --callstackcount;
  return 4;
}

int EH::DataCT::Emit_ASM ( ) const {
  // const -> stack
  ASM::Load_Const(ct_data.type, ct_data.offset);
  return VarType_Size(ct_data.type);
}

int EH::DataRT::Emit_ASM ( ) const {
  // frame -> register -> stack
  int amt = ASM::Push_Stack(VarType_Size(rt_data.type));
  ASM::Load_To_Reg(rt_data.type, rt_data.type, ASM::LoadType::Frame,
                         rt_data.offset);
  ASM::Load_From_Reg(rt_data.type, rt_data.type,ASM::LoadType::Stack, amt);
  return amt;
}

int EH::Declaration::Emit_ASM ( ) const {
  int byte_count = 0;
  for ( const auto& expr : expr_list ) {
    Assert(VariantType<EH::MetaData>(expr.EH_value),
          "Expected MetaData for declaration");
    auto variable =
      Environment::RContainer_Ptr(std::get<EH::MetaData>(expr.EH_value).value, finfc().env);
    Assert(variable != nullptr, "Variable not properly declared");
    byte_count += ASM::Push_Stack(VarType_Size((*variable).type));
  }
  return byte_count;
}

int EH::Assignment::Emit_ASM ( ) const {
  int bytecount = (*rhand).Emit_ASM();
  Assert(lhand.type != VarType::_unknown, "[Assigning to undeclared variable]");
  // stack -> register -> frame
  ASM::Load_To_Reg(lhand.type, (*rhand).RVar_Type(),
                    ASM::LoadType::Stack, bytecount);
  ASM::Load_From_Reg(lhand.type, (*rhand).RVar_Type(),
                    ASM::LoadType::Frame, lhand.offset);
  ASM::Pop_Stack(bytecount);
  return 0;
}

int EH::Block::Emit_ASM ( ) const {
  switch ( type ) {
    case BlockType::_else:
      ASM::Jump(VarType::_int, ASM::JumpType::unconditional,
                Accum(label, "ELSEEND"));
      ASM::Emit_Label(Accum(label, "FALSE"));
    return 0;
    case BlockType::_while:
      ASM::Emit_Label(Accum(label, "TRUE"));
    break;
  }
  int bytecount = (*condit).Emit_ASM();
  Assert(bytecount > 0, "Expression doesn't evaluate a condition; ",
          EHVariant_Str((*condit).EH_value));
  ASM::Jump((*condit).RVar_Type(), ASM::JumpType::zero, Accum(label, "FALSE"));
  ASM::Pop_Stack(bytecount);
  return 0;
}

int EH::BlockPromise::Emit_ASM ( ) const {
  switch ( type ) {
    case BlockType::_while:
      ASM::Jump(VarType::_int, ASM::JumpType::unconditional,
                Accum(label, "TRUE"));
      ASM::Emit_Label(Accum(label, "FALSE"));
    break;
    case BlockType::_if:
      if ( emit_asm )
        ASM::Emit_Label(Accum(label, "FALSE"));
    break;
    case BlockType::_else:
      ASM::Emit_Label(Accum(label, "ELSEEND"));
    break;
  }
  return 0;
}

// --- expression ---
VarType Expression::RVar_Type ( ) const {
  return std::visit([&](const auto& data) -> VarType {
    using T = std::decay_t<decltype(data)>;
    if        constexpr ( std::is_same<T, EH::DataCT>::value ) {
      return data.ct_data.type;
    } else if constexpr ( std::is_same<T, EH::DataRT>::value ) {
      return data.rt_data.type;
    } else if constexpr ( std::is_same<T, EH::Function>::value) {
      if        ( data.function.value == "ecrivez" ) {
        return VarType::_unknown;
      } else if ( Is_Op2(data.function.value) ) {
        if ( data.function.value == ">" || data.function.value == "<" )
          return VarType::_int;
        if ( data.function.value == "==" || data.function.value == "!=" )
          return VarType::_int;
        if ( data.function.value == "<=" || data.function.value == ">=" )
          return VarType::_int;
        const auto& list = data.param_list.param_list;
        auto lhand  = list[0].RVar_Type();
        if ( lhand == list[1].RVar_Type() ) return lhand;
        return VarType::_float;
      } else if ( Is_Op1(data.function.value) ) {
        return data.param_list.param_list[0].RVar_Type();
      } else {
        if ( auto it = finf.find(data.function.value); it != finf.end() ) {
          return std::get<1>(*it).ret_type;
        } else {
          Assert(false, "No known return type for ",
                  EHVariant_Str(EH_value));
        }
      }
    } else {
      Assert(false, "No known return type for ",
                    EHVariant_Str(EH_value));
      return VarType::_unknown;
    }
  }, EH_value);
  return VarType::_unknown;
}

std::variant<int, float> Expression::RCT_Result ( ) const {
  const Container& ct_data = std::get<EH::DataCT>(EH_value).ct_data;
  switch ( ct_data.type ) {
    case VarType::_int: case VarType::_str: return ct_data.offset;
    case VarType::_float: return DataSection::floatz[ct_data.offset];
  }
  return -1;
}

int Expression::Emit_ASM ( ) const {
  std::visit([](auto& f) { f.Emit_ASM(); }, EH_value);
}

/// --- expr stack ---

std::vector<std::string> DataSection::asciiz;
std::vector<float>       DataSection::floatz = { 0.0f };

std::map<std::string, FuncInfo> finf;
std::string finf_label = "";
bool in_func = false;
Environment::Env Environment::head_env;
std::vector<std::string> filename;

EH::ParamList Make_List_From_Stack ( int amt ) {
  ExprList expr_list;
  Assert(finfc().expr_stack.size() >= amt, " Parameter count mismatch");
  while ( -- amt >= 0 ) {
    expr_list.push_back(finfc().expr_stack.back());
    finfc().expr_stack.pop_back();
  }
  return EH::ParamList{expr_list};
}

void Make_Func ( BTS::FuncProto proto ) {
  Environment::Env env;
  if ( proto.param != "" ) {
    Environment::New_Container(proto.paramtype, proto.param, env);
  }
  finf[proto.label] = FuncInfo{proto.label, proto.type, env, proto.paramtype};
}
void Reduce_Expression ( const BTS::Sensible& expr ) {
  std::visit(Match {
    [](const BTS::FuncCall& caller) {
      // construct an expression from this TODO count
      auto fmetadata = EH::MetaData(caller.function);
      auto flabel = fmetadata.value; // metadata stringifies
      if ( flabel == "ecrivez" ) {
        auto func = EH::Function(Make_List_From_Stack(1), fmetadata);
        finfc().expr_stack.push_back(Expression(func));
      }
      else if ( Is_Op2(flabel) ) {
        auto func = EH::Function(Make_List_From_Stack(2), fmetadata);
        finfc().expr_stack.push_back(Expression(func));
      } else if ( Is_Op1(flabel) ) {
        auto func = EH::Function(Make_List_From_Stack(1), fmetadata);
        finfc().expr_stack.push_back(Expression(func));
      } else {
        Assert(flabel != "commencement", "Can't call main");
        if ( auto it = finf.find(flabel); it != finf.end() ) {
          auto& f = std::get<1>(*it);
          int amt = f.paramtype == VarType::_unknown ? 0 : 1;
          auto func = EH::Function(Make_List_From_Stack(amt), fmetadata);
          finfc().expr_stack.push_back(Expression(func));
        } else
          Assert(false, "Unknown function '", flabel, "'");
      }
    },
    [](const BTS::FuncProto& proto) {
      if ( auto it = finf.find(proto.label); it != finf.end() ) {
        Assert(false, "Function prototype redeclaration");
      }
      Make_Func(proto);
    },
    [](const BTS::Func& func) {
      if ( auto it = finf.find(func.label); it != finf.end() ) {
        auto& t = std::get<1>(*it);
        Assert(t.ret_type == func.type, "Function mismatch return types ",
               func.label);
        if ( auto var = t.env.var_list.find("tempparam");
             var != t.env.var_list.end() ) {
          Assert(func.param != "", "Missing parameter in ", func.label);
          Assert(std::get<1>(*var).type == func.paramtype,
                 "Mismatch parameter ", func.label);
          t.env.var_list.erase("tempparam");
          t.paramtype = func.paramtype;
        }
      } else {
        Make_Func(BTS::FuncProto{func.type, func.param, func.paramtype,
                                 func.label});
      }
    },
    [](const BTS::Block& block) {
      if ( block.type == BlockType::_else ) {
        // no expression here, make one
        auto b = EH::Block(block.type, nullptr);
        b.label = (*finfc().last_block).label;
        (*finfc().last_block).emit_asm = false;
        finfc().expr_stack.push_back(b);
        finfc().block_stack.push_back(EH::BlockPromise{b.type, b.label});
        return;
      }
      // if / while etc
      auto&& expr = finfc().expr_stack.back();
      auto b = EH::Block(block.type, std::make_shared<Expression>(expr));
      finfc().expr_stack.pop_back();
      finfc().expr_stack.push_back(b);
      finfc().block_stack.push_back(EH::BlockPromise{b.type, b.label});
    },
    [](const BTS::Return& ret) {
      Assert(finfc().expr_stack.size() > 0, "Must return value");
        auto&& expr = finfc().expr_stack.back();
      auto b = EH::Return(std::make_shared<Expression>(expr));
      finfc().expr_stack.pop_back();
      finfc().expr_stack.push_back(b);
    },
    [](const BTS::Assignment& assig) {
      Assert(finfc().expr_stack.size() > 0, "Improper assignment");
      auto e = Environment::RContainer(assig.variable, finfc().env);
      auto expr = EH::Assignment{
          Environment::RContainer(assig.variable, finfc().env),
              std::make_shared<Expression>(finfc().expr_stack.back())};
      finfc().expr_stack.pop_back();
      finfc().expr_stack.push_back(expr);
    },
    [](const BTS::Declaration& decl) {
      Assert(finfc().expr_stack.size() > 0, "Stack size is 0");
      Assert(finfc().block_stack.size() == 0,
             "Variables can not be defined inside of blocks");
      ExprList decls;
      // expr_stack looks like: metadata container ...
      while ( !finfc().expr_stack.empty() ) {
        std::string varname;
        if ( !VariantType<EH::MetaData>(finfc().expr_stack.back().EH_value) ) {
          finfc().expr_stack.pop_back();
          continue; // throw it away, hopefully a DataRT
        } else {
          varname = std::get<EH::MetaData>(finfc().expr_stack.back().EH_value)
                                          .value;
          finfc().expr_stack.pop_back();
        }
        decls.push_back(EH::MetaData(varname));
        User_Assert(Environment::RContainer(varname, finfc().env).type == VarType::_unknown,
              "Variable redeclaration on ", varname);
        if ( !decl.global_hack )
          Environment::New_Container(decl.type, varname, finfc().env);
        else
          Environment::New_Container(decl.type, varname, Environment::head_env);
      }
      finfc().expr_stack.push_back(EH::Declaration{decls}); // to mixinasm later
    },
    [](auto&   noimpl) { std::cout << "NO IMPL??? \n";}
  }, expr.bison);
}

void Finish_Expr () {
  Assert(finfc().expr_stack.size() == 1, "; Unknown variable");
         // finfc().expr_stack.size(), " [expected 1]: ",
         // EHVariant_Str_All(finfc().expr_stack));
  finfc().expr_program.push_back(finfc().expr_stack[0]);
  finfc().expr_stack.clear();
}
void Finish_Block ( ) {
  Assert(finfc().block_stack.size() > 0, "; Block size is 0 despite finishing");
  auto&& blst = finfc().block_stack.back();
  finfc().expr_program.push_back(Expression(blst));
  std::visit(Match{
    [&](EH::BlockPromise& val) {
      if ( val.type == BlockType::_if )
        finfc().last_block = &val;
    },
    [&](const auto& val) {
      Assert(false, "; Trying to finish block with non-block");
    }
    }, finfc().expr_program[finfc().expr_program.size()-1].EH_value);
  finfc().block_stack.pop_back();
}

void Emit_ASM_Func () {
  ASM::Emit_Start_Function(finf_label, finfc().paramtype);
  Assert(finfc().defined == false,
         "Function redefinition ", finfc().label);
  ASM::Push_Stack(4);
  finfc().defined = true;
  for ( const auto& e: finfc().expr_program ) {
    e.Emit_ASM();
  }
  ASM::Emit_End_Function(finf_label, finfc().ret_type);
}

void Emit_ASM_Entry ( ) {
  for ( auto it : finf ) {
    Assert(std::get<1>(it).defined, "Undefined func ", std::get<0>(it));
  }
  if ( auto it = finf.find("commencement"); it != finf.end() ) {
    // check definition
    Assert(std::get<1>(*it).defined == true,
           "Entry point declared yet undefined");
  } else {
    Assert(false, "Entry point commencement not defined");
  }
  Assert(finf.find("commencement") != finf.end(),
         "Entry point commencement not defined");
  ASM::Program_Entry();
  ASM::Program_Exit();
  ASM::Start_Data_Section();
  for ( int i = 0; i != DataSection::asciiz.size(); ++ i )
    std::cout << "str" << i+1 << ": .asciiz " << DataSection::asciiz[i]<<'\n';
  for ( int i = 0; i != DataSection::floatz.size(); ++ i )
    std::cout << "fl" << i+1 << ": .float "   << std::fixed
              << DataSection::floatz[i] << '\n';
}



// --- constructors, could remove using aggregates but whatever ----

EH::Function::Function ( const EH::ParamList&  param_list,
                         const EH::MetaData&  function )
  : function  (function), param_list(param_list) {}
EH::Function::Function ( EH::ParamList&& param_list, EH::MetaData&& function )
  : function  (std::move(function)),
    param_list(std::move(param_list)) {}
EH::Function::Function ( const EH::Function& f )
  : function  (f.function), param_list(f.param_list) {}

EH::DataCT::DataCT ( const Container&  ct_data ) : ct_data(ct_data ) {}
EH::DataCT::DataCT (       Container&& ct_data ) : ct_data(ct_data ) {}
EH::DataCT::DataCT ( const DataCT&  d ) : ct_data(d.ct_data) {}
EH::DataCT::DataCT (       DataCT&& d ) : ct_data(std::move(d.ct_data)) {}
EH::DataRT::DataRT ( const Container&  rt_data ) : rt_data(rt_data ) {}
EH::DataRT::DataRT (       Container&& rt_data ) : rt_data(rt_data ) {}
EH::DataRT::DataRT ( const DataRT&  d ) : rt_data(d.rt_data) {}
EH::DataRT::DataRT (       DataRT&& d ) : rt_data(std::move(d.rt_data)) {}

Expression::Expression ( const Expression&  expr ):EH_value(expr.EH_value){}
Expression::Expression (       Expression&& expr ):
    EH_value(std::move(expr.EH_value)){}

EH::Return::Return(std::shared_ptr<Expression>&& ret):ret(ret){}
int EH::Return::Emit_ASM ( ) const {
  if ( ret == nullptr ) {
  } else {
    int bytecount = (*ret).Emit_ASM();
    auto type = (*ret).RVar_Type();

    auto functype = finfc().ret_type;
    ASM::Load_To_Reg(functype, type, ASM::LoadType::Stack, 0);
    ASM::Load_From_Reg(functype, functype, ASM::LoadType::Frame, -8);
    std::cout << "\tb _ret" << finf_label << '\n';
  }
  return 0;
}

EH::Block::Block(const Block& b) :
    type(b.type), condit(b.condit), label(b.label){}
EH::Block::Block(BlockType type, std::shared_ptr<Expression>&& condit):
          type(type), condit(condit) {
  static std::string plabel = "AAAAAAAA";
  int indx = plabel.size()-1;
  while ( true ) {
    char t = (char)((int)plabel[indx] + 1);
    t = t > 'Z' ? 'A' : t;
    plabel[indx] = t;
    if ( t == 'A' ) {
      --indx;
    } else break;
  }
  label = plabel;
}

std::string Preprocess ( std::string fname ) {
  // Using system is bad as always but eh not really worth the extra effort
  // here.
  std::string cmd = "./PP ";
  cmd += fname + " ";
  cmd += fname + "preprocesspout";
  system(cmd.c_str());
  return fname + "preprocesspout";
}
