#ifndef __ASM_H__
#define __ASM_H__

#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>

extern int line_number;

/* http://en.cppreference.com/w/cpp/utility/variant/visit */
template<class... Ts> struct Match : Ts... { using Ts::operator()...; };
template<class... Ts> Match(Ts...) -> Match<Ts...>;

template <typename... Args>
inline std::string Accum(Args&&... args) {
  std::stringstream ss;
  (ss << ... << args);
  return ss.str();
}
template <typename... Args>
inline void writeln(Args&&... args) { // emulate D's writeln
  std::cerr << Accum(std::forward<Args>(args)...) << '\n';
}

template <typename... Args>
inline void User_Assert(bool g, Args&&... args) {
  if ( !g ) {
    writeln("ERROR: ", std::forward<Args>(args)...);
    writeln("@", line_number+1);
  }
}
template <typename... Args>
inline void Assert(bool g, Args&&... args) {
  User_Assert(g, std::forward<Args>(args)...);
  if ( !g ) std::exit(-1);
}

enum class VarType { _int, _float, _str, _void, _unknown, size };
template <typename T>
struct ToVarType { };
template <> struct ToVarType<int>;
template <> struct ToVarType<float>;
template <> struct ToVarType<std::string>;
int VarType_Size(VarType v);
std::string VarType_String(VarType v);
VarType String_To_VarType(std::string s);

namespace ASM {
  enum LoadType { Stack, Frame };
  int Pop_Stack ( int offset );
  int Push_Stack ( int offset );
  void Load_Const(VarType type, int offset);
  // Loads to $X0
  void Load_To_Reg  (VarType ltype, VarType rtype, LoadType, int);
  void Load_From_Reg(VarType ltype, VarType rtype, LoadType, int);
  // Pops the stack as well
  void Prepare_Register_From_Stack (VarType ltype, VarType rtype);
  void Prepare_Registers_From_Stack(VarType ltype, VarType rtype);
  void Convert_If_Necessary ( VarType to, VarType from );
  // -------------------
  // math [assumes Prepare_Registers_From_Stack]
  void Add(VarType l, VarType r);
  void Mul(VarType l, VarType r);
  void Div(VarType l, VarType r);
  void Mod(VarType l, VarType r);
  void Sub(VarType l, VarType r);
  void Neg(VarType l);
  void And(VarType l, VarType r);
  void Or (VarType l, VarType r);
  void LT (VarType l, VarType r);
  void LTE(VarType l, VarType r);
  void GT (VarType l, VarType r);
  void GTE(VarType l, VarType r);
  void EQ (VarType l, VarType r);
  void NEQ(VarType l, VarType r);
  void Not(VarType);
  // ------------------
  void Program_Entry ( );
  void Program_Exit ( );
  void Start_Data_Section ( );
  enum class JumpType { zero, not_zero, unconditional };
  void Jump(VarType, JumpType, std::string);
  void Call(std::string, VarType, VarType, VarType);
  void Emit_Label(std::string);
  void Emit_Start_Function(std::string, VarType param);
  void Emit_End_Function(std::string, VarType rettype);
  // ---- functions ----
  void Syscall ( );
  void Print_Str ( int offset );
  std::string RProper_Register ( VarType type );
  // move $XL, $XR
  void Move(VarType type, int from, int to);
  // Grabs from the stack and prints
  void Print ( VarType type, int offset );
};

#endif
