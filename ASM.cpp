#include "ASM.hpp"

template <typename... Args>
inline void Write_ASM(Args&&... args) {
  std::cout << '\t';
  (std::cout << ... << args);
  std::cout << '\n';
}

int line_number=1;

template <> struct ToVarType<int>         {
  static constexpr VarType value = VarType::_int;
};
template <> struct ToVarType<float>       {
  static constexpr VarType value = VarType::_float;
};
template <> struct ToVarType<std::string> {
  static constexpr VarType value = VarType::_str;
};

std::string VarType_String(VarType v) {
  switch ( v ) {
    case VarType::_unknown: return "unknown";
    case VarType::_void:  return "void";
    case VarType::_float: return "float";
    case VarType::_int:   return "int";
    case VarType::_str:   return "string";
  }
  return "void";
}

VarType String_To_VarType(std::string v) {
  if ( v == "void"  ) return VarType::_unknown;
  if ( v == "int"   ) return VarType::_int;
  if ( v == "float" ) return VarType::_float;
  Assert(false, "Invalid variable type: ", v);
  return VarType::_unknown;
}

int VarType_Size(VarType v) {
  switch ( v ) {
    case VarType::_float: case VarType::_int: return 4;
    case VarType::_str: return 0;
  }
  return 0;
}

int ASM::Pop_Stack ( int offset ) {
  Write_ASM("add $sp, $sp, ", offset, " # pop stack");
  return offset;
}
int ASM::Push_Stack ( int offset ) {
  Write_ASM("sub $sp, $sp, ", offset, " # push stack");
  return offset;
}
void ASM::Load_Const(VarType type, int offset) {
  if ( type == VarType::_str ) return; // can't load string to stack
  ASM::Push_Stack(4);
  switch ( type ) {
    default: Assert(false, "Unknown load const"); break;
    case VarType::_int:
      Write_ASM("li $a0, ", offset);
      Write_ASM("sw $a0, ($sp)");
    break;
    case VarType::_float:
      Write_ASM("l.s $f0, fl", offset);
      Write_ASM("s.s $f0, ($sp)");
    break;
  }
}
// Loads to $X0
void ASM::Load_To_Reg(VarType ltype, VarType rtype, LoadType load_type,
                      int offset ) {
  std::string ptr = "";
  switch ( load_type ) {
    case LoadType::Stack: ptr = Accum(             "($sp)"); break;
    case LoadType::Frame: ptr = Accum("-", offset, "($fp)"); break;
  }
  if ( ltype == VarType::_int && rtype == VarType::_float ) {
    Write_ASM("la $t1, ", ptr);
    Write_ASM("l.s $f0, ($t1)");
    Write_ASM("cvt.w.s $f0, $f0");
    Write_ASM("mfc1 $a0, $f0");
    return;
  }
  std::cout << '\t';
  switch ( ltype ) {
    default: std::cerr << "TRYING TO LOAD STRING TO REGISTER!\n"; break;
    case VarType::_float: std::cout << "l.s $f0, ";  break;
    case VarType::_int:   std::cout << "lw $a0, ";   break;
  }
  std::cout << ptr << '\n';
  ASM::Convert_If_Necessary(ltype, rtype);
}
// Loads to stack from $X0
void ASM::Load_From_Reg(VarType ltype, VarType rtype, LoadType load_type,
                        int offset) {
  std::string ptr = "";
  switch ( load_type ) {
    case LoadType::Stack: ptr = Accum(             "($sp)"); break;
    case LoadType::Frame: ptr = offset<0?Accum(-offset, "($fp)"):
                                Accum("-", offset, "($fp)"); break;
  }
  std::cout << '\t';
  switch ( ltype ) {
    default: std::cout << "TRYING TO LOAD STRING TO STACK!\n"; break;
    case VarType::_float: std::cout << "s.s $f0, "; break;
    case VarType::_int:   std::cout << "sw $a0, ";   break;
  }
  std::cout << ptr << '\n';
  ASM::Convert_If_Necessary(ltype, rtype);
}
void ASM::Prepare_Register_From_Stack ( VarType ltype, VarType rtype ) {
  if ( ltype == rtype ) {
    Load_To_Reg(ltype, rtype, LoadType::Stack, 0);
  } else {
    if ( rtype == VarType::_int ) {
      Load_To_Reg(VarType::_float, VarType::_int, LoadType::Stack, 0);
    } else {
      Load_To_Reg(VarType::_float, VarType::_float, LoadType::Stack, 0);
    }
  }
  Pop_Stack(VarType_Size(ltype));
}
void ASM::Prepare_Registers_From_Stack ( VarType ltype, VarType rtype ) {
  if ( ltype == rtype ) { //int/int|float/float, no conversion needed
    Load_To_Reg(ltype, rtype, LoadType::Stack, 0);
    Pop_Stack(VarType_Size(ltype));
    Move(ltype, 0, 1);
    Load_To_Reg(ltype, rtype, LoadType::Stack, 0);
  } else { // float/int, needs conversion to float
    // [ltype, rtype] - load rtype as float
    if ( rtype == VarType::_int ) {
      Load_To_Reg(VarType::_float, VarType::_int, LoadType::Stack, 0);
    } else {
      Load_To_Reg(VarType::_float, VarType::_float, LoadType::Stack, 0);
    }
    Pop_Stack(VarType_Size(ltype));
    Move(VarType::_float, 0, 1);
    if ( ltype == VarType::_int ) {
      Load_To_Reg(VarType::_float, VarType::_int, LoadType::Stack, 0);
    } else {
      Load_To_Reg(VarType::_float, VarType::_float, LoadType::Stack, 0);
    }
  }
  Pop_Stack(VarType_Size(rtype));
}
VarType Reduce(VarType l, VarType r) {
  if ( l == VarType::_float || r == VarType::_float )
    return VarType::_float;
  return VarType::_int;
}
// retruns true if float
bool Store(VarType r) {
  ASM::Push_Stack(VarType_Size(r));
  auto res = r == VarType::_float;
  if ( res ) Write_ASM("s.s $f0, ($sp)");
  else       Write_ASM("sw $a0, ($sp)");
  return res;
}
void ASM::Neg(VarType r) {
  if ( r == VarType::_float ) Write_ASM("neg.s $f0, $f0");
  else                        Write_ASM("neg $a0, $a0");
  Store(r);
}
void ASM::Not(VarType r) {
  if ( r == VarType::_float ) {
    Assert(false, "; '!' operation on float");
  } else {
    Write_ASM("li $a1, 1");
    Write_ASM("xor $a0, $a0, $a1");
  }
  Store(r);
}
void ASM::Mul(VarType l, VarType r) {
  auto t = Reduce(l, r);
  if ( t == VarType::_float )
    Write_ASM("mul.s $f0, $f0, $f1");
  else {
    Write_ASM("mult $a0, $a1");
    Write_ASM("mflo $a0");
  }
  Store(t);
}
void ASM::Div(VarType l, VarType r) {
  auto t = Reduce(l, r);
  if ( t == VarType::_float )
    Write_ASM("div.s $f0, $f1, $f0");
  else {
    Write_ASM("div $a1, $a0");
    Write_ASM("mflo $a0");
  }
  Store(t);
}
void ASM::Add(VarType l, VarType r) {
  auto t = Reduce(l, r);
  if ( t == VarType::_float ) Write_ASM("add.s $f0, $f0, $f1");
  else                        Write_ASM("add $a0, $a0, $a1");
  Store(t);
}
void ASM::Sub(VarType l, VarType r) {
  auto t = Reduce(l, r);
  if ( t == VarType::_float ) Write_ASM("sub.s $f0, $f1, $f0");
  else                        Write_ASM("sub $a0, $a1, $a0");
  Store(t);
}
void ASM::And(VarType l, VarType r) {
  auto t = Reduce(l, r);
  if ( t == VarType::_float ) Write_ASM("and.s $f0, $f0, $f1");
  else                        Write_ASM("and $a0, $a0, $a1");
  Store(t);
}
void ASM::Or (VarType l, VarType r) {
  auto t = Reduce(l, r);
  if ( t == VarType::_float ) Write_ASM("or.s $f0, $f1, $f1");
  else                        Write_ASM("or $a0, $a0, $a1");
  Store(t);
}
enum CCC { LT, LTE, GT, GTE };
void _EQOP ( VarType l, VarType r, CCC c, std::string inop, std::string il, std::string ir ) {
  auto t = Reduce(l, r);
  if ( t == VarType::_float ) {
    static int bcount = 0;
    switch ( c ) {
      case CCC::LT:
        Write_ASM("c.lt.s $f1, $f0");
        Write_ASM("bc1t _cmp", bcount);
        Write_ASM("li $a0, 0");
        Write_ASM("b _acmp", bcount);
        std::cout << "_cmp" << bcount << ":" << '\n';
        Write_ASM("li $a0, 1");
        std::cout << "_acmp" << bcount << ":" << '\n';
      break;
      case CCC::LTE:
      Write_ASM("c.le.s $f1, $f0");
      Write_ASM("bc1t _cmp", bcount);
      Write_ASM("li $a0, 0");
      Write_ASM("b _acmp", bcount);
      std::cout << "_cmp" << bcount << ":" << '\n';
      Write_ASM("li $a0, 1");
      std::cout << "_acmp" << bcount << ":" << '\n';
      break;
      case CCC::GT:
      Write_ASM("c.lt.s $f0, $f1");
      Write_ASM("bc1t _cmp", bcount);
      Write_ASM("li $a0, 0");
      Write_ASM("b _acmp", bcount);
      std::cout << "_cmp" << bcount << ":" << '\n';
      Write_ASM("li $a0, 1");
      std::cout << "_acmp" << bcount << ":" << '\n';
      break;
      case CCC::GTE:
      Write_ASM("c.le.s $f0, $f1");
      Write_ASM("bc1t _cmp", bcount);
      Write_ASM("li $a0, 0");
      Write_ASM("b _acmp", bcount);
      std::cout << "_cmp" << bcount << ":" << '\n';
      Write_ASM("li $a0, 1");
      std::cout << "_acmp" << bcount << ":" << '\n';
      break;
    }
    ++bcount;
  } else {
    Write_ASM(inop + " $a0, $a" + il + ", $a" + ir);
  }
  Store(VarType::_int);
}
/*
  "lt", "1", "0",
  "le", "1", "0",
  "gt", "1", "0",
  "ge", "1", "0",
*/
using VT = VarType;
void ASM::LT (VT l, VT r) { _EQOP(l, r,  CCC::LT,  "slt", "1", "0"); }
void ASM::LTE(VT l, VT r) { _EQOP(l, r,  CCC::LTE, "sge", "0", "1"); }
void ASM::GT (VT l, VT r) { _EQOP(l, r,  CCC::GT,  "slt", "0", "1"); }
void ASM::GTE(VT l, VT r) { _EQOP(l, r,  CCC::GTE, "sge", "1", "0"); }
void _EQOP ( VT l, VT r, bool rev ) {
  auto t = Reduce(l, r);
  if ( t == VarType::_float ) {
    static int bcount=0;
    ++bcount;
    Write_ASM("c.eq.s $f0, $f1");
    Write_ASM("bc1t _eq", bcount);
    Write_ASM("li $a0, ", (rev?"1":"0"));
    Write_ASM("b _aeq", bcount);
    std::cout << "_eq" << bcount << ":" << '\n';
    Write_ASM("li $a0, ", (rev?"0":"1"));
    std::cout << "_aeq" << bcount << ":" << '\n';
  } else {
    Write_ASM("seq $a0, $a0, $a1");
  }
  Store(VarType::_int);
}
void ASM::EQ (VT l, VT r) {
  _EQOP(l, r, false);
}
void ASM::NEQ(VT l, VT r) {
  _EQOP(l, r, true);
}

void ASM::Mod(VarType l, VarType r) {
  Push_Stack(4);
  Assert(l != VarType::_float && r != VarType::_float,
         "; Can't perform modulo on ",
         VarType_String(l), ", ", VarType_String(r));
  Write_ASM("div $a1, $a0");
  Write_ASM("mfhi $a0");
  Write_ASM("sw $a0, ($sp)");
}
void ASM::Convert_If_Necessary ( VarType from, VarType to ) {
  if ( from == to ) return;
  switch ( from ) {
  case VarType::_int:
    Write_ASM("cvt.w.s $f0, $f0");
    break;
  case VarType::_float:
      Write_ASM("cvt.s.w $f0, $f0");
    break;
  }
}
// ---- BRANCHING ----
void ASM::Jump ( VarType var_type, ASM::JumpType jump_type, std::string label ) {
  // load to register
  Load_To_Reg(var_type, var_type, LoadType::Stack, VarType_Size(var_type));
  switch ( jump_type ) {
    case ASM::JumpType::unconditional: Write_ASM(Accum("j ", label)); break;
    case ASM::JumpType::zero:
      if ( var_type == VarType::_int ) {
        Write_ASM("li $a1, 0");
        Write_ASM(Accum("beq $a0, $a1,", label));
      } else {
        Write_ASM("l.s $f1, flzero");
        Write_ASM(Accum("beq $f0, $f1,", label));
      }
    break;
    case ASM::JumpType::not_zero:
      if ( var_type == VarType::_int ) {
        Write_ASM("li $a1, 0");
        Write_ASM(Accum("bnq $a0, $a1,", label));
      } else {
        Write_ASM("l.s $f1, flzero");
        Write_ASM(Accum("bnq $f0, $f1,", label));
      }
    break;
  }
}
void ASM::Call(std::string fn, VarType passtype, VarType paramtype, VarType ret) {
  if ( passtype != VarType::_unknown ) {
    Assert(paramtype != VarType::_unknown, "parameter mismatch");
    Prepare_Register_From_Stack(passtype, paramtype);
    Write_ASM("sub $sp, $sp, 8");
    Write_ASM("lw $t0, -4($fp)");
    Write_ASM("sub $sp, $sp, 4");
    Write_ASM("sw $t0, ($sp)");
    if ( passtype == VarType::_float ) {
      Write_ASM("s.s $f0, ($sp)");
    } else {
      Write_ASM("sw $a0, ($sp)");
    }
  } else {
    Assert(paramtype == VarType::_unknown, "parameter mismatch");
    Write_ASM("sub $sp, $sp, 8");
  }
  Write_ASM("jal _fn" + fn);
  if ( ret == VarType::_int || ret == VarType::_float )
    Store(ret);
}
void ASM::Emit_Label(std::string lbl) {
  std::cout << lbl << ":" << "\n"; // no tabs
}
void ASM::Emit_Start_Function(std::string lbl, VarType vt) {
  Emit_Label("_fn" + lbl);
  Write_ASM("sw $fp, 8($sp)");
  Write_ASM("sw $ra, 4($sp)");
  Write_ASM("la $fp, 4($sp)");
  switch ( vt ) {
    case VarType::_unknown:
    break;
    case VarType::_float:
      Write_ASM("l.s $f0, -4($fp)");
      Write_ASM("sub $sp, $sp, 4");
      Write_ASM("s.s $f0, -4($sp)");
      Write_ASM("s.s $f0, -16($fp)");
    break;
    case VarType::_int:
      Write_ASM("lw $a0, -4($fp)");
      Write_ASM("sub $sp, $sp, 4");
      Write_ASM("sw $a0, -4($sp)");
      Write_ASM("sw $a0, -8($fp)");
    break;
  }
}
void ASM::Emit_End_Function(std::string lbl, VarType vt) {
  Emit_Label("_ret"+lbl);
  Write_ASM("move $sp, $fp");
  Write_ASM("lw $fp, 4($sp)");
  Write_ASM("lw $ra, ($sp)");
  Write_ASM("add $sp, $sp, 8");
  Write_ASM("jalr $ra");
}
// -------------------
void ASM::Program_Entry ( ) {
  std::cout << "main:\n";
  Write_ASM("sub $sp, $sp, 8");
  Write_ASM("jal _fncommencement");
}
void ASM::Program_Exit ( ) {
  Write_ASM("li $v0, 10");
  Write_ASM("syscall");
}
void ASM::Start_Data_Section ( ) {
  Write_ASM(".data");
}
// ---- functions ----
void ASM::Syscall ( ) {
  Write_ASM("syscall");
}
void ASM::Print_Str ( int offset ) {
  Write_ASM("li $v0, 4");
  Write_ASM("la $a0, str", offset);
  Syscall();
}
std::string ASM::RProper_Register ( VarType type ) {
  switch ( type ) {
    case VarType::_str:    return "a";
    case VarType::_int:    return "a";
    case VarType::_float:  return "f";
  }
  return "";
}
// move $XL, $XR
void ASM::Move(VarType type, int from, int to) {
  auto&& regstr = RProper_Register(type);
  Write_ASM((type == VarType::_float ? "mov.s" : "move"), " $",
              regstr, to, ", $", regstr, from);
}
// Grabs from the stack and prints
void ASM::Print ( VarType type, int offset ) {
  int print_type = type == VarType::_float ? 2 : 1;
  ASM::Load_To_Reg(type, type, LoadType::Stack, offset);
  if ( type == VarType::_float ) {
    Move(VarType::_float, 0, 12);
  }
  Write_ASM("li $v0, ", print_type);
  Syscall();
}
