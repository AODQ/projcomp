# french C-like compiler

Uses flex and bison to write a compiler for a crude C-like language that generates MIPS assembly.

Features include;
	Functions (with prototypes and recursion)
	Barebone metaprogramming via #define, #include, #sidef, etc
	Basic arithmetic (*, /, %, +, -) with proper priority
	Integer and float support; implicit int/float conversions
	Conditional statements & branches
