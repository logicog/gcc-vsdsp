/* Target Definitions for vsdsp.
   Copyright (C) 2022 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


#ifndef GCC_VSDSP_H
#define GCC_VSDSP_H

/* Storage Layout */
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1
#define REG_WORDS_BIG_ENDIAN 0

/* Addressing modes */
#define HAVE_PRE_INCREMENT 0
#define HAVE_PRE_DECREMENT 0
#define HAVE_POST_INCREMENT 1
#define HAVE_POST_DECREMENT 1

/* Nonzero if the machine supports pre- or post-address side-effect
   generation involving constants other than the size of the memory
   operand. */
#define HAVE_PRE_MODIFY_DISP 0
#define HAVE_POST_MODIFY_DISP 0

/* Nonzero if the machine supports pre- or
   post-address side-effect generation involving a register displacement. */
#define  HAVE_PRE_MODIFY_REG 0
#define HAVE_POST_MODIFY_REG 1

/* We stick to BITS_PER_UNIT = 8 as this is the size of the
 * a2, b2, c2, d2 registers */

/* Number of storage units in a word; normally the size of a
   general-purpose register, a power of two from 1 or 8.  */
#define UNITS_PER_WORD 2
#define POINTER_SIZE 16

/* Layout of Source Language Data Types */
#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 1

#define VSDSP_A0	0
#define VSDSP_A1	1
#define VSDSP_A2	2
#define VSDSP_B0	3
#define VSDSP_B1	4
#define VSDSP_B2	5
#define VSDSP_C0	6
#define VSDSP_C1	7
#define VSDSP_C2	8
#define VSDSP_D0	9
#define VSDSP_D1	10
#define VSDSP_D2	11
#define VSDSP_I0	12
#define VSDSP_I1	13
#define VSDSP_I2	14
#define VSDSP_I3	15
#define VSDSP_I4	16
#define VSDSP_I5	17
#define VSDSP_I6	18
#define VSDSP_I7	19
#define VSDSP_LR0	20
#define VSDSP_LR1	21
#define VSDSP_MR0	22
#define VSDSP_LC	23
#define VSDSP_LS	24
#define VSDSP_LE	25
#define VSDSP_IPR0	26
#define VSDSP_IPR1	27
#define VSDSP_P0	28
#define VSDSP_P1	29
#define VSDSP_PC	30
// A, B, C, D??? NULL, ONES???

#define REGISTER_NAMES { \
  "a0", "a1", "a2", "b0", "b1", "b2", \
  "c0", "c1", "c2", "d0", "d1", "d2", \
  "i0", "i1", "i2", "i3", "i4", "i5", "i6", "i7", \
  "lr0", "lr1", "mr0", "lc", "ls", "le", "ipr0", "ipr1", \
  "p0", "p1", "pc" }

  
#define FIRST_PSEUDO_REGISTER 31

enum reg_class
{
  NO_REGS,
  ALU_REGS,
  EXTENSION_REGS,
  DATA_REGS,
  ADDR_REGS,
  GENERAL_REGS,
  ACC_REGS,
  SPECIAL_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

extern enum reg_class vsdsp_regno_reg_class (int r);
extern rtx vsdsp_function_value (const_tree, const_tree);

#define REG_CLASS_CONTENTS \
{  { 0x00000000 },  /* Empty */				\
   { 0x000006db },  /* ALU_REGS: a0, a1, .. d0, d1 */   \
   { 0x00000924 },  /* EXTENSION_REGS: a2 .. d2 */   	\
   { 0x00000fff },  /* DATA_REGS: a0 .. d2 */   	\
   { 0x000ff000 },  /* ADDR_REGS: i0 .. i7 */ 		\
   { 0x000fffff },  /* GENERAL_REGS */			\
   { 0x30000000 },  /* p0, p1 */			\
   { 0x4ff00000 },  /* lr, lr1, mr0, lc, ls, le, ipr0, ipr1, pc */ \
   { 0x7fffffff }   /* All registers */		\
}

#define N_REG_CLASSES LIM_REG_CLASSES

#define FIXED_REGISTERS { 0, 0, 0, 0, /* a0 .. b0 */ \
			  0, 0, 0, 0, /* b1 .. c1 */ \
			  0, 0, 0, 0, /* c2 .. d2 */ \
			  0, 0, 0, 0, /* i0 .. i3 */ \
			  0, 0, 1, 1, /* i4 .. i7, i6 is sp, i7 is fp */ \
			  1, 1, 1, 1, /* lr0, lr1, mr0, lc */ \
			  1, 1, 1, 1, /* ls, le, ipr0, ipr1 */ \
			  1, 1, 1 } /* p0, p1, pc */

#define CALL_REALLY_USED_REGISTERS  { \
			  1, 1, 1, 0, /* a0 .. b0 */ \
			  0, 0, 0, 0, /* b1 .. c1 */ \
			  0, 0, 0, 0, /* c2 .. d2 */ \
			  0, 0, 0, 0, /* i0 .. i3 */ \
			  0, 0, 1, 1, /* i4 .. i7, i6 is sp */ \
			  1, 1, 1, 1, /* lr0, lr1, mr0, lc */ \
			  1, 1, 1, 1, /* ls, le, ipr0, ipr1 */ \
			  1, 1, 1 } /* p0, p1, pc */

#define REG_CLASS_NAMES {\
    "NO_REGS", \
    "DATA_REGS", \
    "ADDR_REGS", \
    "GENERAL_REGS", \
    "ACC_REGS", \
    "SPECIAL_REGS", \
    "ALL_REGS" }

/* Program Counter is register number 30 */
#define PC_REGNUM 30

/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.  */
#define CLASS_MAX_NREGS(CLASS, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* A C expression that places additional restrictions on the register
   class to use when it is necessary to copy value X into a register
   in class CLASS.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS


/* Address spaces: xmem, ymem and imem for the 3 memory busses */
enum vsdsp_address_spaces
{
  ADDR_SPACE_DEFAULT = 0,
  ADDR_SPACE_XMEM,
  ADDR_SPACE_YMEM,
  ADDR_SPACE_IMEM,
};

#define REGISTER_TARGET_PRAGMAS() do {                 \
  c_register_addr_space ("__xmem", ADDR_SPACE_XMEM);   \
  c_register_addr_space ("__ymem", ADDR_SPACE_YMEM);   \
  c_register_addr_space ("__imem", ADDR_SPACE_IMEM);   \
} while (0);


/* The Overall Framework of an Assembler File */

#define ASM_COMMENT_START "#"
#define ASM_APP_ON ""
#define ASM_APP_OFF ""

/* Assembler Commands for Alignment */

#define ASM_OUTPUT_ALIGN(STREAM,POWER) \
	fprintf (STREAM, "\t.p2align\t%d\n", POWER);
extern void vsdsp_print_operand (FILE *, rtx, int);
extern void vsdsp_print_operand_address (FILE *, rtx);

#define PRINT_OPERAND(FILE, X, CODE) vsdsp_print_operand (FILE, X, CODE)

#define PRINT_OPERAND_ADDRESS(FILE, A) vsdsp_print_operand_address (FILE, A)

/* Output and Generation of Labels */

#define GLOBAL_ASM_OP "\t.global\t"

/* Passing Arguments in Registers */

/* How Scalar Function Values Are Returned */

#define FUNCTION_VALUE(VALTYPE, FUNC) \
    vsdsp_function_value (VALTYPE, FUNC)

/*
#define LIBCALL_VALUE(MODE) (abort(), 0)
*/

/* STACK AND CALLING */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
/* #define STACK_GROWS_DOWNWARD */

#define STACK_PUSH_CODE PRE_INC

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 0;
#define FIRST_PARM_OFFSET(F) 0

/* Alignment required for a function entry point, in bits.  */
#define FUNCTION_BOUNDARY 16

/* Biggest alignment that any data type can require on this machine,
   in bits.  */
#define BIGGEST_ALIGNMENT 16

/* Instrutions will fail to execute if not strictly aligned.  */
#define STRICT_ALIGNMENT 1

/* Define this macro as a C expression which is nonzero if accessing
   less than a word of memory (i.e. a `char' or a `short') is no
   faster than accessing a word of memory.  */
#define SLOW_BYTE_ACCESS 1

/* Number of storage units in a word; normally the size of a
   general-purpose register, a power of two from 1 or 8.  */
#define UNITS_PER_WORD 2

/* Define this macro to the minimum alignment enforced by hardware
   for the stack pointer on this machine.  The definition is a C
   expression for the desired alignment (measured in bits).  */
#define STACK_BOUNDARY 32

/* Normal alignment required for function parameters on the stack, in
   bits.  All stack parameters receive at least this much alignment
   regardless of data type.  */
#define PARM_BOUNDARY 32

/* Generating Code for Profiling */
#define FUNCTION_PROFILER(FILE,LABELNO) (abort (), 0)

/* Trampolines for Nested Functions.  Abort for now.  */
#define TRAMPOLINE_SIZE (abort (), 0)
#define TRAMPOLINE_ALIGNMENT (abort (), 0)

/* An alias for the machine mode for pointers.  */
#define Pmode         HImode

/* An alias for the machine mode used for memory references to
   functions being called, in `call' RTL expressions.  */
#define FUNCTION_MODE HImode

/* The register number of the stack pointer register, which must also
   be a fixed register according to `FIXED_REGISTERS'.  */
#define STACK_POINTER_REGNUM VSDSP_I6

/* The register number of the frame pointer register, which is used to
   access automatic variables in the stack frame.  We choose i7 */
#define FRAME_POINTER_REGNUM VSDSP_I7

/* The register number of the arg pointer register, which is used to
   access the function's argument list.  */
#define ARG_POINTER_REGNUM VSDSP_I7

/* This macro specifies a table of register pairs used to eliminate
 * unneeded registers that point into the stack frame.
 * The definition of this macro is a list of structure initializations,
 * each of which specifies an original and replacement register. */
#define ELIMINABLE_REGS {                                       \
    { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM },               \
    { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM },               \
    { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM },             \
    { FRAME_POINTER_REGNUM + 1, STACK_POINTER_REGNUM + 1 } }

/* This macro returns the initial difference between the specified pair
   of registers.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)                    \
  do {                                                                  \
    (OFFSET) = 2; break;        \
  } while (0)

/* Return address register is LR0, LR1 in irq context */
#define RETURN_ADDRESS_POINTER_REGNUM VSDSP_LR0

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.
   use a0-d2, i0-i5*/
#define FUNCTION_ARG_REGNO_P(r) (r >= 3 && r < VSDSP_I6)

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which the values of called function may come back.  */
#define FUNCTION_VALUE_REGNO_P(r) (r == 0)

/* A macro whose definition is the name of the class to which a valid
   base register must belong.  A base register is one used in an
   address which is the register value plus a displacement.  */
#define BASE_REG_CLASS ADDR_REGS

#define INDEX_REG_CLASS NO_REGS

/* Map register number to its class */
#define REGNO_REG_CLASS(R) vsdsp_regno_reg_class(R)

/* True for address registers, i0 through i7.  */
#define ADDRESS_REGNO_P(REGNO)  (IN_RANGE (REGNO, 12, 19))

/* A C expression which is nonzero if register number NUM is suitable
   for use as a base register in operand addresses.  */
#define REGNO_OK_FOR_BASE_P(NUM)  (ADDRESS_REGNO_P(NUM))

/* A C expression which is nonzero if register number NUM is suitable
   for use as an index register in operand addresses.  */
#define REGNO_OK_FOR_INDEX_P(NUM) 0

/* The maximum number of bytes that a single instruction can move
   quickly between memory and registers or between two memory
   locations.  */
#define MOVE_MAX 2

/* A C expression that is 1 if the RTX X is a constant which is a
   valid address.  */
#define CONSTANT_ADDRESS_P(X) CONSTANT_P(X)

/* A number, the maximum number of registers that can appear in a
   valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

/* A C type for declaring a variable that is used as the first
   argument of `FUNCTION_ARG' and other related values.  */
#define CUMULATIVE_ARGS unsigned int

/* A C statement (sans semicolon) for initializing the variable CUM
   for the state at the beginning of the argument list.  
   For moxie, the first arg is passed in register 2 (aka $r0).  */
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS) \
  (CUM = VSDSP_A2)

/* An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have.  */
#define CASE_VECTOR_MODE SImode
  
#define TEXT_SECTION_ASM_OP "\t.text"
#define DATA_SECTION_ASM_OP "\t.data"
#define BSS_SECTION_ASM_OP "\t.section .bss"
  
/* Run-time Target Specification */

#define TARGET_CPU_CPP_BUILTINS() \
  { \
    builtin_assert ("cpu=vsdsp"); \
    builtin_assert ("machine=vsdsp"); \
    builtin_define ("__vsdsp__=1"); \
  }
  
#define POINTERS_EXTEND_UNSIGNED 1

#endif /* GCC_VSDSP_H */
