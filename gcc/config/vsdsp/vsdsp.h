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

/* Layout of Source Language Data Types */

#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 1

/* Registers...

   $fp - frame pointer
   $sp - stack pointer
   $r0 - general purpose 32-bit register.
   $r1 - general purpose 32-bit register.
   $r2 - general purpose 32-bit register.
   $r3 - general purpose 32-bit register.
   $r4 - general purpose 32-bit register.
   $r5 - general purpose 32-bit register.

   Special Registers...

   $pc - 32-bit program counter.
   
*/

#define REGISTER_NAMES {	\
  "$fp", "$sp", "$r0", "$r1",   \
  "$r2", "$r3", "$r4", "$r5",   \
  "$pc" }

#define FIRST_PSEUDO_REGISTER 9

enum reg_class
{
  NO_REGS,
  GENERAL_REGS,
  SPECIAL_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define CONSTRAINT_LEN(CHAR,STR) DEFAULT_CONSTRAINT_LEN(CHAR,STR)
#define REG_CLASS_FROM_CONSTRAINT(CHAR,STR) (abort(), 0)
#define CONST_OK_FOR_CONSTRAINT_P(VALUE,C,STR) \
  (abort(), 0)
#define CONST_DOUBLE_OK_FOR_CONSTRAINT_P(VALUE,C,STR) 0
#define EXTRA_CONSTRAINT_STR(VALUE,C,STR) \
  (abort(), 0)
#define EXTRA_MEMORY_CONSTRAINT(C,STR) \
  (abort(), 0)
#define EXTRA_ADDRESS_CONSTRAINT(C,STR) \
  (abort(), 0)

#define REG_CLASS_CONTENTS \
{ { 0x00000000 }, /* Empty */			\
  { (1<<9)-1 },   /* $fp, $sp, $r0 to $r5 */ \
  { (1<<8) },     /* $pc */	\
  { (1<<10)-1 }   /* All registers */ \
}

#define N_REG_CLASSES LIM_REG_CLASSES

#define REG_CLASS_NAMES {\
    "NO_REGS", \
    "GENERAL_REGS", \
    "SPECIAL_REGS", \
    "ALL_REGS" }

#define FIXED_REGISTERS     { 1, 1, 0, 0, \
			      0, 0, 0, 0, \
                              1 }

#define CALL_USED_REGISTERS { 1, 1, 1, 0, \
			      0, 0, 0, 1, \
                              1 }

/* A C expression that is nonzero if it is permissible to store a
   value of mode MODE in hard register number REGNO (or in several
   registers starting with that one).  All gstore registers are 
   equivalent, so we can set this to 1.  */
#define HARD_REGNO_MODE_OK(R,M) 1

/* A C expression whose value is a register class containing hard
   register REGNO.  */
#define REGNO_REG_CLASS(R) ((R < 9) ? GENERAL_REGS : SPECIAL_REGS)

/* A C expression for the number of consecutive hard registers,
   starting at register number REGNO, required to hold a value of mode
   MODE.  */
#define HARD_REGNO_NREGS(REGNO, MODE)			   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)		   \
   / UNITS_PER_WORD)

/* A C expression that is nonzero if a value of mode MODE1 is
   accessible in mode MODE2 without copying.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 1

/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.  */
#define CLASS_MAX_NREGS(CLASS, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* A C expression that places additional restrictions on the register
   class to use when it is necessary to copy value X into a register
   in class CLASS.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS

/* The Overall Framework of an Assembler File */

#define ASM_COMMENT_START "#"
#define ASM_APP_ON ""
#define ASM_APP_OFF ""

/* Assembler Commands for Alignment */

#define ASM_OUTPUT_ALIGN(STREAM,POWER) \
	fprintf (STREAM, "\t.p2align\t%d\n", POWER);

#define PRINT_OPERAND(S,X,C) (abort(), 0)
#define PRINT_OPERAND_ADDRESS(S,X) (abort(), 0)

/* Output and Generation of Labels */

#define GLOBAL_ASM_OP "\t.global\t"

/* Passing Arguments in Registers */

#define FUNCTION_ARG(CA,MODE,TYPE,NAMED) \
  (abort(), 0)

#define CUMULATIVE_ARGS int
#define INIT_CUMULATIVE_ARGS(CA,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS) \
  (abort())
#define FUNCTION_ARG_ADVANCE(CA,MODE,TYPE,NAMED) \
  (abort())

/* How Scalar Function Values Are Returned */

#define FUNCTION_VALUE(VT,F) (abort(), 0)
#define LIBCALL_VALUE(MODE) (abort(), 0)

/* STACK AND CALLING */

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 0;
#define STARTING_FRAME_OFFSET 0
#define FIRST_PARM_OFFSET(F) 0

/* Storage Layout */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1

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
#define INITIALIZE_TRAMPOLINE(a,fn,sc) (abort (), 0)

/* An alias for the machine mode for pointers.  */
#define Pmode         SImode

/* An alias for the machine mode used for memory references to
   functions being called, in `call' RTL expressions.  */
#define FUNCTION_MODE QImode

/* The register number of the stack pointer register, which must also
   be a fixed register according to `FIXED_REGISTERS'.  */
#define STACK_POINTER_REGNUM 1

/* The register number of the frame pointer register, which is used to
   access automatic variables in the stack frame.  */
#define FRAME_POINTER_REGNUM 0

/* The register number of the arg pointer register, which is used to
   access the function's argument list.  */
#define ARG_POINTER_REGNUM 0

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */
#define FUNCTION_ARG_REGNO_P(r) (r > 1 && r < 4)

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which the values of called function may come back.  */
#define FUNCTION_VALUE_REGNO_P(r) (r == 2)

/* A macro whose definition is the name of the class to which a vqalid
   base register must belong.  A base register is one used in an
   address which is the register value plus a displacement.  */
#define BASE_REG_CLASS NO_REGS

#define INDEX_REG_CLASS NO_REGS

/* A C expression which is nonzero if register number NUM is suitable
   for use as a base register in operand addresses.  */
#ifdef REG_OK_STRICT
#define REGNO_OK_FOR_BASE_P(NUM)                 \
  (((NUM < FIRST_PSEUDO_REGISTER)                \
    && ((REGNO_REG_CLASS(NUM) == GENERAL_REGS)   \
        || (NUM == HARD_FRAME_POINTER_REGNUM))))
#else
#define REGNO_OK_FOR_BASE_P(NUM)                 \
  ((REGNO_REG_CLASS(NUM) == GENERAL_REGS)        \
   || (NUM >= FIRST_PSEUDO_REGISTER))
#endif

/* A C expression which is nonzero if register number NUM is suitable
   for use as an index register in operand addresses.  */
#define REGNO_OK_FOR_INDEX_P(NUM) 0

/* The maximum number of bytes that a single instruction can move
   quickly between memory and registers or between two memory
   locations.  */
#define MOVE_MAX 4
#define TRULY_NOOP_TRUNCATION(op,ip) 1

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0

#define LEGITIMATE_CONSTANT_P(X) (abort(), 0)

#define FRAME_POINTER_REQUIRED 1

/* A C expression that is 1 if the RTX X is a constant which is a
   valid address.  */
#define CONSTANT_ADDRESS_P(X) CONSTANT_P(X)

/* A number, the maximum number of registers that can appear in a
   valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

#define TRULY_NOOP_TRUNCATION(op,ip) 1

/* An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* A C compound statement with a conditional `goto LABEL;' executed
   if X (an RTX) is a legitimate memory address on the target machine
   for a memory operand of mode MODE.  */
#define GO_IF_LEGITIMATE_ADDRESS(MODE,X,LABEL)		\
  do {                                                  \
    if (REG_P (X) && REGNO_OK_FOR_BASE_P (REGNO (X)))	\
      goto LABEL;					\
    if (GET_CODE (X) == SYMBOL_REF			\
	|| GET_CODE (X) == LABEL_REF			\
	|| GET_CODE (X) == CONST)			\
      goto LABEL;					\
  } while (0)

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LEBEL)

/* Run-time Target Specification */

#define TARGET_CPU_CPP_BUILTINS() \
  { \
    builtin_assert ("cpu=vsdsp"); \
    builtin_assert ("machine=vsdsp"); \
    builtin_define ("__vsdsp__=1"); \
  }

#endif /* GCC_VSDSP_H */
