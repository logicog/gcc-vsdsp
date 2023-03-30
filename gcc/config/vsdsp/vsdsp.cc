/* Target Code for vsdsp
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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "df.h"
#include "regs.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "output.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "expr.h"
#include "builtins.h"
#include "vsdsp-protos.h"

/* This file should be included last.  */
#include "target-def.h"

/* Worker function for TARGET_RETURN_IN_MEMORY.  */

static bool
vsdsp_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return (size == -1 || size > 2 * UNITS_PER_WORD);
}

enum reg_class
vsdsp_regno_reg_class (int r)
{
   static const enum reg_class reg_class_tab[] =
    {
      /* a0 .. d2 */
      GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
      GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
      GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
      /* i0 .. i7 */
      POINTER_REGS, POINTER_REGS, POINTER_REGS, POINTER_REGS,
      POINTER_REGS, POINTER_REGS, POINTER_REGS, POINTER_REGS,
      /* lr0, lr1, mr0, lc, ls, le, ipr0, ipr1*/
      SPECIAL_REGS, SPECIAL_REGS, SPECIAL_REGS, SPECIAL_REGS,
      SPECIAL_REGS, SPECIAL_REGS, SPECIAL_REGS, SPECIAL_REGS,
      /* p0, p1, pc */
      SPECIAL_REGS, SPECIAL_REGS, SPECIAL_REGS
    };

  if (r <= 30)
    return reg_class_tab[r];

  return ALL_REGS;
}

rtx
vsdsp_function_value (const_tree valtype, 
                    const_tree fn_decl_or_type ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (valtype), VSDSP_A0);
}


/* Return the next register to be used to hold a function argument or
   NULL_RTX if there's no more space.  */
static rtx
vsdsp_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (*cum < 8)
    return gen_rtx_REG (arg.mode, *cum);
  else 
    return NULL_RTX;
}

#define VSDSP_FUNCTION_ARG_SIZE(MODE, TYPE)     \
  ((MODE) != BLKmode ? GET_MODE_SIZE (MODE)     \
   : (unsigned) int_size_in_bytes (TYPE))

static void
vsdsp_function_arg_advance (cumulative_args_t cum_v,
                            const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  *cum = (*cum < VSDSP_D0
          ? *cum + ((3 + VSDSP_FUNCTION_ARG_SIZE (arg.mode, arg.type)) / 4)
          : *cum);
}

/* The Global `targetm' Variable. */

/* Initialize the GCC target structure.  */

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES	hook_bool_const_tree_true

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY		vsdsp_return_in_memory
#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK	must_pass_in_stack_var_size
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE    hook_pass_by_reference_must_pass_in_stack

#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG             vsdsp_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE     vsdsp_function_arg_advance



struct gcc_target targetm = TARGET_INITIALIZER;

void
vsdsp_print_operand_address (FILE *file, rtx x)
{
  switch (GET_CODE (x))
    {
    case REG:
      fprintf (file, "(%s)", reg_names[REGNO (x)]);
      break;
      
    case PLUS:
      switch (GET_CODE (XEXP (x, 1)))
	{
	case CONST_INT:
	  fprintf (file, "%ld(%s)", 
		   INTVAL(XEXP (x, 1)), reg_names[REGNO (XEXP (x, 0))]);
	  break;
	case SYMBOL_REF:
	  output_addr_const (file, XEXP (x, 1));
	  fprintf (file, "(%s)", reg_names[REGNO (XEXP (x, 0))]);
	  break;
	case CONST:
	  {
	    rtx plus = XEXP (XEXP (x, 1), 0);
	    if (GET_CODE (XEXP (plus, 0)) == SYMBOL_REF 
		&& CONST_INT_P (XEXP (plus, 1)))
	      {
		output_addr_const(file, XEXP (plus, 0));
		fprintf (file,"+%ld(%s)", INTVAL (XEXP (plus, 1)),
			 reg_names[REGNO (XEXP (x, 0))]);
	      }
	    else
	      abort();
	  }
	  break;
	default:
	  abort();
	}
      break;

    default:
      output_addr_const (file, x);
      break;
    }
}

void
vsdsp_print_operand (FILE *file, rtx x, int code)
{
  rtx operand = x;

  /* New code entries should just be added to the switch below.  If
     handling is finished, just return.  If handling was just a
     modification of the operand, the modified operand should be put in
     "operand", and then do a break to let default handling
     (zero-modifier) output the operand.  */

  switch (code)
    {
    case 0:
      /* No code, print as usual.  */
      break;

    default:
      printf ("invalid operand modifier letter %d", code);
      return;
    }

  /* Print an operand as without a modifier letter.  */
  switch (GET_CODE (operand))
    {
    case REG:
      fprintf (file, "%s", reg_names[REGNO (operand)]);
      return;

    case MEM:
      output_address (GET_MODE (XEXP (operand, 0)), XEXP (operand, 0));
      return;

    default:
      /* No need to handle all strange variants, let output_addr_const
	 do it for us.  */
      if (CONSTANT_P (operand))
	{
	  output_addr_const (file, operand);
	  return;
	}

      printf ("unexpected operand");
      return;
    }
}
