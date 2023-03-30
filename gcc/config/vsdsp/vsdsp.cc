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
#include "print-rtl.h"

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
      ALU_REGS, ALU_REGS, EXTENSION_REGS, ALU_REGS,
      ALU_REGS, EXTENSION_REGS, ALU_REGS, ALU_REGS,
      EXTENSION_REGS, ALU_REGS, ALU_REGS, EXTENSION_REGS,
      /* i0 .. i7 */
      ADDR_REGS, ADDR_REGS, ADDR_REGS, ADDR_REGS,
      ADDR_REGS, ADDR_REGS, ADDR_REGS, ADDR_REGS,
      /* lr0, lr1, mr0, lc, ls, le, ipr0, ipr1*/
      SPECIAL_REGS, SPECIAL_REGS, SPECIAL_REGS, SPECIAL_REGS,
      SPECIAL_REGS, SPECIAL_REGS, SPECIAL_REGS, SPECIAL_REGS,
      /* p0, p1, pc */
      ACC_REGS, ACC_REGS, SPECIAL_REGS
    };

  if (r <= 30)
    return reg_class_tab[r];

  return ALL_REGS;
}

/* Per-function machine data.  */
struct GTY (()) machine_function
{
  /* The current lable number at the end of a doloop
   * Since we hw-loops cannot be nested, we only need this once */
  int doloop_label;
};

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
vsdsp_init_machine_status (void)
{
  return ggc_cleared_alloc < machine_function > ();
}

/* The TARGET_OPTION_OVERRIDE worker.
   All this curently does is set init_machine_status.  */
static void
vsdsp_option_override (void)
{
  /* Set the per-function-data initializer.  */
  init_machine_status = vsdsp_init_machine_status;
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

bool
vsdsp_print_operand_punct_valid_p (unsigned char code)
{
  return (code == '#');
}

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

    case POST_INC:
      fprintf (file, "(%s)+1", reg_names[REGNO (XEXP (x, 0))]);
      break;

    case POST_DEC:
      fprintf (file, "(%s)-1", reg_names[REGNO (XEXP (x, 0))]);
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

    case '#':
      /* Output a nop if there's nothing in the delay slot.  */
      if (dbr_sequence_length () == 0)
        fprintf (file, "\n\tnop");
      return;

    case 'R':
      /* A 32 bit register name for the ALU-registers a, b, c, d */
      if (GET_CODE (operand) == REG)
	{
	  fprintf (file, "%c", 'a' + (REGNO (operand) / 3));
	} else {
	  printf ("invalid operand for modifier letter R");
	  return;
	}

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

      printf ("unexpected operand, operand type %d, code %d\n", GET_CODE (operand), code);
      return;
    }
}

/* Valid attributes:
 * xmem     -  Put data X memory.
*/

/* Handle a "xmem" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
vsdsp_handle_xmem_attribute (tree *node, tree name,
				tree args ATTRIBUTE_UNUSED,
				int flags ATTRIBUTE_UNUSED,
				bool *no_add_attrs)
{
  printf("%s a\n", __func__);
  if (DECL_P (*node))
    {
      printf("%s b\n", __func__);
      if (TREE_CODE (*node) == TYPE_DECL)
	{
	  /* This is really a decl attribute, not a type attribute,
	     but try to handle it for GCC 3.0 backwards compatibility.  */

	  tree type = TREE_TYPE (*node);
	  tree attr = tree_cons (name, args, TYPE_ATTRIBUTES (type));
	  tree newtype = build_type_attribute_variant (type, attr);

	  TYPE_MAIN_VARIANT (newtype) = TYPE_MAIN_VARIANT (type);
	  TREE_TYPE (*node) = newtype;
	  *no_add_attrs = true;
	}
      else if (TREE_STATIC (*node) || DECL_EXTERNAL (*node))
	{
	  printf("%s c\n", __func__);
          *no_add_attrs = false;
	}
      else
	{
	  warning (OPT_Wattributes, "%qE attribute ignored",
		   name);
	  *no_add_attrs = true;
	}
    }

  printf("%s d\n", __func__);
  return NULL_TREE;
}

/* Look if DECL shall be placed in program memory space by
   means of attribute `progmem' or some address-space qualifier.
   Return non-zero if DECL is data that must end up in Flash and
   zero if the data lives in RAM (.bss, .data, .rodata, ...).

   Return 1  if attribute `xmem' occurs in DECL or ATTRIBUTES
   Return 0   otherwise  */

int
vsdsp_xmem_p (tree decl, tree attributes)
{
  tree a;

  printf("%s a\n", __func__);
  if (TREE_CODE (decl) != VAR_DECL)
    return 0;

  printf("%s b\n", __func__);
  if (NULL_TREE
      != lookup_attribute ("xmem", attributes))
    return 1;

  a = decl;

  printf("%s c\n", __func__);
  do
    a = TREE_TYPE(a);
  while (TREE_CODE (a) == ARRAY_TYPE);

  if (a == error_mark_node)
    return 0;

  printf("%s d\n", __func__);
  if (NULL_TREE != lookup_attribute ("xmem", TYPE_ATTRIBUTES (a)))
    return 1;

  printf("%s e NO XMEM\n", __func__);
  return 0;
}

/* Implement `TARGET_INSERT_ATTRIBUTES'.  */

static void
vsdsp_insert_attributes (tree node, tree *attributes)
{
  /* TODO: THIS DOEAS NOT WORK!!!! Add the section attribute if the variable is in progmem.  */

  if (TREE_CODE (node) == VAR_DECL
      && (TREE_STATIC (node) || DECL_EXTERNAL (node))
      && vsdsp_xmem_p (node, *attributes))
    {
      tree node0 = node;
      printf("%s called in, xmem attr found\n", __func__);

      return;
    }
}

/* Unnamed section callback for progmem*.data/.bss sections.  */

static void
vsdsp_output_progmem_section_asm_op (const char *data)
{
  fprintf (asm_out_file, "\t.section\t%s,\"a\",@progbits\n", data);
}

/* Implement TARGET_ASM_SELECT_SECTION.

   Return the section into which EXP should be placed.

static section *
vsdsp_asm_select_section (tree exp, int reloc, unsigned HOST_WIDE_INT align)
{
  if (TREE_TYPE (exp) != error_mark_node
      && TYPE_ADDR_SPACE (TREE_TYPE (exp)) == ADDR_SPACE_LDS)
    {
      if (!DECL_P (exp))
	return get_section (".lds_bss",
			    SECTION_WRITE | SECTION_BSS | SECTION_DEBUG,
			    NULL);

      return get_named_section (exp, ".lds_bss", reloc);
    }

  return default_elf_select_section (exp, reloc, align);
}
*/

static section *
vsdsp_asm_select_section (tree decl, int reloc, unsigned HOST_WIDE_INT align)
{
  section * sect = default_elf_select_section (decl, reloc, align);

  printf("%s called\n", __func__);
  if (TREE_TYPE (decl) == error_mark_node)
    return sect;
  
  if (sect->common.flags & SECTION_NAMED)
    printf("%s looking at section named %s\n", __func__, sect->named.name);
  else
    printf("%s looking at unnamed section\n", __func__);

  if (!vsdsp_xmem_p (decl, DECL_ATTRIBUTES (decl)))
    return sect;
  
  if (DECL_P(decl))
    printf ("%s is declaration\n", __func__);
  else
    printf ("%s NOT declaration\n", __func__);

  if (DECL_P (decl) && vsdsp_xmem_p (decl, DECL_ATTRIBUTES (decl)))
    {
      printf ("%s: yes\n", __func__);
      return get_named_section (decl, ".xmem_bss", reloc);
    }
  else {
    printf("%s that did not work\n", __func__);
  }
  return sect;
}

/* Implement TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P.
   
   Recognizes RTL expressions that are valid memory addresses for an
   instruction.  The MODE argument is the machine mode for the MEM
   expression that wants to use this address. */

static bool
vsdsp_addr_space_legitimate_address_p (machine_mode mode, rtx x, bool strict,
					addr_space_t as)
{
 //  printf("%s CALLED\n", __func__);
  return true;  
}

/* VSDSP attributes.  */
static const struct attribute_spec vsdsp_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "xmem",   0, 0, false, false, false, false,
    vsdsp_handle_xmem_attribute, NULL },
  { NULL,     0, 0, false, false, false, false, NULL, NULL }
};

/* Define how to find the value returned by a library function.

   We always return values in register a0/a for VSDSP.  */

static rtx
vsdsp_libcall_value (machine_mode mode,
                     const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, VSDSP_A0);
}

static unsigned int
vsdsp_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  int reg_size;

  switch (REGNO_REG_CLASS (regno))
    {
    case DATA_REGS:
    case ALU_REGS:
      /* Store 32bit integers in 40bit registers a,b,c,d which are a0-a2 */
      if (mode == SImode)
	return 2;
      reg_size = 16;
      break;
    case EXTENSION_REGS:
      reg_size = 8;
      break;
    default:
      reg_size = 16;
    }
  if (regno == VSDSP_MR0 && mode == CCmode)
    return 1;

  return CEIL (GET_MODE_SIZE (mode), reg_size);
}

static bool
vsdsp_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  /* We do not allow the register allocator to use the sign-extension
     registers but merely make use of them in explicit asm code. */

  if (REGNO_REG_CLASS(regno) == EXTENSION_REGS)
    return false;

  switch (mode)
    {
    case SImode:
      /* store 32bit ints in a, b, c, or d register */
      if (regno == VSDSP_A0 || regno == VSDSP_B0 
	  || regno == VSDSP_C0 || regno == VSDSP_D0)
	return true;
      if (REGNO_REG_CLASS (regno) == ADDR_REGS)
	return true;
      break;
    case HImode:
	return true;
      break;
    case QImode:
      return true;
    case SFmode:
    case DFmode:
      if (regno == VSDSP_A0 || regno == VSDSP_B0 
	  || regno == VSDSP_C0 || regno == VSDSP_D0)
	return true;
      break;
    default:
      return false;
    }
    
    return false;
}

/* VSDSP implementation of TARGET_CAN_USE_DOLOOP_P  */

bool
vsdsp_can_use_doloop_p (const widest_int &, const widest_int &,
                        unsigned int loop_depth, bool entered_at_top)
{
  return loop_depth == 1 && entered_at_top;
}

const char *
doloop_begin_output(rtx *operands)
{
  const char *s;

  rtx_code_label *end_label = gen_label_rtx ();

  /* loop's cannot be nested */
  gcc_assert(!cfun->machine->doloop_label);
  
  cfun->machine->doloop_label = CODE_LABEL_NUMBER (end_label);
  operands[1] = gen_rtx_LABEL_REF (VOIDmode, end_label);
  s = "loop %0, %l1-1";
  output_asm_insn (s, operands);

  printf("+++++!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! setting label %d\n", cfun->machine->doloop_label);
  return "nop";
}

const char *
doloop_end_output()
{
  printf("+++++!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! using label %d\n", cfun->machine->doloop_label);
  (*targetm.asm_out.internal_label) (asm_out_file, "L",
				      cfun->machine->doloop_label);
  cfun->machine->doloop_label = 0;
  return "nop";
}

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
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE 		vsdsp_libcall_value

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE vsdsp_attribute_table
#undef  TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES vsdsp_insert_attributes

#undef  TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION vsdsp_asm_select_section

#undef  TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P vsdsp_addr_space_legitimate_address_p

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS vsdsp_hard_regno_nregs

#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK vsdsp_hard_regno_mode_ok

#undef TARGET_CAN_USE_DOLOOP_P
#define TARGET_CAN_USE_DOLOOP_P vsdsp_can_use_doloop_p

// TODO: set TARGET_INVALID_WITHIN_DOLOOP

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE vsdsp_option_override

#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P vsdsp_print_operand_punct_valid_p

/* The Global `targetm' Variable. */

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-vsdsp.h"
