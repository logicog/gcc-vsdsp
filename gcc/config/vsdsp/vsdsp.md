;; Copyright (C) 2016-2022 Free Software Foundation, Inc.

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.


(include "constraints.md")

;; -------------------------------------------------------------------------
;; Move instructions  SYMBOL_REF is 50, MEM is 48, REG is 42
;; -------------------------------------------------------------------------

;; SImode

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
 	(match_operand:SI 1 "nonimmediate_operand" ""))]
  ""
  {
    printf("In movsi op0 %d op1 %d, SYMBOL_REF is %d, MEM is %d, REG is %d\n",
      GET_CODE(operands[0]), GET_CODE(operands[1]), SYMBOL_REF, MEM, REG);
    printf("op0 \n");
    print_rtl(stdout, operands[0]);
    printf("\n op1 \n");
    print_rtl(stdout, operands[1]);
    printf("\n");
    /* If this is a store,  or direct memory load force the value into a register. */
    if (! (lra_in_progress || reload_completed)) 
      {
        printf("RELOAD reload_completed\n");
        if (MEM_P (operands[1])) {
          printf("OP 1-0:\n");
          print_rtl(stdout, XEXP (operands[1], 0));
          printf("\n is MEM_P %d\n", MEM_P(XEXP (operands[1], 0)));
        }
        if (MEM_P (operands[0]))
          {
            operands[1] = force_reg (HImode, operands[1]);
            if (MEM_P (XEXP (operands[0], 0)))
              operands[0] = gen_rtx_MEM (HImode, force_reg (SImode, XEXP (operands[0], 0)));
          }
          /* A load from memory */
        else if (MEM_P (operands[1]) && MEM_P (XEXP (operands[1], 0)))
          {
            printf("Direc memory load, reloading to register\n");
            operands[1] = gen_rtx_MEM (HImode, force_reg (SImode, XEXP (operands[1], 0)));
          }
      }
    printf("movsi done\n");
})

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
 	(match_operand:HI 1 "nonimmediate_operand" ""))]
  ""
  {
    static int i = 0;
    printf("In movhi op0 %d op1 %d, SYMBOL_REF is %d, MEM is %d, REG is %d\n",
      GET_CODE(operands[0]), GET_CODE(operands[1]), SYMBOL_REF, MEM, REG);
    printf("op0 \n");
    print_rtl(stdout, operands[0]);
    printf("\n op1 \n");
    print_rtl(stdout, operands[1]);
    printf("\n");
    /* If this is a store,  or direct memory load force the value into a register. */
    if (! (lra_in_progress || reload_completed)) 
      {
        printf("RELOAD reload_completed\n");
        if (MEM_P (operands[0]))
          {
            operands[1] = force_reg (HImode, operands[1]);
            if (MEM_P (XEXP (operands[0], 0)))
              operands[0] = gen_rtx_MEM (HImode, force_reg (SImode, XEXP (operands[0], 0)));
          }
          /* A load from memory */
        else if (MEM_P (operands[1]) && MEM_P (XEXP (operands[1], 0)))
          {
            printf("Direc memory load, reloading to register\n");
            operands[1] = gen_rtx_MEM (HImode, force_reg (SImode, XEXP (operands[1], 0)));
          }
      }
    printf("movsi done\n");
})

(define_insn "*movsi"
  [(set (match_operand:SI 0 "register_operand" "=r, r, r, r")
	(match_operand:SI 1 "general_operand"  "a,  i, A, r"))]
  ""
  { 
    printf("SI Alternative is %d\n", which_alternative);
    printf("op0 \n");
    print_rtl(stdout, operands[0]);
    printf("\n op1 \n");
    print_rtl(stdout, operands[1]);
    printf("\n");
    switch (which_alternative) {
    case 0:
      if (MEM_ADDR_SPACE (operands[1]) == ADDR_SPACE_XMEM)
      {
        return "ldx i0, %0";
      } else {
        return "ldy i0, %0";
      }
    case 1:
      return "ldc %1, %0";
    case 2:
      if (MEM_ADDR_SPACE (operands[1]) == ADDR_SPACE_XMEM)
      {
        return "ldcx %1, %0";
      } else {
        return "ldcy i1, %0";
      }
    case 3:
      return "mvx %1, %0";
    default:
      gcc_unreachable ();
    }
  })

(define_insn "*movhi"
  [(set (match_operand:HI 0 "register_operand" "=r, r, r, r")
	(match_operand:HI 1 "general_operand"  "a,  i, m, r"))]
  ""
  { 
    printf("HI Alternative is %d\n", which_alternative);
    switch (which_alternative) {
    case 0:
      if (MEM_ADDR_SPACE (operands[1]) == ADDR_SPACE_XMEM)
      {
        return "ldx i0, %0";
      } else {
        return "ldy i0, %0";
      }
    case 1:
      return "ldc %1, %0";
    case 2:
      if (MEM_ADDR_SPACE (operands[1]) == ADDR_SPACE_XMEM)
      {
        return "ldcx %1, %0";
      } else {
        return "ldcy i1, %0";
      }
    case 3:
      return "mvx %1, %0";
    default:
      gcc_unreachable ();
    }
  }) 
  
  
  
(define_insn "movsi_x"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  { 
        printf("In movsi_x\n");
        return "ldx_x %1, %0";
  })

(define_insn "movsi_y"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  { 
        printf("In movsi_y\n");
        return "ldy_y %1, %0";
  })

;; -------------------------------------------------------------------------
;; Jump/Call instructions
;; -------------------------------------------------------------------------

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "nonimmediate_operand" "p"))]
  ""
  "jmp %0")

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jmp %l0%#")

(define_expand "call"
  [(call (match_operand:HI 0 "memory_operand" "")
                (match_operand 1 "general_operand" ""))]
  ""
{
  printf("In call\n");
    printf("op0 \n");
    print_rtl(stdout, operands[0]);
    printf("\n op1 \n");
    print_rtl(stdout, operands[1]);
    printf("\n");
  gcc_assert (MEM_P (operands[0]));
})

(define_insn "*call"
  [(call (mem:HI (match_operand:HI
                  0 "nonmemory_operand" "i,r"))
         (match_operand 1 "" ""))]
  ""
  "@
   jsra\\t%0
   jsr\\t%0"
)

(define_expand "call_value"
  [(set (match_operand 0 "" "")
                (call (match_operand 1 "memory_operand" "")
                 (match_operand 2 "" "")))]
  ""
{
  printf("In call_value\n");
    printf("op0 \n");
    print_rtl(stdout, operands[0]);
    printf("\n op1 \n");
    print_rtl(stdout, operands[1]);
    printf("\n");
  gcc_assert (MEM_P (operands[1]));
})

(define_insn "*call_value"
  [(set (match_operand 0 "register_operand" "=r")
        (call (mem (match_operand
                       1 "immediate_operand" "i"))
              (match_operand 2 "" "")))]
  ""
  "call\\t%1"
  )


;; -------------------------------------------------------------------------
;; nop instruction (total NOP is LDC to NOP)
;; -------------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;; -------------------------------------------------------------------------
;; Epilogue
;; -------------------------------------------------------------------------
 
(define_insn "epilogue"
  [(return)]
  ""
  "jr")
  
