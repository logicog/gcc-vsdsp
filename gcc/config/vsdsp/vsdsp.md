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
	  int as = MEM_ADDR_SPACE (XEXP (operands[1], 0));
          printf("OP 1-0, addr space is: %d\n", as);
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
	else if (MEM_P (operands[1]) && (GET_CODE((XEXP (operands[1], 0))) == SYMBOL_REF)) {
	    /* Make sure to copy over the memory attributes */
	    operands[1] = replace_equiv_address(operands[1], 
			    force_reg (Pmode, XEXP (operands[1], 0)));
	    printf("\n op1 now (after indirection) \n");
	    print_rtl(stdout, operands[1]);
	}
	else if (MEM_P (operands[0]) && (GET_CODE((XEXP (operands[0], 0))) == SYMBOL_REF)) {
	    printf("IN MEM-STORE\n");
	    operands[0] = replace_equiv_address(operands[0], 
			    force_reg (Pmode, XEXP (operands[0], 0)));
	    printf("\n op0 now \n");
	    print_rtl(stdout, operands[0]);
	}
      }
    printf("movsi done\n");
})

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
 	(match_operand:HI 1 "nonimmediate_operand" ""))]
  ""
  {
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
            printf("Direct memory load, reloading to register\n");
            operands[1] = gen_rtx_MEM (HImode, force_reg (Pmode, XEXP (operands[1], 0)));
          }
          /* A store to memory */
        else if (MEM_P (operands[0]) && MEM_P (XEXP (operands[0], 0)))
          {
            printf("Direct memory load, reloading to register\n");
            operands[1] = gen_rtx_MEM (HImode, force_reg (Pmode, XEXP (operands[0], 0)));
          }
	else if (MEM_P (operands[1]) && (GET_CODE((XEXP (operands[1], 0))) == SYMBOL_REF)) {
	    printf("IN MEM-LOAD\n");
	    operands[1] = replace_equiv_address(operands[1], 
			    force_reg (Pmode, XEXP (operands[1], 0)));
	    printf("\n op1 now \n");
	    print_rtl(stdout, operands[1]);
	}
	else if (MEM_P (operands[0]) && (GET_CODE((XEXP (operands[0], 0))) == SYMBOL_REF)) {
	    printf("IN MEM-STORE\n");
	    operands[0] = replace_equiv_address(operands[0], 
			    force_reg (Pmode, XEXP (operands[0], 0)));
	    printf("\n op0 now \n");
	    print_rtl(stdout, operands[0]);
	}
      }
    printf("movhi done\n");
})


/* Split SI memory access into 2x HI for X and Y memory */

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	    (match_operand:SI 1 "memory_operand" ""))]
  ""
  [(set (subreg:HI (match_dup 0) 0)
	(match_dup 2) )
   (set (subreg:HI (match_dup 0) 2)
	(match_dup 3) )]
{
/*
  operands[2] = gen_highpart (SImode, operands[0]);
  operands[3] = gen_lowpart (SImode, operands[0]); */
  printf("DOING SPLIT ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  print_rtl(stdout, operands[0]);
  printf("\n");
  print_rtl(stdout, operands[1]);
  printf("\n");
  /*
  operands[2] = change_address(operands[1], HImode, operands[1]);
  operands[3] = XEXP (operands[1], 0);
  */
  operands[2] = gen_highpart (HImode, operands[1]);
  operands[3] = gen_lowpart (HImode, operands[1]);
  printf("\n OP2 now:");
  print_rtl(stdout, operands[2]);
  printf("\n OP3 now:");
  print_rtl(stdout, operands[3]);
  printf("\n");
  printf("Doing SPLIT ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
})

/* Split indirect address with immediate offset into separate post-inc  */

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	    (match_operand:HI 1 "memory_operand" ""))]
  "(GET_CODE (XEXP (operands[1], 0)) == PLUS)
    && (GET_CODE ( XEXP (XEXP (operands[1], 0), 0)) == REG)
    && (GET_CODE ( XEXP (XEXP (operands[1], 0), 1)) == CONST_INT)"
  
  [(set (match_dup 3) 
	(plus:HI (match_dup 3) (match_dup 2)))
   (set (match_dup 0) (match_dup 4) )]
{
  rtx addr;
  printf("DOING SPLIT immediate offsset ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  operands[2] = XEXP (XEXP (operands[1], 0), 1);
  operands[3] = XEXP (XEXP (operands[1], 0), 0);
  addr = gen_rtx_MEM(HImode, operands[3]);
  MEM_COPY_ATTRIBUTES(addr, operands[1]);
  operands[4] = addr;
  printf("\n operand 0: ");
  print_rtl(stdout, operands[0]);
  printf("\n operand 1: ");
  print_rtl(stdout, operands[1]);
  printf("\n  new ins 2: \n");
  print_rtl(stdout, operands[2]);
  printf("\n  new ins 3: \n");
  print_rtl(stdout, operands[3]);
  printf("\n  new ins 4: \n");
  print_rtl(stdout, operands[4]);
  printf("\nDoing SPLIT immediate offsset ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
})

(define_insn "*movsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "general_operand"  "r"))]
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
      return "mv %1, %0 # SI0";
    default:
      gcc_unreachable ();
    }
  })

(define_insn "*movhi"
  [(set (match_operand:HI 0 "register_operand" "=r, r, r")
	(match_operand:HI 1 "general_operand"  "i,  m, r"))]
  ""
  { 
    printf("HI Alternative is %d\n", which_alternative);
    switch (which_alternative) {
    case 0:
      return "ldc %1, %0 # HI0";
    case 1:
      if (MEM_ADDR_SPACE (operands[1]) == ADDR_SPACE_YMEM)
      {
	printf("MEM-MOVE op0 \n");
	print_rtl(stdout, operands[0]);
	printf("\n op1 \n");
	print_rtl(stdout, operands[1]);
	printf("\n");
        return "ldy %1, %0 # HI1";
      } else {
        return "ldx %1, %0 # HI1";
      }
    case 2:
      return "mv %1, %0 # HI2";
    default:
      gcc_unreachable ();
    }
  }) 

;; -------------------------------------------------------------------------
;; Arithmetic instructions
;; -------------------------------------------------------------------------

; Additions

(define_insn "addhi3"
  [(set (match_operand:HI 0 "register_operand" "=b, a")
	  (plus:HI
	   (match_operand:HI 1 "register_operand" "b, 0")
	   (match_operand:HI 2 "general_operand" "b, i")))]
  ""
  "@
  add %1, %2, %0
  ldx %0 + %2, null")


(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=A")
	(mult:SI (sign_extend:SI
		  (match_operand:HI 1 "general_operand" "d"))
		 (sign_extend:SI
		  (match_operand:HI 2 "general_operand" "b"))))]
  ""
  {
    return "mulss %0, %1";
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
  

