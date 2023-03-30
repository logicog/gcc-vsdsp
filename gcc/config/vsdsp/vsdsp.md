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
;; Special formats used for outputting VSDSP instructions:
;;
;;   %#  --  output a nop if there is nothing to put in the delay slot


(include "constraints.md")

(define_constants
  [(REG_A1	 1)
   (REG_B1	 4)
   (REG_C1	 7)
   (REG_D1	10)
   (REG_SP      18)
   (REG_LR0     20)
   (REG_MR0     22)
   (REG_LC	23)
   (REG_LS	24)
   (REG_LE	25)
   ])

(define_c_enum "unspec" [
 UNSPEC_DLS            ; Used for DLS (Do Loop Start)
])

;;---------------------------------------------------------------------------
;; VSDSP pipeline description (3-stages)
;;---------------------------------------------------------------------------

(define_attr "type" "unknown,branch,call,jump,icall,nop,arith"
  (const_string "unknown"))

/* One delay slot for branches/calls */
(define_delay (eq_attr "type" "branch,call,jump")
              [(eq_attr "type" "!branch,call,jump,icall") (nil) (nil)])

/* One delay slot for irq-calls, which is cancelled: TODO: use it!!! */
(define_delay (eq_attr "type" "icall")
              [(eq_attr "type" "!branch,call,jump,icall")
               (eq_attr "type" "!branch,call,jump,icall") (nil)])

(define_automaton   "vsdsp_pipe")
(define_cpu_unit    "flags,alu,fetch" "vsdsp_pipe")

(define_insn_reservation "vsdsp-int" 3
  (eq_attr "type" "nop")
    "fetch, nothing,nothing")

(define_insn_reservation "vsdsp-cof" 3
  (eq_attr "type" "branch,jump,call,nop")
    "flags,flags,nothing")

(define_insn_reservation "vsdsp-arith" 3
  (eq_attr "type" "arith")
    "alu,alu,flags")

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

;; SImode

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
 	(match_operand:SI 1 "general_operand" ""))]
  ""
  {
    printf("\n ----> In movSI op0 %d op1 %d, SYMBOL_REF is %d, MEM is %d, REG is %d\n",
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
        /* A load from memory */
        if (MEM_P (operands[1]) && (GET_CODE((XEXP (operands[1], 0))) == SYMBOL_REF)) {
	    /* Make sure to copy over the memory attributes */
	    printf("SI IN MEM-LOAD\n");
	    operands[1] = replace_equiv_address(operands[1], 
			    force_reg (Pmode, XEXP (operands[1], 0)));
	    printf("\n op1 now (after indirection) \n");
	    print_rtl(stdout, operands[1]);
	}
	else if (MEM_P (operands[0]) && (GET_CODE((XEXP (operands[0], 0))) == SYMBOL_REF)) {
	    printf("SI IN MEM-STORE\n");
	    printf("\n op0 now \n");
	    operands[0] = replace_equiv_address(operands[0], 
			    force_reg (Pmode, XEXP (operands[0], 0)));
	    printf("\n op0 now \n");
	    print_rtl(stdout, operands[0]);
	    printf("\n TESTING OP1 \n");
	    if (CONST_INT_P(operands[1])) {
	      printf("TREATING CONST\n");
	      operands[1] = force_reg (SImode, operands[1]);
	      printf("\n op0 now \n");
	      print_rtl(stdout, operands[1]);
	    }
	    printf("\n :::: op0 now \n");
	    print_rtl(stdout, operands[0]);
	    printf("\n :::: op1 now \n");
	    print_rtl(stdout, operands[1]);
	}
      }
    printf("\n ====> movSI done\n");
})

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
 	(match_operand:HI 1 "general_operand" ""))]
  ""
  {
    printf("\n ----> In movHI op0 %d op1 %d, SYMBOL_REF is %d, MEM is %d, REG is %d\n",
      GET_CODE(operands[0]), GET_CODE(operands[1]), SYMBOL_REF, MEM, REG);
    printf("op0 \n");
    print_rtl(stdout, operands[0]);
    printf("\n op1 \n");
    print_rtl(stdout, operands[1]);
    printf("\n");
    /* If this is a store,  or direct memory load force the value into a register. */
    if (! (lra_in_progress || reload_completed)) 
      {
        printf("RELOAD NOT COMPLETED\n");
        /* A load from memory */
	if (MEM_P (operands[1]) && (GET_CODE((XEXP (operands[1], 0))) == SYMBOL_REF)) {
	    printf("HI IN MEM-LOAD\n");
	    operands[1] = replace_equiv_address(operands[1], 
			    force_reg (Pmode, XEXP (operands[1], 0)));
	    printf("\n op1 now \n");
	    print_rtl(stdout, operands[1]);
	}
	else if (MEM_P (operands[0]) && (GET_CODE((XEXP (operands[0], 0))) == SYMBOL_REF)) {
	    printf("HI IN MEM-STORE\n");
	    printf("\n op0 now \n");
	    operands[0] = replace_equiv_address(operands[0], 
			    force_reg (Pmode, XEXP (operands[0], 0)));
	    printf("\n op0 now \n");
	    print_rtl(stdout, operands[0]);
	}
      }
    printf("\n ====> movHI done\n");
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
  operands[2] = gen_highpart (HImode, operands[1]);
  operands[3] = gen_lowpart (HImode, operands[1]);
  printf("\n OP2 now:");
  print_rtl(stdout, operands[2]);
  printf("\n OP3 now:");
  print_rtl(stdout, operands[3]);
  printf("\n");
  printf("Doing SPLIT ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
})

(define_split
  [(set (match_operand:SI 0 "memory_operand" "")
	    (match_operand:SI 1 "general_operand" ""))]
  ""
  [(set (match_dup 2)
	(subreg:HI (match_dup 1) 0) )
   (set (match_dup 3)
	(subreg:HI (match_dup 1) 2) )]
{
  operands[2] = gen_highpart (HImode, operands[0]);
  operands[3] = gen_lowpart (HImode, operands[0]);
  printf("\n ++++ OP2 now:");
  print_rtl(stdout, operands[2]);
  printf("\n ++++ OP3 now:");
  print_rtl(stdout, operands[3]);
  printf("\n");
  printf("Doing SPLIT ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
})

/* Split indirect address read with immediate offset into separate post-inc  */

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
  printf("\nDOING SPLIT immediate offsset read +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  operands[2] = XEXP (XEXP (operands[1], 0), 1);
  operands[3] = XEXP (XEXP (operands[1], 0), 0);
  addr = gen_rtx_MEM(HImode, operands[3]);
  printf("\n ++++ OP1 was:");
  print_rtl(stdout, operands[1]);
  MEM_COPY_ATTRIBUTES(addr, operands[1]);
  operands[4] = addr;
  printf("\n ++++ OP4 now:");
  print_rtl(stdout, operands[4]);
  
})

/* Split indirect address write with immediate offset into separate post-inc  */

(define_split
  [(set (match_operand:HI 0 "memory_operand" "")
	    (match_operand:HI 1 "register_operand" ""))]
  "(GET_CODE (XEXP (operands[0], 0)) == PLUS)
    && (GET_CODE ( XEXP (XEXP (operands[0], 0), 0)) == REG)
    && (GET_CODE ( XEXP (XEXP (operands[0], 0), 1)) == CONST_INT)"
  
  [(set (match_dup 3) (plus:HI (match_dup 3) (match_dup 2)) )
   (set (match_dup 4) (match_dup 1)  )]
{
  rtx addr;
  printf("\nDOING SPLIT immediate offsset write +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  printf("\n operand 0: ");
  print_rtl(stdout, operands[0]);
  operands[2] = XEXP (XEXP (operands[0], 0), 1);
  operands[3] = XEXP (XEXP (operands[0], 0), 0);
  printf("\n new operand 2: ");
  print_rtl(stdout, operands[2]);
  printf("\n new operand 3: ");
  print_rtl(stdout, operands[3]);
  addr = gen_rtx_MEM(HImode, operands[3]);
  MEM_COPY_ATTRIBUTES(addr, operands[0]);
  operands[4] = addr;
  printf("\n new operand 4: ");
  print_rtl(stdout, operands[3]);
})

(define_insn "*movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r, r, r, A, zw,  r")
	(match_operand:SI 1 "general_operand"      "i,  r, A, r,  r, zw"))]
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
      operands[2] = gen_rtx_REG (HImode, REGNO (operands[0]) + 1);
      operands[3] = gen_rtx_CONST_INT(HImode, XINT(operands[1], 0) & 0xffff);
      operands[4] = gen_rtx_CONST_INT(HImode, XINT(operands[1], 0) >> 16);
      return "ldc\t%3, %2\n\tldc\t%4, %0 # SI0";
    case 1:
      return "mv\t%R1, %R0 # SI1";
    case 2:
      return "add\tnull, p, %R0 # SI1";
    case 3:
      operands[1] = gen_rtx_REG(HImode, REGNO (operands[0]) + 1);
      return "resp\t %0, %1 # SI1";
    case 4:
      return "sdi\t%1, %0 # SI4";
    case 5:
      return "ldi\t%1, %0 # SI5";
    default:
      gcc_unreachable ();
    }
  })

(define_insn "*movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r, r,r,xu,yv,r,A")
	(match_operand:HI 1 "general_operand"      "i,xu,yv,r, r, r,A,r"))]
  ""
  { 
    printf(">>>>>>>>>>>>>>> HI Alternative is %d\n", which_alternative);
    printf("op0 \n");
    print_rtl(stdout, operands[0]);
    printf("\n op1 \n");
    print_rtl(stdout, operands[1]);
    printf("\n");
    printf("HI Alternative is %d\n", which_alternative);
    switch (which_alternative) {
    case 0:
      return "ldc\t%1, %0 # HI0";
    case 1:
        return "ldx\t%1, %0 # HI1";
    case 2:
        return "ldy\t%1, %0 # HI2";
    case 3:
      return "mv\t%1, %0 # HI3";
    case 4:
        return "stx\t%1, %0 # HI4";
    case 5:
        return "sty\t%1, %0 # HI5";
    case 6:
	return "mv\t , %0 # HI6";
    case 7:
	return "mv\t , %0 # HI7";
    default:
      gcc_unreachable ();
    }
  }) 

;; Push a register onto the stack
(define_insn "movsi_push"
  [(set:SI (mem:SI (pre_inc:SI (reg:SI REG_SP)))
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "push   $sp, %0")

;; Pop a register from the stack
(define_insn "movsi_pop"
  [(set:SI (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (post_dec:SI (reg:SI REG_SP))))]
  ""
  "pop    $sp, %0")

;; Push a register onto the stack
(define_insn "movhi_push"
  [(set:HI (mem:HI (pre_inc:HI (reg:HI REG_SP)))
	(match_operand:HI 0 "register_operand" "r"))]
  ""
  "push   $sp, %0")

;; Pop a register from the stack
(define_insn "movhi_pop"
  [(set:HI (match_operand:HI 0 "register_operand" "=r")
	(mem:HI (post_dec:HI (reg:HI REG_SP))))]
  ""
  "pop    $sp, %0")

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

(define_insn "subhi3"
  [(set (match_operand:HI 0 "register_operand" "=b, a")
	  (minus:HI
	   (match_operand:HI 1 "register_operand" "b, 0")
	   (match_operand:HI 2 "general_operand" "b, i")))]
  ""
  "@
  sub %1, %2, %0
  ldx %0 - %2, null")

(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=A")
	(mult:SI (sign_extend:SI
		  (match_operand:HI 1 "register_operand" "b"))
		 (sign_extend:SI
		  (match_operand:HI 2 "general_operand" "b"))))]
  ""
  {
    return "mulss %1, %2 # mulhisi3";
  })

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=A")
	(mult:HI (match_operand:HI 1 "register_operand" "r")
		 (match_operand:HI 2 "general_operand" "r")))]
  ""
  {
    return "mulss %1, %2 #mulhi3";
  })

; Arithmetic shifts left/right
; VSDSP only support arithmetic right shift by one bit
; -> convert asr #n to ashl #-n for n > 1

(define_insn_and_split "ashrhi3"
  [(set (match_operand:HI 0 "register_operand"                "=b")
        (ashiftrt:HI (match_operand:HI 1 "register_operand"   "b")
                     (match_operand:HI 2 "nonimmediate_operand"    "b")))]
  ""
  "#"
  ""
  [(parallel [	(set (match_dup 2) (neg:HI (match_dup 2)))
                (clobber (reg:CC REG_MR0))])
   (parallel [  (set (match_dup 0)
                     (ashift:HI (match_dup 1) (match_dup 2)))
		(clobber (reg:CC REG_MR0))])
  ])

(define_peephole2
  [(set (match_operand:HI 2 "nonimmediate_operand" "") (const_int 1))
   (parallel [	(set (match_dup 2) (neg:HI (match_dup 2)))
                (clobber (reg:CC REG_MR0))])
   (parallel [  (set (match_operand:HI 0 "register_operand" "")
                     (ashift:HI (match_operand:HI 1 "register_operand" "") (match_dup 2)))
		(clobber (reg:CC REG_MR0))])]
  ""
  [(set (match_dup 0)
	(ashiftrt:HI (match_dup 1) (const_int 1)))]
)

(define_insn "ashlhi3"
  [(set (match_operand:HI 0 "register_operand" "=b")
	(ashift:HI 
	   (match_operand:HI 1 "register_operand" "b")
	   (match_operand:HI 2 "general_operand" "b")))
    (clobber (reg:CC REG_MR0))]
  ""
  {
    return "ashl\t%1, %2, %0";
  }
  [(set_attr "type" "arith")])

; TODO: Is this necessry? Compiler seems anyway to produce add r, r, r
(define_peephole2
  [(set (match_operand:HI 2 "nonimmediate_operand" "") (const_int 1))
   (parallel [  (set (match_operand:HI 0 "register_operand" "")
                     (ashift:HI (match_operand:HI 1 "register_operand" "") (match_dup 2)))
		(clobber (reg:CC REG_MR0))])]
  ""
  [(set (match_dup 0)
	(ashift:HI (match_dup 1) (const_int 1)))]
)

(define_insn "*ashrhi3i1"
  [(set (match_operand:HI 0 "register_operand" "=b")
	(ashiftrt:HI 
	   (match_operand:HI 1 "register_operand" "b")
	   (match_operand:HI 2 "immediate_operand" "I")))]
  ""
  {
    return "asr %1, %0";
  }
  [(set_attr "type" "arith")])

; TODO: Is this necessry? Compiler seems anyway to produce add r, r, r
(define_insn "*ashlhi3i1"
  [(set (match_operand:HI 0 "register_operand" "=b")
	(ashift:HI 
	   (match_operand:HI 1 "register_operand" "b")
	   (match_operand:HI 2 "immediate_operand" "I")))]
  ""
  {
    return "lsl %1, %0";
  }
  [(set_attr "type" "arith")])

(define_insn "neghi2"
  [(set (match_operand:HI 0 "register_operand" "=b")
        (neg:HI (match_operand:HI 1 "general_operand" "b")))
    (clobber (reg:CC REG_MR0))
  ]
  ""
  {
    return "sub null, %0, %1";
  }
  [(set_attr "type" "arith")])

(define_insn "lshrhi3i1"
  [(set (match_operand:HI 0 "register_operand" "=b")
	(lshiftrt:HI 
	   (match_operand:HI 1 "register_operand" "b")
	   (match_operand:HI 2 "immediate_operand" "I")))]
  ""
  {
    printf("Outputting lshrhi3i1 ************************** \n");
    return "lsr %1, %0";
  }
  [(set_attr "type" "arith")])

(define_expand "lshrhi3"
  [(set (match_operand:HI 0 "register_operand" "=b")
	(lshiftrt:HI 
	   (match_operand:HI 1 "register_operand" "b")
	   (match_operand:HI 2 "general_operand" "b")))
    (clobber (reg:CC REG_MR0))]
  ""
  {
    rtx test;
    rtx_code_label *j_label;
    int shift;
    printf("Outputting lshrhi3 ************************** \n");
    if (GET_CODE(operands[2]) != CONST_INT)
      {
	j_label = gen_label_rtx ();
	test = gen_rtx_GT (VOIDmode, operands[2], const0_rtx);
        emit_jump_insn (gen_cbranchhi4 (test, operands[2], const0_rtx, j_label));
	emit_insn (gen_lshrhi3i1 (operands[1], operands[1], GEN_INT (1)));
        emit_label (j_label);
        LABEL_NUSES (j_label)++;
	emit_insn (gen_subhi3 (operands[2], GEN_INT (1), operands[2]));
        emit_insn (gen_ashlhi3 (operands[0], operands[1], operands[2]));
        DONE;
       }
    printf("**** is INT\n");
    shift = XINT (operands[2], 0);
    if (shift > 0) {
      emit_insn (gen_lshrhi3i1 (operands[1], operands[1], GEN_INT (1)));
      shift = 1 - shift;
    }
    emit_insn (gen_ashlhi3 (operands[0], operands[1], GEN_INT(shift)));
    DONE;
  }
  [(set_attr "type" "arith")])

;; -------------------------------------------------------------------------
;; Jump/Call instructions
;; -------------------------------------------------------------------------

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "nonimmediate_operand" "p"))]
  ""
  "jmp %0%#"
  [(set_attr "type" "jump")])

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jmp %l0%#"
  [(set_attr "type" "jump")])
  

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
  [(call (mem:HI (match_operand:HI 0 "memory_operand" "i, r"))
         (match_operand 1 "immediate_operand" ""))]
  ""
  "@
   jsra\\t%0%#
   jsr\\t%0%#"
  [(set_attr "type" "call")])

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
  "call\\t%1%#"
  [(set_attr "type" "call")]  )

; TODO: Needed for -O3 foo_mul.s, but is this not just a define_expand ?
(define_insn "sibcall_value"
  [(set (match_operand 0 "" "")
	(call (match_operand:HI 1 "memory_operand" "")
	      (match_operand:HI 2 "general_operand" "")))]
  ""
{
  return "CALL %0 %1 %2%#";
}
  [(set_attr "type" "jump")])

;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

(define_insn_and_split "cbranchhi4"
  [(set (pc)
        (if_then_else (match_operator 0 "ordered_comparison_operator"
                        [(match_operand:HI 1 "register_operand" "")
                         (match_operand:HI 2 "nonmemory_operand" "")])
         (label_ref (match_operand 3 "" ""))
         (pc)))]
   ""
   "#"
   ""
   [(set (reg:CC REG_MR0)
	 (compare:CC (match_dup 1) (match_dup 2)))
    (set (pc)
         (if_then_else (match_op_dup 0
                         [(reg:CC REG_MR0) (const_int 0)])
                       (label_ref (match_dup 3))
                       (pc)))]
   {
    printf("############ cbranchhi4\n");
    if (GET_CODE (operands[0]) == LEU || GET_CODE (operands[0]) == LTU 
        || GET_CODE (operands[0]) == GEU || GET_CODE (operands[0]) == GTU )
      {
        printf("LEU LEU LEU\n");
        operands[1] = gen_rtx_ABS(HImode, operands[1]);
        operands[2] = gen_rtx_ABS(HImode, operands[2]);
      }
    printf("op0 \n");
    print_rtl(stdout, operands[0]);
    printf("\n op1 \n");
    print_rtl(stdout, operands[1]);
    printf("\n op2 \n");
    print_rtl(stdout, operands[2]);
    printf("\n");
   })

(define_insn_and_split "cbranchsi4"
  [(set (pc)
        (if_then_else (match_operator 0 "ordered_comparison_operator"
                        [(match_operand:SI 1 "register_operand" "")
                         (match_operand:SI 2 "nonmemory_operand" "")])
         (label_ref (match_operand 3 "" ""))
         (pc)))]
   ""
   "#"
   ""
   [(set (reg:CC REG_MR0)
	 (compare:CC (match_dup 1) (match_dup 2)))
    (set (pc)
         (if_then_else (match_op_dup 0
                         [(reg:CC REG_MR0) (const_int 0)])
                       (label_ref (match_dup 3))
                       (pc)))]
   {
    printf("############ cbranchsi4\n");
    if (GET_CODE (operands[0]) == LEU || GET_CODE (operands[0]) == LTU 
        || GET_CODE (operands[0]) == GEU || GET_CODE (operands[0]) == GTU )
      printf("LEU LEU LEU\n");
    printf("op0 \n");
    print_rtl(stdout, operands[0]);
    printf("\n op1 \n");
    print_rtl(stdout, operands[1]);
    printf("\n op2 \n");
    print_rtl(stdout, operands[2]);
    printf("\n");
   })

   /* FIXME: for the u-version we need to use unsigned subtraction */
(define_code_iterator cond [ne eq lt ltu gt gtu ge geu le leu])
(define_code_attr CC [(ne "zc") (eq "zs") (lt "lt") (ltu "lt") (gt "gt")
                      (gtu "gt") (ge "ge") (geu "ge") (le "le") (leu "le")])

(define_insn "*jcc<cond:code>"
  [(set (pc)
        (if_then_else (cond (reg:CC REG_MR0)
                            (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  return "j<CC> %l0%#";
}
  [(set_attr "type" "branch")])

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------

(define_insn "*cmphi"
  [(set (reg:CC REG_MR0)
	(compare:CC
	 (match_operand:HI 0 "register_operand" "b")
	 (match_operand:HI 1 "general_operand"	"b")))]
  ""
  "sub\\t%0, %1, %0%#"
  [(set_attr "type" "arith")])

(define_insn "*cmphi_u"
  [(set (reg:CC REG_MR0)
	(compare:CC
	 (abs:HI (match_operand:HI 0 "register_operand" "b"))
	 (abs:HI (match_operand:HI 1 "general_operand"	"b"))))]
  ""
  "sub\\t%0, %1, %0%#"
  [(set_attr "type" "arith")])
  
(define_insn "*cmpsi"
  [(set (reg:CC REG_MR0)
	(compare:CC
	 (match_operand:SI 0 "register_operand" "b")
	 (match_operand:SI 1 "general_operand"	"b")))]
  ""
  "sub\\t%R0, %R1, %R0%#"
  [(set_attr "type" "arith")])

;; -------------------------------------------------------------------------
;; Looping
;; -------------------------------------------------------------------------

(define_expand "doloop_begin"
  [(match_operand 0 "" "")
   (match_operand 1 "" "")]
  ""
  {
    printf("-------- doloop_begin\n");
    printf("op0 \n");
    print_rtl(stdout, operands[0]);
    printf("\n op1 \n");
    print_rtl(stdout, operands[1]);
    printf("\n");
    if (REGNO (operands[0]) == REG_LC) {
        emit_insn (gen_dls_insn (operands[0]));
        DONE;
      }
    else
      FAIL;
  })

(define_insn "dls_insn"
  [(set (reg:HI REG_LC)
        (unspec:HI [(match_operand:HI 0 "register_operand" "r")] UNSPEC_DLS))]
  ""
  {
    return doloop_begin_output(operands);
  })

(define_expand "doloop_end"
  [(parallel [(set (pc)
                (if_then_else
                  (ne (reg:HI REG_LC) (const_int 1))
                  (label_ref (match_operand 1 "" ""))
                 (pc)))
              (set (reg:HI REG_LC) (plus:HI (reg:HI REG_LC) (const_int -1)))])]
  ""
  {
    printf("insn --------------------------++++++++++++++++++++++ doloop_end EXPANDING\n");
    if (optimize > 0)
      {
	if (GET_MODE (operands[0]) != HImode)
           FAIL;
	printf("insn --------------------------++++++++++++++++++++++ doloop_end EXPANDING being done\n");
      }
    else
      FAIL;
 })


(define_insn "*doloop_end_insn"
    [(set (pc)
        (if_then_else
          (ne (reg:HI REG_LC) (const_int 1))
          (label_ref (match_operand 0 "" ""))
          (pc)))
     (set (reg:HI REG_LC) (plus:HI (reg:HI REG_LC) (const_int -1)))]
  ""
  {
    printf("insn --------------------------++++++++++++++++++++++ doloop_end OUTPUT\n");
    return doloop_end_output();
  })

;; -------------------------------------------------------------------------
;; nop instruction (total NOP is LDC to NOP)
;; -------------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type" "nop")])

;; -------------------------------------------------------------------------
;; Epilogue
;; -------------------------------------------------------------------------
 
(define_insn "epilogue"
  [(return)]
  ""
  "jr")
