;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

;; SImode

(define_insn "movsi"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "mov %0, %1")

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
  [(call (match_operand:QI 0 "memory_operand" "")
                (match_operand 1 "general_operand" ""))]
  ""
{
  gcc_assert (MEM_P (operands[0]));
})

(define_insn "*call"
  [(call (mem:QI (match_operand:SI
                  0 "nonmemory_operand" "i,r"))
         (match_operand 1 "" ""))]
  ""
  "@
   jsra\\t%0
   jsr\\t%0"
)

(define_expand "call_value"
  [(set (match_operand 0 "" "")
                (call (match_operand:QI 1 "memory_operand" "")
                 (match_operand 2 "" "")))]
  ""
{
  gcc_assert (MEM_P (operands[1]));
})

(define_insn "*call_value"
  [(set (match_operand 0 "register_operand" "=r")
        (call (mem:QI (match_operand:SI
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
