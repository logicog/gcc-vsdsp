;; Constraint definitions for m68k
;; Copyright (C) 2007-2022 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_register_constraint "a" "ADDR_REGS"
  "Address register.")

(define_register_constraint "d" "DATA_REGS"
  "Data (8/16/40 bit) register.")

(define_register_constraint "b" "ALU_REGS"
  "Data (16 bit) register.")

(define_register_constraint "e" "EXTENSION_REGS"
  "Data (8 bit) register.")

(define_register_constraint "A" "ACC_REGS"
  "P0/P1 accumulator registers.")

(define_constraint "I"
  "Integer constant 1, for immediate shift counts."
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_memory_constraint "x"
  "A register-indirect address in X-Memory."
  (and (match_code "mem")
       (match_test "(vsdsp_is_xmem_p(op))")
       (match_test "(GET_CODE (XEXP (op, 0)) == REG)")))

(define_memory_constraint "y"
  "A register-indirect address in Y-Memory."
  (and (match_code "mem")
       (match_test "(vsdsp_is_ymem_p(op))")
       (match_test "(GET_CODE (XEXP (op, 0)) == REG)")))

(define_memory_constraint "z"
  "A register-indirect address in I-Memory."
  (and (match_code "mem")
       (match_test "(vsdsp_is_imem_p(op))")
       (match_test "(GET_CODE (XEXP (op, 0)) == REG)")))

(define_memory_constraint "u"
  "A post-increment address in X-Memory."
  (and (match_code "mem")
       (match_test "(vsdsp_is_xmem_p(op))")
       (match_test "(GET_CODE (XEXP (op, 0)) == POST_INC)")
       (match_test "(GET_CODE (XEXP (XEXP (op, 0), 0)) == REG)")))

(define_memory_constraint "v"
  "A post-increment address in Y-Memory."
  (and (match_code "mem")
       (match_test "(vsdsp_is_ymem_p(op))")
       (match_test "(GET_CODE (XEXP (op, 0)) == POST_INC)")
       (match_test "(GET_CODE (XEXP (XEXP (op, 0), 0)) == REG)")))

(define_memory_constraint "w"
  "A post-increment address in Z-Memory."
  (and (match_code "mem")
       (match_test "(vsdsp_is_imem_p(op))")
       (match_test "(GET_CODE (XEXP (op, 0)) == POST_INC)")
       (match_test "(GET_CODE (XEXP (XEXP (op, 0), 0)) == REG)")))
