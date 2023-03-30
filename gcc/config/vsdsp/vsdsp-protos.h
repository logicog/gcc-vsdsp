/* Prototypes for exported functions defined in avr.cc
   
   Copyright (C) 2000-2022 Free Software Foundation, Inc.
   Contributed by Denis Chertykov (chertykov@gmail.com)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

extern enum reg_class vsdsp_regno_reg_class (int r);

extern rtx vsdsp_function_value (const_tree, const_tree);

extern void vsdsp_print_operand (FILE *, rtx, int);
extern void vsdsp_print_operand_address (FILE *, rtx);

