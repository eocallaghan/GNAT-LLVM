/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                         F L O A T I N G _ P O I N T                      *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2005 The European Space Agency            *
 *                     Copyright (C) 2003-2005 AdaCore                      *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNARL was developed by the GNARL team at Florida State University.       *
 * Extensive contributions were provided by Ada Core Technologies, Inc.     *
 * The  executive  was developed  by the  Real-Time  Systems  Group  at the *
 * Technical University of Madrid.                                          *
 *                                                                          *
 ****************************************************************************/

/* This file is a SPARC (V7/V8) specific implementation */

#include "sparc.h"

	.data
	.align 4

/* This variable contains the last thread that used the floating point
   unit. Hence, it points to the place where the floating point state
   must be stored. */

latest_user:	.word	0

	.text
	.align 4
	.global initialize_floating_point

/* Procedure that installs the trap handler in charge of executing floating
   point context switches. */

initialize_floating_point:
	/* First we need to obtain the address (within the trap table)
	   where the trap handler needs to be installed. */

	sethi	%hi(trap_table), %l1
	add	%l1, %lo(trap_table), %l1		! trap table address

	add	%l1, 4*4*4, %l1				! position of
							! fp_disabled in the
							! trap table

	/* Now, install the first four instructions of the trap handler in
	   the trap table. */

	/* sethi %hi(fp_context_switch), %l0 */

	sethi	%hi(0x21000000), %l2
	sethi	%hi(fp_context_switch), %l3
	srl	%l3, 10, %l3
	add	%l2, %l3, %l2
	st	%l2, [%l1 + 0*4]

	/* jmpl %l0 + %lo(fp_context_switch), %g0 */

	sethi	%hi(0x81c42000), %l2
	add	%l2, %lo(fp_context_switch), %l2
	st	%l2, [%l1 + 1*4]

	/* mov %psr, %l0 */

	set	0xA1480000, %l2
	st	%l2, [%l1 + 2*4]

	/* nop */

	set	0x01000000, %l2
	st	%l2, [%l1 + 3*4]

	/* Finally, set the currently executing thread as the last one
	   that used the floating point unit. */

	sethi	%hi(running_thread), %l0
	sethi	%hi(latest_user), %l1
	ld	[%l0 + %lo(running_thread)], %l2

	retl					! return from leaf subroutine
	 st	 %l2, [%l1 + %lo(latest_user)]

	/* Trap handler that saves/restores the floating point context in the
	   appropriate thread descriptors. */

fp_context_switch:
	/* First we need to enable the use of the floating point unit. Note
	   that l0 contains the PSR. */

	set	PSR_EF_MASK, %l3
	or	%l0, %l3, %l0
	wr	%l0, %psr			! enable FP

	/* Check whether we need to save/restore the floating point
	   state. If the currently executing thread is the one that
	   was the last using the floating point unit then nothing
	   needs to be done. */

	sethi	%hi(latest_user), %l3
	sethi	%hi(running_thread), %l5
	ld	[%l3 + %lo(latest_user)], %l4
	ld	[%l5 + %lo(running_thread)], %l6
	cmp	%l4, %l6
	be	fp_context_switch_done
	 nop

	/* Save the old floating point state in the task descriptor area */

	st	%fsr, [%l4 + FSR_OFFSET]
	std	%f0,  [%l4 + F0_F1_OFFSET]
	std	%f2,  [%l4 + F2_F3_OFFSET]
	std	%f4,  [%l4 + F4_F5_OFFSET]
	std	%f6,  [%l4 + F6_F7_OFFSET]
	std	%f8,  [%l4 + F8_F9_OFFSET]
	std	%f10, [%l4 + F10_F11_OFFSET]
	std	%f12, [%l4 + F12_F13_OFFSET]
	std	%f14, [%l4 + F14_F15_OFFSET]
	std	%f16, [%l4 + F16_F17_OFFSET]
	std	%f18, [%l4 + F18_F19_OFFSET]
	std	%f20, [%l4 + F20_F21_OFFSET]
	std	%f22, [%l4 + F22_F23_OFFSET]
	std	%f24, [%l4 + F24_F25_OFFSET]
	std	%f26, [%l4 + F26_F27_OFFSET]
	std	%f28, [%l4 + F28_F29_OFFSET]
	std	%f30, [%l4 + F30_F31_OFFSET]

	/* Restore the new floating point state from the task descriptor area */

	ldd	[%l6 + F0_F1_OFFSET],   %f0
	ldd	[%l6 + F2_F3_OFFSET],   %f2
	ldd	[%l6 + F4_F5_OFFSET],   %f4
	ldd	[%l6 + F6_F7_OFFSET],   %f6
	ldd	[%l6 + F8_F9_OFFSET],   %f8
	ldd	[%l6 + F10_F11_OFFSET], %f10
	ldd	[%l6 + F12_F13_OFFSET], %f12
	ldd	[%l6 + F14_F15_OFFSET], %f14
	ldd	[%l6 + F16_F17_OFFSET], %f16
	ldd	[%l6 + F18_F19_OFFSET], %f18
	ldd	[%l6 + F20_F21_OFFSET], %f20
	ldd	[%l6 + F22_F23_OFFSET], %f22
	ldd	[%l6 + F24_F25_OFFSET], %f24
	ldd	[%l6 + F26_F27_OFFSET], %f26
	ldd	[%l6 + F28_F29_OFFSET], %f28
	ldd	[%l6 + F30_F31_OFFSET], %f30
	ld	[%l6 + FSR_OFFSET],     %fsr

	/* Write down the thread that is currently using the floating
	   point unit. */

	st	%l6, [%l3 + %lo(latest_user)]

fp_context_switch_done:
	/* Restore the original PSR */

	wr	%l0, %psr
	nop
	nop
	nop

	jmp	%l1
	 rett	 %l2			! return from trap

/* __gnat_init_float is a routine used for reseting the floating-point
   processor to the default state needed to get correct Ada semantics
  for the target. */

	.global __gnat_init_float
__gnat_init_float:
	retl				! nothing to be done on this target
	 nop
