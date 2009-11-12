/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                      C O N T E X T _ S W I T C H                         *
 *                                                                          *
 *                              Assembly File                               *
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

/* procedure context_switch.
   This routine performs a context switch between the two tasks that are
   specified in running_thread and first_thread. */

/*
   The algorithm for context switch uses simple save and restore
   instructions (without using traps).
   Interrupts are always disabled when entering this procedure. Hence, we
   do not need to disable traps within this function in order to execute
   atomically.

   Save the context:
   1. Store the in and local registers of the current register window on
      the stack.
   2. Save global, output, PSR, Y, and WIM registers in the thread
      descriptor.
   3. Check condition on WIM/CWP. If all used windows have been saved, then
      go to step 5, else continue.
   4. Execute a restore and save the current window to the stack. Then go
      back to step 3.
   5. Save the number of restores executed in the thread descriptor.

   Restore the context:
   1. Update new running task.
   2. Restore the stack pointer.
   3. Retrieve the number of register windows to be restored.
   4. Restore the in and local registers with the data from the stack.
   5. Check whether all the register windows that were used by the task are
      restored. If not, execute a restore and repeat step 4.
   6. Retrieve PSR, Y, global, and output registers from the thread
      descriptor. The WIM register was restored in step 3.
   7. Resume execution by simply returning from leaf function. It jumps to
      the next instruction to execute in the currently running thread, which
      corresponds to the calling address (o7) plus 8 (skip over the call
      instruction and the instruction in the delay slot of the call).
  */

	.text
	.align 4

	.global context_switch
context_switch:
save_running_task_status:
	/* No traps can happen saving the task status */

	/* Save the current window on the stack */

	std	%l0, [%sp + L0_OFFSET]
	std	%l2, [%sp + L2_OFFSET]
	std	%l4, [%sp + L4_OFFSET]
	std	%l6, [%sp + L6_OFFSET]

	std	%i0, [%sp + I0_OFFSET]
	std	%i2, [%sp + I2_OFFSET]
	std	%i4, [%sp + I4_OFFSET]
	std	%i6, [%sp + I6_OFFSET]

	/* Get the address of the task descriptor of the currently running
	   task for saving its state. */

	sethi	%hi(running_thread), %l0
	ld	[%l0 + %lo(running_thread)], %l1 ! get pointer to the running
						 ! thread

	/* Save global and output registers in the task descriptor area. */

	! skip g0
	st	%g1, [%l1 + G1_OFFSET]		! save the global registers
	std	%g2, [%l1 + G2_OFFSET]
	std	%g4, [%l1 + G4_OFFSET]
	std	%g6, [%l1 + G6_OFFSET]

	std	%o0, [%l1 + O0_OFFSET]		! save the output registers
	std	%o2, [%l1 + O2_OFFSET]
	std	%o4, [%l1 + O4_OFFSET]
	std	%o6, [%l1 + O6_OFFSET]		! o6 contains the SP and o7
						! the return address

	/* Save PSR, WIM, and Y registers */

	rd	%psr, %g6
	st	%g6, [%l1 + PSR_OFFSET]		! save status register

	rd	%y, %g2
	st	%g2, [%l1 + Y_OFFSET]		! save Y register

	rd	%wim, %g2
	st	%g2, [%l1 + WIM_OFFSET]		! save WIM register

	/* Flush all windows with valid contents (the current one has already
	   been flushed). Do not flush windows that have not been used. */

	/* g6 contains the PSR and g2 the WIM */

	mov	1, %g3
	sll	%g3, %g6, %g3			! g3 = WIM for CW invalid.
						! we do not need to apply any
						! mask to the psr for extracting
						! the CWP because the shift only
						! takes the 5 lsb (exactly those
						! containing the CWP)

	mov	%l1, %g1			! make a copy of the pointer to
						! the thread descriptor so that
						! we can later store the number
						! of windows to be restored

	mov	-1, %g4				! g4 = counter containing the
						! number of windows flushed

save_frame_loop:
	sll	%g3, 1, %g5			! rotate the CW mask left 1
	srl	%g3, NUMBER_OF_REGISTER_WINDOWS - 1, %g3
	or	%g3, %g5, %g3			! g3 = CW mask if we do one
						! restore

       /* If we have already arrived to the invalid window then stop.
	  Otherwise, continue flushing windows. */

	andcc	%g3,%g2,%g0			! any windows to flush?
	bne	done_flushing			! no, then continue
	 add	 %g4, 1, %g4			! using the delay slot for
						! increasing the number of
						! windows flushed

	restore					! back one window

	/* Save the current window on the stack */

	std	%l0, [%sp + L0_OFFSET]
	std	%l2, [%sp + L2_OFFSET]
	std	%l4, [%sp + L4_OFFSET]
	std	%l6, [%sp + L6_OFFSET]

	! i6 and i7 are saved in the next delay slot
	std	%i0, [%sp + I0_OFFSET]
	std	%i2, [%sp + I2_OFFSET]
	std	%i4, [%sp + I4_OFFSET]

	ba	save_frame_loop
	 std	 %i6, [%sp + I6_OFFSET]		! using the delay slot

done_flushing:
	/* We store the number of extra windows (apart from the one that was
	   being used by the task) that have been saved on the stack. */

	st	%g4, [%g1 + NWIN_OFFSET]

update_running_task:
	/* Update the value of the new running thread */

	sethi	%hi(first_thread), %g3
	sethi	%hi(running_thread), %g4
	ld	[%g3 + %lo(first_thread)], %g1
	st	%g1, [%g4 + %lo(running_thread)] ! set the new running_thread

	/* The status of the registers when entering here is:
	     g1 = task to restore
	     g6 = PSR
	     g2 = WIM
	 */

	.global restore_running_task_status
restore_running_task_status:
	/* g1 contains the address of the task descriptor of the currently
	   running tasks for restoring its state. Note that the CWP may be
	   different from the one that was saved (and that we will restore)
	   so that all the data manipulation executed from here until the end
	   must be done using global registers. */

	ld	[%g1 + PSR_OFFSET], %g4		! g4 = saved PSR
	set	PSR_EF_MASK, %g7
	andn	%g4, %g7, %g4			! g4 = saved PSR with FP disabled

	wr	%g4, %psr			! restore saved PSR but disabling
						! FP since we do not know the state
						! of the FP unit. The FP trap will
						! do what is necessary when needed
						! note that PIL is always equal to
						! 15 within the context switch
						! routine, so that interrupts are
						! disabled

	ld	[%g1 + NWIN_OFFSET], %g2	! retrieve the number of
						! windows to be restored

	ld	[%g1 + WIM_OFFSET], %g3		! retrieve the WIM register
	wr	%g3, %wim

	ld	[%g1 + O6_OFFSET], %sp		! retrieve the saved SP

restore_frame_loop:
	/* Restore the current window from the stack */

	ldd	[%sp + L0_OFFSET], %l0
	ldd	[%sp + L2_OFFSET], %l2
	ldd	[%sp + L4_OFFSET], %l4
	ldd	[%sp + L6_OFFSET], %l6

	ldd	[%sp + I0_OFFSET], %i0
	ldd	[%sp + I2_OFFSET], %i2
	ldd	[%sp + I4_OFFSET], %i4
	ldd	[%sp + I6_OFFSET], %i6

	cmp	%g2, 0
	be	done_restoring
	 add	 %g2, -1, %g2

	ba	restore_frame_loop
	 restore				! use the delay slot

done_restoring:
	/* Come back to the right window by restoring the original PSR (still
	   saved in g4). */

	wr	%g4, %psr

	ld	[%g1 + Y_OFFSET], %g2
	wr	%g2, %y 			! restore the Y register

	! skip g0; g1 will be restored later
	ldd	[%g1 + G2_OFFSET], %g2		! restore the global registers
	ldd	[%g1 + G4_OFFSET], %g4		! do g1 later to avoid
	ldd	[%g1 + G6_OFFSET], %g6		! overwritting the pointer.

	ldd	[%g1 + O0_OFFSET], %o0		! restore the output registers
	ldd	[%g1 + O2_OFFSET], %o2
	ldd	[%g1 + O4_OFFSET], %o4
	ldd	[%g1 + O6_OFFSET], %o6

	retl					! return from leaf function
	 ld	 [%g1 + G1_OFFSET], %g1		! restore g1 at the end
