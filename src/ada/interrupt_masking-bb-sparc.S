/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                    I N T E R R U P T _ M A S K I N G                     *
 *                                                                          *
 *                             Assembly File                                *
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

/* This file is a SPARC specific implementation */

/* Interrupt enabling/disabling primitives.
   These routines are responsible for changing the value of the PIL field
   in the PSR for enabling and disabling interrupts (asynchronous traps). */

#include "sparc.h"

	.text
	.align 4

/* procedure disable_interrupts */

	.global disable_interrupts
disable_interrupts:
	mov	0xF, %o0			! set maximum PIL
	ta	0x11				! call the trap handler in
						! charge of changing PIL

	retl					! return from leaf subroutine
	 nop

/* procedure enable_interrupts (interrupt_level).
   The first and only argument (passed through o0) determines the interrupt
   level above which interrupts will be acknowledged. */

	.global enable_interrupts
enable_interrupts:
	ta	0x11				! call the trap handler in
						! charge of changing PIL

	retl					! return from leaf subroutine
	 nop
