/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                 I V E C                                  *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                          Copyright (C) 2009, AdaCore                     *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* Needed by ravenscar-cert run-time */

/* The following include is here to meet the published VxWorks requirement
   that the __vxworks header appear before any other include.  */

#include "vxWorks.h"
#include <iv.h>
#include <signal.h>

extern void * __gnat_inum_to_ivec (int);

/* This is needed by the GNAT run time to handle Vxworks interrupts.  */
void *
__gnat_inum_to_ivec (int num)
{
  return INUM_TO_IVEC (num);
}

/* Dummy version for 653 cert and VxWorks cert (not in subset) */
extern int sigwaitinfo (const sigset_t * __set, siginfo_t * __value);

int
sigwaitinfo (const sigset_t * __set, siginfo_t * __value)
{
  return -1;
}
