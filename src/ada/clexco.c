/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                 C L E A R _ E X C E P T I O N _ C O U N T                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                     Copyright (C) 2004-2008, AdaCore                     *
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
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). *
 *                                                                          *
 ****************************************************************************/

/* Clear vThreads exception count field for current task */
/* This is a target-specific file for run-times that use the cert version of
Ada.Exceptions */

/* This operation is implicit in the vThreads version of longjmp, but not in
the gcc builtin_longjmp. This operation is performed in init.c for regular
VxWorks / vThreads run-time libraries. The routine is also called in
ravenscar-cert on VxWorks Cert 6, but does nothing */

#ifdef VTHREADS
#include <taskLib.h>
#include <private/vThreadsP.h>
#endif
void
__gnat_clear_exception_count (void)
{
#ifdef VTHREADS
  WIND_TCB *currentTask = (WIND_TCB *) taskIdSelf();
  currentTask->vThreads.excCnt = 0;
#endif
}
