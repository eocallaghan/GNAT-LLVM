/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             A F F I N I T Y                              *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *            Copyright (C) 2005-2009, Free Software Foundation, Inc.       *
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

/* VxWorks SMP CPU affinity */

#include "taskLib.h"
#include "cpuset.h"

extern int __gnat_set_affinity (int tid, unsigned cpu);

int
 __gnat_set_affinity (int tid, unsigned cpu)
{
  cpuset_t cpuset;

  CPUSET_ZERO(cpuset);
  CPUSET_SET(cpuset, cpu);
  return taskCpuAffinitySet (tid, cpu);
}
