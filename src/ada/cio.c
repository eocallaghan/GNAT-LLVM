/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                  C I O                                   *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2008, Free Software Foundation, Inc.         *
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

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#include <sys/stat.h>
#else
#include "config.h"
#include "system.h"
#endif

#include "adaint.h"

/* Don't use macros on GNU/Linux since they cause incompatible changes between
   glibc 2.0 and 2.1 */
#ifdef linux
#undef putchar
#undef getchar
#undef fputc
#undef stderr
#undef stdout
#endif

#ifdef VTHREADS
#undef putchar
#undef getchar
#endif

#ifdef RTX
#include <windows.h>
#include <Rtapi.h>
#endif

int
get_char (void)
{
#ifdef VMS
  return decc$getchar();
#else
  return getchar ();
#endif
}

int
get_int (void)
{
  int x;

  scanf (" %d", &x);
  return x;
}

void
put_int (int x)
{
#ifdef RTX
   RtPrintf ("%d", x);
#else
   /* Use fprintf rather than printf, since the latter is unbuffered
      on vxworks */
   fprintf (stdout, "%d", x);
#endif
}

void
put_int_stderr (int x)
{
#ifdef RTX
  RtPrintf ("%d", x);
#else
  fprintf (stderr, "%d", x);
#endif
}

void
put_char (int c)
{
#ifdef RTX
  RtPrintf ("%c", c);
#else
  putchar (c);
#endif
}

void
put_char_stderr (int c)
{
#ifdef RTX
  RtPrintf ("%c", c);
#else
  fputc (c, stderr);
#endif
}

#ifdef __vxworks

char *
mktemp (char *template)
{
  return tmpnam (NULL);
}
#endif
