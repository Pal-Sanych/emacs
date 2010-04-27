/* s/ file for Unixware.

   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008, 2009, 2010  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


#include "usg5-4.h"

/* fnf@cygnus.com says these exist.  */
#define HAVE_TCATTR
/* #define HAVE_GETWD  (appears to be buggy on SVR4.2) */
#undef HAVE_GETWD

#undef HAVE_SYSV_SIGPAUSE

/* Motif needs -lgen.  */
#define LIBS_SYSTEM -lsocket -lnsl -lelf -lgen

/* This is the same definition as in usg5-4.h, but with sigblock/sigunblock
   rather than sighold/sigrelse, which appear to be BSD4.1 specific and won't
   work if POSIX_SIGNALS is defined.  It may also be appropriate for SVR4.x
   (x<2) but I'm not sure.   fnf@cygnus.com */
/* This sets the name of the slave side of the PTY.  On SysVr4,
   grantpt(3) forks a subprocess, so keep sigchld_handler() from
   intercepting that death.  If any child but grantpt's should die
   within, it should be caught after sigrelse(2). */

#define PTY_TTY_NAME_SPRINTF			\
  {						\
    char *ptsname(), *ptyname;			\
						\
    sigblock(sigmask(SIGCLD));			\
    if (grantpt(fd) == -1)			\
      fatal("could not grant slave pty");	\
    sigunblock(sigmask(SIGCLD));		\
    if (unlockpt(fd) == -1)			\
      fatal("could not unlock slave pty");	\
    if (!(ptyname = ptsname(fd)))		\
      fatal ("could not enable slave pty");	\
    strncpy(pty_name, ptyname, sizeof(pty_name)); \
    pty_name[sizeof(pty_name) - 1] = 0;		\
  }

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* This is totally uncalibrated. */

#define LOAD_AVE_CVT(x) ((int) (((double) (x)) * 100.0 / FSCALE))
#define FSCALE 256.0


#define	PENDING_OUTPUT_COUNT(FILE) ((FILE)->__ptr - (FILE)->__base)

/* arch-tag: d82e92e7-9443-4a60-a581-7f293cbae8a3
   (do not change this comment) */
