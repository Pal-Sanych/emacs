/* Header for Cygwin support routines.
   Copyright (C) 2011  Free Software Foundation, Inc.

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

#ifndef CYGW32_H
#define CYGW32_H
#include <config.h>
#include <windef.h>
#include <sys/cygwin.h>
#include <wchar.h>

#include <signal.h>
#include <stdio.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <setjmp.h>

#include "lisp.h"
#include "coding.h"

/* Character conversion */
#define WCSDATA(x) ((wchar_t*) SDATA (x))
extern wchar_t* to_unicode (Lisp_Object str, Lisp_Object* buf);
extern Lisp_Object from_unicode (Lisp_Object str);

/* Path conversion */
extern Lisp_Object conv_filename_to_w32_unicode (Lisp_Object in,
                                                 int absolute_p);
extern Lisp_Object conv_filename_from_w32_unicode (const wchar_t* in,
                                                   int absolute_p);
EXFUN (Fcygwin_convert_path_to_windows, 2);
EXFUN (Fcygwin_convert_path_from_windows, 2);

/* Functions normally provided by the platform C runtime */
extern BYTE* _mbsinc (const BYTE* current);
extern unsigned _mbsnextc (const BYTE* current);
extern BYTE* _mbsncpy (BYTE* dest, const BYTE* src, size_t count);

/* Misc */
extern void syms_of_cygw32 (void);
extern char * w32_strerror (int error_no);

#endif /* CYGW32_H */
