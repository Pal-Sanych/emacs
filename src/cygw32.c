/* Cygwin support routines.
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


#include "cygw32.h"

static Lisp_Object Qutf_16_le;

/* Advance the given string by one character */
BYTE*
_mbsinc (const BYTE* current)
{
  return (BYTE*)current + 1; /* XXX: stub */
}

unsigned
_mbsnextc (const BYTE* current)
{
  return *current; /* XXX: stub */
}

BYTE*
_mbsncpy (BYTE* dest, const BYTE* src, size_t count)
{
  return strncpy (dest, src, count); /* XXX: stub */
}

extern Lisp_Object
conv_filename_to_w32_unicode (Lisp_Object in, int absolute_p)
{
  ssize_t converted_len;
  Lisp_Object converted;
  unsigned flags;

  flags = CCP_POSIX_TO_WIN_W;
  if (!absolute_p) {
    flags |= CCP_RELATIVE;
  }

  in = ENCODE_UTF_8 (in);

  converted_len = cygwin_conv_path (flags, SDATA (in), NULL, 0);
  if (converted_len < 2)
    error ("cygwin_conv_path: %s", strerror (errno));
  
  converted = make_uninit_string (converted_len - 1);
  if (cygwin_conv_path (flags, SDATA (in),
                        SDATA (converted), converted_len))
    error ("cygwin_conv_path: %s", strerror (errno));
  
  return converted;
}

extern Lisp_Object
conv_filename_from_w32_unicode (const wchar_t* in, int absolute_p)
{
  ssize_t converted_len;
  Lisp_Object converted;
  unsigned flags;

  flags = CCP_WIN_W_TO_POSIX;
  if (!absolute_p) {
    flags |= CCP_RELATIVE;
  }

  converted_len = cygwin_conv_path (flags, in, NULL, 0);
  if (converted_len < 1)
    error ("cygwin_conv_path: %s", strerror (errno));
  
  converted = make_uninit_string (converted_len - 1 /*subtract terminator*/);
  if (cygwin_conv_path (flags, in, SDATA (converted), converted_len))
    error ("cygwin_conv_path: %s", strerror (errno));
  
  return code_convert_string_norecord (converted, Qutf_8, 0);
}

Lisp_Object
from_unicode (Lisp_Object str)
{
  CHECK_STRING (str);
  if (!STRING_MULTIBYTE (str) &&
      SBYTES (str) & 1)
    {
      str = Fsubstring (str, make_number (0), make_number (-1));
    }
  
  return code_convert_string_norecord (str, Qutf_16_le, 0);
}

wchar_t*
to_unicode (Lisp_Object str, Lisp_Object* buf)
{
  *buf = code_convert_string_norecord (str, Qutf_16_le, 1);
  /* We need to make an additional copy to ensure that the final
     string is _doubly_ zero terminated --- that is, that the string
     is terminated by two zero bytes and one utf-16le null character.
     Because strings are already terminated with a single zero byte,
     we just add one additional zero. */
  str = make_uninit_string (SBYTES (*buf) + 1);
  memcpy (SDATA (str), SDATA (*buf), SBYTES (*buf));
  SDATA (str) [SBYTES (*buf)] = '\0';
  *buf = str;
  return WCSDATA (*buf);
}

DEFUN ("cygwin-convert-path-to-windows",
       Fcygwin_convert_path_to_windows, Scygwin_convert_path_to_windows,
       1, 2, 0,
       doc: /* Convert PATH to a Windows path.  If ABSOLUTE-P if
               non-nil, return an absolute path.*/)
  (Lisp_Object path, Lisp_Object absolute_p)
{
  return from_unicode (
    conv_filename_to_w32_unicode (path, absolute_p == Qnil ? 0 : 1));
}

DEFUN ("cygwin-convert-path-from-windows",
       Fcygwin_convert_path_from_windows, Scygwin_convert_path_from_windows,
       1, 2, 0,
       doc: /* Convert a Windows path to a Cygwin path.  If ABSOLUTE-P
               if non-nil, return an absolute path.*/)
  (Lisp_Object path, Lisp_Object absolute_p)
{
  return conv_filename_from_w32_unicode (to_unicode (path, &path),
                                         absolute_p == Qnil ? 0 : 1);
}

void
syms_of_cygw32 (void)
{
  /* No, not utf-16-le: that one has a BOM.  */
  DEFSYM (Qutf_16_le, "utf-16le"); 
  defsubr (&Scygwin_convert_path_from_windows);
  defsubr (&Scygwin_convert_path_to_windows);
}
