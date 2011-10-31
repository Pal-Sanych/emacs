;;; w32-win.el --- parse switches controlling interface with W32 window system

;; Copyright (C) 1993-1994, 2001-2011  Free Software Foundation, Inc.

;; Author: Kevin Gallo
;; Keywords: terminals

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; w32-win.el:  this file is loaded from ../lisp/startup.el when it recognizes
;; that W32 windows are to be used.  Command line switches are parsed and those
;; pertaining to W32 are processed and removed from the command line.  The
;; W32 display is opened and hooks are set for popping up the initial window.

;; startup.el will then examine startup files, and eventually call the hooks
;; which create the first window (s).

;;; Code:


;; These are the standard X switches from the Xt Initialize.c file of
;; Release 4.

;; Command line		Resource Manager string

;; +rv			*reverseVideo
;; +synchronous		*synchronous
;; -background		*background
;; -bd			*borderColor
;; -bg			*background
;; -bordercolor		*borderColor
;; -borderwidth		.borderWidth
;; -bw			.borderWidth
;; -display		.display
;; -fg			*foreground
;; -fn			*font
;; -font		*font
;; -foreground		*foreground
;; -geometry		.geometry
;; -i			.iconType
;; -itype		.iconType
;; -iconic		.iconic
;; -name		.name
;; -reverse		*reverseVideo
;; -rv			*reverseVideo
;; -selectionTimeout    .selectionTimeout
;; -synchronous		*synchronous
;; -xrm

;; An alist of X options and the function which handles them.  See
;; ../startup.el.

;; (if (not (eq window-system 'w32))
;;     (error "%s: Loading w32-win.el but not compiled for w32" (invocation-name)))

(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'faces)
(require 'select)
(require 'menu-bar)
(require 'dnd)
(require 'w32-vars)

;; Keep an obsolete alias for w32-focus-frame and w32-select-font in case
;; they are used by code outside Emacs.
(define-obsolete-function-alias 'w32-focus-frame 'x-focus-frame "23.1")
(declare-function x-select-font "w32font.c"
                  (&optional frame exclude-proportional))
(define-obsolete-function-alias 'w32-select-font 'x-select-font "23.1")

(defvar w32-color-map) ;; defined in w32fns.c
(make-obsolete 'w32-default-color-map nil "24.1")

(declare-function w32-send-sys-command "w32fns.c")
(declare-function set-message-beep "w32console.c")

;; Conditional on new-fontset so bootstrapping works on non-GUI compiles
(if (fboundp 'new-fontset)
    (require 'fontset))

;; The following definition is used for debugging scroll bar events.
;(defun w32-handle-scroll-bar-event (event) (interactive "e") (princ event))

;; (defun w32-drag-n-drop-debug (event)
;;   "Print the drag-n-drop EVENT in a readable form."
;;   (interactive "e")
;;   (princ event))

(defun w32-drag-n-drop (event)
  "Edit the files listed in the drag-n-drop EVENT.
Switch to a buffer editing the last file dropped."
  (interactive "e")
  (save-excursion
    ;; Make sure the drop target has positive co-ords
    ;; before setting the selected frame - otherwise it
    ;; won't work.  <skx@tardis.ed.ac.uk>
    (let* ((window (posn-window (event-start event)))
	   (coords (posn-x-y (event-start event)))
	   (x (car coords))
	   (y (cdr coords)))
      (if (and (> x 0) (> y 0))
	  (set-frame-selected-window nil window))
      (mapc (lambda (file-name)
		(let ((f (subst-char-in-string ?\\ ?/ file-name))
		      (coding (or file-name-coding-system
				  default-file-name-coding-system)))
		  (setq file-name
			(mapconcat 'url-hexify-string
				   (split-string (encode-coding-string f coding)
						 "/")
				   "/")))
		(dnd-handle-one-url window 'private
				    (concat "file:" file-name)))
		(car (cdr (cdr event)))))
  (raise-frame)))

(defun w32-drag-n-drop-other-frame (event)
  "Edit the files listed in the drag-n-drop EVENT, in other frames.
May create new frames, or reuse existing ones.  The frame editing
the last file dropped is selected."
  (interactive "e")
  (mapcar 'find-file-other-frame (car (cdr (cdr event)))))

;; Bind the drag-n-drop event.
(global-set-key [drag-n-drop] 'w32-drag-n-drop)
(global-set-key [C-drag-n-drop] 'w32-drag-n-drop-other-frame)

;; Keyboard layout/language change events
;; For now ignore language-change events; in the future
;; we should switch the Emacs Input Method to match the
;; new layout/language selected by the user.
(global-set-key [language-change] 'ignore)

(defvar x-resource-name)


;;;; Function keys

 ;;; make f10 activate the real menubar rather than the mini-buffer menu
 ;;; navigation feature.
 (defun w32-menu-bar-open (&optional frame)
   "Start key navigation of the menu bar in FRAME.

This initially activates the first menu-bar item, and you can then navigate
with the arrow keys, select a menu entry with the Return key or cancel with
the Escape key.  If FRAME has no menu bar, this function does nothing.

If FRAME is nil or not given, use the selected frame.
If FRAME does not have the menu bar enabled, display a text menu using
`tmm-menubar'."
   (interactive "i")
   (if menu-bar-mode
       (w32-send-sys-command ?\xf100 frame)
     (with-selected-frame (or frame (selected-frame))
       (tmm-menubar))))


;; W32 systems have different fonts than commonly found on X, so
;; we define our own standard fontset here.
(defvar w32-standard-fontset-spec
 "-*-Courier New-normal-r-*-*-13-*-*-*-c-*-fontset-standard"
 "String of fontset spec of the standard fontset.
This defines a fontset consisting of the Courier New variations for
European languages which are distributed with Windows as
\"Multilanguage Support\".

See the documentation of `create-fontset-from-fontset-spec' for the format.")

(defun x-win-suspend-error ()
  "Report an error when a suspend is attempted."
  (error "Suspending an Emacs running under W32 makes no sense"))

(defvar dynamic-library-alist)
(defvar libpng-version)                 ; image.c #ifdef HAVE_NTGUI

;;; Set default known names for external libraries
(setq dynamic-library-alist
      (list
       '(xpm "libxpm.dll" "xpm4.dll" "libXpm-nox4.dll")
       ;; Versions of libpng 1.4.x and later are incompatible with
       ;; earlier versions.  Set up the list of libraries according to
       ;; the version we were compiled against.  (If we were compiled
       ;; without PNG support, libpng-version's value is -1.)
       (if (>= libpng-version 10400)
	   ;; libpng14-14.dll is libpng 1.4.3 from GTK+
	   '(png "libpng14-14.dll" "libpng14.dll")
	 '(png "libpng12d.dll" "libpng12.dll" "libpng3.dll" "libpng.dll"
	       ;; these are libpng 1.2.8 from GTK+
	       "libpng13d.dll" "libpng13.dll"))
       '(jpeg "jpeg62.dll" "libjpeg.dll" "jpeg-62.dll" "jpeg.dll")
       '(tiff "libtiff3.dll" "libtiff.dll")
       '(gif "giflib4.dll" "libungif4.dll" "libungif.dll")
       '(svg "librsvg-2-2.dll")
       '(gdk-pixbuf "libgdk_pixbuf-2.0-0.dll")
       '(glib "libglib-2.0-0.dll")
       '(gobject "libgobject-2.0-0.dll")
       '(gnutls "libgnutls-26.dll")))

;;; multi-tty support
(defvar w32-initialized nil
  "Non-nil if the w32 window system has been initialized.")

(declare-function x-open-connection "w32fns.c"
                  (display &optional xrm-string must-succeed))
(declare-function create-fontset-from-fontset-spec "fontset"
                  (fontset-spec &optional style-variant noerror))
(declare-function create-fontset-from-x-resource "fontset" ())
(declare-function x-get-resource "frame.c"
                  (attribute class &optional component subclass))
(declare-function x-handle-args "common-win" (args))
(declare-function x-parse-geometry "frame.c" (string))
(defvar x-command-line-resources)

(defun w32-initialize-window-system ()
  "Initialize Emacs for W32 GUI frames."

  ;; Do the actual Windows setup here; the above code just defines
  ;; functions and variables that we use now.

  (setq command-line-args (x-handle-args command-line-args))

  ;; Make sure we have a valid resource name.
  (or (stringp x-resource-name)
      (setq x-resource-name
            ;; Change any . or * characters in x-resource-name to hyphens,
            ;; so as not to choke when we use it in X resource queries.
            (replace-regexp-in-string "[.*]" "-" (invocation-name))))

  (x-open-connection "" x-command-line-resources
                     ;; Exit with a fatal error if this fails and we
                     ;; are the initial display
                     (eq initial-window-system 'w32))

  ;; Create the default fontset.
  (create-default-fontset)
  ;; Create the standard fontset.
  (condition-case err
      (create-fontset-from-fontset-spec w32-standard-fontset-spec t)
    (error (display-warning
	    'initialization
	    (format "Creation of the standard fontset failed: %s" err)
	    :error)))
  ;; Create fontset specified in X resources "Fontset-N" (N is 0, 1,...).
  (create-fontset-from-x-resource)

  ;; Apply a geometry resource to the initial frame.  Put it at the end
  ;; of the alist, so that anything specified on the command line takes
  ;; precedence.
  (let* ((res-geometry (x-get-resource "geometry" "Geometry"))
         parsed)
    (if res-geometry
        (progn
          (setq parsed (x-parse-geometry res-geometry))
          ;; If the resource specifies a position,
          ;; call the position and size "user-specified".
          (if (or (assq 'top parsed) (assq 'left parsed))
              (setq parsed (cons '(user-position . t)
                                 (cons '(user-size . t) parsed))))
          ;; All geometry parms apply to the initial frame.
          (setq initial-frame-alist (append initial-frame-alist parsed))
          ;; The size parms apply to all frames.
          (if (and (assq 'height parsed)
                   (not (assq 'height default-frame-alist)))
              (setq default-frame-alist
                    (cons (cons 'height (cdr (assq 'height parsed)))
                          default-frame-alist))
          (if (and (assq 'width parsed)
                   (not (assq 'width default-frame-alist)))
              (setq default-frame-alist
                    (cons (cons 'width (cdr (assq 'width parsed)))
                          default-frame-alist)))))))

  ;; Check the reverseVideo resource.
  (let ((case-fold-search t))
    (let ((rv (x-get-resource "reverseVideo" "ReverseVideo")))
      (if (and rv (string-match "^\\(true\\|yes\\|on\\)$" rv))
          (setq default-frame-alist
                (cons '(reverse . t) default-frame-alist)))))

  ;; Don't let Emacs suspend under w32 gui
  (add-hook 'suspend-hook 'x-win-suspend-error)

  ;; Turn off window-splitting optimization; w32 is usually fast enough
  ;; that this is only annoying.
  (setq split-window-keep-point t)

  ;; W32 expects the menu bar cut and paste commands to use the clipboard.
  (menu-bar-enable-clipboard)

  ;; Don't show the frame name; that's redundant.
  (setq-default mode-line-frame-identification "  ")

  ;; Set to a system sound if you want a fancy bell.
  (set-message-beep 'ok)
  (setq w32-initialized t))

(add-to-list 'handle-args-function-alist '(w32 . x-handle-args))
(add-to-list 'frame-creation-function-alist '(w32 . x-create-frame-with-faces))
(add-to-list 'window-system-initialization-alist '(w32 . w32-initialize-window-system))

(declare-function set-message-beep "w32fns.c")

(declare-function w32-raw-clipboard-fetch-data "w32select.c")
(declare-function x-server-version "w32fns.c" (&optional display))

(defun w32-version ()
  "Return the MS-Windows version numbers.
The value is a list of three integers: the major and minor version
numbers, and the build number."
  (x-server-version))

;;; Fix interface to (X-specific) mouse.el
(defun x-set-selection (type data)
  "Make an X selection of type TYPE and value DATA.
The argument TYPE (nil means `PRIMARY') says which selection, and
DATA specifies the contents.  TYPE must be a symbol.  \(It can also
be a string, which stands for the symbol with that name, but this
is considered obsolete.)  DATA may be a string, a symbol, an
integer (or a cons of two integers or list of two integers).

The selection may also be a cons of two markers pointing to the same buffer,
or an overlay.  In these cases, the selection is considered to be the text
between the markers *at whatever time the selection is examined*.
Thus, editing done in the buffer after you specify the selection
can alter the effective value of the selection.

The data may also be a vector of valid non-vector selection values.

The return value is DATA.

Interactively, this command sets the primary selection.  Without
prefix argument, it reads the selection in the minibuffer.  With
prefix argument, it uses the text of the region as the selection value.

Note that on MS-Windows, primary and secondary selections set by Emacs
are not available to other programs."
  (put 'x-selections (or type 'PRIMARY) data))

(defun x-get-selection (&optional type _data-type)
  "Return the value of an X Windows selection.
The argument TYPE (default `PRIMARY') says which selection,
and the argument DATA-TYPE (default `STRING') says
how to convert the data.

TYPE may be any symbol \(but nil stands for `PRIMARY').  However,
only a few symbols are commonly used.  They conventionally have
all upper-case names.  The most often used ones, in addition to
`PRIMARY', are `SECONDARY' and `CLIPBOARD'.

DATA-TYPE is usually `STRING', but can also be one of the symbols
in `selection-converter-alist', which see."
  (get 'x-selections (or type 'PRIMARY)))

;; x-selection-owner-p is used in simple.el
(defun x-selection-owner-p (&optional type)
  (and (memq type '(nil PRIMARY SECONDARY))
       (get 'x-selections (or type 'PRIMARY))))

;; Set to a system sound if you want a fancy bell.
(set-message-beep nil)

;; The "Windows" keys on newer keyboards bring up the Start menu
;; whether you want it or not - make Emacs ignore these keystrokes
;; rather than beep.
(global-set-key [lwindow] 'ignore)
(global-set-key [rwindow] 'ignore)

(make-obsolete-variable 'w32-enable-italics
                        'w32-enable-synthesized-fonts "21.1")
(make-obsolete-variable 'w32-charset-to-codepage-alist
                        'w32-charset-info-alist "21.1")


;;;; Selections

;; We keep track of the last text selected here, so we can check the
;; current selection against it, and avoid passing back our own text
;; from x-selection-value.
(defvar x-last-selected-text nil)

(defun x-get-selection-value ()
  "Return the value of the current selection.
Consult the selection.  Treat empty strings as if they were unset."
  (if x-select-enable-clipboard
      (let (text)
        ;; Don't die if x-get-selection signals an error.
        (condition-case c
            (setq text (w32-get-clipboard-data))
          (error (message "w32-get-clipboard-data:%s" c)))
        (if (string= text "") (setq text nil))
        (cond
         ((not text) nil)
         ((eq text x-last-selected-text) nil)
         ((string= text x-last-selected-text)
          ;; Record the newer string, so subsequent calls can use the 'eq' test.
          (setq x-last-selected-text text)
          nil)
         (t
          (setq x-last-selected-text text))))))

(defalias 'x-selection-value 'x-get-selection-value)

(defvar w32-clipboard-current-data nil
  "The value currently advertised as being available on the
clipboard.")

;; 13 == CF_UNICODETEXT
(add-hook 'w32-clipboard-render-functions
  (defun w32-clipboard-render-cf-unicodetext (format)
    (when (eql format 13)
      (concat
       (encode-coding-string w32-clipboard-current-data 'utf-16le-dos t)
       "\0"))))
(add-to-list 'w32-clipboard-advertised-types 13)

(defconst w32-clipboard-format-html
  (w32-register-clipboard-format "HTML Format")
  "The system-specific numeric ID of the HTML clipboard format.")

(defconst w32-clipboard-html-header
  (concat "Version:0.9\r\n"
          "StartHTML:%0006d\r\n"
          "EndHTML:%0006d\r\n"
          "StartFragment%0006d\r\n"
          "EndFragment:%0006d\r\n"))

(defconst w32-clipboard-html-fragment-prefix
  (concat "<!DOCTYPE HTML>\r\n"
          "<html><head><title></title></head><body>\r\n"
          "<!--StartFragment-->\r\n"
          "<pre%s>"
))

(defconst w32-clipboard-html-fragment-suffix
  (concat
   "</pre>\r\n"
   "<!--EndFragment-->\r\n"
   "</body></html>\r\n"))

(defun w32-clipboard-color-string (name color)
  (if color
      (progn
        (setq color (color-values color))
        (format "%s:#%02x%02x%02x;"
                name
                (/ (car color) 256)
                (/ (car (cdr color)) 256)
                (/ (car (cdr (cdr color))) 256)))
    ""))

(defun w32-clipboard-face-as-style (face &optional background)
  (if (null face)
      ""
    (concat
     " style=\""
     (w32-clipboard-color-string 
      "color"
      (face-attribute face :foreground nil 'default))
     (if background
         (w32-clipboard-color-string 
          "background-color"
          (face-attribute face :background nil 'default))
       "")
    "\"")))

(defun w32-clipboard-string-to-html (str)
  "Format a string for Windows HTML interchange via the clipboard."
  (let (face prev-face
             body
             result
             chr part
             (i 0) (len (length str)))
    (while (< i len)
      (setq chr (aref str i))
      (cond ((and (>= chr 32)
                  (<= chr 126)
                  (not (memql chr '(?\< ?\> ?\" \?&))))
             (setq part (char-to-string chr)))
            ((eql chr ?\n)
             (setq part "\r\n"))
            (t
             (setq part (format "&#x%x;" chr))))
      (setq face (or (get-text-property i 'face str)))
      (when (or (not body) (not (eq face prev-face)))
        (setq body (cons (format (if body "</span><span%s>" "<span%s>")
                                 (w32-clipboard-face-as-style face))
                         body))
        (setq prev-face face))
      (setq body (cons part body))
      (setq i (1+ i)))

    (when body
      (setq body (cons "</span>" body)))
    (setq body (mapconcat #'identity (nreverse body) ""))
    (let* ((prefix (format w32-clipboard-html-fragment-prefix
                           (w32-clipboard-face-as-style 'default t)))
           (suffix w32-clipboard-html-fragment-suffix)
           (StartHTML (length w32-clipboard-html-header))
           (StartFragment (+ StartHTML (length prefix)))
           (EndFragment (+ StartFragment (length body)))
           (EndHTML (+ EndFragment (length suffix))))
      (setq result
            (concat
             (format w32-clipboard-html-header
                     StartHTML
                     EndHTML
                     StartFragment
                     EndFragment)
             prefix
             body
             suffix))
      (message "%S" result)
      result)))

(add-hook 'w32-clipboard-render-functions
  (defun w32-clipboard-render-cf-html (format)
    (when (eql format w32-clipboard-format-html)
      (w32-clipboard-string-to-html w32-clipboard-current-data))))
(add-to-list 'w32-clipboard-advertised-types w32-clipboard-format-html)

(defun w32-clipboard-render (type)
  (run-hook-with-args-until-success 'w32-clipboard-render-functions type))

(add-hook 'w32-lost-selection-functions
  (defun w32-clipboard-lost-selection (cb)
    (setq w32-clipboard-current-data nil)))

(defun w32-set-clipboard-data (string &optional ignored)
  (setq w32-clipboard-current-data string)
  (w32-claim-clipboard))

(defun w32-get-clipboard-data (&optional ignored)
  "This gets the clipboard data in text format.  "
  ;; Don't return our own selection.
  (unless w32-clipboard-current-data
    (let ((raw-unicode (w32-get-raw-clipboard-data 13)))
      (when raw-unicode
        (decode-coding-string
         ;; Trim off trailing nulls
         (if (> (length raw-unicode) 2)
             (substring raw-unicode 0 -2)
           raw-unicode)
         'utf-16le t)))))

;; Arrange for the kill and yank functions to set and check the clipboard.
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-get-selection-value)

(provide 'w32-win)

;;; w32-win.el ends here
