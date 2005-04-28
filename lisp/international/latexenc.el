;;; latexenc.el --- guess correct coding system in LaTeX files

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Arne J,Ax(Brgensen <arne@arnested.dk>
;; Keywords: mule, coding system, latex

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code tries to guess the correct coding system of a LaTeX file.

;; First it searches for a \inputencoding{...} or
;; \usepackage[...]{inputenc} line in the file and looks up the ... in
;; `latex-inputenc-coding-alist' to find the corresponding coding
;; system.

;; If this fails it will search for AUCTeX's TeX-master or tex-mode's
;; tex-main-file variable in the local variables section and visit
;; that file to get the coding system from the master file. This check
;; can be disabled by setting `latexenc-dont-use-TeX-master-flag' to
;; t.

;; If we have still not found a coding system we will try to use the
;; standard tex-mode's `tex-guess-main-file' and get the coding system
;; from the main file. This check can be disabled by setting
;; `latexenc-dont-use-tex-guess-main-file-flag' to t.

;; The functionality is enabled by adding the function
;; `latexenc-find-file-coding-system' to `file-coding-system-alist'
;; like this

;; (add-to-list 'file-coding-system-alist
;; 	     '("\\.tex\\|\\.ltx\\|\\.dtx\\|\\.drv\\'" . latexenc-find-file-coding-system))

;;; Code:

;;;###autoload
(defcustom latex-inputenc-coding-alist
  '(("ansinew" . windows-1252) ; MS Windows ANSI encoding, extension of Latin-1
    ("applemac" . mac-roman)
    ("ascii" . us-ascii)
    ("cp1250" . windows-1250) ; MS Windows encoding, codepage 1250
    ("cp1252" . windows-1252) ; synonym of ansinew
    ("cp1257" . cp1257)
    ("cp437de" . cp437) ; IBM code page 437 (German version): 225 is \ss
    ("cp437" . cp437) ; IBM code page 437: 225 is \beta
    ("cp850" . cp850) ; IBM code page 850
    ("cp852" . cp852) ; IBM code page 852
    ;; ("cp858" . undecided) ; IBM code page 850 but with a euro symbol
    ("cp865" . cp865) ; IBM code page 865
    ;; The DECMultinational charaterset used by the OpenVMS system
    ;; ("decmulti" . undecided)
    ("latin1" . iso-8859-1)
    ("latin2" . iso-8859-2)
    ("latin3" . iso-8859-3)
    ("latin4" . iso-8859-4)
    ("latin5" . iso-8859-5)
    ("latin9" . iso-8859-15)
    ;; ("latin10" . undecided)
    ;; ("macce" . undecided) ; Apple Central European
    ("next" . next) ; The Next encoding
    ("utf8" . utf-8)
    ("utf8x" . utf-8)) ; used by the Unicode LaTeX package
  "Mapping from encoding names used by LaTeX's \"inputenc.sty\" to Emacs coding systems.
Used by the function `latexenc-find-file-coding-system'."
  :group 'files
  :group 'mule
  :type '(alist :key-type (string :tag "LaTeX input encoding")
		:value-type (coding-system :tag "Coding system")))

;;;###autoload
(defun latexenc-inputenc-to-coding-system (inputenc)
  "Return the corresponding coding-system for the specified input encoding.
Return nil if no matching coding system can be found."
  (cdr (assoc inputenc latex-inputenc-coding-alist)))

;;;###autoload
(defun latexenc-coding-system-to-inputenc (cs)
  "Return the corresponding input encoding for the specified coding system.
Return nil if no matching input encoding can be found."
  (let (result)
    (catch 'result
      (dolist (elem latex-inputenc-coding-alist result)
	(let ((elem-cs (cdr elem)))
	  (when (and (coding-system-p elem-cs)
		     (coding-system-p cs)
		     (eq (coding-system-base cs) (coding-system-base elem-cs)))
	    (setq result (car elem))
	    (throw 'result result)))))))

(defvar latexenc-dont-use-TeX-master-flag nil
  "Non-nil means don't follow TeX-master to find the coding system.")

(defvar latexenc-dont-use-tex-guess-main-file-flag nil
  "Non-nil means don't use tex-guessmain-file to find the coding system.")

;;;###autoload
(defun latexenc-find-file-coding-system (arg-list)
  "Determine the coding system of a LaTeX file if it uses \"inputenc.sty\".
The mapping from LaTeX's \"inputenc.sty\" encoding names to Emacs
coding system names is determined from `latex-inputenc-coding-alist'."
  (if (eq (car arg-list) 'insert-file-contents)
      (save-excursion
        ;; try to find the coding system in this file
        (goto-char (point-min))
        (if (or
             (re-search-forward "^[^%$]*\\inputencoding{\\(.*\\)}" nil t)
             (re-search-forward "^[^%$]*\\usepackage\\[\\(.*\\)\\]{inputenc}" nil t))
            (let* ((match (match-string 1))
                   (sym (intern match)))
              (when (latexenc-inputenc-to-coding-system match)
                (setq sym (latexenc-inputenc-to-coding-system match))
                (when (coding-system-p sym)
		  sym
                  (if (and (require 'code-pages nil t) (coding-system-p sym))
                      sym
                    'undecided))))
          ;; else try to find it in the master/main file
          (let (latexenc-main-file)
            ;; is there a TeX-master or tex-main-file in the local variable section
            (unless latexenc-dont-use-TeX-master-flag
              (goto-char (point-max))
              (when (re-search-backward "^%+ *\\(TeX-master\\|tex-main-file\\): *\"\\(.+\\)\"" nil t)
                (let ((file (concat (file-name-directory (nth 1 arg-list)) (match-string 2))))
                  (if (file-exists-p file)
                      (setq latexenc-main-file file)
                    (if (boundp 'TeX-default-extension)
                        (when (file-exists-p (concat file "." TeX-default-extension))
                          (setq latexenc-main-file (concat file "." TeX-default-extension)))
                      (dolist (ext '("drv" "dtx" "ltx" "tex"))
                        (if (file-exists-p (concat file "." ext))
                            (setq latexenc-main-file (concat file "." ext)))))))))
            ;; try tex-modes tex-guess-main-file
            (when (and (not latexenc-dont-use-tex-guess-main-file-flag)
                       (not latexenc-main-file))
              (when (fboundp 'tex-guess-main-file)
                (let ((tex-start-of-header "\\\\document\\(style\\|class\\)")
                      (default-directory (file-name-directory (nth 1 arg-list))))
                  (setq latexenc-main-file (tex-guess-main-file)))))
            ;; if we found a master/main file get the coding system from it
            (if (and latexenc-main-file
                     (file-readable-p latexenc-main-file))
                (let* ((latexenc-dont-use-tex-guess-main-file-flag t)
                       (latexenc-dont-use-TeX-master-flag t)
                       (latexenc-main-buffer (find-file-noselect latexenc-main-file t)))
                  (or (buffer-local-value 'coding-system-for-write latexenc-main-buffer)
                      (buffer-local-value 'buffer-file-coding-system latexenc-main-buffer)))
              'undecided))))
    'undecided))

(provide 'latexenc)

;;; latexenc.el ends here
