;;; vc-bzr.el --- tests for progmodes/subword.el

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author: Daniel Colascione <dancol@dancol.org>

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

;;; Code:

(require 'ert)
(require 'subword)

(ert-deftest subword-test-forward ()
  "Test that motion in subword-mode stops at the right places."

  (let* ((line "fooBarBAZ quXD g_TESTThingAbc word BLAH test")
         (fwrd "*  *  *  *  * * *    *    *  *    *    *    *")
         (bkwd "*  *  *   * *  * *   *    *   *    *    *   *"))

    (with-temp-buffer
      (subword-mode 1)
      (insert line)

      ;; Test forward motion.
      
      (goto-char (point-min))
      (let ((stops (make-string (length fwrd) ?\ )))
        (while (progn
                 (aset stops (1- (point)) ?\*)
                 (not (eobp)))          
          (forward-word))
        (should (equal stops fwrd)))

      ;; Test backward motion.

      (goto-char (point-max))
      (let ((stops (make-string (length bkwd) ?\ )))
        (while (progn
                 (aset stops (1- (point)) ?\*)
                 (not (bobp)))          
          (backward-word))
        (should (equal stops bkwd))))))
