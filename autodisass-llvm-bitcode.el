;;; autodisass-llvm-bitcode --- Automatically disassemble LLVM bitcode

;; Copyright (C) 2014, George Balatsouras <gbalats(at)gmail(dot)com>
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;; To use, save `autodisass-llvm-bitcode.el' to a directory in your
;; load-path and add the following to your `.emacs'.
;;
;; (require 'autodisass-llvm-bitcode)


;;; Commentary:

;; This package enables automatic disassembly of LLVM bitcode, when
;; opening an LLVM .bc file.
;;
;; When `llvm-mode' is available, it is automatically selected for the
;; current LLVM bitcode-containing buffer.
;;
;; In any case, `llvm-dis' must be installed in the system for this
;; extension to have any effect, since that is the tool that actually
;; performs the disassembly.  If not, you will have to customize the
;; variable `ad-llvm-bitcode-disassembler' to point to another
;; disassembler command.

;;; Code:


(defconst autodisass-llvm-bitcode-version "0.1")

(defgroup autodisass-llvm-bitcode nil
  "Automatic disassembly of LLVM bitcode."
  :tag    "LLVM Bitcode Disassembly"
  :prefix "ad-llvm-bitcode-"
  :group  'autodisass)


(defconst ad-llvm-bitcode-regexp "\\.bc$"
  "Regular expressions that matches LLVM bitcode files.")


(defcustom ad-llvm-bitcode-disassembler "llvm-dis"
  "Return the name of the disassembler command.
If the command is not on your path, you may specify a fully
qualified path to it.  The command should support the -o option
for specifying an output file name, and should accept the input
file name as its last argument."
  :tag "Disassembler command"
  :group 'autodisass-llvm-bitcode
  :type 'string)


(defcustom ad-llvm-bitcode-parameters '("-show-annotations")
  "Extra parameters for the disassembler process."
  :tag "Command line options"
  :group 'autodisass-llvm-bitcode
  :type '(repeat string))


(defun ad-llvm-bitcode-make-temp-file (file)
  "Return a temporary file name for bitcode disassembly.
This will be where the disassembled contents of the bitcode FILE
will be placed."
  (let* ((filename  (file-name-nondirectory file))
         (basename  (file-name-sans-extension filename))
         (temp-file (make-temp-file basename nil ".ll")))
    temp-file))


(defun ad-llvm-bitcode-buffer (file)
  "Disassembles an LLVM Bitcode FILE inside the current buffer."
  (let ((temp-file (ad-llvm-bitcode-make-temp-file file)))
    ;; Print start of disassembly message
    (message "Disassembling %s" file)
    ;; Call disassembler and place contents in temp file
    (apply 'call-process ad-llvm-bitcode-disassembler
           nil t nil (append ad-llvm-bitcode-parameters
                             (list "-o" temp-file file)))
    ;; Read contents of `temp-file' and then delete it
    (insert-file-contents temp-file nil nil nil t)
    (delete-file temp-file)
    ;; Mark as read-only and unmodified
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    ;; Switch to `llvm-mode'
    (when (fboundp 'llvm-mode)
      (llvm-mode))
    ;; Print success message
    (message "Disassembled %s" file)))


(defun ad-llvm-bitcode-disassemble-p ()
  "Return t if automatic disassembly should be performed."
  (let ((file (buffer-file-name)))
    (and (string-match ad-llvm-bitcode-regexp file)
         (executable-find ad-llvm-bitcode-disassembler)
         (y-or-n-p (format "Disassemble %s using %s? " file
                           ad-llvm-bitcode-disassembler)))))


;; Add hook for automatic disassembly
(add-hook 'find-file-hooks
          (lambda () (when (ad-llvm-bitcode-disassemble-p)
                       (ad-llvm-bitcode-buffer (buffer-file-name)))))


(provide 'autodisass-llvm-bitcode)

;;; autodisass-llvm-bitcode ends here
