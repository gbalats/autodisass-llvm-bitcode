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
;; performs the disassembly.

;;; Code:


;; Add handlers for automatically disassembling .class files
(add-to-list 'file-name-handler-alist
             '("\\.bc$" . autodisass-llvm-handler))


;;------------------------------
;; LLVM Bitcode Disassembly
;;------------------------------

(defun autodisass-bitcode-buffer (file &optional jar-file)
  "Disassembles an LLVM Bitcode FILE inside the current buffer, using `javap'.

The JAR-FILE argument is non-nil if the disassembly is happening
inside a jar archive, during auto-extraction."

  (let* ((filename  (file-name-nondirectory file))
         (basename  (file-name-sans-extension filename))
         (temp-file (make-temp-file basename nil ".ll")))

    ;; Disassemble .bc file
    (call-process "llvm-dis" nil t nil
                  "-o" temp-file file)

    ;; Read contents of `temp-file' and then delete it
    (insert-file-contents temp-file)
    (delete-file temp-file)

    ;; Set buffer's filename
    (setq buffer-file-name file)

    ;; Set read-only mode for this buffer
    (setq buffer-read-only t)

    ;; Mark the buffer as unmodified
    (set-buffer-modified-p nil)

    ;; Jump to the beginning of the buffer
    (goto-char (point-min))

    ;; Switch to `llvm-mode'
    (when (fboundp 'llvm-mode)
      (llvm-mode))))


;;------------------------------
;; LLVM bitcode handlers
;;------------------------------

(defun autodisass-llvm-handler (operation &rest args)
  "Handle .bc files by putting the output of `llvm-dis' in the buffer.

OPERATION is the name of the I/O primitive to be handled; ARGS
are the arguments that were passed to that primitive.  This
function only applies to `get-file-buffer' operations."
  (cond ((and (eq operation 'get-file-buffer)
              (executable-find "llvm-dis")
              (y-or-n-p "Disassemble this file using llvm-dis? "))
         (let* ((bc-file     (car args))
                (buffer-name (file-name-nondirectory bc-file)))

           ;; Create new buffer to hold the output of `llvm-dis'
           (with-current-buffer (generate-new-buffer buffer-name)

             ;; Disassemble bitcode inside buffer
             (autodisass-bitcode-buffer bc-file)

             ;; Display some info on what just happened
             (message "Disassembled %s" bc-file)

             ;; Return current buffer
             (current-buffer))))
        (t (let ((inhibit-file-name-handlers
                  (cons 'autodisass-llvm-handler
                        (and (eq inhibit-file-name-operation operation)
                             inhibit-file-name-handlers)))
                 (inhibit-file-name-operation operation))
             (apply operation args)))))


(provide 'autodisass-llvm-bitcode)

;;; autodisass-llvm-bitcode ends here
