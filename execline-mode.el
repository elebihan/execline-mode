;;; execline-mode.el --- Major mode for editing execline scripts -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Eric Le Bihan
;; Author: Eric Le Bihan <eric.le.bihan.dev@free.fr>
;; Version: 0.1.1
;; Keywords: languages
;; URL: https://github.com/elebihan/execline-mode/
;; Package-Requires: ((emacs "24.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

;; Commentary:

;; This mode provides syntax highlighting and interactive commands for
;; [execline][] scripts.
;;
;; Installation:
;;
;; Add the following lines to `.emacs':
;;
;;    (add-to-list 'load-path "/path/to/execline-mode/")
;;    (require 'execline-mode)
;;
;;
;; [execline]: http://skarnet.org/software/execline/index.html

;; Code:

(require 'smie)

(defgroup execline-mode nil
  "Support for editing execline script"
  :link '(url-link "http://skarnet.org/software/execline/index.html")
  :group 'languages)

(defcustom execline-program
  (or (executable-find "execlineb") "/usr/bin/execlineb")
  "*Location of the execlineb program"
  :type  '(file :must-match t)
  :group 'execline-mode)

(defcustom execline-buffer "*execline output*"
  "Name of the buffer to attach to the execlineb process"
  :type '(string :tag "Buffer name")
  :group 'execline-mode)

(defcustom execline-indent-offset 2
  "Indent execline code by this number of spaces."
  :type 'integer
  :group 'execline-mode)

(defconst execline-proc-progs
  '("cd"
    "umask"
    "emptyenv"
    "export"
    "unexport"
    "fdclose"
    "fdblock"
    "fdmove"
    "fdswap"
    "fdreserve"
    "redirfd"
    "piperw"
    "heredoc"
    "wait"
    "getcwd"
    "getpid"
    "exec"
    "tryexec"
    "exit"
    "trap"
    "withstdinas"))

(defconst execline-block-progs
  '("foreground"
    "background"
    "if"
    "ifelse"
    "ifte"
    "backtick"
    "ifthenelse"
    "pipeline"
    "runblock"))

(defconst execline-var-progs
  '("define"
    "importas"
    "import"
    "elglob"
    "elgetpositionals"
    "multidefine"
    "multisubstitute"))

(defconst execline-loop-progs
  '("forx"
    "forstdin"
    "forbacktickx"
    "loopwhilex"))

(defconst execline-opt-progs
  '("elgetopt"
    "shift"
    "dollarat"))

(defconst execline-misc-progs
  '("homeof"))

(defconst execline-builtin-progs
  (append execline-proc-progs
          execline-block-progs
          execline-var-progs
          execline-loop-progs
          execline-opt-progs
          execline-misc-progs))

(defvar execline-font-lock-keywords
  (list
   '("#.*" . font-lock-comment-face)
   '("\\$\\([[:alpha:]_][[:alnum:]_]*\\|[0-9]+\\)" . font-lock-variable-name-face)
   `(,(regexp-opt execline-builtin-progs 'words) . font-lock-builtin-face))
  "Syntax highlighting for execline")

(defvar execline-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for execline mode")

(defvar execline-mode-hook nil)

(defun execline-execute-string (string)
  "Execute the given STRING as execline script"
  (interactive "sScript to execute: ")
  (let ((execline-args (list mode-name
                             execline-buffer
                             execline-program
                             "-c"
                             string)))
    (set-process-sentinel
     (apply 'start-process execline-args)
     'execline-sentinel)))

(defun execline-execute-region (start end)
  "Execute the region delimited by START and END as execline script"
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (execline-execute-string text)))

(defun execline-execute-buffer (buffer)
  "Execute entire buffer as execline script"
  (interactive "bScript buffer to execute: ")
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (execline-execute-region (point-min) (point-max)))))

(defun execline-execute-file (filename)
  "Execute an execline script file"
  (interactive "fScript file to execute: ")
  (let ((execline-args (list mode-name
                             execline-buffer
                             execline-program
                             filename)))
    (set-process-sentinel
     (apply 'start-process execline-args)
     'execline-sentinel)))

(defun execline-sentinel (proc event)
  (let ((buffer (process-buffer proc))
        (message (replace-regexp-in-string "\n$" "" event)))
    (if (or (string= message "finished")
            (string-match "exited abnormally" message))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (switch-to-buffer-other-window buffer)
            (end-of-buffer)
            (message "Execution of execline script %s" message)))
      (message "Failed to execute execline script: %s" message))))

(defvar execline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-s" 'execline-execute-string)
    (define-key map "\C-c\C-r" 'execline-execute-region)
    (define-key map "\C-c\C-c" 'execline-execute-buffer)
    (define-key map "\C-c\C-l" 'execline-execute-file)
    map)
  "Keymap for `execline-mode'.")

(defun execline-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) execline-indent-offset)
    (`(:before . "{")
     (if (not (smie-rule-hanging-p))
         (smie-rule-parent 0)))
    (`(:after . "}") (smie-rule-parent (- execline-indent-offset)))))

;;;###autoload
(define-derived-mode execline-mode prog-mode
  "execline"
  "Mode for editing execline scripts

\\{execline-mode-map} "
  (setq font-lock-defaults '((execline-font-lock-keywords)))
  (setq comment-start "#")
  (setq indent-tabs-mode nil)
  (smie-setup nil #'execline-smie-rules)
  (setq smie-indent-basic execline-indent-offset)
  (setq electric-indent-chars (cons "}" electric-indent-chars))
  (set-syntax-table execline-mode-syntax-table)
  (run-hooks 'execline-mode-hook))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("execlineb" . execline-mode))

(provide 'execline-mode)

;;; execline-mode.el ends here
