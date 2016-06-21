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

;; This mode provides syntax highlighting for execline scripts.

;; Code:

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

;;;###autoload
(define-derived-mode execline-mode prog-mode
  "execline"
  "Mode for editing execline scripts"
  (setq font-lock-defaults '((execline-font-lock-keywords)))
  (setq comment-start "#")
  (set-syntax-table execline-mode-syntax-table)
  (run-hooks 'execline-mode-hook))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("execlineb" . execline-mode))

(provide 'execline-mode)

;;; execline-mode.el ends here
