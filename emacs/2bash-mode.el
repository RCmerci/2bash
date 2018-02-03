;;; 2bash-mode.el --- major mode for .2bash file     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  RCmerci

;; Author: RCmerci <RCmerci@rcmerci@gmail.com>
;; Keywords: languages
;; Package-Requires: ((emacs "24.4"))
;; Version: 20180203.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(defconst 2bash-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"")
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    table
    ))

(setq 2bash-font-lock-keywords
  (let* ((keywords '("fun" "return" "for" "while" "in" "if" "else" "break" "continue"))
	 (types '("num" "bool" "string" "num list" "bool list" "string list"))
	 (builtins '("call" "printf" "sprintf" "println" "exists" "list" "num"))
	 (consts '("true" "false"))
	 (keywords-regex (regexp-opt keywords 'words))
	 (types-regex (regexp-opt types 'words))
	 (builtins-regex (regexp-opt builtins 'words))
	 (consts-regex (regexp-opt consts 'words))
	 )
    `((,keywords-regex . font-lock-keyword-face)
      (,types-regex . font-lock-type-face)
      (,builtins-regex . font-lock-builtin-face)
      (,consts-regex . font-lock-constant-face)
      )
    )
  )



;;; keywords completion
(setq 2bash-completion-keywords
      '("fun" "return" "for" "while" "in" "if" "else" "break" "continue" "num" "bool" "string" "num list" "bool list" "string list" "call" "printf" "sprintf" "println" "exists" "list" "true" "false"))

(defun 2bash-keywords-completion-at-point ()
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'symbol))
	 (start (car bds))
	 (end (cdr bds)))
    (list start end 2bash-completion-keywords . nil )
    )
  )





;; (defun 2bash-compile-current-file ()
;;   (interactive)
;;   (command-execute "2bash.exe")

;;   )






(define-derived-mode 2bash-mode fundamental-mode "2bash"
  "major mode for .2bash file"
  :syntax-table 2bash-syntax-table
  (setq font-lock-defaults '(2bash-font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (add-hook 'completion-at-point-functions #'2bash-keywords-completion-at-point nil 'local)
  )











(provide '2bash-mode)
;;; 2bash-mode.el ends here
