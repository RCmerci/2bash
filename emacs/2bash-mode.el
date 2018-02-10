;;; 2bash-mode.el --- major mode for .2bash file     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  RCmerci

;; Author: RCmerci <RCmerci@rcmerci@gmail.com>
;; Keywords: languages
;; Package-Requires: ((emacs "24.4") (s "20160508.2357"))
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
(require 's)

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



(defun 2bash-backward-up-list ()
  (condition-case nil
      (backward-up-list)
    (scan-error nil)))

(defmacro 2bash-in-string-p ()
  `(nth 3 (syntax-ppss)))

(defmacro 2bash-paren-level ()
  `(car (syntax-ppss)))

(defmacro 2bash-in-string-or-comment-p ()
  `(nth 8 (syntax-ppss)))

;;; indent
(defun 2bash-indentation-at-line-begin ()
  (save-excursion
    (back-to-indentation)
    (let ((origin-line-num (line-number-at-pos))
	  (origin-paren-level (2bash-paren-level)))
      (cond ((2bash-in-string-p)
	     (current-indentation))
	    ((zerop (2bash-paren-level))
	     0)
	    ((looking-at "[])}]")
	     (2bash-backward-up-list)
	     (current-indentation))
	    ((progn (2bash-backward-up-list) (< (2bash-paren-level) origin-paren-level))
	     (if (= origin-line-num (line-number-at-pos))
		 (error "it's impossible, because `back-to-indentation' is called before")
	       (+ (current-indentation) tab-width)))
	    (t
	     (current-indentation))
	    ))))

(defun 2bash-indent-line ()
  (interactive)
  (let (result-pos
	origin-indent
	(pos (- (point-max) (point)))
	(origin (point))
	(beg (line-beginning-position)))
      (back-to-indentation)
      (if (2bash-in-string-or-comment-p)
	  (goto-char origin)
	(setq indent (2bash-indentation-at-line-begin))
	(setq origin-indent (current-indentation))
	(delete-region (line-beginning-position) (point))
	(indent-to indent))

      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos)))
	)
  )



(defun 2bash-compile-current-file ()
  (interactive)
  (let ((cmd (executable-find "sbash.exe"))
	(output-filename (s-concat (s-chop-suffix ".2bash" buffer-file-name) ".sh")))
    (compilation-start (format "%s compile %s -o %s" cmd buffer-file-name output-filename))))



;;; keymap
(defvar 2bash-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'2bash-compile-current-file)
    m)
  "Keymap used by `2bash-mode'.")




;;;###autoload
(define-derived-mode 2bash-mode fundamental-mode "2bash"
  "major mode for .2bash file"
  :syntax-table 2bash-syntax-table
  (setq font-lock-defaults '(2bash-font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (add-hook 'completion-at-point-functions #'2bash-keywords-completion-at-point nil 'local)
  (setq-local indent-line-function #'2bash-indent-line)
  )

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.2bash\\'" '2bash-mode))



(provide '2bash-mode)
;;; 2bash-mode.el ends here
