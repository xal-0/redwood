;;; redwood-mode.el --- Major mode for the Redwood programming language  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author: Sam Schweigel <sam@schweigel.ca>
;; Keywords: languages

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

;;; Code:

(defconst redwood-font-lock-keywords
  `(,(rx (seq symbol-start
              (or "func" "for" "in" "while" "if" "else" "return")
              symbol-end))
    (,(rx (seq symbol-start (or "null" "true" "false" "pi" "E") symbol-end))
     . font-lock-constant-face)
    (,(rx (seq symbol-start
               (or "println" "print" "push" "delete" "string" "length"
                   "sin" "cos" "tan" "asin" "acos" "atan" "sqrt" "log"
                   "rand" "randi" "ceil" "floor" "round" "abs" "key"
                   "circle" "line" "sprite" "text" "rect")
               symbol-end))
     . font-lock-builtin-face)
    (,(rx (seq (group (+ word)) (* space) ":"))
     1 font-lock-variable-name-face)
    (,(rx (seq symbol-start "func" symbol-end (* space)
               symbol-start (group (+ word)) symbol-end))
     (1 font-lock-function-name-face)
     (,(rx (seq "(" (* space) (group (+ word)))) nil nil (1 font-lock-variable-name-face))
     (,(rx (seq "," (* space) (group (+ word)))) nil nil (1 font-lock-variable-name-face)))))

(defvar redwood-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?# "<" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    (modify-syntax-entry ?_ "w" syntax-table)
    syntax-table))

(defun redwood-indent-line-function ()
  (save-excursion
    (beginning-of-line)
    (while
        (search-forward-regexp "\\s)" (line-end-position) t))
    (indent-line-to (* 2 (car (syntax-ppss)))))
  (when
      ;; Suggested by https://emacs.stackexchange.com/questions/16792
      (save-excursion
        (beginning-of-line)
        (looking-at-p "[[:space:]]*$"))
    (end-of-line)))

;;;###autoload
(define-derived-mode redwood-mode prog-mode "redwood"
  "Major mode for the Redwood programming language."
  (setq font-lock-defaults '(redwood-font-lock-keywords))
  (set-syntax-table redwood-syntax-table)
  (setq-local
   comment-start "# "
   comment-end ""
   indent-tabs-mode nil
   tab-width 2
   indent-line-function 'redwood-indent-line-function))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.rw\\'" 'redwood-mode))

(provide 'redwood-mode)
;;; redwood-mode.el ends here
