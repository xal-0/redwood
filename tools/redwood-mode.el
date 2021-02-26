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
  `(("func\\s-+\\(\\sw+\\)" (1 font-lock-function-name-face)
     ("(\\(\\w+\\)"  nil nil (1 font-lock-variable-name-face))
     (",\s-*\\(\\w+\\)"  nil nil (1 font-lock-variable-name-face)))
    (,(regexp-opt '("func\s" "for\s" "in\s" "while\s" "if\s" "else\s" "return\s"))
     . font-lock-keyword-face)
    (,(regexp-opt '("null\s" "true\s" "false\s")) . font-lock-constant-face)
    (,(regexp-opt '("^\\(print\\)$")) 1 font-lock-builtin-face)))

(defvar redwood-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?# "<" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    (modify-syntax-entry ?_ "w" syntax-table)
    syntax-table))

(defun redwood-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) sample-indent-basic)
    (`(,_ . ",") (smie-rule-separator kind))
    (`(:after . ":=") sample-indent-basic)
    (`(:before . ,(or `"begin" `"(" `"{")))
     (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before . "if")
     (and (not (smie-rule-bolp)) (smie-rule-prev-p "else")
          (smie-rule-parent))))

;;;###autoload
(define-derived-mode redwood-mode prog-mode "redwood"
  "Major mode for the Redwood programming language."
  (setq font-lock-defaults '(redwood-font-lock-keywords))
  (set-syntax-table redwood-mode-syntax-table)
  (setq-local comment-start "# "
              comment-end ""
              indent-tabs-mode nil
              tab-width 2
              indent-line-function 'tab-to-tab-stop))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.rw\\'" 'redwood-mode))

(provide 'redwood-mode)
;;; redwood-mode.el ends here
