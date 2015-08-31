;;; fstar-mode.el --- support for the F* language in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015 Clément Pit--Claudel
;; Author: Clément Pit--Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/FStarLang/fstar.el

;; Created: 27 Aug 2015
;; Version: 0.1
;; Package-Requires: ((flycheck "0.25") (emacs "24.3"))
;; Keywords: convenience, languages

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file implements basic support for programming in F* in Emacs.

;;; Code:

;;; Imports

(require 'dash)
(require 'flycheck) ;; Real-time verification

;;; Group

(defgroup fstar nil
  "F* mode."
  :group 'languages)

;;; Flycheck

(flycheck-def-executable-var fstar "fstar")

(flycheck-define-command-checker 'fstar
  "Flycheck checker for F*."
  :command '("fstar" source-inplace)
  :error-patterns
  `((error "ERROR: " (message) "near line " line ", character " column " in file " (file-name))
    (error (file-name) "(" line "," column "-" (+ (any digit)) "," (+ (any digit)) ")"
           (* (any ": ")) (? "Error" (* (any " \n")))
           (message)))
  :error-filter #'flycheck-increment-error-columns
  :modes '(fstar-mode))

(add-to-list 'flycheck-checkers 'fstar)

;;; Prettify symbols

(defconst fstar-symbols-alist '(("exists" . ?∃) ("forall" . ?∀) ("fun" . ?λ)
                                ("nat" . ?ℕ) ("int" . ?ℤ)
                                ("True" . ?⊤) ("False" . ?⊥)
                                ("*" . ?×) ("~>" . ?↝)
                                ("<=" . ?≤) (">=" . ?≥) ("::" . ?⸬)
                                ("/\\" . ?∧) ("\\/" . ?∨) ("=!=" . ?≠)
                                ("&&" . ?∧) ("||" . ?∨) ("<>" . ?≠)
                                ("<==>" . ?⟺) ("==>" . ?⟹)
                                ("=>" . ?⇒) ("->" . ?→)
                                ("'a" . ?α) ("'b" . ?β) ("'c" . ?γ)
                                ("'d" . ?δ) ("'e" . ?η))
  "Fstar symbols.")

;;; Font-Lock

;; Loosely derived from https://github.com/FStarLang/atom-fstar/blob/master/grammars/fstar.cson

(defconst fstar-syntax-structure
  (regexp-opt '("begin" "end"
                "let" "rec" "in" "val"
                "kind" "type" "logic"
                "private" "opaque" "total" "default"
                "open" "module")
              'symbols))

(defconst fstar-syntax-keywords
  (regexp-opt '("and"
                "forall" "exists"
                "assert" "assume"
                "fun" "function"
                "match" "with"
                "if" "then" "else"
                "ALL" "All" "DIV" "Div" "EXN" "Ex" "Exn" "GHOST" "GTot" "Ghost"
                "Lemma" "PURE" "Pure" "ST" "STATE" "St" "Tot")
              'symbols))

(defconst fstar-syntax-builtins
  (regexp-opt '("requires" "ensures" "modifies" "decreases"
                "effect" "new_effect" "sub_effect")
              'symbols))

(defconst fstar-syntax-types
  (regexp-opt '("Type")
              'symbols))

(defconst fstar-syntax-constants
  (regexp-opt '("False" "True")
              'symbols))

(defface fstar-structure-face
  '((((background light)) (:bold t))
    (t :bold t :foreground "salmon"))
  "Face used to highlight structural keywords."
  :group 'fstar)

(defface fstar-subtype-face
  '((t :inherit font-lock-type-face :italic t))
  "Face used to highlight subtyping clauses."
  :group 'fstar)

(defface fstar-attribute-face
  '((t :italic t))
  "Face used to highlight subtyping clauses."
  :group 'fstar)

(defface fstar-decreases-face
  '((t :italic t))
  "Face used to highlight subtyping clauses."
  :group 'fstar)

(defvar-local should-cancel-match nil)

(defun fstar-group-pre-matcher (prefix-len allowed-newlines)
  "Prepare for highlighting.

Go back PREFIX-LEN chars, skip one sexp forward without moving
point and report that position; point is placed at beginning of
original match. If ALLOWED-NEWLINES is non-nil, then allow the
sexp to span at most that many extra lines."
  (prog1
      (min (save-excursion
             (backward-char prefix-len)
             (forward-sexp)
             (point))
           (save-excursion
             (forward-line (or allowed-newlines 0))
             (point-at-eol)))
    (goto-char (match-beginning 0))))

(defun fstar-subexpr-pre-matcher (rewind-to &optional bound-to)
  (goto-char (match-end rewind-to))
  (match-end (or bound-to 0)))

(defconst fstar-syntax-id (rx symbol-start
                              (? (or "#" "'"))
                              letter (* (or wordchar (syntax symbol)))
                              (? "." (* (or wordchar (syntax symbol))))
                              symbol-end))

(defconst fstar-syntax-cs (rx symbol-start
                              (any "A-Z") (* (or wordchar (syntax symbol)))
                              (? ".v")
                              symbol-end))

(defun fstar-find-id-with-type (bound)
  (let ((start (point)) (found t) (rejected t))
    (while (and found rejected)
      (setq found (re-search-forward (concat "\\(" fstar-syntax-id "\\) *:") bound t))
      (setq rejected (and found (or (eq (char-after) ?:)
                                    (save-excursion
                                      (goto-char (match-beginning 0))
                                      (skip-syntax-backward "-")
                                      (or (eq (char-before) ?|)
                                          (save-match-data
                                            (looking-back "\\_<val" start))))))))
    found))

(defun fstar-id-with-type-pre-matcher ()
  (or (save-excursion
        (ignore-errors
          (goto-char (match-beginning 0))
          (skip-syntax-backward "-")
          (when (eq (char-before) ?\()
            (up-list)
            (point))))
      (save-excursion
        (condition-case nil
            (forward-sexp)
          (error nil (forward-char)))
        (point))))

(defun fstar-find-fun-and-args (bound)
  (let ((found))
    (while (and (not found) (re-search-forward "\\_<fun\\_>" bound t))
      (-when-let* ((mdata (match-data))
                   (fnd   (search-forward "->" bound t)))
        (set-match-data `(,(car mdata) ,(match-end 0)
                          ,@mdata))
        (setq found t)))
    found))

(defun fstar-find-subtype-annotation (bound)
  (let ((found))
    (while (and (not found) (re-search-forward "{[^:]" bound t))
      (setq found
            (save-excursion
              (backward-char)
              (not (looking-at-p "[^ ]+ +with")))))
    found))

(defconst fstar-syntax-additional
  (let ((id fstar-syntax-id))
    `(
      (fstar-find-subtype-annotation
       ("\\({\\)\\(\\(?:.\\|\n\\)+\\)\\(}\\)"
        (fstar-group-pre-matcher 2 2) nil
        (1 'font-lock-type-face append)
        (2 'fstar-subtype-face append)
        (3 'font-lock-type-face append)))
      ("{:" (,(concat "\\({\\)\\(:" id "\\)\\(\\(?: +.+\\)?\\)\\(}\\)")
             (fstar-group-pre-matcher 2 2) nil
             (2 'font-lock-function-name-face append)
             (3 'fstar-attribute-face append)))
      ("%\\[" ("%\\[\\([^]]+\\)\\]"
              (fstar-group-pre-matcher 1 0) nil
              (1 'fstar-decreases-face append)))
      (,(concat "\\_<\\(let\\(?: +rec\\)?\\)\\(\\(?: +" id "\\)?\\) +[^=]+=")
       (1 'fstar-structure-face)
       (2 'font-lock-function-name-face)
       (,id (fstar-subexpr-pre-matcher 2) nil
            (0 'font-lock-variable-name-face)))
      (fstar-find-id-with-type
       (1 'font-lock-variable-name-face)
       (,id (fstar-id-with-type-pre-matcher) nil
            (0 'font-lock-type-face)))
      (,(concat "\\_<\\(type\\|kind\\)\\( +" id "\\)\\s-*$")
       (1 'fstar-structure-face)
       (2 'font-lock-function-name-face))
      (,(concat "\\_<\\(type\\|kind\\)\\( +" id "\\)[^=]+=")
       (1 'fstar-structure-face)
       (2 'font-lock-function-name-face)
       (,id (fstar-subexpr-pre-matcher 2) nil
            (0 'font-lock-variable-name-face)))
      ("\\(forall\\|exists\\) [^.]+\."
       (0 'font-lock-keyword-face)
       (,id (fstar-subexpr-pre-matcher 1) nil
            (0 'font-lock-variable-name-face)))
      (fstar-find-fun-and-args
       (1 'font-lock-keyword-face)
       (,id (fstar-subexpr-pre-matcher 1) nil
            (0 'font-lock-variable-name-face)))
      (,(concat "\\_<\\(val\\) +\\(" id "\\) *:")
       (1 'fstar-structure-face)
       (2 'font-lock-function-name-face)
       (,id nil nil
            (0 'font-lock-type-face)))
      (,fstar-syntax-cs (0 'font-lock-constant-face)))))

;;; Syntax table

(defvar fstar-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?' "_" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?*  ". 23" table)
    (modify-syntax-entry ?/  ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\( "()1n" table)
    (modify-syntax-entry ?\) ")(4n" table)
    table)
  "Syntax table for F*.")

;;; Indentation

(defun fstar-indentation-points ()
  "Find reasonable indentation points on the current line."
  (let ((points)) ;; FIXME first line?
    (save-excursion
      (forward-line -1)
      (while (re-search-forward "\\s-+" (point-at-eol) t)
        (push (current-column) points)))
    (when points
      (push (+ 2 (apply #'min points)) points))
    (push 0 points)
    (push 2 points)
    (-distinct (sort points #'<))))

(defun fstar-indent ()
  "Cycle between vaguely relevant indentation points."
  (interactive)
  (let* ((current-ind (current-indentation))
         (points      (fstar-indentation-points))
         (remaining   (-filter (lambda (x) (> x current-ind)) points))
         (target      (car (or remaining points))))
    (if (> (current-column) current-ind)
        (save-excursion (indent-line-to target))
      (indent-line-to target))))

;;; Main mode

(defun fstar-setup-prettify ()
  "Setup prettify-symbols for use with F*."
  (setq-local prettify-symbols-alist (append fstar-symbols-alist
                                             prettify-symbols-alist))
  (prettify-symbols-mode))

(defun fstar-setup-fontlock ()
  "Setup font-lock for use with F*."
  (setq font-lock-defaults
        `(((,fstar-syntax-constants . 'font-lock-constant-face)
           (,fstar-syntax-types     . 'font-lock-type-face)
           (,fstar-syntax-keywords  . 'font-lock-keyword-face)
           (,fstar-syntax-builtins  . 'font-lock-builtin-face)
           (,fstar-syntax-structure . 'fstar-structure-face)
           ,@fstar-syntax-additional)
          nil nil))
  (font-lock-set-defaults))

(defun fstar-setup-indentation ()
  "Setup indentation for F*."
  (setq-local indent-line-function #'fstar-indent)
  (electric-indent-mode -1))

;;;###autoload
(define-derived-mode fstar-mode prog-mode "F✪"
  :syntax-table fstar-syntax-table
  (fstar-setup-fontlock)
  (fstar-setup-prettify)
  (fstar-setup-indentation)
  (flycheck-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fs[ity]\\'" . fstar-mode))

;;; Footer

(provide 'fstar-mode)
;;; fstar-mode.el ends here
