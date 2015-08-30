;;; fstar-mode.el -- support for the F* language in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015 Clément Pit--Claudel
;; Author: Clément Pit--Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/boogie-org/boogie-friends/

;; Keywords: convenience, languages

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This file implements basic support for programming in F* in Emacs.

;;; Code:

;;; imports

(require 'tuareg) ;; Syntax highlighting
(require 'flycheck) ;; Real-time verification

;;; group

(defgroup fstar nil
  "F* mode."
  :group 'languages)

;;; flycheck

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

;;; prettify-symbols-mode

(defconst fstar-symbols-alist '(("exists" . ?∃) ("forall" . ?∀) ("fun" . ?λ)
                                ("nat" . ?ℕ)
                                ("<=" . ?≤) (">=" . ?≥) ("=!=" . ?≠)
                                ("/\\" . ?∧) ("\\/" . ?∨) ("::" . ?⸬)
                                ("<==>" . ?⟺) ("==>" . ?⟹)
                                ("=>" . ?⇒) ("->" . ?→))
  "Fstar symbols.")

;;; font-lock

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

(defconst fstar-syntax-constants
  (regexp-opt '("False" "True")
              'symbols))

(defface fstar-subtype-face
  '((t :italic t))
  "Face used to highlight subtyping clauses."
  :group 'fstar)

(defface fstar-subtype-brace-face
  '((t :bold t))
  "Face used to highlight braces around subtyping clauses."
  :group 'fstar)

(defun fstar-group-pre-matcher (prefix-len allowed-newlines)
  (backward-char prefix-len)
  (min (save-excursion
         (forward-sexp)
         (point))
       (save-excursion
         (forward-line allowed-newlines)
         (point-at-eol))))

(defconst fstar-syntax-additional
  '(("{" ("\\({\\)\\(\\(?:.\\|\n\\)+\\)\\(}\\)"
          (fstar-group-pre-matcher 1 2)
          nil
          (1 'fstar-subtype-brace-face append)
          (2 'fstar-subtype-face append)
          (3 'fstar-subtype-brace-face append)))
    ("%\\[" ("\\(%\\[\\)\\(.+\\)\\(\\]\\)"
            (fstar-group-pre-matcher 2 0)
            nil
            (1 'fstar-subtype-brace-face append)
            (2 'fstar-subtype-face append)
            (3 'fstar-subtype-brace-face append)))))

;;; fstar-mode

(defun fstar-setup-prettify ()
  "Setup prettify-symbols for use with F*."
  (setq-local prettify-symbols-alist (append fstar-symbols-alist
                                             prettify-symbols-alist))
  (prettify-symbols-mode))

(defun fstar-setup-fontlock ()
  "Setup font-lock for use with F*."
  (font-lock-add-keywords
   nil `((,fstar-syntax-constants . 'font-lock-constant-face)
         (,fstar-syntax-keywords  . 'font-lock-keyword-face)
         (,fstar-syntax-builtins  . 'font-lock-builtin-face)
         (,fstar-syntax-structure . 'tuareg-font-lock-governing-face)
         ,@fstar-syntax-additional)))

(define-derived-mode fstar-mode tuareg-mode "F✪"
  (fstar-setup-prettify)
  (fstar-setup-fontlock))

(add-to-list 'auto-mode-alist '("\\.fst\\'" . fstar-mode))

;;; footer

(provide 'fstar-mode)
;;; fstar-mode.el ends here
