;;; fstar-mode.el --- support for the F* language in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015 Clément Pit--Claudel
;; Author: Clément Pit--Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/FStarLang/fstar.el

;; Created: 27 Aug 2015
;; Version: 0.1
;; Package-Requires: ((flycheck "0.25") (emacs "24.3"))
;; Keywords: convenience, languages

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

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

(flycheck-def-executable-var fstar "fstar.exe")

(defconst fstar-flycheck-patterns
  (let ((fstar-pat '((message) "near line " line ", character " column " in file " (file-name)))
        (z3-pat  '((file-name) "(" line "," column "-" (+ (any digit)) "," (+ (any digit)) ")"
                   (* (any ": ")) (? "Error" (* (any " \n")))
                   (message))))
    `((error "ERROR: " ,@fstar-pat)
      (warning "WARNING: " ,@fstar-pat)
      (error ,@z3-pat))))

(flycheck-define-command-checker 'fstar
  "Flycheck checker for F*."
  :command '("fstar.exe" source-inplace)
  :error-patterns fstar-flycheck-patterns
  :error-filter #'flycheck-increment-error-columns
  :modes '(fstar-mode))

(add-to-list 'flycheck-checkers 'fstar)

;;; Build config

(defconst fstar-build-config-header "(*--build-config")
(defconst fstar-build-config-footer "--*)")

;; (defun fstar-read-build-config ()
;;   (save-excursion
;;     (save-restriction
;;       (widen)
;;       (goto-char (point-min))
;;       (-when-let* ((beg (search-forward fstar-build-config-header nil t))
;;                    (end (search-forward fstar-build-config-footer nil t)))))))

(defun fstar-setup-flycheck ()
  (flycheck-mode))

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
                                ;; ("(|" . 10629) ("|)" . 10630)
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

(defconst fstar-syntax-preprocessor
  (regexp-opt '("#set-options")
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

;; (defconst fstar-syntax-types
;;   (regexp-opt '("Type")
;;               'symbols))

(defconst fstar-syntax-constants
  (regexp-opt '("False" "True")
              'symbols))

(defface fstar-structure-face
  '((((background light)) (:bold t))
    (t :bold t :foreground "salmon"))
  "Face used to highlight structural keywords."
  :group 'fstar)

(defface fstar-subtype-face
  '((t :italic t))
  "Face used to highlight subtyping clauses."
  :group 'fstar)

(defface fstar-attribute-face
  '((t :italic t))
  "Face used to highlight attributes."
  :group 'fstar)

(defface fstar-decreases-face
  '((t :italic t))
  "Face used to highlight decreases clauses."
  :group 'fstar)

(defvar-local should-cancel-match nil)

(defun fstar-group-pre-matcher (prefix-len allowed-newlines)
  "Prepare for highlighting.

Go back PREFIX-LEN chars, skip one sexp forward without moving
point and report that position; point is placed at beginning of
original match.  If ALLOWED-NEWLINES is non-nil, then allow the
sexp to span at most that many extra lines."
  (prog1
      (min (save-excursion
             (backward-char prefix-len)
             (ignore-errors (forward-sexp))
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
                              (any "a-z_") (* (or wordchar (syntax symbol)))
                              (? "." (* (or wordchar (syntax symbol))))
                              symbol-end))

(defconst fstar-syntax-cs (rx symbol-start
                              (any "A-Z") (* (or wordchar (syntax symbol)))
                              symbol-end))

(defun fstar-find-id-with-type (bound)
  (fstar-find-id-maybe-type bound t))

(defun fstar-find-id-maybe-type (bound must-find-type)
  (let ((found t) (rejected t))
    (while (and found rejected)
      (setq found (re-search-forward (concat "\\(" fstar-syntax-id "\\) *\\(:\\)?") bound t))
      (setq rejected (and found (or (eq (char-after) ?:) ; h :: t
                                    (and must-find-type (not (match-beginning 2))) ; no type
                                    (save-excursion
                                      (goto-char (match-beginning 0))
                                      (skip-syntax-backward "-")
                                      (or (eq (char-before) ?|) ; | X: int
                                          (save-match-data
                                            (looking-back "\\_<val" (point-at-bol))))))))) ; val x : Y:int
    (when (and found (match-beginning 2))
      (ignore-errors
        (goto-char (match-end 2))
        (forward-sexp)
        (let ((md (match-data)))
          (set-match-data `(,(car md) ,(point) ,@(cddr md))))))
    found))

(defun fstar-find-id (bound)
  (fstar-find-id-maybe-type bound nil))

(defun fstar-find-fun-and-args (bound)
  (let ((found))
    (while (and (not found) (re-search-forward "\\_<fun\\_>" bound t))
      (-when-let* ((mdata (match-data))
                   (fnd   (search-forward "->" bound t))) ;;FIXME
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
       ("{\\(\\(?:.\\|\n\\)+\\)}"
        (fstar-group-pre-matcher 2 5) nil
        (1 'fstar-subtype-face append)))
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
       (fstar-find-id (fstar-subexpr-pre-matcher 2) nil
                      (1 'font-lock-variable-name-face)))
      (fstar-find-id-with-type
       (1 'font-lock-variable-name-face))
      (,(concat "\\_<\\(type\\|kind\\)\\( +" id "\\)\\s-*$")
       (1 'fstar-structure-face)
       (2 'font-lock-function-name-face))
      (,(concat "\\_<\\(type\\|kind\\)\\( +" id "\\)[^=]+=")
       (1 'fstar-structure-face)
       (2 'font-lock-function-name-face)
       (fstar-find-id (fstar-subexpr-pre-matcher 2) nil
                      (1 'font-lock-variable-name-face)))
      ("\\_<\\(forall\\|exists\\) [^.]+\."
       (0 'font-lock-keyword-face)
       (fstar-find-id (fstar-subexpr-pre-matcher 1) nil
                      (1 'font-lock-variable-name-face)))
      (fstar-find-fun-and-args
       (1 'font-lock-keyword-face)
       (fstar-find-id (fstar-subexpr-pre-matcher 1) nil
                      (1 'font-lock-variable-name-face)))
      (,(concat "\\_<\\(val\\) +\\(" id "\\) *:")
       (1 'fstar-structure-face)
       (2 'font-lock-function-name-face))
      (,fstar-syntax-cs (0 'font-lock-constant-face))
      ("(\\*--\\(build-config\\)"
       (1 'font-lock-preprocessor-face prepend)))))

(defun fstar-setup-font-lock ()
  "Setup font-lock for use with F*."
  (setq font-lock-defaults
        `(((,fstar-syntax-constants    . 'font-lock-constant-face)
           (,fstar-syntax-keywords     . 'font-lock-keyword-face)
           (,fstar-syntax-builtins     . 'font-lock-builtin-face)
           (,fstar-syntax-preprocessor . 'font-lock-preprocessor-face)
           (,fstar-syntax-structure    . 'fstar-structure-face)
           ,@fstar-syntax-additional)
          nil nil))
  (font-lock-set-defaults))

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

(defun fstar-setup-indentation ()
  "Setup indentation for F*."
  (setq-local indent-line-function #'fstar-indent)
  (electric-indent-local-mode -1))

;;; Comment syntax

(defun fstar-syntactic-face-function-aux (_ _ _ in-string comment-depth _ _ _ comment-start-pos _)
  (cond (in-string font-lock-string-face)
        ((and comment-depth
              comment-start-pos
              (numberp comment-depth))
         (save-excursion
           (goto-char comment-start-pos)
           (cond
            ((looking-at (regexp-quote fstar-build-config-header)) font-lock-doc-face)
            ((looking-at (regexp-quote "(*** ")) '(:inherit font-lock-doc-face :height 2.5))
            ((looking-at (regexp-quote "(**+ ")) '(:inherit font-lock-doc-face :height 1.8))
            ((looking-at (regexp-quote "(**! ")) '(:inherit font-lock-doc-face :height 1.5))
            ((looking-at (regexp-quote "(** "))  font-lock-doc-face)
            (t font-lock-comment-face))))))

(defun fstar-syntactic-face-function (args)
  (apply #'fstar-syntactic-face-function-aux args))

(defun fstar-setup-comments ()
  (setq-local comment-start      "(*")
  (setq-local comment-end        "*)")
  (setq-local comment-start-skip "\\s-*(\\*+\\s-**")
  (setq-local comment-end-skip   "\\s-*\\*+)\\s-*")
  (setq-local font-lock-syntactic-face-function #'fstar-syntactic-face-function))

;;; Main mode

(defcustom fstar-enabled-modules
  '(font-lock prettify indentation comments flycheck)
  "Which F* mode modules to load."
  :group 'fstar)

;;;###autoload
(define-derived-mode fstar-mode prog-mode "F✪"
  :syntax-table fstar-syntax-table
  (dolist (module fstar-enabled-modules)
    (funcall (intern (concat "fstar-setup-" (symbol-name module))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fs[ity]\\'" . fstar-mode))

;;; Footer

(provide 'fstar-mode)
;;; fstar-mode.el ends here
