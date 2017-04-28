;;; fstar-mode.el --- Support for F* programming -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Clément Pit-Claudel
;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/FStarLang/fstar.el

;; Created: 27 Aug 2015
;; Version: 0.4
;; Package-Requires: ((emacs "24.3") (dash "2.11") (company "0.8.12") (quick-peek "1.0") (yasnippet "0.11.0")  (flycheck "30.0"))
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

;; This file implements support for F* programming in Emacs, including:
;;
;; * Syntax highlighting
;; * Unicode math (prettify-symbols-mode)
;; * Documentation and search
;; * Relative indentation
;; * Type hints (Eldoc)
;; * Autocompletion (Company)
;; * Type-aware snippets (Yasnippet)
;; * Interactive proofs (à la Proof-General)
;; * Real-time verification (Flycheck)
;; * Remote editing (Tramp)
;;
;; See https://github.com/FStarLang/fstar-mode.el for setup and usage tips.

;;; Code:

;;; Imports

(require 'cl-lib)
(require 'eldoc)
(require 'help-at-pt)
(require 'ansi-color)
(require 'easymenu)
(require 'tramp)
(require 'tramp-sh)
(require 'crm)
;; replace.el doesn't `provide' in Emacs < 26
(ignore-errors (require 'replace))

(require 'dash)
(require 'company)
(require 'quick-peek)
(require 'yasnippet)
(require 'flycheck)
(require 'let-alist)
(require 'company-quickhelp nil t)

(defconst fstar--script-full-path
  (or (and load-in-progress load-file-name)
      (bound-and-true-p byte-compile-current-file)
      (buffer-file-name))
  "Full path of this script.")

(defconst fstar--directory
  (file-name-directory fstar--script-full-path)
  "Full path to directory of this script.")

;;; Compatibility with older Emacsen

(defmacro fstar-assert (&rest args)
  "Call `cl-assert' on ARGS, if available."
  (declare (debug t))
  `(when (fboundp 'cl-assert)
     (cl-assert ,@args)))

(eval-and-compile
  (unless (featurep 'subr-x)
    (defsubst string-trim-left (string)
      "Remove leading whitespace from STRING."
      (if (string-match "\\`[ \t\n\r]+" string)
          (replace-match "" t t string)
        string))
    (defsubst string-trim-right (string)
      "Remove trailing whitespace from STRING."
      (if (string-match "[ \t\n\r]+\\'" string)
          (replace-match "" t t string)
        string))))

(defun fstar--string-trim (s)
  "Trim S, or return nil if nil."
  (when s (string-trim-left (string-trim-right s))))

;;; Customization

(defgroup fstar nil
  "F* mode."
  :group 'languages)

(defcustom fstar-executable "fstar.exe"
  "Where to find F*.
May be either “fstar.exe” (search in $PATH) or an absolute path."
  :group 'fstar
  :type 'file
  :risky t)

(defcustom fstar-smt-executable "z3"
  "Where to find the SML solver.
May be one of “z3” (search in $PATH), an absolute path, or a
function taking a string (the full path to the F* executable) and
returning a string (the full path to the SMT solver)."
  :group 'fstar
  :type 'file
  :risky t)

(defconst fstar-known-modules
  '((font-lock        . "Syntax highlighting")
    (prettify         . "Unicode math (e.g. display forall as ∀; requires emacs 24.4 or later)")
    (indentation      . "Indentation (relative to previous lines)")
    (comments         . "Comment syntax and special comments ('(***', '(*+', etc.)")
    (flycheck         . "Real-time verification with Flycheck")
    (interactive      . "Interactive verification (à la Proof-General)")
    (eldoc            . "Type annotations in the minibuffer.")
    (company          . "Completion with company-mode.")
    (company-defaults . "Opinionated company-mode configuration.")
    (spinner          . "Blink the modeline while F* is busy.")
    (overlay-legend   . "Show a legend in the modeline when hovering an F* overlay."))
  "Available components of F*-mode.")

(defcustom fstar-enabled-modules
  '(font-lock prettify indentation comments flycheck interactive
              eldoc company company-defaults spinner overlay-legend)
  "Which F*-mode components to load."
  :group 'fstar
  :type `(set ,@(cl-loop for (mod . desc) in fstar-known-modules
                         collect `(const :tag ,desc ,mod))))

;;; Utilities

(defun fstar--indent-str (str amount &optional skip-first-line)
  "Indent all lines of STR by AMOUNT.
If AMOUNT is a string, use that as indentation.
With SKIP-FIRST-LINE, don't indent the first one."
  (let ((indent (if (stringp amount) amount (make-string amount ?\s))))
    (if skip-first-line
        (replace-regexp-in-string "\n" (concat "\n" indent) str)
      (replace-regexp-in-string "^" indent str))))

(defun fstar--property-range (pos prop)
  "Find range around POS with consistent non-nil value of PROP."
  (let* ((val-1 (get-text-property (1- pos) prop))
         (val (get-text-property pos prop)))
    (when (or val val-1)
      (list (if (eq (or val val-1) val-1)
                (previous-single-property-change pos prop) pos)
            (if val
                (next-single-property-change pos prop) pos)
            (or val val-1)))))

(defun fstar--expand-snippet (&rest args)
  "Ensure that Yasnippet is on and forward ARGS to `yas-expand-snippet'."
  (unless yas-minor-mode (yas-minor-mode 1))
  (apply #'yas-expand-snippet args))

(defun fstar--refresh-eldoc ()
  "Tell Eldoc to do its thing.
It's often incorrect to add commands to `eldoc-message-commands',
as that leads to outdated information being displayed when the
command is asynchronous.  For example, adding
`fstar-insert-match-dwim' to `eldoc-message-commands' causes
eldoc to show the type of the hole *before* destruction, not
after."
  (eldoc-print-current-symbol-info))

(defun fstar--unwrap-paragraphs (str)
  "Remove hard line wraps from STR."
  (replace-regexp-in-string " *\n\\([^\n\t ]\\)" " \\1" str))

(defun fstar--resolve-fn-value (fn-or-v)
  "Return FN-OR-V, or the result of calling it if it's a function."
  (if (functionp fn-or-v)
      (funcall fn-or-v)
    fn-or-v))

(defun fstar--remote-p ()
  "Check if current buffer is remote."
  (and buffer-file-name (tramp-tramp-file-p buffer-file-name)))

(defun fstar--maybe-cygpath (path)
  "Translate PATH using Cygpath if appropriate."
  (if (and (eq system-type 'cygwin) (not (fstar--remote-p)))
      (string-trim-right (car (process-lines "cygpath" "-w" path)))
    path))

(defun fstar--make-tramp-file-name (fname)
  "Convert FNAME to a name on remote host."
  (with-parsed-tramp-file-name buffer-file-name nil
    (tramp-make-tramp-file-name method user host fname)))

(defun fstar--tramp-find-executable (prog)
  "Check if PROG is in the remote path."
  (with-parsed-tramp-file-name buffer-file-name vec
    (tramp-find-executable vec prog nil)))

(defun fstar--hide-buffer (buf)
  "Hide window displaying BUF, if any.
Return value indicates whether a window was hidden."
  (-when-let* ((doc-wins (and (buffer-live-p (get-buffer buf))
                              (get-buffer-window-list buf nil t))))
    (mapc (apply-partially #'quit-window nil) doc-wins)
    t))

(defun fstar--read-string (prompt default)
  "Read a string with PROMPT and DEFAULT.
Prompt should have one string placeholder to accommodate DEFAULT."
  (let ((default-info (if default (format " (default ‘%s’)" default) "")))
    (setq prompt (format prompt default-info)))
  (read-string prompt nil nil default))

(defun fstar--syntax-ppss (pos)
  "Like `syntax-ppss' at POS, but don't move point."
  (save-excursion (syntax-ppss pos)))

(defun fstar-in-comment-p (&optional pos)
  "Return non-nil if POS is inside a comment."
  (nth 4 (fstar--syntax-ppss pos)))

(defun fstar--column-in-commment-p (column)
  "Return non-nil if point at COLUMN is inside a comment."
  (save-excursion
    (move-to-column column)
    (fstar-in-comment-p)))

(defun fstar-subp--column-number-at-pos (pos)
  "Return column number at POS."
  (save-excursion (goto-char pos) (- (point) (point-at-bol))))

(defun fstar--goto-line-col (line column)
  "Go to position indicated by LINE, COLUMN."
  (goto-char (point-min))
  (forward-line (1- line))
  ;; min makes sure that we don't spill to the next line.
  (forward-char (min (- (point-at-eol) (point-at-bol)) column)))

(defun fstar--line-col-offset (line column)
  "Convert a (LINE, COLUMN) pair into a buffer offset."
  ;; LATER: This would be much easier if the interactive mode returned
  ;; an offset instead of a line an column.
  (save-excursion
    (fstar--goto-line-col line column)
    (point)))

(defun fstar--match-strings-no-properties (ids &optional str)
  "Get (match-string-no-properties ID STR) for each ID in IDS."
  (mapcar (lambda (num) (match-string-no-properties num str)) ids))

(defvar fstar--fqn-at-point-syntax-table)

(defun fstar--fqn-at-point (&optional pos)
  "Return symbol at POS (default: point)."
  (setq pos (or pos (point)))
  (with-syntax-table fstar--fqn-at-point-syntax-table
    (save-excursion
      (goto-char pos)
      (-when-let* ((s (symbol-at-point)))
        (substring-no-properties (symbol-name s))))))

(defun fstar--propertize-title (title)
  "Format TITLE as a title."
  (propertize title 'face '(:height 1.5)))

(defun fstar--push-mark ()
  "Save current position in mark ring and xref stack."
  (push-mark nil t)
  (when (fboundp 'xref-push-marker-stack)
    (xref-push-marker-stack)))

(defun fstar--navigate-to (fname line col display-action)
  "Navigate to LINE, COL of FNAME.
DISPLAY-ACTION determines where the resulting buffer is
shown (nil for same window, `window' for a new window, and
`frame' for a new frame)."
  (fstar--push-mark)
  (catch 'not-found
    (unless (file-exists-p fname)
      (message "File not found: %S" fname)
      (throw 'not-found nil))
    (pcase display-action
      (`nil (find-file fname))
      (`window (find-file-other-window fname))
      (`frame (find-file-other-frame fname)))
    (fstar--goto-line-col line col)
    (recenter)
    (when (fboundp 'pulse-momentary-highlight-one-line)
      (pulse-momentary-highlight-one-line (point)))))

(defun fstar--visit-link-target (marker)
  "Jump to file indicated by entry at MARKER."
  (find-file (get-text-property (marker-position marker)
                                'fstar--target (marker-buffer marker))))

(defun fstar--lispify-null (x)
  "Return X, or nil if X is `:json-null'."
  (unless (eq x :json-null) x))

;;; Debugging

(defvar fstar-debug nil
  "If non-nil, print debuging information in interactive mode.")

(defun fstar-toggle-debug ()
  "Toggle `fstar-debug'."
  (interactive)
  (message "F*: Debugging %s."
           (if (setq-default fstar-debug (not fstar-debug)) "enabled" "disabled"))
  (when fstar-debug
    (display-buffer (fstar--log-buffer))))

(define-obsolete-variable-alias 'fstar-subp-debug 'fstar-debug "0.4")
(define-obsolete-function-alias 'fstar-subp-toggle-debug 'fstar-toggle-debug "0.4")

(defconst fstar--log-buffer-keywords
  '(("!!!" . font-lock-warning-face)
    (">>>" . font-lock-constant-face)
    ("\\(;;;\\)\\(.*\\)"
     (1 font-lock-constant-face prepend)
     (2 font-lock-comment-face prepend))
    ("^#done-n?ok" . font-lock-builtin-face)))

(defun fstar--log-buffer ()
  "Get or create log buffer."
  (or (get-buffer "*fstar-debug*")
      (with-current-buffer (get-buffer-create "*fstar-debug*")
        (setq-local truncate-lines t)
        (setq-local window-point-insertion-type t)
        (setq-local font-lock-defaults '(fstar--log-buffer-keywords))
        (font-lock-mode 1)
        (buffer-disable-undo)
        (current-buffer))))

(defun fstar--log (kind format &rest args)
  "Log a message of kind KIND, conditional on `fstar-debug'.

FORMAT and ARGS are as in `message'.
LEVEL is one of `info', `input', `output'."
  (with-current-buffer (fstar--log-buffer)
    (goto-char (point-max))
    (let* ((raw (apply #'format format args))
           (head (cdr (assq kind '((info . ";;; ") (in . ">>> ")
                                   (warning . "!!! ") (out . ""))))))
      (fstar-assert head)
      (insert (fstar--indent-str raw head)
              (if (eq kind 'out) "" "\n")))))

(defmacro fstar-log (kind format &rest args)
  "Log a message of kind KIND, conditional on `fstar-debug'.

FORMAT and ARGS are as in `message'."
  (declare (debug t))
  `(when fstar-debug
     (fstar--log ,kind ,format ,@args)))

(defun fstar--write-transcript-1 (fname lines)
  "Write (nreverse LINES) to FNAME."
  (with-temp-buffer
    (dolist (line (nreverse lines))
      (insert line "\n"))
    (write-region (point-min) (point-max) fname)))

(defun fstar-write-transcript (prefix)
  "Write latest transcript to a PREFIX.IN and PREFIX.OUT."
  (interactive "FSave transcript as: ")
  (unless fstar-debug
    (user-error "Use `fstar-toggle-debug' to collect traces"))
  (with-current-buffer (fstar--log-buffer)
    (goto-char (point-max))
    (unless (re-search-backward "^;;; \n" nil t)
      (user-error "Could not find a complete transcript to save"))
    (let ((log (buffer-substring-no-properties (match-end 0) (point-max)))
          (exclude-re (concat "^" (regexp-opt '(">>> " "!!! " ";;; "))))
          (in nil) (out nil))
      (dolist (line (delete "" (split-string log "\n")))
        (cond
         ((string-match "^>>> " line)
          (push (substring line (match-end 0)) in))
         ((not (string-match-p exclude-re line))
          (unless (equal line "") (push line out)))))
      (fstar--write-transcript-1 (concat prefix ".in") in)
      (fstar--write-transcript-1 (concat prefix ".out") out))))

;;; Compatibility across F* versions

(defvar-local fstar--vernum nil
  "F*'s version number.")

(defcustom fstar-assumed-vernum "0.9.4.2"
  "Version number to assume if F* returns an unknown version number.
This is useful when running an F#-compiled F*.  Use 42.0 to
enable all experimental features."
  :group 'fstar
  :type 'string)

(defconst fstar--features-min-version-alist
  '((absolute-linums-in-errors . "0.9.3.0-beta2")
    (lookup . "0.9.4.1")
    (info-includes-symbol . "0.9.4.2")
    (autocomplete . "0.9.4.2")
    (json-subp . "0.9.4.3")))

(defvar-local fstar--features nil
  "List of available F* features.")

(defun fstar--query-vernum (executable)
  "Ask F* EXECUTABLE for its version number."
  (with-temp-buffer
    (process-file executable nil t nil "--version")
    (buffer-string)))

(defun fstar--init-compatibility-layer (executable)
  "Adjust compatibility settings based on EXECUTABLE's version number."
  (let* ((version-string (fstar--query-vernum executable)))
    (if (string-match "^F\\* \\([- .[:alnum:]]+\\)" version-string)
        (setq fstar--vernum (match-string 1 version-string))
      (message "F*: Can't parse version number from %S; assuming %s"
               version-string fstar-assumed-vernum)
      (setq fstar--vernum "unknown")))
  (let ((vernum (if (equal fstar--vernum "unknown") fstar-assumed-vernum fstar--vernum)))
    (pcase-dolist (`(,feature . ,min-version) fstar--features-min-version-alist)
      (when (version<= min-version vernum)
        (push feature fstar--features)))))

(defun fstar--has-feature (feature &optional error-fn)
  "Check if FEATURE is available in the current F*.
If not, call ERROR-FN if supplied with a relevant message."
  (or (null fstar--vernum)
      (memq feature fstar--features)
      (ignore
       (and error-fn
            (funcall error-fn "This feature isn't available \
in your version of F*.  You're running version %s" fstar--vernum)))))

;;; Whole-buffer Flycheck

(defcustom fstar-flycheck-checker 'fstar-interactive
  "Which Flycheck checker to use in F*-mode."
  :group 'fstar
  :type '(choice (const :tag "No Flycheck support" nil)
                 (const :tag "Whole-buffer verification (slow)" 'fstar)
                 (const :tag "Lightweight typechecking (fast)" 'fstar-interactive)))

(make-variable-buffer-local 'fstar-flycheck-checker)

(defconst fstar-error-patterns
  (let ((fstar-pat '((message) "near line " line ", character " column " in file " (file-name)))
        (z3-pat  '((file-name) "(" line "," column "-" (+ (any digit)) "," (+ (any digit)) ")"
                   (* (any ": ")) (? "Error" (* (any " \n")))
                   (message))))
    `((error "ERROR: " ,@fstar-pat)
      (warning "WARNING: " ,@fstar-pat)
      (error ,@z3-pat))))

(defvaralias 'flycheck-fstar-executable 'fstar-executable)
(make-obsolete-variable 'flycheck-fstar-executable 'fstar-executable "0.2" 'set)

(defun fstar--flycheck-enabled (checker)
  "Check whether CHECKER should be used."
  (eq fstar-flycheck-checker checker))

(defun fstar--flycheck-verify-enabled (checker)
  "Create a verification result announcing whether CHECKER is enabled."
  (list
   (flycheck-verification-result-new
    :label "Checker selection"
    :message (if (fstar--flycheck-enabled checker) "OK, checker selected."
               (format "Set ‘fstar-flycheck-checker’ \
to ‘%S’ to use this checker." checker))
    :face (if (fstar--flycheck-enabled checker) 'success '(bold error)))))

(defun fstar--flycheck-verify-subp-available ()
  "Create a verification result announcing availability of the F* subprocess."
  (list
   (flycheck-verification-result-new
    :label "Subprocess running"
    :message (if (fstar-subp-live-p) "OK, F* started."
               "This checker requires a running F* subprocess.")
    :face (if (fstar-subp-live-p) 'success '(bold error)))
   (flycheck-verification-result-new
    :label "Subprocess started"
    :message (if (fstar-subp-available-p) "OK, F* idle."
               "This checker only runs when the F* subprocess is idle.")
    :face (if (fstar-subp-available-p) 'success '(bold error)))))

(flycheck-define-command-checker 'fstar
  "Flycheck checker for F*."
  :command '("fstar.exe" source-inplace)
  :error-patterns fstar-error-patterns
  :error-filter #'flycheck-increment-error-columns
  :modes '(fstar-mode)
  :verify #'fstar--flycheck-verify-enabled
  :predicate (apply-partially #'fstar--flycheck-enabled 'fstar))

(add-to-list 'flycheck-checkers 'fstar)

(defun fstar-setup-flycheck ()
  "Prepare Flycheck for use with F*."
  (flycheck-mode 1))

;;; Prettify symbols

(defcustom fstar-symbols-alist '(("exists" . ?∃) ("forall" . ?∀) ("fun" . ?λ)
                            ("nat" . ?ℕ) ("int" . ?ℤ)
                            ("True" . ?⊤) ("False" . ?⊥)
                            ("*" . ?×) (":=" . ?≔) ("::" . ?⸬)
                            ("<=" . ?≤) (">=" . ?≥) ("<>" . ?≠)
                            ("/\\" . ?∧) ("\\/" . ?∨) ("~" . ?¬)
                            ("<==>" . ?⟺) ("==>" . ?⟹) ;; ("<==" . ?⟸)
                            ("->" . ?→) ("~>" . ?↝) ("=>" . ?⇒)
                            ("<-" . ?←) ("<--" . ?⟵)
                            ("<<" . ?≪) ;; (">>" . ?≫)
                            ("<|" . ?◃) ("|>" . ?▹) ;; ("<||" . ?⧏) ("||>" . ?⧐)
                            ;; ("(|" . ?⦅) ("|)" . ?⦆)
                            ("'a" . ?α) ("'b" . ?β) ("'c" . ?γ)
                            ("'d" . ?δ) ("'e" . ?ε) ("'f" . ?φ)
                            ("'g" . ?χ) ("'h" . ?η) ("'i" . ?ι)
                            ("'k" . ?κ) ("'m" . ?μ) ("'n" . ?ν)
                            ("'p" . ?π) ("'q" . ?θ) ("'r" . ?ρ)
                            ("'s" . ?σ) ("'t" . ?τ) ("'u" . ?ψ)
                            ("'w" . ?ω) ("'x" . ?ξ) ("'z" . ?ζ))
  "Fstar symbols."
  :group 'fstar
  :type 'alist)

(defun fstar-setup-prettify ()
  "Setup prettify-symbols for use with F*."
  (when (and (boundp 'prettify-symbols-alist)
             (fboundp 'prettify-symbols-mode))
    (setq-local prettify-symbols-alist (append fstar-symbols-alist
                                               prettify-symbols-alist))
    (prettify-symbols-mode)))

;;; Font-Lock

(defconst fstar-syntax-structural-headers
  '("open" "module" "include"
    "let" "let rec" "val" "and"
    "effect" "new_effect" "sub_effect" "new_effect_for_free"
    "kind" "type"))

(defconst fstar-syntax-preprocessor-directives
  '("#set-options" "#reset-options"))

(defconst fstar-syntax-headers
  `(,@fstar-syntax-preprocessor-directives ,@fstar-syntax-structural-headers))

(defconst fstar-syntax-structural-qualifiers
  '("new" "abstract" "logic" "assume" "visible"
    "unfold" "irreducible" "inline_for_extraction" "noeq" "noextract"
    "private" "opaque" "total" "default" "reifiable" "reflectable"))

(defconst fstar-syntax-qualifiers
  `("assume" ,@fstar-syntax-structural-qualifiers))

(defconst fstar-syntax-block-header-re
  (format "^\\(?:%s \\)*%s "
          (regexp-opt fstar-syntax-qualifiers)
          (regexp-opt fstar-syntax-headers))
  "Regexp matching headers to display in the outline buffer.")

(defconst fstar-syntax-block-start-re
  (format "^\\(?:%s \\)*%s "
          (regexp-opt fstar-syntax-qualifiers)
          (regexp-opt (remove "and" fstar-syntax-headers)))
  "Regexp matching starts of semantic blocks.")

(defconst fstar-syntax-structure
  (regexp-opt `("begin" "end" "in"
                ,@fstar-syntax-structural-headers
                ,@fstar-syntax-structural-qualifiers)
              'symbols))

(defconst fstar-syntax-preprocessor
  (regexp-opt fstar-syntax-preprocessor-directives 'symbols))

(defconst fstar-syntax-keywords
  (regexp-opt '("of"
                "forall" "exists"
                "assert" "assert_norm" "assume"
                "fun" "function"
                "try" "match" "with"
                "if" "then" "else"
                "ALL" "All" "DIV" "Div" "EXN" "Ex" "Exn" "GHOST" "GTot" "Ghost"
                "Lemma" "PURE" "Pure" "Tot" "ST" "STATE" "St"
                "Unsafe" "Stack" "Heap" "StackInline" "Inline")
              'symbols))

(defconst fstar-syntax-builtins
  (regexp-opt '("requires" "ensures" "modifies" "decreases" "attributes"
                "effect_actions")
              'symbols))

(defconst fstar-syntax-ambiguous
  (regexp-opt `("\\/" "/\\")))

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

(defface fstar-subscript-face
  '((t :height 1.0))
  "Face used to highlight subscripts"
  :group 'fstar)

(defface fstar-braces-face
  '((t))
  "Face to use for { and }."
  :group 'fstar)

(defface fstar-dereference-face
  '((t :inherit font-lock-negation-char-face))
  "Face to use for !."
  :group 'fstar)

(defface fstar-ambiguous-face
  '((t :inherit font-lock-negation-char-face))
  "Face to use for /\\ and \\/."
  :group 'fstar)

(defface fstar-universe-face
  '((t :foreground "forest green"))
  "Face used for universe levels and variables"
  :group 'fstar)

(defface fstar-operator-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for backticked infix operators."
  :group 'fstar)

(defconst fstar-comment-start-skip "\\(//+\\|(\\*+\\)[ \t]*")

(defconst fstar-syntax-id "\\_<[#']?[a-z_]\\(?:\\sw\\|\\s_\\)*\\_>")
(defconst fstar-syntax-cs "\\_<[#']?[A-Z]\\(?:\\sw\\|\\s_\\)*\\_>")

(defconst fstar-syntax-id-with-subscript
  "\\_<[#']?_*[a-z]\\(?:[a-z0-9_']*[a-z]\\)?\\([0-9]+\\)['_]*\\_>")

(defconst fstar-syntax-universe-id-unwrapped (rx "'u" (* (or wordchar (syntax symbol)))))

(defconst fstar-syntax-universe-id (concat "\\_<" fstar-syntax-universe-id-unwrapped "\\_>"))

(defconst fstar-syntax-universe (concat "\\(" fstar-syntax-universe-id "\\|u#([^)]*)\\)"))

(defconst fstar-syntax-ids (concat "\\(" fstar-syntax-id "\\(?: +" fstar-syntax-id "\\)*\\)"))

(defconst fstar-syntax-ids-and-type (concat fstar-syntax-ids " *:"))

(defun fstar-find-id-maybe-type (bound must-find-type &optional extra-check)
  "Find var:type or var1..varN:type pair between point and BOUND.

If MUST-FIND-TYPE is nil, the :type part is not necessary.
If EXTRA-CHECK is non-nil, it is used as an extra filter on matches."
  (let ((found t) (rejected t)
        (regexp (if must-find-type fstar-syntax-ids-and-type fstar-syntax-ids)))
    (while (and found rejected)
      (setq found (re-search-forward regexp bound t))
      (setq rejected (and found (or (eq (char-after) ?:) ; h :: t
                                    (save-excursion
                                      (goto-char (match-beginning 0))
                                      (skip-syntax-backward "-")
                                      (or (and extra-check
                                               (not (funcall extra-check)))
                                          (eq (char-before) ?|) ;; | X: int
                                          (save-match-data
                                            ;; val x : Y:int
                                            (re-search-forward "\\_<\\(val\\|let\\|type\\)\\_>"
                                                               (match-end 0) t))))))))
    (when (and found must-find-type)
      (ignore-errors
        (goto-char (match-end 0))
        (forward-sexp)
        (let ((md (match-data)))
          (set-match-data `(,(car md) ,(point) ,@(cddr md))))))
    found))

(defvar-local fstar--font-lock-anchor nil)

(defun fstar--beginning-of-sexp (pos)
  "Find beginning of sexp enclosing POS."
  (or (nth 1 (fstar--syntax-ppss pos)) 1))

(defun fstar-subexpr-pre-matcher (rewind-to &optional bound-to)
  "Move past REWIND-TO th group, then return end of BOUND-TO th.
This also records the beginning of enclosing sexp in
`fstar--font-lock-anchor'."
  (setq-local fstar--font-lock-anchor (fstar--beginning-of-sexp (match-beginning 0)))
  (goto-char (match-end rewind-to))
  (match-end (or bound-to 0)))

(defun fstar--directly-under-anchor ()
  "Check whether point is directly under a binder.
This works by moving up in the sexp tree and checking that this
leads to the binder's start."
  (= (fstar--beginning-of-sexp (point)) (or fstar--font-lock-anchor 1)))

(defun fstar--find-formal (bound)
  "Find unparenthesized variable name between point and BOUND."
  (fstar-find-id-maybe-type bound nil #'fstar--directly-under-anchor))

(defun fstar-find-id-with-type (bound)
  "Find var:type pair between point and BOUND."
  (fstar-find-id-maybe-type bound t))

(defun fstar--search-forward-in-sexp (needle bound)
  "Find next NEEDLE at current level before BOUND or end of current sexp."
  (let ((found t) (rejected t)
        (fstar--font-lock-anchor (fstar--beginning-of-sexp (point))))
    (save-excursion
      (while (and found rejected)
        (setq found (search-forward needle bound t))
        (setq rejected (and found (not (fstar--directly-under-anchor))))))
    (when found (goto-char found))))

(defun fstar--find-head-and-args (head tail bound)
  "Find HEAD .. TAIL expression between point and BOUND."
  (let ((found))
    (while (and (not found) (re-search-forward head bound t))
      (-when-let* ((mdata (match-data))
                   (fnd (fstar--search-forward-in-sexp tail (min bound (point-at-eol)))))
        (set-match-data `(,(car mdata) ,(match-end 0) ,@mdata))
        (setq found t)))
    found))

(defun fstar--find-fun-and-args (bound)
  "Find lambda expression between point and BOUND."
  (fstar--find-head-and-args "\\_<fun\\_>" "->" bound))

(defun fstar--find-quantifier-and-args (bound)
  "Find quantified expression between point and BOUND."
  (fstar--find-head-and-args "\\_<\\(?:forall\\|exists\\)\\_>" "." bound))

(defun fstar-find-subtype-annotation (bound)
  "Find {...} group between point and BOUND."
  (let ((found) (end))
    (while (and (not found) (re-search-forward "{[^:].*}" bound t))
      (setq end (save-excursion
                  (goto-char (match-beginning 0))
                  (ignore-errors (forward-sexp))
                  (point)))
      (setq found (and (<= end bound)
                       (<= end (point-at-eol))
                       (memq (char-syntax (char-before (match-beginning 0))) '(?w ?_)))))
    (when found
      (set-match-data `(,(1+ (match-beginning 0)) ,(1- end))))
    found))

(defconst fstar-syntax-additional
  (let ((id fstar-syntax-id))
    `((,fstar-syntax-cs
       (0 'font-lock-type-face))
      (,fstar-syntax-universe
       (1 'fstar-universe-face))
      ("`.+?`"
       (0 'fstar-operator-face append))
      ("^[[:space:]]*\\(@summary\\)\\_>"
       (1 'font-lock-constant-face prepend))
      (,(concat "{\\(:" id "\\) *\\([^}]*\\)}")
       (1 'font-lock-builtin-face append)
       (2 'fstar-attribute-face append))
      (,(concat "\\_<\\(let\\(?: +rec\\)?\\|and\\)\\_>\\(\\(?: +" id "\\( *, *" id "\\)*\\)?\\)")
       (1 'fstar-structure-face)
       (2 'font-lock-function-name-face))
      (,(concat "\\_<\\(type\\|kind\\)\\( +" id "\\)")
       (1 'fstar-structure-face)
       (2 'font-lock-function-name-face))
      (,(concat "\\_<\\(val\\) +\\(" id "\\) *:")
       (1 'fstar-structure-face)
       (2 'font-lock-function-name-face))
      (fstar-find-id-with-type
       (1 'font-lock-variable-name-face append))
      (fstar-find-subtype-annotation
       (0 'fstar-subtype-face append))
      ("%\\[\\([^]]+\\)\\]"
       (1 'fstar-decreases-face append))
      (fstar--find-quantifier-and-args
       (1 'font-lock-keyword-face)
       (fstar--find-formal (fstar-subexpr-pre-matcher 1) nil (1 'font-lock-variable-name-face append)))
      (fstar--find-fun-and-args
       (1 'font-lock-keyword-face)
       (fstar--find-formal (fstar-subexpr-pre-matcher 1) nil (1 'font-lock-variable-name-face append)))
      (,fstar-syntax-ambiguous
       (0 'fstar-ambiguous-face append))
      ("!"
       (0 'fstar-dereference-face))
      ("[{}]"
       (0 'fstar-braces-face append))
      (,fstar-syntax-id-with-subscript
       (1 '(face fstar-subscript-face display (raise -0.3)) append)))))

(defconst fstar--scratchpad-name " *%s-scratchpad*")

(defvar-local fstar--scratchpad nil
  "Temporary buffer used to highlight strings.")

(defun fstar--init-scratchpad ()
  "Get or create scratchpad buffer of current F* buffer."
  (unless (buffer-live-p fstar--scratchpad)
    (setq fstar--scratchpad
          (get-buffer-create (format fstar--scratchpad-name (buffer-name))))
    (with-current-buffer fstar--scratchpad
      (setq-local fstar-enabled-modules '(font-lock prettify))
      (fstar-mode))))

(defun fstar--cleanup-type (type)
  "Clean up TYPE."
  (replace-regexp-in-string "\\(?:uu___:\\|[@#][0-9]+\\_>\\)" "" type t t))

(defun fstar--unparens (str)
  "Remove parentheses surrounding STR, if any."
  (if (and str
           (> (length str) 2)
           (eq (aref str 0) ?\()
           (eq (aref str (1- (length str))) ?\)))
      (substring str 1 (- (length str) 1))
    str))

(defun fstar-highlight-string (str)
  "Highlight STR as F* code."
  (fstar--init-scratchpad)
  (with-current-buffer fstar--scratchpad
    (erase-buffer)
    (insert (fstar--cleanup-type str))
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (buffer-string)))

(defun fstar--highlight-docstring-region (beg end)
  "Highlight BEG..END as an F* docstring."
  (let ((inhibit-read-only t))
    (goto-char beg)
    (while (search-forward "[" end t)
      (let* ((beg (point))
             (end (ignore-errors (backward-char) (forward-sexp) (1- (point))))
             (str (and end (buffer-substring-no-properties beg end))))
        (if (null str)
            (goto-char end)
          (goto-char beg)
          (delete-region beg end)
          (insert (fstar-highlight-string str)))))))

(defun fstar--highlight-docstring (str)
  "Highlight STR as an F* docstring."
  (with-temp-buffer
    (insert str)
    (fstar--highlight-docstring-region (point-min) (point-max))
    (buffer-string)))

(defun fstar-setup-font-lock ()
  "Setup font-lock for use with F*."
  (font-lock-mode -1)
  (setq-local
   font-lock-defaults
   `(((,fstar-syntax-constants    . 'font-lock-constant-face)
      (,fstar-syntax-keywords     . 'font-lock-keyword-face)
      (,fstar-syntax-builtins     . 'font-lock-builtin-face)
      (,fstar-syntax-preprocessor . 'font-lock-preprocessor-face)
      (,fstar-syntax-structure    . 'fstar-structure-face)
      ,@fstar-syntax-additional)
     nil nil))
  (font-lock-set-defaults)
  (add-to-list 'font-lock-extra-managed-props 'display)
  (add-to-list 'font-lock-extra-managed-props 'invisible)
  (font-lock-mode))

(defun fstar-teardown-font-lock ()
  "Disable F*-related font-locking."
  (when (buffer-live-p fstar--scratchpad)
    (kill-buffer fstar--scratchpad)))

;;; Syntax table

(defvar fstar-syntax-table
  (let ((table (make-syntax-table)))
    ;; Symbols
    (modify-syntax-entry ?# "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?' "_" table)
    ;; Punctuation
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    ;; Comments and strings
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; ‘/’ is handled by a `syntax-propertize-function'.  For background on this
    ;; see http://lists.gnu.org/archive/html/emacs-devel/2017-01/msg00144.html.
    ;; The comment enders are left here, since they don't match the ‘(*’ openers.
    ;; (modify-syntax-entry ?/ ". 12c" table)
    (modify-syntax-entry ?\n  ">" table)
    (modify-syntax-entry ?\^m ">" table)
    (modify-syntax-entry ?\( "()1nb" table)
    (modify-syntax-entry ?*  ". 23b" table)
    (modify-syntax-entry ?\) ")(4nb" table)
    table)
  "Syntax table for F*.")

(defconst fstar--fqn-at-point-syntax-table
  (let ((tbl (make-syntax-table fstar-syntax-table)))
    (modify-syntax-entry ?. "_" tbl)
    tbl))

(defconst fstar-mode-syntax-propertize-function
  (let ((opener-1 (string-to-syntax ". 1"))
        (opener-2 (string-to-syntax ". 2")))
    (syntax-propertize-rules
     ("//" (0 (let* ((pt (match-beginning 0))
                     (state (fstar--syntax-ppss pt)))
                (unless (or (nth 3 state) (nth 4 state))
                  (put-text-property pt (+ pt 1) 'syntax-table opener-1)
                  (put-text-property (+ pt 1) (+ pt 2) 'syntax-table opener-2)
                  (ignore (goto-char (point-at-eol))))))))))

;;; Comment syntax

(defun fstar-syntactic-face-function (args)
  "Choose face to display based on ARGS."
  (pcase-let ((`(_ _ _ ,in-string _ _ _ _ ,comment-start-pos _) args))
    (cond (in-string ;; Strings
           font-lock-string-face)
          (comment-start-pos ;; Comments ('//' doesnt have a comment-depth
           (save-excursion
             (goto-char comment-start-pos)
             (cond
              ((looking-at "(\\*\\*\\*[ \t\n]") '(:inherit font-lock-doc-face :height 2.5))
              ((looking-at "(\\*\\*?\\+[ \t\n]") '(:inherit font-lock-doc-face :height 1.8))
              ((looking-at "(\\*\\*?\\![ \t\n]") '(:inherit font-lock-doc-face :height 1.5))
              ((looking-at "(\\*\\*[ \t\n]")  font-lock-doc-face)
              (t font-lock-comment-face)))))))

(defun fstar-setup-comments ()
  "Set comment-related variables for F*."
  (setq-local comment-multi-line t)
  (setq-local comment-use-syntax t)
  (setq-local comment-start      "(*")
  (setq-local comment-continue   " *")
  (setq-local comment-end        "*)")
  (setq-local comment-start-skip fstar-comment-start-skip)
  (setq-local font-lock-syntactic-face-function #'fstar-syntactic-face-function)
  (setq-local syntax-propertize-function fstar-mode-syntax-propertize-function))

;;; Keymaps

(define-prefix-command 'fstar-query-map)
(define-key 'fstar-query-map (kbd "C-o") #'fstar-outline)
(define-key 'fstar-query-map (kbd "C-c") #'fstar-insert-match-dwim)
(define-key 'fstar-query-map (kbd "C-e") #'fstar-eval)
(define-key 'fstar-query-map (kbd "C-S-e") #'fstar-eval-custom)
(define-key 'fstar-query-map (kbd "C-s") #'fstar-search)
(define-key 'fstar-query-map (kbd "C-d") #'fstar-doc)
(define-key 'fstar-query-map (kbd "C-p") #'fstar-print)
(define-key 'fstar-query-map (kbd "C-q") #'fstar-quit-windows)
(define-key 'fstar-query-map (kbd "C-j C-d") #'fstar-visit-dependency)
(define-key 'fstar-query-map (kbd "h w") #'fstar-browse-wiki)
(define-key 'fstar-query-map (kbd "h W") #'fstar-browse-wiki-in-browser)
(define-key 'fstar-query-map (kbd "h o") #'fstar-list-options)

(defvar fstar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'fstar-newline-and-indent)
    (define-key map (kbd "C-h .") #'display-local-help) ;; For Emacs < 25
    (define-key map (kbd "M-.") #'fstar-jump-to-definition)
    (define-key map (kbd "C-x 4 .") #'fstar-jump-to-definition-other-window)
    (define-key map (kbd "C-x 5 .") #'fstar-jump-to-definition-other-frame)
    (define-key map (kbd "C-c C-'") #'fstar-jump-to-related-error)
    (define-key map (kbd "C-x 4 '") #'fstar-jump-to-related-error-other-window)
    (define-key map (kbd "C-x 5 '") #'fstar-jump-to-related-error-other-frame)
    (define-key map (kbd "C-RET") #'company-manual-begin)
    (define-key map (kbd "<C-return>") #'company-manual-begin)
    (define-key map (kbd "<backtab>") #'fstar-unindent)
    (define-key map (kbd "S-TAB") #'fstar-unindent)
    (define-key map (kbd "C-h M-w") #'fstar-copy-help-at-point)
    ;; (define-key map (kbd "C-c C-d") #'fstar-doc-at-point-dwim)
    (define-key map (kbd "C-c C-a") #'fstar-visit-interface-or-implementation)
    (define-key map (kbd "<menu>") #'fstar-quick-peek)
    (define-key map (kbd "M-<f12>") #'fstar-quick-peek)
    (define-key map (kbd "C-c C-s") 'fstar-query-map)
    map))

(defun fstar-newline-and-indent (arg)
  "Call (newline ARG), and indent resulting line as the previous one."
  (interactive "*P")
  (if (save-excursion (beginning-of-line) (looking-at-p "[ \t]*$"))
      (progn (delete-region (point-at-bol) (point-at-eol))
             (newline arg)) ;; 24.3 doesn't support second argument
    (let ((indentation (current-indentation)))
      (newline arg)
      (indent-line-to indentation))))

(put 'fstar-newline-and-indent 'delete-selection t)

(defun fstar-copy-help-at-point ()
  "Copy contents of help-echo at point."
  (interactive)
  (kill-new (help-at-pt-string)))

;;; Indentation

(defun fstar--indentation-previous-line ()
  "Go to previous non-blank line."
  (goto-char (point-at-bol))
  (skip-chars-backward " \t\n"))

(defun fstar--indentation-points-before (&optional max-column)
  "Add indentation points before MAX-COLUMN or current point to POINTS."
  (let ((points nil)
        (bol (point-at-bol)))
    (when max-column
      (move-to-column max-column))
    (while (> (point) bol)
      (skip-chars-backward " \t" bol)
      (when (/= (skip-chars-backward "^ \t" bol) 0)
        (push (current-column) points)))
    points))

(defun fstar--indentation-points ()
  "Collect indentation points for current line."
  (let ((points nil)
        (in-comment (fstar-in-comment-p (point-at-bol))))
    (save-excursion
      (while (and (not (bobp)) (> (or (car points) 1) 0))
        (fstar--indentation-previous-line)
        (let ((line-points (fstar--indentation-points-before (car points))))
          (when in-comment
            (setq line-points (-filter #'fstar--column-in-commment-p line-points)))
          (when (and (null points) line-points)
            (cl-pushnew (+ (car line-points) 2) (cdr line-points)))
          (setq points (nconc line-points points)))))
    points))

(defun fstar--indentation-insert-points ()
  "Show indentation points on current line."
  (beginning-of-line)
  (dolist (point (fstar--indentation-points))
    (move-to-column point t)
    (insert "^")))

(defun fstar--indent-1 (backwards)
  "Cycle (forwards or BACKWARDS) between relevant indentation points."
  (let* ((current-ind (current-indentation))
         (points (fstar--indentation-points))
         (pred (apply-partially (if backwards #'> #'<) current-ind))
         (remaining (or (-filter pred points) points))
         (target (car (if backwards (last remaining) remaining))))
    (if (> (current-column) current-ind)
        (save-excursion (indent-line-to target))
      (indent-line-to target))))

(defun fstar-indent ()
  "Cycle forwards between vaguely relevant indentation points."
  (interactive)
  (fstar--indent-1 nil))

(defun fstar-unindent ()
  "Cycle backwards between vaguely relevant indentation points."
  (interactive)
  (fstar--indent-1 t))

(defun fstar-setup-indentation ()
  "Setup indentation for F*."
  (setq-local indent-line-function #'fstar-indent)
  (when (boundp 'electric-indent-inhibit) ; Emacs ≥ 24.4
    (setq-local electric-indent-inhibit t)))

(defun fstar-teardown-indentation ()
  "Remove indentation support for F*."
  (kill-local-variable 'electric-indent-inhibit)
  (kill-local-variable 'indent-line-function))

;;; Switching between interface and implementation

(defun fstar-visit-interface-or-implementation ()
  "Switch between interface and implementation."
  (interactive)
  (unless buffer-file-name
    (user-error "Save this file before switching"))
  (when (string-match "\\`\\(.*\\)\\.fst\\(i\\)?\\'" buffer-file-name)
    (let ((other (if (match-end 2) ".fst" ".fsti")))
      (find-file (concat (match-string 1 buffer-file-name) other)))))

(defun fstar--visiting-interface-p ()
  "Check whether current buffer is an interface file."
  (and buffer-file-name
       (string-match-p "\\.fsti\\'" buffer-file-name)))

;;; Wiki

(defconst fstar--wiki-directory
  (expand-file-name "wiki" fstar--directory))

(defconst fstar--wiki-url
  "https://github.com/FStarLang/FStar.wiki.git")

(defun fstar-wiki--run-git-command (reason progress-msg error-fmt &rest args)
  "Run git with ARGS.
REASON is used in error messages if Git isn't available.
Progress is indicated with PROGRESS-MSG.  If git fails, print
ERROR-FMT with error message."
  (message "%s…" progress-msg)
  (fstar-find-executable "git" (format "Git (needed to %s)" reason))
  (with-temp-buffer
    (unless (eq 0 (apply #'process-file "git" nil (current-buffer) nil args))
      (error error-fmt (string-trim (buffer-string)))))
  (message "%s… done." progress-msg))

(defun fstar-wiki-init (&optional force)
  "Download F*'s wiki to /wiki.
No-op if the directory already exists and FORCE is nil.
Return non-nil if a new clone was made."
  (interactive "P")
  (unless (and (file-directory-p fstar--wiki-directory) (not force))
    (delete-directory fstar--wiki-directory t)
    (fstar-wiki--run-git-command
     "fetch F*'s wiki"
     (format "Fetching F*'s wiki from %s" fstar--wiki-url)
     (format "Could not clone %s to %s: “%%s”" fstar--wiki-url fstar--wiki-directory)
     "clone" "--depth" "1" fstar--wiki-url fstar--wiki-directory)
    t))

(defun fstar-wiki-refresh (&optional force)
  "Refresh clone of F*'s wiki.
With FORCE, make a fresh clone."
  (interactive)
  (unless (fstar-wiki-init force)
    (let ((label "refresh F*'s wiki")
          (default-directory fstar--wiki-directory))
      (fstar-wiki--run-git-command label
                              (format "Fetching F*'s wiki from %s" fstar--wiki-url)
                              (format "Could not fetch %s: “%%s”" fstar--wiki-url)
                              "fetch" "origin" "master")
      (fstar-wiki--run-git-command label
                              "Resetting F*'s wiki to latest HEAD"
                              "Could not reset wiki: “%s”"
                              "reset" "--hard" "FETCH_HEAD")
      (fstar-wiki--run-git-command label
                              "Cleaning up orphaned wiki files"
                              "Could not clean up wiki: “%s”"
                              "clean" "-df"))))

(defun fstar--wiki-titles ()
  "List available articles on the F* wiki."
  (unless (fstar-wiki-init)
    (with-demoted-errors "Error while updating the wiki: %s"
      (fstar-wiki-refresh)))
  (let ((names nil))
    (dolist (fname (directory-files fstar--wiki-directory))
      (when (string-match "\\`\\(.*\\)\\.md\\'" fname)
        (let* ((www-name (match-string 1 fname))
               (clean (replace-regexp-in-string "-" " " www-name t t)))
          (push (list clean fname www-name) names))))
    (nreverse names)))

(defun fstar-wiki--read-topic ()
  "Read an F* wiki topic interactively."
  (let* ((titles (fstar--wiki-titles))
         (topic (completing-read "Topic (TAB to show all): " titles nil t)))
    (or (assq topic titles)
        (user-error "Unknown topic “%s”" topic))))

(defun fstar-browse-wiki (fname)
  "Visit FNAME in `fstar--wiki-directory'.
Interactively, offer titles of F* wiki pages."
  (interactive (list (nth 1 (fstar-wiki--read-topic))))
  (find-file (expand-file-name fname fstar--wiki-directory)))

(defun fstar-browse-wiki-in-browser (page-name)
  "Visit PAGE-NAME on F*'s wiki on Github."
  (interactive (list (nth 2 (fstar-wiki--read-topic))))
  (browse-url (format "https://github.com/FStarLang/FStar/wiki/%s" page-name)))

;;; Hiding windows

(defvar fstar--all-temp-buffer-names nil)

(defun fstar-quit-windows ()
  "Hide all temporary F* windows."
  (interactive)
  (mapc #'fstar--hide-buffer fstar--all-temp-buffer-names))

;;; Outline

(defun fstar--outline-close-and-goto (&optional _)
  "Close occur buffer and go to position at point."
  (interactive)
  (let ((pos (occur-mode-find-occurrence))
        (occur-buf (current-buffer)))
    (switch-to-buffer (marker-buffer pos))
    (goto-char pos)
    (kill-buffer occur-buf)))

(defun fstar--outline-cleanup (title)
  "Clean up outline buffer and give it a proper TITLE."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (delete-region (point) (1+ (point-at-eol)))
    (insert (fstar--propertize-title title) "\n")
    (when (fboundp 'font-lock--remove-face-from-text-property)
      (font-lock--remove-face-from-text-property (point) (point-max) 'face 'match))))

(defvar fstar--outline-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map occur-mode-map)
    (dolist (command '(occur-mode-goto-occurrence occur-mode-mouse-goto))
      (substitute-key-definition command #'fstar--outline-close-and-goto map))
    map))

(defun fstar-outline ()
  "Show an outline of the current buffer."
  (interactive)
  (let ((same-window-buffer-names '("*Occur*"))
        (outline-buffer-title (format "Outline of ‘%s’\n" (buffer-name)))
        (outline-buffer-name (format "*fstar-outline: %s*" (buffer-name))))
    (-when-let* ((buf (get-buffer outline-buffer-name)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    (occur fstar-syntax-block-header-re)
    (when (buffer-live-p (get-buffer "*Occur*"))
      (with-current-buffer "*Occur*"
        (rename-buffer outline-buffer-name t)
        (toggle-truncate-lines t)
        (use-local-map fstar--outline-map)
        (fstar--outline-cleanup outline-buffer-title)))))

;;; Interactive proofs (fstar-subp)

(cl-defstruct fstar-subp-query
  query args)

(defconst fstar-subp-legacy--success "ok")
(defconst fstar-subp-legacy--failure "nok")
(defconst fstar-subp-legacy--done "\n#done-")

(defconst fstar-subp-legacy--cancel "#pop")
(defconst fstar-subp-legacy--footer "\n#end #done-ok #done-nok")

(defconst fstar-subp-statuses '(pending busy processed))

(defvar-local fstar-subp--process nil
  "Interactive F* process running in the background.")

(defvar-local fstar-subp--continuations (make-hash-table :test 'equal)
  "Indicates which continuation to run on next output from F* subprocess.
This is a map from query ID to continuation.  In legacy mode, it
never contains more than one entry (with ID nil).")

(defvar-local fstar-subp--queue-timer nil)

(defvar fstar-subp--lax nil
  "Whether to process newly sent regions in lax mode.")

(defface fstar-subp-overlay-lax-face
  '((t :slant italic))
  "Face used to highlight lax-checked sections of the buffer."
  :group 'fstar)

(defface fstar-subp-overlay-pending-face
  '((((background light)) :background "#AD7FA8")
    (((background dark))  :background "#5C3566"))
  "Face used to highlight pending sections of the buffer."
  :group 'fstar)

(defface fstar-subp-overlay-pending-lax-face
  '((t :inherit (fstar-subp-overlay-pending-face fstar-subp-overlay-lax-face)))
  "Face used to highlight pending lax-checked sections of the buffer."
  :group 'fstar)

(defface fstar-subp-overlay-busy-face
  '((((background light)) :background "mistyrose")
    (((background dark))  :background "mediumorchid"))
  "Face used to highlight busy sections of the buffer."
  :group 'fstar)

(defface fstar-subp-overlay-busy-lax-face
  '((t :inherit (fstar-subp-overlay-busy-face fstar-subp-overlay-lax-face)))
  "Face used to highlight busy lax-checked sections of the buffer."
  :group 'fstar)

(defface fstar-subp-overlay-processed-face
  '((((background light)) :background "#EAF8FF")
    (((background dark))  :background "darkslateblue"))
  "Face used to highlight processed sections of the buffer."
  :group 'fstar)

(defface fstar-subp-overlay-processed-lax-face
  '((t :inherit (fstar-subp-overlay-processed-face fstar-subp-overlay-lax-face)))
  "Face used to highlight processed lax-checked sections of the buffer."
  :group 'fstar)

(defface fstar-subp-overlay-error-face
  '((t :underline (:color "red" :style wave)))
  "Face used to highlight errors."
  :group 'fstar)

(defface fstar-subp-overlay-warning-face
  '((t :underline (:color "orange" :style wave)))
  "Face used to highlight warnings."
  :group 'fstar)

(defmacro fstar-subp-with-process-buffer (proc &rest body)
  "If PROC is non-nil, move to PROC's buffer to eval BODY."
  (declare (indent defun) (debug t))
  `(-when-let* ((procp    ,proc)
                (buf      (process-buffer ,proc))
                (buflivep (buffer-live-p buf)))
     (with-current-buffer buf
       ,@body)))

(defmacro fstar-subp-with-source-buffer (proc &rest body)
  "If PROC is non-nil, move to parent buffer of PROC to eval BODY."
  (declare (indent defun) (debug t))
  `(-when-let* ((procp    ,proc)
                (buf      (process-get ,proc 'fstar-subp-source-buffer))
                (buflivep (buffer-live-p buf)))
     (with-current-buffer buf
       ,@body)))

;;; ;; Overlay classification

(defun fstar-subp-issue-overlay-p (overlay)
  "Return non-nil if OVERLAY is an fstar-subp issue overlay."
  (overlay-get overlay 'fstar-subp-issue))

(defun fstar-subp-orphaned-issue-overlay-p (overlay)
  "Return non-nil if OVERLAY is an orphaned fstar-subp issue overlay."
  (and (overlay-get overlay 'fstar-subp-issue)
       (let ((parent (overlay-get overlay 'fstar-subp-issue-parent-overlay)))
         (or (null parent) (null (overlay-buffer parent))))))

(defun fstar-subp-issue-overlays (beg end)
  "Find all -subp issues overlays in BEG END."
  (-filter #'fstar-subp-issue-overlay-p (overlays-in beg end)))

(defun fstar-subp-issue-orphaned-overlays (beg end)
  "Find all -subp issues overlays in BEG END."
  (-filter #'fstar-subp-orphaned-issue-overlay-p (overlays-in beg end)))

(defun fstar-subp-issue-overlays-at (pt)
  "Find all -subp issues overlays at point PT."
  (-filter #'fstar-subp-issue-overlay-p (overlays-at pt)))

(defun fstar-subp-tracking-overlay-p (overlay)
  "Return non-nil if OVERLAY is an fstar-subp tracking overlay."
  (fstar-subp-status overlay))

(defun fstar-subp-tracking-overlays (&optional status pos)
  "Find all -subp tracking overlays with status STATUS at POS.

If STATUS is nil, return all fstar-subp overlays.  If POS is nil,
look in the entire buffer."
  (sort (cl-loop for overlay in (if pos (overlays-at pos)
                                  (overlays-in (point-min) (point-max)))
                 when (fstar-subp-tracking-overlay-p overlay)
                 when (or (not status) (fstar-subp-status-eq overlay status))
                 collect overlay)
        (lambda (o1 o2) (< (overlay-start o1) (overlay-start o2)))))

(defun fstar-subp-remove-tracking-overlays ()
  "Remove all F* tracking overlays in the current buffer."
  (mapc #'delete-overlay (fstar-subp-tracking-overlays)))

(defun fstar-subp-remove-issue-overlays (beg end)
  "Remove all F* issue overlays in BEG .. END."
  (mapc #'delete-overlay (fstar-subp-issue-overlays beg end)))

(defun fstar-subp-remove-orphaned-issue-overlays (beg end)
  "Remove all F* issue overlays in BEG .. END whose overlay is dead."
  (mapc #'delete-overlay (fstar-subp-issue-orphaned-overlays beg end)))

;;; ;; Overlay status legend in modeline

(defconst fstar-subp--overlay-legend-mode-line
  (let ((sp (propertize " " 'display '(space :width 1.5)))
        (it (propertize " " 'display '(space :width 0.3))))
    `(,(propertize " " 'display '(space :align-to 0)) ;; 'face 'fringe
      ,(propertize "Legend: " 'face nil)
      ,(mapconcat
        (lambda (label-face)
          (propertize (concat "​" sp "​" (car label-face) "​" sp "​")
                      'face (cdr label-face)))
        `(("pending" . (fstar-subp-overlay-pending-face))
          ("busy" . (fstar-subp-overlay-busy-face))
          ("processed" . (fstar-subp-overlay-processed-face))
          (,(concat "lax-checked" it) . (fstar-subp-overlay-lax-face (:box -1))))
        "")
      " ")))

(defconst fstar-subp--overlay-legend-str
  (format-mode-line fstar-subp--overlay-legend-mode-line))

(defvar fstar-subp--overlay-legend-buffer nil
  "Buffer currently showing legend in modeline.")

(defun fstar-subp--hide-overlay-legend-mode-line ()
  "Hide legend of overlay statuses."
  (when (buffer-live-p fstar-subp--overlay-legend-buffer)
    (with-current-buffer fstar-subp--overlay-legend-buffer
      (setq fstar-subp--overlay-legend-buffer nil)
      (when (eq (car mode-line-format) fstar-subp--overlay-legend-mode-line)
        (pop mode-line-format)))))

(defun fstar-subp--show-overlay-legend-mode-line ()
  "Update modeline to contain legend of overlay statuses."
  (fstar-subp--hide-overlay-legend-mode-line)
  (setq fstar-subp--overlay-legend-buffer (current-buffer))
  (push fstar-subp--overlay-legend-mode-line mode-line-format))

(defun fstar-subp--overlay-legend-help-function (fn help-string)
  "Show legend of overlay statuses in buffer indicated by HELP-STRING.
If HELP-STRING doesn't have `fstar-subp--legend' property, call
FN instead."
  (-if-let* ((buf (and help-string
                       (get-text-property 0 'fstar-subp--legend help-string))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (fstar-subp--show-overlay-legend-mode-line)))
    (fstar-subp--hide-overlay-legend-mode-line)
    (funcall fn help-string)))

(defun fstar-setup-overlay-legend ()
  "Enable overlay legends in modeline."
  (tooltip-mode 1)
  ;; Must use advice, because tooltip mode aggressively resets
  ;; `show-help-function' to `tooltip-show-help'.
  (when (fboundp 'advice-add)
    (dolist (fn '(tooltip-show-help tooltip-show-help-non-mode))
      (advice-add fn :around #'fstar-subp--overlay-legend-help-function))))

(defun fstar-teardown-overlay-legend ()
  "Disable overlay legends in modeline."
  ;; No way to disable buffer-locally
  ;; (when (fboundp 'advice-remove)
  ;;   (dolist (fn '(tooltip-show-help tooltip-show-help-non-mode))
  ;;     (advice-remove fn #'fstar-subp--overlay-legend-help-function)))
  )

;;; ;; Basic subprocess operations

(defun fstar-subp-live-p (&optional proc)
  "Return t if the PROC is a live F* subprocess.
If PROC is nil, use the current buffer's `fstar-subp--process'."
  (setq proc (or proc fstar-subp--process))
  (and proc (process-live-p proc)))

(defun fstar-subp--busy-p ()
  "Return t if current `fstar-subp--process' is live and busy."
  (and (fstar-subp-live-p) (> (hash-table-count fstar-subp--continuations) 0)))

(defun fstar-subp-available-p ()
  "Return t if current `fstar-subp--process' is live and idle."
  (and (fstar-subp-live-p) (= (hash-table-count fstar-subp--continuations) 0)))

(defun fstar-subp--ensure-available (error-fn &optional feature)
  "Raise an error with ERROR-FN if F* isn't available.
Also raise an error if current subprocess doesn't meet version requirements for
FEATURE, if specified."
  (unless (fstar-subp-live-p)
    (funcall error-fn "Please start F* to use this feature"))
  (when (fstar-subp--busy-p)
    (funcall error-fn "F* seems busy; please wait until processing is complete"))
  (when feature
    (fstar--has-feature feature error-fn)))

(defun fstar-subp--serialize-query (query id)
  "Serialize QUERY with ID to JSON."
  (let ((json-encoding-pretty-print nil)
        (dummy-alist '(("dummy" . nil))))
    (json-encode `(("query-id" . ,id)
                   ("query" . ,(fstar-subp-query-query query))
                   ("args" . ,(or (fstar-subp-query-args query) dummy-alist))))))

(defvar-local fstar-subp--next-query-id 0)

(defun fstar-subp--query (query continuation)
  "Send QUERY to F* subprocess; handle results with CONTINUATION."
  (let ((id nil))
    (when (fstar-subp-query-p query)
      (setq id (number-to-string (cl-incf fstar-subp--next-query-id)))
      (setq query (fstar-subp--serialize-query query id)))
    (fstar-log 'in "%s" query)
    (if continuation
        (puthash id continuation fstar-subp--continuations)
      (remhash id fstar-subp--continuations))
    (fstar-subp-start)
    (process-send-string fstar-subp--process (concat query "\n"))))

(defun fstar-subp--query-and-wait-1 (start query results-cell max-delay continuation)
  "Issues QUERY and wait for an answer for up to MAX-DELAY after START.
Results are passed to CONTINUATION, which should set (car
RESULTS-CELL) to t when invoked."
  (if (not (fstar-subp-available-p))
      (funcall continuation nil nil)
    ;; Issue query immediately, storing results in a shared reference.
    (fstar-subp--query query continuation)
    ;; Wait for a bit, hoping to get candidates quickly.  We need a loop,
    ;; because output may come in chunks.
    (while (and (not (car results-cell))
                (< (float-time (time-since start)) max-delay))
      (accept-process-output fstar-subp--process 0.005 nil 0))))

(defun fstar-subp--query-and-wait (query max-sync-delay)
  "Issue QUERY and try to return RESULTS synchronously.
Wait for MAX-SYNC-DELAY seconds at most.  If results were
received while waiting return a cons (t . (STATUS RESULTS)).
Otherwise, return a cons ('needs-callback . _).  In the latter
case, the caller should write a callback to the `cdr' of the
return value."
  (let* ((start (current-time))
         (results-cell (cons nil nil))
         (callback-cell (cons 'needs-callback nil)))
    ;; Issue query immediately and wait for a bit
    (fstar-subp--query-and-wait-1
     start query results-cell max-sync-delay
     (lambda (status results)
       (setf (car results-cell) t)
       (setf (cdr results-cell) (list status results))
       (when (cdr callback-cell)
         (apply (cdr callback-cell) (cdr results-cell)))))
    ;; Check for results
    (cond
     ((car results-cell) ;; Got results in time!
      (fstar-log 'info "[%.2fms] Fetched results for %S"
            (* 1000 (float-time (time-since start))) query)
      results-cell)
     (t ;; Results are late.  Set callback to company-supplied one.
      (fstar-log 'info "Results for %S are late" query)
      callback-cell))))

(defun fstar-subp-legacy--find-response (proc)
  "Find full response in PROC's legacy-mode buffer; handle it if found."
  (setq ansi-color-context-region nil)
  (ansi-color-filter-region (point-min) (point-max))
  (goto-char (point-min)) ;; FIXME better protocol wouldn't require re-scans
  (when (search-forward fstar-subp-legacy--done nil t)
    (let* ((status        (cond
                           ((looking-at fstar-subp-legacy--success) 'success)
                           ((looking-at fstar-subp-legacy--failure) 'failure)
                           (t 'unknown)))
           (resp-beg      (point-min))
           (resp-end      (point-at-bol))
           (resp-real-end (point-at-eol))
           (response      (fstar--string-trim (buffer-substring resp-beg resp-end))))
      (fstar-log 'info "Complete message received (status: %S)" status)
      (delete-region resp-beg resp-real-end)
      (when (fstar-subp-live-p proc)
        (fstar-subp-with-source-buffer proc
          (unless (memq status '(success failure))
            (fstar-subp-kill)
            (error "Unknown status [%s] from F* subprocess \
\(response was [%s])" status response))
          (fstar-subp--process-response nil status response))))))

(defun fstar-subp-json--parse-status (status)
  "Convert STATUS to a symbol indicating success."
  (pcase status
    ((or "success" "failure") (intern status))
    (_ (warn "Unknown status %S from F* subprocess" status)
       `failure)))

(defun fstar-subp-json--read-response (beg end)
  "Read JSON message from BEG to END."
  (goto-char beg)
  (let* ((json-null :json-null)
         (json-false :json-false)
         (json-key-type 'symbol)
         (json-array-type 'list)
         (json-object-type 'alist)
         (json (when (eq (char-after) ?\{)
                 (condition-case nil (json-read)
                   (json-error nil)))))
    (cond
     ((null json)
      (ignore (fstar-log 'warning "%s" (buffer-substring beg end))
              (message "[F* raw output] %s" (buffer-substring beg end))))
     ((/= (point) end)
      (ignore (fstar-log 'warning "Junk follows JSON message: %s" (buffer-substring beg end))))
     (t
      (fstar-log 'info "Complete message received: (status: %S; message: %S)"
            (let-alist json .status) json)
      json))))

(defun fstar-subp-json--find-response (proc)
  "Find full response in PROC's json-mode buffer; handle it if found."
  (while (search-forward "\n" nil t)
    (let* ((js-end (1- (point)))
           (json (and (> js-end (point-min)) ;; Strip empty lines
                      (fstar-subp-json--read-response (point-min) js-end))))
      (goto-char (point-min))
      (delete-region (point-min) (1+ js-end))
      (when json
        (let-alist json
          (fstar-subp-with-source-buffer proc
            (pcase .kind
              ("message"
               (fstar-subp--process-message .level .contents))
              ("protocol-info"
               (when (fstar-subp-live-p proc)
                 (fstar-subp--process-protocol-info .version .features)))
              ("response"
               (let ((status (fstar-subp-json--parse-status .status)))
                 (when (fstar-subp-live-p proc)
                   (fstar-subp--process-response .query-id status .response))))))))))
  ;; Skip to end of partial response
  (goto-char (point-max)))

(defun fstar-subp-find-response (proc)
  "Find full response in PROC's buffer; handle it if found."
  (if (fstar--has-feature 'json-subp)
      (fstar-subp-json--find-response proc)
    (fstar-subp-legacy--find-response proc)))

(defun fstar-subp--process-message (level contents)
  "Process CONTENTS of real-time feedback message at LEVEL."
  (let ((header (format "[F* %s] " level)))
    (setq contents (string-trim contents))
    (message "%s" (fstar--indent-str contents header))))

(defun fstar-subp--process-protocol-info (_vernum proto-features)
  "Register information (VERNUM and PROTO-FEATURES) about the protocol."
  (setq proto-features (mapcar #'intern proto-features))
  (setq fstar--features (append proto-features fstar--features))
  (fstar-subp-with-process-buffer fstar-subp--process
    (setq fstar--features (append proto-features fstar--features))))

(defun fstar-subp--process-response (id status response)
  "Process STATUS and RESPONSE for query ID from F* subprocess."
  (let* ((source-buffer (current-buffer))
         (continuation (gethash id fstar-subp--continuations)))
    (when (null continuation)
      (let ((conts (format "%S" fstar-subp--continuations)))
        (fstar-subp-kill)
        (error "Invalid state: Received orphan response %S to query %S.
Table of continuations was %s" response id conts)))
    (remhash id fstar-subp--continuations)
    (unwind-protect
        (funcall continuation status response)
      (with-current-buffer source-buffer
        (fstar-subp--set-queue-timer)))))

(defun fstar-subp-warn-unexpected-output (string)
  "Warn user about unexpected output STRING."
  (message "F*: received unexpected output from subprocess (%s)" string))

(defun fstar-subp-filter (proc string)
  "Handle PROC's output (STRING)."
  (when string
    (fstar-log 'out "%s" string)
    (let ((proc-buf (process-buffer proc)))
      (if (buffer-live-p proc-buf)
          (fstar-subp-with-process-buffer proc
            (save-excursion
              (goto-char (point-max))
              (insert string))
            (fstar-subp-find-response proc))
        (run-with-timer 0 nil #'fstar-subp-warn-unexpected-output string)))))

(defun fstar-subp-sentinel (proc signal)
  "Handle PROC's SIGNAL."
  (fstar-log 'info "Signal received: [%s] [%s]" signal (process-status proc))
  (when (or (memq (process-status proc) '(exit signal))
            (not (process-live-p proc)))
    (message "F*: subprocess exited.")
    (fstar-subp-with-source-buffer proc
      (fstar-subp-killed))))

(defun fstar-subp--clear-continuations ()
  "Get rid of all pending continuations."
  (maphash (lambda (_ cont) (funcall cont 'interrupted nil)) fstar-subp--continuations)
  (clrhash fstar-subp--continuations))

(defun fstar-subp-killed ()
  "Clean up current source buffer."
  (fstar-subp-with-process-buffer fstar-subp--process
    (let ((leftovers (fstar--string-trim (buffer-string))))
      (unless (equal leftovers "")
        (message "F* subprocess died early: %s" leftovers)))
    (kill-buffer))
  (fstar-subp-remove-issue-overlays (point-min) (point-max))
  (fstar-subp-remove-tracking-overlays)
  (fstar-subp--clear-continuations)
  (setq fstar--vernum nil
        fstar--features nil
        fstar-subp--process nil
        fstar-subp--queue-timer nil
        fstar-subp--next-query-id 0))

(defun fstar-subp-kill ()
  "Kill F* subprocess and clean up current buffer."
  (interactive)
  (when (fstar-subp-live-p)
    (kill-process fstar-subp--process)
    (accept-process-output fstar-subp--process 0.25 nil t))
  (fstar-subp-killed))

(defun fstar-subp-kill-all ()
  "Kill F* subprocesses in all buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf) ; May have been killed by previous iterations
      (with-current-buffer buf
        (when (derived-mode-p 'fstar-mode)
          (fstar-subp-kill))))))

(defun fstar-subp-kill-one-or-many (&optional arg)
  "Kill current F* subprocess.
With prefix argument ARG, kill all F* subprocesses."
  (interactive "P")
  (if (consp arg)
      (fstar-subp-kill-all)
    (fstar-subp-kill)))

(defun fstar-subp-kill-proc (proc)
  "Same as `fstar-subp-kill', but for PROC instead of `fstar-subp--process'."
  (fstar-subp-with-source-buffer proc
    (when fstar-subp--process
      (fstar-assert (eq proc fstar-subp--process))
      (fstar-subp-kill))))

(defconst fstar--ps-line-regexp
  "^ *\\([0-9]+\\) +\\([0-9]+\\) \\(.+\\) *$")

(defun fstar--ps-processes ()
  "Collect all running processes using `ps'.
Each return value is a list (PID PARENT-PID CMD)."
  (mapcar (lambda (line)
            (if (string-match fstar--ps-line-regexp line)
                (list (string-to-number (match-string 1 line))
                      (string-to-number (match-string 2 line))
                      (match-string 3 line))
              (error "Unexpected line in PS output: %S" line)))
          (cdr (process-lines "ps" "-axww" "-o" "pid,ppid,comm"))))

(defun fstar--elisp-process-attributes (pid)
  "Get attributes of process PID, or nil."
  (let* ((attrs (process-attributes pid)))
    (when attrs
      (list pid
            (cdr (assq 'ppid attrs))
            (cdr (assq 'comm attrs))))))

(defun fstar--elisp-processes ()
  "Collect all running processes using `system-processes'.
Each return value is a list (PID PPID CMD ARGS).  Return nil if
`list-system-processes' or `process-attributes' is unsupported."
  (delq nil (mapcar #'fstar--elisp-process-attributes (list-system-processes))))

(defun fstar--system-processes ()
  "Collect all running processes.
Each return value is a list (PID PPID CMD ARGS).  Uses
`list-processes' when available, and `ps' otherwise."
  (or (fstar--elisp-processes) (fstar--ps-processes)))

(defun fstar-subp-kill-z3 (all)
  "Kill Z3 subprocesses associated with the current F* process.
With non-nil ALL (interactively, with prefix argument), kill all
Z3 processes.  If F* isn't busy and ALL is nil this function
returns without doing anything."
  (interactive "P")
  (when (and (not all) (fstar-subp-available-p))
    (user-error "No busy F* process to interrupt"))
  (let ((parent-live (fstar-subp-live-p)))
    (unless (or all parent-live)
      (user-error "No F* process in this buffer"))
    (let ((subp-pid (and parent-live (process-id fstar-subp--process))))
      (pcase-dolist (`(,pid ,ppid ,cmd) (fstar--system-processes))
        (when (and (or all (eq ppid subp-pid))
                   (or (string-suffix-p "z3" cmd)
                       (string-suffix-p "z3.exe" cmd)))
          (signal-process pid 'int)
          (message "Sent SIGINT to %S (%S)" pid cmd))))))

;;; ;; Parsing and display issues

(defun fstar-subp--pop ()
  "Issue a `pop' query.
Recall that the legacy F* protocol doesn't ack pops."
  (if (fstar--has-feature 'json-subp)
      (fstar-subp--query (make-fstar-subp-query :query "pop" :args nil) #'ignore)
    (fstar-subp--query fstar-subp-legacy--cancel nil)))

(defun fstar-subp--overlay-continuation (overlay status response)
  "Handle the results (STATUS and RESPONSE) of processing OVERLAY."
  (unless (eq status 'interrupted)
    (fstar-subp-parse-and-highlight-issues status response overlay)
    (if (eq status 'success)
        (fstar-subp-set-status overlay 'processed)
      (fstar-subp-remove-unprocessed)
      ;; Legacy protocol requires a pop after failed pushes
      (unless (fstar--has-feature 'json-subp)
        (fstar-subp--pop)))))

(cl-defstruct fstar-location
  filename line-from line-to col-from col-to)

(defun fstar--loc-to-string (loc)
  "Turn LOC into a string."
  (format "%s(%d,%d-%d,%d)"
          (fstar-location-filename loc)
          (fstar-location-line-from loc)
          (fstar-location-col-from loc)
          (fstar-location-line-to loc)
          (fstar-location-col-to loc)))

(defun fstar-location-beg-offset (location)
  "Compute LOCATION's beginning offset in the current buffer."
  (fstar--line-col-offset (fstar-location-line-from location)
                     (fstar-location-col-from location)))

(defun fstar-location-end-offset (location)
  "Compute LOCATION's end offset in the current buffer."
  (fstar--line-col-offset (fstar-location-line-to location)
                     (fstar-location-col-to location)))

(cl-defstruct fstar-issue
  level locs message)

(defun fstar-issue-alt-locs (issue)
  "Extract locations attached to ISSUE, except the first one."
  (cdr (fstar-issue-locs issue)))

(defconst fstar-subp-issue-location-regexp
  "\\(.*?\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\)-\\([[:digit:]]+\\),\\([[:digit:]]+\\))")

(defconst fstar-subp-legacy--issue-regexp
  (concat "^" fstar-subp-issue-location-regexp "\\s-*:\\s-*"))

(defconst fstar-subp--also-see-regexp
  (concat " *(\\(?:Also see:\\|see also\\) *" fstar-subp-issue-location-regexp ")"))

(defun fstar-subp-legacy--parse-issue (limit)
  "Construct an issue object from the current match data up to LIMIT."
  (pcase-let* ((issue-level 'error)
               (message (buffer-substring (match-end 0) limit))
               (`(,filename ,line-from ,col-from ,line-to ,col-to)
                (or (save-match-data
                      (when (string-match fstar-subp--also-see-regexp message)
                        (prog1 (fstar--match-strings-no-properties '(1 2 3 4 5) message)
                          (setq message (substring message 0 (match-beginning 0))))))
                    (fstar--match-strings-no-properties '(1 2 3 4 5)))))
    (pcase-dolist (`(,marker . ,level) '(("(Warning) " . warning)
                                         ("(Error) " . error)))
      (when (string-prefix-p marker message)
        (setq issue-level level)
        (setq message (substring message (length marker)))))
    (make-fstar-issue :level issue-level
                      :locs `(,(make-fstar-location
                                :filename filename
                                :line-from (string-to-number line-from)
                                :col-from (string-to-number col-from)
                                :line-to (string-to-number line-to)
                                :col-to (string-to-number col-to)))
                      :message message)))

(defun fstar-subp-json--extract-alt-locs (message)
  "Extract secondary locations (see also, Also see) from MESSAGE.
Returns a pair of (CLEAN-MESSAGE . LOCATIONS)."
  (let ((locations nil))
    (while (string-match fstar-subp--also-see-regexp message)
      (pcase-let* ((`(,filename ,line-from ,col-from ,line-to ,col-to)
                    (fstar--match-strings-no-properties '(1 2 3 4 5) message)))
        (push (make-fstar-location
               :filename filename
               :line-from (string-to-number line-from)
               :line-to (string-to-number line-to)
               :col-from (string-to-number col-from)
               :col-to (string-to-number col-to))
              locations))
      (setq message (replace-match "" t t message)))
    (cons message locations)))

(defun fstar-subp-json--parse-location (json)
  "Convert JSON info an fstar-mode location."
  (let-alist json
    (make-fstar-location
     :filename .fname
     :line-from (elt .beg 0)
     :line-to (elt .end 0)
     :col-from (elt .beg 1)
     :col-to (elt .end 1))))

(defun fstar-subp-json--parse-issue (json)
  "Convert JSON issue into fstar-mode issue."
  (let-alist json
    (pcase-let* ((`(,msg . ,alt-locs) (fstar-subp-json--extract-alt-locs .message)))
      (make-fstar-issue
       :level (intern .level)
       :locs (append (mapcar #'fstar-subp-json--parse-location .ranges) alt-locs)
       :message msg))))

(defun fstar-subp-parse-issues (response)
  "Parse RESPONSE into a list of issues."
  (cond
   ((fstar--has-feature 'json-subp)
    (mapcar #'fstar-subp-json--parse-issue response))
   ((stringp response)
    (unless (equal response "")
      (with-temp-buffer
        (insert response)
        (let ((bound (point-max)))
          (goto-char (point-max))
          ;; Matching backwards makes it easy to capture multi-line issues.
          (cl-loop while (re-search-backward fstar-subp-legacy--issue-regexp nil t)
                   collect (fstar-subp-legacy--parse-issue bound)
                   do (setq bound (match-beginning 0)))))))))

(defun fstar-subp--local-loc-p (location)
  "Check if LOCATION came from the current buffer."
  (and location
       (string= (expand-file-name buffer-file-name)
                (expand-file-name (fstar-location-filename location)))))

(defun fstar-subp-cleanup-issue (issue &optional ov)
  "Fixup ISSUE: include a file name, and adjust line numbers wrt OV."
  (dolist (loc (fstar-issue-locs issue))
    ;; Clean up file names
    (when (member (fstar-location-filename loc) '("unknown" "<input>"))
      (cl-assert buffer-file-name) ;; FIXME "unknown"
      (setf (fstar-location-filename loc) buffer-file-name))
    ;; Adjust line numbers
    (unless (or (fstar--has-feature 'absolute-linums-in-errors) (null ov))
      (let ((linum (1- (line-number-at-pos (overlay-start ov)))))
        (cl-incf (fstar-location-line-from loc) linum)
        (cl-incf (fstar-location-line-to loc) linum))))
  ;; Put local ranges first
  (let ((partitioned (-group-by #'fstar-subp--local-loc-p (fstar-issue-locs issue))))
    (setf (fstar-issue-locs issue)
          (append (cdr (assq t partitioned))
                  (cdr (assq nil partitioned)))))
  issue)

(defun fstar-subp--issues-at (pt)
  "Get issues at PT or PT - 1, if any."
  (mapcar (lambda (ov) (overlay-get ov 'fstar-subp-issue))
          (cl-remove-duplicates
           (append (fstar-subp-issue-overlays-at pt)
                   (and (> pt (point-min))
                        (fstar-subp-issue-overlays-at (1- pt)))))))

(defun fstar-subp--in-issue-p (pt)
  "Check if PT is covered by an F* issue overlay."
  (or (get-char-property pt 'fstar-subp-issue)
      (and (> pt (point-min))
           (get-char-property (1- pt) 'fstar-subp-issue))))

(defun fstar-subp--alt-locs-at (pt)
  "Return a list of alternate locations at PT."
  (apply #'append (mapcar #'fstar-issue-alt-locs (fstar-subp--issues-at pt))))

(defun fstar-subp-remove-issue-overlay (overlay &rest _args)
  "Remove OVERLAY."
  (delete-overlay overlay))

(defun fstar-subp--help-echo-for-alt-locs (locs)
  "Prepare a string describing LOCS and how to browse to them."
  (if (null locs) ""
    (propertize
     (concat
      (substitute-command-keys
       (if (cdr locs)
           "\nRelated locations (\\[fstar-jump-to-related-error] to visit):\n"
         "\nRelated location (\\[fstar-jump-to-related-error] to visit): "))
      (mapconcat #'fstar--loc-to-string locs "\n"))
     'face 'italic)))

(defun fstar-subp--help-echo-at (pos)
  "Compute help-echo message at POS."
  (-when-let* ((issues (fstar-subp--issues-at pos)))
    (concat (mapconcat #'fstar-issue-message issues "\n")
            (fstar-subp--help-echo-for-alt-locs (fstar-subp--alt-locs-at pos)))))

(defun fstar-subp--help-echo (_window object pos)
  "Concatenate -subp messages found at POS of OBJECT."
  (-when-let* ((buf (cond ((bufferp object) object)
                          ((overlayp object) (overlay-buffer object)))))
    (with-current-buffer buf
      (fstar-subp--help-echo-at pos))))

(defun fstar-subp-issue-face (issue)
  "Compute a face for ISSUE's overlay."
  (pcase (fstar-issue-level issue)
    (`error 'fstar-subp-overlay-error-face)
    (`warning 'fstar-subp-overlay-warning-face)))

(defun fstar-subp-highlight-issue (issue parent)
  "Highlight ISSUE in current buffer.
PARENT is the overlay whose processing caused this issue to be
reported."
  (-when-let* ((loc (car (fstar-issue-locs issue)))
               (from (fstar-location-beg-offset loc))
               (to (fstar-location-end-offset loc))
               (overlay (make-overlay from (max to (1+ from)) (current-buffer) t nil)))
    (overlay-put overlay 'fstar-subp-issue issue)
    (overlay-put overlay 'fstar-subp-issue-parent-overlay parent)
    (overlay-put overlay 'face (fstar-subp-issue-face issue))
    (overlay-put overlay 'help-echo #'fstar-subp--help-echo)
    (overlay-put overlay 'modification-hooks '(fstar-subp-remove-issue-overlay))
    (when (fboundp 'pulse-momentary-highlight-region)
      (pulse-momentary-highlight-region from to))))

(defun fstar-subp-highlight-issues (issues parent)
  "Highlight ISSUES caused by processing of PARENT overlay."
  (mapc (lambda (i) (fstar-subp-highlight-issue i parent)) issues))

(defun fstar-subp-jump-to-issue (issue)
  "Jump to ISSUE in current buffer."
  (-when-let* ((loc (car (fstar-issue-locs issue))))
    (goto-char (fstar-location-beg-offset loc))))

(defun fstar-subp--local-issue-p (issue)
  "Check if any location in ISSUE came from the current buffer."
  ;; Local locations come first in list of locations
  (fstar-subp--local-loc-p (car (fstar-issue-locs issue))))

(defun fstar-subp-parse-and-highlight-issues (status response overlay)
  "Parse issues in RESPONSE (caused by processing OVERLAY) and display them.
Complain if STATUS is `failure' and RESPONSE doesn't contain issues."
  (let* ((raw-issues (fstar-subp-parse-issues response))
         (issues (mapcar (lambda (i) (fstar-subp-cleanup-issue i overlay)) raw-issues))
         (partitioned (-group-by #'fstar-subp--local-issue-p issues))
         (local-issues (cdr (assq t partitioned)))
         (other-issues (cdr (assq nil partitioned))))
    (when (and (eq status 'failure) (null issues))
      (warn "No issues found in response despite prover failure: [%s]" response))
    (when other-issues
      (message "F* reported issues in other files: [%S]" other-issues))
    (when issues
      (fstar-log 'info "Highlighting issues: %s" issues))
    (when local-issues
      (fstar-subp-jump-to-issue (car local-issues))
      (fstar-subp-highlight-issues local-issues overlay)
      (display-local-help))))

;;; ;; Visiting related errors

(defun fstar-jump-to-related-error (&optional display-action)
  "Jump to secondary error location of error at point.
DISPLAY-ACTION is nil, `window', or `frame'."
  (interactive)
  (-if-let* ((loc (car (fstar-subp--alt-locs-at (point)))))
      (fstar--navigate-to (fstar-location-filename loc)
                     (fstar-location-line-from loc)
                     (fstar-location-col-from loc)
                     display-action)
    (if (fstar-subp--in-issue-p (point))
        (user-error "No secondary locations for this issue")
      (user-error "No error here"))))

(defun fstar-jump-to-related-error-other-window ()
  "Jump to secondary location of error at point in new window."
  (interactive)
  (fstar-jump-to-related-error 'window))

(defun fstar-jump-to-related-error-other-frame ()
  "Jump to secondary location of error at point in new frame."
  (interactive)
  (fstar-jump-to-related-error 'frame))

;;; ;; Tracking and updating overlays

(defun fstar-subp-status (overlay)
  "Get status of OVERLAY."
  (overlay-get overlay 'fstar-subp-status))

(defun fstar-subp-status-eq (overlay status)
  "Check if OVERLAY has status STATUS."
  (eq (fstar-subp-status overlay) status))

(defun fstar-subp-remove-unprocessed ()
  "Remove pending and busy overlays."
  (cl-loop for overlay in (fstar-subp-tracking-overlays)
           unless (fstar-subp-status-eq overlay 'processed)
           do (delete-overlay overlay)))

(defun fstar-subp-legacy--strip-comments (str)
  "Remove comments in STR (replace them with spaces).
This was needed when control and data were mixed."
  (with-temp-buffer
    (set-syntax-table fstar-syntax-table)
    (fstar-setup-comments)
    (comment-normalize-vars)
    (insert str)
    (goto-char (point-min))
    (let (start)
      (while (setq start (comment-search-forward nil t))
        (goto-char start)
        (forward-comment 1)
        (save-match-data
          (let* ((comment (buffer-substring-no-properties start (point)))
                 (replacement (replace-regexp-in-string "." " " comment t t)))
            (delete-region start (point))
            (insert replacement)))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun fstar-subp--clean-buffer-substring (beg end)
  "Make a clean copy of range BEG..END before sending it to F*."
  (if (fstar--has-feature 'json-subp)
      (buffer-substring-no-properties beg end)
    (fstar-subp-legacy--strip-comments (buffer-substring-no-properties beg end))))

(defun fstar--subp-push-peek-query-1 (query pos kind code)
  "Helper for push/peek (QUERY) with POS KIND and CODE."
  (make-fstar-subp-query
   :query query
   :args `(("kind" . ,(symbol-name kind))
           ("code" . ,code)
           ("line" . ,(line-number-at-pos pos))
           ("column" . ,(fstar-subp--column-number-at-pos pos)))))

(defun fstar-subp--push-query (pos kind code)
  "Prepare a push query for a region starting at POS.
KIND is one of `lax', `full'.  CODE is the code to push."
  (if (fstar--has-feature 'json-subp)
      (fstar--subp-push-peek-query-1 "push" pos kind code)
    (format "#push %d %d%s\n%s%s"
            (line-number-at-pos pos)
            (fstar-subp--column-number-at-pos pos)
            (pcase kind (`lax " #lax") (`full ""))
            code
            fstar-subp-legacy--footer)))

(defun fstar-subp--peek-query (pos kind code)
  "Prepare a peek query for a region starting at POS.
KIND is one of `syntax' or `light'.  CODE is the code to
push."
  (fstar-assert (fstar--has-feature 'json-subp))
  (fstar--subp-push-peek-query-1 "peek" pos kind code))

(defun fstar-subp-push-region (beg end kind continuation)
  "Push the region between BEG and END to the inferior F* process.
KIND indicates how to check BEG..END (one of `lax', `full').
Handle results with CONTINUATION."
  (let* ((payload (fstar-subp--clean-buffer-substring beg end)))
    (fstar-subp--query (fstar-subp--push-query beg kind payload) continuation)))

(defun fstar-subp-peek-region (beg end kind continuation)
  "Ask the inferior F* process about the region between BEG and END.
KIND indicates how to check BEG..END (one of `syntax', `light').
Handle results with CONTINUATION."
  (let* ((payload (fstar-subp--clean-buffer-substring beg end)))
    (fstar-subp--query (fstar-subp--peek-query beg kind payload) continuation)))

(defun fstar-subp-overlay-attempt-modification (overlay &rest _args)
  "Allow or prevent attempts to modify OVERLAY.

Modifications are only allowed if it is safe to retract up to the beginning of the current overlay."
  (let ((inhibit-modification-hooks t))
    (when (overlay-buffer overlay) ;; Hooks can be called multiple times
      (cond
       ;; Always allow modifications in comments
       ((fstar-in-comment-p) t)
       ;; Allow modifications (after retracting) in pending overlays, and in
       ;; processed overlays provided that F* isn't busy
       ((or (not (fstar-subp--busy-p))
            (fstar-subp-status-eq overlay 'pending))
        (fstar-subp-retract-until (overlay-start overlay)))
       ;; Disallow modifications in processed overlays when F* is busy
       ((fstar-subp-status-eq overlay 'processed)
        (user-error "Cannot retract a processed section while F* is busy"))
       ;; Always disallow modifications in busy overlays
       ((fstar-subp-status-eq overlay 'busy)
        (user-error "Cannot retract a busy section"))))))

(defun fstar-subp--status-face (status lax)
  "Get face for STATUS, optionally with LAX modifier."
  (intern (format "fstar-subp-overlay-%s-%sface" (symbol-name status)
                  (if lax "lax-" ""))))

(defun fstar-subp-set-status (overlay status)
  "Set status of OVERLAY to STATUS."
  (fstar-assert (memq status fstar-subp-statuses))
  (let* ((inhibit-read-only t)
         (lax (overlay-get overlay 'fstar-subp--lax))
         (help-echo (propertize " " 'fstar-subp--legend (current-buffer))))
    (overlay-put overlay 'fstar-subp-status status)
    (overlay-put overlay 'priority -1)
    (overlay-put overlay 'help-echo help-echo)
    (overlay-put overlay 'face (fstar-subp--status-face status lax))
    (overlay-put overlay 'insert-in-front-hooks '(fstar-subp-overlay-attempt-modification))
    (overlay-put overlay 'modification-hooks '(fstar-subp-overlay-attempt-modification))))

(defun fstar-subp-process-overlay (overlay)
  "Send the contents of OVERLAY to the underlying F* process."
  (fstar-assert (not (fstar-subp--busy-p)))
  (fstar-subp-start)
  (fstar-subp-set-status overlay 'busy)
  (let ((lax (overlay-get overlay 'fstar-subp--lax)))
    (fstar-subp-push-region
     (overlay-start overlay) (overlay-end overlay) (if lax 'lax 'full)
     (apply-partially #'fstar-subp--overlay-continuation overlay))))

(defun fstar-subp--set-queue-timer ()
  "Set the queue timer for the current buffer."
  (unless fstar-subp--queue-timer
    (setq fstar-subp--queue-timer
          (run-with-timer 0 nil #'fstar-subp-process-queue (current-buffer)))))

(defun fstar-subp-process-queue (buffer)
  "Process the next pending overlay of BUFFER, if any."
  (with-current-buffer buffer
    (setq fstar-subp--queue-timer nil)
    (fstar-subp-start)
    (unless (fstar-subp--busy-p)
      (-if-let* ((overlay (car-safe (fstar-subp-tracking-overlays 'pending))))
          (progn (fstar-log 'info "Processing queue")
                 (fstar-subp-process-overlay overlay))
        (fstar-log 'info "Queue is empty (%d overlays)" (length (fstar-subp-tracking-overlays)))))))

;;; ;; Advancing and retracting

(defun fstar-subp-unprocessed-beginning ()
  "Find the beginning of the unprocessed buffer area."
  (or (cl-loop for overlay in (fstar-subp-tracking-overlays)
               maximize (overlay-end overlay))
      (point-min)))

(defun fstar-skip-spaces-backwards-from (point)
  "Go to POINT, skip spaces backwards, and return position."
  (save-excursion
    (goto-char point)
    (skip-chars-backward "\n\r\t ")
    (point)))

(defun fstar-subp-enqueue-until (end &optional no-error)
  "Mark region up to END busy, and enqueue the newly created overlay.

If NO-ERROR is set, do not report an error if the region is empty."
  (fstar-subp-start)
  (let ((beg (fstar-subp-unprocessed-beginning))
        (end (fstar-skip-spaces-backwards-from end)))
    (when (eq (char-after end) ?\n)
      (cl-incf end))
    (fstar-assert (cl-loop for overlay in (overlays-in beg end)
                      never (fstar-subp-tracking-overlay-p overlay)))
    (if (<= end beg)
        (unless no-error
          (user-error "Nothing more to process!"))
      (let ((overlay (make-overlay beg end (current-buffer) nil nil)))
        (fstar-subp-remove-orphaned-issue-overlays (point-min) (point-max))
        (overlay-put overlay 'fstar-subp--lax fstar-subp--lax)
        (fstar-subp-set-status overlay 'pending)
        (fstar-subp--set-queue-timer)))))

(defcustom fstar-subp-block-sep
  (concat "\\(?:\\'\\|\n\\(?:[[:space:]]*\n\\)+"
          "\\(?:" fstar-syntax-block-start-re
          "\\|" fstar-comment-start-skip "\\)\\)")
  "Regular expression used when looking for source blocks."
  :group 'fstar
  :type 'string
  :safe #'stringp)

(defun fstar-subp-skip-comments-and-whitespace ()
  "Skip over comments and whitespace."
  (while (or (> (skip-chars-forward " \r\n\t") 0)
             (comment-forward 1))))

(defun fstar-subp-next-block-sep (bound)
  "Find the next block separator before BOUND.
Ignores separators found in comments."
  (let (pos)
    (save-excursion
      (while (and (null pos) (re-search-forward fstar-subp-block-sep bound t))
        (goto-char (point-at-bol))
        (unless (fstar-in-comment-p)
          (setq pos (point)))))
    (when pos
      (goto-char pos))))

(defun fstar-subp--next-unprocessed-start (n)
  "Find the start of the at most (N + 1)th unprocessed block."
  (save-excursion
    (goto-char (fstar-subp-unprocessed-beginning))
    (dotimes (_ n)
      (fstar-subp-skip-comments-and-whitespace)
      (fstar-subp-next-block-sep nil))
    (point)))

(defun fstar-subp-goto-beginning-of-unprocessed ()
  "Go to beginning of unprocessed region."
  (interactive)
  (goto-char (fstar-subp-unprocessed-beginning)))

(defun fstar-subp-advance-next ()
  "Process buffer until `fstar-subp-block-sep'."
  (interactive)
  (fstar-subp-start)
  (-if-let* ((next-start (fstar-subp--next-unprocessed-start 1)))
      (fstar-subp-enqueue-until (goto-char next-start))
    (user-error "Cannot find a full block to process")))

(defun fstar-subp-pop-overlay (overlay)
  "Remove overlay OVERLAY and issue the corresponding `pop' command."
  ;; F* might be busy, but not with overlays
  (fstar-assert (null (fstar-subp-tracking-overlays 'busy)))
  (delete-overlay overlay)
  (fstar-subp--pop))

(defun fstar-subp-retract-one (overlay)
  "Retract OVERLAY, with some error checking."
  (cond
   ((not (fstar-subp-live-p)) (user-error "F* subprocess not started"))
   ((not overlay) (user-error "Nothing to retract"))
   ((fstar-subp-status-eq overlay 'pending) (delete-overlay overlay))
   ((fstar-subp-status-eq overlay 'busy) (user-error "Cannot retract busy region"))
   ((fstar-subp-status-eq overlay 'processed) (fstar-subp-pop-overlay overlay))))

(defun fstar-subp-retract-last ()
  "Retract last processed block."
  (interactive)
  (fstar-subp-retract-one (car-safe (last (fstar-subp-tracking-overlays)))))

(defun fstar-subp-retract-until (pos)
  "Retract blocks until POS is in unprocessed region."
  (cl-loop for overlay in (reverse (fstar-subp-tracking-overlays))
           when (> (overlay-end overlay) pos) ;; End point is not inclusive
           do (fstar-subp-retract-one overlay)))

(defun fstar-subp-advance-until (pos)
  "Submit or retract blocks to/from prover until POS."
  (fstar-subp-start)
  (save-excursion
    (goto-char (fstar-subp-unprocessed-beginning))
    (let ((found (cl-loop do (fstar-subp-skip-comments-and-whitespace)
                          while (and (< (point) pos) (fstar-subp-next-block-sep pos))
                          do (fstar-subp-enqueue-until (point))
                          collect (point))))
      (fstar-subp-enqueue-until pos found))))

(defun fstar-subp-reload-to-point (pos lax)
  "Retract everything and process (possibly in LAX mode) again to POS."
  (interactive (list (point) (not (null current-prefix-arg))))
  (fstar-subp-start)
  (fstar-subp-retract-until (point-min))
  (let ((fstar-subp--lax lax))
    (fstar-subp-advance-until pos)))

(defun fstar-subp-advance-or-retract-to-point (&optional arg)
  "Advance or retract proof state to reach point.

With prefix argument ARG, when advancing, do not split region
into blocks; process it as one large block instead."
  (interactive "P")
  (fstar-subp-start)
  (let ((limit (fstar-subp-unprocessed-beginning)))
    (cond
     ((<= (point) limit) (fstar-subp-retract-until (point)))
     ((>  (point) limit) (if (consp arg)
                             (fstar-subp-enqueue-until (point))
                           (fstar-subp-advance-until (point)))))))

(defun fstar-subp-advance-or-retract-to-point-lax (&optional arg)
  "Like `fstar-subp-advance-or-retract-to-point' with ARG, in lax mode."
  (interactive "P")
  (let ((fstar-subp--lax t))
    (fstar-subp-advance-or-retract-to-point arg)))

(defun fstar-subp-advance-to-point-max-lax (&optional arg)
  "Like `fstar-subp-advance-or-retract-to-point' on `point-max', in lax mode.
Pass ARG to `fstar-subp-advance-or-retract-to-point'."
  (interactive "P")
  (let ((fstar-subp--lax t))
    (save-excursion
      (goto-char (point-max))
      (fstar-subp-advance-or-retract-to-point arg))))

;;; ;; Features based on subp

;;; ;; ;; Dynamic Flycheck

(defun fstar-subp--can-run-flycheck ()
  "Check whether it's a reasonable time to start a syntax check."
  ;; FIXME check that there are no pending overlays ?
  (fstar-subp-available-p))

(defun fstar-subp--make-flycheck-issue (issue)
  "Convert an F* ISSUE to a Flycheck issue."
  (-when-let* ((loc (car (fstar-issue-locs issue))))
    (flycheck-error-new-at
     (fstar-location-line-from loc)
     (1+ (fstar-location-col-from loc))
     (fstar-issue-level issue)
     (fstar-issue-message issue)
     :checker 'fstar-interactive
     :filename (fstar-location-filename loc))))

(defun fstar-subp--flycheck-continuation (callback status response)
  "Forward results of Flycheck check (STATUS and RESPONSE) to CALLBACK."
  (if (eq status 'interrupted)
      (funcall callback 'interrupted nil)
    (let* ((raw-issues (fstar-subp-parse-issues response))
           (issues (mapcar #'fstar-subp-cleanup-issue raw-issues)))
      (fstar-log 'info "Highlighting Flycheck issues: %S" issues)
      (funcall callback 'finished
               (delq nil (mapcar #'fstar-subp--make-flycheck-issue issues))))))

(defun fstar-subp--start-syntax-check (_checker callback)
  "Start a light syntax check; pass results to CALLBACK."
  (if (fstar-subp--can-run-flycheck)
      (let ((beg (fstar-subp-unprocessed-beginning))
            (end (fstar-subp--next-unprocessed-start 3))) ;; FIXME customize number
        (if (< beg end)
            (fstar-subp-peek-region
             beg end 'lax ;; FIXME make configurable
             (apply-partially #'fstar-subp--flycheck-continuation callback))
          (funcall callback 'finished nil)))
    (funcall callback 'interrupted nil)))

(flycheck-define-generic-checker 'fstar-interactive
  "Flycheck checker for F*'s interactive mode.
This checker uses the F* subprocess to do light real-time
checking of the first few chunks of the unprocessed region of the
buffer."
  :start #'fstar-subp--start-syntax-check
  :modes '(fstar-mode)
  :verify (lambda (checker) (append (fstar--flycheck-verify-enabled checker)
                               (fstar--flycheck-verify-subp-available)))
  :predicate (lambda ()
               (and (fstar--has-feature 'peek)
                    (fstar--flycheck-enabled 'fstar-interactive)
                    (fstar-subp--can-run-flycheck))))

(add-to-list 'flycheck-checkers 'fstar-interactive)

;;; ;; ;; Lookup queries

(defun fstar-subp--positional-lookup-query (pos fields)
  "Prepare a header for an info query at POS with FIELDS."
  (declare (indent 1))
  (if (fstar--has-feature 'json-subp)
      (make-fstar-subp-query
       :query "lookup"
       :args `(("symbol" . ,(or (fstar--fqn-at-point pos) ""))
               ("requested-info" . ,fields)
               ("location" .
                (("filename" . "<input>")
                 ("line" . ,(line-number-at-pos pos))
                 ("column" . ,(fstar-subp--column-number-at-pos pos))))))
    (if (fstar--has-feature 'info-includes-symbol)
        (format "#info %s <input> %d %d"
                (or (fstar--fqn-at-point pos) "")
                (line-number-at-pos pos)
                (fstar-subp--column-number-at-pos pos))
      (format "#info <input> %d %d"
              (line-number-at-pos pos)
              (fstar-subp--column-number-at-pos pos)))))

(defconst fstar-subp-legacy--info-response-header-regex
  "^(defined at \\(.+?\\)(\\([0-9]+\\),\\([0-9]+\\)-\\([0-9]+\\),\\([0-9]+\\))) *\\([^ ]+\\) *: +")

(defconst fstar-subp-legacy--info-response-body-regex
  "\\([^\0]+?\\)\\(?:#doc \\([^\0]+?\\)\\)?\\'")

(cl-defstruct fstar-lookup-result
  name def-loc type doc def)

(defun fstar-lookup-result-sig (info &optional help-kbd)
  "Format signature of INFO.
When HELP-KBD is non nil and info includes a docstring, suggest
to use HELP-KBD to show documentation."
  (concat (fstar-highlight-string (format "%s: %s"
                                     (fstar-lookup-result-name info)
                                     (fstar-lookup-result-type info)))
          (and help-kbd (fstar-lookup-result-doc info)
               (substitute-command-keys (format " (%s for help)" help-kbd)))))

(defun fstar-lookup-result-def-loc-str (info)
  "Format a location information from INFO."
  (fstar--loc-to-string (fstar-lookup-result-def-loc info)))

(defun fstar-lookup-result-docstring (info)
  "Format docstring of INFO, if any."
  (-when-let* ((doc (fstar-lookup-result-doc info)))
    (fstar--highlight-docstring doc)))

(defun fstar-subp-legacy--parse-info (response)
  "Parse info structure from RESPONSE."
  (when (string-match fstar-subp-legacy--info-response-header-regex response)
    (pcase-let* ((body (substring response (match-end 0)))
                 (`(,file ,start-r ,start-c ,end-r ,end-c ,name)
                  (mapcar #'fstar--string-trim
                          (fstar--match-strings-no-properties
                           '(1 2 3 4 5 6) response)))
                 (`(,type ,doc)
                  (and (string-match fstar-subp-legacy--info-response-body-regex body)
                       (mapcar #'fstar--string-trim (fstar--match-strings-no-properties
                                                '(1 2) body)))))
      (make-fstar-lookup-result
       :name name
       :def-loc (make-fstar-location
                 :filename file
                 :line-from (string-to-number start-r)
                 :line-to (string-to-number end-r)
                 :col-from (string-to-number start-c)
                 :col-to (string-to-number end-c))
       :type (and type (fstar--unparens type))
       :doc (fstar--string-trim doc)
       :def nil))))

(defun fstar-subp-json--parse-defined-at (defined-at)
  "Parse DEFINED-AT part of lookup result."
  (when (fstar--lispify-null defined-at)
    (let-alist defined-at
      (make-fstar-location
       :filename .fname
       :line-from (elt .beg 0)
       :col-from (elt .beg 1)
       :line-to (elt .end 0)
       :col-to (elt .end 1)))))

(defun fstar-subp-json--parse-info (json)
  "Parse info structure from JSON."
  (let-alist json
    (make-fstar-lookup-result
     :name .name
     :def-loc (fstar-subp-json--parse-defined-at .defined-at)
     :type (fstar--unparens (fstar--lispify-null .type)) ;; FIXME remove once F* is fixed
     :doc (fstar--string-trim (fstar--lispify-null .documentation))
     :def (fstar--unparens (fstar--lispify-null .definition)))))

(defun fstar-subp--pos-check-wrapper (pos continuation)
  "Construct a continuation that runs CONTINUATION if point is POS.
Otherwise, call CONTINUATION with nil.  If POS is nil, the POS
check is ignored."
  (declare (indent 1))
  (lambda (status response)
    (if (or (null pos) (eq (point) pos))
        (funcall continuation status response)
      (funcall continuation 'interrupted nil))))

(defun fstar-subp--lookup-wrapper (pos continuation)
  "Handle the results of a lookup query at POS.
If response is valid, forward results to CONTINUATION.  With nil POS, this
function can also handle results of position-less lookup queries."
  (declare (indent 1))
  (fstar-subp--pos-check-wrapper pos
    (lambda (status response)
      (-if-let* ((info (and (eq status 'success)
                            (if (fstar--has-feature 'json-subp)
                                (fstar-subp-json--parse-info response)
                              (fstar-subp-legacy--parse-info response)))))
          (funcall continuation info)
        (funcall continuation nil)))))

(defun fstar--eldoc-continuation (continuation info)
  "Pass highlighted type information from INFO to CONTINUATION."
  (when info
    (funcall continuation (fstar-lookup-result-sig info "\\[fstar-doc]"))))

(defun fstar--eldoc-function ()
  "Compute an eldoc string for current point.
Briefly tries to get results synchronously to reduce flicker, and
then returns nil (in that case, results are displayed
asynchronously after the fact)."
  (-if-let* ((hole-info (get-text-property (point) 'fstar--match-var-type)))
      (format "This hole has type `%s'" (car hole-info))
    (when (and (fstar--has-feature 'lookup) (fstar-subp-available-p)
               (not (fstar-subp--in-issue-p (point))))
      (let* ((query (fstar-subp--positional-lookup-query (point)
                      '(type documentation))) ;; Needs doc for C-c C-s C-d hint
             (retv (fstar-subp--query-and-wait query 0.01)))
        (pcase retv
          (`(t . (,status ,results))
           (funcall (fstar-subp--lookup-wrapper (point)
                      (apply-partially #'fstar--eldoc-continuation #'identity))
                    status results))
          (`(needs-callback . ,_)
           (setf (cdr retv)
                 (fstar-subp--lookup-wrapper (point)
                   (apply-partially #'fstar--eldoc-continuation #'eldoc-message)))
           nil))))))

(defun fstar--eldoc-truncate-message (fn &rest args)
  "Forward ARGS to FN within scope of binding for `message-truncate-lines'."
  (if (derived-mode-p 'fstar-mode)
      (let ((message-truncate-lines t))
        (apply fn args))
    (apply fn args)))

(defun fstar-setup-eldoc ()
  "Set up eldoc support."
  ;; Add-function doesn't work on 'eldoc-documentation-function in Emacs < 25,
  ;; due to the default value being nil instead of `ignore'.
  (setq-local eldoc-documentation-function #'fstar--eldoc-function)
  ;; A better default, since it's pretty crucial in fstar-mode
  (setq-local eldoc-idle-delay 0.1)
  ;; LATER the following should move to yasnippet itself
  (eldoc-add-command 'yas-next-field-or-maybe-expand 'yas-prev-field
                     'yas-expand 'yas-expand-from-keymap
                     'yas-expand-from-trigger-key)
  (when (fboundp 'advice-add)
    (advice-add 'eldoc-message :around #'fstar--eldoc-truncate-message))
  (eldoc-mode))

(defun fstar-teardown-eldoc ()
  "Tear down eldoc support."
  (kill-local-variable 'eldoc-documentation-function)
  (when (fboundp 'advice-remove)
    (advice-remove 'eldoc-message #'fstar--eldoc-truncate-message)))

;;; ;; ;; Doc at point

(defconst fstar--doc-buffer-name "*fstar: doc*")
(push fstar--doc-buffer-name fstar--all-temp-buffer-names)

(defun fstar--doc-buffer-populate-1 (title value)
  "Insert TITLE and VALUE to current buffer."
  (declare (indent 1))
  (when value
    (insert "\n\n" (propertize title 'face '(:height 0.9))
            "\n" (fstar--indent-str value 2))))

(defun fstar--doc-buffer-populate (info)
  "Compute contents of doc buffer from INFO."
  (let* ((doc (fstar-lookup-result-doc info))
         (type (fstar-lookup-result-type info))
         (def (fstar-lookup-result-def info))
         (name (fstar-lookup-result-name info))
         (def-loc (fstar-lookup-result-def-loc-str info))
         (title (fstar--propertize-title name))
         (subtitle (propertize def-loc 'face '(:height 0.9))))
    (save-excursion
      (insert title "\n" subtitle)
      (fstar--doc-buffer-populate-1 "Type"
        (and type (fstar-highlight-string type)))
      (fstar--doc-buffer-populate-1 "Definition"
        (and def (fstar-highlight-string def)))
      (fstar--doc-buffer-populate-1 "Documentation"
        (and doc (fstar--highlight-docstring (fstar--unwrap-paragraphs doc)))))))

(defun fstar--doc-at-point-continuation (info)
  "Show documentation and type in INFO."
  (if info
      (with-help-window fstar--doc-buffer-name
        (with-current-buffer standard-output
          (fstar--doc-buffer-populate info)))
    (message "No information found.")))

(defun fstar-doc-at-point-dwim ()
  "Show documentation of identifier at point, if any."
  (interactive)
  (unless (and (eq last-command this-command)
               (fstar--hide-buffer fstar--doc-buffer-name))
    (fstar-subp--ensure-available #'user-error 'lookup/documentation)
    (fstar-subp--query (fstar-subp--positional-lookup-query (point)
                    '(type defined-at documentation))
                  (fstar-subp--lookup-wrapper (point)
                    #'fstar--doc-at-point-continuation))))

(defun fstar-doc (id)
  "Show information and documentation about ID.
Interactively, prompt for ID."
  (interactive '(interactive))
  (fstar-subp--ensure-available #'user-error 'lookup/documentation)
  (when (eq id 'interactive)
    (setq id (fstar--read-string "Show docs for%s: " (fstar--fqn-at-point))))
  (fstar-subp--query (fstar-subp--positionless-lookup-query id
                  '(type defined-at documentation))
                (fstar-subp--lookup-wrapper (point)
                  #'fstar--doc-at-point-continuation)))

;;; ;; ;; Print

(defun fstar-print (id)
  "Show definition of ID.
Interactively, prompt for ID."
  (interactive '(interactive))
  (fstar-subp--ensure-available #'user-error 'lookup/definition)
  (when (eq id 'interactive)
    (setq id (fstar--read-string "Show definition of%s: " (fstar--fqn-at-point))))
  (fstar-subp--query (fstar-subp--positionless-lookup-query id
                  '(type defined-at documentation definition))
                (fstar-subp--lookup-wrapper (point)
                  #'fstar--doc-at-point-continuation)))

;;; ;; ;; Insert a match

(defun fstar--split-match-var-annot (str)
  "Split var name and <<<>>> type annotation in STR."
  (save-match-data
    (if (string-match "\\(.+?\\)<<<\\(.+?\\)>>>" str)
        (let ((var (match-string 1 str))
              (type (match-string 2 str)))
          (cons var (list type)))
      (cons str nil))))

(defun fstar--prepare-match-snippet (snippet)
  "Prepare SNIPPET: add numbers and replace type annotations."
  (let ((counter 0))
    (replace-regexp-in-string
     "\\$\\(?:{\\(.+?\\)}\\|\\$\\)"
     (lambda (match)
       (pcase-let* ((name (or (match-string-no-properties 1 match) ""))
                    (`(,name . ,type) (fstar--split-match-var-annot name)))
         (propertize (format "${%d:%s}" (cl-incf counter) name)
                     'fstar--match-var-type type)))
     snippet t t)))

(defun fstar--format-one-branch (branch)
  "Format a single match BRANCH."
  (format "| %s -> $$" (replace-regexp-in-string "\\`(\\(.*\\))\\'" "\\1" branch)))

(defun fstar--insert-match-continuation (type response)
  "Handle RESPONSE to a `show-match' query.
TYPE is used in error messages"
  (-if-let* ((branches (and response
                            (if (fstar--has-feature 'json-subp)
                                response
                              (split-string response "\n")))))
      (let* ((name-str (car branches))
             (branch-strs (mapconcat #'fstar--format-one-branch (cdr branches) "\n"))
             (match (format "match ${%s} with\n%s" name-str branch-strs))
             (indented (fstar--indent-str match (current-column) t)))
        (let ((yas-indent-line nil))
          (fstar--expand-snippet (fstar--prepare-match-snippet indented))))
    (message "No match found for type `%s'." type)))

(defun fstar--read-type-name ()
  "Read a type name."
  (read-string "Type to match on \
(‘list’, ‘either’, ‘option (nat * _)’, …): "))

(defun fstar-subp--show-match-query (type)
  "Prepare a `show-match' query for TYPE."
  (setq type (replace-regexp-in-string "\n" " " type))
  (if (fstar--has-feature 'json-subp)
      (make-fstar-subp-query
       :query "show-match"
       :args `(("type" . ,type)))
    (format "#show-match %s" type)))

(defun fstar-insert-match (type)
  "Insert a match on TYPE at point."
  (interactive '(interactive))
  (fstar-subp--ensure-available #'user-error 'show-match)
  (when (eq type 'interactive)
    (setq type (fstar--read-type-name)))
  (fstar-subp--query (fstar-subp--show-match-query type)
                (fstar-subp--pos-check-wrapper (point)
                  (apply-partially #'fstar--insert-match-continuation type))))

(defun fstar--destruct-var-continuation (from to type status response)
  "Replace FROM..TO (with TYPE) with match from RESPONSE.
STATUS is the original query's status."
  (pcase (and (eq status 'success) (split-string response "\n"))
    (`nil
     (message "No match found for type `%s'." type))
    (`(,_name ,branch)
     (remove-text-properties from to '(fstar--match-var-type nil))
     (let ((yas-indent-line nil)
           (snip (fstar--prepare-match-snippet branch)))
       (fstar--expand-snippet snip from to)
       (fstar--refresh-eldoc)))
    (_
     (message "Can't destruct `%s' in place (it has more than one constructor)" type))))

(defun fstar--destruct-match-var-1 (from to type)
  "Replace FROM..TO by pattern matching on TYPE."
  (fstar-subp--ensure-available #'user-error 'show-match)
  (fstar-subp--query
   (fstar-subp--show-match-query type)
   (fstar-subp--pos-check-wrapper (point)
     (apply-partially #'fstar--destruct-var-continuation from to type))))

(defun fstar--destruct-match-var-at-point ()
  "Destruct match variable inserted with `fstar-insert-match'."
  (pcase (fstar--property-range (point) 'fstar--match-var-type)
    (`(,from ,to (,type))
     (fstar--destruct-match-var-1 from to type)
     t)))

(defun fstar-insert-match-dwim ()
  "Insert a match, or destruct the identifier at point.
This works in two steps: the first invocation prompts for a type
name and inserts a full match.  Subsequent invocations (on
variables of the constructors of the first match) just destruct
that variable."
  (interactive)
  (if (region-active-p)
      (fstar--destruct-match-var-1 (region-beginning) (region-end) (fstar--read-type-name))
    (or (fstar--destruct-match-var-at-point)
        (fstar-insert-match (fstar--read-type-name)))))

;;; ;; ;; Eval

(defconst fstar-subp--eval-all-rules '("beta" "delta" "iota" "zeta"))

(defun fstar-subp--eval-rule-to-char (rule)
  "Convert RULE to a single char."
  (pcase rule
    ("beta" "β")
    ("delta" "δ")
    ("iota" "ι")
    ("zeta" "ζ")
    (r (user-error "Unknown reduction rule %s" r))))

(defun fstar--reduction-arrow (arrow rules)
  "Annotate ARROW with RULES."
  (concat (propertize arrow 'face 'minibuffer-prompt)
          (propertize rules ;;'display '(raise -0.3)
                      'face '((:height 0.7) minibuffer-prompt))))

(defun fstar-subp--eval-continuation (term rules status response)
  "Handle results (STATUS, RESPONSE) of evaluating TERM with RULES."
  (setq rules (mapconcat #'fstar-subp--eval-rule-to-char rules ""))
  (pcase status
    (`success (message "%s%s%s%s%s"
                       (fstar-highlight-string term)
                       (if (string-match-p "\n" term) "\n" " ")
                       (fstar--reduction-arrow "↓" rules)
                       (if (string-match-p "\n" response) "\n" " ")
                       (fstar-highlight-string (fstar--unparens response))))
    (`failure (message "Evaluation of [%s] failed: %s"
                       (fstar-highlight-string term)
                       (string-trim response)))))

(defun fstar-subp--eval-query (term rules)
  "Prepare a `compute' query for TERM with specific reduction RULES."
  (make-fstar-subp-query :query "compute"
                         :args `(("term" . ,term)
                                 ("rules" . ,rules))))

(defun fstar-eval (term rules)
  "Reduce TERM with RULES in F* subprocess and display the result.
Interactively, use the current region or prompt for a term.  With
a prefix argument, prompt for rules as well."
  (interactive (list 'interactive (if current-prefix-arg 'interactive)))
  (fstar-subp--ensure-available #'user-error 'compute)

  (when (eq rules 'interactive)
    (setq rules (completing-read-multiple
                 "Reduction rules (comma-separated): "
                 fstar-subp--eval-all-rules nil t nil nil nil)))
  (setq rules (or rules fstar-subp--eval-all-rules))

  (when (eq term 'interactive)
    (setq term
          (fstar--read-string
           (format "Term to %s-reduce%%s: " (mapconcat #'fstar-subp--eval-rule-to-char rules ""))
           (cond
            ((region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end)))
            ((eq (char-before) (if (fstar-in-comment-p) ?\] ?\)))
             (buffer-substring-no-properties
              (1- (point)) (save-excursion (backward-list) (1+ (point)))))
            (t (fstar--fqn-at-point))))))
  (setq term (string-trim term))

  (fstar-subp--query (fstar-subp--eval-query term rules)
                (apply-partially #'fstar-subp--eval-continuation term rules)))

(defun fstar-eval-custom ()
  "List `fstar-eval-custom', but always prompt for rules."
  (interactive)
  (fstar-eval 'interactive 'interactive))

;;; ;; ;; Search

(defconst fstar--search-buffer-name "*fstar: search*")
(push fstar--search-buffer-name fstar--all-temp-buffer-names)

(defun fstar-subp--search-insert-result (result)
  "Insert formatted RESULT of search."
  (let-alist result
    (insert (propertize .lid 'face 'font-lock-function-name-face)
            (propertize  "\n" 'line-height `(nil . 1.5) 'line-spacing 0.2))
    (let ((from (point))
          (type (fstar--unparens .type)))
      (insert (fstar-highlight-string (fstar--indent-str type 2)))
      (font-lock-append-text-property from (point) 'face '(:height 0.9)))))

(defun fstar-subp--search-continuation (terms status response)
  "Handle results (STATUS, RESPONSE) of searching for TERMS."
  (pcase status
    (`success
     (with-help-window fstar--doc-buffer-name
       (with-current-buffer standard-output
         (visual-line-mode)
         (when (fboundp 'adaptive-wrap-prefix-mode)
           (adaptive-wrap-prefix-mode))
         (dolist (result response)
           (fstar-subp--search-insert-result result)
           (insert "\n")))))
    (`failure
     (message "Search for [%s] failed: %s" terms (string-trim response)))))

(defun fstar-subp--search-query (terms)
  "Prepare a `search' terms for TERMS."
  (make-fstar-subp-query :query "search"
                         :args `(("terms" . ,terms))))

(defun fstar-search (terms)
  "Query F* subprocess for matches against TERMS and display the results.
Interactively, prompt for terms.  Repeating this command hides
the search buffer."
  (interactive '(interactive))
  (fstar-subp--ensure-available #'user-error 'compute)
  (when (eq terms 'interactive)
    (setq terms (fstar--read-string "Search terms%s: " (fstar--fqn-at-point))))
  (setq terms (string-trim terms))
  (fstar-subp--query (fstar-subp--search-query terms)
                (apply-partially #'fstar-subp--search-continuation terms)))

;;; ;; ;; Jump to definition

(defun fstar--jump-to-definition-continuation (display-action info)
  "Jump to position in INFO.
DISPLAY-ACTION indicates how: nil means in the current window;
`window' means in a side window."
  (-if-let* ((def-loc (and info (fstar-lookup-result-def-loc info))))
      (let* ((target-fname (fstar-location-filename def-loc)))
        (if (equal target-fname "<input>")
            (setq target-fname buffer-file-name))
        (fstar--navigate-to target-fname
                            (fstar-location-line-from def-loc)
                            (fstar-location-col-from def-loc)
                            display-action))
    (message "No definition found")))

(defun fstar-jump-to-definition-1 (pos disp)
  "Jump to definition of identifier at POS, if any.
DISP should be nil (display in same window) or
`window' (display in a side window)."
  (fstar-subp--ensure-available #'user-error 'lookup)
  (fstar-subp--query (fstar-subp--positional-lookup-query pos
                  '(defined-at))
                (fstar-subp--lookup-wrapper pos
                  (apply-partially #'fstar--jump-to-definition-continuation disp))))

(defun fstar-jump-to-definition ()
  "Jump to definition of identifier at point, if any."
  (interactive)
  (fstar-jump-to-definition-1 (point) nil))

(defun fstar-jump-to-definition-other-window ()
  "Jump to definition of identifier at point, if any."
  (interactive)
  (fstar-jump-to-definition-1 (point) 'window))

(defun fstar-jump-to-definition-other-frame ()
  "Jump to definition of identifier at point, if any."
  (interactive)
  (fstar-jump-to-definition-1 (point) 'frame))

;;; ;; ;; Jump to dependency

(defconst fstar--visit-dependency-buffer-name "*fstar: dependencies*")
(push fstar--visit-dependency-buffer-name fstar--all-temp-buffer-names)

(defun fstar-subp--visit-dependency-insert (source-buf deps)
  "Insert information about DEPS of SOURCE-BUF in current buffer."
  (setq deps (sort deps #'string<))
  (let ((title (format "Dependencies of %s" (buffer-name source-buf))))
    (insert (fstar--propertize-title title) "\n\n"))
  (dolist (fname deps)
    (insert "  ")
    (insert-text-button fname 'fstar--target fname
                        'face 'default 'follow-link t
                        'action 'fstar--visit-link-target)
    (insert "\n")))

(defun fstar-subp--visit-dependency-continuation (source-buf status response)
  "Let user jump to one of the dependencies in RESPONSE.
SOURCE-BUF indicates where the query was started from.  STATUS is
the original query's status."
  (-if-let* ((deps (and (eq status 'success)
                        (let-alist response .loaded-dependencies)))
             (help-window-select t))
      (with-help-window fstar--visit-dependency-buffer-name
        (with-current-buffer standard-output
          (fstar-subp--visit-dependency-insert source-buf deps)
          (goto-char (point-min))
          (search-forward "\n\n  " nil t) ;; Find first entry
          (set-marker help-window-point-marker (point))))
    (message "Query `describe-repl' failed")))

(defun fstar-subp--describe-repl-query ()
  "Prepare a `describe-repl' query."
  (make-fstar-subp-query :query "describe-repl" :args nil))

(defun fstar-visit-dependency ()
  "Jump to a file that the current file depends on."
  (interactive)
  (fstar-subp--ensure-available #'user-error 'describe-repl)
  (fstar-subp--query (fstar-subp--describe-repl-query)
                (fstar-subp--pos-check-wrapper (point)
                  (apply-partially #'fstar-subp--visit-dependency-continuation
                                   (current-buffer)))))

;;; ;; ;; Quick-peek

(defun fstar--quick-peek-continuation (info)
  "Show type from INFO in inline pop-up."
  (if info
      (let ((segments (delq nil (list (fstar-lookup-result-sig info)
                                      (fstar-lookup-result-docstring info)))))
        (quick-peek-show (mapconcat #'identity segments "\n\n")
                         (car (with-syntax-table fstar--fqn-at-point-syntax-table
                                (bounds-of-thing-at-point 'symbol)))))
    (message "No definition found")))

(defun fstar-quick-peek ()
  "Toggle inline window showing type of identifier at point."
  (interactive)
  (fstar-subp--ensure-available #'user-error 'lookup)
  (when (= (quick-peek-hide) 0)
    (fstar-subp--query (fstar-subp--positional-lookup-query (point)
                    '(type documentation))
                  (fstar-subp--lookup-wrapper (point)
                    #'fstar--quick-peek-continuation))))

;;; ;; ;; Company

(defun fstar-subp--completion-query (prefix)
  "Prepare a `completions' query from PREFIX."
  (fstar-assert (not (string-match-p " " prefix)))
  (if (fstar--has-feature 'json-subp)
      (make-fstar-subp-query
       :query "autocomplete"
       :args `(("partial-symbol" . ,prefix)))
    (format "#completions %s #" prefix)))

(defun fstar-subp-company-legacy--prepare-candidate (line)
  "Extract a candidate from LINE."
  (pcase (split-string line " ")
    (`(,match-end ,ns ,candidate . ,_)
     (setq match-end (string-to-number match-end))
     (propertize candidate 'match match-end 'ns ns))))

(defun fstar-subp-company-json--prepare-candidate (record)
  "Extract a candidate from RECORD."
  (pcase-let* ((`(,match-end ,ns ,candidate) record))
    (propertize candidate 'match match-end 'ns ns)))

(defun fstar-subp-company--candidates-continuation (callback status response)
  "Handle the results (STATUS, RESPONSE) of an `autocomplete' query.
Return (CALLBACK CANDIDATES)."
  (pcase status
    ((or `failure `interrupted)
     (funcall callback nil))
    (`success
     (save-match-data
       (funcall callback
                (if (fstar--has-feature 'json-subp)
                    (mapcar #'fstar-subp-company-json--prepare-candidate response)
                  (delq nil (mapcar #'fstar-subp-company-legacy--prepare-candidate
                                    (split-string response "\n")))))))))

(defun fstar-subp--positionless-lookup-query (symbol fields)
  "Prepare a header for an info query for SYMBOL with FIELDS."
  (declare (indent 1))
  (when (equal symbol "")
    (user-error "Looking up an empty name"))
  (if (fstar--has-feature 'json-subp)
      (make-fstar-subp-query
       :query "lookup"
       :args `(("symbol" . ,symbol)
               ("requested-info" . ,fields)))
    (format "#info %s" symbol)))

(defun fstar-subp-company--candidate-fqn (candidate)
  "Compute the fully qualified name of CANDIDATE."
  (let* ((ns (get-text-property 0 'ns candidate)))
    (if (string= ns "") candidate
      (concat ns "." candidate))))

(defun fstar-subp-company--async-lookup (candidate fields continuation)
  "Pass info FIELDS about CANDIDATE to CONTINUATION.
If F* is busy, call CONTINUATION directly with symbol `busy'."
  (declare (indent 2))
  (if (fstar-subp-available-p)
      (fstar-subp--query
       (fstar-subp--positionless-lookup-query
           (fstar-subp-company--candidate-fqn candidate) fields)
       (fstar-subp--lookup-wrapper nil continuation))
    (funcall continuation 'busy)))

(defun fstar-subp-company--meta-continuation (callback info)
  "Forward type INFO to CALLBACK.
CALLBACK is the company-mode asynchronous meta callback."
  (funcall callback (pcase info
                      (`nil nil)
                      (`busy "F* subprocess unavailable")
                      (_ (fstar-lookup-result-sig info "\\[company-show-doc-buffer]")))))

(defun fstar-subp-company--async-meta (candidate callback)
  "Find type of CANDIDATE and pass it to CALLBACK."
  (fstar-subp-company--async-lookup candidate '(type)
    (apply-partially #'fstar-subp-company--meta-continuation callback)))

(defun fstar-subp-company--doc-buffer-continuation (callback info)
  "Forward documentation INFO to CALLBACK.
CALLBACK is the company-mode asynchronous doc-buffer callback."
  (funcall callback (when (fstar-lookup-result-p info)
                      (with-current-buffer
                          (get-buffer-create "*company-documentation*")
                        (erase-buffer)
                        (fstar--doc-buffer-populate info)
                        (current-buffer)))))

(defun fstar-subp-company--async-doc-buffer (candidate callback)
  "Find documentation of CANDIDATE and pass it to CALLBACK."
  (fstar-subp-company--async-lookup candidate '(type defined-at documentation)
    (apply-partially #'fstar-subp-company--doc-buffer-continuation callback)))

(defun fstar-subp-company--quickhelp-continuation (callback info)
  "Forward documentation INFO to CALLBACK.
CALLBACK is the company-mode asynchronous quickhelp callback."
  (funcall callback (or (and (fstar-lookup-result-p info)
                             (fstar-lookup-result-doc info))
                        "")))

(defun fstar-subp-company--async-quickhelp (candidate callback)
  "Find documentation of CANDIDATE and pass it to CALLBACK."
  (fstar-subp-company--async-lookup candidate '(documentation)
    (apply-partially #'fstar-subp-company--quickhelp-continuation callback)))

(defun fstar-subp-company--location-continuation (callback info)
  "Forward type INFO to CALLBACK.
CALLBACK is the company-mode asynchronous meta callback."
  (-if-let* ((def-loc (and (fstar-lookup-result-p info)
                           (fstar-lookup-result-def-loc info))))
      (pcase-let* ((fname (fstar-location-filename def-loc)))
        (funcall callback (if (string= fname "<input>")
                              (cons (current-buffer)
                                    (fstar-location-beg-offset def-loc))
                            (cons fname (fstar-location-line-from def-loc)))))
    (funcall callback nil)))

(defun fstar-subp-company--async-location (candidate callback)
  "Find location of CANDIDATE and pass it to CALLBACK."
  (fstar-subp-company--async-lookup candidate '(defined-at)
    (apply-partially #'fstar-subp-company--location-continuation callback)))

(defun fstar-subp-company-candidates (prefix)
  "Compute candidates for PREFIX.
Briefly tries to get results synchronously to reduce flicker (see
URL https://github.com/company-mode/company-mode/issues/654), and
then returns an :async cons, as required by company-mode."
  (let ((retv (fstar-subp--query-and-wait (fstar-subp--completion-query prefix) 0.03)))
    (pcase retv
      (`(t . (,status ,results))
       (fstar-subp-company--candidates-continuation #'identity status results))
      (`(needs-callback . ,_)
       `(:async . ,(lambda (cb)
                     (setf (cdr retv)
                           (apply-partially
                            #'fstar-subp-company--candidates-continuation cb))))))))

(defun fstar-subp-company-backend (command &optional arg &rest _)
  "Company backend for F*.
Candidates are provided by the F* subprocess.
COMMAND, ARG: see `company-backends'."
  (interactive '(interactive))
  ;; (fstar-log 'info "fstar-subp-company-backend: %S %S" command arg)
  (when (fstar--has-feature 'autocomplete)
    (pcase command
      (`interactive
       (company-begin-backend #'fstar-subp-company-backend))
      (`prefix
       (when (fstar-subp-available-p)
         (with-syntax-table fstar--fqn-at-point-syntax-table
           (-when-let* ((prefix (company-grab-symbol)))
             (substring-no-properties prefix)))))
      (`candidates
       (fstar-subp-company-candidates arg))
      (`meta
       `(:async . ,(apply-partially #'fstar-subp-company--async-meta arg)))
      (`doc-buffer
       `(:async . ,(apply-partially #'fstar-subp-company--async-doc-buffer arg)))
      (`quickhelp-string
       `(:async . ,(apply-partially #'fstar-subp-company--async-quickhelp arg)))
      (`location
       `(:async . ,(apply-partially #'fstar-subp-company--async-location arg)))
      (`sorted t)
      (`no-cache t)
      (`duplicates nil)
      (`match (get-text-property 0 'match arg))
      (`annotation (get-text-property 0 'ns arg)))))

(defun fstar-setup-company ()
  "Set up Company support."
  (setq-local company-backends
              (cons #'fstar-subp-company-backend company-backends))
  (company-mode))

(defun fstar-teardown-company ()
  "Tear down Company support."
  (kill-local-variable 'company-backends)
  (company-mode -1))


(defun fstar-setup-company-defaults ()
  "Set up Company support."
  (when (fboundp 'company-quickhelp-local-mode)
    (company-quickhelp-local-mode 1))
  (setq-local company-idle-delay 0.01)
  (setq-local company-tooltip-align-annotations t)
  (setq-local company-abort-manual-when-too-short t))

(defun fstar-teardown-company-defaults ()
  "Tear down Company support."
  (when (fboundp 'company-quickhelp-local-mode)
    (company-quickhelp-local-mode -1))
  (kill-local-variable 'company-idle-delay)
  (kill-local-variable 'company-tooltip-align-annotations)
  (kill-local-variable 'company-abort-manual-when-too-short))

;;; ;; ;; Options list

(defconst fstar--list-options-buffer-name "*fstar: options*")
(push fstar--list-options-buffer-name fstar--all-temp-buffer-names)

(defconst fstar--options-true (propertize "true" 'face 'success))
(defconst fstar--options-false (propertize "false" 'face 'error))
(defconst fstar--options-unset (propertize "unset" 'face 'warning))

(defun fstar-subp--option-val-to-string (val)
  "Format VAL for display."
  (cond
   ((eq val t) fstar--options-true)
   ((eq val :json-null) fstar--options-unset)
   ((eq val :json-false) fstar--options-false)
   ((numberp val) (number-to-string val))
   ((stringp val) (propertize (prin1-to-string val) 'face 'font-lock-string-face))
   ((listp val)
    (concat "[" (mapconcat #'fstar-subp--option-val-to-string val " ") "]"))
   (t (warn "Unexpected value %S" val)
      (format "%S" val))))

(defun fstar-subp--is-default (opt-info)
  "Check if OPT-INFO has its val equal to its default."
  (let-alist opt-info
    (equal .value .default)))

(defun fstar-subp--list-options-1 (display-default sp1 sp2 nl opt-info)
  "Insert a row of the option table.
OPT-INFO js a JSON object representing with information about an
F* option; DISPLAY-DEFAULT says whether default values should be
printed.  SP1, SP2, and NL are spacers."
  (let-alist opt-info
    (let ((doc (fstar--lispify-null .documentation))
          (doc-face '(font-lock-comment-face (:height 0.7))))
      (setq doc (string-trim (or doc "")))
      (setq doc (replace-regexp-in-string " *(default.*)" "" doc t t))
      (setq doc (replace-regexp-in-string " *\n+ *" " " doc t t))
      (when (eq doc "") (setq doc "(undocumented)"))
      (insert "  " .name sp1 (fstar-subp--option-val-to-string .value)
              (if (not display-default) "\n"
                (concat sp2 (fstar-subp--option-val-to-string .default) "\n"))
              "  " (propertize doc 'face doc-face) nl))))

(defun fstar-subp--list-options-continuation-1 (options display-default title)
  "Print a batch of OPTIONS under TITLE.
With DISPLAY-DEFAULT, also show default values."
  (declare (indent 2))
  (let ((sp1 (concat (propertize " " 'display '(space :align-to 30)) " "))
        (sp2 (concat (propertize " " 'display '(space :align-to 60)) " "))
        (nl (propertize "\n" 'line-spacing 0.4))
        (hdr-face '(bold underline)))
    (insert (fstar--propertize-title title) "\n\n")
    (insert "  " (propertize "Name" 'face hdr-face)
            sp1 (propertize "Value" 'face hdr-face)
            (if (not display-default) nl
              (concat sp2 (propertize "Default value" 'face hdr-face) nl)))
    (dolist (opt-info options)
      (fstar-subp--list-options-1 display-default sp1 sp2 nl opt-info))))

(defun fstar-subp--list-options-continuation (source-buf status response)
  "Let user jump to one of the dependencies in RESPONSE.
SOURCE-BUF indicates where the query was started from.  STATUS is
the original query's status."
  (if (eq status 'success)
      (let-alist response
        (with-help-window fstar--list-options-buffer-name
          (with-current-buffer standard-output
            (let* ((options (cl-sort .options #'string-lessp
                                     :key (lambda (k) (cdr (assoc "name" k)))))
                   (grouped (-group-by #'fstar-subp--is-default options)))
              (fstar-subp--list-options-continuation-1 (cdr (assq nil grouped)) t
                (format "Options set in %s" (buffer-name source-buf)))
              (fstar-subp--list-options-continuation-1 (cdr (assq t grouped)) nil
                "\nUnchanged options")))))
    (message "Query `describe-repl' failed")))

(defun fstar-list-options ()
  "Show information about the current process."
  (interactive)
  (fstar-subp--ensure-available #'user-error 'describe-repl)
  (fstar-subp--query (fstar-subp--describe-repl-query)
                (fstar-subp--pos-check-wrapper (point)
                  (apply-partially #'fstar-subp--list-options-continuation
                                   (current-buffer)))))

;;; ;; ;; Busy spinner

(defvar-local fstar--spin-timer nil)

(defvar-local fstar--spin-counter 0)

(defcustom fstar-spin-theme
  "⍟✪"
  "Which theme to use in indicating that F* is busy."
  :type '(choice (const "⍟✪")
                 (const "●✪")
                 (const "☆★")
                 (const "●○")
                 (const "❂✪")
                 (const "🌠✪")
                 (const "✩✭✮")
                 (const "🟀🟄🟉✶🟎✹")
                 (const "🟃🟇🟍✵🟑🟔")
                 (const "🟃🟀🟇🟄🟍✶✵🟎🟔🟓")
                 (string :tag "Custom string")))

(defun fstar--spin-cancel ()
  "Cancel spin timer."
  (when fstar--spin-timer
    (cancel-timer fstar--spin-timer)
    (setq-local fstar--spin-timer nil)))

(defun fstar--spin-tick (buffer timer)
  "Update fstar-mode's mode-line indicator in BUFFER.
TIMER is the timer that caused this event to fire."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((icon nil))
          (cond
           ((and fstar--spin-timer (fstar-subp--busy-p))
            (setq fstar--spin-counter (mod (1+ fstar--spin-counter) (length fstar-spin-theme)))
            (setq icon (substring fstar-spin-theme
                                  fstar--spin-counter (1+ fstar--spin-counter))))
           (t
            (setq fstar--spin-counter -1)
            (setq icon "✪")))
          (setq icon (compose-string icon 0 1 (format "\t%c\t" (aref icon 0))))
          (setq-local mode-name `("F" ,icon))
          (force-mode-line-update)))
    (cancel-timer timer)))

(defun fstar-setup-spinner ()
  "Enable dynamic F* icon."
  (let* ((timer nil)
         (buf (current-buffer))
         (ticker (lambda () (fstar--spin-tick buf timer))))
    (setq timer (run-with-timer 0 0.5 ticker))
    (setq-local fstar--spin-timer timer)))

(defun fstar-teardown-spinner ()
  "Disable dynamic F* icon."
  (fstar--spin-cancel))

;;; ;; Starting the F* subprocess

(defun fstar--check-executable (path prog-name var-name)
  "Check if PATH exists and is executable.
PROG-NAME and VAR-NAME are used in error messages."
  (unless (and path (file-exists-p path))
    (user-error "%s (“%s”) not found%s" prog-name path var-name
                (if var-name (format "; please adjust `%s'" var-name) "")))
  (unless (file-executable-p path)
    (user-error "%s (“%s”) not executable%s" prog-name path
                (if var-name (format "; please check `%s'" var-name) ""))))

(defun fstar-find-executable (prog prog-name &optional var-name)
  "Compute the absolute path to PROG.
Check that the binary exists and is executable; if not, raise an
error referring to PROG as PROG-NAME and VAR-NAME."
  (let* ((local (not (fstar--remote-p)))
         (abs (if local (executable-find prog) prog)))
    (if local
        (fstar--check-executable (or abs prog) prog-name var-name)
      (with-parsed-tramp-file-name buffer-file-name nil
        (or (tramp-find-executable v abs nil)
            (fstar--check-executable
             (tramp-make-tramp-file-name method user host abs)
             (concat "Remote " prog-name) var-name))))
    abs))

(defun fstar-subp-buffer-killed ()
  "Kill F* process associated to current buffer."
  (-when-let* ((proc (get-buffer-process (current-buffer))))
    (run-with-timer 0 nil #'fstar-subp-kill-proc proc)))

(defun fstar-subp-make-buffer ()
  "Create a buffer for the F* subprocess."
  (let ((vernum fstar--vernum)
        (features fstar--features))
    (with-current-buffer (generate-new-buffer
                          (format " *F* interactive for %s*" (buffer-name)))
      (add-hook 'kill-buffer-hook #'fstar-subp-buffer-killed t t)
      (setq fstar--vernum vernum fstar--features features)
      (buffer-disable-undo)
      (current-buffer))))

(defcustom fstar-subp-prover-args nil
  "Used for computing arguments to pass to F* in interactive mode.

If set to a string, that string is considered to be a single
argument to pass to F*.  If set to a list of strings, each element
of the list is passed to F*.  If set to a function, that function
is called in the current buffer without arguments, and expected
to produce a string or a list of strings.

Some examples:

- (setq fstar-subp-prover-args \"--ab\") results in F* being
called as ‘fstar.exe --in --ab’.

- (setq fstar-subp-prover-args \\='(\"--ab\" \"--cd\")) results in
F* being called as ‘fstar.exe --in --ab --cd’.

- (setq fstar-subp-prover-args (lambda () \\='(\"--ab\" \"--cd\")))
results in F* being called as ‘fstar.exe --in --ab --cd’.

To debug unexpected behaviors with this variable, try
evaluating (fstar-subp-get-prover-args).  Note that passing
multiple arguments as one string will not work: you should use
\\='(\"--aa\" \"--bb\"), not \"--aa --bb\""
  :group 'fstar
  :type '(repeat string))

(defun fstar-subp-find-fstar ()
  "Find path to F* executable."
  (fstar-find-executable fstar-executable "F*" 'fstar-executable))

(defun fstar-subp-find-smt-solver ()
  "Find path to SMT solver executable."
  (fstar-find-executable fstar-smt-executable "SMT solver" 'fstar-smt-executable))

(defun fstar-subp--parse-prover-args ()
  "Translate `fstar-subp-prover-args' into a list of strings."
  (let ((args (fstar--resolve-fn-value fstar-subp-prover-args)))
    (cond ((listp args) args)
          ((stringp args) (list args))
          (t (user-error "Interpreting `fstar-subp-prover-args' \
led to invalid value [%s]" args)))))

(defun fstar-subp-get-prover-args ()
  "Compute F*'s arguments.
Function is public to make it easier to debug `fstar-subp-prover-args'."
  (let ((smt-path (fstar--maybe-cygpath (fstar-subp-find-smt-solver)))
        (usr-args (fstar-subp--parse-prover-args))
        (ide-flag (if (fstar--has-feature 'json-subp) "--ide" "--in")))
    `(,(fstar-subp--buffer-file-name) ,ide-flag "--smt" ,smt-path ,@usr-args)))

(defun fstar-subp--buffer-file-name ()
  "Find name of current buffer, as sent to F*."
  (if (fstar--remote-p)
      (tramp-file-name-localname
       (tramp-dissect-file-name buffer-file-name))
    (fstar--maybe-cygpath buffer-file-name)))

(defun fstar-subp--start-process (buf prog args)
  "Start an F* subprocess PROG in BUF with ARGS."
  (apply #'start-file-process "F* interactive" buf prog args))

(defun fstar-subp-start ()
  "Start an F* subprocess attached to the current buffer, if none exists."
  (interactive)
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Can't start F* subprocess without a backing file (save this buffer first)"))
  (unless (process-live-p fstar-subp--process)
    (let ((f*-abs (fstar-subp-find-fstar)))
      (fstar--init-compatibility-layer f*-abs)
      (let* ((buf (fstar-subp-make-buffer))
             (process-connection-type nil)
             (tramp-process-connection-type nil)
             (args (fstar-subp-get-prover-args))
             (proc (fstar-subp--start-process buf f*-abs args)))
        (fstar-log 'info "")
        (fstar-log 'info "Started F* interactive: %S" (cons f*-abs args))
        (set-process-query-on-exit-flag proc nil)
        (set-process-filter proc #'fstar-subp-filter)
        (set-process-sentinel proc #'fstar-subp-sentinel)
        (set-process-coding-system proc 'utf-8 'utf-8)
        (process-put proc 'fstar-subp-source-buffer (current-buffer))
        (setq fstar-subp--process proc)))))

;;; ;; Keybindings

(defconst fstar-subp-keybindings-table
  '(("C-c C-n"        "C-S-n" fstar-subp-advance-next)
    ("C-c C-u"        "C-S-p" fstar-subp-retract-last)
    ("C-c C-p"        "C-S-p" fstar-subp-retract-last)
    ("C-c RET"        "C-S-i" fstar-subp-advance-or-retract-to-point)
    ("C-c <C-return>" "C-S-i" fstar-subp-advance-or-retract-to-point)
    ("C-c C-l"        "C-S-l" fstar-subp-advance-or-retract-to-point-lax)
    ("C-c C-."        "C-S-." fstar-subp-goto-beginning-of-unprocessed)
    ("C-c C-b"        "C-S-b" fstar-subp-advance-to-point-max-lax)
    ("C-c C-r"        "C-S-r" fstar-subp-reload-to-point)
    ("C-c C-x"        "C-M-c" fstar-subp-kill-one-or-many)
    ("C-c C-c"        "C-M-S-c" fstar-subp-kill-z3))
  "Proof-General and Atom bindings table.")

(defun fstar-subp-refresh-keybinding (bind target unbind)
  "Bind BIND to TARGET, and unbind UNBIND."
  (define-key fstar-mode-map (kbd bind) target)
  (define-key fstar-mode-map (kbd unbind) nil))

(defun fstar-subp-refresh-keybindings (style)
  "Adjust keybindings to match STYLE."
  (cl-loop for (pg atom target) in fstar-subp-keybindings-table
           do (pcase style
                (`pg   (fstar-subp-refresh-keybinding pg target atom))
                (`atom (fstar-subp-refresh-keybinding atom target pg))
                (other (user-error "Invalid keybinding style: %S" other)))))

(defun fstar-subp-set-keybinding-style (var style)
  "Set VAR to STYLE and update keybindings."
  (set-default var style)
  (fstar-subp-refresh-keybindings style))

(defcustom fstar-interactive-keybinding-style 'pg
  "Which style of bindings to use in F* interactive mode."
  :group 'fstar
  :set #'fstar-subp-set-keybinding-style
  :type '(choice (const :tag "Proof-General style bindings" pg)
                 (const :tag "Atom-style bindings" atom)))

;;; ;; Main fstar-subp entry point

(defun fstar-setup-interactive ()
  "Setup interactive F* mode."
  (fstar-subp-refresh-keybindings fstar-interactive-keybinding-style)
  (setq-local help-at-pt-display-when-idle t)
  (setq-local help-at-pt-timer-delay 0.2)
  (help-at-pt-cancel-timer)
  (help-at-pt-set-timer))

(defun fstar-teardown-interactive ()
  "Cleanup F* interactive mode."
  (help-at-pt-cancel-timer))


;;; Menu

(defun fstar-customize ()
  "Open `fstar-mode'\\='s customization menu."
  (interactive)
  (customize-group 'fstar))

(easy-menu-define fstar-mode-menu fstar-mode-map
  ;; Putting the menu in `fstar-mode-map' (a local map) ensures that it appears after
  ;; all global menus (File, Edit, …)
  #("F✪'s main menu" 1 2 (composition ((1 . "\t✪\t"))))
  '(#("F✪" 1 2 (composition ((1 . "\t✪\t"))))
    ("Navigation"
     ["Visit interface file"
      fstar-visit-interface-or-implementation :visible (not (fstar--visiting-interface-p))]
     ["Visit implementation file"
      fstar-visit-interface-or-implementation :visible (fstar--visiting-interface-p)]
     ["Visit a dependency of this file"
      fstar-visit-dependency :visible (fstar-subp-available-p)]
     ["Show an outline of this file"
      fstar-outline]
     ["Close all temporary F* windows"
      fstar-quit-windows])
    (#("F✪ subprocess" 1 2 (composition ((1 . "\t✪\t"))))
     [#("Start F✪ subprocess" 7 8 (composition ((1 . "\t✪\t"))))
      fstar-subp-start (not (process-live-p fstar-subp--process))]
     ["Interrupt Z3"
      fstar-subp-kill-z3 (fstar-subp--busy-p)]
     [#("Kill F✪ subprocess" 6 7 (composition ((1 . "\t✪\t"))))
      fstar-subp-kill-one-or-many (process-live-p fstar-subp--process)])
    ("Proof state"
     ["Typecheck next definition"
      fstar-subp-advance-next]
     ["Retract last definition"
      fstar-subp-retract-last]
     ["Typecheck everything up to point"
      fstar-subp-advance-or-retract-to-point]
     ["Typecheck everything up to point (lax)"
      fstar-subp-advance-or-retract-to-point-lax]
     ["Typecheck whole buffer (lax)"
      fstar-subp-advance-to-point-max-lax]
     ["Show current value of all F* options"
      fstar-list-options (fstar-subp-available-p)])
    ("Interactive queries"
     ["Evaluate an expression"
      fstar-eval (fstar-subp-available-p)]
     ["Evaluate an expression (with custom reduction rules)"
      fstar-eval-custom (fstar-subp-available-p)]
     ["Show type and docs of an identifier"
      fstar-doc (fstar-subp-available-p)]
     ["Show definition of an identifier"
      fstar-print (fstar-subp-available-p)]
     ["Search for functions or theorems"
      fstar-search (fstar-subp-available-p)]
     ["Quick peek"
      fstar-quick-peek (fstar-subp-available-p)])
    ("Utilities"
     ["Copy error message at point"
      fstar-copy-help-at-point])
    ["Configuration" fstar-customize]))

;;; Main mode

(defun fstar-setup-hooks ()
  "Setup hooks required by F*-mode."
  (add-hook 'before-revert-hook #'fstar-subp-kill nil t)
  (add-hook 'change-major-mode-hook #'fstar-mode-unload-function nil t)
  (add-hook 'before-revert-hook #'fstar-mode-unload-function nil t)
  (add-hook 'kill-buffer-hook #'fstar-mode-unload-function nil t))

(defun fstar-teardown-hooks ()
  "Remove hooks required by F*-mode."
  (remove-hook 'before-revert-hook #'fstar-subp-kill t)
  (remove-hook 'change-major-mode-hook #'fstar-mode-unload-function t)
  (remove-hook 'before-revert-hook #'fstar-mode-unload-function t)
  (remove-hook 'kill-buffer-hook #'fstar-mode-unload-function t))

(defun fstar-enable-disable (enable)
  "ENABLE or disable F* mode components."
  (dolist (module (cons 'hooks fstar-enabled-modules))
    (let* ((prefix (if enable "fstar-setup-" "fstar-teardown-"))
           (fsymb  (intern (concat prefix (symbol-name module)))))
      (when (fboundp fsymb)
        (funcall fsymb)))))

;;;###autoload
(define-derived-mode fstar-mode prog-mode "F✪"
  :syntax-table fstar-syntax-table
  (fstar-enable-disable t))

(defun fstar-mode-unload-function ()
  "Unload F* mode components."
  (fstar-enable-disable nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fsti?\\'" . fstar-mode))

;;; Footer

;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; End:

(provide 'fstar-mode)
;;; fstar-mode.el ends here
