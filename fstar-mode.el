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

(require 'cl)
(require 'dash)
(require 'flycheck)

;;; Group

(defgroup fstar nil
  "F* mode."
  :group 'languages)

;;; Flycheck

(flycheck-def-executable-var fstar "fstar.exe")

(defconst fstar-error-patterns
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
  :error-patterns fstar-error-patterns
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


(defun fstar-setup-prettify ()
  "Setup prettify-symbols for use with F*."
  (setq-local prettify-symbols-alist (append fstar-symbols-alist
                                             prettify-symbols-alist))
  (prettify-symbols-mode))

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
  (setq-local
   font-lock-defaults
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

;;; Interaction with the server

(defconst fstar-subp--success "ok")
(defconst fstar-subp--failure "nok")
(defconst fstar-subp--done "\n#done-")

(defconst fstar-subp--header "#push\n")
(defconst fstar-subp--footer "\n#end #done-ok #done-nok\n")

(defconst fstar-subp-statuses '(pending busy processed))

(defvar-local fstar-subp--process nil
  "Interactive F* process running in the background.")

(defvar-local fstar-subp--busy-now nil
  "Indicates which overlay the F* subprocess is currently processing, if any.")

(defface fstar-subp-overlay-pending-face
  '((t :background "grey"))
  "Face used to highlight pending sections of the buffer."
  :group 'fstar)

(defface fstar-subp-overlay-busy-face
  '((t :background "mediumorchid"))
  "Face used to highlight busy sections of the buffer."
  :group 'fstar)

(defface fstar-subp-overlay-processed-face
  '((t :background "darkslateblue"))
  "Face used to highlight processed sections of the buffer."
  :group 'fstar)

(defmacro fstar-subp-log (format &rest args)
  "Log a query or response.

FORMAT and ARGS are as in `message'."
  (declare (debug t))
  `(message ,format ,@args))

(defun fstar-subp-live-p (&optional proc)
  "Return t if the PROC is a live F* subprocess.

If PROC is nil, use the current buffer's `fstar-subp--process'."
  (setq proc (or proc fstar-subp--process))
  (and proc (process-live-p proc)))

(defun fstar-subp-killed (proc)
  "Clean up buffer after PROC was killed."
  (-when-let* ((procp proc)
               (buf   (process-buffer proc))
               (bufp  (buffer-live-p buf)))
    (kill-buffer buf))
  (fstar-subp-remove-overlays)
  (setq fstar-subp--busy-now nil
        fstar-subp--process nil))

(defun fstar-subp-kill ()
  "Kill F* subprocess and clean up buffer."
  (interactive)
  (when (fstar-subp-live-p)
    (kill-process fstar-subp--process)
    (accept-process-output fstar-subp--process))
  (fstar-subp-killed fstar-subp--process))

(defun fstar-subp-sentinel (proc _signal)
  "Hamdle signals from PROC."
  (when (not (process-live-p proc))
    (fstar-subp-killed proc)))

(defun fstar-subp-remove-overlays ()
  "Remove all F* overlays in the current buffer."
  (mapcar #'delete-overlay (fstar-subp-overlays)))

(defmacro fstar-subp-with-process-buffer (proc &rest body)
  "Move to PROC's buffer to eval BODY."
  (declare (indent defun) (debug t))
  `(-when-let* ((livep    (fstar-subp-live-p ,proc))
                (buf      (process-buffer ,proc))
                (buflivep (buffer-live-p buf)))
     (with-current-buffer buf
       ,@body)))

(defmacro fstar-subp-with-source-buffer (proc &rest body)
  "Move to parent buffer of PROC to eval BODY."
  (declare (indent defun) (debug t))
  `(-when-let* ((livep    (fstar-subp-live-p ,proc))
                (buf      (process-get ,proc 'fstar-subp-source-buffer))
                (buflivep (buffer-live-p buf)))
     (with-current-buffer buf
       ,@body)))

(defun fstar-subp-find-response (proc)
  "Find full response in PROC's buffer; handle it if found."
  (goto-char (point-min))
  (when (search-forward fstar-subp--done nil t)
    (let* ((status        (cond
                           ((looking-at fstar-subp--success) t)
                           ((looking-at fstar-subp--failure) nil)))
           (resp-beg      (point-min))
           (resp-end      (point-at-bol))
           (resp-real-end (point-at-eol))
           (response      (string-trim (buffer-substring resp-beg resp-end))))
      ;; string-trim is defined by flycheck if not present
      (fstar-subp-log "RESPONSE [%s] [%s]" status response)
      (delete-region resp-beg resp-real-end)
      (fstar-subp-with-source-buffer proc
        (fstar-subp-process-response status response)))))

(defun fstar-subp-process-response (status response)
  "Process output STATUS and RESPONSE from F* subprocess."
  (let* ((overlay fstar-subp--busy-now))
    (unless overlay
      (fstar-subp-kill)
      (error "Invalid state: Received output, but no region was busy"))
    (setq fstar-subp--busy-now nil)
    (pcase status
      (`t   (fstar-subp-handle-success response overlay))
      (`nil (fstar-subp-handle-failure response overlay))
      (_    (fstar-subp-kill)
            (error "Unknown status [%s] from F* subprocess (response was [%s])"
                   status response)))))

(defun fstar-subp-handle-success (response overlay)
  "Process success RESPONSE from F* subprocess for OVERLAY."
  (fstar-subp-set-status overlay 'processed)
  (run-with-timer 0 nil #'fstar-subp-process-queue))

(defun fstar-subp-parse-errors (response)
  "Parse RESPONSE into a list of errors."
  (flycheck-parse-with-patterns response 'fstar (current-buffer)))

(defun fstar-subp-realign-error (overlay-start-line err)
  "Use first line of overlay (OVERLAY-START-LINE) to realign error ERR."
  (when (string= (flycheck-error-filename err) "<input>")
    (setf (flycheck-error-filename err) (buffer-file-name)) ;; FIXME ensure we have a file name?
    (setf (flycheck-error-line err) (+ (flycheck-error-line err)
                                       (1- overlay-start-line))))
  err)

(defun fstar-subp-realign-errors (overlay errs)
  "Use beginning of OVERLAY to realign errors ERRS."
  (let ((start-line (line-number-at-pos (overlay-start overlay))))
    (mapcar (apply-partially #'fstar-subp-realign-error start-line) errs)))

(defun fstar-subp-handle-failure (response overlay)
  "Process failure RESPONSE from F* subprocess for OVERLAY."
  (-when-let* ((errors  (fstar-subp-errors response))
               (aligned (fstar-subp-realign-errors overlay errors))
               (first   (car aligned)))
    (message "%s" aligned)
    (fstar-subp-remove-unprocessed)))

(defun fstar-subp-status-eq (overlay status)
  "Check if OVERLAY has status STATUS."
  (eq (overlay-get overlay 'fstar-subp-status) status))

(defun fstar-subp-remove-unprocessed ()
  "Remove pending and busy overlays."
  (cl-loop for overlay in (fstar-subp-overlays)
           unless (fstar-subp-status-eq overlay 'processed)
           do (delete-overlay overlay)))

(defun fstar-subp-filter (proc string)
  "Handle PROC's output (STRING)."
  (when string
    (fstar-subp-log "OUTPUT [%s]" (replace-regexp-in-string "\n" " // " string t t))
    (fstar-subp-with-process-buffer proc
      (goto-char (point-max))
      (insert string)
      (fstar-subp-find-response proc))))

(defun fstar-subp-make-buffer ()
  "Create a buffer for the F* subprocess."
  (with-current-buffer (generate-new-buffer
                        (format " *F* interactive for %s" (buffer-name)))
    (buffer-disable-undo)
    (current-buffer)))

(defun fstar-subp-start ()
  "Start an F* subprocess attached to the current buffer, if none exists."
  (unless fstar-subp--process
    (let* ((buf (fstar-subp-make-buffer))
           (proc (start-process "F* interactive" buf
                                flycheck-fstar-executable "--in")))
      (set-process-filter proc #'fstar-subp-filter)
      (set-process-sentinel proc #'fstar-subp-sentinel)
      (process-put proc 'fstar-subp-source-buffer (current-buffer))
      (setq fstar-subp--process proc))))

(defun fstar-subp-send-region (beg end)
  "Send the region between BEG and END to the inferior F* process."
  (interactive "r")
  (fstar-subp-start)
  (let ((msg (concat fstar-subp--header
                     (buffer-substring-no-properties beg end)
                     fstar-subp--footer)))
    (fstar-subp-log "QUERY [%s]" msg)
    (process-send-string fstar-subp--process msg)))

(defun fstar-subp-overlay-p (overlay)
  "Return non-nil if OVERLAY is an fstar-subp overlay."
  (overlay-get overlay 'fstar-subp-status))

(defun fstar-subp-overlays (&optional status)
  "Find all fstar-subp overlays with status STATUS in the current buffer.

If STATUS is nil, return all fstar-subp overlays."
  (sort (cl-loop for overlay being the overlays of (current-buffer)
                 when (fstar-subp-overlay-p overlay)
                 when (or (not status) (fstar-subp-status-eq overlay status))
                 collect overlay)
        (lambda (o1 o2) (< (overlay-start o1) (overlay-start o2)))))

(defun fstar-subp-set-status (overlay status)
  "Set status of OVERLAY to STATUS."
  (assert (memq status fstar-subp-statuses))
  (let ((inhibit-read-only t)
        (face-name (concat "fstar-subp-overlay-" (symbol-name status) "-face")))
    (overlay-put overlay 'fstar-subp-status status)
    (overlay-put overlay 'face (intern face-name))
    (overlay-put overlay 'read-only t))) ;;FIXME: Read-only

(defun fstar-subp-process-overlay (overlay)
  "Send the contents of OVERLAY to the underlying F* process."
  (assert (not fstar-subp--busy-now))
  (fstar-subp-set-status overlay 'busy)
  (setq fstar-subp--busy-now overlay)
  (fstar-subp-send-region (overlay-start overlay) (overlay-end overlay)))

(defun fstar-subp-process-queue ()
  "Process the next pending overlay, if any."
  (unless fstar-subp--busy-now
    (-if-let* ((overlay (car-safe (fstar-subp-overlays 'pending))))
        (progn (fstar-subp-log "Processing queue")
               (fstar-subp-process-overlay overlay))
      (fstar-subp-log "Queue is empty"))))

(defun fstar-subp-unprocessed-beginning ()
  "Find the beginning of the unprocessed buffer area."
  (or (cl-loop for overlay in (fstar-subp-overlays)
               maximize (overlay-end overlay))
      (point-min)))

(defun fstar-skip-spaces-backwards-from (point)
  "Go to POINT, skip spaces backwards, and return position."
  (save-excursion
    (goto-char point)
    (skip-chars-backward "\n\r\t ")
    (point)))

(defun fstar-subp-enqueue-until (end)
  "Mark region up to END busy, and enqueue the newly created overlay."
  (let ((beg (fstar-subp-unprocessed-beginning)))
    (assert (cl-loop for overlay in (overlays-in beg end)
                     never (fstar-subp-overlay-p overlay)))
    ;; TODO skip backwards to push end back spaces
    (setq end (fstar-skip-spaces-backwards-from end))
    (when (<= end beg)
      (user-error "Nothing to process!"))
    (let ((overlay (make-overlay beg end (current-buffer) nil nil)))
      (fstar-subp-set-status overlay 'pending)
      (fstar-subp-process-queue))))

;;; Comment syntax

(defun fstar-syntactic-face-function-aux (_ _b _c in-string comment-depth _d _e _f comment-start-pos _g)
  "Choose face to display.

Arguments IN-STRING COMMENT-DEPTH and COMMENT-START-POS ar as in
`font-lock-syntactic-face-function'."
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
  "Set comment-related variables for F*."
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
