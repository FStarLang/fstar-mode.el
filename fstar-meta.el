;;; fstar-meta.el --- Custom commands and bindings for the F* mode

;; Author: Son HO <son.marc.ho@hotmail.com>

;;; Code:

;;; Imports

;;; Customization

;;; Constants
(defconst fem-log-buffer "*fstar-extended-debug*")

(defconst fem-message-prefix "[F*] ")
(defconst fem-tactic-message-prefix "[F*] TAC>> ")
(defconst fem-start-fstar-msg "\n%FEM:FSTAR_META:START%")
(defconst fem-end-fstar-msg "\n[F*] %FEM:FSTAR_META:END%")

(defconst fem-messages-buffer "*Messages*")

;; Small trick to solve the undo problems: we use temporary buffer names which
;; start with a ' ' so that emacs deactivates undo by default in those buffers,
;; preventing the insertion of problematic undo-boundary.
;; Note that for now we switch buffers "by hand" rather than using the emacs
;; macros like with-current-buffer because it leaves a trace which helps debugging.
(defconst fem-process-buffer1 " *fstar-temp-buffer1*")
(defconst fem-process-buffer2 " *fstar-temp-buffer2*")

(defconst fem-pos-marker "(*[_#%s#_]*) ")
(defvar fem-pos-marker-overlay nil)
(defvar fem-saved-pos nil)

;;; Type definitions

(cl-defstruct fem-pair
  fst snd)

(cl-defstruct fem-triple
  fst snd thd)

(cl-defstruct fem-subexpr
  "A parsed expression of the form 'let _ = _ in', '_;' or '_' (return value)"
  beg end ;; point delimiters
  is-let-in ;; is of the form: 'let _ = _ in'
  has-semicol ;; is of the form: '_;'
  )

(cl-defstruct fem-result
  "Results exported by F* to the *Messages* buffer"
  error pres posts)

;;; Debugging and errors

(define-error 'fstar-meta-parsing "Error while parsing F*")

(defvar fem-debug nil "If t debug mode is activated")

(defun fem-switch-debug ()
  (interactive)
  "Switch between debugging/non-debugging mode."
  (setq fem-debug (not fem-debug)))

(defun fem-log-msg (FORMAT-STRING &rest FORMAT-PARAMS)
  "Log a message in the log buffer.
Format FORMAT-PARAMS according to FORMAT-STRING."
  (apply #'message FORMAT-STRING FORMAT-PARAMS))

(defun fem-log-dbg (FORMAT-STRING &rest FORMAT-PARAMS)
  "Log a message in the log buffer if fem-debug is t.
Format FORMAT-PARAMS according to FORMAT-STRING."
  (when fem-debug (apply #'message FORMAT-STRING FORMAT-PARAMS)))

;;; Utilities

(defun fem-replace-in-string (FROM TO STR)
  "Replace FROM with TO in string STR."
  (replace-regexp-in-string (regexp-quote FROM) TO STR nil 'literal))

(defun fem-back-to-indentation-or-beginning ()
  "Switch between beginning of line or end of indentation."
  (interactive)
   (if (= (point) (progn (back-to-indentation) (point)))
       (beginning-of-line)))

(defun fem-current-line-is-whitespaces ()
  "Check if the current line is only made of spaces."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[ ]*$")))

(defun fem-current-line-is-comments-and-spaces ()
  "Check if the current line is only made of comments and spaces."
  (save-excursion
    (beginning-of-line)
    (fem-skip-comments-and-spaces t (point-at-eol))
    (= (point) (point-at-eol))))

(defun fem-count-lines-in-string (STR)
  "Count the number of lines in a string"
  (save-match-data
    (let (($num-lines 1) (pos 0))
      (while (string-match (regexp-quote "\n") STR pos)
        (setq pos (match-end 0))
        (setq $num-lines (+ $num-lines 1)))
      $num-lines)))

(defun fem-consume-string-forward (STR &optional NO_ERROR)
  "If the pointer looks at string STR, moves the pointer after it. Otherwise,
returns nil or raises an error depending on NO_ERROR."
  (if (looking-at-p (regexp-quote STR))
      (progn (forward-char (length STR)) t)
    (if NO_ERROR nil (error (format "fem-consume-string-forward %s failed" STR)))))      

(defun fem-insert-newline-term (TERM)
  "Insert a new line if the current one is not empty, then insert TERM."
  (interactive)
  (let (($indent-str nil))
    (if (fem-current-line-is-whitespaces) ()
      ;; Create the new line
      (end-of-line)
      (newline)
      (indent-according-to-mode))
    (insert TERM)))

(defun fem-previous-char-is-semicol-p (&optional POS)
  "Return t if the point before POS is ';'."
  (ignore-errors
    (= (char-before POS) ?;)))

(defun fem-next-char-is-semicol-p (&optional POS)
  "Return t if the point before POS is ';'."
  (ignore-errors
    (= (char-after POS) ?;)))

(defun fem-parse-next-sexp-p (&optional POS LIMIT)
  "Skip comments and spaces and parse the next sexp as a pair of positions.
Return nil if no sexp was found."
  (let (($limit (or LIMIT (point-max)))
        $p0)
    (goto-char (or POS (point)))
    (fem-skip-comments-and-spaces t $limit)
    (setq $p0 (point))
    ;; Ignore the errors: if can't parse a sexp, return nil
    (ignore-errors
      (forward-sexp)
      (make-fem-pair :fst $p0 :snd (point)))))

(defun fem-parse-next-sexp-as-string-p (&optional POS LIMIT)
  "Skip comments and spaces and parse the next sexp as a string.
Return nil if no sexp was found."
  (let ($sexp)
    (setq $sexp (fem-parse-next-sexp-p POS LIMIT))
    (if $sexp
        (buffer-substring-no-properties (fem-pair-fst $sexp) (fem-pair-snd $sexp))
      nil)))

(defun fem-parse-previous-sexp-p (&optional POS LIMIT)
  "Skip comments and spaces and parse the previous sexp as a pair of positions.
Return nil if no sexp was found."
  (let (($limit (or LIMIT (point-min)))
        $p0)
    (goto-char (or POS (point)))
    (fem-skip-comments-and-spaces nil $limit)
    (setq $p0 (point))
    ;; Ignore the errors: if can't parse a sexp, return nil
    (ignore-errors
      (backward-sexp)
      (make-fem-pair :fst (point) :snd $p0))))

(defun fem-parse-previous-sexp-as-string-p (&optional POS LIMIT)
  "Skip comments and spaces and parse the previous sexp as a string.
Return nil if no sexp was found."
  (let ($sexp)
    (setq $sexp (fem-parse-previous-sexp-p POS LIMIT))
    (if $sexp
        (buffer-substring-no-properties (fem-pair-fst $sexp) (fem-pair-snd $sexp))
      nil)))

;; TODO: this can be greatly improved by using more precise parsing primitives
;; - take example on fem-parse-control-flow
(defun fem-parse-previous-letb (&optional LIMIT)
  "Return a fem-pair delimiting the parsed let expression, nil otherwise.
The expression can also of the form: '...;', which is syntactic sugar for 'let _ = ... in;'"
  (let (($limit (or LIMIT (point-min)))
        ($p0 (point))
        ($p (point))
        ($p1 nil)
        ($continue t)
        ($in-cnt 0) ;; Count the number of 'in' encountered, to handle nested let expressions
        $parse-sexp
        $exp)
    (fem-log-dbg "[> fem-parse-previous-letb")
    ;; Find the end of expression delimiter
    (fem-skip-comments-and-spaces nil $limit)
    ;; Check if previous is ';': if so, find next occurrence of ';' or 'in'
    (if (fem-previous-char-is-semicol-p)
        (progn
          (fem-log-dbg "End delimiter is ';'")
          (backward-char 1)
          (while (and $continue (not (= (point) (point-min))))
            (setq $p (point))
            (fem-skip-comments-and-spaces nil $limit)
            (if (fem-previous-char-is-semicol-p)
                ;; Semicol: stop here, but ignore the comments
                (setq $p1 (point) $continue nil)
              ;; Otherwise: parse the next sexp
              (setq $exp (fem-parse-previous-sexp-as-string-p))
              (if (not $exp)
                  ;; Error: abort
                  (setq $continue nil)
                ;; Check if 'in' - note that we don't have to worry about nested let here
                (when (string-equal "in" $exp)
                  (setq $p1 $p $continue nil)))))
          ;; Return
          (if (not $p1)
              nil
            (goto-char $p1)
            (fem-skip-comments-and-spaces t)
            (setq $p1 (point))
            (if $p1 (make-fem-pair :fst $p1 :snd $p0) nil)))
      ;; Check if previous is 'in': if so find next occurrence of 'let'
      (setq $exp (fem-parse-previous-sexp-as-string-p))
      (if (and $exp (string-equal $exp "in"))
          (progn
          (fem-log-dbg "End delimiter is 'in'")
            (while $continue
              (fem-skip-comments-and-spaces nil $limit)
              (setq $exp (fem-parse-previous-sexp-as-string-p))
              (if (not $exp)
                  ;; Error: abort
                  (setq $continue nil)
                ;; Check if 'let'
                (fem-log-dbg (concat "Parsed [" $exp "]"))
                (cond
                 ;; If find a 'in': increment the counter
                 ((string-equal "in" $exp)
                  (setq $in-cnt (+ $in-cnt 1)))
                 ;; If find a 'let': stop if counter is 0, otherwise decrement it
                 ((string-equal "let" $exp)
                  (if (= $in-cnt 0)
                      (setq $p1 (point) $continue nil)
                    (setq $in-cnt (- $in-cnt 1))))
                 ;; Otherwise: do nothing
                 (t nil))))
            ;; Return
            (if $p1 (make-fem-pair :fst $p1 :snd $p0) nil))
        ;; Otherwise: return nil
        nil))))

(defun fem-insert-newline-term-smart-indent (TERM)
  "Insert a new line if the current one is not empty, then insert TERM."
  (interactive)
  (let (($indent-str nil)
        ($p0 (point))
        $p1
        $letb)
    ;; If the current line is empty: insert in this line
    (if (fem-current-line-is-whitespaces) ()
      ;; Go to the end of the line then move backward until we find some code
      (end-of-line)
      (fem-skip-comments-and-spaces nil)
      (setq $p1 (point))
      ;; Try to parse the expression
      (setq $letb (fem-parse-previous-letb))
      ;; If we managed to parse the expression: use the indent of the expression
      (if $letb (goto-char (fem-pair-fst $letb))
        ;; Otherwise, use the indent of the line where we were
        (goto-char $p1))
      ;; Compute the indent
      (beginning-of-line)
      (fem-skip-spaces t (point-at-eol))
      (setq $indent-str (make-string (- (point) (point-at-bol)) ? ))
      ;; Go to the original position
      (goto-char $p0)
      ;; Create the new line
      (end-of-line)
      (newline))
    ;; Insert
    (if $indent-str (insert $indent-str) (indent-according-to-mode))
    (insert TERM)))

(defun fem-newline-keep-indent ()
  "Insert a newline with an indentation equal to the current column position."
  (interactive)
  (let ($p $i)
    (setq $p (point))
    (back-to-indentation)
    (setq $i (- (point) (line-beginning-position)))
    (goto-char $p)
    (newline)
    (dotimes (i $i) (insert " "))
    $i))

(defun fem-empty-line ()
  "Delete all the characters on the current line.
Return the number of deleted characters."
  (interactive)
  (let ($p)
   (setq $p (- (line-end-position) (line-beginning-position)))
   (delete-region (line-beginning-position) (line-end-position))
   $p))

(defun fem-empty-delete-line ()
  "Remove all the characters on the line if not empty, delete the line otherwise."
  (interactive)
  (if (equal (line-beginning-position) (line-end-position))
      (progn (move-backward) (delete-char 1) 1) (fem-empty-line)))

(defun fem-delete-always-line ()
  "Delete the current line."
  (interactive)
  (let ($c)
    (if (equal (line-beginning-position) (line-end-position))
	(progn (move-backward) (delete-char 1) 1)
	(progn (setq $c (fem-empty-line))
	       (move-backward) (delete-char 1) (+ $c 1)))))

(defun fem-find-region-delimiters (ALLOW_SELECTION INCLUDE_CURRENT_LINE
                                   ABOVE_PARAGRAPH BELOW_PARAGRAPH &optional POS)
  "Find the delimiters for the region around the pointer.
Mostly works by moving forward/backward by a paragraph, remembering the positions we reached."
  (save-excursion
    (let ($p $p1 $p2)
      ;; Save the current point
      (when POS (goto-char POS))
      (setq $p (point))
      ;; Find the region delimiters (and move the pointer back to its original position):
      ;; First check if we need to use the selected region
      (if (and (use-region-p) ALLOW_SELECTION)
          ;; Use the selected region
          (setq $p1 (region-beginning) $p2 (region-end))
        ;; Compute a new region
        (progn
          ;; - beginning of region
          (progn (if ABOVE_PARAGRAPH (backward-paragraph)
                   (if INCLUDE_CURRENT_LINE (move-beginning-of-line ()) (move-end-of-line ())))
                 (setq $p1 (point)) (goto-char $p))
          ;; - end of region
          (progn (if BELOW_PARAGRAPH (forward-paragraph)
                   (if INCLUDE_CURRENT_LINE (move-end-of-line ()) (move-beginning-of-line ())))
                 (setq $p2 (point)) (goto-char $p))))
      (make-fem-pair :fst $p1 :snd $p2))))

(defun fem-apply-in-current-region (ACTION ALLOW_SELECTION INCLUDE_CURRENT_LINE
                                    ABOVE_PARAGRAPH BELOW_PARAGRAPH)
  "Apply the action given as argument to the region around the pointer.
The ACTION function should move the pointer back to its (equivalent) original position."
  (let ($delimiters $p1 $p2 $r)
    ;; Find the region delimiters
    (setq $delimiters (fem-find-region-delimiters ALLOW_SELECTION INCLUDE_CURRENT_LINE
                                                  ABOVE_PARAGRAPH BELOW_PARAGRAPH))
    (setq $p1 (fem-pair-fst $delimiters) $p2 (fem-pair-snd $delimiters))
    ;; Apply the action in the delimited region
    (save-restriction
      (narrow-to-region $p1 $p2)
      (setq $r (funcall ACTION)))
    ;; return the result of performing the action
    $r))

(defun fem-apply-in-current-line (ACTION)
  "Apply the ACTION given as argument to the current line.
The ACTION function should move the pointer back to its (equivalent) original position."
  (fem-apply-in-current-region ACTION nil t nil nil))

(defun fem-replace-all-in (FROM TO &optional IGNORE_COMMENTS FULL_SEXP BEG END)
  "Replace all the occurrences of FROM by TO.
Return the number of characters by which the pointer was shifted.
BEG and END delimit the region where to perform the replacement.
If IGNORE_COMMENTS is t, don't replace inside comments.
If FULL_SEXP, check if the term to replace is a sexpression before replacing it."
  (let (($p0 (point)) ;; original position
        ($p (point)) ;; current position
        ($shift 0) ;; number of characters by which we shift the original position
        ($length-dif (- (length TO) (length FROM))) ;; shift of one replacement
        ($beg (or BEG (point-min)))
        ($end (or END (point-max)))
        $replace
        $exp)
    ;; Replace all the occurrences of FROM
    (goto-char $beg)
    (while (and (< (point) $end) (search-forward FROM $end t))
      ;; Check if we need to replace
      (cond
       ;; Ignore comments
       ((and IGNORE_COMMENTS (fem-in-general-comment-p))
        (setq $replace nil))
       ;; Check if full sexp
       (FULL_SEXP
        (goto-char (match-beginning 0))
        (setq $exp (fem-sexp-at-p-as-string))
        (if $exp (setq $replace (string-equal $exp FROM))
          (setq $replace nil)))
       (t (setq $replace t)))
      (goto-char (match-end 0))
      ;; Replace          
      (when $replace
        (progn
          ;; Compute the pointer shift: if the current position is smaller or equal
          ;; to the original position with the current shift, add $length-dif
          ;; to the shift
          (setq $p (point))
          (when (<= $p (+ $p0 $shift)) (setq $shift (+ $shift $length-dif)))
          ;; Replace
          (replace-match TO))
        ;; Otherwise: just move
        (goto-char (match-end 0))))
    ;; Move to the shifted position and return the shift
    (goto-char (+ $p0 $shift))
    $shift))

(defun fem-replace-in-current-region (FROM TO IGNORE_COMMENTS FULL_SEXP
                                      ALLOW_SELECTION INCLUDE_CURRENT_LINE
                                      ABOVE_PARAGRAPH BELOW_PARAGRAPH)
  "Replace FROM by TO in the current region."
  (let (($r (apply-partially 'fem-replace-all-in FROM TO IGNORE_COMMENTS FULL_SEXP)))
    ;; Apply the replace function
    (fem-apply-in-current-region $r ALLOW_SELECTION INCLUDE_CURRENT_LINE
                             ABOVE_PARAGRAPH BELOW_PARAGRAPH)))

;;; General F* code management commands

(defun fem-switch-assert-assume-in-current-region (ALLOW_SELECTION INCLUDE_CURRENT_LINE
                                                   ABOVE_PARAGRAPH BELOW_PARAGRAPH)
  (interactive)
  "Switch between assertions and assumptions in the current region.
First check if there are assertions in the current region.
If so, replace them with assumptions.
Ohterwise, replace the assumptions with assumptions."
  (let ($p $p1 $p2 $keep-selection $has-asserts $replace $delimiters)
    ;; Find the region delimiters and restrain the region
    (setq $delimiters (fem-find-region-delimiters ALLOW_SELECTION INCLUDE_CURRENT_LINE
                                                  ABOVE_PARAGRAPH BELOW_PARAGRAPH))
    (setq $p1 (fem-pair-fst $delimiters)
          $p2 (fem-pair-snd $delimiters)
          $keep-selection (and (use-region-p) ALLOW_SELECTION))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (setq $p (point))
      ;; Check if there are assertions to know whether to replace assertions
      ;; by assumptions or the revert
      (goto-char (point-min))
      (setq $has-asserts (fem-search-forward-not-comment "assert" t nil))
      (goto-char $p)
      ;; Replace
      (if $has-asserts
          (progn
             (fem-replace-all-in "assert_norm" "assume(*norm*)" t t)
             (fem-replace-all-in "assert" "assume" t t))
           (progn
             (fem-replace-all-in "assume(*norm*)" "assert_norm" t nil)
             (fem-replace-all-in "assume" "assert" t t))))
      ;; Maintain the selection if there was one
    (when $keep-selection
      (setq deactivate-mark nil)
      (exchange-point-and-mark)
      (exchange-point-and-mark))))

(defun fem-switch-assert-assume-p-or-selection ()
  (interactive)
  "Switch the assertions/assumptiosn under the pointer, or in the active selection."
  (let ($p $passert $p1 $p2 $keep-selection $has-asserts $replace $delimiters)
    (setq $p (point))
    ;; Find the region delimiters and restrain the region
    (if (use-region-p)
        ;; Use selection
        (setq $p1 (region-beginning) $p2 (region-end) $keep-selection t)
      ;; Otherwise: look for the assertion/assumption under the pointer
      (setq $passert (fem-find-assert-assume-p))
      ;; If we couldn't find the assertion, try to move backward
      (when (not $passert)
        ;; Go to the left, check if we find a semicolon
        (fem-skip-comments-and-spaces nil (point-at-bol))
        ;; If there is a semicolon, ignore it
        (when (fem-previous-char-is-semicol-p)
          (backward-char))
        ;; Retry the search
        (setq $passert (fem-find-assert-assume-p)))
      ;; If still not found, move forward
      (when (not $passert)
        (goto-char $p)
        (fem-skip-comments-and-spaces t (point-at-eol))
        (setq $passert (fem-find-assert-assume-p)))
      ;; Use the assertion if we found one, otherwise use an empty search
      (if (not $passert)
          ;; Use an empty search
          (setq $p1 (point) $p2 (point))
        ;; Use the region containing the assertion - note that we include the
        ;; assertion parameter, because if we limit ourselves to the head
        ;; keyword, we might not replace an "assert(*norm*)"
        (setq $p1 (fem-pair-fst (fem-pair-fst $passert))
              $p2 (fem-pair-snd (fem-pair-snd $passert)))))
    (setq $p1 (min $p1 $p) $p2 (max $p2 $p))
    ;; Replace
    ;; Check if there are assertions to know whether to replace assertions
    ;; by assumptions or the revert
    (goto-char $p1)
    (setq $has-asserts (fem-search-forward-not-comment "assert" t $p2))
    (when (not $has-asserts)
      (goto-char $p1)
      (setq $has-asserts (fem-search-forward-not-comment "assert_norm" t $p2)))
    (goto-char $p)
    ;; Replace
    (if $has-asserts
        (progn
          (fem-replace-all-in "assert_norm" "assume(*norm*)" t t $p1 $p2)
          (fem-replace-all-in "assert" "assume" t t $p1 $p2))
      (progn
        (fem-replace-all-in "assume(*norm*)" "assert_norm" t nil $p1 $p2)
        (fem-replace-all-in "assume" "assert" t t $p1 $p2)))
    ;; Maintain the selection if there was one
    (when $keep-selection
      (setq deactivate-mark nil)
      (exchange-point-and-mark)
      (exchange-point-and-mark))))

(defun fem-switch-assert-assume-in-above-paragraph ()
  (interactive)
  "Switch between assertions/assumptions in the current paragraph."
  (fem-switch-assert-assume-in-current-region t t t nil))

(defun fem-switch-assert-assume-in-current-line ()
  (interactive)
  "Switch between assertions/assumptions in the current line."
  (fem-switch-assert-assume-in-current-region t t nil nil))

(defun fem-roll-delete-term (TERM FORWARD BEGIN END)
  (interactive)
  "Roll TERM around.
Used for the rolling admit technique.
Look for the last/first occurence of TERM in the region and ask the user
if he wants to delete it, if there is any. Delete the following semicolon if
there is any. Leave the pointer at its original position (before the command was
called).
Return a tuple: (found term, optional shift if term was deleted, deleted a semicolon)"
  (let ($p $s $f $r $semicol $opt_shift)
    (setq $s 0)
    ;; Retrieve the original position
    (setq $p (point))
    ;; Look for an admit
    (if FORWARD (setq $f (fem-search-forward-not-comment TERM nil END))
		(setq $f (fem-search-backward-not-comment TERM nil BEGIN)))
    ;; If there is an occurrence of TERM, ask for removal
    (when $f
      (when (y-or-n-p (concat "Remove this '" (concat TERM "'?")))
	(progn
	  (replace-match "")
	  (setq $r t)
          ;; Look for a semicolon to delete
          (when (char-equal ?\; (following-char))
            (progn
              (delete-char 1)
              (setq $semicol t)
              (when (not FORWARD) (setq $s 1))))
          ;; Delete the whole line if it is empty
	  (when (fem-current-line-is-whitespaces) (setq $s (fem-delete-always-line)))
	  ;; Compute the position shift
	  (when (not FORWARD) (setq $s (+ (length TERM) $s)))
	  )))
    ;; Go to the original position
    (goto-char (- $p $s))
    ;; Return the shift if we deleted a TERM
    (if $r (setq $opt_shift $s) (setq $opt_shift nil))
    ;; Return
    (list (cons 'found $f) (cons 'opt_shift $opt_shift) (cons 'semicol $semicol))))

(defun fem-roll-admit ()
  (interactive)
  "Roll an admit.
We start by looking for an admit after the cursor position, then before."
  (let ($p $p1 $p2 $s)
    ;; Save the current point
    (setq $p (point))
    ;; Find the region delimiters
    (progn (forward-paragraph) (setq $p2 (point))
	   (progn (goto-char $p) (backward-paragraph) (setq $p1 (point)))
	   (goto-char $p))
    ;; Delete forward
    (setq $s (fem-roll-delete-term "admit()" t $p1 $p2))
    ;; Delete backward
    (when (not (cdr (assoc 'opt_shift $s)))
      (setq $s (fem-roll-delete-term "admit()" nil $p1 $p2)))
    ;; Insert the admit
    (if (cdr (assoc 'semicol $s))
        (fem-insert-newline-term-smart-indent "admit();")
      (fem-insert-newline-term-smart-indent "admit()"))))

;;; Parsing commands

(defun fem-in-general-comment-p (&optional POS)
  "Return t is POS is in an F* comment."
  (save-restriction
    (or (fstar-in-comment-p POS) (fstar-in-literate-comment-p))))

(defun fem-search-forward-not-comment (STR &optional FULL_SEXP LIMIT)
    "Look for the first occurrence of STR not inside a comment.
Return t and move the pointer immediately after if found one.
Don't move the pointer and return nil otherwise.
If FULL_SEXP, look for the first occurrence which is a sexpression."
    (let (($p (point))
          $exp)
      (fstar--search-predicated-forward
       (lambda ()
         (if (fem-in-general-comment-p)
             nil
           (if (not FULL_SEXP)
               t
             (goto-char (match-beginning 0))
             (setq $exp (fem-sexp-at-p-as-string))
             (goto-char (match-end 0))
             (if $exp
                 (string-equal $exp STR)
               nil))))
       STR LIMIT)
      (not (= $p (point)))))

(defun fem-search-backward-not-comment (STR &optional FULL_SEXP LIMIT)
    "Look backward for the first occurrence of STR not inside a comment."
    (let (($p (point)) $exp)
      (fem-fstar--search-predicated-backward
       (lambda ()
         (if (fem-in-general-comment-p)
             nil
           (if (not FULL_SEXP)
               t
             (goto-char (match-beginning 0))
             (setq $exp (fem-sexp-at-p-as-string))
             (goto-char (match-end 0))
             (if $exp
                 (string-equal $exp STR)
               nil))))
       STR LIMIT)
      (not (= $p (point)))))

;; TODO: use forward-comment
(defun fem-skip-comment (FORWARD &optional LIMIT)
  "Move the cursor forward or backward until out of a comment.
Stop and don't fail if we reach the end of the buffer."
  (let ($stop)
    ;; Set the limit to the move
    (if FORWARD (setq $stop (or LIMIT (point-max)))
                (setq $stop (or LIMIT (point-min))))
    (cond
     ;; Inside a comment
     ((fstar-in-comment-p)
      (if FORWARD
          ;; Forward: go forward until we are out of the comment
          (while (and (fstar-in-comment-p) (< (point) $stop)) (forward-char))
        ;; Backward: we can use the parsing state to jump
        (goto-char (nth 8 (fstar--syntax-ppss (point))))))
     ;; Inside a literate comment
     ((fstar-in-literate-comment-p)
      (if FORWARD (if (search-forward "\n" $stop t) (point) (goto-char $stop))
        (if (search-backward "\n" $stop t) (point) (goto-char $stop))))
     (t (point)))))

(defun fem-is-at-comment-limit (FORWARD &optional LIMIT)
  "Check if the pointer is just before/after a comment symbol."
  (if FORWARD
      ;; If forward: the comments delimiters are always made of two characters
      ;; and we can't know if we are inside a comment unless we process those
      ;; two characters
      (progn
        (if (> (+ (point) 2) (or LIMIT (point-max))) nil
          (fstar-in-comment-p (+ (point) 2))))
      ;; If backward: we just need to go one character back
      (progn
        (if (< (- (point) 1) (or LIMIT (point-min))) nil
          (fstar-in-comment-p (- (point) 1))))))

(defun fem-skip-chars (FORWARD CHARS &optional LIMIT)
  "Move until the current character is not in CHARS.
FORWARD controls the direction, LIMIT delimits where to stop."
  (if FORWARD
      (skip-chars-forward CHARS LIMIT)
      (skip-chars-backward CHARS LIMIT)))

(defun fem-skip-spaces (FORWARD &optional LIMIT)
  "Move the cursor until there are no spaces.
FORWARD controls the direction, LIMIT delimits where to stop."
  (let ($continue $p1 $p2 $limit $reached-limit)
    (if FORWARD (setq $p1 (point) $p2 (or LIMIT (point-max)))
                (setq $p2 (point) $p1 (or LIMIT (point-min))))
    (if FORWARD (setq $limit $p2) (setq $limit $p1))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (fem-skip-chars FORWARD fstar--spaces))
    (point)))

(defun fem-skip-comments-and-spaces (FORWARD &optional LIMIT)
  "Move the cursor until we are out of a comment and there are no spaces.
FORWARD controls the direction, LIMIT delimits the region."
  (let ($continue $p1 $p2 $limit $reached-limit)
    (if FORWARD (setq $p1 (point) $p2 (or LIMIT (point-max)))
                (setq $p2 (point) $p1 (or LIMIT (point-min))))
    (if FORWARD (setq $limit $p2) (setq $limit $p1))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (setq $continue t)
      (while $continue
        (fem-skip-comment FORWARD LIMIT)
        (fem-skip-chars FORWARD fstar--spaces)
        (setq $reached-limit (= (point) $limit))
        (if $reached-limit (setq $continue nil)
          (if (fem-is-at-comment-limit FORWARD)
            (if FORWARD (forward-char 2) (backward-char 1))
            (setq $continue nil)))))
    (point)))

(defun fem-skip-forward-pragma (&optional LIMIT)
    "Skip a pragma instruction (#push-options, #pop-options...).
If we are at the beginning of a #push-options or #pop-options instruction,
move forward until we are out of it or reach LIMIT.
Don't move if there isn't such an instruction.
Returns the position where the pointer is left."
  (save-restriction
    (narrow-to-region (point) (if LIMIT LIMIT (point-max)))
    (let (($continue t) $go)
      (defun go (STR CONTINUE)
        (if (looking-at-p (regexp-quote STR))
            (progn (forward-char (length STR)) (setq $continue CONTINUE) t)
          nil))
      (cond ((go "#set-options" t) ())
            ((go "#reset-options" t) ())
            ((go "#push-options" t) ())
            ((go "#pop-options" nil) ())
            ((go "#restart-solver" nil) ())
            ((go "#light" nil) ())
            (t (setq $continue nil)))
      ;; Skip the parameters (the string) - note that there may be comments
      ;; between the pragma and the paramters
      (when $continue
        (fem-skip-comments-and-spaces t)
        (forward-sexp)))))

(defun fem-skip-forward-open-module ()
  "Skip an 'open ...' instruction.
Don't move if there isn't such an instruction."
  ;; TODO: so far, doesn't ignore comments
  (save-match-data
    (when (looking-at  "open[\t\n\r ]+[a-zA-Z0-9]+\\(\\.[a-zA-Z0-9]+\\)*")
      (goto-char (match-end 0)))
    (point)))

(defun fem-skip-forward-open-rename-module ()
  "Skip a 'module ... = ...' instruction.
Don't move if there isn't such an instruction."
  ;; TODO: so far, doesn't ignore comments
  (save-match-data
    (when (looking-at  "module[\t\n\r ]+[a-zA-Z0-9]+[\t\n\r ]+=[\t\n\r ]+[a-zA-Z0-9]+\\(\\.[a-zA-Z0-9]+\\)*")
      (goto-char (match-end 0)))
    (point)))

(defun fem-skip-forward-module ()
  "Skip a 'open ...' or 'module ... = ...' instruction"
  ;; TODO: so far, doesn't ignore comments
  (fem-skip-forward-open-module)
  (fem-skip-forward-open-rename-module))

(defun fem-skip-forward-square-brackets (&optional LIMIT)
  "If look at '[', go after the closing ']'.
LIMIT delimits the end of the search."
  (save-restriction
    (narrow-to-region (point) (if LIMIT LIMIT (point-max)))
    (when (looking-at-p (regexp-quote "["))
      (forward-sexp)))
  (point))

(defun fem-skip-forward-comments-pragmas-modules-spaces (&optional LIMIT)
  "Go forward until there are no comments, pragma instructions or module openings instructions.
Stop at LIMIT."
  (save-restriction
    (narrow-to-region (point) (or LIMIT (point-max)))
    (let (($continue t)
          ($p (point)))
      (while $continue
        (fem-skip-comments-and-spaces t)
        (fem-skip-forward-pragma)
        (fem-skip-forward-module)
        (when (or (= (point) $p) (= (point) (point-max)))
          (setq $continue nil))
        (setq $p (point))))))

(defun fem-region-is-comments-and-spaces (BEG END &optional NO_NEWLINE)
  "Check if a region is only made of spaces and comments.
The region is delimited by BEG and END.
NO_NEWLINE controls whether newline characters are considered spaces or not."
  (let (($p (point)) ($continue t))
    (goto-char BEG)
    (fem-skip-comments-and-spaces t END)
    (if (< (point) END) nil
      ;; If we reached the end: eventually check if there are new line characters
      (goto-char BEG)
      (if NO_NEWLINE (not (search-forward "\n" END t)) t))))

(defun fem-region-is-tuple (BEG END)
  "Return t if the text region delimited by BEG and END is a tuple.
In practice, simply check if there is a ',' inside."
  (save-excursion
    (save-restriction
      (goto-char BEG)
      (fem-search-forward-not-comment "," nil END))))

(defun fem-space-after-p (&optional POS)
  "Return t if there is a space at POS.
POS defaults to the pointer's position."
  (seq-contains fstar--spaces (char-after POS)))

(defun fem-space-before-p (&optional POS)
  "Return t if there is a space before POS.
POS defaults to the pointer's position."
  (seq-contains fstar--spaces (char-before POS)))

(defun fem-is-in-spaces-p (&optional POS)
  "Return t if there are spaces before and after POS.
POS defaults to the pointer's position."
  (and (fem-space-after-p POS) (fem-space-before-p POS)))

(defun fem-safe-backward-sexp (&optional ARG)
  "Call (backward-sexp ARG) and return nil instead of raising errors."
  (ignore-errors (backward-sexp ARG)))

(defun fem-safe-forward-sexp (&optional ARG)
  "Call (forward-sexp ARG) and return nil instead of raising errors."
  (ignore-errors (forward-sexp ARG)))

(defun fem-sexp-at-p (&optional POS ACCEPT_COMMENTS)
  "Find the sexp at POS.
POS defaults to the pointer's position.
Returns a fem-pair of positions if succeeds, nil otherwise.
If ACCEPT_COMMENTS is nil, return nil if the sexp is inside a comment."
  (save-excursion
    (let (($p0 (or POS (point))) ($not-ok nil) $beg $end)
      ;; Must not be in a comment (unless the user wants it) or surrounded by spaces
      (setq $not-ok (or (and (fem-in-general-comment-p) (not ACCEPT_COMMENTS))
                        (fem-is-in-spaces-p)))
      ;; Find the beginning and end positions
      (if $not-ok nil
        ;; End: if looking at space, this is the end position. Otherwise,
        ;; go to the end of the sexp
        (when (not (fem-space-after-p)) (fem-safe-forward-sexp))
        (setq $end (point))
        ;; Beginning: just go backward
        (fem-safe-backward-sexp)
        (setq $beg (point))
        ;; Sanity check
        (if (and (<= $beg $p0) (>= $end $p0))
            (make-fem-pair :fst $beg :snd $end)
          nil)))))

(defun fem-sexp-at-p-as-string (&optional POS)
  "Return the sexp at POS."
  (let (($r (fem-sexp-at-p)))
    (if $r (buffer-substring-no-properties (fem-pair-fst $r) (fem-pair-snd $r))
      nil)))

;; TODO: could be dramatically improved by using proper regexps
(defun fem-parse-subexpr (BEG END)
  "Parse a sub-expression of the form 'let _ = _ in', '_;' or '_'.
Parses in the region delimited by BEG and END.
Returns a fem-subexpr."
  (let ($delimiters $beg $end $is-let-in $has-semicol)
    ;; Parse: 3 cases:
    ;; - let _ = _ in
    ;; - _;
    ;; - _
    (setq $is-let-in nil $has-semicol nil)
    ;; Note that there may be a comment/spaces at the beginning and/or at the end
    ;; of the processed region, which we need to skip:
    ;; - beginning
    (goto-char BEG)
    (fem-skip-comments-and-spaces t)
    (setq $beg (point))
    ;; - end
    (goto-char END)
    (fem-skip-comments-and-spaces nil $beg)
    (setq $end (point))
    ;; We do the regexp matching in a narrowed region
    (save-restriction
      (narrow-to-region $beg $end)
      ;; Check if the narrowed region matches: 'let _ = _ in'
      (goto-char (point-min))      
      (setq $is-let-in
            ;; TODO: rewrite the regexp
            (re-search-forward "\\=let[[:ascii:][:nonascii:]]+in\\'" (point-max) t 1))
      ;; Check if the narrowed region matches: '_ ;'
      (goto-char (point-min))
      (setq $has-semicol
            ;; We could just check if the character before last is ';'
            ;; TODO: rewrite the regexp
            (re-search-forward "\\=[[:ascii:][:nonascii:]]+;\\'" (point-max) t 1))
      ;; Otherwise: it is a return value (end of function)
      ) ;; end of regexp matching
    ;; Return
    (make-fem-subexpr :beg $beg :end $end :is-let-in $is-let-in :has-semicol $has-semicol)))

(defun fem-shift-subexpr-pos (SHIFT SUBEXPR)
  "Shift by SHIFT the positions in the fem-subexpr SUBEXPR.
Return the updated fem-subexpr."
  (setf (fem-subexpr-beg SUBEXPR) (+ SHIFT (fem-subexpr-beg SUBEXPR)))
  (setf (fem-subexpr-end SUBEXPR) (+ SHIFT (fem-subexpr-end SUBEXPR)))
  SUBEXPR)

(defun fem-find-encompassing-assert-assume-p (&optional POS BEG END)
  "Find the encompassing F* assert(_norm)/assume.
Takes an optional pointer position POS and region delimiters BEG and END.
Returns a fem-pair of fem-pair of positions if found (for the assert identifier and
the content of the assert), nil otherwise."
  (save-excursion
    (save-restriction
      ;; The strategy is very simple: look for the closest previous asset/assume
      ;; which is not in a comment and such that, ignoring comments, the next
      ;; sexp contains the current point
      (let (($rbeg (if BEG BEG (point-min))) ;; region beginning
            ($rend (if END END (point-max))) ;; region end
            ($pos (if POS POS (point)))
            $abeg $aend ;; assert/assume beginning/end positions
            $pbeg $pend ;; proposition beginning/end positions
            $pred ;; predicate function (just for variable shadowing)
            )
        (narrow-to-region $rbeg $rend)
        ;; The predicate function for the search
        (defun $pred ()
          (save-match-data
            (save-excursion
              (let ($ar $str $pr)
                ;; Check that we are looking at an assert(_norm)/assume
                (setq $ar (fem-sexp-at-p))
                ;; $ar might be nil here
                (if (not $ar) nil
                  (setq $abeg (fem-pair-fst $ar) $aend (fem-pair-snd $ar))
                  (setq $str (buffer-substring-no-properties $abeg $aend))
                  (if (not (or (string-equal $str "assert")
                               (string-equal $str "assert_norm")
                               (string-equal $str "assume")))
                      ;; Not ok
                      nil
                    ;; Ok: check if the pointer is inside the argument
                    (goto-char $aend)
                    (fem-skip-comments-and-spaces t)
                    (setq $pbeg (point))
                    (fem-safe-forward-sexp)
                    (setq $pend (point))
                    (and (<= $pbeg $pos) (>= $pend $pos))))))))
        ;; Search and return
        (when (fstar--re-search-predicated-backward '$pred "assert\\|assume" $rbeg)
          ;; Return
          nil
          (make-fem-pair :fst (make-fem-pair :fst $abeg :snd $aend)
                     :snd (make-fem-pair :fst $pbeg :snd $pend))
          )))))

(defun fem-find-assert-assume-p (&optional POS BEG END)
  "Find the F* assert(_norm)/assume at the pointer position.
Takes an optional pointer position POS and region delimiters BEG and END.
Returns a fem-pair of fem-pair of positions if found (for the assert identifier and
the content of the assert), nil otherwise.
At the difference of find-encompassing-assert-assume-p, the pointer doesn't
have to be inside the assertion/assumption.  It can for instance be on an
``assert`` identifier."
  (save-excursion
    (save-restriction
      (let (($rbeg (if BEG BEG (point-min))) ;; region beginning
            ($rend (if END END (point-max))) ;; region end
            ($pos (if POS POS (point)))
            $sexp $pbeg $pend $str)
        ;; First check if we are not exactly on the identifier, otherwise
        ;; call find-encompassing-assert-assume-p
        (goto-char $pos)
        (setq $sexp (fem-sexp-at-p))
        (if $sexp
            (setq $str (buffer-substring-no-properties (fem-pair-fst $sexp)
                                                       (fem-pair-snd $sexp)))
          (setq $str ""))
        (if (or (string-equal $str "assert")
                (string-equal $str "assert_norm")
                (string-equal $str "assume"))
            ;; Ignore comments and parse the next sexp
            (progn
              (goto-char (fem-pair-snd $sexp))
              (fem-skip-comments-and-spaces t)
              (setq $pbeg (point))
              (fem-safe-forward-sexp)
              (setq $pend (point))
              (make-fem-pair :fst $sexp :snd (make-fem-pair :fst $pbeg :snd $pend)))
          ;; Otherwise, move then call find-encompassing-assert-assume-p
          (goto-char $pos)
          (fem-find-encompassing-assert-assume-p $pos $rbeg $rend))))))

(defun fem-find-encompassing-let-in (TERM_BEG TERM_END &optional BEG END)
  "Look for the 'let _ = _ in' or '_;' expression around the term.
The term is indicated by TERM_BEG and TERM_END.
Region is delimited by BEG and END.
Returns an optional fem-subexpr."
  (save-excursion
    (save-restriction
      (let (($rbeg (if BEG BEG (point-min))) ;; region beginning
            ($rend (if END END (point-max))) ;; region end
            $has-semicol
            $beg $end
            $b-beg $b-end
            $e-beg $e-end
            $bterm
            $tmp
            )
        ;; First look for a subsequent ';'
        (goto-char TERM_END)
        (fem-skip-comments-and-spaces t)
        (setq $has-semicol (looking-at-p (regexp-quote ";")))
        (if $has-semicol
            ;; If found a semicol
            (progn
              (setq $end (+ (point) 1))
              (make-fem-subexpr :beg TERM_BEG :end $end :is-let-in nil :has-semicol t))
          ;; Otherwise: look for a 'let _ = _ in' construct
          ;; First look for the '=' (note that it doesn't work with sexpressions)
          (goto-char TERM_BEG)
          (fem-skip-comments-and-spaces nil)
          (backward-char)
          ;; We should look at an '=' and not be preceded by a '=' (not sure it
          ;; is necessary to check the character before)
          (if (not (and (char-equal (char-after) ?=)
                        (not (char-equal (char-before) ?=))))
              ;; Failed
              (make-fem-subexpr :beg TERM_BEG :end TERM_END :is-let-in nil :has-semicol nil)
            ;; Save position
            (fem-skip-comments-and-spaces nil)
            (setq $b-end (point))
            ;; Look for the closest previous let which is not in a comment
            (if (not (fem-search-backward-not-comment "let"))
                ;; Failed
                (make-fem-subexpr :beg TERM_BEG :end TERM_END :is-let-in nil :has-semicol nil)
              ;; Return
              (setq $beg (point))
              (forward-sexp)
              (fem-skip-comments-and-spaces t)
              (setq $b-beg (point)) ;; $b-end set previously
              ;; Look for the 'in'
              (goto-char TERM_END)
              (fem-skip-comments-and-spaces t)
              (setq $tmp (fem-sexp-at-p))
              (if (not (string-equal "in" (fem-sexp-at-p-as-string)))
                  ;; Failed
                  (make-fem-subexpr :beg TERM_BEG :end TERM_END :is-let-in nil :has-semicol nil)
                (forward-sexp)
                (setq $end (point))                
                (make-fem-subexpr :beg $beg :end $end :is-let-in t :has-semicol nil))
                )))))))

;;; Extraction of information for the *Messages* buffer

(defun fem-search-data-delimiter (DELIMITER BACKWARD CONSUME-DELIMITER NO-ERROR
                                  &optional BEG END)
  "Search for DELIMITER in the buffer.
BEG and END are optional region delimiters.
BACKWARD gives the search direction, CONSUME-DELIMITER controls whether to leave
the pointer where it is or backtrack to put it just before (after) the DELIMITER
 (depending on the search direction).
If the DELIMITER could not be found, raises an error if NO-ERROR is nil, returns
nil otherwise."
  (let (($beg (or BEG (point-min)))
        ($end (or END (point-max)))
        $p)
    (if BACKWARD
        (setq $p (search-backward DELIMITER $beg t))
      (setq $p (search-forward DELIMITER $end t)))
    (unless (or NO-ERROR $p)
      (error (concat "Could not find the delimiter: " DELIMITER)))
    (when (and $p (not CONSUME-DELIMITER))
      (if BACKWARD (goto-char (+ $p (length DELIMITER)))
        (goto-char (- $p (length DELIMITER)))))
    (if $p (point) nil)))

(defun fem-extract-info-from-buffer (PREFIX ID &optional NO-ERROR POST-PROCESS LIMIT)
  "Extracts meta data from the current buffer and optionally post-processes it.
Returns a string (or nil if we we couldn't find the information)
Leaves the pointer at the end of the parsed data (just before the next data)."
  ;; Find where the data is
  (let* (($beg (point))
         ($end (or LIMIT (point-max)))
         ($full-id (concat PREFIX ID ":\n"))
         ($full-id-length (length $full-id))
         $p $p1 $p2 (res nil) (pp-res nil))
    ;; Find the delimiters
    (setq $p (fem-search-data-delimiter $full-id nil t NO-ERROR $beg $end))
    ;; If we found the full-id, extract the data
    (when $p
      ;; Retrieve the boundaries of the information sub-buffer
      ;; - beginning:
      (setq $p1 (point))
      ;; - end: we look for the next occurence of 'prefix' followed by a '\n'
      (backward-char 1)
      (setq $p2 (fem-search-data-delimiter (concat "\n" PREFIX ":") nil nil NO-ERROR (point) $end))
      ;; From now onwards, the pointer is at the position where it should be
      ;; left in the original buffer
      (setq $p2 (point))
      (when (< $p2 (- $p1 1)) (error "extract-info-from-messages bug")) ;; should not happen
      ;; If the data is not null, post-process it in place
      (when (>= $p2 $p1)
        ;; Start by restreigning the region
        (save-restriction
          (narrow-to-region $p1 $p2)
          ;; Post-process the data
          (when POST-PROCESS (setq pp-res (funcall POST-PROCESS)))
          ;; Save the content of the whole narrowed region
          (setq res (buffer-substring-no-properties (point-min) (point-max)))
          ;; The size of the region might have changed: go to the end, save
          ;; save the new point to p2
          (goto-char (point-max)))
        ;; Update the end of the region
        (setq $p2 (point))))
    ;; Return
    (when fem-debug
      (let ((res-str (if res (concat "[" res "]") "nil")))
        (fem-log-dbg "extract-info-from-messages:\n- prefix: %s\n- id: %s\n- res: %s "
                     PREFIX ID res-str)))
    res)) ;; end of function

(defun fem-meta-info-pp-buffer-as-seq (&optional WITH_PARENTHESES)
  "Rewrite nicely the next occurrence of h.[|buffer|].
Such terms are printed by F* as: `( .[||] ) h buffer`.
If WITH_PARENTHESES is t, look for parenthesized terms."
  (save-match-data
    (let (($re (concat "( \\.\\[||\\] )" fem-opt-spaces-re
                       "\\(" fem-identifier-regexp "\\)"  fem-spaces-re
                       "\\(" fem-identifier-regexp "\\)")))
      (when WITH_PARENTHESES (setq $re (concat "(" fem-opt-spaces-re $re fem-opt-spaces-re ")")))
      (when
          ;; Find the term
          (re-search-forward $re (point-max) t)
        ;; Find the variables and rewrite
        (let (($v1 (match-string 1)) ($v2 (match-string 3)))
          (delete-region (match-beginning 0) (match-end 0))
          (goto-char (match-beginning 0))
          (insert (concat $v1 ".[|" $v2 "|]"))
          (point))))))

(defun fem-meta-info-pp-uint_to_t (&optional WITH_PARENTHESES)
  "Rewrite nicely the next occurrence of FStar.?Int??.__uint_to_t n."
  (save-match-data
    (let (($re (concat "FStar\\." "\\([a-zA-Z0-9]+\\)"
                       "\\.__uint_to_t" fem-spaces-re "\\([0-9]+\\)")))
      (when WITH_PARENTHESES (setq $re (concat "(" fem-opt-spaces-re $re fem-opt-spaces-re ")")))
      (when
          ;; Find the term
          (re-search-forward $re (point-max) t)
        ;; Find the variables and rewrite
        (let (($uint (match-string 1)) ($n (match-string 2)))
          (delete-region (match-beginning 0) (match-end 0))
          (goto-char (match-beginning 0))
          ;; Depending on the uint type, we rewrite differently
          (cond
           ((string= "UInt32" $uint) (insert (concat $n "ul")))
           ((string= "UInt64" $uint) (insert (concat $n "UL")))
           (t (insert (concat $uint ".uint_to_t " $n))))
          (point))))))

(defun fem-meta-info-pp-remove-namespace (NAME)
  "Remove a useless namespace"
  (save-match-data
    (when (re-search-forward (concat "\\(\\=\\|[;,\\[({\t\n\t ]\\)" "\\(" NAME "\\.\\)") (point-max) t)
      (delete-region (match-beginning 2) (match-end 0))
      (point))))

(defun fem-meta-info-post-process ()
  "Post-process parsed data.
Replaces some identifiers (Prims.l_True -> True...)."
  ;; Greedy replacements
  (fem-replace-all-in "Prims.l_True" "True")
  (fem-replace-all-in "Prims.l_False" "False")
  ;; Rewrite the occurrences of h.[|buffer|]
  (goto-char (point-min)) (while (fem-meta-info-pp-buffer-as-seq t) nil)
  (goto-char (point-min)) (while (fem-meta-info-pp-buffer-as-seq nil) nil)
  ;; Rewrite the occurrences of FStar.?Int??.__uint_to_t n
  (goto-char (point-min)) (while (fem-meta-info-pp-uint_to_t t) nil)
  (goto-char (point-min)) (while (fem-meta-info-pp-uint_to_t nil) nil)
  ;; Remove the occurrences of FStar., Prims.
  (goto-char (point-min)) (while (fem-meta-info-pp-remove-namespace "FStar\\.Pervasives\\.Native") nil)
  (goto-char (point-min)) (while (fem-meta-info-pp-remove-namespace "Prims") nil)
  (goto-char (point-min)) (while (fem-meta-info-pp-remove-namespace "FStar") nil)
  nil)

(defun fem-extract-string-from-buffer (PREFIX ID &optional NO-ERROR LIMIT)
  "Extract a string for the current buffer."
  (fem-log-dbg "extract-string-from-buffer:\n- prefix: %s\n- id: %s" PREFIX ID)
  (fem-extract-info-from-buffer PREFIX ID NO-ERROR nil LIMIT))

(defun fem-extract-term-from-buffer (PREFIX ID &optional NO-ERROR LIMIT)
  "Extract a term from the current buffer.
Contrary to a string, a term is post-processed."
  (fem-log-dbg "extract-term-from-buffer:\n- prefix: %s\n- id: %s" PREFIX ID)
  (fem-extract-info-from-buffer PREFIX ID NO-ERROR
                            (apply-partially 'fem-meta-info-post-process)
                            LIMIT))

(defun fem-extract-assertion-from-buffer (PREFIX ID INDEX &optional NO-ERROR LIMIT)
  "Extract an assertion from the current buffer.
Returns a fem-meta-info structure."
  (fem-log-dbg "extract-assertion-from-buffer:\n- prefix: %s\n- id: %s\n- index: %s"
           PREFIX ID (number-to-string INDEX))
  (let* (($full-id (concat ID (number-to-string INDEX))))
    (fem-extract-term-from-buffer PREFIX $full-id NO-ERROR LIMIT)))

(defun fem-extract-assertion-list-from-buffer (PREFIX ID INDEX NUM &optional NO-ERROR LIMIT)
  "Extract a given number of assertions as a list of strings."
  (fem-log-dbg "extract-assertion-list-from-buffer:\n\
- prefix: %s\n- id: %s\n- index: %s\n- num: "
           PREFIX ID (number-to-string INDEX) (number-to-string NUM))
  (if (>= INDEX NUM) nil
    (let (($param nil) ($params nil))
      ;; Extract (forward) the assertion given by 'index'
      (setq $param
            (fem-extract-assertion-from-buffer PREFIX ID INDEX NO-ERROR LIMIT))
      ;; Recursive call
      (setq $params
            (fem-extract-assertion-list-from-buffer PREFIX ID (+ INDEX 1) NUM
                                                    NO-ERROR LIMIT))
      (cons $param $params))))

(defun fem-extract-assertion-num-and-list-from-buffer (PREFIX ID &optional NO-ERROR LIMIT)
  "Reads how many assertions to extract from the current buffer, then
extracts those assertions."
  (fem-log-dbg "extract-assertion-num-and-list-from-buffer:\n\
- prefix: %s\n- id: %s" PREFIX ID)
  ;; Extract the number of assertions
  (let (($id-num (concat ID ":num"))
        ($id-prop (concat ID ":prop"))
        $num $num-data)
    (setq $num-data (fem-extract-string-from-buffer PREFIX $id-num NO-ERROR LIMIT))
    (setq $num (string-to-number $num-data))
    (fem-log-dbg "> extracting %s terms" $num)
    ;; Extract the proper number of parameters
    (fem-extract-assertion-list-from-buffer PREFIX $id-prop 0 $num NO-ERROR
                                        LIMIT)))

(defun fem-extract-error-from-buffer (PREFIX ID &optional NO_ERROR lIMIT)
  "Extracts a (potentially nil) error from the current buffer"
  (fem-log-dbg "extract-error-from-buffer:\n\- prefix: %s\n- id: %s" PREFIX ID)
  (let (($error (fem-extract-string-from-buffer PREFIX (concat ID ":error"))))
    (if (string-equal $error "") nil $error)))
  

(defun fem-extract-result-from-buffer (PREFIX ID &optional NO_ERROR LIMIT)
  "Extracts an assertion structure from the current buffer"
  (fem-log-dbg "extract-result-from-buffer:\n\- prefix: %s\n- id: %s" PREFIX ID)
  (let ($error
        ($id-pres (concat ID ":pres"))
        ($id-posts (concat ID ":posts"))
        $pres $posts)
    (setq $error (fem-extract-error-from-buffer PREFIX ID NO_ERROR LIMIT))
    (setq $pres (fem-extract-assertion-num-and-list-from-buffer PREFIX $id-pres NO_ERROR LIMIT))
    (setq $posts (fem-extract-assertion-num-and-list-from-buffer PREFIX $id-posts NO_ERROR LIMIT))
    (make-fem-result :error $error :pres $pres :posts $posts)))

(defun fem-copy-data-from-messages-to-buffer (BEG_DELIMITER END_DELIMITER
                                              INCLUDE_DELIMITERS DEST_BUFFER
                                              &optional NO_ERROR CLEAR_DEST_BUFFER)
    "When extracting information from the *Messages* buffer, we start by locating
it by finding its begin and end delimiters, then copy it to another buffer where
we can parse/modify it. The reasons are that it is easier to modify the data in
place (while the *Messages* buffer is read-only) and that more messages
may be sent to the *Messages* buffer (by the current process or other process)
which may mess up with the data treatment and prevents us from using commands
like narrow-to-region. Moreover, it makes debugging easier. The function returns
the fem-pair of point coordinates delimiting the copied data in the destination
buffer.
include-delimiters controls whether to copy the delimiters or not"
    ;; This command MUST NOT send any message to the *Messages* buffer
    (let (($prev-buffer (current-buffer))
          ($backward t)
          ($beg nil) ($end nil)
          ($data nil)
          ($p1 nil) ($p2 nil))
      ;; Switch to the *Messages* buffer
      (switch-to-buffer fem-messages-buffer)
      ;; Find the delimiters
      (goto-char (point-max))
      (setq $beg (fem-search-data-delimiter BEG_DELIMITER t INCLUDE_DELIMITERS NO_ERROR))
      (setq $end (fem-search-data-delimiter END_DELIMITER nil INCLUDE_DELIMITERS NO_ERROR))
      ;; Check if successful
      (if (or (not $beg) (not $end))
          ;; Failure
          (progn
            (when (not NO_ERROR)
              (error (concat "copy-data-from-messages-to-buffer: "
                             "could not find the delimiters: "
                             BEG_DELIMITER ", " END_DELIMITER)))
            nil)
        ;; Success
        ;; Copy in the dest-buffer
        (setq $data (buffer-substring-no-properties $beg $end))
        (switch-to-buffer DEST_BUFFER)
        (goto-char (point-max))
        (when CLEAR_DEST_BUFFER (erase-buffer))
        (setq $p1 (point))
        (insert $data)
        (setq $p2 (point))
        ;; Switch back to the original buffer
        (switch-to-buffer $prev-buffer)
        ;; Return
        (make-fem-pair :fst $p1 :snd $p2))))

(defun fem-clean-data-from-messages (&optional BEG END)
    "Once data has been copied from the messages buffer, it needs some processing
to be cleaned before being parsed: this function cleans the data in the current
buffer."
    (let ($new-end)
      (setq-default BEG (point))
      (setq-default END (point-max))
      (save-restriction
        (narrow-to-region BEG END)
        ;; Start by removing all the occurrences of '[F*] TAC' and '[F*]':
        ;; - make sure they all start by '\n'
        (goto-char (point-min))
        (insert "\n")
        ;; - replace (the order is important)
        (fem-replace-all-in (concat "\n" fem-tactic-message-prefix) "\n")
        (fem-replace-all-in (concat "\n" fem-message-prefix) "\n")
        ;; - remove the introduced '\n' (note that the pointer will be left at
        ;; its original position
        (goto-char (point-min))
        (delete-char 1)
        ;; Save the new region end
        (goto-char (point-max))
        (setq $new-end (point))
        (goto-char (point-min)))
      ;; Return the new end of the region
      (+ (point) $new-end)))

(defun fem-extract-result-from-messages (PREFIX ID &optional PROCESS-BUFFER NO-ERROR
                                         CLEAR-PROCESS-BUFFER)
  "Extracts a meta analysis result from the *Messages* buffer. Returns an fem-result structure.
process-buffer is the buffer to use to copy and process the raw data
(*fstar-data1* by default)."
  (setq-default PROCESS-BUFFER fem-process-buffer2)
  (fem-log-dbg "extract-result-from-messages:\n\
- prefix: %s\n- id: %s\n- process buffer: '%s'\n" PREFIX ID PROCESS-BUFFER)
  (let (($prev-buffer (current-buffer))
        ($region nil)
        ($result nil)
        ($beg-delimiter (concat "[F*] TAC>> " PREFIX ID ":BEGIN"))
        ($end-delimiter (concat "[F*] " PREFIX ID ":END")))
    ;; Copy the data
    (setq $region (fem-copy-data-from-messages-to-buffer $beg-delimiter $end-delimiter
                                                    t PROCESS-BUFFER NO-ERROR
                                                    CLEAR-PROCESS-BUFFER))
    (if (not $region)
        (progn
          (when (not NO-ERROR)
            (error (concat "extract-eterm-info-from-messages (prefix: "
                           PREFIX ") (id: " ID "): "
                           "could not find the region to copy from *Messages*")))
          nil)
      ;; Switch to the process buffer
      (switch-to-buffer PROCESS-BUFFER)
      (goto-char (fem-pair-fst $region))
      ;; Restrain the region and process it
      (save-restriction
        (narrow-to-region (fem-pair-fst $region) (fem-pair-snd $region))
        ;; Clean
        (fem-clean-data-from-messages)
        ;; Extract the eterm-info
        (setq $result (fem-extract-result-from-buffer PREFIX ID NO-ERROR)))
      ;; Switch back to the original buffer
      (switch-to-buffer $prev-buffer)
      ;; Return
      $result)))

;;; Commands to generate F* code

(defun fem-insert-with-indent (INDENT-STR txt &optional INDENT-FIRST-LINE)
  (when INDENT-FIRST-LINE (insert INDENT-STR))
  (insert (fem-replace-in-string "\n" (concat "\n" INDENT-STR) txt)))

(defun fem-generate-assert-from-term (INDENT-STR AFTER-TERM DATA &optional COMMENT)
  "Inserts an assertion in the code. after-term must be t if the assert is
after the focused term, nil otherwise. comment is an optional comment"
  (when DATA
    ;; If we are after the studied term: insert a newline
    (when AFTER-TERM (insert "\n"))
    (when COMMENT
      (insert INDENT-STR)
      (insert COMMENT)
      (insert "\n"))
    (when AFTER-TERM (insert INDENT-STR))
    (insert "assert(")
    (when (> (fem-count-lines-in-string DATA) 1)
      (insert "\n")
      (insert INDENT-STR)
      (insert "  "))
    (fem-insert-with-indent (concat INDENT-STR "  ") DATA)
    (insert ");")
    ;; If we are before the studied term: insert a newline
    (when (not AFTER-TERM) (insert "\n") (insert INDENT-STR))))

(defun fem-insert-assert-pre-post--continuation (INDENT_STR TERM_BEG TERM_END
                                                 OVERLAY STATUS RESPONSE)
  "Process the information output by F* to add it to the user code.
If F* succeeded, extract the information and add it to the proof."
  (unless (eq STATUS 'interrupted)
    ;; Delete the overlay
    (delete-overlay OVERLAY)
    ;; The F* query may have failed on purpose, because we may have aborted
    ;; the proof during post-processing to save time (previously, we ended
    ;; the post-processing with a call to `trefl()`, but this is actually
    ;; super expensive).
    ;; If F* succeeded, we need to pop the state. If it failed, we need to
    ;; check for information in the output indicating an unexpected failure
    ;; or a failure on purpose.
    (if (eq STATUS 'success)
        (progn
          (fem-log-dbg "F* succeeded")
          ;; The sent query "counts for nothing" so we need to pop it to reset
          ;; F* to its previous state
          (fstar-subp--pop))
      (progn
        ;; Check if it was on purpose
        (let (($prev-buffer (current-buffer)) ($NO-ERROR nil))
          (switch-to-buffer fem-messages-buffer)
          (goto-char (point-max))
          (when (search-backward fem-start-fstar-msg (point-min) t)
            (setq $NO-ERROR (search-forward fem-end-fstar-msg (point-max) t)))
          (switch-to-buffer $prev-buffer)
          (when (not $NO-ERROR)
            (when (y-or-n-p "F* failed: do you want to see the F* query?")
              (switch-to-buffer fem-process-buffer1))
            (error "F* failed"))
          (fem-log-dbg "F* succeeded (the post-processing aborted on purpose))"))))
    ;; If we reach this point it means there was no error: we can extract
    ;; the generated information and add it to the code
    ;;
    ;; Extract the data. Note that we add two spaces to the indentation, because
    ;; if we need to indent the data, it is because it will be in an assertion.
    (let (($result (fem-extract-result-from-messages "ainfo" ""
                                                     fem-process-buffer2 t t)))
      ;; Check if error
      (when (fem-result-error $result)
        (when (y-or-n-p (concat "F* failed: " (fem-result-error $result)
                                "\n-> Do you want to see the F* query?"))
          (switch-to-buffer fem-process-buffer1))
        (error "F* failed"))
      ;; Print the information
      ;; - before the focused term
      (goto-char TERM_BEG)
      (dolist (a (fem-result-pres $result))
        (fem-generate-assert-from-term INDENT_STR nil a))
      ;; - after the focused term
      (forward-char (- TERM_END TERM_BEG))
      (dolist (a (fem-result-posts $result))
        (fem-generate-assert-from-term INDENT_STR t a))
      )))

(defun fem-same-opt-num (P1 P2)
  "Return t if P1 and P2 are the same numbers or are both nil."
  (if (and P1 P2)
      (= P1 P2)
    (and (not P1) (not P2))))

(defun fem-get-pos-markers (&optional END)
  "Return the saved pos markers above the pointer and remove them from the code.
Returns a (potentially nil) fem-pair.
END is the limit of the region to check"
  (save-match-data
    (let ($original-pos $p0 $p1 $mp0 $mp1)
      (setq $original-pos (point))
      (setq $p0 (fstar-subp--untracked-beginning-position))
      (setq $p1 (or END (point)))
      ;; First marker
      (goto-char $p0)
      (if (not (search-forward (format fem-pos-marker 0) $p1 t))
          ;; No marker
          (progn (goto-char $original-pos) nil)
        ;; There is a marker: save the position and remove it
        (when (>= $original-pos (match-end 0))
          (setq $original-pos (- $original-pos (- (match-end 0) (match-beginning 0)))))
        (setq $mp0 (match-beginning 0))
        (replace-match "")
        ;; Look for the second one
        (goto-char $p0)
        (if (not (search-forward (format fem-pos-marker 1) $p1 t))
            (setq $mp1 nil)
          (setq $mp1 (match-beginning 0))
          (when (>= $original-pos (match-end 0))
            (setq $original-pos (- $original-pos (- (match-end 0) (match-beginning 0)))))
          (replace-match ""))
        ;;Return
        (goto-char $original-pos)
        (make-fem-pair :fst $mp0 :snd $mp1)))))

(defun fem-insert-pos-markers ()
  "Insert a marker in the code to indicate the pointer position.
This is a way of saving the pointer's position for a later function call,
while indicating where this position is to the user.
TODO: use overlays."
  (interactive)
  (let ($p $p1 $p2 $op1 $op2 $overlay)
    (setq $p (point))
    ;; Retract so that the current point is not in a processed region
    (fstar-subp-retract-until $p)
    ;; Check if there are other markers. If there are, remove them.
    ;; Otherwise, insert new ones.
    (if (fem-get-pos-markers)
        nil
      ;; Save the active region (if there is) or the pointer position
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        ;; Pointer position: move the pointer if we are above a term
        (when (not (or (fem-space-before-p) (fem-space-after-p)))
          (fem-safe-backward-sexp))
        (setq $p1 (point) $p2 nil))
      ;; Insert the markers (starting with the second not to have to handle shifts)
      (when $p2 (goto-char $p2) (insert (format fem-pos-marker 1)))
      (goto-char $p1)
      (insert (format fem-pos-marker 0)))))

(defun fem-find-region-delimiters-or-markers ()
  "Find the region to process."
  (save-excursion
    (let ($p0 $markers $p1 $p2 $p $allow-selection $delimiters)
      ;; Check for saved markers
      (setq $markers (fem-get-pos-markers))
      (setq $p0 (point)) ;; save the original position
      ;; If we found two markers: they delimit the region
      ;; If we found one: use it as the current pointer position
      (if (and $markers
               (fem-pair-fst $markers)
               (fem-pair-snd $markers))
          (setq $p1 (fem-pair-fst $markers) $p2 (fem-pair-snd $markers))
        (setq $p (if $markers (fem-pair-fst $markers) (point)))
        (setq $allow-selection (not $markers))
        (setq $delimiters (fem-find-region-delimiters $allow-selection t nil nil $p)))
      (goto-char $p0)
      $delimiters)))

;;
;; General control flow parsing (CFP)
;;
;; Parse an F* expression up to some point in order to retrieve the list
;; of the parent expressions, in terms of control flow.
;; We call it "control-flow" parsing intead of simply "parsing" because it
;; is a simplified version of parsing: whe only want to understand the
;; control-flow, so that we know if the pointer is
;; inside the branch of a match, a let expression, a 'begin ... end', etc.

(defconst fem-spaces-re (concat "[" fstar--spaces "]+")) ;; [\t\n\r ]+
(defconst fem-opt-spaces-re (concat "[" fstar--spaces "]*")) ;; [\t\n\r ]*

(defun fem-parse-re-p (REGEXP)
  "Parse a regexp and return a pair of position if succeeds, nil otherwise."
  (save-match-data
    (if (looking-at REGEXP)
        (make-fem-pair :fst (match-beginning 0) :snd (match-end 0))
      nil)))  

(defun fem-parse-special-delimiter-p ()
  "Match a special delimiter, which can't be used in combination with other symbols."
  (fem-parse-re-p "[;,]"))

;; TODO: we might need to put "'"
(defun fem-parse-special-symbols-p ()
  "Try to parse special symbols and return a pair of positions if successful."
  (fem-parse-re-p "[\\^*\+-/=\|=\$:><!&%]+"))

(defconst fem-identifier-regexp
  "[`]*[#]?[']?[_[:alpha:]][[:alpha:]0-9_']*[\\?]?\\(\\.[[:alpha:]0-9_']*[\\?]?\\)*[`]*")

(defconst fem-identifier-charset
  "`#[:alpha:]0-9_'\\?\\.")

(defun fem-parse-identifier-p ()
  "Parse an identifier and return a pair of positions if successful.
Note that we consider valids incomplete identifiers of the form 'Foo.' (which
might be used to use a specific namespace in a parenthesized expression).
Also, we check back ticks in a very lax manner."
  (fem-parse-re-p fem-identifier-regexp))

(defun fem-parse-number-p ()
  "Parse a number, in a very broad sense (might be hexadecimal, etc.)."
  (fem-parse-re-p "[0-9][a-zA-Z0-9]*"))

(defun fem-looking-at-open-bracket-p ()
  "Return a bracket type if looking at an open bracket (in a wide sense: '(', '[' or '{')."
  (cond
   ((looking-at "#(") 'open-bracket) ;; TODO: use a different symbol name
   ((looking-at "(") 'open-bracket)
   ((looking-at "{") 'open-curly)
   ((looking-at "\\[") 'open-square)
   (t nil)))

(defun fem-parse-parenthesized-exp-p ()
  "Parse a balanced parenthesized expression."
  (save-excursion
    (save-match-data
      (ignore-errors ;; the call to forward-sexp may fail
        (if (looking-at "#(") (forward-char))
        (if (fem-looking-at-open-bracket-p)
            (let (($p0 (point)))
              (forward-sexp) ;; This may fail
              (make-fem-pair :fst $p0 :snd (point)))
          nil)))))

(defun fem-looking-at-string-p ()
  "Return t if looking at the beginning of a string."
  (looking-at "\""))

(defun fem-parse-string-p ()
  "Parse a string."
  (save-excursion
    (save-match-data
      (ignore-errors ;; the call to forward-sexp may fail
        (if (fem-looking-at-string-p)
            (let (($p0 (point)))
              (forward-sexp) ;; This may fail
              (make-fem-pair :fst $p0 :snd (point)))
          nil)))))

;; TODO: use more
(defun fem-substring-from-pos-pair (POS_PAIR)
  "Return the substring delimited by the field of the fem-pair POS_PAIR"
  (buffer-substring-no-properties (fem-pair-fst POS_PAIR)
                                  (fem-pair-snd POS_PAIR)))  

;; Control-flow parsing: token
(cl-defstruct fem-cfp-tk
  symbol ;; the parsed symbol
  pos ;; fem-pair indicating the beginning and the end positions
  index ;; unique index identifying this symbol - mostly used for debugging
  parent ;; a parent related symbol (for 'if then else' expressions, we link 'then' and 'else' to the 'if')
  )

;; Control-flow parsing: state
(cl-defstruct fem-cfp-state
  pos ;; the pointer position
  stack ;; the current stack of tokens
  counter ;; stack tokens counter - used to give a unique identifier to each token
  all-tokens ;; all the tokens encountered so far (contrary to stack, we don't pop items)
  )

(defun fem-create-cfp-state (&optional POS)
  "Create and initialize a fem-cfp-state.
POS is the optional position to use as the starting point."
  (make-fem-cfp-state :pos (or POS (point)) :stack nil :counter 0))

;; Utility functions to manipulate a CFP state
(defun fem-cfp-state-top-is (STATE SYMBOL)
  "Check if the first token on the STATE stack is equal to SYMBOL.
Return nil if the stack is empty."
  (fem-log-dbg "[> fem-cfp-state-top-is (%s)" SYMBOL)
  (let (($stack (fem-cfp-state-stack STATE)))
    (if $stack
        (progn
          (fem-log-dbg "%s" (fem-cfp-tk-symbol (car $stack)))
          (string= SYMBOL (fem-cfp-tk-symbol (car $stack))))
      nil)))

(defun fem-cfp-state-pop (STATE &optional DEBUG_INSERT)
  "Pop the first token of the STATE stack and return it.
If DEBUG_INSERT is t, insert a comment in the code for debugging."
  (let* (($stack (fem-cfp-state-stack STATE))
         ($p (car $stack))
         ($symbol (fem-cfp-tk-symbol $p))
         ($index (fem-cfp-tk-index $p))
         ($msg (format " (* pop: %s {#%s} *) " $symbol $index)))
    (fem-log-dbg "[> fem-cfp-state-pop")
    (setq $stack (cdr $stack))
    (setf (fem-cfp-state-stack STATE) $stack)
    ;; For debugging
    (fem-log-dbg $msg)
    (when DEBUG_INSERT (insert $msg))
    $p))

(defun fem-cfp-state-push (STATE SYMBOL POS_PAIR &optional PARENT DEBUG_INSERT)
  "Push SYMBOL at position POS_PAIR to the STATE stack.
PARENT is another symbol related to the current one (for example, if
we push an 'else, we keep track of the 'if starting the 'if then else'
construct.
If DEBUG_INSERT is t, insert a comment in the code for debugging."
  (let* (($counter (fem-cfp-state-counter STATE))
         ($stack (fem-cfp-state-stack STATE))
         ($all-tokens (fem-cfp-state-all-tokens STATE))
         ($tk (make-fem-cfp-tk :symbol SYMBOL :pos POS_PAIR
                               :index $counter :parent PARENT))
         $msg)
    ;; If the token has a parent: print it in the message number
    (if (not PARENT)
        (setq $msg (format " (* push: %s {#%s} *) " SYMBOL $counter))
      (setq $msg (format " [parent: %s {#%s}] "
                         (fem-cfp-tk-symbol PARENT)
                         (fem-cfp-tk-index PARENT)))
      (setq $msg (format " (* push: %s {#%s} %s *) " SYMBOL $counter $msg)))
    ;; Debugging
    (fem-log-dbg $msg)
    (when DEBUG_INSERT (insert $msg))
    ;; Update the stacks
    (setq $stack (cons $tk $stack))
    (setq $all-tokens (cons $tk $all-tokens))
    (setq $counter (+ $counter 1))
    (setf (fem-cfp-state-stack STATE) $stack)
    (setf (fem-cfp-state-all-tokens STATE) $all-tokens)
    (setf (fem-cfp-state-counter STATE) $counter)))

(defun fem-cfp-state-pop-until-pred (STATE PRED INCLUSIVE &optional DEBUG_INSERT)
  "Pop all the STATE stack tokens up to the first occurrence satisfying PRED.
If INCLUSIVE is t, also pop the token satisfying PRED."
  (let (($continue t)
        ($ret nil))
    (fem-log-dbg "[> fem-cfp-state-pop-until-pred")
    (while $continue
      (if (or (not (fem-cfp-state-stack STATE))
              (funcall PRED (car (fem-cfp-state-stack STATE))))
          ;; Last pop
          (progn
            (setq $continue nil)
            (when (fem-cfp-state-stack STATE)
              (if INCLUSIVE
                  (setq $ret (fem-cfp-state-pop STATE DEBUG_INSERT))
                (setq $ret (car (fem-cfp-state-stack STATE))))))
        (fem-cfp-state-pop STATE DEBUG_INSERT)))
    $ret))

(defun fem-cfp-state-pop-until (STATE SYMBOL &optional DEBUG_INSERT)
  "Pop all the STATE stack tokens up to (and including) the first occurrence of SYMBOL."
  (fem-cfp-state-pop-until-pred STATE
                                (lambda (TK) (string= (fem-cfp-tk-symbol TK) SYMBOL))
                                t DEBUG_INSERT))

(defun fem-cfp-state-find-first-pred (STATE PRED)
  "Find the first token in the STATE stack satisfying PRED."
  (let (($stack (fem-cfp-state-stack STATE))
        ($continue t)
        ($ret nil))
    (fem-log-dbg "[> fem-cfp-state-find-first-pred")
    (while (and $stack $continue)
      (if (funcall PRED (car $stack))
          (progn
            (setq $continue nil)
            (setq $ret (car $stack)))
        (setq $stack (cdr $stack))))
    $ret))

(defun fem-cfp-state-find-prev-decl-end-token (STATE)
  "Find the token delimiting the end of the previous declaration"
  ;; If there is previous declaration, it must be ended by a 'semicolon or a 'in.
  ;; Otherwise, look for: 'equal1, 'begin, 'open-bracket, 'open-curly, 'open-square,
  ;; 'if, 'then, 'else, 'arrow
  (let (($pred (lambda (TK)
                 (let (($symbol (fem-cfp-tk-symbol TK)))
                   (or (string= $symbol 'in)
                       (string= $symbol 'semicol)
                       (string= $symbol 'equal1)
                       (string= $symbol 'begin)
                       (string= $symbol 'if)
                       (string= $symbol 'then)
                       (string= $symbol 'else)
                       (string= $symbol 'arrow)
                       (string= $symbol 'open-bracket)
                       (string= $symbol 'open-curly)
                       (string= $symbol 'open-square))))))
  (fem-cfp-state-find-first-pred STATE $pred)))

(defun fem-cfp-state-top (STATE)
  "Return the last token parsed by STATE."
  (fem-log-dbg "[> fem-cfp-state-top")
  (let (($stack (fem-cfp-state-all-tokens STATE)))
    (if $stack (car $stack) nil)))

(defun fem-cfp-state-move-p (STATE &optional POS)
  "Move the pos field in fem-cfp-state STATE to POS or (point)."
  (setf (fem-cfp-state-pos STATE) (or POS (point))))

(defun fem-cfp-parse-update-identifier (STATE &optional DEBUG_INSERT)
  "Parse one identifier and update STATE.
If DEBUG_INSERT is t insert comments in the code for debugging.
In case of success, return the symbol which was found, or 'unknown-identifier.
Otherwise, return nil."
  (let (($tk (fem-parse-identifier-p))
        $tk-str
        fem-cfp-handle-ident)
    (if (not $tk) nil
      ;; Read the identifier
      (setq $tk-str (fem-substring-from-pos-pair $tk))
      (fem-log-dbg "[> parsed an identifier: \"%s\"" $tk-str)
      ;; Move forward - note that as we might insert debugging information in
      ;; code, we update the state position later
      (goto-char (fem-pair-snd $tk))
      ;; Small helper
      (defun fem-cfp-handle-ident (NAME IDENT POP_UNTIL PUSH &optional PARENT_OF_PARENT)
        "Handle one case.
NAME: identifier name.
IDENT: identifier
POP_UNTIL: symbol to which to pop the stack, or nil. Will be used as parent if we push.
PUSH: t if push the current symbol.
PARENT_OF_PARENT: if t, together with POP_UNTIL and PUSH, use the parent of the last popped
identifier as parent when pushing.
If $tk-str is not equal to NAME, return nil. Otherwise return IDENT"
        (if (not (string-equal NAME $tk-str)) nil
          ;; Equal to NAME
          (let (($parent nil))
            ;; Pop until
            (when POP_UNTIL
              (setq $parent (fem-cfp-state-pop-until STATE POP_UNTIL DEBUG_INSERT))
              (when (and $parent PARENT_OF_PARENT)
                (setq $parent (fem-cfp-tk-parent $parent))))
            ;; Push
            (when PUSH
              (fem-cfp-state-push STATE IDENT $tk $parent DEBUG_INSERT))
            (when DEBUG_INSERT (insert " (* id *) "))
            ;; Update the state position
            (fem-cfp-state-move-p STATE)
            ;; Return the identifier
            IDENT)))
      ;; Switch on the identifier - we use the fact that or returns the value
      ;; returned by the first function which doesn't return nil
      (or
       ;; BEGIN
       (fem-cfp-handle-ident "begin" 'begin nil t)
       ;; END: this delimiter might end several expressions at once
       (fem-cfp-handle-ident "end" 'end 'begin t)
       ;; MATCH
       (fem-cfp-handle-ident "match" 'match nil t)
       ;; WITH
       (fem-cfp-handle-ident "with" 'with 'match t)
       ;; IF
       (fem-cfp-handle-ident "if" 'if nil t)
       ;; THEN: might end several expressions at once
       (fem-cfp-handle-ident "then" 'then 'if t)
       ;; ELSE: might end several expressions at once
       ;; Also note that we want to use the 'if as parent, not the 'else
       (fem-cfp-handle-ident "else" 'else 'then t)
       ;; LET
       (fem-cfp-handle-ident "let" 'let nil t)
       ;; IN: might end several expressions at once
       (fem-cfp-handle-ident "in" 'in 'let t)
       ;; Otherwise: just update the state position
       (progn
         (fem-cfp-state-push STATE 'ident $tk nil DEBUG_INSERT)
         (when DEBUG_INSERT (insert " (* id *) "))
         (fem-cfp-state-move-p STATE)
         'unknown-identifier)))))

(defun fem-cfp-parse-one-token (STATE &optional END DEBUG_INSERT)
  "Parse one control-flow token and udpate STATE.
Return nil if an error occurred.
Return the parsed symbol otherwise, or'eob if we reached the end of the parsing region.
END delimits the end of the parsing refion.
If DEBUG_INSERT is t, insert comments in the code for debugging."
  (fem-log-dbg "[> fem-cfp-parse-one-token")
  (let (($beg (fem-cfp-state-pos STATE))
        ($end (or END (point-max)))
        $tk $tk-str $ident $bracket-type $parent)
    (goto-char $beg)
    (save-restriction
      (narrow-to-region $beg $end)
      ;; Ignore comments and spaces
      (fem-skip-comments-and-spaces t)
      ;; Below: we use the fact that setq returns the last value it assigned
      ;; to switch between cases (all the functions we call return nil if
      ;; they failed)
      (cond

       ;; Check if we reached the end: if so, skip the remaining instructions
       ((= (point) $end)
        (setf (fem-cfp-state-pos STATE) (point))
        'eob)

       ;; SPECIAL DELIMITERS (which can't be composed): ';', ','
       ((setq $tk (fem-parse-special-delimiter-p))
        ;; Read
        (setq $tk-str (fem-substring-from-pos-pair $tk))
        (fem-log-dbg "[> parsed special delimiters: %s" $tk-str)
        ;; Move forward - as we might insert comments in the code, we update the
        ;; state position later
        (goto-char (fem-pair-snd $tk))
        ;; Case disjunction
        (cond
         ;; CASE1: If it is a ';':
         ;; Check if we are in the branch of an 'if ... then ... else ...
         ((string= $tk-str ";")
          (fem-log-dbg "[> special delimiter is a ';'")
          ;; If inside a 'then': it should have only one branch (of type unit): pop it
          ;; ;; If inside an 'else": this is the end of this branch (pop it)
          (if (or (fem-cfp-state-top-is STATE 'then)
                  (fem-cfp-state-top-is STATE 'else))
              (setq $parent (fem-cfp-state-pop STATE DEBUG_INSERT))
            (setq $parent (fem-cfp-state-find-prev-decl-end-token STATE)))
          ;; Push the semicol because it delimits a (sugarized) let binding
          (fem-cfp-state-push STATE 'semicol $tk $parent DEBUG_INSERT))
         ;; OTHERWISE: push a 'special-delimiters
         (t (fem-cfp-state-push STATE 'special-delimiters $tk nil DEBUG_INSERT)))
        ;; Update the state position and return
        (when DEBUG_INSERT (insert " (* spec. delim. *)"))
        (fem-cfp-state-move-p STATE)
        'special-delimiters)

       ;; SPECIAL SYMBOLS (which can be composed): '=', '|', '+', etc.
       ((setq $tk (fem-parse-special-symbols-p))
        (fem-log-dbg "[> parsed special symbols: %s" (fem-substring-from-pos-pair $tk))
        (goto-char (fem-pair-snd $tk))
        (setq $tk-str (fem-substring-from-pos-pair $tk))
        ;; Disjunction on the symbol
        (cond
         ;; If "=" push 'equal1
         ((string= $tk-str "=")
          (fem-cfp-state-push STATE 'equal1 $tk nil DEBUG_INSERT))
         ;; If "->": push 'arrow
         ((string= $tk-str "->")
          (fem-cfp-state-push STATE 'arrow $tk nil DEBUG_INSERT))
         ;; Otherwise push 'special-symbols
         (t
          (fem-cfp-state-push STATE 'special-symbols $tk nil DEBUG_INSERT)))
        (when DEBUG_INSERT (insert " (* spec. symb. *) "))
        (fem-cfp-state-move-p STATE)
        'special-symbols)

       ;; NUMBERS
       ((setq $tk (fem-parse-number-p))
        ;; Just move forward
        (fem-log-dbg "[> parsed a number: %s" (fem-substring-from-pos-pair $tk))
        (goto-char (fem-pair-snd $tk))
        (fem-cfp-state-push STATE 'number $tk nil DEBUG_INSERT)
        (when DEBUG_INSERT (insert " (* num *) "))
        (fem-cfp-state-move-p STATE)
        'number)

       ;; PARENTHESES
       ((setq $bracket-type (fem-looking-at-open-bracket-p))
        ;; Try to parse a well-balanced group
        (setq $tk (fem-parse-parenthesized-exp-p))
        (if $tk
            ;; If success: move forward
            (progn
              (fem-log-dbg "[> parsed a well-balanced parenthesized expression")
              (goto-char (fem-pair-snd $tk))
              (when DEBUG_INSERT (insert " (* (...) *) "))
              (fem-cfp-state-push STATE 'balanced-brackets $tk nil DEBUG_INSERT)
              (fem-cfp-state-move-p STATE)
              'parentheses)
          ;; Otherwise: update the stack to dive in
          (fem-log-dbg "[> parsed an unbalanced open parenthesis: diving in")
          (forward-char)
          (fem-cfp-state-push STATE $bracket-type
                              (make-fem-pair :fst (point) :snd (+ (point) 1))
                              nil DEBUG_INSERT)
          (when DEBUG_INSERT (insert " (* '(' *) "))
          (fem-cfp-state-move-p STATE)
          'open-bracket))       

       ;; STRINGS
       ((setq $tk (fem-parse-string-p))
        ;; Just move forward
        (fem-log-dbg "[> parsed a string: %s" (fem-substring-from-pos-pair $tk))
        (goto-char (fem-pair-snd $tk))
        (fem-cfp-state-push STATE 'string $tk nil DEBUG_INSERT)
        (when DEBUG_INSERT (insert " (* str *) "))
        (fem-cfp-state-move-p STATE)
        'number)

       ;; IDENTIFIERS: 'if', 'then', 'else', 'begin', 'match', etc.
       ((setq $ident (fem-cfp-parse-update-identifier STATE DEBUG_INSERT))
        $ident)

       ;; Last case: parsing error
       (t
        (fem-log-dbg "[> parsing error:\n%s"
                     (buffer-substring-no-properties (point) (point-max)))
        nil)) ;; end of cond
      )))

(defun fem-cfp-parse-tokens (END &optional STATE DEBUG_INSERT)
  "Parse the region by using STATE as the current parsing state.
Stop at END.
If DEBUG_INSERT is t, insert debugging information in the code.
If STATE is nil, initialize a parsing STATE from the current pointer position."
  (fem-log-dbg "[> fem-cfp-parse-tokens")
  (let* (($state (or STATE (fem-create-cfp-state)))
         ($beg (fem-cfp-state-pos $state))
         ($continue t) ;; loop condition
         )
    (goto-char $beg)
    (save-restriction
      (narrow-to-region $beg END)
      (while $continue
        ;; Check if we reached the end of the region
        ;; Note that as we might insert comments in the code we can't use END
        (if (= (point) (point-max))
            (setq $continue nil)
          ;; Parse the next token
          (when (not (fem-cfp-parse-one-token STATE (point-max) DEBUG_INSERT))
            (fem-log-dbg "[> fem-cfp-parse-tokens: parsing errors")
            (error "fem-cfp-parse-tokens: parsing errors")))))))

(defun fem-parse-tokens-debug ()
  "Parse the unprocessed part of the buffer up to the current position.
Insert parsing information in the code at the same time.
This function is mainly used for debugging the parser."
  (interactive)
  (fem-cfp-parse-tokens (point)
                        (fem-create-cfp-state
                         (fstar-subp--untracked-beginning-position))
                        t))

(defun fem-cfp-state-filter-stack (STACK)
  "Filter a token STACK to remove the first 'let, which is  top-level, and the
tokens which will be ignored for control-flow.
Return a pair (filtered stack, filtered the top-level let)."
  (let ($rev-stack $new-stack $filtered-let $tk $symbol)
    ;; The function was initially written in a recursive style, however the
    ;; stack can be quite big, which lead to failures, forcing us to increase
    ;; the elisp recursion depth. As doing this seems to be generally a bad
    ;; idea, we rewrote the function to use a loop.
    ;; First revert the stack
    (setq $rev-stack (reverse STACK))
    ;; Filter the stack and reconstruct it (in the proper order) while doing so
    (while $rev-stack
      (setq $tk (car $rev-stack) $rev-stack (cdr $rev-stack))
      (setq $symbol (fem-cfp-tk-symbol $tk))
      ;; Case disjunction on the current symbol
      (cond
       ;; If it is a let, check if it is the first one (the top-level let): in this
       ;; case, filter it. Otherwise, keep it
       ((string= $symbol 'let)
        (if $filtered-let
            ;; Stack
            (setq $new-stack (cons $tk $new-stack))
          ;; Filter
          (setq $filtered-let t)))
       ;; If 'if, 'then, 'begin, 'match or open bracket: keep the token
       ((or (string= $symbol 'if)
            (string= $symbol 'then)
            (string= $symbol 'begin)
            (string= $symbol 'match)
            (string= $symbol 'with)
            (string= $symbol 'open-bracket)
            (string= $symbol 'open-curly)
            (string= $symbol 'open-square))
        (setq $new-stack (cons $tk $new-stack)))
       ;; Otherwise:filter
       (t nil)))
    ;; Return
    $new-stack))

(defun fem-cfp-state-filter-state-stack (STACK)
  "Filter the STATE stack to remove the first 'let, which is  top-level, and the
tokens which will be ignored for control-flow"
  (fem-cfp-state-filter-stack (fem-cfp-state-stack STACK)))

(defun fem-copy-def-for-meta-process (BEG END INSERT_ADMITS SUBEXPR PARSE_STATE
                                      DEST_BUFFER PP_INSTR)
  "Copy code for meta-processing and update the parsed result positions.
Leaves the pointer at the end of the DEST_BUFFER where the code has been copied.
PP_INSTR is the post-processing instruction to insert for F*."
  (let ($p0 $str1 $str2 $str3 $shift $new-length $original-length $focus-shift
             $prev-buffer $stack $symbol $res)
    (goto-char BEG)
    ;; - copy to the destination buffer. We do the parsing to remove the current
    ;;   attributes inside the F* buffer, which is why we copy the content
    ;;   in several steps. TODO: I don't manage to confifure the parsing for the
    ;;   destination buffer correctly.
    (fem-skip-forward-comments-pragmas-modules-spaces)
    (setq $str1 (buffer-substring BEG (point)))
    (fem-skip-forward-square-brackets) ;; (optionally) go over the attribute
    (setq $p0 (point))
    (fem-skip-forward-comments-pragmas-modules-spaces)
    (setq $str2 (buffer-substring $p0 (point)))
    (setq $str3 (buffer-substring (point) END))
    (setq $prev-buffer (current-buffer))
    (switch-to-buffer DEST_BUFFER)
    (erase-buffer)
    (insert $str1)
    (insert $str2)
    ;; Insert an option to deactivate the proof obligations
    (insert "#push-options \"--admit_smt_queries true\"\n")
    ;; Insert the post-processing instruction
    (insert "[@(FStar.Tactics.postprocess_with (")
    (insert PP_INSTR)
    (insert "))]\n")
    ;; Insert the function code
    (insert $str3)
    ;; Compute the current shift: the shift is just the difference of length between the
    ;; content in the destination buffer and the original content, because all
    ;; the deletion/insertion so far should have happened before the points of interest
    (setq $original-length (- END BEG)
          $new-length (- (point-max) (point-min)))
    (setq $shift (- $new-length $original-length))
    (setq $shift (+ (- $shift BEG) 1))
    (setq $focus-shift $shift) ;; the $shift for the focused expression
    ;; Insert admits to fill the function holes
    (when INSERT_ADMITS
      ;; Define utility functions, to insert text and update the shift at the same time
      (let*
          ;; Insert text and update the focused term shift at the same time
          (($insert-shift
            (lambda (TEXT)
              ;; If before the term under focus: update the shift
              (when (<= (point) (+ $focus-shift (fem-subexpr-beg SUBEXPR)))
                (setq $focus-shift (+ $focus-shift (length TEXT))))
              ;; Insert the text
              (insert TEXT)))
           ;; Compute the indentation for a token in the original buffer
           ($compute-token-indent
            (lambda (TK)
              (let ($indent)
                (switch-to-buffer $prev-buffer)
                (setq $indent (fem-compute-local-indent-p (fem-pair-fst (fem-cfp-tk-pos TK))))
                (switch-to-buffer DEST_BUFFER)
                $indent)))
           ;; Compute the indentation for a specific position in the ORIGINAL buffer
           ($compute-indent
            (lambda (POS)
              (let ($indent)
                (switch-to-buffer $prev-buffer)
                (setq $indent (fem-compute-local-indent-p POS))
                (switch-to-buffer DEST_BUFFER)
                $indent)))
           ;; Go to the beginning of a specific token in the TARGET buffer
           (target-goto-token
            (lambda (TK)
              (goto-char (+ (fem-pair-fst (fem-cfp-tk-pos TK)) $shift))))
           ;; Insert some text before a token and at the end of the target buffer,
           ;; and use the same indentation for the text at the end
           (insert-same-indent
            (lambda (TK TEXT_BEFORE TEXT_AFTER &optional USE_PARENT)
              (let* (($tk (if USE_PARENT (fem-cfp-tk-parent TK) TK))
                     ($indent (funcall $compute-token-indent $tk)))
                ;; Insert at the end
                (when TEXT_AFTER
                  (goto-char (point-max))
                  (funcall $insert-shift "\n")
                  (funcall $insert-shift $indent)
                  (funcall $insert-shift
                           (fem-replace-in-string "\n" (concat "\n" $indent) TEXT_AFTER)))
                ;; Insert before the token
                (when TEXT_BEFORE
                  (funcall target-goto-token $tk)
                  (funcall $insert-shift TEXT_BEFORE)))))
           )
        ;; Start filling the holes
        ;; First: if the focused expression ends with 'in or 'semicol we need to insert
        ;; an ``admit()``
        (when (or (fem-subexpr-is-let-in SUBEXPR)
                  (fem-subexpr-has-semicol SUBEXPR))
          (goto-char (point-max))
          (funcall $insert-shift " admit()"))
        ;; Then, we need to use the stack to determine where to insert admit
        ;; First, filter the stack in order to remove the first 'let (which is
        ;; at the top level declaring the current definition)
        (setq $stack (fem-cfp-state-filter-state-stack PARSE_STATE))
        ;; Then, whenever we encounter 'let, 'match, 'then, 'begin or an open bracket,
        ;; we fill the hole
        (while $stack
          (let (($tk (car $stack)))
            (setq $stack (cdr $stack)
                  $symbol (fem-cfp-tk-symbol $tk))
            ;; Switch on the token
            (cond
             ((string= $symbol 'let)
              (funcall insert-same-indent $tk nil "in admit()"))
             ;; Note: the user shouldn't use the command inside the condition
             ;; of an if, but it costs nothing to implement this part for now.
             ((string= $symbol 'if)
              (funcall insert-same-indent $tk nil "then admit() else admit()"))
             ((string= $symbol 'then)
              (funcall insert-same-indent $tk nil "else admit()" t))
             ((string= $symbol 'begin)
              (funcall insert-same-indent $tk nil "end"))
             ;; Note: same as for 'if: shouldn't use the command inside the
             ;; scrutinee of a match
             ((string= $symbol 'match)
              (funcall insert-same-indent $tk "begin " "with | _ -> admit()\nend"))
             ((string= $symbol 'with)
              (funcall insert-same-indent $tk "begin " "| _ -> admit()\nend" t))
             ((string= $symbol 'open-bracket)
              (funcall insert-same-indent $tk nil ")"))
             ((string= $symbol 'open-curly)
              (funcall insert-same-indent $tk nil "}"))
             ((string= $symbol 'open-square)
              (funcall insert-same-indent $tk nil "]"))
             ;; Default case: do nothing
             (t nil)))))) ;; end of (when INSERT_ADMITS ...)
    ;; Shift and return the parsing information
    (setq $res (copy-fem-subexpr SUBEXPR))
    (fem-shift-subexpr-pos $focus-shift $res)))

(defun fem-query-fstar-on-buffer-content (OVERLAY_END PAYLOAD LAX CONTINUATION)
  "Send PAYLOAD to F* and call CONTINUATION on the result.
CONTINUATION must an overlay, a status and a response as arguments.
If LAX is t, perform lax type checking.
OVERLAY_END gives the position at which to stop the overlay."
  (let* (($beg (fstar-subp--untracked-beginning-position))
         $typecheck-kind $overlay)
    ;; Create the overlay
    (setq $overlay (make-overlay (fstar-subp--untracked-beginning-position)
                                 OVERLAY_END (current-buffer) nil nil))
    (fstar-subp-remove-orphaned-issue-overlays (point-min) (point-max))
    (overlay-put $overlay 'fstar-subp--lax nil)
    (fstar-subp-set-status $overlay 'busy)
    ;; Query F*
    (fem-log-dbg "sending query to F*:[\n%s\n]" PAYLOAD)
    ;; Before querying F*, log a message signifying the beginning of the F* output
    (message "%s" fem-start-fstar-msg)
    (setq $typecheck-kind (if LAX `lax `full))
    (fstar-subp--query (fstar-subp--push-query $beg $typecheck-kind PAYLOAD)
                       (apply-partially CONTINUATION $overlay))))

(defun fem-generate-fstar-check-conditions ()
  "Check that it is safe to run some F* meta-processing."
  (save-excursion
    (let (($p (point)) $next-point)
      ;; F* mustn't be busy as we won't push a query to the queue but will directly
      ;; query the F* sub-process: if some processes are queued, we will mess up
      ;; with the internal proof state
      (when (fstar-subp--busy-p) (user-error "The F* process must be live and idle"))
      ;; Retract so that the current point is not in a processed region
      (fstar-subp-retract-until $p)
      ;; Check if the point is in the next block to process: if not, abort.
      ;; If we can't compute the next block it is (quite) likely that there are
      ;; no previous blocks to process.
      (setq $next-point (fstar-subp--find-point-to-process 1))
      (when (and $next-point (< $next-point $p))
        (user-error (concat "There may be unprocessed definitions above the "
                            "current position: they must be processed"))))))

(defun fem-compute-local-indent-p (&optional POS)
  "Return a string corresponding to the indentation up to POS.
If the characters between the beginning of the line and the current position
are comments and spaces, the returned string is equal to those - the reason
is that it allows formatting of ghosted code (which uses (**)).
Otherwise, the string is made of a number of spaces equal to the column position"
  (save-restriction
    (when POS (goto-char POS))
    (let* (($ip2 (point))
           ($ip1 (progn (beginning-of-line) (point)))
           ($indent (- $ip2 $ip1)))
      (if (fem-region-is-comments-and-spaces $ip1 $ip2)
          (buffer-substring-no-properties $ip1 $ip2)
        (make-string $indent ? )))))

(defun bool-to-string (B)
  "Return 'false' if B is nil, 'true' otherwise."
  (if B "true" "false"))

;; Parsing result
(cl-defstruct fem-cfp-result
  subexpr ;; the last parsed subexpression
  state ;; the parsing state
  )

(defun fem-parse-until-assert (&optional P0)
  "Detect an assertion/assumption under the pointer and parse until after the assertion."
  (let ($parse-beg $passert $p0 $assert-beg $assert-end $subexpr $state $parse-end)
    (setq $p0 (or P0 (point)))
    ;; Parse the assertion/assumption. Note that we may be at the beginning of a
    ;; line with an assertion/assumption, or just after the assertion/assumption.
    ;; so we first try to move.
    (setq $parse-beg (fstar-subp--untracked-beginning-position))
    ;; First: we are inside or before the assert: move forward
    (when (or (fem-is-in-spaces-p) (fstar-in-comment-p)) (fem-skip-comments-and-spaces t))  
    (setq $passert (fem-find-assert-assume-p (point) $parse-beg))
    ;; If not found: move backward and eventually ignore a ';'
    (when (not $passert)
      (goto-char $p0)
      (fem-skip-comments-and-spaces nil (point-at-bol))
      (when (fem-previous-char-is-semicol-p)
        (backward-char))
      (setq $passert (fem-find-assert-assume-p (point) $parse-beg)))
    (when (not $passert) (error "Pointer not over an assert/assert_norm/assume"))
    ;; Parse the encompassing let (if there is)
    (setq $assert-beg (fem-pair-fst (fem-pair-fst $passert))
          $assert-end (fem-pair-snd (fem-pair-snd $passert)))
    (setq $subexpr (fem-find-encompassing-let-in $assert-beg $assert-end))
    (when (not $subexpr) (error "Could not parse the expression around the assertion/assumption"))
    ;; Parse the function up to (and including) the assertion
    (setq $state (fem-create-cfp-state $parse-beg))
    (setq $parse-end (fem-subexpr-end $subexpr))
    (fem-cfp-parse-tokens $parse-end $state)
    (make-fem-cfp-result :subexpr $subexpr :state $state)))

(defun fem-split-assert-assume-conjuncts ()
  "Split the conjunctions in an assertion/assumption."
  (interactive)
  (let ($markers $p0
        $parse-beg $parse-end
        $subexpr $state
        $parse-result
        $indent-str $insert-beg $insert-end
        $cbuffer $insert-admits $pp-instr $subexpr1
        $prefix $prefix-length $payload)
    (fem-log-dbg "split-assert-conjuncts")
    ;; Sanity check
    (fem-generate-fstar-check-conditions)
    ;; Look for position markers
    (setq $markers (fem-get-pos-markers))
    (setq $p0 (point))
    (when $markers (goto-char (fem-pair-fst $markers)))
    ;; Parse the assertion
    (setq $parse-result (fem-parse-until-assert))
    (setq $subexpr (fem-cfp-result-subexpr $parse-result)
          $state (fem-cfp-result-state $parse-result))
    (setq $parse-beg (fstar-subp--untracked-beginning-position)
          $parse-end (fem-cfp-state-pos $state))
    ;; Compute the indentation
    (setq $indent-str (fem-compute-local-indent-p (fem-subexpr-beg $subexpr)))
    ;; Expand the region to ignore comments
    (setq $insert-beg (fem-subexpr-beg $subexpr))
    (goto-char (fem-subexpr-end $subexpr))
    (fem-skip-comments-and-spaces t (point-at-eol))
    (when (fem-in-general-comment-p) (goto-char (fem-subexpr-end $subexpr)))
    (setq $insert-end (point))
    ;; Remember which is the original buffer
    (setq $cbuffer (current-buffer))
    ;; Copy and start processing the content
    (setq $insert-admits (not $markers))
    (setq $pp-instr (concat "FStar.InteractiveHelpers.pp_split_assert_conjs " (bool-to-string fem-debug)))
    (setq $subexpr1 (fem-copy-def-for-meta-process $parse-beg $parse-end $insert-admits
                                                   $subexpr $state fem-process-buffer1
                                                   $pp-instr))
    ;; We are now in the destination buffer
    ;; Insert the ``focus_on_term`` indicator at the proper place, together
    ;; with an admit after the focused term.
    ;; Note that we don't need to keep track of the positions modifications:
    ;; we will send the whole buffer to F*.
    ;; Prefix
    (goto-char (fem-subexpr-beg $subexpr1))
    (setq $prefix (concat "let _ = FStar.InteractiveHelpers.focus_on_term in\n" $indent-str))
    (setq $prefix-length (length $prefix))
    (insert $prefix)
    ;; Suffix
    (goto-char (+ (fem-subexpr-end $subexpr1) $prefix-length))
    ;; Copy the buffer content
    (setq $payload (buffer-substring-no-properties (point-min) (point-max)))
    ;; We need to switch back to the original buffer to query the F* process
    (switch-to-buffer $cbuffer)
    ;; Go back to the original point to prevent screen movements
    (goto-char $p0)
    ;; Deactivate the mark to prevent it from appearing on top of the overlay (it is ugly)
    (setq deactivate-mark t)
    ;; Query F*
    (fem-query-fstar-on-buffer-content $insert-end $payload t
                                       (apply-partially #'fem-insert-assert-pre-post--continuation
                                                        $indent-str $insert-beg $insert-end))))

(defun fem-identifier-at-p (&optional POS)
  "Find the identifier under the pointer or POS.
Return a pair of positions delimiting the beginning and end of the identifier."
  (save-match-data
    (save-excursion
      (let ($p0 $p1)
        (skip-chars-backward fem-identifier-charset)
        (setq $p0 (point))
        (if (not (looking-at fem-identifier-regexp))
            nil
          (setq $p1 (match-end 0))
          (make-fem-pair :fst $p0 :snd $p1))))))

(defun fem-unfold-in-assert-assume ()
  "Unfold an identifier in an assertion/assumption."
  (interactive)
  (let ($markers $p0 $id
        $parse-result $parse-beg $parse-end $subexpr $state
        $indent-str $insert-beg $insert-end $cbuffer $insert-admits
        $subexpr1 $pp-instr $insert-shift $shift $payload)
    (fem-log-dbg "unfold-in-assert-assume")
    ;; Sanity check
    (fem-generate-fstar-check-conditions)
    ;; Look for position markers
    (setq $markers (fem-get-pos-markers))
    (setq $p0 (point))
    (when $markers (goto-char (fem-pair-fst $markers)))
    ;; Find the identifier by computing its delimiting positions
    (if (and $markers (fem-pair-snd $markers))
        ;; There is a second marker: use it as the end of the identifier
        (setq $id $markers)
      ;; Otherwise: use the active selection if there is one
      (if (use-region-p)
          ;; Use the selected region
          (setq $id (make-fem-pair :fst (region-beginning) :snd (region-end)))
        ;; Last case: parse the identifier under the pointer
        (setq $id (fem-identifier-at-p))
        (when (not $id) (error "Pointer not over a term"))))
    ;; Parse the assertion/assumption.
    ;; Parse the assertion
    (setq $parse-result (fem-parse-until-assert))
    (setq $subexpr (fem-cfp-result-subexpr $parse-result)
          $state (fem-cfp-result-state $parse-result))
    (setq $parse-beg (fstar-subp--untracked-beginning-position)
          $parse-end (fem-cfp-state-pos $state))
    ;; Compute the indentation
    (setq $indent-str (fem-compute-local-indent-p (fem-subexpr-beg $subexpr)))
    ;; Expand the region to ignore comments
    (setq $insert-beg (fem-subexpr-beg $subexpr))
    (goto-char (fem-subexpr-end $subexpr))
    (fem-skip-comments-and-spaces t (point-at-eol))
    (when (fem-in-general-comment-p) (goto-char (fem-subexpr-end $subexpr)))
    (setq $insert-end (point))
    ;; Remember which is the original buffer
    (setq $cbuffer (current-buffer))
    ;; Copy and start processing the content
    (setq $insert-admits (not $markers))
    (setq $pp-instr (concat "FStar.InteractiveHelpers.pp_unfold_in_assert_or_assume " (bool-to-string fem-debug)))
    (setq $subexpr1 (fem-copy-def-for-meta-process $parse-beg $parse-end $insert-admits
                                                   $subexpr $state fem-process-buffer1
                                                   $pp-instr))
    ;; We are now in the destination buffer
    ;; Insert the ``focus_on_term`` indicators at the proper places, together
    ;; with an admit after the focused term.
    ;; Prefixes:
    (setq $insert-shift 0)
    (defun $insert-and-shift (STR)
      (setq $insert-shift (+ $insert-shift (length STR)))
      (insert STR))
    ;; - for the identifier - note that we need to compute the shift between the
    ;;   buffers
    (setq $shift (- (fem-subexpr-beg $subexpr1) (fem-subexpr-beg $subexpr)))
    (goto-char (+ (fem-pair-snd $id) $shift))
    ($insert-and-shift "))")
    (goto-char (+ (fem-pair-fst $id) $shift))
    ($insert-and-shift "(let _ = FStar.InteractiveHelpers.focus_on_term in (")
    ;; - for the assert/assume - note that the above insertion should have been made
    ;;   below the point where we now insert
    (goto-char (fem-subexpr-beg $subexpr1))
    ($insert-and-shift "let _ = FStar.InteractiveHelpers.focus_on_term in\n")
    ;; Copy the buffer content
    (setq $payload (buffer-substring-no-properties (point-min) (point-max)))
    ;; We need to switch back to the original buffer to query the F* process
    (switch-to-buffer $cbuffer)
    ;; Go back to the original point to prevent screen movements
    (goto-char $p0)
    ;; Deactivate the mark to prevent it from appearing on top of the overlay (it is ugly)
    (setq deactivate-mark t)
    ;; Query F*
    (fem-query-fstar-on-buffer-content $insert-end $payload t
                                       (apply-partially #'fem-insert-assert-pre-post--continuation
                                                        $indent-str $insert-beg $insert-end))))

(defun fem-parse-until-decl (STATE P0 &optional P1)
  "Parse until P0 and return the declaration under P0.
Use STATE as the parsing STATE and update it during parsing.
If P1 is nil, parse until P0.
If P1 is not nil, parse until P0 and consider the term is delimited by P0 and P1.
Return a fem-subexpr."
  (let ($state $parent $last-tk $last-symbol $last-tk-pos $tk-beg $tk-end
        $is-let-in $has-semicol)
    (setq $state STATE)
    (cond

     ;; If P1 is not nil
     (P1
      ;; Parse until P0
      (fem-cfp-parse-tokens P0 $state)
      ;; Parse until the end of P1
      (fem-cfp-parse-tokens P1 $state)
      ;; Retrieve the last token
      (setq $last-tk (fem-cfp-state-top $state))
      (setq $last-symbol (fem-cfp-tk-symbol $last-tk))
      ;; Switch on the last token to figure out the expression type
      (cond
       ;; Case 1: 'in
       ((string= $last-symbol 'in)
        (setq $is-let-in t))
       ;; Case 2: 'semicol
       ((string= $last-symbol 'semicol)
        (setq $has-semicol t))
       (t nil))
      ;; Compute the begining and end positions
      (goto-char P0)
      (fem-skip-comments-and-spaces t P1)
      (setq $tk-beg (point))
      (goto-char P1)
      (fem-skip-comments-and-spaces nil $tk-beg)
      (setq $tk-end (point))
      )

     ;; If P1 is nil
     (t
      (fem-cfp-parse-tokens P0 $state)
      ;; Check the parsing result
      (setq $last-tk (fem-cfp-state-top $state))
      (when (not $last-tk) (error "Could not parse the current function"))
      (setq $last-tk (fem-cfp-state-top $state))
      (setq $last-tk-pos (fem-cfp-tk-pos $last-tk))
      (setq $last-symbol (fem-cfp-tk-symbol $last-tk))
      (cond
       ;; Case 1: last symbol is 'in
       ((string= $last-symbol 'in)
        (setq $is-let-in t)
        ;; There should be a parent 'let
        (setq $parent (fem-cfp-tk-parent $last-tk))
        ;; Retrieve the beginning and end delimiters
        (setq $tk-beg (fem-pair-fst (fem-cfp-tk-pos $parent))
              $tk-end (fem-pair-snd $last-tk-pos)))
       ;; Case 2: last symbol is 'semicol
       ((string= $last-symbol 'semicol)
        (setq $has-semicol t)
        ;; There should be a parent
        (setq $parent (fem-cfp-tk-parent $last-tk))
        ;; Retrieve the beginning and end delimiters - notice that we don't
        ;; include the parent delimiter
        (goto-char (fem-pair-snd (fem-cfp-tk-pos $parent)))
        (fem-skip-comments-and-spaces t)
        (setq $tk-beg (point)
              $tk-end (fem-pair-snd $last-tk-pos)))
       ;; Case 3: unknown last symbol: the studied term is a return value
       (t
        ;; Look for the beginning of the current token
        (setq $parent (fem-cfp-state-find-prev-decl-end-token $state))
        ;; Retrieve the beginning and end delimiters - notice that we don't
        ;; include the parent delimiter
        (goto-char (fem-pair-snd (fem-cfp-tk-pos $parent)))
        (fem-skip-comments-and-spaces t)
        (setq $tk-beg (point)
              $tk-end (fem-pair-snd $last-tk-pos))))))

     ;; Return
     (make-fem-subexpr :beg $tk-beg :end $tk-end :is-let-in $is-let-in
                       :has-semicol $has-semicol)))

(defun fem-analyze-effectful-term (WITH_GPRE WITH_GPOST)
  "Insert assertions with proof obligations and postconditions around a term.
If WITH_GPRE/WITH_GPOST is t, try to insert the goal precondition/postcondition."
  (interactive)
  (fem-log-dbg "insert-assert-pre-post")
  ;; Sanity check
  (fem-generate-fstar-check-conditions)
  (let (
        $p0 $markers $state $parse-beg $parse-end $term $term-beg $term-end $indent-str
        $insert-beg $insert-end $insert-admits $cbuffer $pp-instr $term1 $payload)
    (setq $parse-beg (fstar-subp--untracked-beginning-position))
    (setq $state (fem-create-cfp-state $parse-beg))
    ;; Find markers
    (setq $markers (fem-get-pos-markers))
    (setq $p0 (point)) ;; save the current position
    (when $markers (goto-char (fem-pair-fst $markers)))
    ;; Do a different parsing depending on the presence of markers or not
    (cond
     ;; If there are no markers: check if there is a selection
     ((not $markers)
      ;; If there is a selection: use it
      (if (use-region-p)
          (progn
            (setq $term-beg (region-beginning) $parse-end (region-end))
            (goto-char $term-beg)
            (fem-skip-comments-and-spaces t $parse-end)
            (setq $term-beg (point))
            (goto-char $parse-end)
            (fem-skip-comments-and-spaces nil $term-beg)
            (setq $parse-end (point))
            (setq $term (fem-parse-until-decl $state $term-beg $parse-end)))
        ;; Otherwise: parse until the current position
        (goto-char $p0)
        (fem-skip-comments-and-spaces nil $parse-beg)        
        (setq $parse-end (point))
        (setq $term (fem-parse-until-decl $state $parse-end))))
     ;; If there are two markers, they delimit a region: parse until the beginning,
     ;; then parse until the end of the region, then check the shape of the binding
     ;; (semicol declaration, let-binding or unknown and thus return value)
     ((and (fem-pair-fst $markers) (fem-pair-snd $markers))
      (setq $parse-end (fem-pair-snd $markers))
      (setq $term (fem-parse-until-decl $state (fem-pair-fst $markers) $parse-end)))
     ;; If there is only one marker: just parse until there
     (t
      (setq $parse-end (fem-pair-fst $markers))
      (setq $term (fem-parse-until-decl $state $parse-end))))
    (setq $term-beg (fem-subexpr-beg $term) $term-end (fem-subexpr-end $term))
    ;; Find the points where to insert the generated assertions (go at the
    ;; beginning of the term and try to reach the beginning of the line,
    ;; go at the end of the term and try to reach the end of the line)
    (goto-char $term-beg)
    (setq $insert-beg (point))
    (goto-char $term-end)
    (fem-skip-comments-and-spaces t (point-at-eol))
    (when (fem-in-general-comment-p) (goto-char $term-end))
    (setq $insert-end (point))
    ;; Compute the local indent
    (setq $indent-str (fem-compute-local-indent-p $term-beg))
    ;; Options for the copying function: insert admits only if there are no markers
    (setq $insert-admits (not $markers))
    ;; Remember the current buffer in order to be able to switch back
    (setq $cbuffer (current-buffer))
    ;; Copy and start processing the content
    (setq $pp-instr (concat "FStar.InteractiveHelpers.pp_analyze_effectful_term "
                            (bool-to-string fem-debug) " "
                            (bool-to-string WITH_GPRE) " "
                            (bool-to-string WITH_GPOST)))
    (setq $term1 (fem-copy-def-for-meta-process $parse-beg $parse-end $insert-admits
                                                $term $state fem-process-buffer1
                                                $pp-instr))
    ;; We are now in the destination buffer
    ;; Modify the copied content and leave the pointer at the end of the region
    ;; to send to F*
    ;;
    ;; Insert the ``focus_on_term`` indicator at the proper place, together
    ;; with an admit after the focused term.
    ;; Note that we don't need to keep track of the positions modifications:
    ;; we will send the whole buffer to F*.
    (goto-char (fem-subexpr-beg $term1))
    (insert "let _ = FStar.InteractiveHelpers.focus_on_term in\n")
    (insert $indent-str)
    ;; Copy the buffer content
    (setq $payload (buffer-substring-no-properties (point-min) (point-max)))
    ;; We need to switch back to the original buffer to query the F* process
    (switch-to-buffer $cbuffer)
    ;; Go back to the original point to prevent screen movements
    (goto-char $p0)
    ;; Deactivate the mark to prevent it from appearing on top of the overlay (it is ugly)
    (setq deactivate-mark t)
    ;; Query F*
    (fem-query-fstar-on-buffer-content $insert-end $payload t
                                   (apply-partially #'fem-insert-assert-pre-post--continuation
                                                    $indent-str $insert-beg $insert-end))))

(defun fem-switch-assert-assume ()
  "Switch between assertion and assumption under the pointer or in the current selection."
  (interactive)
  (fem-switch-assert-assume-p-or-selection))

(defun fem-analyze-effectful-term-no-goal ()
  "Insert assertions with proof obligations and postconditions around a term."
  (interactive)
  (fem-analyze-effectful-term nil nil))

(defun fem-analyze-effectful-term-with-goal ()
 "Do the same as fem-analyze-effectful-term but also include global pre/postcondition."
  (interactive)
  (fem-analyze-effectful-term t t))

(defun fem-analyze-effectful-term-no-pre ()
  "Insert assertions with proof obligations and postconditions around a term."
  (interactive)
  (fem-analyze-effectful-term nil t))

(provide 'fstar-meta)
;;; fstar-meta.el ends here
