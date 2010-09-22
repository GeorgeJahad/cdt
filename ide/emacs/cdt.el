;;;; cdt.el --- Emacs interface to the Clojure Debugging Toolkit
;;;  derived from jdb mode in emacs gud.el
;;;  New portions: Copyright (C) 2010 George Jahad
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;


;; This is a pretty bad hack job; just wanted to get the proof of
;; concept working

(require 'gud)
(require 'thingatpt)

(defun cljdb-repl ()
  (interactive)
  (switch-to-buffer gud-comint-buffer))

(defun cljdb-here ()
  (interactive)
  (gud-call "(print-current-location)"))

(defun cljdb-print ()
  (interactive)
  (gud-call (format "(reval %s)" (thing-at-point 'sexp))))

(defun cdt (command-line)
  "Run cdt with command line COMMAND-LINE in a buffer.
The buffer is named \"*gud*\" if no initial class is given or
\"*gud-<initial-class-basename>*\" if there is.  If the \"-classpath\"
switch is given, omit all whitespace between it and its value."

  (interactive
   (list (gud-query-cmdline 'cdt)))

  (gud-common-init command-line 'gud-jdb-massage-args
		   'gud-jdb-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'jdb)

  (gud-def gud-break  "(line-bp \"%d%f\" %l)"  "\C-b" "breakpoint on current line")
  (gud-def gud-stepi  "(stepi)"         "\C-i" "Step the smallest possible increment.")
  (gud-def gud-step   "(step)"          "\C-s" "Step one source line with display.")
  (gud-def gud-next   "(step-over)"     "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "(cont)"          "\C-c" "Continue with display.")
  (gud-def gud-finish "(finish)"        "\C-f" "Continue until current method returns.")
  (gud-def gud-up     "(up)"            "<"    "Up one stack frame.")
  (gud-def gud-up2    "(up)"            "\C-u" "Up one stack frame.")
  (gud-def gud-down   "(down)"          ">"    "Down one stack frame.")
  (gud-def gud-down2  "(down)"          "\C-d" "Down one stack frame.")
  (gud-def gud-this   "(reval this)"    "\C-t" "print this pointer")
  (gud-def gud-locals "(locals)"        "\C-l" "print locals")

  (global-set-key (vconcat gud-key-prefix "\C-h") 'cljdb-here)
  (global-set-key (vconcat gud-key-prefix "\C-r") 'cljdb-repl)
  (global-set-key (vconcat gud-key-prefix "\C-p") 'cljdb-print)

  (setq comint-prompt-regexp "^[^ ]*=>")

  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'jdb-mode-hook))
	  
(setq cdt-el-version .1)

(defun gud-jdb-marker-filter (string)

  ;; Build up the accumulator.
  (setq gud-marker-acc
	(if gud-marker-acc
	    (concat gud-marker-acc string)
	  string))

  (let (file-found)
    ;; Process each complete marker in the input.
    (while

	;; Do we see a marker?
	(string-match
	 "CDT location is \\(.+\\):\\(.+\\):"
	 gud-marker-acc)

      ;; Figure out the line on which to position the debugging arrow.
      ;; Return the info as a cons of the form:
      ;;
      ;;     (<file-name> . <line-number>) .

      (if (match-string 1 gud-marker-acc)
	  (setq gud-last-frame
		(cons (match-string 1 gud-marker-acc)
		      (string-to-number (match-string 2 gud-marker-acc)))))


      ;; Set the accumulator to the remaining text.
      (setq gud-marker-acc (substring gud-marker-acc (match-end 0))))

    (if (string-match comint-prompt-regexp gud-marker-acc)
	(progn
	  ;; Clear the accumulator
	  (setq gud-marker-acc (substring gud-marker-acc (match-end 0)))
	  (setq gud-jdb-lowest-stack-level 999))))

  ;; Do not allow gud-marker-acc to grow without bound. If the source
  ;; file information is not within the last 3/4
  ;; gud-marker-acc-max-length characters, well,...
  (if (> (length gud-marker-acc) gud-marker-acc-max-length)
      (setq gud-marker-acc
	    (substring gud-marker-acc
		       (- (/ (* gud-marker-acc-max-length 3) 4)))))

  ;; We don't filter any debugger output so just return what we were given.
  string)
