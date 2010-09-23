;;;; cdt.el --- Emacs interface to the Clojure Debugging Toolkit
;;;  derived from jdb mode in emacs gud.el
;;;  New portions: Copyright (C) 2010 George Jahad
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;

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
  (gud-call (format "(reval-display %s)" (thing-at-point 'sexp))))

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
  (gud-def gud-cont   "(cont)"          "\C-g" "Go")
  (gud-def gud-finish "(finish)"        "\C-f" "Go until current method returns.")
  (gud-def gud-up2    "(up)"            "\C-u" "Up one stack frame.")
  (gud-def gud-down2  "(down)"          "\C-d" "Down one stack frame.")
  (gud-def gud-this   "(reval-display this)"   "\C-t" "print this pointer")

  (global-set-key (vconcat gud-key-prefix "\C-h") 'cljdb-here)
  (global-set-key (vconcat gud-key-prefix "\C-r") 'cljdb-repl)
  (global-set-key (vconcat gud-key-prefix "\C-p") 'cljdb-print)

  (setq comint-prompt-regexp "^[^ ]*=>")

  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'jdb-mode-hook))

(setq cdt-el-version .1)

(defun set-frame ()
  (setq gud-last-frame
	(cons (match-string 1 gud-marker-acc)
	      (string-to-number (match-string 2 gud-marker-acc)))))

(defun display-message ()
  (message (match-string 1 gud-marker-acc)))

(setq gbjc 0)
(defun display-no-source ()
  (setq gbjc (+ 1 gbjc))
  (message "Source not found"))

(defun filter-input (regex action)
  (while (string-match regex gud-marker-acc)
    (if (match-string 1 gud-marker-acc)
	(funcall action))
    ;; Set the accumulator to the remaining text.
    (setq gud-marker-acc (substring gud-marker-acc (match-end 0)))))


(defun gud-jdb-marker-filter (string)

  ;; Build up the accumulator.
  (setq gud-marker-acc
	(if gud-marker-acc
	    (concat gud-marker-acc string)
	  string))

  (let (file-found)
    ;; Process each complete marker in the input.
    (filter-input "^Source not found$"  'display-no-source)
    (filter-input "CDT location is \\(.+\\):\\(.+\\):" 'set-frame)
    (filter-input "CDT reval returned \\(.+\\)$"  'display-message)



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
