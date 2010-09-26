;;;; cdt.el --- Emacs interface to the Clojure Debugging Toolkit
;;;  derived from jdb mode in emacs gud.el
;;;  New portions: Copyright (C) 2010 George Jahad
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;

(require 'gud)
(require 'thingatpt)

(defun cdt-repl ()
  (interactive)
  (switch-to-buffer gud-comint-buffer))

(defun cdt-here ()
  (interactive)
  (gud-call "(print-current-location)"))

(defun cdt-print ()
  (interactive)
  (gud-call (format "(reval-display '%s)" (thing-at-point 'sexp))))

(defun strip-trail (path)
  (if (= (elt path (- (length path) 1)) ?/)
      (substring path 0 (- (length path) 1))
    path))

(defun cdt-query-cmdline ()
  (let ((path (strip-trail cdt-dir)))
    (format "java -classpath%s/lib/clojure-1.2.0.jar:%s/lib/clojure-contrib-1.2.0.jar:%s/lib/debug-repl-0.3.0-20091229.021828-3.jar:%s/src clojure.main --repl"
	    path path path path)))

(defun cdt (port)
  "Run cdt with command line COMMAND-LINE in a buffer.
The buffer is named \"*gud*\" if no initial class is given or
\"*gud-<initial-class-basename>*\" if there is.  If the \"-classpath\"
switch is given, omit all whitespace between it and its value."

  (interactive "sPort to connect to : ")

  (gud-common-init (cdt-query-cmdline) 'gud-jdb-massage-args
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
  (gud-def gud-status  "(status-report)"  "s" "Status report")
  (gud-def gud-this   "(reval-display 'this)"   "\C-t" "print this pointer")

  (global-set-key (vconcat gud-key-prefix "\C-h") 'cdt-here)
  (global-set-key (vconcat gud-key-prefix "\C-r") 'cdt-repl)
  (global-set-key (vconcat gud-key-prefix "\C-p") 'cdt-print)

  (setq comint-prompt-regexp "^[^ ]*=>")

  (setq paragraph-start comint-prompt-regexp)
  (gud-call "(use 'com.georgejahad.cdt)")
  (gud-call (format "(set-source-path \"%s\")" cdt-source-path))
  (gud-call (format "(cdt-attach %s)" port))
  (run-hooks 'jdb-mode-hook))

(setq cdt-el-version .1)

(defun gud-display-frame ()
  "Find and obey the last filename-and-line marker from the debugger.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (when gud-last-frame
    (gud-set-buffer)
    (gud-display-line (car gud-last-frame) (cdr gud-last-frame))
    (message (format "Frame: %s %s %s" cdt-frame (car gud-last-frame) (cdr gud-last-frame)))
    (setq gud-last-last-frame gud-last-frame
	  gud-last-frame nil)))

(defun set-frame ()
  (setq gud-last-frame
	(cons (match-string 1 gud-marker-acc)
	      (string-to-number (match-string 2 gud-marker-acc))))
  (setq cdt-frame (match-string 3 gud-marker-acc)))

(defun display-match ()
  (message (match-string 1 gud-marker-acc)))

(defun display-match0 ()
  (message (match-string 0 gud-marker-acc)))

(defun display-message (arg)
  (message arg))

(defun display-bp-error ()
  (message (format "no bp found at line %s" (match-string 1 gud-marker-acc))))

(defun filter-input (regex action &optional arg)
  (while (string-match regex gud-marker-acc)
    (if (match-string 1 gud-marker-acc)
	(if arg
	    (funcall action arg)
	  (funcall action)))
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
    (filter-input "\\(Source not found\\)"  'display-match)
    (filter-input "\\(command can only be run after stopping at an breakpoint or exception\\)"  'display-match0)
    (filter-input "CDT location is \\(.+\\):\\(.+\\):\\(.+\\)" 'set-frame)
    (filter-input "CDT reval returned \\(.+\\)$"  'display-match)
    (filter-input "no breakpoints found at line \\(.+\\)$"  'display-match0)
    (filter-input "bp set on \\(.+\\)$"  'display-match0)
    (filter-input "Status of current thread is \\(.+\\)$" 'display-match0)
    (filter-input "\\(starting event handler\\)$"  'display-message "CDT ready") 
    (filter-input "exception in event handler \\(.+\\)$" 'display-match0) 
    (filter-input "Unexpected exception generated: \\(.+\\)$" 'display-match0) 
    (filter-input "already at \\(.+\\) of stack $" 'display-match0) 


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
