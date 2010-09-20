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

(defun cdt (command-line)
  "Run cdt with command line COMMAND-LINE in a buffer.
The buffer is named \"*gud*\" if no initial class is given or
\"*gud-<initial-class-basename>*\" if there is.  If the \"-classpath\"
switch is given, omit all whitespace between it and its value."

  (interactive
   (list (gud-query-cmdline 'cdt)))
  (setq gud-jdb-classpath nil)
  (setq gud-jdb-sourcepath nil)
  
  ;; Set gud-jdb-classpath from the CLASSPATH environment variable,
  ;; if CLASSPATH is set.
  (setq gud-jdb-classpath-string (getenv "CLASSPATH"))
  (if gud-jdb-classpath-string
      (setq gud-jdb-classpath
	    (gud-jdb-parse-classpath-string gud-jdb-classpath-string)))
  (setq gud-jdb-classpath-string nil)	; prepare for next

  (gud-common-init command-line 'gud-jdb-massage-args
		   'gud-jdb-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'jdb)

  ;; If a -classpath option was provided, set gud-jdb-classpath
  (if gud-jdb-classpath-string
      (setq gud-jdb-classpath
	    (gud-jdb-parse-classpath-string gud-jdb-classpath-string)))
  (setq gud-jdb-classpath-string nil)	; prepare for next
  ;; If a -sourcepath option was provided, parse it
  (if gud-jdb-sourcepath
      (setq gud-jdb-sourcepath
	    (gud-jdb-parse-classpath-string gud-jdb-sourcepath)))


  (gud-def gud-break  "(line-bp %d/%f %l)"  "\C-b" "breakpoint on current line")
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
  (gud-def gud-print  "(reval %e)"      "\C-p" "Evaluate clojure expression at point.")

  (setq comint-prompt-regexp "^[^ ]*=>")

  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'jdb-mode-hook)

  (if gud-jdb-use-classpath
      ;; Get the classpath information from the debugger
      (progn
	(if (string-match "-attach" command-line)
	    (gud-call "classpath"))
	(fset 'gud-jdb-find-source
	      'gud-jdb-find-source-using-classpath))

    ;; Else create and bind the class/source association list as well
    ;; as the source file list.
    (setq gud-jdb-class-source-alist
	  (gud-jdb-build-class-source-alist
	   (setq gud-jdb-source-files
		 (gud-jdb-build-source-files-list gud-jdb-directories
						  "\\.java$"))))
    (fset 'gud-jdb-find-source 'gud-jdb-find-source-file)))
	  
(setq cdt-el-version .1)

(defun gud-jdb-marker-filter (string)

  ;; Build up the accumulator.
  (setq gud-marker-acc
	(if gud-marker-acc
	    (concat gud-marker-acc string)
	  string))

  ;;  GBJ NOTE: could this happen multiple times??
  ;; Add stack trace to the breakpoint string because it has more info
  ;; (if   (string-match jdb-break-step-string gud-marker-acc)
  ;;     (gud-call "where"))

  ;; ;; Look for classpath information until gud-jdb-classpath-string is found
  ;; ;; (interactive, multiple settings of classpath from jdb
  ;; ;;  not supported/followed)
  ;; (if (and gud-jdb-use-classpath
  ;; 	   (not gud-jdb-classpath-string)
  ;; 	   (or (string-match "classpath:[ \t[]+\\([^]]+\\)" gud-marker-acc)
  ;; 	       (string-match "-classpath[ \t\"]+\\([^ \"]+\\)" gud-marker-acc)))
  ;;     (setq gud-jdb-classpath
  ;; 	    (gud-jdb-parse-classpath-string
  ;; 	     (setq gud-jdb-classpath-string
  ;; 		   (match-string 1 gud-marker-acc)))))

  ;; We process STRING from left to right.  Each time through the
  ;; following loop we process at most one marker. After we've found a
  ;; marker, delete gud-marker-acc up to and including the match
  (let (file-found)
    ;; Process each complete marker in the input.
    (while

	;; Do we see a marker?
	(string-match
	 "CDT location is \\(.+\\):\\(.+\\)"
	 gud-marker-acc)

      ;; A good marker is one that:
      ;; has an "[n] " prefix and n is the lowest prefix seen
      ;;    since the last prompt
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
