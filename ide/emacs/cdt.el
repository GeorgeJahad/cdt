;;;; cdt.el --- Emacs interface to the Clojure Debugging Toolkit
;;;  (gud-jdb-find-source-using-classpath) and (jdb) derived from 
;;;        jdb mode in emacs gud.el
;;;  New portions: Copyright (C) 2010 George Jahad
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;



(require 'sregex)
(require 'gud)



(defun jdb (command-line)
  "Run jdb with command line COMMAND-LINE in a buffer.
The buffer is named \"*gud*\" if no initial class is given or
\"*gud-<initial-class-basename>*\" if there is.  If the \"-classpath\"
switch is given, omit all whitespace between it and its value.

See `gud-jdb-use-classpath' and `gud-jdb-classpath' documentation for
information on how jdb accesses source files. Alternatively (if
`gud-jdb-use-classpath' is nil), see `gud-jdb-directories' for the
original source file access method.

For general information about commands available to control jdb from
gud, see `gud-mode'."
  (interactive
   (list (gud-query-cmdline 'jdb)))
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


  (gud-def gud-step   "step"          "\C-s" "Step one source line with display.")
  (gud-def gud-next   "next"          "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "(cont)"          "\C-r" "Continue with display.")
  (gud-def gud-finish "step up"       "\C-f" "Continue until current method returns.")
  (gud-def gud-up     "up\C-Mwhere"   "<"    "Up one stack frame.")
  (gud-def gud-up2     "up\C-Mwhere"   "\C-u"    "Up one stack frame.")
  (gud-def gud-down   "down\C-Mwhere" ">"    "Up one stack frame.")
  (gud-def gud-down2   "down\C-Mwhere" "\C-d"    "Down one stack frame.")
  (gud-def gud-print  "print %e"  "\C-p" "Evaluate Java expression at point.")

  (global-set-key (vconcat gud-key-prefix "\C-b") 'cljdb-break)
  (global-set-key (vconcat gud-key-prefix "\C-t") 'cljdb-print-this)
  (global-set-key (vconcat gud-key-prefix "\C-l") 'cljdb-print-locals)
  (setq clj-classes nil)

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

	







(defun jdb-next-token ()
  (save-excursion
    (forward-sexp)
    (forward-sexp)
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring-no-properties (point) end))))


(defun jdb-defn-name ()
  (save-excursion
    (beginning-of-defun)
    (forward-char)
    (let ((defn-name  (jdb-next-token)))
      ;; skip meta data
      (while (string= (substring defn-name 0 1) "#")
	(forward-sexp)
	(setq defn-name (jdb-next-token)))
      defn-name)))

(setq jdb-ns-regex  (sregexq bol "(" (0+ " ") (or "ns" "in-ns") word-boundary))

(defun jdb-find-ns ()
  (interactive)
  ;; An ugly hack using regex's to find ns/in-ns, but its all I've got
  ;; for now
  (if (not (re-search-forward jdb-ns-regex (buffer-size) t))
      (error "Clojure namespace not found in file"))
  (backward-sexp))

    

(defun jdb-ns ()
  (save-excursion
    (beginning-of-buffer)
    (jdb-find-ns)
    (let ((namespace  (jdb-next-token)))
      ;; skip meta data
      (while (string= (substring namespace 0 1) "#")
	(forward-sexp)
	(setq namespace (jdb-next-token)))
      (if  (string= (substring namespace 0 1) "'")
	  (substring namespace  1 (length namespace))
	namespace))))


(setq jdb-fixup-strings 
      '("-"  "_" 
	"+"  "_PLUS_" 
	">"  "_GT_" 
	"<"  "_LT_" 
	"="  "_EQ_" 
	"*"  "_STAR_" 
	"/"  "_SLASH_" 
	"!"  "_BANG_" 
	"?"  "_QMARK_"))

(defun jdb-fixup-name (name strings)
  (if (not (car strings))
      name
    (jdb-fixup-name
     (replace-regexp-in-string (car strings) (cadr strings) name)
     (cddr strings))))

(defun cljdb-java-class-name ()
  (interactive)
  )

(defun jdb-class-name ()
  (interactive)
  (if (string-match  "\\.java" (buffer-file-name))
      (gud-find-class (buffer-file-name)  (line-number-at-pos))
    (jdb-fixup-name (format "%s$%s__" (jdb-ns) (jdb-defn-name))
		    jdb-fixup-strings)))
    

(defun jdb-find-class-intern ( line)
  (interactive)
  (let ((classes clj-classes)
	(class-name  (jdb-class-name))
	(found nil))
    (while (and (not found) (car classes))
      (if (string-match class-name (car classes))
	  (setq found 
		(jdb-set-breakpoint
		 (format "%s:%s" (car classes) line))))
      (setq classes (cdr classes)))
    found))




(defun jdb-find-class ()
  (interactive)
  (let ((line (line-number-at-pos)))
    (if (not (jdb-find-class-intern line))
	(progn
	  (if gud-comint-buffer
	      (setq clj-classes 
		    (gud-gdb-run-command-fetch-lines 
		     "classes" gud-comint-buffer)))
	  (if (not (jdb-find-class-intern line))
	      (message 
	       (format "unable to set breakpoint for line %s in %s" 
		       line (jdb-class-name)))
	    (message
	     (format "set breakpoint for line %s in %s"
		     line (jdb-class-name)))))
      (message
       (format "set breakpoint for line %s in %s"
	       line (jdb-class-name))))))
	  


(defun jdb-set-breakpoint (string)
  (interactive)
  (let ((output 
	 (gud-gdb-run-command-fetch-lines 
	  (format "stop at %s" string)
	  gud-comint-buffer)))
    (if (not (string-match "^Set breakpoint " (car output)))
	(progn 
	  (gud-gdb-run-command-fetch-lines 
	   (format "clear %s" string)
	   gud-comint-buffer)
	  nil)
      output)))

(defun cljdb-break ()
  (interactive)
    (jdb-find-class))
	  
(setq cljdb-version .4)









(setq cljdb-replace-regex (sregexq (or "("  ":")))

(setq jdb-break-step-string
       (sregexq bol (or "Step completed: " "Breakpoint hit: ") 
		(1+ any) "bci=" (1+ any) eol))


(defun gud-cljdb-find-source (p f)
  "Find source file corresponding to fully qualified class p.
Convert p from jdb's output, converted to a pathname
relative to a classpath directory."
  (save-match-data
    (let*
	( ;; Replace dots with slashes and append ".java" to generate file
	 ;; name relative to classpath
	 (classname
	  (mapconcat 'identity
		     (split-string
		      ;; Eliminate any subclass references in the class
		      ;; name string. These start with a "$"
		      ((lambda (x)
			 (if (string-match "$.*" x)
			     (replace-match "" t t x) p))
		       p)
		      "\\.") "/"))
	 (file-prefix
	  (car
	   (split-string 
	    (replace-regexp-in-string cljdb-replace-regex "" f) 
	    "\\.")))

	 (filename 
	  (if (string-match (regexp-quote ".java:") f)
	      (concat classname ".java")
	    (if (string= (car (last (split-string classname "/")))
			 file-prefix)
		(concat classname ".clj")
	      (concat classname "/" file-prefix ".clj"))))

	 (cplist (append gud-jdb-sourcepath gud-jdb-classpath))
	 found-file)
      (while (and cplist
		  (not (setq found-file
			     (file-readable-p
			      (concat (car cplist) "/" filename)))))
	(setq cplist (cdr cplist)))
      (if found-file (concat (car cplist) "/" filename)))))

(defun gud-jdb-marker-filter (string)
  		(setq gud-last-frame
		    (cons "/home/gbj/incoming/cdt.new.diff" 63 ))
		string)
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

(defun cljdb-print-this ()
  (interactive)
  (switch-to-buffer gud-comint-buffer)
  (gud-call "print this"))

(defun cljdb-insert-line (line)
  (insert (format "%s\n" line))
  (set-marker (process-mark (get-buffer-process gud-comint-buffer)) (point)))



(defun cljdb-print-locals ()
  (interactive)
  (let ((locals 
	 	  (cljdb-print-locals-intern      	  
		   (gud-gdb-run-command-fetch-lines 
		    "locals"
		    gud-comint-buffer)
		   nil)))
    (switch-to-buffer gud-comint-buffer)
    (end-of-buffer)
    (cljdb-insert-line "\n")
    (mapcar #'cljdb-insert-line locals)
    (cljdb-insert-line "\n"))
  (gud-call "\n"))

(setq cljdb-locals-regex ( sregexq (group (1+ any)) " ="))

(defun cljdb-print-locals-intern (locals acc)
  (if  (car locals)
      (progn
	(if (string-match cljdb-locals-regex (car locals))
	    (cljdb-print-locals-intern 
	     (cdr locals) 
	     (append acc 
		     (gud-gdb-run-command-fetch-lines  
		      (format "print %s" (match-string 1 (car locals)))
		      gud-comint-buffer)))
	  (if (string-match "No local variables" (car locals))
	      '("No local variables")
	    (cljdb-print-locals-intern (cdr locals) acc))))
    acc))





