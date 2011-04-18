;;;; cdt.el --- Emacs interface to the Clojure Debugging Toolkit
;;;  derived from jdb mode in emacs gud.el
;;;  New portions: Copyright (C) 2010 George Jahad
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;

(require 'gud)
(require 'thingatpt)
(require 'arc-mode)

(defvar cdt-dir "")
(defvar cdt-source-path "")

(defun cdt-repl ()
  (interactive)
  (switch-to-buffer gud-comint-buffer))

(defun cdt-here ()
  (interactive)
  (gud-call "(print-current-location (ct) (cf))"))

(defun cdt-print ()
  (interactive)
  (gud-call (format "(reval-display (ct) (cf) '%s)" (thing-at-point 'sexp))))

(defun strip-trail (path)
  (if (= (elt path (- (length path) 1)) ?/)
      (substring path 0 (- (length path) 1))
    path))

(defun cdt-query-cmdline ()
  (let ((path (strip-trail cdt-dir)))
    (format "java -classpath%s/lib/clojure-1.2.0.jar:%s/lib/clojure-contrib-1.2.0.jar:%s/lib/debug-repl-0.3.1.jar:%s/src clojure.main --repl"
	    path path path path)))

(defun cdt (port)
  (interactive "sPort number to connect to : ")

  (gud-common-init (cdt-query-cmdline) 'gud-jdb-massage-args
		      'gud-jdb-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'jdb)

  (gud-def gud-break  "(line-bp \"%d%f\" %l)"  "\C-b" "breakpoint on current line")
  (gud-def gud-stepi  "(stepi (ct))"         "\C-i" "Step the smallest possible increment.")
  (gud-def gud-step   "(step (ct))"          "\C-s" "Step one source line with display.")
  (gud-def gud-next   "(step-over (ct))"     "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "(continue-vm)"          "\C-g" "Go")
  (gud-def gud-finish "(finish (ct))"        "\C-f" "Go until current method returns.")
  (gud-def gud-up2    "(up (ct) (cf))"            "\C-u" "Up one stack frame.")
  (gud-def gud-down2  "(down (ct) (cf))"          "\C-d" "Down one stack frame.")
  (gud-def gud-status  "(status-report (ct))"  "s" "Status report")
  (gud-def gud-this   "(reval-display (ct) (cf) 'this)"   "\C-t" "print this pointer")

  (global-set-key (vconcat gud-key-prefix "\C-h") 'cdt-here)
  (global-set-key (vconcat gud-key-prefix "\C-r") 'cdt-repl)
  (global-set-key (vconcat gud-key-prefix "\C-p") 'cdt-print)

  (setq comint-prompt-regexp "^[^ ]*=>")

  (setq paragraph-start comint-prompt-regexp)
;  (gud-call "(use 'cdt.ui 'cdt.utils 'cdt.events 'cdt.break)")
  (gud-call "(do (use 'cdt.ui) (require '[cdt.utils :as cdtu]))")
  (gud-call "(reset! cdtu/CDT-DISPLAY-MSG true)")
  (gud-call (format "(cdtu/set-source-path \"%s\")" cdt-source-path))
  (gud-call (format "(cdt-attach %s)" port))
  (run-hooks 'jdb-mode-hook))

(setq cdt-el-version .2)

(defun get-jar-entry (file entry)
     (set-buffer (find-file-noselect file t))
     (goto-char (point-min))
     (re-search-forward (concat "  " entry "$"))
     (let ((buffer (save-window-excursion
                     (archive-extract)
                     (current-buffer))))
       (set-buffer buffer)
       (goto-char (point-min))
       buffer))

(defun check-for-jars (file)
  (if (string-equal (substring file 0 1) "!")
      (progn
	(string-match "!\\(.+\\)!\\(.+\\)" file)
	(get-jar-entry (match-string 1 file) (match-string 2 file)))
    (find-file-noselect file 'nowarn)))

(defun gud-find-file (file)
  (while (string-match "//+" file)
    (setq file (replace-match "/" t t file)))
  (let ((minor-mode gud-minor-mode)
	(buf (check-for-jars file)))
    (when buf
      ;; Copy `gud-minor-mode' to the found buffer to turn on the menu.
      (with-current-buffer buf
	(set (make-local-variable 'gud-minor-mode) minor-mode)
	(set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
	(make-local-variable 'gud-keep-buffer))
      buf)))

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
  (let ((filename
	 (if (not (= (length (match-string 4 gud-marker-acc)) 0))
	     (format "!%s!%s" (match-string 4 gud-marker-acc) (match-string 1 gud-marker-acc))
	     (match-string 1 gud-marker-acc))))
    (setq gud-last-frame
	  (cons filename
		(string-to-number (match-string 2 gud-marker-acc)))))
  (setq cdt-frame (match-string 3 gud-marker-acc)))

(defun display-match ()
  (message "%s" (match-string 1 gud-marker-acc)))

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
    (filter-input "CDT location is \\(.+\\):\\(.+\\):\\(.+\\):\\(.*\\)" 'set-frame)
    (filter-input "CDT Display Message: \\(.+\\)$" 'display-match) 


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
  (replace-regexp-in-string "CDT Display Message: " "" string))
