
(defvar sqlplus-2-mode-map
  (let ((sqlplus-2-mode-map (make-keymap)))
    (define-key sqlplus-2-mode-map  [(control return)] 'sqlplus-2-process)
    sqlplus-2-mode-map)
  "Keymap for sqlplus-2 major mode")


(defvar sqlpus-2-mode-font-lock-keywords
   '(("\\(--.*\\)" 1 'font-lock-comment-face)))

(defvar sqlplus-2-interrupted nil)

(define-derived-mode sqlplus-2-mode fundamental-mode "sqlplus-2"
   "Major mode to edit sql and send it to sqlplus. Does formatting on output of select statements"
   (use-local-map sqlplus-2-mode-map)
   (set (make-local-variable 'font-lock-keywords)
        '(sqlplus-2-mode-font-lock-keywords))
   (set (make-local-variable 'comment-start) "--"))



(defun sqlplus-2-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\\n]*\\|[[:space:]\\n]*$\\)" "" s)))


(defun sqlplus-2-trim-trailing-whitespace (str)
  (string-match "^\\(.*\\S-\\)\\s-*$" str )
  (replace-match "\\1" nil nil str))

(defun sqlplus-2-fill-with-blank (s l)
  (if (< (length s) l)
      (sqlplus-2-fill-with-blank (concat s " ") l)
    s
    ))
  

(defun sqlplus-2-split-string-to-cols (s l)
  (if (cdr l)
      (cons (substring s 0 (car l)) (sqlplus-2-split-string-to-cols (substring s (+ 1 (car l))) (cdr l)))
    (list s) ))



(defun sqlplus-2-trimmax (l)
  (let* ((ltrimmed (mapcar 'chomp l))
         (max-l (apply 'max (mapcar 'length ltrimmed))))
    (mapcar (lambda (u) (sqlplus-2-fill-with-blank u max-l)) ltrimmed)))

(defun sqlplus-2-transpose (l)
  (if (not (car l)) nil
    (cons (mapcar (lambda (q) (car q)) l) (sqlplus-2-transpose (mapcar (lambda (q) (cdr q)) l)))))


(defun sqlplus-2-split-string-to-cols (s l)
  (cons (substring (sqlplus-2-fill-with-blank s (car l)) 0 (car l))
	(if (cdr l)
	    (sqlplus-2-split-string-to-cols (substring (sqlplus-2-fill-with-blank s (+ 1 (car l)))(+ 1 (car l))) (cdr l))
	  nil)))

(defun sqlplus-2-parse-select-output- (txt)
  (let* (
	 (ol (split-string txt "\n" t))
	 (colnames (car ol))
	 (dashes (car (cdr ol)))
	 (cols-lengths (mapcar 'length (split-string dashes)))
	 (data (cons colnames (cdr (cdr ol))))
	 (normalized-data (mapcar (lambda (q) (sqlplus-2-split-string-to-cols q cols-lengths)) data))
	 )
    normalized-data))

    
(defun sqlplus-2-normalize-select-output (txt)
  (sqlplus-2-transpose (mapcar 'trimmax (sqlplus-2-transpose (sqlplus-2-parse-select-output- txt) ))))



(defun sqlplus-2-send-select (sql)
  (interactive)
  (progn
    (with-buffer "sql"
      (progn
	(insert "set feed on lin 32767 tab off emb on pages 0 newp 0 head on sqlp 'SQL> '")
	(comint-send-input)
	(erase-buffer)
	(insert sql)
	(insert "\n")
	(setq b (point))
	(comint-send-input)

	(while (progn
		 (if sqlplus-2-interrupted (error "Interrupt")
		   (progn
		     (goto-char b)
		     (sit-for 1)
		     (not (equal "SQL> " (buffer-substring-no-properties (- (point-max) 5) (point-max))))))))
	  
	(goto-char 1)
	(if (re-search-forward "\\([0-9]+\\) rows selected." nil t)
	    (progn
	      (setq lines (string-to-number (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
	      (goto-line (- (count-lines 1 (point)) 1))
	      (setq e (point))
	      (goto-line (- (count-lines 1 (point)) lines 1))
	      (setq a (point))
	      (setq x (buffer-substring-no-properties a e))
	      (with-buffer "sqlplus-output"
		(progn
		  (erase-buffer)
		  (mapcar
		   (lambda (q)
		     (progn
		       (insert (mapconcat 'identity q " "))
		       (insert "\n")))
		   (sqlplus-2-normalize-select-output x)))))
	  (progn
	    (setq x (buffer-substring-no-properties (search-forward "SQL> " nil t) (point-max)))
	    (with-buffer "sqlplus-output"
	      (progn
		(erase-buffer)
		(insert x)))))))))



(defun sqlplus-2-remove-linebreaks (txt)
  (replace-regexp-in-string "\n" " " txt))


(defun sqlplus-2-process ()
  "nimmt das sql-Kommando entgegen. Wenn es ein select ist, dann dann an sql-send-select uebergeben."
  (interactive)
  (let ((x (sqlplus-2-mark-current)))
    (sqlplus-2-send-select (sqlplus-2-remove-linebreaks (buffer-substring-no-properties (car x) (cdr x))))))

(defun sqlplus-2-mark-current ()
  "Marks the current SQL for sending to the SQL*Plus process.  Marks are placed around a region defined by empty lines."
  (let (begin end empty-line-p empty-line-p next-line-included tail-p)
    (save-excursion
      (beginning-of-line)
      (setq empty-line-p (when (looking-at "^[ \t]*\\(\n\\|\\'\\)") (point)))
      (setq next-line-included (and empty-line-p (save-excursion (skip-chars-forward " \t\n") (> (current-column) 0))))
      (setq tail-p (and empty-line-p
			(or (bobp) (save-excursion (beginning-of-line 0) (looking-at "^[ \t]*\n"))))))
    (unless tail-p
      (save-excursion
	(end-of-line)
	(re-search-backward "\\`\\|\n[\r\t ]*\n[^ \t]" nil t)
	(skip-syntax-forward "-")
	(setq begin (point)))
      (save-excursion
	(beginning-of-line)
	(re-search-forward "\n[\r\t ]*\n[^ \t]\\|\\'" nil t)
	(unless (zerop (length (match-string 0)))
	  (backward-char 1))
	(skip-syntax-backward "-")
	(setq end (or (and (not next-line-included) empty-line-p) (point)))))
    (cons begin end)))

