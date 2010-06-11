
(defun display-sql-table (txt)
  (progn
    (erase-buffer)
    (mapcar
     (lambda (q)
       (progn
	 (insert (mapconcat 'identity q " "))
	 (insert "\n")))
     (sqlplus-normalize-select-output txt))))


(defun sql-send-select (sql)
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
		 (if sqlplus-interrupted (error "Interrupt")
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
		   (sqlplus-normalize-select-output x)))))
	  (progn
	    (setq x (buffer-substring-no-properties (search-forward "SQL> " nil t) (point-max)))
	    (with-buffer "sqlplus-output"
	      (progn
		(erase-buffer)
		(insert x)))))))))




(sql-send-select "select * from tab where rownum < 10;")

(setq sqlplus-interrupted nil)


(with-buffer "uuu" (progn (erase-buffer) (insert x)))

also:

(defun sqlplus-execute-command-at-point ()
  "nimmt das sql-Kommando entgegen. Wenn es ein select ist, dann dann an sql-send-select uebergeben."
  (interactive)
  (let ((x (sqlplus-mark-current)))
    (sql-send-select (sqlplus-remove-linebreaks (buffer-substring-no-properties (car x) (cdr x))))))

(defun sqlplus-mark-current ()
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

(defun sqlplus-remove-linebreaks (txt)
  (replace-regexp-in-string "\n" " " txt))


