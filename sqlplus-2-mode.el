(defvar sqlplus-2-hidden-sqlplus-interaction-buffer-name "*sqlplus-2-hidden-interaction*")
(defvar sqlplus-2-hidden-sqlplus-interaction-buffer nil)
(defvar sqlplus-2-output-buffer-name "*sqlplus-2-output*")
;(defvar sqlplus-2-output-buffer nil)

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
  (let* ((ltrimmed (mapcar 'sqlplus-2-chomp l))
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
  (sqlplus-2-transpose (mapcar 'sqlplus-2-trimmax (sqlplus-2-transpose (sqlplus-2-parse-select-output- txt) ))))

(defun sqlplus-2-is-sql-prompt-in-current-buffer ()
  (equal "SQL> " (buffer-substring-no-properties (- (point-max) 5) (point-max))))

(defun sqlplus-2-ensure-sql-prompt (buf)
  (with-buffer buf
    (if (sqlplus-2-is-sql-prompt-in-current-buffer) buf
      (error "Please start sqlplus in this buffer"))))

(defun sqlplus-2-get-or-create-output-buffer ()
    (get-buffer-create sqlplus-2-output-buffer-name))

(defun sqlplus-2-get-or-create-interaction-buffer ()
  (if sqlplus-2-hidden-sqlplus-interaction-buffer sqlplus-2-hidden-sqlplus-interaction-buffer
    (shell (get-buffer-create sqlplus-2-hidden-sqlplus-interaction-buffer-name))))


(defun sqlplus-2-command-to-sqlplus (buf cmd)
  (let* ((process (get-buffer-process buf)))
    (setq comint-process-echoes nil)
    (process-send-string process (concat cmd "\n"))
    (sit-for 1); fuer das echo
    (while 
	(let ((o (accept-process-output process 10)))
	  (sleep 1)
	  (and
	   (or
	    (not o)
	    (not (with-buffer buf (sqlplus-2-is-sql-prompt-in-current-buffer))))
	   (y-or-n-p "Continue waiting?"))))))
  
(defun sqlplus-2-wait-for-prompt (times)
  (progn
    (when (or (= 0 times) nil) (progn (setq sqlplus-2-interrupted nil) (error "Interrupt or timeout")))
    (if (sqlplus-2-is-sql-prompt-in-current-buffer) t
      (progn
	(sleep-for 1)
	(message (concat "Waiting..." (number-to-string times)))
	(sqlplus-2-wait-for-prompt (- times 1))))))

(defun sqlplus-2-wrap-select-limit-lines (s)
  (if (equal "select" (substring s 0 6)) (concat "select * from (" (replace-regexp-in-string "[; ]+$" "" s) ") where rownum < 20;")
    s))

(defun sqlplus-2-send-select (sql)
  (interactive)
  (let* ((interaction-buffer (sqlplus-2-ensure-sql-prompt (sqlplus-2-get-or-create-interaction-buffer)))
	(output-buffer (sqlplus-2-get-or-create-output-buffer)))
    (with-buffer interaction-buffer
      (progn
;	(goto-char 1)
	(erase-buffer)
	(insert "set feed on lin 32767 tab off emb on pages 0 newp 0 head on sqlp 'SQL> '\n")
	(comint-send-input)
	(sqlplus-2-wait-for-prompt 12)
	(erase-buffer)
	(insert (sqlplus-2-wrap-select-limit-lines sql))
;	(insert "\n")
	(setq b (point))
	(comint-send-input)
;	(sleep 3)
	(sqlplus-2-wait-for-prompt 12)
	(goto-char (point-max))

;; 	(while (progn
;; 		 (if sqlplus-2-interrupted (error "Interrupt")
;; 		   (progn
;; 		     (goto-char b)
;; 		     (sit-for 1)
;; 		     (not (sqlplus-2-is-sql-prompt-in-current-buffer))))))
	  
	(goto-char 1)
	(if (re-search-forward "\\([0-9]+\\) row[s]* selected." nil t)
	    (progn
	      (setq lines (string-to-number (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
	      (goto-line (- (count-lines 1 (point)) 1))
	      (setq e (point))
	      (goto-line (- (count-lines 1 (point)) lines 1))
	      (setq a (point))
	      (setq x (buffer-substring-no-properties a e))
	      (with-buffer output-buffer
		(progn
		  (erase-buffer)
		  (mapcar
		   (lambda (q)
		     (progn
		       (insert (mapconcat 'identity q " "))
		       (insert "\n")))
		   (sqlplus-2-normalize-select-output x))
		  (sqlplus-2-highlight-first-line))))
	  (progn
	    (setq x (buffer-substring-no-properties 1 (point-max)))
	    (with-buffer output-buffer
	      (progn
		(erase-buffer)
		(insert x)))))))))



(defun sqlplus-2-remove-linebreaks (txt)
  (replace-regexp-in-string "\n" " " txt))

(defun sqlplus-2-highlight-first-line ()
  (save-excursion
   (progn
     (goto-char 1)
     (end-of-line)
     (put-text-property 1 (point)  'face '(:foreground "yellow")))))
    
(defun sqlplus-2-process ()
  "nimmt das sql-Kommando entgegen. Wenn es ein select ist, dann dann an sql-send-select uebergeben."
  (interactive)
  (let ((x (sqlplus-2-mark-current))
	(cb (current-buffer)))
    (sqlplus-2-send-select (sqlplus-2-remove-linebreaks (buffer-substring-no-properties (car x) (cdr x))))
    (switch-to-buffer-other-window cb)
    (switch-to-buffer-other-window (sqlplus-2-get-or-create-output-buffer))
    (switch-to-buffer-other-window cb)))

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

