
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
  (with-buffer "sql"
    (progn
      (insert "set lin 32767 tab off emb on pages 0 newp 0 head on sqlp SQL>")
      (comint-send-input)
      (erase-buffer)
      (insert sql)
      (setq b (point))
      (comint-send-input)
      (while (progn			
	       (goto-char b)
	       (sleep-for 1)
	       (not (re-search-forward "\\([0-9]+\\) rows selected." nil t))))
      (setq lines (string-to-number (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
      (goto-line (- (count-lines 1 (point)) 1))
      (setq e (point))
      (goto-line (- (count-lines 1 (point)) lines 1))
      (setq a (point))
      (setq x (buffer-substring-no-properties a e))
    )))

(buffer-substring-no-properties a e)
(- 6 2 1)

(sql-send-select "select * from tab where rownum < 9;")


(setq uuu 4)

(insert (make-string 3))

(with-buffer "uuu" (progn (erase-buffer) (insert x) (insert (number-to-string a)) (insert (number-to-string e))))

(buffer-substring-no-properties)
(goto-char 0)