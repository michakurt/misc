

(setq txt "
abc123      def aaaaaaaa
----------- --- --------
oeu         oeu eeee
1111        u   e
")


(setq txt "
TNAME                          TABTYPE             CLUSTERID
------------------------------ ------- ---------------------
ADRESSE                        TABLE                       e
ADRESS_ART                     TABLE
ALWIS_LOG                      TABLE                 oeuuuuu
APP_FUNKTIONEN                 TABLE
APP_FUNKTIONEN_ROLLEN          TABLE
ARBEITSPLATZ                   TABLE
AUFTRAG                        SYNONYM
AUFTRAG_ADRESSEN               TABLE
AUFTRAG_LIEFER_INDEX           TABLE
")




(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\\n]*\\|[[:space:]\\n]*$\\)" "" s)))


(defun trim-trailing-whitespace (str)
  (string-match "^\\(.*\\S-\\)\\s-*$" str )
  (replace-match "\\1" nil nil str))

(defun fill-with-blank (s l)
  (if (< (length s) l)
      (fill-with-blank (concat s " ") l)
    s
    ))
  

(defun split-string-to-cols (s l)
  (if (cdr l)
      (cons (substring s 0 (car l)) (split-string-to-cols (substring s (+ 1 (car l))) (cdr l)))
    (list s) ))



(defun trimmax (l)
  (let* ((ltrimmed (mapcar 'chomp l))
         (max-l (apply 'max (mapcar 'length ltrimmed))))
    (mapcar (lambda (u) (fill-with-blank u max-l)) ltrimmed)))

(defun transpose (l)
  (if (not (car l)) nil
    (cons (mapcar (lambda (q) (car q)) l) (transpose (mapcar (lambda (q) (cdr q)) l)))))


(defun split-string-to-cols (s l)
  (cons (substring (fill-with-blank s (car l)) 0 (car l))
	(if (cdr l)
	    (split-string-to-cols (substring (fill-with-blank s (+ 1 (car l)))(+ 1 (car l))) (cdr l))
	  nil)))

(defun sqlplus-parse-select-output- (txt)
  (let* (
	 (ol (split-string txt "\n" t))
	 (colnames (car ol))
	 (dashes (car (cdr ol)))
	 (cols-lengths (mapcar 'length (split-string dashes)))
	 (data (cons colnames (cdr (cdr ol))))
	 (normalized-data (mapcar (lambda (q) (split-string-to-cols q cols-lengths)) data))
	 )
    normalized-data))

    
(defun sqlplus-normalize-select-output (txt)
  (transpose (mapcar 'trimmax (transpose (sqlplus-parse-select-output- txt) ))))



(with-buffer "sqlplus-interaction"
  (progn
    (erase-buffer)
    (mapcar
     (lambda (q)
       (progn
	 (insert (mapconcat 'identity q " "))
	 (insert "\n")))
     (sqlplus-normalize-select-output txt))))



;(defun trim-trailing-whitespace (str)
;  (string-match "^\\(.*\\S-\\)\\s-*$" str )
;  (replace-match "\\1" nil nil str));
;;

;(defun fill-with-blank (s l)
;  (if (< (length s) l)
;      (fill-with-blank (concat s " ") l)
;    s
;    ))
  
		 

;(defun trimmax (l)
;  (let* ((ltrimmed (mapcar 'trim-trailing-whitespace l))
;	 (max-l (apply 'max (mapcar 'length ltrimmed))))
;    (mapcar (lambda (u) (fill-with-blank u max-l)) ltrimmed)))



;(defun transpose (l)
;  (if (not (car l)) nil
 ;   (cons (mapcar (lambda (q) (car q)) l) (transpose (mapcar (lambda (q) (cdr q)) l)))))


  