(defconst coding-system-for-japanese
  (let ((cs (cdr (assoc 'coding-system
			(assoc  "Japanese" language-info-alist)))))
    (append
     (mapcar 
      #'(lambda (c)
	  (intern-soft (concat (symbol-name c) "-unix"))) cs)
     (mapcar 
      #'(lambda (c)
	  (intern-soft (concat (symbol-name c) "-dos"))) cs)
     (mapcar 
      #'(lambda (c)
	  (intern-soft (concat (symbol-name c) "-mac"))) cs))))

(defun japanese-p (list)
  (cond ((atom list) nil)
	((member (car list)
		 coding-system-for-japanese)
	 t)
	(t (japanese-p (cdr list)))))

(defun migemo-isearch-auto-enable ()
  (unless (local-variable-p 'migemo-isearch-enable-p)
    (set (make-local-variable 'migemo-isearch-enable-p)
	 (japanese-p (detect-coding-with-language-environment
		      (point-min) (point-max) "japanese")))))

(add-hook 'isearch-mode-hook #'migemo-isearch-auto-enable)

(provide 'migemo-isearch-auto-enable)
