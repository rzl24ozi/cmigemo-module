(defvar migemo-directory
      (if (eq system-type 'windows-nt)
	  (concat data-directory "migemo")
	"/usr/local/share/migemo"))
(defvar migemo-dictionary
      (expand-file-name "utf-8/migemo-dict" migemo-directory))
(defvar migemo-user-dictionary nil)
(defvar migemo-regex-dictionary nil)

(require 'migemo)
(unless (fboundp 'cmigemo-open)
  (require 'cmigemo-module))

(defvar cmigemo-initialized nil)

; modified migemo-init
(defun cmigemo-init ()
  (when (and migemo-use-frequent-pattern-alist
	     migemo-frequent-pattern-alist-file
	     (null migemo-frequent-pattern-alist))
    (setq migemo-frequent-pattern-alist
	  (migemo-pattern-alist-load migemo-frequent-pattern-alist-file)))
  (when (and migemo-use-pattern-alist
	     migemo-pattern-alist-file
	     (null migemo-pattern-alist))
    (setq migemo-pattern-alist
	  (migemo-pattern-alist-load migemo-pattern-alist-file)))
  (when (and migemo-use-default-isearch-keybinding
             migemo-register-isearch-keybinding-function)
    (funcall migemo-register-isearch-keybinding-function))
  ;; begin modification for cmigemo
  (if (fboundp 'cmigemo-open)
      (when (not cmigemo-initialized)
	(setq cmigemo-initialized
	      (cmigemo-open migemo-dictionary))
        (when migemo-user-dictionary
	  (if (consp migemo-user-dictionary)
	      (dolist (d migemo-user-dictionary)
	        (cmigemo-load d))
	    (cmigemo-load migemo-user-dictionary))))))
  ;; end modification for cmigemo

; modified migemo-get-pattern
(defun cmigemo-get-pattern (word)
  (cond
   ((< (length word) migemo-isearch-min-length)
    "")
   (t
    (let (deactivate-mark pattern freq alst)
      (set-text-properties 0 (length word) nil word)
      (migemo-init)
      (when (and migemo-pre-conv-function
		 (functionp migemo-pre-conv-function))
	(setq word (funcall migemo-pre-conv-function word)))
      (setq pattern
	    (cond
	     ((setq freq (and migemo-use-frequent-pattern-alist
			      (assoc word migemo-frequent-pattern-alist)))
	      (cdr freq))
	     ((setq alst (and migemo-use-pattern-alist
			      (assoc word migemo-pattern-alist)))
	      (setq migemo-pattern-alist (cons alst (delq alst migemo-pattern-alist)))
	      (cdr alst))
	     (t
              ;; begin modification for cmigemo
	      (if (fboundp 'cmigemo-open)
		  (setq pattern (cmigemo-query word)))
              ;; end modification for cmigemo
	      (when (and (memq system-type '(windows-nt OS/2 emx))
			 (> (length pattern) 1)
			 (eq ?\r (aref pattern (1- (length pattern)))))
		(setq pattern (substring pattern 0 -1)))
	      (when migemo-use-pattern-alist
		(setq migemo-pattern-alist
		      (cons (cons word pattern) migemo-pattern-alist))
		(when (and migemo-pattern-alist-length
			   (> (length migemo-pattern-alist)
			      (* migemo-pattern-alist-length 2)))
		  (setcdr (nthcdr (1- (* migemo-pattern-alist-length 2))
				  migemo-pattern-alist) nil)))
	      pattern)))
      (if (and migemo-after-conv-function
	       (functionp migemo-after-conv-function))
	  (funcall migemo-after-conv-function word pattern)
	(migemo-replace-in-string pattern "\a" migemo-white-space-regexp))))))

; modified migemo-kill
(defun cmigemo-kill ()
  (interactive)
  ;; begin modification for cmigemo
  (if (fboundp 'cmigemo-open)
      (when cmigemo-initialized
	(cmigemo-close)
	(setq cmigemo-initialized nil))))
  ;; end modification for cmigemo

(advice-add 'migemo-init        :override #'cmigemo-init)
(advice-add 'migemo-get-pattern :override #'cmigemo-get-pattern)
(advice-add 'migemo-kill        :override #'cmigemo-kill)

(provide 'cmigemo)
