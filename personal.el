(require 'ess-site)
;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; R modes
;;; (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

(load-theme 'birds-of-paradise-plus t)

;; http://ess.r-project.org/Manual/ess.html#Command-History
(eval-after-load "comint"
 '(progn
     (define-key comint-mode-map [up]
       'comint-previous-matching-input-from-input)
     (define-key comint-mode-map [down]
       'comint-next-matching-input-from-input)
;; also recommended for ESS use --
     (setq comint-scroll-to-bottom-on-output 'others)
       (setq comint-scroll-show-maximum-output t)
;; somewhat extreme, almost disabling writing in *R*, 
;;    *shell* buffers above prompt:
     (setq comint-scroll-to-bottom-on-input 'this)
))

;;https://joostkremers.github.io/pandoc-mode/
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)


;; i don't want the frame title to say PRELUDE...
(setq frame-title-format '("" invocation-name (:eval (if (buffer-file-name)
						       (abbreviate-file-name (buffer-file-name)) "%b"))))

;; http://delhey.info/inc/ess-rmarkdown.pdf
(defun ess-rmarkdown ()
  "Compile R markdown (.Rmd). Should work for any output type."
  (interactive)
  ; Check if attached R-session
  (condition-case nil
		  (ess-get-process)
		  (error
		    (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
	     (sbuffer (process-buffer sprocess))
	     (buf-coding (symbol-name buffer-file-coding-system))
	     (R-cmd
	       (format "library(rmarkdown); rmarkdown::render(\"%s\",output_format=\"pdf_document\")"
		       buffer-file-name)))
	(message "Running rmarkdown on %s" buffer-file-name)
	(ess-execute R-cmd 'buffer nil nil)
	(switch-to-buffer rmd-buf)
	(ess-show-buffer (buffer-name sbuffer) nil)))))

(server-start)
