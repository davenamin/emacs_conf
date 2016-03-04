(prelude-require-packages '(evil-leader ess polymode))

(global-evil-leader-mode)

(require 'ess-site)
(require 'evil-leader)
(evil-leader/set-leader "SPC")

;; stolen from spacemacs ESS layer
    (evil-leader/set-key-for-mode 'ess-julia-mode
      "si" 'julia)
    (evil-leader/set-key-for-mode 'ess-mode
      "si" 'R
      ;; noweb
      "cC" 'ess-eval-chunk-and-go
      "cc" 'ess-eval-chunk
      "cd" 'ess-eval-chunk-and-step
      "cm" 'ess-noweb-mark-chunk
      "cN" 'ess-noweb-previous-chunk
      "cn" 'ess-noweb-next-chunk
      ;; REPL
      "sB" 'ess-eval-buffer-and-go
      "sb" 'ess-eval-buffer
      "sD" 'ess-eval-function-or-paragraph-and-step
      "sd" 'ess-eval-region-or-line-and-step
      "sL" 'ess-eval-line-and-go
      "sl" 'ess-eval-line
      "sR" 'ess-eval-region-and-go
      "sr" 'ess-eval-region
      "sT" 'ess-eval-function-and-go
      "st" 'ess-eval-function
      ;; R helpers
      "hd" 'ess-R-dv-pprint
      "hi" 'ess-R-object-popup
      "ht" 'ess-R-dv-ctable
      )
    (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
    (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
    (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input)


;; for mac..
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

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
