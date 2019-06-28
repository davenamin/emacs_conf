;;; Daven Amin
;;; Commentary:
;;; a really basic .emacs that i plan on building up
;;; Code:

;; only if needed, proxy setup from
;; http://stackoverflow.com/questions/1595418/emacs-behind-http-proxy
;;(setq url-proxy-services '(("no_proxy" . "work\\.com")
;;                           ("http" . "proxy.work.com:911")
;;			   ("https" . "proxy.work.com:911")))

;; only if needed, spellchecking on windows (need Aspell installed)
;; http://alienexp.blogspot.com/2013/04/emacs-flyspellispell-aspell-and-windows.html
;;(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin")
;;(setq ispell-program-name "aspell")
;;(setq ispell-dictionary "american")


;; melpa "getting started"
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; https://www.reddit.com/r/emacs/comments/3nm0cf/whats_the_best_way_to_sync_emacs_settings_between/
;; http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

(setq package-list '(
		     use-package
		     exec-path-from-shell
                     evil
                     evil-leader
                     evil-collection
                     ess
		     counsel
		     magit
                     auctex
                     pandoc-mode
		     markdown-mode
                     polymode
		     leuven-theme
		     flycheck
		     org
		     syndicate
		     persistent-scratch
                     js2-mode
		     pdf-tools
		     lsp-mode
		     lsp-ui
                     ;; (and more packages...)
                     ))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;http://stackoverflow.com/questions/744672/unable-to-hide-welcome-screen-in-emacs
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;;http://superuser.com/questions/127420/how-can-i-hide-the-tool-bar-in-emacs-persistently
(tool-bar-mode -1)

;; http://stackoverflow.com/questions/2680389/how-to-remove-all-files-ending-with-made-by-emacs
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )


;; for mac..
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; i don't want the frame title to say PRELUDE...
(setq frame-title-format '("" invocation-name (:eval (if (buffer-file-name)
                                                         (abbreviate-file-name (buffer-file-name)) "%b"))))
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; use-package
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;;(add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  )

(use-package evil-leader
  :after (evil ess-site)
  :ensure t
  :config
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

  )

(use-package evil-collection
  :after (evil evil-leader)
  :ensure t
  :config
  (evil-collection-init)
  )

(use-package org
  :config
  ;; keep scratch buffer and set mode to org
  ;; http://emacsredux.com/blog/2014/07/25/configure-the-scratch-buffers-mode/
  ;; https://github.com/Fanael/persistent-scratch
  (setq initial-major-mode 'org-mode)
  (setq initial-scratch-message nil)
  )

(use-package persistent-scratch
  :after org
  :config
  (persistent-scratch-setup-default)
  )

(use-package syndicate)

(use-package ess-site
  :config
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

  ;; http://delhey.info/inc/ess-rmarkdown.pdf
  (defun ess-rmarkdown ()
    "Compile R markdown (.Rmd). Should work for any output type."
    (interactive)
    ;; Check if attached R-session
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

  ;; https://www.reddit.com/r/Julia/comments/460fxo/help_julia_hangs_with_emacs_in_windows_10/
  ;; https://github.com/emacs-ess/ESS/issues/377
  ;; workaround for julia hiccups in ESS
  ;;(setq inferior-julia-args "-L C:\\Users\\Daven\\.julia\\ess_workaround.jl")
  (when (and load-file-name (eq system-type 'windows-nt))
    (setq inferior-julia-args
	  (format "-i -L%s" (expand-file-name "ess_workaround.jl" "~/.julia"))))

  ;; disable eldoc for `julia'
  (defadvice julia (around disable-eldoc activate)
    (let (ess-use-eldoc)
      ad-do-it))

  )

(use-package flycheck
  :config
  (global-flycheck-mode)
  )

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )

;; js2-mode
(use-package js2-mode
  :after flycheck
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  ;; use flycheck-eslint if in node_modules, else jshint. modified from:
  ;; https://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  ;; https://stackoverflow.com/questions/29066675/use-flycheck-with-eslint-on-emacs-when-editing-files-from-a-particular-project
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
		  (or (buffer-file-name) default-directory)
		  "node_modules"))
	   (eslint (or
		    (and root
			 (expand-file-name "node_modules/.bin/eslint.cmd"
					   root))
		    (and root
			 (expand-file-name "node_modules/eslint/bin/eslint.js"
					   root)))))
      (when (and eslint (file-executable-p eslint))
	(progn
	  (setq-local flycheck-javascript-eslint-executable eslint)
	  (setq-local flycheck-disabled-checkers '(javascript-jshint))
	  (setq-local flycheck-checkers '(javascript-eslint))
	  ))))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  )

(use-package polymode
  :config
  ;; MARKDOWN
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

  ;; R modes
  ;; (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  )

(use-package pandoc-mode
  :config
  ;;https://joostkremers.github.io/pandoc-mode/
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  )

(use-package lsp-mode
  :hook (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode
  )

(pdf-tools-install)

(load-theme 'leuven t)
(require 'server)
(unless (server-running-p) (server-start))
