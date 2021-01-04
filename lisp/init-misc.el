;;; init-misc.el --- All other misc configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs All other misc configurations.
;;

;;; Code:

;; welcome board
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package ace-window
  :custom-face
  (aw-leading-char-face ((t ( :bold t :height 2.0))))
  :bind (([remap other-window] . ace-window)
         ("C-c w" . ace-window-hydra/body))
  :hook (emacs-startup . ace-window-display-mode)
  :config
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))
      (user-error "`toggle-window-split' only supports two windows")))

  ;; Bind hydra to dispatch list
  (add-to-list 'aw-dispatch-alist '(?w ace-window-hydra/body) t)

  ;; Select widnow via `M-1'...`M-9'
  (defun aw--select-window (number)
    "Slecet the specified window."
    (when (numberp number)
      (let ((found nil))
        (dolist (win (aw-window-list))
          (when (and (window-live-p win)
                     (eq number (string-to-number (window-parameter win 'ace-window-path))))
            (setq found t)
            (aw-switch-to-window win)))
        (unless found
          (message "No specified window: %d" number)))))
  (dotimes (n 9)
    (bind-key (format "M-%d" (1+ n))
              (lambda ()
                (interactive)
                (aw--select-window (1+ n))))))

(use-package shackle
  :functions org-switch-to-buffer-other-window
  :commands shackle-display-buffer
  :hook (after-init . shackle-mode)
  :config
  (with-no-warnings
    (defvar shackle--popup-window-list nil) ; all popup windows
    (defvar-local shackle--current-popup-window nil) ; current popup window
    (put 'shackle--current-popup-window 'permanent-local t)

    (defun shackle-last-popup-buffer ()
      "View last popup buffer."
      (interactive)
      (ignore-errors
        (display-buffer shackle-last-buffer)))
    (bind-key "C-h z" #'shackle-last-popup-buffer)

    ;; Add keyword: `autoclose'
    (defun shackle-display-buffer-hack (fn buffer alist plist)
      (let ((window (funcall fn buffer alist plist)))
        (setq shackle--current-popup-window window)

        (when (plist-get plist :autoclose)
          (push (cons window buffer) shackle--popup-window-list))
        window))

    (defun shackle-close-popup-window-hack (&rest _)
      "Close current popup window via `C-g'."
      (setq shackle--popup-window-list
            (cl-loop for (window . buffer) in shackle--popup-window-list
                     if (and (window-live-p window)
                             (equal (window-buffer window) buffer))
                     collect (cons window buffer)))
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p)))
        (let (window buffer)
          (if (one-window-p)
              (progn
                (setq window (selected-window))
                (when (equal (buffer-local-value 'shackle--current-popup-window
                                                 (window-buffer window))
                             window)
                  (winner-undo)))
            (setq window (caar shackle--popup-window-list))
            (setq buffer (cdar shackle--popup-window-list))
            (when (and (window-live-p window)
                       (equal (window-buffer window) buffer))
              (delete-window window)

              (pop shackle--popup-window-list))))))

    (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
    (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack))

  ;; HACK: compatibility issue with `org-switch-to-buffer-other-window'
  (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

  ;; rules
  (setq shackle-default-size 0.4
        shackle-default-alignment 'below
        shackle-default-rule nil
        shackle-rules
        '((("*Help*" "*Apropos*") :select t :size 0.3 :align 'below :autoclose t)
          (compilation-mode :select t :size 0.3 :align 'below :autoclose t)
          (comint-mode :select t :size 0.4 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align 'below)
          (("*Warnings*" "*Messages*") :size 0.3 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          (("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3 :align 'below)
          ("^\\*vc-.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("*gud-debug*" :select t :size 0.4 :align 'below :autoclose t)
          ("\\*ivy-occur .*\\*" :regexp t :select t :size 0.3 :align 'below)
          (" *undo-tree*" :select t)
          ("*quickrun*" :select t :size 15 :align 'below)
          ("*tldr*" :size 0.4 :align 'below :autoclose t)
          ("*osx-dictionary*" :size 20 :align 'below :autoclose t)
          ("*Youdao Dictionary*" :size 15 :align 'below :autoclose t)
          ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
          ("^\\*macro expansion\\**" :regexp t :size 0.4 :align 'below)
          ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below :autoclose t)
          (" *Install vterm* " :size 0.35 :same t :align 'below)
          (("*Paradox Report*" "*package update results*") :size 0.2 :align 'below :autoclose t)
          ("*Package-Lint*" :size 0.4 :align 'below :autoclose t)
          (("*Gofmt Errors*" "*Go Test*") :select t :size 0.3 :align 'below :autoclose t)
          ("*How Do You*" :select t :size 0.5 :align 'below :autoclose t)

          (("*Org Agenda*" " *Agenda Commands*" " *Org todo*" "*Org Dashboard*" "*Org Select*") :select t :size 0.1 :align 'below :autoclose t)
          (("\\*Capture\\*" "^CAPTURE-.*\\.org*") :regexp t :select t :size 0.3 :align 'below :autoclose t)

          ("*ert*" :size 15 :align 'below :autoclose t)
          (overseer-buffer-mode :size 15 :align 'below :autoclose t)

          (" *Flycheck checkers*" :select t :size 0.3 :align 'below :autoclose t)
          ((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
           :select t :size 0.25 :align 'below :autoclose t)

          (("*lsp-help*" "*lsp session*") :size 0.3 :align 'below :autoclose t)
          ("*DAP Templates*" :select t :size 0.4 :align 'below :autoclose t)
          (dap-server-log-mode :size 15 :align 'below :autoclose t)
          ("*rustfmt*" :select t :size 0.3 :align 'below :autoclose t)
          ((rustic-compilation-mode rustic-cargo-clippy-mode rustic-cargo-outdated-mode rustic-cargo-test-mode) :select t :size 0.3 :align 'below :autoclose t)

          (profiler-report-mode :select t :size 0.5 :align 'below)
          ("*ELP Profiling Restuls*" :select t :size 0.5 :align 'below)

          ((inferior-python-mode inf-ruby-mode swift-repl-mode) :size 0.4 :align 'below)
          ("*prolog*" :size 0.4 :align 'below)

          ((grep-mode rg-mode deadgrep-mode ag-mode pt-mode) :select t :size 0.4 :align 'below)
          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (gnus-article-mode :select t :size 0.7 :align 'below :autoclose t)
          (helpful-mode :select t :size 0.3 :align 'below :autoclose t)
          ((process-menu-mode cargo-process-mode) :select t :size 0.3 :align 'below :autoclose t)
          (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
          (tabulated-list-mode :size 0.4 :align 'below))))

(use-package linum-off
	:ensure t)

(use-package undo-tree
  :ensure t
	:diminish ""
	:config
	(global-undo-tree-mode)
	(make-variable-buffer-local 'undo-tree-visualizer-diff)
	(setq-default undo-tree-visualizer-diff t)
	(custom-set-faces
	 '(undo-tree-visualizer-current-face ((t (:foreground "default"))))	 
	 '(undo-tree-visualizer-active-branch-face ((t (:foreground "default" :weight bold))))
	 '(undo-tree-visualizer-default-face ((t (:foreground "default"))))
	 ))

;; project manager
(use-package projectile
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind (("C-c f" . projectile-find-file))
  )

(use-package counsel-projectile
  :diminish counsel-projectile-mode
  :config
  (counsel-projectile-mode))

(use-package treemacs
	:ensure t
	:bind
	(:map global-map
				([f8] . treemacs)
				("M-0" . treemacs-select-window)
				("C-c 1" . treemacs-no-delete-other-windows)))

(use-package treemacs-projectile
	:after treemacs projectile
	:bind (:map global-map
							("C-c o p" . treemacs-projectile)))

(use-package treemacs-magit
  :ensure t
  :after treemacs magit)

;; Enhance fuzzy matching
(use-package flx
  :ensure t)

;; command history
(use-package amx
  :ensure t
  )

(use-package which-key
  :ensure t
  :defer nil
  :delight
  ;; :custom
  (which-key-idle-delay 0.4)
  :config
  (which-key-mode t))
  
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-global-mode)
  :config
  (sp-pair "(" ")" :unless '(sp-point-before-word-p))
  (sp-pair "{" "}" :unless '(sp-point-before-word-p))
  (setq sp-escape-quotes-after-insert nil))
  
;; multi edit
(use-package multiple-cursors
  :ensure t
  :config
  ;; C-S c, C-> or C-< not work with my keyboard
  (global-set-key (kbd "C-c m l") 'mc/edit-lines)
  (global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c m w") 'mc/mark-all-words-like-this)
  )

(use-package diminish
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(defun epe-theme-pipeline-simple ()
  "A eshell-prompt theme with full path, smiliar to oh-my-zsh theme."
  (setq eshell-prompt-regexp "^[$#] ")
  (concat
   (epe-colorize-with-face "[" 'epe-pipeline-delimiter-face)
   (let ((f (cond ((eq epe-path-style 'fish) 'epe-fish-path)
                  ((eq epe-path-style 'single) 'epe-abbrev-dir-name)
                  ((eq epe-path-style 'full) 'abbreviate-file-name))))
     (epe-colorize-with-face (funcall f (eshell/pwd)) 'epe-dir-face))
   (when (epe-git-p)
     (concat
      (epe-colorize-with-face ":" 'epe-dir-face)
      (epe-colorize-with-face
       (epe-git-branch)
       ;; (concat (epe-git-branch)
       ;; 	       (epe-git-dirty)
       ;; 	       (epe-git-untracked)
       ;; 	       (let ((unpushed (epe-git-unpushed-number)))
       ;; 		 (unless (= unpushed 0)
       ;; 		   (concat ":" (number-to-string unpushed)))))
       'epe-git-face)))
   (epe-colorize-with-face  "]\n" 'epe-pipeline-delimiter-face)
   (if (= (user-uid) 0) "#" "$")
   " "))

(use-package eshell-prompt-extras
  :ensure t
  :init
  (setq epe-path-style 'full
	eshell-highlight-prompt t
	eshell-prompt-function 'epe-theme-pipeline-simple))

(use-package wgrep :ensure t)

;; ==============================================================================
;; flycheck
;; ==============================================================================
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  )

;; ==============================================================================
;; auto snippet
;; ==============================================================================
(use-package auto-yasnippet
  :ensure t)

(use-package imenu-anywhere
  :ensure t
  )

;; ==============================================================================
;; company, complete anything
;; ==============================================================================
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-async-timeout 10
		company-show-numbers t
		company-dabbrev-downcase nil
		company-idle-delay 0.1
		)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
	(setq company-global-modes '(not eshell-mode))
  )

(use-package json-mode
  :ensure t)

(use-package midnight
  :ensure t
  :config
  (midnight-delay-set 'midnight-delay "5:00am")
  (setq clean-buffer-list-delay-general 2)
  (midnight-mode 1))

(use-package bison-mode
  :ensure t
  :custom
  (bison-rule-enumeration-column 8))

(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(use-package ws-butler
  :ensure t
  :init
  (setq ws-butler-keep-whitespace-before-point nil)
  :diminish ws-butler-mode
  :commands (ws-butler-mode)
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'org-mode-hook #'ws-butler-mode)
  :config
  (setq ws-butler-convert-leading-tabs-or-spaces t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(when (string-equal system-type "darwin")
  (when (executable-find "gls")
    ;; Use GNU ls as `gls' from `coreutils' if available.
    (setq insert-directory-program (executable-find "gls"))
    (setq dired-use-ls-dired t)
    (setq dired-listing-switches "-alh --group-directories-first")))




(provide 'init-misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-misc.el ends here
