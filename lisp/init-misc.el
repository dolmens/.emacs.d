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
