;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Increase gc size when startup, will be reset later by gcmh
(setq gc-cons-threshold most-positive-fixnum)

;; for child process pipe
(setq read-process-output-max (* 1024 1024))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq
 inhibit-splash-screen t
 inhibit-startup-message t
 initial-scratch-message nil)

(setq ring-bell-function 'ignore)

;; show column number in modeline
(column-number-mode)

;; enable y and n for emacs queries
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default abbrev-mode t)

(require 'recentf)
(setq recentf-filename-handlers
      (append '(abbreviate-file-name) recentf-filename-handlers))
(recentf-mode 1)

;; buffers reflect external file changes
(global-auto-revert-mode t)

;; better keep backup files
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/autosaves" t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      auto-save-file-name-transforms  '((".*" "~/.emacs.d/autosaves/\\1" t))
      delete-old-versions -1
      version-control t)

(setq package-archives '(("gnu"           . "https://elpa.gnu.org/packages/")
                          ("melpa"        . "https://melpa.org/packages/")
                          ("org"          . "https://orgmode.org/elpa/")))

(package-initialize)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t
      use-package-always-defer t)

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

(use-package exec-path-from-shell
  :defer nil
  :config (exec-path-from-shell-initialize))

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '("Hack" "SF Mono" "Source Code Pro" "Fira Code"
                         "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height 130))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Symbola" "Apple Symbols" "Symbol" "icons-in-terminal")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))


(use-package all-the-icons
  :if window-system)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package winum
  :hook (after-init . winum-mode))

;; ivy

(use-package ivy
  :diminish ivy-mode
  :init
  (setq
   ivy-initial-inputs-alist nil
   ;; dont show . and .. in directory list
   ivy-extra-directories nil
   ;; use switch buffer to reopen recent killed files
   ivy-use-virtual-buffers t
   ivy-virtual-abbreviate 'abbreviate
   ivy-count-format "(%d/%d) ")
  :bind (;; ("C-s" . swiper-isearch)
         ("s-f" . counsel-grep-or-swiper)
         ("C-c s" . counsel-rg)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-j" . ivy-immediate-done))
  :hook (after-init . ivy-mode))

(use-package counsel
  :diminish counsel-mode
  :after ivy
  :hook (ivy-mode . counsel-mode))

(use-package ivy-rich
  :after (ivy counsel)
  :init
  (setq ivy-rich-path-style 'abbrev)
  :hook (counsel-mode . ivy-rich-mode))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-hydra)

(use-package ivy-avy)

(use-package prescient
  :config (prescient-persist-mode))

(use-package ivy-prescient
  :after ivy
  :hook
  (ivy-mode . ivy-prescient-mode))

(use-package ivy-xref
  :after ivy
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs
        xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; evil

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-search-module 'evil-search)
  :hook
  (after-init . evil-mode))

(use-package evil-collection
  :after evil
  :init (setq evil-collection-company-use-tng nil)
  :hook
  (evil-mode . evil-collection-init))

(add-hook 'eshell-mode-hook
          (lambda ()
            (evil-collection-define-key 'insert 'eshell-mode-map
              (kbd "C-a") 'eshell-bol)))

(use-package general
  :config
  (general-def
    :states '(normal visual motion)
    "C-e" 'evil-end-of-line)
  (general-def
    :states '(normal motion insert)
    "M-." 'xref-find-definitions)
  (general-def
    :states 'insert
    "C-a" 'beginning-of-line
    "C-e" 'end-of-line
    "C-k" 'kill-line)
  (general-def
    :states '(normal insert)
    :keymaps 'prog-mode-map
    "C-a" 'mwim-beginning
    "C-e" 'mwim-end)
  (general-def
    :prefix "SPC"
    :states '(normal visual)
    :keymaps 'override
    "SPC" 'counsel-M-x
    "b" '(:ignore t :which-key "buffer")
    "c" '(:ignore t :which-key "code")
    "f" '(:ignore t :which-key "file")
    "p" '(:ignore t :which-key "project")
    "h" '(:ignore t :which-key "help")
    "l" '(:ignore t :which-key "lsp")
    "w" '(:ignore t :which-key "window"))
  (general-def
   :prefix "SPC f"
   :states '(normal visual)
   :keymaps 'override
   "f" 'counsel-find-file
   "r" 'counsel-recentf)
  (general-def
    :prefix "SPC p"
    :states '(normal visual)
    :keymaps 'override
    "f" 'counsel-projectile-find-file
    "p" 'counsel-projectile-switch-project)
   (general-def
    :prefix "SPC b"
    :states '(normal visual)
    :keymaps 'override
    "b" 'ivy-switch-buffer
    "d" 'all-the-icons-ivy-rich-kill-buffer
    "s" 'save-buffer)
   (general-def
    :prefix "SPC c"
    :states '(normal visual)
    :keymaps 'override
    "c" 'compile
    "r" 'recompile
    "x" 'flycheck-list-errors)
   (general-def
     :prefix "SPC l"
     :states '(normal visual)
     :keymaps 'override
     "=" '(:ignore t :which-key "format")
     "= =" 'lsp-format-buffer
     "= r" 'lsp-format-region
     "t" '(:ignore t :which-key "treemacs")
     "t s" 'lsp-treemacs-symbols
     "t e" 'lsp-treemacs-errors-list
     "r" 'lsp-rename)
  (general-def
    :prefix "SPC w"
    :states '(normal visual)
    :keymaps 'override
    "o" 'other-window
    "n" 'next-window
    "p" 'previous-window
    "0" 'winum-select-window-0
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9)
   (general-def
    :prefix "SPC h"
    :states '(normal visual)
    :keymaps 'override
    "f" 'counsel-describe-function
    "v" 'counsel-describe-variable
    "m" 'describe-mode
    "k" 'describe-key))

(use-package which-key
  :diminish
  :custom
  (which-key-idle-delay 0.4)
  :hook
  (after-init . which-key-mode))

(use-package shackle
  :hook (after-init . shackle-mode)
  :init
  (setq shackle-default-size 0.4
	shackle-default-alignment 'below
	shackle-rules
	'(("*Help*" :select t :size 0.4 :align 'below)
	  (compilation-mode :select t :size 0.4 :align 'below)
	  (xref-mode :select t :size 0.4 :align 'below)
          ("*LSP Lookup*" :select t :size 0.4 :align 'below)
          ("*Flycheck errors*" :select t :size 0.4 :align 'below)
	  (Buffer-menu-mode :select t :size 20 :align 'below))))


(use-package projectile
  :diminish
  :bind-keymap ("C-c p" . projectile-command-map)
		("s-p" . projectile-command-map))

(use-package counsel-projectile
  :hook
  (counsel-mode . counsel-projectile-mode)
  :init
  (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

(use-package treemacs
  :bind (([f8]  . treemacs)
	 ("M-0" . treemacs-select-window)))

(use-package treemacs-evil
  :after treemacs evil
  :demand t)

;; rime

(use-package rime
  :init
  (setq rime-librime-root "~/.emacs.d/librime/dist"
	rime-show-candidate 'posframe)
  :bind ("M-SPC" . toggle-input-method)
  :custom
  (default-input-method "rime"))


;; misc

(use-package osx-trash
  :if (eq system-type 'darwin)
  :init
  (osx-trash-setup))

(setq delete-by-moving-to-trash t)

(use-package dired
  :ensure nil
  :init
  (setq dired-recursive-deletes 'always)
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  :custom
  (dired-listing-switches "-al --group-directories-first")
  :bind (("C-x C-j" . dired-jump)
	 :map dired-mode-map
	 ("C-<backspace>" . dired-up-directory)))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package flycheck)

(use-package yasnippet
  :diminish
  :hook
  (after-init . yas-global-mode))

(use-package company
  :diminish
  :custom
  (company-async-timeout 15)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :hook (after-init . global-company-mode))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-diff t
	undo-tree-visualizer-timestamps t))


;; orgmode

(use-package org
  :pin org
  :ensure
  org-plus-contrib)

(use-package org-superstar
  :hook ((org-mode . org-superstar-mode)))

(use-package org-preview-html)

;; magit

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package evil-magit
  :after (magit evil))

;; prog

;; no hard tabs
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

(use-package ws-butler
  :diminish
  :custom (ws-butler-keep-whitespace-before-point nil)
  :hook (prog-mode . ws-butler-mode))

(use-package mwim
  :bind (:map prog-mode-map
              ("C-a" . mwim-beginning)))

(require 'cc-mode)

(define-key c-mode-base-map (kbd "<f6>") 'compile)
(define-key c-mode-base-map (kbd "<C-f6>") 'recompile)

(defun xah-comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line.

URL `http://ergoemacs.org/emacs/emacs_toggle_comment_by_line.html'
Version 2016-10-25"
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            ;;(forward-line )
            ))))))

(global-set-key (kbd "M-;") 'xah-comment-dwim)

(setq show-paren-delay 0)
(show-paren-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :diminish
  :hook (prog-mode . smartparens-mode)
  :config
  (sp-pair "(" ")" :unless '(sp-point-before-word-p))
  (sp-pair "{" "}" :unless '(sp-point-before-word-p)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(c-add-style "my-cc-style"
             '("stroustrup"
               (c-offsets-alist
                (innamespace . [0])
                (inline-open . 0)
                (inher-cont . c-lineup-multi-inher)
                (arglist-cont-nonempty . +)
                (template-args-cont . +))))
(setq c-default-style "my-cc-style")

(use-package lsp-mode
  :commands lsp
  :hook ((c++-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
	      ("C-." . company-search-candidates)
              ("C-," . lsp-signature-activate))
  :init
  (let ((clangd "/usr/local/opt/llvm/bin/clangd"))
    (when (file-exists-p clangd)
      (setq lsp-clients-clangd-executable clangd)))
  (setq lsp-clients-clangd-args
	'("--header-insertion=never")))

;; (use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-symbols)

(use-package dap-mode)

;;; specific custom-file to keep init.el clean
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(use-package gcmh
  :diminish
  :hook
  (after-init . gcmh-mode))

;;; init.el ends here
