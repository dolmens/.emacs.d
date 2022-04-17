;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; for child process pipe
(setq read-process-output-max (* 1024 1024))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq
 inhibit-splash-screen t
 inhibit-startup-message t
 initial-scratch-message nil)

(setq ring-bell-function 'ignore)

(unless window-system
  (xterm-mouse-mode t))

;; (unless window-system
;;   (setq frame-background-mode 'dark))

(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; show column number in modeline
(column-number-mode)

;; enable y and n for emacs queries
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default abbrev-mode t)

(require 'recentf)
(setq recentf-filename-handlers
      (append '(abbreviate-file-name) recentf-filename-handlers))
(recentf-mode 1)

(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")

;; buffers reflect external file changes
(global-auto-revert-mode t)

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

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
                                      :height 140))

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
  :if window-system
  :hook (after-init . doom-modeline-mode))

(use-package winum
  :hook (after-init . winum-mode))

(use-package wgrep-ag)

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
  :bind (("C-s" . swiper-isearch)
         ("s-f" . counsel-grep-or-swiper)
         ("C-c s" . counsel-rg)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-l" . ivy-immediate-done))
  :hook (after-init . ivy-mode))

(use-package counsel
  :diminish counsel-mode
  :after ivy
  :hook (ivy-mode . counsel-mode))

(defun +ivy-rich-describe-variable-transformer (cand)
  "Previews the value of the variable in the minibuffer"
  (let* ((sym (intern cand))
         (val (and (boundp sym) (symbol-value sym)))
         (print-level 3))
    (replace-regexp-in-string
     "[\n\t\^[\^M\^@\^G]" " "
     (cond ((booleanp val)
            (propertize (format "%s" val) 'face
                        (if (null val)
                            'font-lock-comment-face
                          'success)))
           ((symbolp val)
            (propertize (format "'%s" val)
                        'face 'highlight-quoted-symbol))
           ((keymapp val)
            (propertize "<keymap>" 'face 'font-lock-constant-face))
           ((listp val)
            (prin1-to-string val))
           ((stringp val)
            (propertize (format "%S" val) 'face 'font-lock-string-face))
           ((numberp val)
            (propertize (format "%s" val) 'face 'highlight-numbers-number))
           ((format "%s" val)))
     t)))

(use-package ivy-rich
  :after (ivy counsel)
  :init
  (setq ivy-rich-path-style 'abbrev)
  (plist-put ivy-rich-display-transformers-list
             'counsel-describe-variable
             '(:columns
               ((counsel-describe-variable-transformer (:width 40))
                (+ivy-rich-describe-variable-transformer (:width 50))
                (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))))
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
  :defines my-evil-initial-emacs-modes
  :init
  (setq evil-want-keybinding nil
        evil-search-module 'evil-search)
  (setq my-evil-initial-emacs-modes '(flycheck-error-list-mode))
  :hook
  (after-init . evil-mode)
  (evil-mode . (lambda ()
                 (dolist (mode my-evil-initial-emacs-modes)
                   (evil-set-initial-state mode 'emacs))))
  (eshell-mode . (lambda ()
                   (define-key evil-insert-state-local-map
                     (kbd "C-a") 'eshell-bol)
                   (define-key evil-insert-state-local-map
                     (kbd "C-e") 'end-of-line)
                   (define-key evil-insert-state-local-map
                     (kbd "C-k") 'kill-line))))

(use-package evil-collection
  :after evil
  :hook
  (evil-mode . evil-collection-init))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))


(defun lsp-format-region-or-buffer ()
  "Format region if region activated or format the whole buffer."
  (interactive)
  (if (region-active-p)
      (lsp-format-region (region-beginning) (region-end))
    (lsp-format-buffer)))

(use-package general
  :config
  (general-def
    :states '(normal visual motion insert)
    "C-a" 'move-beginning-of-line
    "C-e" 'move-end-of-line
    "C-p" 'previous-line
    "C-n" 'next-line
    "C-k" 'kill-line)
  (general-def
    :states '(normal motion insert)
    "M-." 'xref-find-definitions)
  (general-def
    :states '(normal insert)
    :keymaps 'prog-mode-map
    "C-a" 'mwim-beginning
    "C-e" 'mwim-end)
  (general-def
    :states 'insert
    "C-y" 'yank)
  (general-def
    :prefix "SPC"
    :states '(normal visual)
    :keymaps 'override
    "SPC" 'counsel-M-x
    "f" 'counsel-find-file
    "b" 'ivy-switch-buffer
    "e" 'eshell
    "t" 'lsp-treemacs-type-hierarchy
    "o" 'ff-get-other-file
    "s" 'counsel-rg
    "c" '(:ignore t :which-key "code")
    "p" '(:ignore t :which-key "project")
    "h" '(:ignore t :which-key "help")
    "l" '(:ignore t :which-key "lsp")
    "j" 'projectile-compile-project
    "w" '(:ignore t :which-key "window"))
  (general-def
    :prefix "SPC p"
    :states '(normal visual)
    :keymaps 'override
    "f" 'counsel-projectile-find-file
    "p" 'counsel-projectile-switch-project
    "s" 'counsel-rg)
   (general-def
    :prefix "SPC c"
    :states '(normal visual)
    :keymaps 'override
    "i" 'lsp-ui-imenu
    "e" 'flycheck-list-errors
    "=" 'clang-format-region-or-buffer
    )
   (general-def
     :prefix "SPC l"
     :states '(normal visual)
     :keymaps 'override
     "=" 'lsp-format-region-or-buffer
     "t" '(:ignore t :which-key "treemacs")
     "t s" 'lsp-treemacs-symbols
     "t e" 'lsp-treemacs-errors-list
     "r" 'lsp-rename)
   (general-def
     :prefix "SPC n"
     :states '(normal visual)
     :keymaps 'override
     "f" 'org-roam-node-find
     "r" 'org-roam-node-random)
   (general-def
     :prefix "SPC n"
     :states '(normal visual)
     :keymaps  'org-mode-map
     "i" 'org-roam-node-insert
     "o" 'org-id-get-create
     "t" 'org-roam-tag-add
     "a" 'org-roam-alias-add
     "l" 'org-roam-buffer-toggle
     "b" 'org-mark-ring-goto)
  (general-def
    :prefix "SPC w"
    :states '(normal visual)
    :keymaps 'override
    "w" 'winner-undo
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

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode))

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


(when (string-equal system-type "darwin")
  (setenv "DICTIONARY" "en_US"))

(use-package flyspell
  ;; :init
  ;; (setq ispell-local-dictionary "en_US")
  :config
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US")
  :hook (text-mode . flyspell-mode)
  :bind (("M-<f7>" . flyspell-buffer)
         ("<f7>" . flyspell-word)
         ("C-;" . flyspell-auto-correct-previous-word)))

;; orgmode

(use-package org
  :pin org
  :ensure
  org-plus-contrib
  :config
 (setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300)))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  :hook ((org-mode . org-superstar-mode)))

(use-package org-preview-html)

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/roam")
  :config
  (org-roam-setup)
  :bind
  (("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n r" . org-roam-node-random)
           (:map org-mode-map
                 (("C-c n i" . org-roam-node-insert)
                  ("C-c n o" . org-id-get-create)
                  ("C-c n t" . org-roam-tag-add)
                  ("C-c n a" . org-roam-alias-add)
                  ("C-c n l" . org-roam-buffer-toggle)))))

(use-package evil-org
  :ensure t
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; magit

(use-package magit
  :bind ("C-x g" . magit-status))

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

(define-key prog-mode-map (kbd "<f6>") 'projectile-compile-project)
(define-key prog-mode-map (kbd "<C-f6>") 'recompile)
(add-hook 'compilation-mode-hook
          (lambda ()
            (define-key compilation-mode-map (kbd "<f6>") 'compile)
            (define-key compilation-mode-map (kbd "<C-f6>") 'recompile)))

(setq compilation-scroll-output t)

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

(use-package go-mode
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4)))
  :bind (:map go-mode-map)
  ("<f6>" . compile))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(defcustom filter-lsp-directories nil
  "lsp enabled directories.
'nil' means all directories enabled."
  :group 'lsp
  :type '(repeat directory))

(use-package f :defer nil)
(require 'f)

(defun filtered/lsp ()
  (when (or (not filter-lsp-directories)
            (and (buffer-file-name)
                 (cl-some (lambda (path)
                            (f-descendant-of-p (buffer-file-name) path))
                          filter-lsp-directories)))
      (lsp)))

(use-package lsp-mode
  :commands lsp
  :hook ((c++-mode . filtered/lsp)
         (c-mode . filtered/lsp)
         (go-mode . filtered/lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
	      ("C-." . company-search-candidates)
              ("C-," . lsp-signature-activate))
  :custom
  (lsp-enable-snippet t))

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-symbols)

(setq python-indent-guess-indent-offset-verbose nil)

(use-package lsp-pyright
  :init
  (setq lsp-pyright-python-executable-cmd "python3")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (filtered/lsp))))

(use-package dap-mode
  :config
  (require 'dap-lldb)
  (setq dap-lldb-debug-program '("/usr/local/opt/llvm/bin/lldb-vscode"))
  (setq dap-lldb-debugged-program-function
        (lambda () (read-file-name "select file to debug")))
  (defun dap-lldb-edit ()
    "Edit the C++ debugging configuration or create + edit if none exists yet."
    (interactive)
    (let ((filename (concat (lsp-workspace-root) "/launch.json"))
	  (default "~/.emacs.d/resources/lldb-launch.json"))
      (unless (file-exists-p filename)
	(copy-file default filename))
      (find-file-existing filename))))

(use-package clang-format+
  :init
  (add-hook 'c-mode-common-hook #'clang-format+-mode))

(defun clang-format-region-or-buffer ()
  "Format region if region activated or format the whole buffer."
  (interactive)
  (if (region-active-p)
      (clang-format-region (region-beginning) (region-end))
    (clang-format-buffer)))

(use-package cmake-mode
  :init
  (setq cmake-tab-width 4))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          '(lambda()
             (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;;; mac double finger
(defvar my-previous-buffer-last-time
      (float-time))

(winner-mode 1)

(defun my-previous-buffer ()
  (interactive)
  (let* ((current-time (float-time))
         (elapse (- current-time my-previous-buffer-last-time)))
    (setq my-previous-buffer-last-time current-time)
    (when (> elapse 0.5)
      (previous-buffer))))

(defvar my-next-buffer-last-time
      (float-time))

(defun my-next-buffer ()
  (interactive)
  (let* ((current-time (float-time))
          (elapse (- current-time my-next-buffer-last-time)))
       (setq my-next-buffer-last-time current-time)
       (when (> elapse 0.5)
         (next-buffer))))

(global-set-key [double-wheel-left] 'my-previous-buffer)
(global-set-key [double-wheel-right] 'my-next-buffer)

;;; specific custom-file to keep init.el clean
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(use-package gcmh
  :diminish
  :hook
  (after-init . gcmh-mode))

;;; init.el ends here
