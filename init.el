;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Increase gc size when startup, will be reset later by gcmh
(setq gc-cons-threshold most-positive-fixnum)

;; for child process pipe
(setq read-process-output-max (* 1024 1024))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-message t)

(setq ring-bell-function 'ignore)

;; show column number in modeline
(column-number-mode)

;; no hard tabs
(setq indent-tabs-mode nil)

;; enable y and n for emacs queries
(defalias 'yes-or-no-p 'y-or-n-p)

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

(use-package which-key
  :diminish
  :custom
  (which-key-idle-delay 0.4)
  :hook
  (after-init . which-key-mode))

(use-package ace-window
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))  
  :bind (([remap other-window] . ace-window)))

(use-package shackle
  :hook (after-init . shackle-mode)
  :init
  (setq shackle-default-size 0.4
	shackle-default-alignment 'below
	shackle-rules
	'(("*Help*" :select t :size 0.4 :align 'below :autoclose t)
	  (compilation-mode :select t :size 0.4 :align 'below :autoclose t)
	  (xref-mode :select t :size 0.4 :align 'below :autoclose t))))


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
	 ("M-0" . treemacs-select-window))
  :config
  (with-eval-after-load 'ace-window
    (setq aw-ignored-buffers (delete 'treemacs-mode aw-ignored-buffers))))

;; misc

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


;; prog

(require 'cc-mode)

(define-key c-mode-base-map (kbd "<f6>") 'compile)

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
	      ("M-SPC" . lsp-signature-activate))
  :init
  (setq lsp-clients-clangd-args
	'("--header-insertion=never")))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode)

;;; specific custom-file to keep init.el clean
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(use-package gcmh
  :diminish
  :hook
  (after-init . gcmh-mode))

;;; init.el ends here
