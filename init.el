;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Defer garbage collection further back in the startup process
;; We will use gcmh to reset it later
(setq gc-cons-threshold most-positive-fixnum)

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))

;; dont pollute the init.el
(setq custom-file (concat user-emacs-directory "custom.el"))

(setq frame-inhibit-implied-resize t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq
 inhibit-splash-screen t
 inhibit-startup-message t
 initial-scratch-message nil)

(setq ring-bell-function 'ignore)

(setq use-short-answers t)

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t
      ;; Keep it out of `doom-emacs-dir' or the local directory.
      auto-save-list-file-prefix (concat user-emacs-directory "autosave/")
      tramp-auto-save-directory  (concat user-emacs-directory "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

(cond
 (IS-MAC
  ;; mac-* variables are used by the special emacs-mac build of Emacs by
  ;; Yamamoto Mitsuharu, while other builds use ns-*.
  (setq mac-command-modifier      'super
        mac-option-modifier       'meta
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none))
 (IS-WINDOWS
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))

(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(setq load-prefer-newer t)

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t
      use-package-always-defer t)

(use-package diminish)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :init
  (load-theme 'doom-one t))

(use-package minions
  :hook
  (after-init .minions-mode))

(column-number-mode 1)

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ([remap isearch-forward] . consult-line))

  :hook
  (completion-list-mode . consult-preview-at-point-mode)

  :init
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  :config
  (setq consult-narrow-key "<"))

(use-package embark)

(use-package embark-consult)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(recentf-mode)

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command]  . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         :map emacs-lisp-mode-map
         ("C-c C-d"                 . helpful-at-point)
         :map lisp-interaction-mode-map
         ("C-c C-d"                 . helpful-at-point)
         :map helpful-mode-map
         ("r"                       . remove-hook-at-point)))

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer))

(use-package company
  :diminish
  :hook
  (after-init . global-company-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package projectile
  :init
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map))

(use-package rg)

(use-package mwim)

(use-package rust-mode)

(use-package go-mode)

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

(setq read-process-output-max (* 1024 1024))

(use-package eglot
  :hook
  ((eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
   (c++-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (go-mode . eglot-ensure)))

(use-package magit)

(use-package undo-fu
  :bind
  (([remap undo] . undo-fu-only-undo)
   ([remap redo] . undo-fu-only-redo)))

(use-package vundo
  :bind
  ("s-u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(when (display-graphic-p)
  ;; Set default font
  (set-face-attribute 'default nil
		      :family "Cascadia Code"
                      :weight 'regular
		      :height 140)
  ;; Set mode-line font
  (set-face-attribute 'mode-line-active nil :family "Menlo" :height 140)
  (set-face-attribute 'mode-line-inactive nil :family "Menlo" :height 140)
  ;; Specify font for all unicode characters
  (set-fontset-font t 'symbol (font-spec :family "Symbol") nil 'prepend)
  ;; Emoji
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)
  ;; Specify font for Chinese characters
  (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family "PingFang SC")))

(use-package which-key
  :diminish
  :hook
  (after-init . which-key-mode))
  
(use-package gcmh
  :diminish
  :hook
  (emacs-startup . gcmh-mode))

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-fu)
  :hook
  (after-init . evil-mode)
  :custom
  (evil-symbol-word-search t))

(use-package evil-collection
  :after evil
  :hook
  (evil-mode . evil-collection-init))

(use-package general
  :config
  (general-evil-setup t)

  (general-def
    "s-`" 'other-frame
    ;; fix OS window/frame navigation/manipulation keys
    "s-w" 'delete-window
    "s-W" 'delete-frame
    "s-n" '+default/new-buffer
    "s-N" 'make-frame
    "C-s-f" 'toggle-frame-fullscreen
    ;; Restore somewhat common navigation
    "s-l" 'goto-line
    ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
    ;; it imposes some other functionality and overhead we don't need)
    "s-f" 'consult-line
    "s-z" 'undo
    "s-Z" 'redo
    "s-c" 'evil-yank
    "s-v" 'yank
    "s-s" 'save-buffer
    "s-x" 'execute-extended-command
    "s-a" 'mark-whole-buffer
    "s-/" 'xah-comment-dwim)

  (general-def
    :states '(visual)
    "s-x" 'kill-region)

  (general-def
    :states '(normal visual insert)
    "C-a" 'move-beginning-of-line
    "C-e" 'move-end-of-line)

  (general-def
    :states '(normal visual insert)
    :keymaps 'prog-mode-map
    "C-a" 'mwim-beginning-of-code-or-line
    "C-e" 'mwim-end-of-code-or-line)

  (general-def
    :states '(normal visual insert)
    "M-." 'xref-find-definitions)

  (general-def
    :states '(normal visual emacs)
    :prefix "SPC"
    "SPC" 'projectile-find-file
    "j" 'projectile-compile-project
    "x" 'execute-extended-command
    "." 'find-file
    ";" 'execute-extended-command
    ":" 'pp-eval-expression
    "/" 'consult-ripgrep

    "c" '(:ignore t :which-key "code")
    "cc" 'compile
    "cr" 'eglot-rename
    "cx" 'consult-flymake

    "f" '(:ignore t :which-key "file")
    "ff" 'find-file
    "fr" 'consult-recent-file

    "g" '(:ignore t :which-key "git")
    "gg" 'magit-status
    "gb" 'magit-branch-checkout

    "h" '(:ignore t :which-key "helps")
    "hf" 'describe-function
    "hk" 'describe-key
    "hv" 'describe-variable

    "p" '(:ignore t :which-key "projects")
    "pc" 'projectile-compile-project
    "pf" 'projectile-find-file
    "po" 'projectile-find-other-file

    "s" '(:ignore t :which-key "search")
    "sb" 'consult-line
    "sp" 'consult-ripgrep))

;;; init.el ends here
