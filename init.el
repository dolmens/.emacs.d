;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Package setup
(require 'package)

;; Package Manager Settings
;;
;; add MELPA to the package archive
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

 
(package-initialize)

;; update the package metadata when missing
(unless package-archive-contents
  (package-refresh-contents))

;; install use-package - https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(use-package auto-package-update :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; submodules ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let ((default-directory  "~/.emacs.d/site-lisp/"))
	(normal-top-level-add-subdirs-to-load-path))

(require 'init-basic)

(require 'init-ui)

;; essential packages
(require 'init-common-packages)

;; function & commands
(require 'init-common-functions)

;; common editor settings
(require 'init-common)

;; the best editor for ever
;; (require 'init-evil)

;; ivy and other package from abo
(require 'init-ivy)

(require 'init-magit)

(require 'init-lsp)

(require 'init-c++)

(require 'init-python)

;;; specific custom-file to keep init.el clean
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; init.el ends here
