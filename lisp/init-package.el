;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Package management configurations.
;;

;;; Code:

(require 'package)

;; Package Manager Settings
;;
;; add MELPA to the package archive
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; add orgmode to the package archive
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

;; update the package metadata when missing
(unless package-archive-contents
  (package-refresh-contents))

;; install use-package - https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Not enabled
(use-package auto-package-update)

;; Required by `use-package
(use-package diminish)
(use-package bind-key)

(let ((default-directory  "~/.emacs.d/site-lisp/"))
        (normal-top-level-add-subdirs-to-load-path))

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
