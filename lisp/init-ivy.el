;;; init-ivy.el --- Ivy configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Ivy configurations.
;;

;;; Code:

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
         ("s-f" . swiper-isearch)
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
  :config (ivy-rich-mode))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
