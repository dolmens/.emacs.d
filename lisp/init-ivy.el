(use-package counsel
  :diminish ivy-mode counsel-mode
  :init
  (setq
   ;; dont show . and .. in directory list
   ivy-extra-directories nil
   ;; use switch buffer to reopen recent killed files
   ivy-use-virtual-buffers t
   ivy-virtual-abbreviate 'abbreviate
   ivy-count-format "(%d/%d) ")
  (counsel-mode)
  :bind (("C-s" . swiper-isearch)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-j" . ivy-immediate-done))
  :config
  (ivy-mode))

(use-package ivy-rich
  :after ivy
  :init
  (setq ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode))

(provide 'init-ivy)
