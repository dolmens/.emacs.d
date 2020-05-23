(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  ;; dont show . and .. in directory list
  (setq ivy-extra-directories nil)
  ;; use switch buffer to reopen recent killed files
  ;; (setq ivy-use-virtual-buffers t)
  (ivy-mode)
  (with-eval-after-load 'ido
      (ido-mode -1)
      ;; Enable ivy again
      (ivy-mode 1))

  (bind-keys
   :map ivy-minibuffer-map
   ;; RET to continue like ido
   ("C-m" . ivy-alt-done)
   ("C-i" . ivy-immediate-done)
   )
  
  :config
  
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  (with-eval-after-load 'imenu-anywhere
     (bind-key "C-c i" #'ivy-imenu-anywhere)))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :init
  (counsel-mode)
	:bind (("M-y" . counsel-yank-pop))
  )

(use-package swiper
  :ensure t
  :bind
  ([remap isearch-forward] . swiper-isearch)
  )

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   :foreground "deep sky blue"
   :weight 'bold
   :height 3.0)
  (set-face-attribute
   'aw-mode-line-face nil
   :inherit 'mode-line-buffer-id
   :foreground "lawn green")
  (setq aw-dispatch-always t
	aw-background nil
	aw-minibuffer-flag t
	aw-dispatch-alist
	'((?x aw-delete-window "Ace - Delete Window")
	  (?c aw-swap-window "Ace - Swap Window")
	  (?n aw-flip-window)
	  (?v aw-split-window-vert "Ace - Split Vert Window")
	  (?h aw-split-window-horz "Ace - Split Horz Window")
	  (?m delete-other-windows "Ace - Maximize Window")
	  (?g delete-other-windows)
	  (?b balance-windows)
	  (?u (lambda ()
		(progn
		  (winner-undo)
		  (setq this-command 'winner-undo))))
	  (?r winner-redo)
	  (?? aw-show-dispatch-help))))
	
(use-package ivy-hydra
  :ensure t)

(use-package hydra
  :ensure t)

(provide 'init-ivy)
