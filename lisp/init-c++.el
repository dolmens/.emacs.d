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

;; (with-eval-after-load "lsp-mode"
;;   (add-hook 'c-mode-common-hook #'yas-minor-mode-on)
;;   (add-hook 'c-mode-common-hook #'my/lsp)
;;   )

(provide 'init-c++)

