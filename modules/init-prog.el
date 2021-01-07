;;; init-prog.el --- Programming configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Programming configurations.
;;

;;; Code:


;; c++

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

;; python
(use-package ein)

(use-package cmake-mode)

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
