(require 'org)
(org-babel-load-file
 (expand-file-name "README.org"
                   user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-bullets elpy auctex counsel swiper ivy smartparens ace-jump-mode htmlize magit web-mode js2-mode php-mode pug-mode zoom emmet-mode markdown-mode indent-guide company yasnippet-snippets yasnippet multiple-cursors projectile exec-path-from-shell doom-modeline srcery-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:background "#1C1B19" :foreground "#FCE8C3")) (((class color) (min-colors 89)) (:background "black" :foreground "brightwhite")))))
