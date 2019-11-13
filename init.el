;; v 1.0
(require 'package)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq package-enable-at-startup nil)
(package-initialize nil)

;; Установка use-package по необходимости
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Автоустановка и автообновление пакетов
(require 'use-package)
(setq use-package-always-ensure t)

;;
;; Расширения
;;

;;(load-file "~/.emacs.d/org-to-pdf.el")

;;
;; ВНЕШНИЙ ВИД
;;

;; Тема

(use-package cyberpunk-theme
  :if (window-system)
  :ensure t
  :init
  (progn
    (load-theme 'cyberpunk t)
    ;; Убрать границу вокруг строки состояния активного окна
    (set-face-attribute `mode-line nil :box nil)
    ;; Убрать границу вокруг строки состояния неактивного окна
    (set-face-attribute `mode-line-inactive nil :box nil)
    ))

;; Шрифт
(set-frame-font "Hack 12" nil t)

;; Лучшая отрисовка буфера
(setq redisplay-dont-pause t)

;; Отключаем звуковой сигнал
(setq ring-bell-function 'ignore)

;; Отключить окно приветсвия
(setq inhibit-splash-screen  t)
;; Отключить приветсвенные сообщения
(setq ingibit-startup-message t)

;; Отключить подсказку в пустом окне
(setq initial-scratch-message "")
;; Отключить сообщение в минибуфере
(defun display-startup-echo-area-message ()
  (message ""))

;; Включить текстовый режим при старте
(setq initial-major-mode 'text-mode)

;; Отключаем подсказки
(tooltip-mode -1)
;; Отключаем меню
(menu-bar-mode -1)
;; Отключаем панель инструментов
(tool-bar-mode -1)
;; Отключаем полосу прокрутки
(scroll-bar-mode -1)
;; Отключить мигание курсора
(blink-cursor-mode -1)

;; Отключить диалоговые окна
(setq use-dialog-box nil)
;; Сокращенные ответы в минибуфере
(defalias 'yes-or-no-p 'y-or-n-p)

;; Скрыть строку состояния
;;(setq-default mode-line-format nil)

;; Сохранить историю минибуферов
(savehist-mode 1)
;; Сохранить позицию курсора
(save-place-mode 1)
;; Сохранить последнюю сессию
(desktop-save-mode 1)

;; Отключаем подсветку текущей строки
(global-hl-line-mode -1)
;; Переключение между буферами META и стрелки
(windmove-default-keybindings 'meta)

;; Прокручивать по одной строке
(setq scroll-step 1)
;; Смешать буфер весли курсор в 10ти строках от края
(setq scroll-margin 10)
;; Прокрутка без рывка
(setq scroll-conservatively 10000)

;; Отключаем автозаполнение строки по ширине
(auto-fill-mode -1)
;; Переносить по словам
(setq word-wrap t)
;;
(set-default 'truncate-lines t)
;; Не отображать символ переноса
;;(global-visual-line-mode t)

;; Удалять выделенный текст при вводе
(delete-selection-mode)
;; Подсвечивать парные скобки
(show-paren-mode)
;; Подсвечивать содержимое
;;(setq show-paren-style 'expression)

;; Отоброжать номер символа в строке
(column-number-mode)
;; Отображать размер файла
(size-indication-mode)
;; Отобажать время
(display-time-mode)
;; Формат времени
(setq-default display-time-24hr-format t)

;; Отключить курсор в неактивных окнах
(setq cursor-in-non-selected-windows nil)
;; Табуляция из четырех символов
(setq default-tab-widthq 4)

;; Тип курсора
(setq-default cursor-type 'hollow)
;; Цвет курсора
(set-cursor-color "#cd1076")
;; Цвет отступа окна
(set-face-background 'fringe "black")

;;
;; Резервное копирование
;;

(let ((backup-dir "~/.emacs.d/backups")
      (auto-saves-dir "~/.emacs.d/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq make-backup-files t               ; Создавать резервную копию при первом сохранении
      backup-by-copying t               ; Копировать, а не создавать ссылки
      version-control t                 ; Добавлять номер версии в имя файла
      delete-old-versions t             ; Удалять старые резервные копии
      kept-old-versions 5               ; Сохранять старые версии
      kept-new-versions 10              ; Сохранять новые версии
      auto-save-default t               ; Автоматическое сохранение
      auto-save-timeout 20              ; Сохранять каждые 20 секунд
      auto-save-interval 200            ; Сохранять через кажды 200 набраных символов
      )

;; Пути

(setenv "PATH"
  (concat
   "/usr/local/bin" ":"
   "/Library/TeX/texbin" ":"
   (getenv "PATH")
  )
)

;;
;; Пакеты
;;

;; eshell
(add-hook 'shell-mode-hook
	  (lambda ()
	    (define-key shell-mode-map (kbd "<M-up>") 'comint-previous-input)
	    (define-key shell-mode-map (kbd "<M-down>") 'comint-next-input)
	    )
	  )

;; org-mode
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda))
  :config
  (progn
    (setq org-directory "~/Drive/dev/doc/org")
    (setq org-agenda-files
          (mapcar (lambda (path) (concat org-directory path))
                  '("/todo.org")))
;; Подсветка блока с кодом.
    (setq org-src-fontify-natively t)
    )
  )

;; https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (:map modi-mode-map
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :bind (:map region-bindings-mode-map
         ("a" . mc/mark-all-like-this)
         ("p" . mc/mark-previous-like-this)
         ("n" . mc/mark-next-like-this)
         ("P" . mc/unmark-previous-like-this)
         ("N" . mc/unmark-next-like-this)
         ("[" . mc/cycle-backward)
         ("]" . mc/cycle-forward)
         ("m" . mc/mark-more-like-this-extended)
         ("h" . mc-hide-unmatched-lines-mode)
         ("\\" . mc/vertical-align-with-space)
         ("#" . mc/insert-numbers) ; use num prefix to set the starting number
         ("^" . mc/edit-beginnings-of-lines)
         ("$" . mc/edit-ends-of-lines))
  :init)

;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :config
  (yas-global-mode t)
  :init
  (setq yas-alias-to-yas/prefix-p nil))
(use-package yasnippet-snippets
  :ensure t)

;; https://company-mode.github.io
(use-package company
  :ensure t
  :diminish company-mode
  :defer 2
  :bind ("C-<tab>" . company-complete)
  :config
  (global-company-mode t))

;; https://github.com/zk-phi/indent-guide
(use-package
  indent-guide
  :config (indent-guide-global-mode 1))

;; markdown-mode
(use-package markdown-mode
  :mode "\\.md\\'"
  :ensure t)

;; https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :ensure    t)

;; https://github.com/hlissner/emacs-pug-mode
(use-package pug-mode
  :ensure t
  :mode
  ("\\.pug\\'" . pug-mode)
  :init
  (setq pug-tab-width 2))

;; php-mode
(use-package php-mode
  :ensure t
  :mode
  ("\\.php\\'" . php-mode))

;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :ensure    t
  :config
  (add-hook 'js-mode-hook #'js2-minor-mode))

;; web-mode

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"
         "\\.css\\'"
         "\\.php\\'")
  :config
  (progn
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)))

;; https://github.com/cyrus-and/zoom
(use-package zoom
  :config
  (setq zoom-size '(0.618 . 0.618))
  (zoom-mode t))

;; https://magit.vc
(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

;; https://www.gnu.org/software/auctex/
(use-package tex
 :ensure auctex)

;;https://github.com/hniksic/emacs-htmlize
(use-package htmlize
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired
             htmlize-region))

;; https://www.gnu.org/software/auctex/
(use-package ace-jump-mode
  :diminish ace-jump-mode
  :config (progn
            (autoload
              'ace-jump-mode
              "ace-jump-mode"
              "Emacs quick move minor mode"
              t)
            ;; you can select the key you prefer to
            ;;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode
            (define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode))
  :ensure t)

;;
;; Тест
;;

;; http://www.djcbsoftware.nl/code/mu/
;; brew install mu
;; sudo mount -uw /
;; sudo ln -s /Applications/Emacs.app/Contents/MacOS/Emacs /usr/bin/emacs
;; EMACS=$(which emacs)

;;(add-to-list 'load-path "/usr/local/bin/mu")
;;(require 'mu4e)

;; ivy

;;(ivy-mode 1)
;;(global-set-key "\C-s" 'swiper)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auctex magit zoom web-mode js2-mode php-mode pug-mode emmet-mode markdown-mode indent-guide company yasnippet multiple-cursors projectile cyberpunk-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
