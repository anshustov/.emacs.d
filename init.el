;; Автоматическая установка пакетов
(require 'cl)
(require 'package)

;; Для автоматической установки пакетов
(defvar cfg-var:packages '(
			   cyberpunk-theme
			   markdown-mode
			   multiple-cursors
			   yasnippet
			   company
			   magit
			   indent-guide
			   markdown-mode
			   web-mode
			   mmm-mode
			   web-beautify
			   zoom
			   auctex
			   latex-preview-pane
			   htmlize
			   ))

(defun cfg:install-packages ()
    (let ((pkgs (remove-if #'package-installed-p cfg-var:packages)))
        (when pkgs
            (message "%s" "Emacs refresh packages database...")
            (package-refresh-contents)
            (message "%s" " done.")
            (dolist (p cfg-var:packages)
                (package-install p)))))

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)3
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(cfg:install-packages)

;;
;; ВНЕШНИЙ ВИД
;;

;; Тема
(load-theme 'cyberpunk t)

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

;; Сохранить историю минибуферов
(savehist-mode 1)
;; Сохранить позицию курсора
(save-place-mode 1)

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
(setq default-tab-width 4)

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

;; org-mode
(setq org-src-fontify-natively t)

;; Скрыть строку состояния
(setq-default mode-line-format nil)

;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; https://github.com/joaotavora/yasnippet
(require 'yasnippet)
(yas-reload-all)

;; https://company-mode.github.io
(require 'company)

;; https://github.com/zk-phi/indent-guide
(require 'indent-guide)
(indent-guide-global-mode)

;;
;; Режимы
;;

;; https://github.com/yasuyk/web-beautify
;; npm -g install js-beautify
(require 'web-beautify)

;; https://github.com/cyrus-and/zoom
(zoom-mode t)
(global-set-key (kbd "C-x z") 'zoom)

;;eshell
(add-hook 'shell-mode-hook
	  (lambda ()
	    (define-key shell-mode-map (kbd "<M-up>") 'comint-previous-input)
	    (define-key shell-mode-map (kbd "<M-down>") 'comint-next-input)
	    )
	  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (htmlize latex-preview-pane auctex zoom yasnippet web-mode web-beautify powerline multiple-cursors mmm-mode markdown-mode magit indent-guide cyberpunk-theme company)))
 '(zoom-size (quote (0.618 . 0.618))))
