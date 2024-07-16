
;; When Opening emacs it opens in Fullscreen.
(setq initial-frame-alist '((fullscreen . maximized)))
(setq default-frame-alist '((fullscreen . maximized)))

;; Smooth Scrolling 
(setq scroll-step 1
      scroll-conservatively 10000
      scroll-margin 1
      scroll-preserve-screen-position t)



;; stuff to pretty up the GUI of Emacs
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(global-visual-line-mode 1)


;; For the Emacs Daemon. If this is not present the Daemon sometimes loads up miniscule. Like really miniscule
(defun my-after-frame (frame)
    (if (display-graphic-p frame)
        (progn
            (add-to-list 'default-frame-alist '(font . "Iosevka 14"))
            (set-face-attribute 'default nil :font "Iosevka 14")
            (set-face-attribute 'default nil :height 160)
            (set-frame-font "Iosevka 14" nil t))))

(mapc 'my-after-frame (frame-list))
(add-hook 'after-make-frame-functions 'my-after-frame)


;; more basic stuff to pretty up Emacs.
(setq visible-bell nil)
(setq package-check-signature nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message "Hey:3 This is my Emacs Configuration\n\n")
(setq default-directory "~/")
(setq auto-save-default nil)
(setq make-backup-files nil)
(set-default 'truncate-lines t)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(setq initial-major-mode 'org-mode)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Allow for our own custom themes. Maybe I decide to write some themes or something!
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; Internet repositories for new packages.
(setq package-archives '(("gnu"    . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "http://melpa.org/packages/")))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; Update local list of available packages:
;; Get descriptions of all configured ELPA packages,
;; and make them available for download.
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)


(setq use-package-always-ensure t)


;; Vertico setup
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; Orderless setup
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)))


;; Awesome Consult Package!! One of my Favorites!
(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
	 ("C-x b" . consult-buffer))
  :init
  (setq consult-project-root-function #'projectile-project-root))




(setq completion-styles '(substring orderless basic))

;; Setup electric-pairs
;; Never have to type both ()
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))
(electric-pair-mode 1)


;; Company is all I really Use. It's pretty nice.
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; Setup yasnippet
(use-package yasnippet :init
  (add-hook 'java-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode)
  (add-hook 'html-mode-hook #'yas-minor-mode)
  (add-hook 'dart-mode-hook #'yas-minor-mode)
  (add-hook 'latex-mode-hook #'yas-minor-mode)
  (add-hook 'js-mode-hook #'yas-minor-mode)
  (add-hook 'css-mode-hook #'yas-minor-mode)
  (add-hook 'python-mode-hook #'yas-minor-mode))

(use-package java-snippets)

;;golden ratio package
(use-package golden-ratio
  :ensure t
  :hook(after-init . golden-ratio-mode)
  :custom
  (golden-ratio-exclude-modes '(occur-mode)))

(use-package beacon)
(beacon-mode 1)

;; Setup rainbow delimiters
(use-package rainbow-delimiters
  :hook
  (java-mode . rainbow-delimiters-mode))

;; Setup tree sitter
(use-package tree-sitter
  :init
  (require 'tree-sitter)
  (add-hook 'java-mode-hook #'tree-sitter-mode)
  (add-hook 'go-mode-hook #'tree-sitter-mode)
  (add-hook 'js-mode-hook #'tree-sitter-mode)
  (add-hook 'python-mode-hook #'tree-sitter-mode))


(use-package tree-sitter-langs
  :init
  (require 'tree-sitter-langs)
  (add-hook 'java-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'go-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'js-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode))

(use-package pdf-tools)

(pdf-tools-install)  ; Standard activation command
(pdf-loader-install) ; On demand loading, leads to faster startup time
(blink-cursor-mode 1)
(setq-default cursor-type '(hbar . 5) )

(set-cursor-color "#F0DFAF") 

(defun my-after-frame (frame)
    (if (display-graphic-p frame)
        (progn
            (add-to-list 'default-frame-alist '(font . "Iosevka 14"))
            (set-face-attribute 'default nil :font "Iosevka 14")
            (set-face-attribute 'default nil :height 160)
            (set-frame-font "Iosevka 14" nil t))))

(mapc 'my-after-frame (frame-list))
(add-hook 'after-make-frame-functions 'my-after-frame)


;; This is Transparency 
(set-frame-parameter nil 'alpha-background 75) ; For current frame
(add-to-list 'default-frame-alist '(alpha-background . 75)) ; For all new frames henceforth



(defun open-terminal-in-horizontal-split ()
  "Open a terminal in a horizontal split below the current window, in the current file's directory."
  (interactive)
  (let ((default-directory (file-name-directory (or (buffer-file-name) default-directory))))
    (split-window-right)
    (other-window 1)
    (term "/bin/bash")
    (term-send-raw-string (concat "cd " default-directory "\n"))))


(global-set-key (kbd "C-x t") 'open-terminal-in-horizontal-split)


(defun reload-pdf ()
  (interactive
  (let* ((fname buffer-file-name)
        (fname-no-ext (substring fname 0 -4))
        (pdf-file (concat fname-no-ext ".pdf"))
        (cmd (format "pdflatex %s" fname)))
    (delete-other-windows)
    (split-window-horizontally)
    (split-window-vertically)
    (shell-command cmd)
    (other-window 2)
    (find-file pdf-file)
    (balance-windows))))

(global-set-key "\C-x\p" 'reload-pdf)

;; for a dashboard if I really want it.
(use-package dashboard)
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
;; Set the title
(setq dashboard-banner-logo-title "emacs:)")
;; Set the banner
(setq dashboard-startup-banner "~/Downloads/the_gang.png")
;; Value can be:
;;  - 'official which displays the official emacs logo.
;;  - 'logo which displays an alternative emacs logo.
;;  - an integer which displays one of the text banners
;;    (see dashboard-banners-directory files).
;;  - a string that specifies a path for a custom banner
;;    currently supported types are gif/image/text/xbm.
;;  - a cons of 2 strings which specifies the path of an image to use
;;    and other path of a text file to use if image isn't supported.
;;    ("path/to/image/file/image.png" . "path/to/text/file/text.txt").
;;  - a list that can display an random banner,
;;    supported values are: string (filepath), 'official, 'logo and integers.

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)
;; vertically center content
(setq dashboard-vertically-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)



;; For pretty != and <= signs. Once this is installed you will see this different lol.
(use-package ligature)
(global-ligature-mode 1)
(use-package zen-mode)
(use-package grammarly)
(use-package emms)

(dashboard-open)
;; mess with later
;;(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
;;(run-with-idle-timer 2 t (lambda () (garbage-collect)))


(add-hook 'emacs-startup-hook
  (lambda ()
    (load-theme 'gruber-darker)
    ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" default))
 '(package-selected-packages
   '(quelpa soundklaus emms-soundcloud emms-player-simple-mpv flycheck lsp-java which-key lsp-ui lsp multiple-cursors zen-mode vertico tree-sitter-langs rainbow-delimiters projectile pdf-tools orderless ligature java-snippets gruber-darker-theme grammarly golden-ratio emms dashboard consult company beacon)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package multiple-cursors)
;;(global-set-key (kbd "C-r") 'mc/edit-lines)

(use-package lsp-ui)
(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
(lsp-completion-mode 1)

(use-package soundklaus
  :ensure t
  :commands
  (soundklaus-activities
   soundklaus-connect
   soundklaus-my-favorites
   soundklaus-my-playlists
   soundklaus-my-tracks
   soundklaus-playlists
   soundklaus-tracks))

(emms-all)
(setq emms-player-list '(emms-player-vlc)
      emms-info-functions '(emms-info-native
                            emms-info-metaflac
                            emms-info-ogginfo))
(emms-streams)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))


(load-file "~/.emacs.d/empv.el")
(yas-global-mode 1)
(rainbow-delimiters-mode 1)
