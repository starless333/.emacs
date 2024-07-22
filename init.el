;; When Opening emacs it opens in Fullscreen.
(setq initial-frame-alist '((fullscreen . maximized)))
(setq default-frame-alist '((fullscreen . maximized)))



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
(setq-default cursor-type '(box . 5))

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
(set-frame-parameter nil 'alpha-background 100) ; For current frame
(add-to-list 'default-frame-alist '(alpha-background . 100)) ; For all new frames henceforth



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



(use-package zen-mode)
(use-package grammarly)
(use-package emms)

(dashboard-open)
;; mess with later
;;(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
;;(run-with-idle-timer 2 t (lambda () (garbage-collect)))


(add-hook 'emacs-startup-hook
  (lambda ()
    (load-theme 'ef-cherie)
    ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(ef-cherie))
 '(custom-safe-themes
   '("6fc9e40b4375d9d8d0d9521505849ab4d04220ed470db0b78b700230da0a86c1" "b216e9b72dc8c2b702e4fcfd3c0af2d73c87eba46fd4db824ddb50863447d6a9" "ca47f7b222eb380e3035fb594d52032acd89dae0a49eac3da722a5cd3f485e3b" "0664443859604a53d2257701f034459edf8eab3cc2be50c7d8ae36740fe35578" "049749b8d7585b250e1df9e96a008d1ecd5dc3de6a3d44d153ec8452a81bd0e5" "f12083eec1537fc3bf074366999f0ee04ab23ab3eaba57614785d88b9db2a5d4" "9f27d5afd6d78b40bf1290c10722818e0b90f141fc3758d3c2d284ccb565de15" "355e3439089e3b37bb143afc0a60ce091533fe467db2ab0f2ae34d13be7a47c5" "ffdf8617d6e0f1264e5879d3ac919d0f1d8c91d38f2c769e4fa633ddbab248bf" "515ebca406da3e759f073bf2e4c8a88f8e8979ad0fdaba65ebde2edafc3f928c" "c42587b19ee1c9aa1a9dd1d8ace37ece24ca2a322243035cd6ba07f44fb466db" "6b839977baf10a65d9d7aed6076712aa2c97145f45abfa3bad1de9d85bc62a0e" "ed1b7b4db911724b2767d4b6ad240f5f238a6c07e98fff8823debcfb2f7d820a" "317754d03bb6d85b5a598480e1bbee211335bbf496d441af4992bbf1e777579e" "546862540e7b7d758a64b328bf3ceec7ae98dd87d80551496b45485ec26e05e5" "28d91e89883df5dd87c7da27f6a15e8e41bb92a0c1341eaa9f397ed67b10b21d" "159a29ab0ec5ba4e2811eddd9756aa779b23467723dcbdd223929fbf2dde8954" "9d01a8af1bdd5c79b136dc5eb23b90d53675c3f4cb938dc15c4d8bc98d2bb86e" "063095cf0fe6ed3990546ec77e5d3798a1e2ad5043350063467a71c69518bb24" "841b6a0350ae5029d6410d27cc036b9f35d3bf657de1c08af0b7cbe3974d19ac" "84b3c4fa1bbccd01a173839b7eebc226105fafd6b108f8400995eb79c67c9adf" "702d0136433ca65a7aaf7cc8366bd75e983fe02f6e572233230a528f25516f7e" "4343cbc036f09361b2912119c63573433df725f599bfbdc16fb97f1e4847a08b" "263e3a9286c7ab0c4f57f5d537033c8a5943e69d142e747723181ab9b12a5855" "aa688776604bbddbaba9e0c0d77e8eb5f88d94308f223d1962b6e6b902add6a0" "694dbeb8f98dddfb603a2fe0c04101f3fe457ee49bf90a6a581271e7f9c580c8" "d4b608d76e3a087b124c74c2b642c933d8121b24e53d4bbd5e7327c36cc69ccc" "7776ba149258df15039b1f0aba4b180d95069b2589bc7d6570a833f05fdf7b6d" "0c5d7ffa7cdf8b889342d2cd38f6ddd2cc716561048feca89b17fda56677d6b8" "76ddb2e196c6ba8f380c23d169cf2c8f561fd2013ad54b987c516d3cabc00216" "b11edd2e0f97a0a7d5e66a9b82091b44431401ac394478beb44389cf54e6db28" "6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875" "04aa1c3ccaee1cc2b93b246c6fbcd597f7e6832a97aaeac7e5891e6863236f9f" "d78df7afed952636ca40cfcaeab34353f872f74587c258618ec32ad64d545229" "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" default))
 '(package-selected-packages
   '(fireplace ef-themes evil color-theme-sanityinc-tomorrow pyenv-mode pyvenv leetcode quelpa emms-soundcloud emms-player-simple-mpv flycheck lsp-java which-key lsp-ui lsp multiple-cursors zen-mode vertico tree-sitter-langs rainbow-delimiters projectile pdf-tools orderless java-snippets gruber-darker-theme grammarly golden-ratio emms dashboard consult company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package multiple-cursors)
(global-set-key (kbd "C-r") 'mc/edit-lines)

 
(use-package lsp-ui)
(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
(setq flycheck-check-syntax-automatically '(save))
(lsp-completion-mode 1)


(emms-all)
(setq emms-player-list '(emms-player-vlc)
      emms-info-functions '(emms-info-native
                            emms-info-metaflac
                            emms-info-ogginfo))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))


(load-file "~/.emacs.d/empv.el")
(yas-global-mode 1)
(rainbow-delimiters-mode 1)

(use-package fireplace)

(require 'pyvenv)
(pyvenv-activate "~/.emacs.d/venv")

(use-package leetcode)
(setq leetcode-prefer-language "java")


(setq-default c-basic-offset 4)
(setq-default c++-basic-offset 4)

(use-package evil)
(evil-mode -1)

;; Set the default tab width to 4 spaces
(setq-default tab-width 4)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Ensure that the above settings apply to all programming modes
(defun my-setup-indent (n)
  ;; java/c/c++
  (setq-local c-basic-offset n)
  ;; web development
  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's js-switch-indent-offset
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
  )

(setq-default indent-line-function 'insert-tab)

;; Apply the settings to common programming modes
(add-hook 'prog-mode-hook (lambda () (my-setup-indent 4)))

;; Suggestions from official docs for performance
(setq gc-cons-threshold 100000000)
(setq lsp-completion-provider :capf)
(setq lsp-idle-delay 1.750)
(setq lsp-log-io nil)

;; Annoying stuff
(setq lsp-enable-links nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-doc-enable nil)
(setq lsp-completion-enable-additional-text-edit nil)
(setq lsp-headerline-breadcrumb-mode -1)

(defun my-dired-here ()
  "Open Dired in the current directory without asking for a path."
  (interactive)
  (dired default-directory))

(global-set-key (kbd "C-x d") 'my-dired-here)
