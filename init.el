;;; Package --- Summary
;;; Commentary:
;; This is an init I made at my lowest.  Try to pretty my life.


;; ;; This is Transparency

(set-frame-parameter nil 'alpha-background 100) ; For current frame

(add-to-list 'default-frame-alist '(alpha-background . 100)) ; For all new frames henceforth


;; more basic stuff to pretty up Emacs.
;;; Code:
(setq visible-bell nil)
(setq package-check-signature nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message "emacs from when i hit the bottom and found something\n")
(setq default-directory "~/")
(setq auto-save-default nil)
(setq make-backup-files nil)
(set-default 'truncate-lines t)
(setq initial-major-mode 'org-mode)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(scroll-bar-mode -1)
(setq show-paren-delay 0)
(show-paren-mode 1)
(global-display-line-numbers-mode 1)


(setq c-brace-offset 0)

;; Bind Shift-Meta-a and Shift-Meta-e for text marking in all buffers
(global-set-key (kbd "S-M-a") 'mark-beginning-of-sentence)  ;; Marks to the beginning of a sentence
(global-set-key (kbd "S-M-e") 'mark-end-of-sentence)        ;; Marks to the end of a sentence


(setq org-agenda-files '("~/College/"))


;; stuff to pretty up the GUI of Emacs
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(global-visual-line-mode 1)
(setq show-paren-delay 0)
(show-paren-mode 1)


;; Set up Ido mode. pretty simple. Try not to go crazy with packages like vertico.
;; (ido-mode 1)


;; Firacode
;; For the Emacs Daemon. If this is not present the Daemon sometimes loads up miniscule. Like really miniscule
(defun my-after-frame (frame)
    (if (display-graphic-p frame)
        (progn
            (add-to-list 'default-frame-alist '(font . "Firacode 11"))
            (set-face-attribute 'default nil :font "Firacode 11")
            (set-face-attribute 'default nil :height 160)
            (set-frame-font "Firacode 11" nil t))))
;; BigBlueTermPlusNerdFont-Regular
(mapc 'my-after-frame (frame-list))
(add-hook 'after-make-frame-functions 'my-after-frame)
;; BigBlueTermPlus Nerd Font
;; Inconsolatagonerdfontmono





(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)



;; Refresh package list if necessary
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; Install the Catppuccin theme
 (use-package catppuccin-theme
   :ensure t
   :config
   (load-theme 'catppuccin t))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(catppuccin))
 '(custom-safe-themes
   '("51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd" "6e13ff2c27cf87f095db987bf30beca8697814b90cd837ef4edca18bdd381901" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "c32fbcb7c68d9a3cddf5e213e58afc9c29c55ff3835d10562280e4a690292590" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" default))
 '(package-selected-packages
   '(gruber-darker-theme solarized-theme vertico which-key lsp-ui gcmh lsp-java flycheck company catppuccin-theme catppuccin)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "default" :foundry "default" :slant normal :weight normal :height 160 :width normal)))))

;; init.el

;; Function to update the mode line with file name and current line number
(defun my-update-mode-line ()
  "Update mode line to show the current file name and line number."
  (let ((file-name (if (buffer-file-name)
                       (file-name-nondirectory (buffer-file-name))
                     "%b"))
        (line-number (number-to-string (line-number-at-pos))))
    (setq mode-line-format
          (list
           " "
           file-name
           " | Line: "
           line-number))))

;; Set the mode line format initially
(my-update-mode-line)

;; Add a hook to update the mode line when the cursor moves
(add-hook 'post-command-hook 'my-update-mode-line)


(setq-default cursor-type 'box)

;; smooth scroll
(setq scroll-conservatively 101)  ;; Don't scroll until point is at the top or bottom
(setq scroll-margin 1)            ;; Keep some margin at the top and bottom
(setq smooth-scroll-margin 1)     ;; Margin for smooth scrolling
(setq pixel-scroll-precision-mode t) ;; Enable pixel-level scrolling (if supported)


(setq make-backup-files nil) ; stop creating ~ files


;; Function to open dired in the current buffer's directory
(defun dired-open-current-directory ()
  "Open Dired in the directory of the current buffer."
  (interactive)
  (dired (file-name-directory (or buffer-file-name default-directory))))

;; Bind C-x d to the new function
(global-set-key (kbd "C-x d") 'dired-open-current-directory)

(setq initial-major-mode 'org-mode)   ;; Set the major mode to Org mode


;; init.el

;; Enable electric pair mode globally
(electric-pair-mode 1)

;; Optional: Customize to include more pairs if desired
(setq electric-pair-pairs '(
                             (?{ . ?})   ;; Curly braces
                             (?[ . ?])   ;; Square brackets
                             (?\" . ?\") ;; Double quotes
                             (?\' . ?\') ;; Single quotes
                             (?` . ?`)   ;; ticks
                             ))



;; Function to open Alacritty in the current buffer's directory
(defun open-alacritty-in-current-directory ()
  "Open Alacritty in the directory of the current buffer."
  (interactive)
  (let ((dir (or (file-name-directory buffer-file-name)
                 default-directory)))
    (start-process "alacritty" nil "alacritty" "--working-directory" dir)))

;; Bind C-x t to the new function
(global-set-key (kbd "C-x t") 'open-alacritty-in-current-directory)




;; Company is all I really Use. It's pretty nice.
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package lsp)
(use-package lsp-ui)
(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
(setq flycheck-check-syntax-automatically '(save))
(lsp-completion-mode 1)

;; Suggestions from official docs for performance
;; This slows down emacs I think, I wouldn't exactly recommend it. Going to try out lsp +
;; company without any set threshold to see how it runs.
;; (setq gc-cons-threshold 100000000)


;; just some basic lsp setup.
(setq lsp-completion-provider :capf)
(setq lsp-idle-delay .5)
(setq lsp-log-io nil)


;; This stuff also puts less load on the LSP which in turns makes it faster.
;; Annoying stuff
(setq lsp-enable-links nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-doc-enable nil)
(setq lsp-completion-enable-additional-text-edit nil)
(setq lsp-headerline-breadcrumb-mode -1)



;; an attempt to reconcile thingy's
(use-package gcmh)
(gcmh-mode 1)
(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864) ; 64M or whatever value you like
  (garbage-collect))
(run-with-idle-timer 4 nil #'my-cleanup-gc)

(use-package vertico)
(vertico-mode 1)

(use-package solarized-theme)


(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "chromium %o/*.pdf")) ; Adjust this path if necessary


(org-babel-do-load-languages
 'org-babel-load-languages '((java . t)))



(defun load-file-into-scratch (file-path)
  "Load the contents of FILE-PATH into the *scratch* buffer."
  (with-current-buffer "*scratch*"
    (erase-buffer)  ;; Clear the scratch buffer
    (insert-file-contents file-path)))  ;; Insert the file's contents

;; (load-file-into-scratch "~/Notes/TODO.org")
;; incase you want to paste some stuff into your scratch buffer


(provide 'init)
;;; init.el ends here
