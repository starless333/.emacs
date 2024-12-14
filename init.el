;;; Package --- Summary
;;; Commentary:
;; This is an init I made at my lowest.  Try to pretty my life.


;; ;; This is Transparency 

(set-frame-parameter nil 'alpha-background 70) ; For current frame

(add-to-list 'default-frame-alist '(alpha-background . 70)) ; For all new frames henceforth


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
          (add-to-list 'default-frame-alist '(font . "Jetbrains Mono 11"))
          (set-face-attribute 'default nil :font "Jetbrains Mono 11")
            (set-face-attribute 'default nil :height 160)
            (set-frame-font "Jetbrains Mono 11" nil t))))
;; BigBlueTermPlusNerdFont-Regular
(mapc 'my-after-frame (frame-list))
(add-hook 'after-make-frame-functions 'my-after-frame)
;; BigBlueTermPlus Nerd Font
;; Inconsolatagonerdfontmono
;; MononokiNerdFontMono
;; Jetbrains Mono
;; Iosevka

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(defun my/java-compile ()
  "Compile the current Java file using javac."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (and filename (string= (file-name-extension filename) "java"))
        (let ((default-directory (file-name-directory filename)))
          (compile (concat "javac " (shell-quote-argument filename))))
      (message "Not a Java file!"))))

;; Bind the F5 key to run the compile function
(global-set-key (kbd "<f5>") 'my/java-compile)



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
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("b8e08919a9bbb24d4296fcccb9433d5c6b3249997f29619b4abead03915128f4" "7fac152a13c430ee81f0fed959305e3331a6355765b3ae825006933b9ec36861" "cfb0bb44259cf576124b900613628f4b589df1c3860185d24fffde6666bc88cf" "cdad4e5bc718111deb1f1a2a35e9781e11724144a1eb2064732c337160946760" "adb2c32015c42ac06e4cadc87796c6255d7f7d107a2a5f9650672fe90fedd244" "afde6368be6868e8e3dd53fad1ac51223d5484f9e6836496e7987802c9a9663d" "6f177b9a2579197e650918c8e53440997063b543fc854763e3597b5a4c33860d" "a60b04e5c0fef30209f9576f04651938472b57cb1dae0375d80a53a78f515f69" "d10f3a1a3bc7cef84cd6b6812b450a8b511bec4b67a62fb7d4510fc0430d1bbf" "042fe5b1f435086a27456eaec477f5154adf4b34994e433d08dac0d82877822a" "1a2a53c7a0517dafcb85e0196a5de451668adac22cd8b0f112bf605e87489eda" "6940b1c837efb74240fb1e1a86eb334448323e92ca87fc8dd30117e7397a29ef" "c9de9d09e9c1bb04ee78fbbdfbac4fd2afee3ff1fbc7243070abe46a86fd2e17" "47e6f8c23eaea064b89ed1361b5824ee4f9562a8c4a30774ee9ee69f9b9d4f69" "1d89fcf0105dd8778e007239c481643cc5a695f2a029c9f30bd62c9d5df6418d" "368ff345ec3caf52bb4f4c4891d9246824b029ab61d1e227153ef8ca0b19a7b2" "d425e2488aee5a2f71abc4a49b1854423dd62575a3529f28aed55eb32190992c" "84b3818b23951dec337a094167369a6e768c3364a94be55c6c78de65b84ef5e2" "27b3336b6115451a340275d842de6e8b1c49ce0bba45210ed640902240f8961d" "ea066684e9ace1e618719fab683b24a0fbcd3de82692190b1fe54e6b1b2a29bc" "2ed177de0dfc32a6a32d6109ddfd1782a61bcc23916b7b967fa212666d1aa95c" "5912c255e7e46432d6c1c057a2124cce807ad4b901a99bc43e838db0754dff91" "729215180f26504a9b0c238d0fdd0e2f9dcae2a86b7e53b3af3604e54ea36cbc" "28cf1f7cc54ab4ee1ba4a4644046bd661941be92ef8327af56909f340cb9d3d5" "6b234feec8db588ad5ec2a9d9d7b935f7a155104b25ccfb94d921c45a2ff7d22" "e7b34efca11a7841d4ec0a07af3772d2c59795862dd3a2e5c4ef92580e1dfc61" "b1739fbbd16aa4ce7cef132cf4d5308637970e69124f134b8ae1ffdba1282dd9" "0517759e6b71f4ad76d8d38b69c51a5c2f7196675d202e3c2507124980c3c2a3" "5aedf993c7220cbbe66a410334239521d8ba91e1815f6ebde59cecc2355d7757" "18a1d83b4e16993189749494d75e6adb0e15452c80c431aca4a867bcc8890ca9" "75b371fce3c9e6b1482ba10c883e2fb813f2cc1c88be0b8a1099773eb78a7176" "51fa6edfd6c8a4defc2681e4c438caf24908854c12ea12a1fbfd4d055a9647a3" "5a0ddbd75929d24f5ef34944d78789c6c3421aa943c15218bac791c199fc897d" "8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "e9d685df93947908816c34446af008825c0ebe2f140d6df068d2d77dcd6b1c0c" "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5" "e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02" "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "98efadbd3372189889c38e29f278b8bfe7b0cbe0a5811e60753a872f5c02e978" "262589c790e262af5fa62d59838f40d0e23bc6455e267aca1816eda86c936c8c" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "a22b002b3b0946b8ab8e156b74929ec88252b385c868e1be934631f56535ae1d" "2ad8cb4f6f8509341afb79f6f39636816e8cbf2d42681ff54cae6836b51d7cee" "dc15dbd4b0a00c64610fd4379a89424e0be1b418f09457e0f062cac931e8ca82" "acfe7ff6aacb9432f124cde4e35d6d2b4bc52916411de73a6ccded9750c9fa97" "b8bd60a23b9e2f08b0c437231ee84f2dacc70fdc4d5a0fb87229bb9926273fdd" "9f986dcc0de26c1c8b6dfd749eb7351b1a3c8db31b3330a7dfdd25be1b47cb22" "4edad12267c88bb57aab5a5c0d2e23740c6f552b6a36fb785dfb4e4725808eab" "6ef8291bcfd3d7c8f26b0921e62838514dbefa23ce5be09ab4663087f2868363" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "d73b18c3a0182550dc9a60d7b8af1dc21f82d89bb508730fe5aca83b80cb03ce" "3ec12a9bce6b2ff1d805593de06e012b1d999963ed8e75750760eb6bab7b0092" "71ef2c8ced402aa3cd319a799e748631a1c080a8aeb0852469c589b59547be76" "24b98f100f51b09564e2874ab4ec7ab459d89e111432375a4fcec1200c387b70" "7c7026a406042e060bce2b56c77d715c3a4e608c31579d336cb825b09e60e827" "ba5c5bdef8de41ffa8eca30fc56e7404d6e787d7836702f5e367794a759393c5" "e970c30a3664e485abba230c9bbc8474e018e366fe06fb37d92f01455c08be69" "98b4ef49c451350c28a8c20c35c4d2def5d0b8e5abbc962da498c423598a1cdd" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd" "6e13ff2c27cf87f095db987bf30beca8697814b90cd837ef4edca18bdd381901" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "c32fbcb7c68d9a3cddf5e213e58afc9c29c55ff3835d10562280e4a690292590" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" default))
 '(package-selected-packages
   '(pdf-tools auctex lush-theme nyx-theme standard-themes humanoid-themes ewal-spacemacs-themes kaolin-themes kanagawa-themes app-monochrome-themes gruvbox-theme ligature uwu-theme nano-theme sublime-themes darktooth-theme tao-theme minimal-theme purp-theme nordic-night-theme nordless-theme nord-theme gruber-darker-theme solarized-theme vertico which-key lsp-ui gcmh lsp-java flycheck company catppuccin-theme catppuccin)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

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

(require 'auctex)
(use-package auctex
  :ensure t
  :hook
  (LaTeX-mode . turn-on-prettier-symbols-mode)
  (LaTeX-mode . turn-on-flyspell))

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

(load-theme 'gruber-darker)
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

(use-package pdf-tools)

(org-babel-do-load-languages
 'org-babel-load-languages '((java . t)))



(defun load-file-into-scratch (file-path)
  "Load the contents of FILE-PATH into the *scratch* buffer."
  (with-current-buffer "*scratch*"
    (erase-buffer)  ;; Clear the scratch buffer
    (insert-file-contents file-path)))  ;; Insert the file's contents

;; (load-file-into-scratch "~/Notes/TODO.org")
;; incase you want to paste some stuff into your scratch buffer
(global-display-line-numbers-mode 0)


;; Enable LaTeX previews in Org mode
(setq org-latex-preview-latex-process '("latex" "%f")) ;; Latex process for generating previews
(setq org-latex-preview-scale 2.0)

;; Customize latex preview with dvipng or imagemagick for better quality
(setq org-latex-preview-latex-process '("latex" "-interaction=nonstopmode" "%f"))
(setq org-latex-preview-default-process 'dvipng)
(setq org-latex-preview-dvipng-options '("-D" "150" "-T" "tight" "-bg" "Transparent"))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))


;; Enable Org-mode LaTeX previews automatically when you open an Org file
(add-hook 'org-mode-hook (lambda ()
                           (setq org-startup-with-latex-preview t)))



(provide 'init)
;;; init.el ends here
