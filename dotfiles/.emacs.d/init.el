;;; Startup
;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold to speed up garbage collection
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;;; Backups
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      vc-make-backup-files t
      version-control t
      kept-old-versions 0
      kept-new-versions 10
      delete-old-versions t
      backup-by-copying t)

(require 'package)
(setq package-archives '(
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

;;; BOOTSTRAP USE-PACKAGE
(package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq package-native-compile t)
(setq comp-async-report-warnings-errors nil)
(setq comp-deferred-compilation t)

;; Install and load `quelpa-use-package'.
(setq quelpa-update-melpa-p nil)
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)

;;; ASYNC
;; Emacs look SIGNIFICANTLY less often which is a good thing.
;; asynchronous bytecode compilation and various other actions makes
(use-package async
  :ensure t
  :defer t
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all)))

(defalias 'yes-or-no-p 'y-or-n-p)

;; setq's
(setq vc-follow-symlinks t)
;(setq-default truncate-lines 1)
(setq byte-compile-warnings '(cl-functions))
(setq inhibit-startup-screen t)
(setq compile-command "")
(setq compilation-ask-about-save nil)
(setq scroll-margin 8)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Set default font
;; (set-frame-font "UbuntuMono 23" nil t)
;; (set-face-attribute 'default nil
;;                     :family "Ubuntu Mono"
;;                     :height 230
;;                     :weight 'normal
;;                     :width 'normal)
;; (set-frame-font "Liberation Mono 20" nil t)
(set-frame-font "Iosevka 23" nil t)

(define-minor-mode minor-mode-blackout-mode
 "Hides minor modes from the mode line."
 t)

(catch 'done
 (mapc (lambda (x)
         (when (and (consp x)
                    (equal (cadr x) '("" minor-mode-alist)))
           (let ((original (copy-sequence x)))
             (setcar x 'minor-mode-blackout-mode)
             (setcdr x (list "" original)))
           (throw 'done t)))
       mode-line-modes))

;; Set emacs modes
(blink-cursor-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(column-number-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
(menu-bar--display-line-numbers-mode-relative)
(fringe-mode 0)

;; set keybindings
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-u") 'backward-kill-sentence)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x j") 'async-shell-command)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; Install and configure packages
(use-package evil
  :ensure
  :init
  (setq evil-want-keybinding nil)
  :config
  (setq cursor-type 'box)
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-motion-state-cursor 'box)
  (setq evil-replace-state-cursor 'box)
  (setq evil-operator-state-cursor 'box)
  (global-set-key (kbd "C-u") 'evil-scroll-page-up)
  (evil-mode 1))

(use-package evil-collection
             :after evil
             :ensure t
             :config
             (evil-collection-init))

(use-package disable-mouse
  :ensure
  :config
  (mapc #'disable-mouse-in-keymap
	(list evil-motion-state-map
	      evil-normal-state-map
	      evil-visual-state-map
	      evil-insert-state-map)))

(use-package magit
  :ensure
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package flycheck
  :ensure
  :init (global-flycheck-mode))

(use-package projectile
  :ensure
  :init
  (when (file-directory-p "~/documents")
    (setq projectile-project-search-path '("~/documents")))
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (projectile-mode 1))

(use-package vertico
  :ensure
  :init (vertico-mode 1))

(use-package orderless
  :ensure
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package which-key
  :ensure
  :init
  (which-key-mode 1))

(use-package diminish
  :ensure
  :init
  (diminish 'which-key-mode)
  (diminish 'flycheck-mode)
  (diminish 'projectile-mode))

(use-package undo-tree
  :ensure
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package vterm :ensure t)
(use-package gruber-darker-theme :ensure t)
(use-package zenburn-theme :ensure t)

(use-package lsp-mode
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (rust-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :init
  (add-hook 'lsp-completion-mode-hook
	    (lambda ()
	      (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless flex)))))))
  :config
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-eldoc-enable-hover nil)
(use-package haskell-mode :ensure t)
(use-package lsp-haskell :ensure t :hook (haskell-mode . lsp-deferred))
(use-package lsp-pyright :ensure t :hook (python-mode . (lambda ()
							  (require 'lsp-pyright)
							  (lsp-deferred))))
(use-package lsp-ui  :ensure t
  :init
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions t))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (completion-ignore-case t) 
  (company-idle-delay 0.2))

(use-package company-posframe
  :ensure)

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;; languages
(use-package rust-mode
  :ensure)

(use-package yaml-mode :ensure)

;; keybindings
(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal 'global (kbd "C-j") 'projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>jw") 'find-grep)
(evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>jf") '(lambda() (interactive)
						       (cd "~/")
						       (call-interactively 'find-file)))
(evil-define-key 'normal 'global (kbd "<leader>jv") 'dired-jump)
; Projectile
(evil-define-key 'normal 'global (kbd "<leader>jj") 'projectile-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>ja") '(lambda() (interactive)
						       (cd "~/")
						       (call-interactively 'projectile-add-known-project)))
(evil-define-key 'normal 'global (kbd "<leader>jr") 'projectile-remove-known-project)

(evil-define-key 'normal 'global (kbd "<leader>u") 'undo-tree-visualize)
(evil-define-key 'normal 'global (kbd "<leader>g") 'magit)
(evil-define-key 'normal 'global (kbd "C-o") 'previous-buffer)
(evil-define-key 'normal 'global (kbd "<leader>mm") 'compile)
(evil-define-key 'normal 'global (kbd "<leader>mr") 'recompile)
(evil-define-key 'normal 'global (kbd "C-f i") 'eww)

; lsp keybindings
(evil-define-key 'normal 'global (kbd "<leader>r") 'lsp-rename)
(evil-define-key 'normal 'global (kbd "<leader>lr") 'lsp-find-references)
(evil-define-key 'normal 'global (kbd "<leader>ld") 'lsp-find-defintion)
(evil-define-key 'normal 'global (kbd "<leader>lc") 'lsp-execute-code-action)
(evil-define-key 'normal 'global (kbd "<leader>lf") 'lsp-format-buffer)
(evil-define-key 'normal 'global (kbd "<leader>ll") 'flycheck-list-errors)
(evil-define-key 'normal 'global (kbd "<leader>ln") 'flycheck-next-error)
(evil-define-key 'normal 'global (kbd "<leader>lp") 'flycheck-previous-error)
(evil-define-key 'normal 'global (kbd "<leader>lh") 'lsp-ui-doc-glance)

; Company keybindigns
(with-eval-after-load 'company (define-key company-active-map (kbd "C-w") 'backward-kill-word))
(with-eval-after-load 'company (define-key company-active-map (kbd "C-e") 'company-abort))
(with-eval-after-load 'company (define-key company-active-map (kbd "<tab>") 'yas-next-field))
(with-eval-after-load 'company (global-set-key (kbd "C-<SPC>") 'company-complete))

;; hooks
(add-hook 'dired-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'dired-mode-hook 'auto-revert-mode)
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'eww-mode-hook (lambda () (display-line-numbers-mode -1)))

;; functions
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(defun my/jump-to-par (&rest args)
    (when (< (save-excursion
         (search-forward "("))
       (save-excursion
         (search-forward ")")))
    (search-forward "(")))
(advice-add 'evil-inner-paren :before #'my/jump-to-par)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "dc6d3dbbfc3dde0a6d04b86a2659e208474646a5b559f05c95f2c450742fffb7" "6c4c97a17fc7b6c8127df77252b2d694b74e917bab167e7d3b53c769a6abb6d6" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "70cfdd2e7beaf492d84dfd5f1955ca358afb0a279df6bd03240c2ce74a578e9e" "a37d20710ab581792b7c9f8a075fcbb775d4ffa6c8bce9137c84951b1b453016" "33ea268218b70aa106ba51a85fe976bfae9cf6931b18ceaf57159c558bbcd1e6" "3d2e532b010eeb2f5e09c79f0b3a277bfc268ca91a59cdda7ffd056b868a03bc" default))
 '(package-selected-packages
   '(monokai-theme company-posframe zenburn-theme projectile flycheck magit evil-collection evil company use-package))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
