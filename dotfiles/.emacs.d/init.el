(require 'package)
(setq package-archives '(
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(setq package-native-compile t)
(setq native-comp-deferred-compilation t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; performance
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 10000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; setq's
(setq vc-follow-symlinks t)
(setq-default truncate-lines 1)
(setq byte-compile-warnings '(cl-functions))
(setq inhibit-startup-screen t)
(setq compile-command "")
(setq compilation-ask-about-save nil)
(setq scroll-margin 8)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Set default font
(set-frame-font "UbuntuMono 23" nil t)
;; (set-frame-font "Iosevka 23" nil t)

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
;; (fringe-mode 0)

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

(use-package vterm
  :ensure)

(use-package gruber-darker-theme
  :ensure)

(use-package zenburn-theme
  :ensure)

(use-package lsp-mode
  :ensure
  :hook ((c-mode . lsp)
	  (rust-mode . lsp))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics nil))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (completion-ignore-case t) 
  (company-idle-delay 0.2))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(setq lsp-enable-links nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-eldoc-enable-hover nil)

;; languages
(use-package rust-mode
  :ensure)

;; keybindings
(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal 'global (kbd "C-p") 'projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>ph") '(lambda() (interactive)
						       (cd "~/")
						       (call-interactively 'find-file)))
(evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>pv") 'dired-jump)
(evil-define-key 'normal 'global (kbd "<leader>pp") 'projectile-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>pa") 'projectile-add-known-project)
(evil-define-key 'normal 'global (kbd "<leader>pr") 'projectile-remove-known-project)
(evil-define-key 'normal 'global (kbd "<leader>u") 'undo-tree-visualize)
(evil-define-key 'normal 'global (kbd "<leader>g") 'magit)
(evil-define-key 'normal 'global (kbd "C-o") 'previous-buffer)
(evil-define-key 'normal 'global (kbd "<leader>mm") 'compile)
(evil-define-key 'normal 'global (kbd "<leader>mr") 'recompile)
(evil-define-key 'normal 'global (kbd "S") 'async-shell-command)

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
   '("6c4c97a17fc7b6c8127df77252b2d694b74e917bab167e7d3b53c769a6abb6d6" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "70cfdd2e7beaf492d84dfd5f1955ca358afb0a279df6bd03240c2ce74a578e9e" "a37d20710ab581792b7c9f8a075fcbb775d4ffa6c8bce9137c84951b1b453016" "33ea268218b70aa106ba51a85fe976bfae9cf6931b18ceaf57159c558bbcd1e6" "3d2e532b010eeb2f5e09c79f0b3a277bfc268ca91a59cdda7ffd056b868a03bc" default))
 '(package-selected-packages
   '(zenburn-theme hc-zenburn-theme projectile flycheck magit evil-collection evil company use-package))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
