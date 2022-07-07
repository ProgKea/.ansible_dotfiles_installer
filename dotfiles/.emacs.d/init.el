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

;; keep custom variables inside its own file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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
(add-to-list 'default-frame-alist `(font . ,"Iosevka-20"))
;;(add-to-list 'default-frame-alist `(font . ,"Ubuntu Mono-20"))

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
(use-package gruber-darker-theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))
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
