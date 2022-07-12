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
;;(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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
(setq byte-compile-warnings '(cl-functions))
(setq inhibit-startup-screen t)
(setq compile-command "")
(setq compilation-ask-about-save nil)
(setq scroll-margin 8)
(setq auto-save-default nil)
(setq-default word-wrap t)
(setq-default indent-tabs-mode nil)
(setq-default compilation-scroll-output t)

;; add-to-lists
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

;; activate some modes
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
(blink-cursor-mode 1)

(load "~/.emacs.d/elpa/simpc-mode.el" t)
;;(add-to-list 'load-path "~/.emacs.d/simpc-mode.el")

(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

(defun astyle-buffer (&optional justify)
  (interactive)
  (let ((saved-line-number (line-number-at-pos)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "astyle --style=kr"
     nil
     t)
    (goto-line saved-line-number)))

(add-hook 'simpc-mode-hook
          (lambda ()
            (interactive)
            (setq-local fill-paragraph-function 'astyle-buffer)))

;; Install and configure packages
(use-package evil
  :ensure
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-minibuffer t)
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

(use-package lsp-mode
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

(use-package zig-mode
  :ensure
  :init
  (setq zig-format-on-save nil))

;; keybindings
(global-set-key (kbd "C-x j") 'async-shell-command)

;; evil keybindings
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

;; replace
(evil-define-key 'normal 'global (kbd "<leader>s") 'replace-string)
(evil-define-key 'normal 'global (kbd "<leader>S") 'replace-regexp)
(evil-define-key 'normal 'global (kbd "<leader>r") 'query-replace)
(evil-define-key 'normal 'global (kbd "<leader>R") 'query-replace-regexp)
(evil-define-key 'normal 'global (kbd "<leader>jr") 'projectile-replace)
(evil-define-key 'normal 'global (kbd "<leader>jR") 'projectile-replace-regexp)
(evil-define-key 'visual 'global (kbd "<leader>s") 'replace-string)
(evil-define-key 'visual 'global (kbd "<leader>S") 'replace-regexp)
(evil-define-key 'visual 'global (kbd "<leader>r") 'query-replace)
(evil-define-key 'visual 'global (kbd "<leader>R") 'query-replace-regexp)

(evil-define-key 'visual 'global (kbd "<leader>a") 'align-regexp)
(evil-define-key 'visual 'global (kbd "C") 'comment-or-uncomment-region)

;; find
(evil-define-key 'normal 'global (kbd "C-j") 'projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>jw") 'find-grep)
(evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>jf") '(lambda() (interactive)
                                                       (cd "~/")
                                                       (call-interactively 'find-file)))

(evil-define-key 'normal 'global (kbd "<leader>jv") 'dired-jump)
(evil-define-key 'normal 'global (kbd "<leader>jj") 'projectile-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>u") 'undo-tree-visualize)
(evil-define-key 'normal 'global (kbd "<leader>g") 'magit)
(evil-define-key 'normal 'global (kbd "C-o") 'evil-buffer)
(evil-define-key 'normal 'global (kbd "C-f i") 'eww)

;; Compilation
(evil-define-key 'normal 'global (kbd "<leader>mm") 'compile)
(evil-define-key 'normal 'global (kbd "<leader>mr") 'recompile)
(evil-define-key 'normal 'global (kbd "<leader>mk") 'kill-compilation)

;; escape minibuffer
(evil-define-key 'normal 'global (kbd "<escape>") 'abort-minibuffers)

;; make leader key work in dired mode
(with-eval-after-load 'dired (evil-define-key 'normal dired-mode-map (kbd "<SPC>") 'evil-send-leader))

(evil-define-key 'normal 'global (kbd "<leader>ln") 'next-error)
(evil-define-key 'normal 'global (kbd "<leader>lp") 'previous-error)
(evil-define-key 'normal 'global (kbd "<leader>i") 'imenu)

;; lsp keybindings
(evil-define-key 'normal 'global (kbd "<leader>lr") 'lsp-rename)
(evil-define-key 'normal 'global (kbd "<leader>lr") 'lsp-find-references)
(evil-define-key 'normal 'global (kbd "<leader>ld") 'lsp-find-defintion)
(evil-define-key 'normal 'global (kbd "<leader>lc") 'lsp-execute-code-action)
(evil-define-key 'normal 'global (kbd "<leader>lf") 'lsp-format-buffer)
(evil-define-key 'normal 'global (kbd "<leader>ll") 'flycheck-list-errors)
(evil-define-key 'normal 'global (kbd "<leader>lh") 'lsp-ui-doc-glance)

;; Company keybindings
(with-eval-after-load 'company (define-key company-active-map (kbd "C-w") 'backward-kill-word))
(with-eval-after-load 'company (define-key company-active-map (kbd "C-e") 'company-abort))
(with-eval-after-load 'company (define-key company-active-map (kbd "<tab>") 'yas-next-field))
(with-eval-after-load 'company (global-set-key (kbd "C-<SPC>") 'company-complete))

;; Vertico keybindings
(with-eval-after-load 'vertico (evil-define-key 'normal vertico-map (kbd "j") 'vertico-next))
(with-eval-after-load 'vertico (evil-define-key 'normal vertico-map (kbd "k") 'vertico-previous))
(with-eval-after-load 'vertico (evil-define-key 'normal vertico-map (kbd "G") 'vertico-last))
(with-eval-after-load 'vertico (evil-define-key 'normal vertico-map (kbd "gg") 'vertico-first))
(with-eval-after-load 'vertico (evil-define-key 'normal vertico-map (kbd "C-u") 'vertico-scroll-down))
(with-eval-after-load 'vertico (evil-define-key 'normal vertico-map (kbd "C-d") 'vertico-scroll-up))
(with-eval-after-load 'vertico (evil-define-key 'insert vertico-map (kbd "C-n") 'vertico-next))
(with-eval-after-load 'vertico (evil-define-key 'insert vertico-map (kbd "C-p") 'vertico-previous))
(with-eval-after-load 'vertico (evil-define-key 'normal vertico-map (kbd "<tab>") 'vertico-insert))
(with-eval-after-load 'vertico (evil-define-key 'normal vertico-map (kbd "<RET>") '(lambda () (interactive)
                                                                                     (vertico-insert)
                                                                                     (vertico-exit-input))))

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

;; maybe useful
;;(evil-define-key 'normal minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
;;(evil-define-key 'normal minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
;;(evil-define-key 'normal minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
;;(evil-define-key 'normal minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
;;(evil-define-key 'normal minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-mc zig-mode zenburn-theme yasnippet yaml-mode which-key vterm vertico unicode-escape undo-tree rust-mode quelpa-use-package projectile orderless monokai-theme magit lsp-ui lsp-pyright lsp-haskell hydra haskell-mode gruber-darker-theme frame-local flycheck evil-collection disable-mouse diminish company-posframe autothemer auctex async ansible))
 '(whitespace-style
   '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
