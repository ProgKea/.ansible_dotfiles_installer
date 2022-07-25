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
(setq byte-compile-warnings '(cl-functions))
(setq inhibit-startup-screen t)
(setq compile-command "")
(setq compilation-ask-about-save nil)
(setq scroll-margin 8)
(setq auto-save-default nil)
(setq-default word-wrap t)
(setq-default indent-tabs-mode nil)
(setq-default compilation-scroll-output t)
(setq-default dired-dwim-target t)

;; add-to-lists
(add-to-list 'default-frame-alist `(font . ,"Iosevka-20"))
;; (add-to-list 'default-frame-alist `(font . ,"Ubuntu Mono-20"))

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

(use-package evil-multiedit
  :ensure t
  :config
  (evil-multiedit-mode 1))

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

(use-package consult
  :ensure)

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

;; Themes
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

(use-package lsp-pyright :ensure t)
(use-package lsp-ui  :ensure t
  :init
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions t))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  :init
  (global-corfu-mode))

(use-package yasnippet
  :ensure
  :config
  (setq yas/triggers-in-field nil)
  (setq yas-snippet-dirs '("~/.emacs.snippets/"))
  (yas-global-mode 1))

;; languages
(use-package rust-mode
  :ensure)

(use-package go-mode
  :ensure)

(use-package haskell-mode :ensure t)

(use-package yaml-mode :ensure)

(use-package typescript-mode :ensure)

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
(evil-define-key 'visual 'global (kbd "<leader>s") 'replace-string)
(evil-define-key 'visual 'global (kbd "<leader>S") 'replace-regexp)
(evil-define-key 'visual 'global (kbd "<leader>r") 'query-replace)
(evil-define-key 'visual 'global (kbd "<leader>R") 'query-replace-regexp)

(evil-define-key 'visual 'global (kbd "<leader>a") 'align-regexp)
(evil-define-key 'visual 'global (kbd "C") 'comment-or-uncomment-region)

;; find
(evil-define-key 'normal 'global (kbd "C-j") 'projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>jf") '(lambda() (interactive)
                                                       (cd "~/")
                                                       (call-interactively 'find-file)))

(evil-define-key 'normal 'global (kbd "<leader>jv") 'dired-jump)
(evil-define-key 'normal 'global (kbd "<leader>jj") 'projectile-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>u") 'undo-tree-visualize)
(evil-define-key 'normal 'global (kbd "<leader>g") 'magit)
(evil-define-key 'normal 'global (kbd "<leader>i") 'eww)
(evil-define-key 'normal 'global (kbd "C-o") 'evil-buffer)

;; consult
(evil-define-key 'normal 'global "/" 'consult-line)
(evil-define-key 'normal 'global (kbd "<leader>jr") 'consult-ripgrep)
(evil-define-key 'normal 'global (kbd "<leader>jg") 'consult-git-grep)
(evil-define-key 'normal 'global (kbd "<leader>jb") 'consult-buffer)

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

;; lsp keybindings
(evil-define-key 'normal 'global (kbd "<leader>lr") 'lsp-rename)
(evil-define-key 'normal 'global (kbd "<leader>lf") 'lsp-find-references)
(evil-define-key 'normal 'global (kbd "<leader>ld") 'evil-goto-definition)
(evil-define-key 'normal 'global (kbd "<leader>lc") 'lsp-execute-code-action)
(evil-define-key 'normal 'global (kbd "<leader>la") 'lsp-format-buffer)
(evil-define-key 'normal 'global (kbd "<leader>ll") 'flycheck-list-errors)
(evil-define-key 'normal 'global (kbd "<leader>lh") 'lsp-ui-doc-glance)
(evil-define-key 'normal 'global (kbd "<leader>lo") '(lambda () (interactive)
                                                       (lsp-ui-doc-show)
                                                       (lsp-ui-doc-focus-frame)))

(evil-define-key 'insert corfu-map (kbd "C-e") 'corfu-quit)
(evil-define-key 'insert corfu-map (kbd "<RET>") 'corfu-complete)
(evil-define-key 'insert 'global (kbd "C-<SPC>") 'complete-symbol)
(evil-define-key 'insert corfu-map (kbd "TAB") 'yas-expand)
(evil-define-key 'insert corfu-map [tab] 'yas-expand)

;; Multicursor
(evil-define-key 'normal evil-multiedit-mode-map (kbd "<escape>") 'evil-multiedit-abort)
(evil-define-key 'normal 'global "z" 'evil-multiedit-match-symbol-and-next)
(evil-define-key 'normal evil-multiedit-mode-map "z" 'evil-multiedit-match-symbol-and-next)
(evil-define-key 'normal 'global "Z" 'evil-multiedit-match-symbol-and-prev)
(evil-define-key 'visual 'global "z" 'evil-multiedit-match-and-next)
(evil-define-key 'visual 'global "Z" 'evil-multiedit-match-and-prev)
(evil-define-key 'visual 'global "R" 'evil-multiedit-match-all)

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

(evil-define-key 'insert minibuffer-mode-map (kbd "C-k") 'previous-history-element)
(evil-define-key 'insert minibuffer-mode-map (kbd "C-j") 'next-history-element)
(evil-define-key 'normal minibuffer-mode-map (kbd "C-k") 'previous-history-element)
(evil-define-key 'normal minibuffer-mode-map (kbd "C-j") 'next-history-element)
(evil-define-key 'normal minibuffer-mode-map (kbd "<RET>") '(lambda () (interactive)
                                                              (evil-insert)
                                                              (evil-ret)))

;; hooks
(add-hook 'dired-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'dired-mode-hook 'auto-revert-mode)
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'eww-mode-hook (lambda () (display-line-numbers-mode -1)))
;; Haskell
(add-hook 'haskell-mode-hook #'haskell-doc-mode)
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)
;; fix haskell being slow
(setq-default flycheck-disabled-checkers '(haskell-stack-ghc))

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
 '(custom-safe-themes
   '("5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" default))
 '(package-selected-packages
   '(move-text astyle zig-mode zenburn-theme yasnippet yaml-mode which-key vterm vertico unicode-escape undo-tree typescript-mode rust-mode quelpa-use-package projectile orderless lsp-pyright iedit hydra haskell-mode gruber-darker-theme frame-local flymake-easy flycheck-nimsuggest evil-collection epc disable-mouse commenter autothemer auctex async ansible)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; TODO: move line keybindings 
