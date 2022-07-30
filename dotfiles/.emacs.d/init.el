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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq package-native-compile t)
(setq comp-async-report-warnings-errors nil)
(setq comp-deferred-compilation t)
(setq use-package-always-ensure t)

;; Install and load `quelpa-use-package'.
(setq quelpa-update-melpa-p nil)
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)

;;; ASYNC
;; Emacs look SIGNIFICANTLY less often which is a good thing.
;; asynchronous bytecode compilation and various other actions makes
(use-package async
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
;; (setq-default word-wrap t)
(setq-default indent-tabs-mode nil)
(setq-default compilation-scroll-output t)
(setq-default dired-dwim-target t)

;; (add-to-list 'default-frame-alist `(font . ,"Iosevka-20"))
(add-to-list 'default-frame-alist `(font . ,"Ubuntu Mono-20"))

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
(column-number-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
(menu-bar--display-line-numbers-mode-relative)
;; (fringe-mode 0)

;; Install and configure packages
(use-package evil
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
  (evil-mode 1)
  ;; keybindings
  (add-to-list 'evil-normal-state-modes 'shell-mode)
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

  (evil-define-key 'normal 'global (kbd "<leader>la") 'fill-paragraph)
  (evil-define-key 'normal 'global (kbd "gr") 'revert-buffer)
  (evil-define-key 'visual 'global (kbd "<leader>a") 'align-regexp)
  (evil-define-key 'visual 'global (kbd "C") 'comment-or-uncomment-region)
  (evil-define-key 'normal 'global (kbd "gc") 'comment-dwim)

  ;; find
  (evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>jv") 'dired-jump)
  (evil-define-key 'normal 'global (kbd "<leader>u") 'undo-tree-visualize)
  (evil-define-key 'normal 'global (kbd "<leader>g") 'magit)
  (evil-define-key 'normal 'global (kbd "<leader>i") 'eww)
  (evil-define-key 'normal 'global (kbd "C-o") 'evil-buffer)

  ;; Compilation
  (evil-define-key 'normal 'global (kbd "<leader>mm") 'compile)
  (evil-define-key 'normal 'global (kbd "<leader>mr") 'recompile)
  (evil-define-key 'normal 'global (kbd "<leader>mk") 'kill-compilation)
  (evil-define-key 'normal 'global (kbd "<leader>ms") 'async-shell-command)

  ;; escape minibuffer
  (global-set-key (kbd "C-x ESC") nil)
  (evil-define-key 'normal 'global (kbd "<escape>") 'abort-minibuffers)

  (evil-define-key 'normal 'global (kbd "<leader>ln") 'next-error)
  (evil-define-key 'normal 'global (kbd "<leader>lp") 'previous-error)

  ;; windows and buffers
  (evil-define-key 'normal 'global (kbd "<leader>o") 'delete-other-windows)
  (evil-define-key 'normal 'global (kbd "<leader>q") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<leader>h") 'evil-window-split)
  (evil-define-key 'normal 'global (kbd "<leader>ö") 'evil-window-vsplit)
  (evil-define-key 'normal 'global (kbd "<leader>0") 'evil-window-next)
  (evil-define-key 'normal 'global (kbd "<leader>9") 'evil-window-prev)
  (evil-define-key 'normal 'global (kbd "<leader>jk") 'kill-buffer)
  (evil-define-key 'normal minibuffer-mode-map (kbd "C-k") 'previous-history-element)
  (evil-define-key 'normal minibuffer-mode-map (kbd "C-j") 'next-history-element)
  (evil-define-key 'normal minibuffer-mode-map (kbd "<RET>") 'exit-minibuffer)
  (evil-define-key 'insert minibuffer-mode-map (kbd "C-k") 'previous-history-element)
  (evil-define-key 'insert minibuffer-mode-map (kbd "C-j") 'next-history-element))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-multiedit
  :after evil
  :config
  (evil-multiedit-mode 1)
  (evil-define-key 'normal evil-multiedit-mode-map (kbd "<escape>") 'evil-multiedit-abort)
  (evil-define-key 'normal 'global "z" 'evil-multiedit-match-symbol-and-next)
  (evil-define-key 'normal evil-multiedit-mode-map "z" 'evil-multiedit-match-symbol-and-next)
  (evil-define-key 'normal 'global "Z" 'evil-multiedit-match-symbol-and-prev)
  (evil-define-key 'visual 'global "z" 'evil-multiedit-match-and-next)
  (evil-define-key 'visual 'global "Z" 'evil-multiedit-match-and-prev)
  (evil-define-key 'visual 'global "R" 'evil-multiedit-match-all))

(use-package yasnippet
  :after evil
  :config
  (setq yas-triggers-in-field nil)
  (setq yas-snippet-dirs '("~/.emacs.snippets/"))
  (yas-global-mode 1))

(use-package disable-mouse
  :config
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map)))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package projectile
  :init
  (when (file-directory-p "~/documents")
    (setq projectile-project-search-path '("~/documents")))
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (projectile-mode 1)
  (evil-define-key 'normal 'global (kbd "<leader>jf") 'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>jj") 'projectile-switch-project))

(use-package consult
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd "<leader>k") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>ll") 'consult-flymake)
  (evil-define-key 'normal 'global (kbd "<leader>jl") 'consult-line)
  (evil-define-key 'normal 'global (kbd "<leader>jr") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader>jg") 'consult-git-grep))

(use-package vertico
  :after evil
  :init (vertico-mode 1)
  :config
  (evil-define-key 'normal vertico-map (kbd "j") 'vertico-next)
  (evil-define-key 'normal vertico-map (kbd "k") 'vertico-previous)
  (evil-define-key 'normal vertico-map (kbd "G") 'vertico-last)
  (evil-define-key 'normal vertico-map (kbd "gg") 'vertico-first)
  (evil-define-key 'normal vertico-map (kbd "C-u") 'vertico-scroll-down)
  (evil-define-key 'normal vertico-map (kbd "C-d") 'vertico-scroll-up)
  (evil-define-key 'insert vertico-map (kbd "C-n") 'vertico-next)
  (evil-define-key 'insert vertico-map (kbd "C-p") 'vertico-previous)
  (evil-define-key 'normal vertico-map (kbd "<tab>") 'vertico-insert))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package undo-tree
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package astyle
  :config
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
              (setq-local fill-paragraph-function 'astyle-buffer))))

;; (use-package gruber-darker-theme
;;   :config
;;   (load-theme 'gruber-darker t))
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package eglot
  :init
  (setq eldoc-echo-area-display-truncation-message t)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  :config
  (evil-define-key 'normal flymake-mode-map (kbd "<leader>ln") 'flymake-goto-next-error)
  (evil-define-key 'normal flymake-mode-map (kbd "<leader>lp") 'flymake-goto-prev-error)
  (evil-define-key 'normal 'global (kbd "<leader>lr") 'eglot-rename)
  (evil-define-key 'normal 'global (kbd "<leader>li") 'eglot-find-implementation)
  (evil-define-key 'normal 'global (kbd "<leader>lf") 'xref-find-references)
  (evil-define-key 'normal 'global (kbd "<leader>la") 'eglot-format-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>ld") 'eglot-find-declaration)
  (evil-define-key 'normal 'global (kbd "<leader>lc") 'eglot-code-actions)
  (evil-define-key 'normal 'global (kbd "<leader>lh") 'eldoc-doc-buffer))

(use-package corfu
  :after yasnippet
  :custom
  (corfu-auto t)
  (tab-always-indent 'complete)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-preselect-first nil)
  :init
  (global-corfu-mode)
  :config
  (evil-define-key 'insert corfu-map (kbd "C-e") 'corfu-quit)
  (evil-define-key 'insert corfu-map (kbd "<RET>") 'evil-ret)
  (evil-define-key 'insert 'global (kbd "C-<SPC>") 'complete-symbol)
  (evil-define-key 'insert corfu-map (kbd "TAB") #'(lambda () (interactive) ;; i dont know if this is a good idea (maybe find a better solution)
                                                     (indent-for-tab-command)
                                                     (yas-expand)))
  (evil-define-key 'insert corfu-map [tab] #'(lambda () (interactive)
                                                     (indent-for-tab-command)
                                                     (yas-expand))))

(use-package cape
  :defer 10
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (setq cape-dabbrev-min-length '1)
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))))

;; languages
(use-package rust-mode)
(use-package go-mode)
(use-package haskell-mode 
  :config
  (add-hook 'haskell-mode-hook #'haskell-doc-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))
  ;; fix haskell being slow
  ;; (setq-default flycheck-disabled-checkers '(haskell-stack-ghc)))
(use-package yaml-mode)
(use-package typescript-mode)
(use-package zig-mode
  :init
  (setq zig-format-on-save nil))

;; dired
(add-hook 'dired-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'dired-mode-hook 'auto-revert-mode)
(with-eval-after-load 'dired (evil-define-key 'normal dired-mode-map (kbd "<SPC>") 'evil-send-leader))

;; eww
(add-hook 'eww-mode-hook (lambda () (display-line-numbers-mode -1)))

;; functions
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
   '("a3e99dbdaa138996bb0c9c806bc3c3c6b4fd61d6973b946d750b555af8b7555b" "5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" default))
 '(package-selected-packages
   '(eglot move-text astyle zig-mode zenburn-theme yasnippet yaml-mode vertico unicode-escape undo-tree typescript-mode rust-mode quelpa-use-package projectile orderless iedit hydra haskell-mode gruber-darker-theme frame-local flymake-easy flycheck-nimsuggest epc disable-mouse commenter autothemer auctex async ansible)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Take a look at paredit
