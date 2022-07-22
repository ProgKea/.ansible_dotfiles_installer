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
(setq-default dired-dwim-target t)

;; add-to-lists
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
(blink-cursor-mode 0)
(column-number-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
(menu-bar--display-line-numbers-mode-relative)
(fringe-mode 0)
(blink-cursor-mode 1)

(load "~/.emacs.d/elpa/simpc-mode.el" t)

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
  ;; (setq cursor-type 'box)
  ;; (setq evil-normal-state-cursor 'box)
  ;; (setq evil-insert-state-cursor 'box)
  ;; (setq evil-visual-state-cursor 'box)
  ;; (setq evil-motion-state-cursor 'box)
  ;; (setq evil-replace-state-cursor 'box)
  ;; (setq evil-operator-state-cursor 'box)
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
  (setq yas/triggers-in-field nil)
  (setq yas-snippet-dirs '("~/.emacs.snippets/"))
  (yas-global-mode 1))

;; languages
(use-package rust-mode
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
(evil-define-key 'normal 'global (kbd "<leader>lf") 'lsp-find-references)
(evil-define-key 'normal 'global (kbd "<leader>ld") 'evil-goto-definition)
(evil-define-key 'normal 'global (kbd "<leader>lc") 'lsp-execute-code-action)
(evil-define-key 'normal 'global (kbd "<leader>la") 'lsp-format-buffer)
(evil-define-key 'normal 'global (kbd "<leader>ll") 'flycheck-list-errors)
(evil-define-key 'normal 'global (kbd "<leader>lh") 'lsp-ui-doc-glance)
(evil-define-key 'normal 'global (kbd "<leader>lo") '(lambda () (interactive)
                                                       (lsp-ui-doc-show)
                                                       (lsp-ui-doc-focus-frame)))

;; Autocompletion keybindings
;; Company
(with-eval-after-load 'company (define-key company-active-map (kbd "C-w") 'backward-kill-word))
(with-eval-after-load 'company (define-key company-active-map (kbd "C-e") 'company-abort))
(with-eval-after-load 'company (global-set-key (kbd "C-<SPC>") 'company-complete))

;; Yasnippet
(evil-define-key 'insert company-mode-map (kbd "<tab>") 'yas-expand)
(evil-define-key 'insert company-mode-map [tab] 'yas-expand)
(define-key company-active-map [tab] 'yas-expand)
(define-key company-active-map (kbd "TAB") 'yas-expand)
(define-key yas-minor-mode-map [tab] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-keymap [tab] 'yas-next-field)
(define-key yas-keymap (kbd "TAB") 'yas-next-field)

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
   '("cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "0cd00c17f9c1f408343ac77237efca1e4e335b84406e05221126a6ee7da28971" "93553b47c1837b65bcbbc7cb2024da14584223985620e50fc5d5d48a6c7ce0e2" "19759a26a033dcb680aa11ee08677e3146ba547f1e8a83514a1671e0d36d626c" "5a611788d47c1deec31494eb2bb864fde402b32b139fe461312589a9f28835db" "4a288765be220b99defaaeb4c915ed783a9916e3e08f33278bf5ff56e49cbc73" "46b2d7d5ab1ee639f81bde99fcd69eb6b53c09f7e54051a591288650c29135b0" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "fb83a50c80de36f23aea5919e50e1bccd565ca5bb646af95729dc8c5f926cbf3" "e3a1b1fb50e3908e80514de38acbac74be2eb2777fc896e44b54ce44308e5330" "b6269b0356ed8d9ed55b0dcea10b4e13227b89fd2af4452eee19ac88297b0f99" "b02eae4d22362a941751f690032ea30c7c78d8ca8a1212fdae9eecad28a3587f" "c8b83e7692e77f3e2e46c08177b673da6e41b307805cd1982da9e2ea2e90e6d7" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "3d2e532b010eeb2f5e09c79f0b3a277bfc268ca91a59cdda7ffd056b868a03bc" "d84603447cb3b5291abfd7d03a0d79b156c240663687d19e911dde438af15eba" "a3e99dbdaa138996bb0c9c806bc3c3c6b4fd61d6973b946d750b555af8b7555b" default))
 '(package-selected-packages
   '(modus-themes typescript-mode zenburn-theme evil-mc zig-mode yasnippet yaml-mode which-key vterm vertico unicode-escape undo-tree rust-mode quelpa-use-package orderless magit lsp-pyright hydra haskell-mode gruber-darker-theme frame-local disable-mouse diminish company-posframe autothemer auctex async ansible))
 '(whitespace-style
   '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
