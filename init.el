(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1) ; Disalbe the toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1) ; Disable the menu bar
(display-battery-mode t) ; Show battery
(display-time-mode t) ; Show time

;; Make ESCc qquit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Theme
(set-face-attribute 'default nil :font "Monaco" :height 170)

;; Make Full screen when open Emacs
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;

;; Modus Themes
;; (add-to-list 'load-path "~/.emacs.d/modus-themes")
;; (load-theme 'modus-operandi)            ; Light theme
;; (load-theme 'modus-vivendi)             ; Dark theme

  


;; Mapping macOS keybindings to emacs C,M
(setq mac-control-modifier 'control)
(setq mac-right-option-modifier 'control)


;; Reload Emacs configuration
(defun reload-init-file ()
  (interactive)
  (load-file "~/.emacs"))


;; Reload file with Control c r
(global-set-key (kbd "C-c r") 'reload-init-file)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))


(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Counsel setup
(use-package counsel
  :bind (("M-x" . counsel-M-x)
  ("C-x b" . counsel-ibuffer)
	("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start seaches with x

;; Ivy - Auto completion for Emacs and Swiper which is file Search engine
(use-package ivy
  :diminish
  :bind (("s-f" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Bottom Modeline
;; Config and install modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (custom-set-faces
   '(mode-line ((t (:family "SF Mono" :height 0.85)))))
  (custom-set-faces
   '(mode-line-inactive ((t (:family "SF Mono" :height 0.85)))))

  (setq doom-modeline-buffer-encoding nil
        doom-modeline-percent-position nil
        doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 50
        doom-modeline-major-mode-icon nil
        doom-modeline-project-detection 'projectile
        doom-modeline-icon t
        doom-modeline-modal t
        doom-modeline-modal-icon nil
        doom-modeline-lsp t
        doom-modeline-workspace-name nil
        doom-modeline-persp-name nil
        doom-modeline-bar-width 5
        doom-modeline-height 38
        doom-modeline-hud nil
	doom-modeline-window-width-limit 85
	doom-modeline-project-detection 'auto
        doom-modeline-buffer-state-icon nil
	doom-modeline-buffer-name t
	doom-modeline-env-enable-swift t
        doom-modeline-time-icon nil)
  (setq evil-normal-state-tag   (propertize "NORMAL" 'face '((:background "green" :foreground "black")))
        evil-emacs-state-tag    (propertize "EMACS" 'face '((:background "orange" :foreground "black")))
        evil-insert-state-tag   (propertize "INSERT" 'face '((:background "red") :foreground "white"))
        evil-motion-state-tag   (propertize "MOTION" 'face '((:background "blue") :foreground "white"))
        evil-visual-state-tag   (propertize "VISUAL" 'face '((:background "grey80" :foreground "black")))
        evil-operator-state-tag (propertize "OPERATOR" 'face '((:background "purple")))))

;; For Emacs Parenthies checking
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(org-agenda-files
   '("~/Desktop/Org-files/PersonalDevelopment/professional-discipline.org"))
 '(package-selected-packages
   '(pdf-tools helm-icons spacemacs-theme yasnippet lsp-ui rustic rust-mode typescript-mode deno-fmt markdown-soma xcode-mode ob-swiftui centaur-tabs nerd-icons-dired treemacs-projectile treemacs-evil treemacs-magit treemacs dashboard evil-magit magit counsel-projectile projectile hydra evil-collection evil general all-the-icons which-key use-package rainbow-delimiters ivy-rich helpful doom-themes doom-modeline counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:family "SF Mono" :height 0.85))))
 '(mode-line-inactive ((t (:family "SF Mono" :height 0.85)))))

;; Theming
(use-package doom-themes
  :after doom-modeline
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
        doom-themes-treemacs-theme "doom-colors")
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))
(use-package doom-themes
  :init (load-theme 'spacemacs-dark t))


(use-package all-the-icons
   :init
   (unless (member "all-the-icons" (font-family-list))
     (all-the-icons-install-fonts t)))


;; Global Key Bindings
(general-define-key
 :states '(normal insert visual emacs)
 "C-f" 'counsel-find-file
 "C-r" 'counsel-switch-buffer)

;; Whenever we call this function, it opens our init.el
(setq vc-follow-symlinks t) ; This is for opening config symlink file inside emacs, without prompt question
(defun open-init-file ()
  (interactive)
  (find-file user-init-file))

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "C-SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "ts" '(hydra-text-scale/body :which-key "scale text")
    "c"  'open-init-file))
 


;; Make Enter works in all mode in mini buffers with Evil mode
(defun my-minibuffer-setup ()
  (define-key minibuffer-local-map (kbd "RET") 'minibuffer-complete-and-exit))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)

;; Eval configuraation VI
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Run project C-c p u
;; Run tests: C-c p P
;; Edit dir-locals: C-c p E (reload with hack-dir-local-variables-non-file-buffer)
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Users/tornike-mac/Development")
    (setq projectile-project-search-path '("~/Users/tornike-mac/Development")))
  (setq projectile-switch-project-action #'projectile-dired))

;; counsel-projectile
;; https://github.com/ericdanan/counsel-projectile
;; Advanced project switching: C-c p p
;; Quick searching with counsel-projectile-rg - C-c p s r
;; Results to buffer with C-c C-o
(use-package counsel-projectile
 :after projectile
 :config
 (counsel-projectile-mode 1))

;; 🦄
;; https://github.com/magit/magit
;; https://magit.vc/
;; https://magit.vc/manual/magit/
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Operations
;; Run magit-status in buffer from Git repo, press ? for command panel.
;; Refresh buffer with g r
;; Diffs
;; Commit log
;; Blame
;; Stashes
;; Branches
;; Rebase
;; Pull/push


;; Welcome Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq
 dashboard-banner-logo-title "Welcome Tornike 🚀, Have a happy Emacs Day! 🦸🏻‍♂️"
 dashboard-startup-banner "~/mfemacs/themes/true.png")

(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nyan-mode
  :ensure t
  :init
  (nyan-mode 1))

(add-to-list 'load-path "~/mfemacs/localpackages/")

(use-package swift-mode
  :defer t
  :mode "\\.swift\\'"
  :config
  (setq swift-mode:basic-offset 2
        swift-mode:parenthesized-expression-offset 2
        swift-mode:multiline-statement-offset 2
        swift-mode:highlight-anchor t))

(add-hook 'swift-mode-hook (lambda () (lsp)))
(add-hook 'swift-mode-hook
          (lambda ()
            (electric-pair-mode 1)
            (setq electric-pair-pairs '((?{ . ?})))
            (setq electric-pair-text-pairs '((?{ . ?})))
            ))

(use-package ios-simulator
  :ensure nil
  :after swift-mode
  :bind
  ("C-c x b" . #'ios-simulator:terminate-current-app)
  ("C-c x c" . #'ios-simulator:appcontainer)
  ("C-c x l" . #'ios-simulator:change-language))


;; Currently we are using EGLOT client for LSP, so we at this moment have commented use-package code for LSP
;; (use-package lsp-sourcekit
;;  :after lsp-mode
;;  :config
;;  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package eglot
    :ensure t
    :hook (swift-mode . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs '(swift-mode . ("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))))


;; Markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; Smerge mode
(setq smerge-command-prefix "\C-cv")

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Rustik
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; LSP UI with custom rust clippy analyzer
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  ;; (lsp-inlay-hint-enable nil) ;; This option turns on hints if there is such in Rust or Swift analyzer.
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t) ;; Chain hints
  ;;(lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t) ;; Closure hints
  ;; (lsp-rust-analyzer-display-parameter-hints nil) ;; Parameters Hints
  (lsp-rust-analyzer-display-reborrow-hints nil) ;; Reborrow hints
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t))

;; Company Mode
(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))


;; Deno EGLOT
;; (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))
;;  (defclass eglot-deno (eglot-lsp-server) ()
;;    :documentation "A custom class for deno lsp.")

;;  (cl-defmethod eglot-initialization-options ((server eglot-deno))
;;    "Passes through required deno initialization options"
;;   (list :enable t
;;    :lant t))


;; Set scroll conervatively like in Spacemacs, to avoid FPS issues
(setq scroll-conservatively 101)




;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))



;; Custom shell mode behaviors
;; Kill the buffer when shell process exists
(defun my-shell-mode-hook ()
  "Custom `shell-mode' behaviours."
  (let* ((proc (get-buffer-process (current-buffer)))
         (sentinel (process-sentinel proc)))
    (set-process-sentinel
     proc
     `(lambda (process signal)
        ;; Call the original process sentinel first.
        (funcall #',sentinel process signal)
        ;; Kill the buffer on an exit signal.
        (and (memq (process-status process) '(exit signal))
             (buffer-live-p (process-buffer process))
             (kill-buffer (process-buffer process)))))))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)
