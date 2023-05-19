(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1) ; Disalbe the toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1) ; Disable the menu bar
(display-battery-mode t) ; Show battery
(display-time-mode t) ; Show time


;; Make ESCc qquit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Font Fira code size 200 because of DP
;; (set-face-attribute 'default nil :font "Iosevka Aile" :height 170 :weight 'light)
(set-face-attribute 'default nil :font "Fira Code" :height 170)

;; Make Full screen when open Emacs
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;

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
(global-display-line-numbers-mode t)

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
 '(package-selected-packages
   '(nerd-icons-dired treemacs-projectile treemacs-evil treemacs-magit treemacs dashboard evil-magit magit counsel-projectile projectile hydra evil-collection evil general all-the-icons which-key use-package rainbow-delimiters ivy-rich helpful doom-themes doom-modeline counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Theming
(use-package doom-themes
  :after doom-modeline
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
        doom-themes-treemacs-theme "doom-colors")
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))(use-package doom-themes
  :init (load-theme 'doom-dracula t))

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
 dashboard-startup-banner "~/mfemacs/themes/emacs.png")

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
