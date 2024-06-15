;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(defconst font-size 22)
(defconst big-font-size 24)
;; (defconst font-size 26)
;; (defconst big-font-size 32)
;;
(setq
      doom-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size font-size)
      ;; doom-font (font-spec :family "CaskaydiaCove Nerd Font Mono" :size font-size)
      doom-variable-pitch-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size font-size)
      doom-big-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size big-font-size)
      ;; Do not set size of unicode font or it wont scale on zoom.
      doom-symbol-font (font-spec :family "DejaVu Sans")) ;; doom-unicode-font

(setq catppuccin-flavor
      'latte)
      ;; 'frappe)
      ;; 'macchiato)
      ;; 'mocha)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(use-package doom-themes
  ;; :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme
        ;;;; Favorites
        ;; 'doom-one
        'catppuccin
        ;; 'doom-one-light
        ;; 'adwaita ;; a bit like vs code
        ;; 'doom-palenight
        ;; 'doom-challenger-deep
        ;; 'doom-snazzy
        ;; 'doom-vibrant
        ;; 'doom-dark+ ;; vs code dark
        ;; 'doom-sourcerer -- looks like Loop Hero
        ;; 'default

        ;;;; Synthwave Themes
        ;; 'doom-laserwave
        ;; 'doom-outrun-electric
        ;; 'doom-shades-of-purple
        t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (setq doom-themes-neotree-file-icons t)
  ;; (setq neo-window-fixed-size nil)
  ;; (setq neo-window-width 40)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
;; (setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;; ADDITIONS BY PAUL AFTER THIS LINE ;;;;

;; Configure projectile
(setq projectile-indexing-method 'alien
      ;; projectile-sort-order 'recently-active ;; this won't work at all so I just don' set it :(
      projectile-enable-caching t
      )

(setq evil-snipe-override-evil-repeat-keys nil)
;; Make "," be the local leader instead of SPC-m
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-SPC ,")

;; SPC s s: search buffer
;; g s s: evil-avy-goto-char-2
;; g s /: evil-avy-goto-char-timer
;; g s l: evil-avy-goto-line
(after! evil
  (map! :map (evil-normal-state-map evil-visual-state-map)
        ;; switch default bindings for "j" and "gj" and for "k" and "gk".
        "j" #'evil-next-visual-line
        "k" #'evil-previous-visual-line
        "gj" #'evil-next-line
        "gk" #'evil-previous-line
        ;; Some rebinds for faster avy.
        ;; Replace snipe by avy-goto-char-2:
        ;; I like snipe but it only jumps within a single line which is useless to me often.
        "s" #'evil-avy-goto-char-timer
        "S" #'evil-avy-goto-line
        ;; Replace single line t and T with avy-go-to-char-timer
        "t" #'evil-avy-goto-char-2-below
        "T" #'evil-avy-goto-char-2-above
        ;; By default, "gss" is #'evil-avy-goto-char-2
        ;; By default, "gs/" is #'evil-avy-goto-char-timer
        ;; Let's add a similar binding for lines.
        "gsl" #'evil-avy-goto-line
        )
  ;; copy and paste like done by normal people
  (map! :map (evil-insert-state-map evil-visual-state-map)
        "C-c" #'evil-yank
        )
  (map! :map evil-insert-state-map
        "C-v" #'evil-paste-after
        )
  )

(map! :leader
      :prefix "c"
      (:desc "Compile project" "c" #'project-compile)
      (:desc "Compile (here)" "l" #'compile)
      (:desc "Make" "m" #'+make/run)
      )

(defun open-terminal (args)
  "Open a terminal with the given arguments"
  (message args)
  (start-process "terminal-from-emacs" nil "kitty" args)
  )

(defun open-terminal-here ()
  "Open a terminal at the current directory"
  (interactive)
  (open-terminal ".")
  )

(defun open-dolphin-here ()
  "Open a terminal at the current directory"
  (interactive)
  (start-process "dolphin-from-emacs" nil "dolphin" ".")
  )

(defun open-ranger-at (dir-string)
  "Open the file explorer with the given arguments"
  (message (concat "kitty --session launch-ranger.kitty " dir-string))
  (start-process "explorer-from-emacs" nil "kitty" "--session" "launch-ranger.kitty" dir-string)
)

(defun open-ranger-here ()
  "Open a terminal at the current directory"
  (interactive)
  (open-ranger-at ".")
  )

(map! :leader
      (:prefix "o"
               (:desc "Ranger here" "e" #'open-ranger-here)
               (:desc "Explorer here (Dolphin)" "d" #'open-dolphin-here) ;; alias for my manjaro system
               (:desc "Terminal here" "t" #'open-terminal-here)
               (:desc "Neotree" "n" #'+neotree/find-this-file)
               )
      )

;; Disable annoying smartparens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; Scroll settings
;; scroll acceleration
(setq mouse-wheel-progressive-speed nil)
;; scroll two lines at a time but only one when pressing shift
(setq mouse-wheel-scroll-amount '(2 ((shift) 1) ((control) nil)))

;; Make which-key menu (the menu when pressing SPC) show up instantly
;; (This tweak was recommended by hlissner in doom emacs issue #1839)
(require 'which-key)
(setq which-key-idle-delay 0.1)

;; set evil to not move the cursor when exiting insert mode
;; (setq evil-move-cursor-back nil)

;; Re-bind SPC SPC to behave like in spacemacs: It opens the emacs command prompt (M-x).
;; (map! :leader "SPC" 'execute-extended-command)

;;;;; Misc key bindings
(map! "C-s" 'save-buffer)
;; TODO USE key "g c c" instead.
(defun comment-eclipse ()
    (interactive)
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      ;; when we have a region selected, set 'start and 'end to cover the region
      (when (or (not transient-mark-mode) (region-active-p))
        (setq start (save-excursion
                      (goto-char (region-beginning))
                      (beginning-of-line)
                      (point))
              end (save-excursion
                    (goto-char (region-end))
                    ;; (end-of-line)
                    (beginning-of-line)
                    (point)))
        )
      (comment-or-uncomment-region start end)
      (setq deactivate-mark nil) ;; keep the region selected afterwards
      ))

(map! :n "C-t" #'comment-eclipse)

;;;;;; Agda setup

;; Disable font-lock mode in agda2-mode because they do not work well together.
;; font-lock mode saves resources by applying syntax highlighting only to visible
;; code areas, and then lazily updating the highlights when other parts of the code
;; become visible.
;; In agda2-mode, updating the highlights requires to re-run the type-checker (manually!)
;; which is quite annoying.
(defun font-lock-not-in-agda (mode)
  (if (derived-mode-p 'agda2-mode)
    (font-lock-default-function nil)
    (font-lock-default-function mode)
  )
)
(setq font-lock-function 'font-lock-not-in-agda)

;; nix setup for agda
(load! "nix-shell.el")

(defun global-agda ()
  (interactive)
  (add-load-path!
    (file-name-directory (shell-command-to-string "agda-mode locate")))
  (if (require 'agda2 nil t)
      (progn
        (normal-mode)
        (agda2-load)
        )
      (message "Failed to find the `agda2' package")))

(defun nix-agda (nix-shell-path)
  (interactive (list (nix-shell-read-path "nix expression path: ")))
  (nix-shell-activate nix-shell-path)
  (global-agda))

;; Can't use `use-package!' because it will try to load agda2-mode immediately.
(load! "modules/lang/agda/config.el" doom-emacs-dir)
(after! agda2
  (add-hook! 'agda2-mode-hook
    (setq-local evil-shift-width 2)
    (activate-input-method "Agda"))
  (prependq! auto-mode-alist '(("\\.lagda.md\\'" . agda2-mode)))
  ;; Make evil repeat (pressing .) ignore agda2-load.
  (evil-add-command-properties 'agda2-load :repeat 'ignore)
  ;; Add agda holes as evil-surround braces
  (after! evil-surround
    (embrace-add-pair ?! "{!" "!}")))

(setq dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/.myconfig.git")))
(setq dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))

(defun dotfiles-status ()
  "Run magit-status for my dotfiles repository."
  (interactive)
  (add-to-list 'magit-git-global-arguments dotfiles-git-dir)
  (add-to-list 'magit-git-global-arguments dotfiles-work-tree)
  (call-interactively 'magit-status)
  ;; I tried to only override the magit-git-global-arguments locally via let.
  ;; This worked but only for the invocation of magit-status, which returned immediately
  ;; after opening the magit buffer.
  ;; When performing tasks in the magit buffer, the variable overwrite would be forgotten,
  ;; and hence, I was unable to stage any changes for example.
  ;; As a hack, I reset the globally changed variables now, whenever
  ;; I launch magit-status via my own wrapper functions (see below).
  ;; let ((magit-git-global-arguments (append (list dotfiles-git-dir dotfiles-work-tree) magit-git-global-arguments)))
  )

(defun dotfiles-stage-buffer-file ()
  "Run magit-status for my dotfiles repository."
  (interactive)
  (add-to-list 'magit-git-global-arguments dotfiles-git-dir)
  (add-to-list 'magit-git-global-arguments dotfiles-work-tree)
  (magit-stage-buffer-file)
  )

(defun my-magit-reset-global-args ()
  "Function that removes any custom additions
   for dotfiles from the magit-git-global-arguments."
  (setq magit-git-global-arguments (remq dotfiles-git-dir magit-git-global-arguments))
  (setq magit-git-global-arguments (remq dotfiles-work-tree magit-git-global-arguments))
  )

(defun my-magit-status ()
  "Wrapper for magit-status that clears dotfiles from variables."
  (interactive)
  (my-magit-reset-global-args)
  (call-interactively 'magit-status)
  )

(defun my-magit-status-here ()
  "Wrapper for magit-status that clears dotfiles from variables."
  (interactive)
  (my-magit-reset-global-args)
  (call-interactively 'magit-status-here)
  )

;; some keybindings for faster interaction with doom
(map! :leader
      (:prefix ("d" . "Doom (Custom)")
               (:desc "Ranger" "." #'ranger)
               (:desc "Paste from kill-ring" "p" #'consult-yank-pop)
               (:desc "Reload" "r" #'doom/reload)
               (:desc "Agda (system)" "a" #'global-agda)
               (:desc "Agda (nix)" "A" #'nix-agda)
               (:desc "Config" "c" #'doom/goto-private-config-file)
               (:desc "Calendar" "C" #'calendar)
               (:desc "Help Search" "h" #'doom/help-search)
               (:prefix ("g" . "Dotfiles Git")
                 (:desc "status" "g" #'dotfiles-status)
                 (:desc "stage current buffer's file" "s" #'dotfiles-stage-buffer-file)
                 )
               )
      (:prefix "g"
               ("g" #'my-magit-status)
               ("G" #'my-magit-status-here)
               )
      )

(map! :leader
  (:desc "Search+Replace in project" "r" #'projectile-replace)
  (:desc "Previous Buffer" "<left>" #'previous-buffer)
  (:desc "Next Buffer" "<right>" #'next-buffer)
)

(after! centaur-tabs
  ;; (setq centaur-tabs-style "wave")
  ;; (setq centaur-tabs-set-bar 'under)
  ;; (setq x-underline-at-descent-line t)
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
  (setq centaur-tabs-excluded-prefixes
        (append centaur-tabs-excluded-prefixes
               '("*Async-native-compile-log"
                 "*Agda information"
                 "*agda2"
                 "*Quail Completions"
                 "*Native-compile-Log"
                 "*scratch"
                 "*doom"
                 "*Messages"
                 "*Ibuffer"
                 )))

  (map! :map (evil-normal-state-map evil-visual-state-map)
        "ö" #'centaur-tabs-backward
        "ä" #'centaur-tabs-forward
        "Ö" #'centaur-tabs-move-current-tab-to-left
        "Ä" #'centaur-tabs-move-current-tab-to-right
        "C-ö" #'centaur-tabs-backward-group
        "C-ä" #'centaur-tabs-forward-group
  )
)

;; fix weird behavor on SPC f p which requires to type at least two chars
(defun find-file-in-private-config ()
  "Search for a file in `doom-user-dir'."
  (interactive)
  (let ((default-directory (file-truename doom-user-dir)))
    (call-interactively #'find-file)))
(map! :leader "fp" #'find-file-in-private-config)

;; Haskell setup
(after! haskell
 (map!
   :localleader
   :map haskell-mode-map
   (:desc "Check current buffer" "l" #'haskell-process-load-file)
 )
)

;; custom highlighting for todo keywords
(after! hl-todo
  (setq hl-todo-keyword-faces
    (append hl-todo-keyword-faces
        '(("PB-TODO" . "#dc8cc3")
          )))

  (map! :leader
    (:prefix-map ("i" . "insert")
       (:desc "Todo" "t" #'hl-todo-insert)
       ))

  (setq global-hl-todo-mode t)
  )

(add-hook! 'better-jumper-post-jump-hook :append #'recenter-top-bottom)
(add-hook! 'better-jumper-pre-jump-hook  :append #'recenter-top-bottom)

;;;;;; LaTeX
(require 'latex)
(setq-default TeX-master nil) ;; this will make auctex ask me which file is master whenever I open a tex file
(after! latex
  ;; Make evince and ocular known to the LaTeX module
  ;; Use okular to view build pdfs.
  (setq TeX-view-program-selection '((output-pdf "Okular")))
  ;; Use evince to view build pdfs.
  ;; (setq TeX-view-program-selection '((output-pdf "Evince")))
  (electric-indent-local-mode)
  )

(defun demolish-tex-help ()
  (interactive)
  (if (get-buffer "*TeX Help*") ;; Tests if the buffer exists
      (progn ;; Do the following commands in sequence
        (if (get-buffer-window (get-buffer "*TeX Help*")) ;; Tests if the window exists
            (delete-window (get-buffer-window (get-buffer "*TeX Help*")))
          ) ;; That should close the window
        (kill-buffer "*TeX Help*") ;; This should kill the buffer
        )
    )
  )

(setq TeX-process-asynchronous nil
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex
      TeX-PDF-mode t
      )
;; Refresh the PDF buffer after compilation
;; We need the following only if we watch the pdf within emacs.
;; (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(defun run-latexmk ()
  (interactive)
  (let ((TeX-save-query nil)
        (master-file (TeX-master-file nil nil t))
        (master-buffer (current-buffer)))
    (TeX-save-document "")
    ;; (TeX-command-run-all nil)
    (TeX-run-TeX "latexmk"
                 ;; (TeX-command-expand "make")
                 (TeX-command-expand "latexmk -pdflatex='pdflatex --file-line-error --synctex=1 --shell-escape' -pdf %s")
                 master-file)
    ;; FIXME: For the condition, there actually exists the following function but it didnt work for some reason.
    ;;        (TeX-error-report-has-errors-p)
    (if (plist-get TeX-error-report-switches (intern master-file))
        (progn
          (TeX-next-error)
          (minibuffer-message "[ERROR] latexmk exited with errors")
          )
        (progn
          (with-current-buffer master-buffer
            (TeX-view))
          (demolish-tex-help)
          (minibuffer-message "[SUCCESS] latexmk done")
          )
        )
    ))

;; from https://stackoverflow.com/questions/2477195/latex-indentation-formatting-in-emacs
;; to get 2 spaces indentation in evironments
(setq LaTeX-item-indent 0)

;; TODO Activate todo minor mode in latex major mode
;; (hl-todo-mode)

(after! latex
 (map!
  (:map (latex-mode-map LaTeX-mode-map)
   :localleader
   ;; (:desc "Compile" "l" #'TeX-command-run-all)
   (:desc "Compile (Custom)" "l" #'run-latexmk)
   (:desc "Next Error" "e" #'TeX-next-error)
   (:desc "View PDF" "v" #'TeX-view)
   (:desc "Compilation Log" "o" #'TeX-recenter-output-buffer)
   (:desc "Close Environment" "E" #'LaTeX-close-environment)
   ;; (:desc "\\item" "i" #'LaTeX-insert-item)
  )
 )
)

;;; emojify
(setq emojify-display-style 'unicode)

;;; neotree
(defun +neotree/is-focused ()
  "Return t if NeoTree is the active window, nil otherwise."
  (and neo-global--window (eq (selected-window) neo-global--window)))

(defun +neotree/toggle-find-this-file()
  "Runs +neotree/find-this-file except when neotree is in focus.
   Then, this function closes neotree instead."
  (interactive)
  (require 'neotree)
  (if (+neotree/is-focused)
      (neotree-hide)
      (+neotree/find-this-file)))

;; Shortcut to toggle neotree view just with ü
(map! :map 'evil-normal-state-map "ü" #'+neotree/toggle-find-this-file)
(after! neotree
  ;; Make h and l not move the cursor in neotree but instead close or open directories.
  ;; The usual keybinds commented out below won't work well.
  ;; They will only work in insert mode in neotree for some reason.
  ;;   (map! :map 'evil-collection-neotree-maps "h" #'+neotree/collapse-or-up)
  ;;   (map! :map 'evil-collection-neotree-maps "l" #'+neotree/expand-or-open)
  ;; However, this works:
  (evil-collection-define-key 'normal 'neotree-mode-map
    "h" '+neotree/collapse-or-up
    "l" '+neotree/expand-or-open)

  (map! :leader
    (:desc "Focus Neotree" "0" #'neotree)
  )

  ;; hide some files in neotree
  (setq neo-hidden-regexp-list
        (append (list
                ;; latex aux files
                "\\.aux$"
                "\\.fdb_latexmk$"
                "\\.fls$"
                ;; "\\.log$"
                "\\.nav$"
                "\\.out$"
                "\\.snm$"
                "\\.synctex\\.gz$"
                "\\.toc$"
                "\\.vrb$"
                ;; agda build files
                "\\.agdai$"
                )
        neo-hidden-regexp-list))

  (setq-default neo-show-hidden-files nil)
  )

;; (setq x-select-enable-clipboard t)
;; (setq x-select-enable-primary t)
;; (setq select-enable-clipboard t)
;; (setq select-enable-primary t)

;; (defun wsl-copy-clip(&rest _args)
;;   (setq mytemp (make-temp-file "winclip"))
;;   (write-region
;;    (current-kill 0 t)
;;    nil
;;    mytemp
;;    )
;;   (shell-command (concat "clip.exe<" mytemp ))
;;   (delete-file mytemp)
;;    )
;; (advice-add 'kill-new :after #'wsl-copy-clip)

;;;;;; neotree configuration
;; (defun neotree-startup ()
;;   (interactive)
;;   (neotree-show)
;;   (call-interactively 'other-window))

;; (if (daemonp)
;;     (add-hook 'server-switch-hook (lambda () ()));; #'neotree-startup)
;;   (add-hook 'after-init-hook #'neotree-startup)
;; )
;;;;;; treemacs configuration
;; (require 'treemacs)
;; (map! :leader
;;       ("f t" '+treemacs/toggle)

;;       ("s w" 'treemacs-switch-workspace)
;;       ("o w" 'treemacs-switch-workspace)
;;       ("0" 'treemacs-select-window))
;; (treemacs-follow-mode)
;; (treemacs-project-follow-mode)
;; (setq treemacs--project-follow-delay 0.1)
;; (setq treemacs-file-follow-delay 0.1)
;; (setq treemacs-project-follow-cleanup t)
;; (setq treemacs-follow-after-init t)
;; (setq treemacs-width 30)
;; (after! 'treemacs 'treemacs-hide-gitignored-files-mode)

;; make treemacs dirs expand by single click
;; (with-eval-after-load 'treemacs
;;   (progn
;;     (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
;;     (treemacs-hide-gitignored-files-mode)
;;     (setq treemacs-workspace-switch-cleanup 'files)))

;; set the highlight color in treemacs to something I can see
;; (setf treemacs-window-background-color '(nil .
;;                                          ;; "#b48ead"
;;                                          ;; "DarkSlateGray4"
;;                                          ;; "SteelBlue4"
;;                                          ;; "purple4"
;;                                          ;; "tomato4"
;;                                          "aquamarine4"
;;                                          ))

(defun get-current-theme()
  "Return the name of the current theme."
  (car custom-enabled-themes)
  )

(defun get-my-preferred-color-for-agda-inductive-constructors ()
  "Returns a color code as string that I wish to be the color of inductive constructors in Agda
   This fetches the color that is considered to be 'green from the current theme."
  (if (eq (get-current-theme) 'catppuccin)
      (catppuccin-get-color 'green)
      (doom-color 'green)
      )
  )

;; (message "%s = %s" "get-current-theme()" (get-current-theme))
;; (message (concat "my-agda-inductive-constructor-face-color = " (get-my-preferred-color-for-agda-inductive-constructors)))
(custom-set-faces!
  `(font-lock-comment-face :foreground
    "light slate gray")
    ;; "LightSteelBlue4")
    ;; "PaleTurquoise4")
    ;; "LightSkyBlue4")
  `(agda2-highlight-inductive-constructor-face :foreground ,(get-my-preferred-color-for-agda-inductive-constructors))
  )

;; I got the following configuration of rainbow-mode from DistroTube:
;; > Rainbox mode displays the actual color for any hex value color. [...]
;; > The following creates a global minor mode for rainbow-mode and
;; > enables it (exception: org-agenda-mode since rainbow-mode destroys
;; > all highlighting in org-agenda).
;; Hint: This mode caused underlines to appear in the doom dashboard so I
;;       also disabled it there.
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode '+doom-dashboard-mode)))
      (rainbow-mode 1)
      )
  ))
(global-rainbow-mode 1)


;; (after! dired
;;   (add-hook! 'dired-mode-hook
;;     (dired-hide-details-mode))
;;   (add-hook! 'dired-after-readin-hook
;;     (dired-git-info-auto-enable))

;;   (map! :map dired-mode-map
;;     :n "-" #'dired-create-directory
;;     :n "+" #'dired-create-empty-file
;;     :n "DEL" #'dired-up-directory
;;     )
;;   )
(setf dired-kill-when-opening-new-dired-buffer t)
(setq ranger-override-dired 'ranger)
(setq ranger-cleanup-eagerly t)
(setq ranger-show-hidden t)
(setq ranger-preview-file nil)
(setq ranger-dont-show-binary t)
(setq ranger-modify-header t)
(after! ranger
  (map! :map ranger-normal-mode-map
    :prefix "c" (
      "d" #'dired-create-directory
      "f" #'dired-create-empty-file
    )
    :n "DEL" #'dired-up-directory
  )
;;     (setq ranger-override-dired 'ranger)
)
;; (after! ranger
  ;; (map! :map ranger-mode-map
        ;; "q" nil
        ;; ))

;; (require 'dirvish)
;; (dirvish-override-dired-mode)
;; (setq dirvish-attributes
;;       '(vc-state subtree-state collapse)) ;;all-the-icons
;; (setq dirvish-open-with-programs
;;     ;; (concat dirvish-open-with-programs
;;     `((("pdf") . ("evince" "%f"))
;;       ))
;; (map! :map dired-mode-map
;;       :n "TAB" #'dirvish-toggle-subtree
;;       )
;; (setq dired-omit-files (concat dired-omit-files "\\." "\\.\\."))

;;;; At the very last run any additional dynamic config ;;;;
;; This will dynamically run the code in $DOOMDIR/dynamic-args.el
;; on startup and whenever a client connects.
;; Also see my ~/.emacsrc.
(defun my/on-startup (&optional frame)
  "Load my custom emacs lisp arguments on startup."
  (progn
    (if (equal frame nil)
      (message "[Server/Standalone Started] Loading dynamic-args.el")
      (message "[Client Connected] Loading dynamic-args.el"))
    (load! "dynamic-args")
  ))

;; Evaluate immediately for server/standalone emacs.
(my/on-startup)
;; Register a hook for future clients.
(add-hook! 'after-make-frame-functions 'my/on-startup)
