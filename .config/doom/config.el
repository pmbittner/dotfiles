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
(setq doom-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size font-size)
      doom-variable-pitch-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size font-size)
      doom-big-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size big-font-size)
      ;; Do not set size of unicode font or it wont scale on zoom.
      doom-symbol-font (font-spec :family "DejaVu Sans")) ;; doom-unicode-font

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
        'doom-one
        ;; 'adwaita
        ;; 'whiteboard
        ;; 'doom-material
        ;; 'doom-palenight
        ;; 'doom-challenger-deep
        ;; 'doom-snazzy
        ;; 'doom-vibrant

        ;;;; Light themes
        ;; 'default
        ;; 'leuven
        ;; 'modus-operandi

        ;;;; Synthwave Themes
        ;; 'doom-outrun-electric
        ;; 'doom-laserwave
        ;; 'doom-shades-of-purple

        ;;;; Other themes
        ;; 'doom-sourcerer -- looks like Loop Hero
        ;; 'doom-zenburn
        ;; 'doom-rouge
        ;; 'doom-lantern
        ;; 'doom-peacock
        ;; 'doom-old-hope
        ;; 'doom-monokai-pro
        ;; 'doom-monokai-machine
        ;; 'doom-monokai-classic
        ;; 'doom-monokai-octagon
        ;; 'doom-monokai-ristretto
        ;; 'doom-monokai-spectrum
        ;; 'doom-homage-white
        ;; 'doom-henna
        ;; 'doom-xcode
        ;; 'doom-wilmersdorf
        ;; 'doom-ephemeral
        ;; 'doom-1337
        ;; 'doom-nova
        ;; 'doom-city-lights
        ;; 'doom-tokyo-night
        ;; 'doom-ayu-dark
        t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (setq doom-themes-neotree-file-icons t)
  ;; (setq neo-window-fixed-size nil)
  (setq neo-window-width 40)
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

;; some keybindings for faster interaction with doom
(map! :leader
      (:prefix ("d" . "Doom (Custom)")
               (:desc "Reload" "r" #'doom/reload)
               )
      )

(map! :leader
      :prefix "c"
      (:desc "Compile project" "c" #'project-compile)
      (:desc "Compile (here)" "l" #'compile)
      (:desc "Make" "m" #'+make/run)
      )

(defun open-explorer (args)
  "Open the file explorer with the given arguments"
  (message args)
  (start-process "explorer-from-emacs" nil "explorer.exe" args)
  )

(defun open-explorer-here ()
  "Open the file explorer at the current directory"
  (interactive)
  (open-explorer ".")
  )

(map! :leader
      (:prefix "o"
               (:desc "Explorer here" "e" #'open-explorer-here)
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
                    (end-of-line)
                    (point)))
        )
      (comment-or-uncomment-region start end)
      (setq deactivate-mark nil) ;; keep the region selected afterwards
      ))

(map! :n "C-t" #'comment-eclipse)

;;;;;; Agda setup
;; workaround for a bug in evil: https://github.com/emacs-evil/evil/pull/1768
;; The workaround as proposed here: https://github.com/agda/agda/issues/2141
(defun set-agda-input-method ()
  (evil-without-input-method-hooks ;; Disable evil's hooks which reset current-input-method in favor of evil-input-method before the evil minor mode has been loaded.
    (set-input-method "Agda")))
(add-hook 'agda2-mode-hook 'set-agda-input-method)

;; auto-load agda-mode for .agda and .lagda.md
(setq auto-mode-alist
      (append
       '(("\\.agda\\'" . agda2-mode)
         ("\\.lagda.md\\'" . agda2-mode))
       auto-mode-alist))

;; Haskell setup
(after! haskell
 (map!
   :localleader
   :map haskell-mode-map
   (:desc "Check current buffer" "l" #'haskell-process-load-file)
 )
)

;;;;;; LaTeX
(require 'latex)
(setq-default TeX-master nil) ;; this will make auctex ask me which file is master whenever I open a tex file
;; Use evince to view build pdfs.
(after! latex
	(setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
	(setq TeX-view-program-selection '((output-pdf "Evince"))))

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

(defun run-latexmk ()
  (interactive)
  (let ((TeX-save-query nil)
        (TeX-process-asynchronous nil)
        (master-file (TeX-master-file nil nil t))
        (master-buffer (current-buffer)))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk"
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


(map! :leader
      (:desc "Focus Neotree" "0" #'neotree)
      )

;; hide some files in neotree
(setq neo-hidden-regexp-list
  (append
    (list
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

(setq projectile-indexing-method 'hybrid)
(setq projectile-sort-order 'recentf)
(after! projectile
  (progn
    (message "Set projectile sort order")
  ))

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
