;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
      ;; 'latte)
      'frappe)
      ;; 'macchiato)
      ;; 'mocha)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
(setq doom-theme
  ;;;; Favorites
  ;; 'doom-snazzy
  ;; 'catppuccin
  ;; 'everforest-soft-dark
  ;; 'everforest-hard-light
  ;; 'leuven
  'doom-one-light
  ;; 'doom-vibrant
  ;; 'doom-nova
  ;; 'kaolin-temple
  ;; 'kaolin-valley-light ;; cursor has color with too low contrast here

  ;;;; Other cool themes
  ;; 'everforest-hard-dark
  ;; 'kaolin-breeze
  ;; 'kaolin-galaxy
  ;; 'kaolin-ocean
  ;; 'kaolin-valley-dark ;; The real synthwave
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
  )

(use-package doom-themes
  ;; :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (setq doom-themes-neotree-file-icons t)
  (setq neo-window-fixed-size nil)
  (setq doom-themes-neotree-enable-variable-pitch t)
  ;; (setq doom-themes-neotree-enable-folder-icons nil)
  ;; (setq neo-window-width 40)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (setq nerd-icons-font-family "JetBrainsMonoNL Nerd Font")
;; (after! neotree
;;    (setq neo-theme (if (display-graphic-p) 'icons)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq-default display-line-numbers t)
(setq display-line-numbers-type t)
(global-display-line-numbers-mode)

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Notes/")
(setq org-roam-directory "~/Documents/Notes/Roam")
(setq pb/org-agenda-directory (concat org-directory "Agenda/"))
(setq pb/main-agenda-file (concat pb/org-agenda-directory "Agenda.org"))
(setq org-agenda-files (list pb/org-agenda-directory))
(setq org-icalendar-combined-agenda-file (concat pb/org-agenda-directory "org-agenda-export.ics"))
(setq org-default-todo-file (concat pb/org-agenda-directory "Capture.org"))

(defun pb/open-main-agenda-file ()
  (interactive)
  (find-file pb/main-agenda-file))

(map! :leader (:desc "Agenda" "a" #'pb/open-main-agenda-file))
(after! org
  ;; TODO types
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "INACTIVE(i)" "|" "DONE(d)" "CANCELLED(c)")
          (sequence "|" "LOOP(l)" "🔁(L)") ;; These are reoccuring todos or events and have to determined ending.
          (sequence "[ ](T)" "[/](P)" "[?](W)" "|" "[X](D)")))
  (setq org-todo-keyword-faces
        ;; TODO These faces do not exist in all themes. I can just set these to any color I like (maybe fetch from the particular theme
        '(("NEXT" . +org-todo-active)
          ("WAITING" . +org-todo-onhold)
          ("INACTIVE" . +org-todo-onhold)
          ("[/]" . +org-todo-active)
          ("[?]" . +org-todo-onhold)
          ("CANCELLED" . org-agenda-dimmed-todo-face)))

  ;; Capturing
  (setq org-refile-targets '(("Agenda.org" :maxlevel . 9)))
  (setq org-capture-templates
        '(("t" "todo" entry (file org-default-todo-file)
           "* TODO %?\n - created on %U\n" :clock-resume t :empty-lines 1)
          ;; ("c" "note on current task" entry (clock)
          ;;  "* %?")
          ("n" "note" entry (file org-default-todo-file) ;;(function +default/org-notes-headlines)
           "* Note taken on %U\n%?" :clock-resume t :empty-lines 1)
          ("m" "meeting" entry (file org-default-todo-file)
           "* MEETING with %?\n%U" :clock-resume t :empty-lines 1)
          ))

  ;; Scheduled config
  (setq org-agenda-scheduled-leaders '("🕰" "Sched.%2dx: "))
)

;;; Org CalDav
(setq auth-source-debug 'trivia)
;; Include my secret connection lisp file.
;; It sets the following variables:
;;   org-icalendar-timezone
;;   org-caldav-url
;;   org-caldav-calendar-id
(load! "secrets/org-caldav-connection.el")
(setq org-caldav-inbox (concat pb/org-agenda-directory "Inbox.org") ;; Org filename where new entries from calendar stored
      org-icalendar-include-todo 'all
      org-caldav-sync-todo t)
;; This ensures all org "deadlines" show up, and show up as due dates
(setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
;; This ensures "scheduled" org items show up, and show up as start times
(setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
(setq org-icalendar-deadline-summary-prefix "[Deadline] ")
(setq org-icalendar-scheduled-summary-prefix "") ;; Do not put "S: " in front of every entry with a SCHEDULED.
(setq org-caldav-todo-percent-states
      '((0 "TODO") (0 "NEXT") (0 "WAITING") (0 "INACTIVE") (100 "DONE") (100 "CANCELLED")
        (100 "LOOP") (100 "🔁")
        (0 "[ ]") (50 "[/]") (0 "[?]") (100 "[X]")
      ))

;;;###autoload
(defun pb/set-org-caldav-files-to (filelist)
  (message "[SYNC] set org-caldav-files to %s" filelist)
  (setq org-caldav-files filelist))

;;;###autoload
(defun pb/org-caldav-files-full ()
  "Returns a list of all my org files relevant
for calendar synchronization.
The result value is meant to be used to set org-caldav-files to."
  ;; gather all files in my org directory, then remove ignored files
   (let ((org-files-for-caldav-sync (directory-files-recursively pb/org-agenda-directory ".*\\.org" t t)))
     (delete org-default-todo-file org-files-for-caldav-sync) ;; remove captured todos from sync
     org-files-for-caldav-sync))

;;;###autoload
(defun pb/org-caldav-files-minimum ()
  "Returns a list of just the most recent / central
of my org files for calendar synchronization. The idea
is to speed up syncing by not syncing old, already synced
entries again, but just push the most recent changes.
The result value is meant to be used to set org-caldav-files to."
  (list
    pb/main-agenda-file
    (concat pb/org-agenda-directory "Events.org")
    ))

;;;###autoload
(defun pb/sync-org-agenda-to-calendar ()
  "Synchronizes the org entries from org-caldav-files to my calendar.
   Wraps org-caldav-sync with updating some
   internal state, and closing any org buffers
   opened by org-caldav-sync."
  ;; remember currently opened org buffers
  (let ((previously-open-org-buffers (doom-matching-buffers "\\.org"))
        )
    (message "[SYNC] Starting org-caldav sync")
    (message "[SYNC] Currently open buffers are: %s" previously-open-org-buffers)
    ;; run the actual syncing, catch any errors and store them in the "sync-error" variable
    (let ((sync-error
      (condition-case
         err
         (org-caldav-sync)
         (error err)
         )))
      ;; close all newly opened org buffers
      (message "[SYNC] Finished org-caldav sync. Killing new buffers...")
      (let* ((afterward-open-org-buffers (doom-matching-buffers "\\.org"))
             (new-org-buffers (seq-difference afterward-open-org-buffers previously-open-org-buffers))
             )
        (message "[SYNC] Killing buffers: %s" new-org-buffers)
        (dolist (e new-org-buffers)
          (save-buffer e)
          (kill-buffer e)
          )
        )
      ;; report any errors that occurred during sync
      (when sync-error
        (message "[SYNC] %s" sync-error)
        )
      )
    )
  )

;;;###autoload
(defun pb/with-org-caldav-files (filelist body)
  "Temporarily sets org-caldav-files to FILELIST,
invokes BODY and then resets org-caldav-files to
its previous value."
  (require 'org-caldav)
  (let ((previous-org-caldav-files org-caldav-files))
    (pb/set-org-caldav-files-to filelist)
    (funcall body)
    (pb/set-org-caldav-files-to previous-org-caldav-files)
    ))

;;;###autoload
(defun pb/sync-org-agenda-to-calendar-full ()
  "Syncs all my org entries to the calendar."
  (interactive)
  (pb/with-org-caldav-files (pb/org-caldav-files-full)
    'pb/sync-org-agenda-to-calendar))

;;;###autoload
(defun pb/sync-org-agenda-to-calendar-minimum ()
  "Syncs only the most recent org entries to my calendar.
Recentness is determined by being in my Agenda.org file or in my Events.org."
  (interactive)
  (require 'org-caldav)
  (let ((old-value org-caldav-delete-calendar-entries))
    (setq org-caldav-delete-calendar-entries 'never)
    (pb/with-org-caldav-files (pb/org-caldav-files-minimum)
      'pb/sync-org-agenda-to-calendar)
    (setq org-caldav-delete-calendar-entries old-value)
    ))

;;;###autoload
(defun pb/sync-org-agenda-to-calendar-anew (boolSure)
  "Deletes the calendar entries and starts a new sync."
  (interactive (list (y-or-n-p "Are you sure to reset and restart calendar sync?")))
  (if boolSure
      (progn
        (require 'org-caldav)
        (pb/with-org-caldav-files (pb/org-caldav-files-full)
          (lambda ()
            (org-caldav-delete-everything "prefix")
            (pb/sync-org-agenda-to-calendar))))
      (message "Ok, I did nothing.")))

(after! org-caldav
  (setq org-caldav-delete-calendar-entries 'ask)
  (pb/set-org-caldav-files-to (pb/org-caldav-files-full))
  )

;; (defun sync-org-agenda-to-calendar-at-close ()
;;   (sync-org-agenda-to-calendar)
;;   (save-some-buffers))
;; (add-hook 'kill-emacs-hook 'sync-org-agenda-to-calendar-at-close)

;; Yas
;; Chat-GPT can help here.
;; (after! yasnippet
;;   (setq yas-global-mode t)
;;   ;; disable the C-s default binding
;;   (map! :map (org-mode-map evil-insert-state-map global-map)
;;         "C-i" nil)
;;   (map! :map yas-minor-mode-map
;;         "C-i" #'yas-expand)
;;   (map! :map yas-keymap
;;         "C-i" #'yas-next-field-or-maybe-expand))

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

;;;; Some miscellaneous global settings

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
(setq which-key-idle-delay 0.5)

(add-hook! 'better-jumper-post-jump-hook :append #'recenter-top-bottom)
(add-hook! 'better-jumper-pre-jump-hook  :append #'recenter-top-bottom)
(map! "C-i" 'better-jumper-jump-forward)

;;;; Customize themes

(defun get-theme-color-green (theme)
  "This fetches the color that is considered to be green
   from the theme and returns it as a string."
  (cl-case theme
    (catppuccin (catppuccin-get-color 'green))
    (everforest-soft-dark  (everforest-soft-dark-with-color-variables  everforest-soft-dark-green))
    (everforest-hard-dark  (everforest-hard-dark-with-color-variables  everforest-hard-dark-green))
    (everforest-hard-light (everforest-hard-light-with-color-variables everforest-hard-light-cyan))
    (t (doom-color 'green))
    )
  )

(defun get-theme-color-red (theme)
  "This fetches the color that is considered to be red
   from the theme and returns it as a string."
  (cl-case theme
    (catppuccin (catppuccin-get-color 'red))
    (everforest-soft-dark  (everforest-soft-dark-with-color-variables  everforest-soft-dark-red))
    (everforest-hard-dark  (everforest-hard-dark-with-color-variables  everforest-hard-dark-red))
    (everforest-hard-light (everforest-hard-light-with-color-variables everforest-hard-light-red))
    (t (doom-color 'red))
    )
  )

(defun get-current-theme()
  "Return the name of the current theme."
  (car custom-enabled-themes)
  )

(defun customize-current-theme ()
  "This function modifies the current theme to my liking. :)"
  (interactive)
  (let ((theme (get-current-theme)))
    (pcase theme
      ('doom-one
        (custom-theme-set-faces! 'doom-one
          '(font-lock-comment-face :foreground
             "light slate gray")
             ;; "LightSteelBlue4")
             ;; "PaleTurquoise4")
             ;; "LightSkyBlue4")
          )
        )
      ;; The default everforest highlight colors are the same as for visual mode and that is annoying
      ('everforest-hard-dark
        (custom-theme-set-faces! 'everforest-hard-dark
          (list 'tab-line :inherit)
          (list 'region :background (everforest-hard-dark-with-color-variables everforest-hard-dark-border))))
      ('everforest-hard-light
        (custom-theme-set-faces! 'everforest-hard-light
          (list 'region :background (everforest-hard-light-with-color-variables everforest-hard-light-gutter))))
      )
    ;; define some faces I would like to have consistent across all themes.
    (let ((green (get-theme-color-green theme))
          (red   (get-theme-color-red   theme)))
      ;; (message "!!!!!!! Green: %s, Red: %s, Theme: %s" green red theme)
      (custom-set-faces!
        (list 'agda2-highlight-inductive-constructor-face :foreground green)
        (list 'git-commit-overlong-summary :foreground red)))
  )
)

(defadvice! customize-existing-themes (theme &rest _)
  :after 'load-theme
  (customize-current-theme))

(customize-current-theme)

;;; :ui modeline
;; An evil mode indicator is redundant with cursor shape
(setq doom-modeline-modal nil)
(setq doom-modeline-project-name t)

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;; :tools lsp
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil
        ;; If an LSP server isn't present when I start a prog-mode buffer, you
        ;; don't need to tell me. I know. On some machines I don't care to have
        ;; a whole development environment for some ecosystems.
        lsp-enable-suggest-server-download nil
        lsp-modeline-diagnostics-enable nil ;; disables summary of issues next to a light bulb in the modeline
        lsp-response-timeout 1))

;;;; Configure projectile

(after! projectile
  (setq projectile-enable-caching t)
  ;; 'alien is the fastest but it does not respect any sorting.
  ;; So we do not set any sorting.
  (setq projectile-indexing-method 'alien
        ;; Default value: "git ls-files -zco --exclude-standard"
        ;; Set it to the following to ignore .gitignore entries (i.e., list _all_ files).
        projectile-git-command "git ls-files -zco"
        )

  ;; Sometimes, I want to see all files.
  ;; Whereas the 'alien sort oder exludes all the files from .gitignore
  ;; this will ignore only files ignored by .projectile (I think).
  ;; It definitely ignores .gitignore. ;)
  ;; (setq projectile-indexing-method 'native
  ;;       projectile-sort-order 'recently-active
  ;;       )
  )

;;;; Various (Global) Keybindings

(setq evil-snipe-override-evil-repeat-keys nil)
;; Make "," be the local leader instead of SPC-m
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-SPC ,")

(map! "C-s" 'save-buffer)

(defun comment-eclipse ()
    "Eclipse IDE style multi-line comment toggling
     for the selected region."
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

;; https://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
;; Maybe try to extend these functions to check whether a region is active and instead move
;; the lines of the selected regions.
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

;; These are analogous keybindings as for org.
(map! :n "M-k" #'move-line-up)
(map! :n "M-j" #'move-line-down)

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
        ;; Replace snipe by avy-goto-char:
        ;; I like snipe but it only jumps within a single line which is useless to me often.
        "s" #'evil-avy-goto-char
        "S" #'evil-avy-goto-char-timer
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

;; hyprctl notify 1 2000 0 'begin emacs-sync' ;
;; hyprctl notify 1 2000 0 'emacs-sync done' ;
(defun doom-reload-and-restart-server ()
  "1. Kills all emacs clients and servers.
   2. Runs doom sync
   3. Restarts the emacs server.
   4. Starts a new emacs client.
   Beware: This will kill your current client/emacs!"
  (interactive)
  (start-process "doom reload and restart server" nil "setsid" "zsh" "-c"
                 "source ~/.zshrc ;
                  kitty --hold --session launch-emacs-sync.kitty ;
                  emacsclient
                  ")
  )

(defun goto-doom-config-file ()
  "Opens my doom config.
   There was a function for this in DOOM but apparently it's gone.
   So I implemented it myself here."
  (interactive)
  (find-file (concat doom-user-dir doom-module-config-file)))

(defun goto-doom-init-file ()
  "Opens my doom init.el."
  (interactive)
  (find-file (concat doom-user-dir doom-module-init-file)))

;; Build my own menu.
;; (SPC-d was free so I called it "Doom" and then just put a lot of custom global commands in there.)
(map! :leader
      (:prefix ("d" . "Doom (Custom)")
               (:desc "Ranger" "." #'open-ranger-here)
               (:desc "Paste from kill-ring" "p" #'consult-yank-pop)
               (:desc "Reload" "r" #'doom/reload)
               (:desc "Reload and restart server" "R" #'doom-reload-and-restart-server)
               (:desc "config.el" "c" #'goto-doom-config-file)
               (:desc "init.el" "i" #'goto-doom-init-file)
               ;; (:desc "Calendar" "C" #'calendar)
               (:desc "Help Search" "h" #'doom/help-search)
               (:prefix ("a" . "Agenda")
                        (:desc "open agenda file" "a" #'pb/open-main-agenda-file)
                        (:desc "open agenda" "A" #'org-agenda-list)
                        (:desc "export to file" "e" #'org-icalendar-combine-agenda-files)
                        (:desc "sync " "s" #'pb/sync-org-agenda-to-calendar-minimum)
                        (:prefix ("S" . "sync (special)")
                                 (:desc "sync full" "f" #'pb/sync-org-agenda-to-calendar-full)
                                 (:desc "reset and sync anew" "r" #'pb/sync-org-agenda-to-calendar-anew)
                                 )
                        )
               (:prefix ("A" . "Agda")
                        (:desc "switch to global Agda (default)" "g" #'use-global-agda)
                        (:desc "switch to nix Agda" "n" #'use-nix-agda)
                        )
               (:prefix ("g" . "Dotfiles Git")
                 (:desc "status" "g" #'dotfiles-status)
                 (:desc "stage current buffer's file" "s" #'dotfiles-stage-buffer-file)
                 )
               (:prefix ("n" . "NixOS")
                 (:desc "rebuild + switch" "r" #'nixos-rebuild-switch)
                 (:desc "config" "c" #'goto-nix-config-file)
                 )
               (:desc "toggle input method" "t" #'toggle-input-method)
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
(map! :leader
      (:desc "Make" "m" #'+make/run)
      )
(map! :leader
      :prefix "c"
      (:desc "Compile project" "c" #'project-compile)
      (:desc "Compile (here)" "L" #'compile)
      (:desc "Recompile (here)" "l" #'recompile)
      (:desc "Make" "m" #'+make/run)
      )

;;;; Org Roam

(defun pb/org-roam-open-home-page ()
  "Open my org-roam home page."
  (interactive)
  (use-package! org-roam)
  (let ((node (org-roam-node-from-title-or-alias "Home")))
    (if node
      (org-roam-node-visit node)
      (user-error "There is no org roam node called \"Home\"!"))))

(map! :leader
      :prefix "n"
      (:desc "Find note" "f" #'org-roam-node-find) ;; "f" for "find"
      (:desc "Insert link to a note here" "i" #'org-roam-node-insert) ;; "i" for "insert"
      (:desc "New note" "n" #'org-roam-capture) ;; "n" for "note / new / new note"
      (:desc "Promote heading to node" "p" #'org-id-get-create) ;; "p" for "promote heading to node"
      (:desc "Open home page" "h" 'pb/org-roam-open-home-page) ;; "h" for "home"
      )
(map! :map doom-leader-insert-map "n" #'org-roam-node-insert) ;; "SPC i n" for "Insert Note"

;;;; Doom main directory

(defun pb/doom-set-current-directory (dir)
  (interactive "D")
  (setq +doom-dashboard-pwd-policy dir)
  (+doom-dashboard-reload)
  )

;;;; Modify Splashscreen

(defun get-random-splashscreen-file ()
  "Returns a random image from the available splash screens within the `splashes'
   directory in the doom private configuration directory.
   The image is returned as a string containing an absolute path to that file.
   Returns nil if there are no image files in the `splashes' directory."
  (let* ((splash-dir (concat doom-private-dir (file-name-as-directory "splashes")))
         (image-file-names (directory-files splash-dir nil ".*\\.png")) ;; Currently, this only checks for PNGs. We might also want to allow other image types in the future.
         (num-available-splashscreens (length image-file-names)))
    (if (eq num-available-splashscreens 0)
        nil
        (concat splash-dir (nth (random num-available-splashscreens) image-file-names)))))

(let ((splash-screen (get-random-splashscreen-file)))
  (when splash-screen
    (setq fancy-splash-image splash-screen)))

;; hide key recommendations
;; TODO: Customize what is shown.

;;;; Open with
;; open pdfs with evince

(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "evince" (file))))

;;;; Miscellaneous fixes or overrides

;; Unbind C-z which enters emacs-state and I am always confused when this accidentally happens
;; Apparently, I have no problem exiting Vim but Emacs!
(map! :nv "C-z" nil)

;; Bindings for font resizing
(map! :nv "C-+" #'text-scale-increase)
(map! :nv "C--" #'text-scale-decrease)
(map! :nv "C-0" #'doom/reset-font-size)

;; Code folding
(map! :nv "<backtab>" #'+fold/close)

(after! magit (map! :map magit-mode-map "C-z") nil)

;; fix weird behavor on SPC f p which requires to type at least two chars
(defun find-file-in-private-config ()
  "Search for a file in `doom-user-dir'."
  (interactive)
  (let ((default-directory (file-truename doom-user-dir)))
    (call-interactively #'find-file)))
(map! :leader "fp" #'find-file-in-private-config)

;;;; opening external programs
;; We use "setsid" for opening external programs such that
;; the new processes are not spawned as child processes of emacs.
;; This makes the processes stay alive when we exit emacs / the emacs client.
;; Otherwise, when we would close emacs / the emacs client, we would also
;; kill all child processes, and hence any terminals or explorers we spawned.

(defun open-terminal (args)
  "Open a terminal with the given arguments"
  (message (concat "setsid " "kitty --session launch-zsh.kitty " args))
  (start-process "terminal-from-emacs" nil "setsid" "kitty" "--session" "launch-zsh.kitty" args)
  )

(defun open-terminal-here ()
  "Open a terminal at the current directory"
  (interactive)
  (open-terminal ".")
  )

(defun open-dolphin-here ()
  "Open a terminal at the current directory"
  (interactive)
  (message "setsid nautilus .")
  (start-process "dolphin-from-emacs" nil "setsid" "nautilus" ".")
  )

(defun open-ranger-at (dir-string)
  "Open the file explorer with the given arguments"
  (message (concat "setsid " "kitty --hold --session launch-ranger.kitty " dir-string))
  (start-process "explorer-from-emacs" nil "setsid" "kitty" "--hold" "--session" "launch-ranger.kitty" dir-string)
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

;;;; Agda setup

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
;; (setq font-lock-global-modes (not 'agda2-mode))
;; (setq font-lock-global-modes nil)
;; (global-font-lock-mode 0)
;; (add-hook! font-lock-mode-hook
;;           (message "font-lock-mode-hode activated"))

;; nix setup for agda
(load! "nix-shell.el")

(defun customize-agda ()
  (setq default-input-method "Agda")
  (add-hook! 'agda2-mode-hook
    (setq-local evil-shift-width 2)
    (activate-input-method "Agda"))
  (prependq! auto-mode-alist '(("\\.agda\\'" . agda2-mode)))
  (prependq! auto-mode-alist '(("\\.lagda.md\\'" . agda2-mode)))
  (prependq! auto-mode-alist '(("\\.lagda.tex\\'" . agda2-mode)))
  ;; Make evil repeat (pressing .) ignore agda2-load.
  (evil-add-command-properties 'agda2-load :repeat 'ignore)
  ;; Add agda holes as evil-surround braces
  (after! evil-surround
    (embrace-add-pair ?! "{!" "!}"))

  (setq agda-input-user-translations `(
    ("ot"   . ("←"))
    ("==>"  . ("⇛"))
    ("nat"  . ("ℕ"))
    ("o="   . ("≗")) ;; I always misstype this.
    ("pand"   . ("∧"))
    ("por"   . ("∨"))
    ("pimplies"   . ("⇒"))
    ("pequiv"   . ("⇔"))

    ;; ("king" . ("♚" "♔"))
    ;; ("queen" . ("♛" "♕"))
    ;; ("rook" . ("♜" "♖"))
    ;; ("bishop" . ("♝" "♗"))
    ;; ("knight" . ("♞" "♘"))
    ;; ("pawn" . ("♟" "♙"))

    ;; ("kingb" . ("♚"))
    ;; ("queenb" . ("♛"))
    ;; ("rookb" . ("♜"))
    ;; ("bishopb" . ("♝"))
    ;; ("knightb" . ("♞"))
    ;; ("pawnb" . ("♟"))

    ;; ("kingw" . ("♔"))
    ;; ("queenw" . ("♕"))
    ;; ("rookw" . ("♖"))
    ;; ("bishopw" . ("♗"))
    ;; ("knightw" . ("♘"))
    ;; ("pawnw" . ("♙"))
  ))
)

(defvar *current-agda-version* 'global
  "Arbitrary identifier for the currently loaded Agda version.
This is only used to skip `switch-agda-version' in case `*current-agda-version*'
wouldn't change.")

(defun switch-agda-version (version-identifier)
  (message "[Agda] Trying to switch to Agda version: %s" version-identifier)
  (if (equal version-identifier *current-agda-version*)
      (message "[Agda] Skipping the Agda version switch as the versions are the same.")
      (if (featurep 'agda2)
          (agda2-set-program-version nil)
          ;; First time loading Agda
          (add-load-path!
            (file-name-directory (shell-command-to-string "agda-mode locate")))
          (if (require 'agda2 nil t)
              (normal-mode)
              (error "Failed to find the `agda2' package"))
          (normal-mode)
          (customize-agda)
          (customize-current-theme)
          )
      (setq *current-agda-version* version-identifier)
      (message "[Agda] Switched to agda version %s" *current-agda-version*)
      )
  )

(defun use-global-agda ()
  (interactive)
  (nix-shell-deactivate)
  (switch-agda-version 'global))

(defun use-nix-agda (nix-shell-path)
  (interactive (list (nix-shell-read-path "nix expression path: ")))
  (nix-shell-activate nix-shell-path)
  (switch-agda-version (list 'nix nix-shell-path)))

;; (defun use-agda ()
;;   (interactive)
;;   (let ((nix-file (nix-shell-locate-expression)))
;;     (if nix-file
;;         (use-nix-agda nix-file)
;;       (use-global-agda))))

;; (add-hook 'projectile-after-switch-project-hook 'use-agda)

;; Can't use `use-package!' because it will try to load agda2-mode immediately.
;; (load! "modules/lang/agda/config.el" doom-emacs-dir)
(after! agda2
  (customize-agda))

;;;; Magit for my Dotfiles

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
  (interactive)
  (setq magit-git-global-arguments (remove dotfiles-git-dir magit-git-global-arguments))
  (setq magit-git-global-arguments (remove dotfiles-work-tree magit-git-global-arguments))
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

;; NixOS

(defun goto-nix-config-file ()
  "Opens my nix config."
  (interactive)
  (find-file "~/nix/configuration.nix"))


(defun nixos-rebuild-switch ()
  "Opens a terminal, runs
   \='nix-os rebuild switch\=' (requires sudo)
   and shows the result."
  (interactive)
  (start-process "nixos rebuild from within doom" nil
                 "setsid" "kitty" "--hold" "--session" "launch-nix-rebuild.kitty"
                 ))

;; Haskell

(after! haskell
 (map!
   :localleader
   :map haskell-mode-map
   (:desc "Check current buffer" "l" #'haskell-process-load-file)
 )
)

;;;; Markdown

(after! markdown-mode
  (setq markdown-command "pandoc --standalone --embed-resource -c ~/.config/pandoc/github-markdown.css -f gfm+tex_math_dollars -t html5")
  (map! :map markdown-mode-map
        :localleader
        "l" #'markdown-preview)) ;; also bind preview to "<localleader> l" just because I am so used to that key.

;;;; Java
(add-hook! 'java-mode-hook
  (setq c-basic-offset 4)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'statement-cont '+)
  (c-set-offset 'case-label '+)
  )

(after! lsp-java
  :config
  (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms100m"))
  (setq lsp-java-jdt-ls-prefer-native-command t)
  (setq lsp-java-server-install-dir (f-dirname (f-dirname (executable-find lsp-java-jdt-ls-command))))
  ;; bugfix: Wait for https://github.com/emacs-lsp/lsp-java/pull/497 to be accepted.
  (defadvice! lsp-java--locate-server-jar-nixos-fix ()
    :override 'lsp-java--locate-server-jar
    t)

  (map! :map java-mode-map :localleader
     (:desc "LSP code action" "a" #'lsp-execute-code-action)
     )
  ;; ;; IntelliJ like binding
  ;; (map! :map java-mode-map
  ;;    (:desc "LSP code action" "C-RET" #'lsp-execute-code-action)
  ;;    )
  )

;;;; Centaur tabs

(after! centaur-tabs
  ;; (setq centaur-tabs-style "wave")
  (setq centaur-tabs-set-bar nil)
  ;; (setq x-underline-at-descent-line t)
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
  ;; (setq centaur-tabs-excluded-prefixes
  ;;       (append centaur-tabs-excluded-prefixes '(
  ;;                "*doom"
  ;;                "*org-roam"
  ;;                )))

  ;; overwriting this functions is recommended by the centaur-tabs readme.
  ;; It is just a copy of the function as implemented in the package, extended by a few new
  ;; string-prefix-p filters. This is such a bad customization design, omg.
  (defun centaur-tabs-hide-tab (x)
    "Customized version of ~centaur-tabs-hide-tab~.
     This functions returns true if the buffer X should not be a tab."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))
  
       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*mybuf" name)
  
       ;; custom additions
       (string-prefix-p "*doom" name)
       (string-prefix-p "*org-roam" name)
       (string-prefix-p "*Messages" name)
       (string-prefix-p "*Native-compile-log" name)
       (string-prefix-p "*scratch" name)
  
       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       )))

  (defun my/fix-centaur-tabs-get-tabsets-tabset (groups)
    ;; (when (eq centaur-tabs-cycle-scope 'groups)
      (set groups (cl-remove-if
                   (lambda (group)
                     ;; group has type "Pair Buffer GroupName". Example: (*Async-native-compile-log* . Emacs)
                     (string-prefix-p "*" (buffer-name (car group)))
                     )
                   (symbol-value groups)))
      ;; )
    ;; (message "%s" (symbol-value groups))
    groups
    )

  (advice-add 'centaur-tabs-get-tabsets-tabset :filter-return #'my/fix-centaur-tabs-get-tabsets-tabset)

  (map! :map (evil-normal-state-map evil-visual-state-map)
        "ö" #'centaur-tabs-backward
        "ä" #'centaur-tabs-forward
        "Ö" #'centaur-tabs-move-current-tab-to-left
        "Ä" #'centaur-tabs-move-current-tab-to-right
        "C-ö" #'centaur-tabs-backward-group
        "C-ä" #'centaur-tabs-forward-group
        "C-ü" #'centaur-tabs-ace-jump
  )
)

;;;; Custom Highlighting for Todo Keywords

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

;;;; LaTeX

;; TODO: We do not have to always load latex?
;; (require 'latex)
(add-to-list 'auto-mode-alist '("\\.tikz\\'" . latex-mode))
(setq-default TeX-master nil) ;; this will make auctex ask me which file is master whenever I open a tex file
(after! latex
  ;; Make evince and ocular known to the LaTeX module
  ;; Use okular to view build pdfs.
  ;; (setq TeX-view-program-selection '((output-pdf "Okular")))
  ;; Use evince to view build pdfs.
  (setq TeX-view-program-selection '((output-pdf "Evince")))
  (setq TeX-newline-function #'electric-indent-just-newline)
  ;; (electric-indent-local-mode)
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
                 (TeX-command-expand "latexmk -pdflatex='pdflatex --file-line-error --synctex=1 --shell-escape' -pdf -interaction=nonstopmode %s")
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
   (:desc "Compile (latexmk)" "L" #'run-latexmk)
   ;; (:desc "Recompile" "l" #'recompile)
   (:desc "Compile (Make)" "l" #'+make/run)
   ;; (:desc "Compile" "L" #'compile)
   ;; (:desc "Compile" "L" #'+make/run)
   (:desc "Set Master File" "a" #'TeX-master-file-ask)
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
   Otherwise closes neotree instead."
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
                "\\.acn$"
                "\\.blg$"
                "\\.glo$"
                "\\.ist$"
                "\\.lof$"
                "\\.lot$"
                "\\.nto$"
                "\\.slnc$"
                "\\.tcc0$"
                "\\.tcc1$"
                "\\.tcc2$"
                "\\.tcc3$"
                "\\.tcc4$"
                "\\.tcc5$"
                "\\.wrt$"
                "\\.bcf$"
                "\\.synctex(busy)$"
                ;; agda build files
                "\\.agdai$"
                )
        neo-hidden-regexp-list))

  (setq-default neo-show-hidden-files nil)
  )

;;;; Rainbow Mode

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

;;;; Dired and Ranger

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
