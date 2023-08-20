;; Initialize use-package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defvar doom-private-dir (concat (getenv "HOME") "/.config/doom"))
(load-file (expand-file-name "elisp/variables.el" doom-private-dir))
(load-file (expand-file-name "elisp/theme.el" doom-private-dir))

(defvar rz/default-font-size 110)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "Iosevka" :height rz/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height rz/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Overpass" :height 120 :weight 'regular)

(setq display-time-24hr-format t)

(use-package autothemer
  :straight t)

(use-package catppuccin-theme
  :straight (:host github
             :repo "catppuccin/emacs"
             :files ("*.el")))
            ;;:files ("catppuccin-frappe-theme.el" "catppuccin-latte-theme.el" "catppuccin-mocha-theme.el")))

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package mixed-pitch
  :straight t
  :config
  ;; If you want it in all text modes:
  (add-hook 'text-mode-hook 'mixed-pitch-mode))

(use-package evil
  :straight t
  :config
  (evil-mode))

(use-package evil-escape
  :straight t
  :after evil
  :config
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode))

(defun rz/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org Mode Configuration ------------------------------------------------------

(defun rz/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Overpass" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :straight (:type built-in)
  :hook (org-mode . rz/org-mode-setup)
  :config
  (setq org-ellipsis " ⤵"
        org-startup-indented t
        org-hide-emphasis-markers t
        org-directory org_file_dir
        org-priority-lowest ?D
        org-pretty-entities t
        org-use-property-inheritance t
        org-log-done 'time                          ; having the time a item is done sounds convenient
        org-export-in-background t                  ; run export processes in external emacs process
        org-catch-invisible-edits 'smart            ; try not to accidently do weird stuff in invisible regions
        org-export-with-sub-superscripts '{})
  (rz/org-font-setup))      ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}

(defun rz/org-journal-save-entry-and-exit()
      (interactive)
      (save-buffer)
      (kill-buffer-and-window))

(use-package org-journal
  :straight t
  :config
  (setq org-journal-carryover-items ""
        org-journal-enable-cache t
        org-journal-dir org_journal
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-prefix "#+DATE: "
        org-journal-date-format "%A, %d %B %Y"
        org-extend-today-until 4)

  (define-key org-journal-mode-map (kbd "C-c C-c") 'rz/org-journal-save-entry-and-exit))

(defun rz/org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

(with-eval-after-load 'org
      (setq org-capture-templates
          ;; Tasks
        `(("t" "Tasks / Projects")
          ("tt" "Today" entry (file+olp ,(concat org_agenda_dir "next.org") "Today's Tasks")
           "* NEXT %?\nSCHEDULED:%^T  %U\n  %a\n  %i" :empty-lines 1)
          ("td" "Today Deadline" entry (file ,(concat org_agenda_dir "next.org") "Today's Tasks")
           "* TODO %? \nDEADLINE: %^T\n %U" :empty-lines 1)
          ("ts" "Inbox Scheduled" entry (file+olp ,(concat org_agenda_dir "inbox.org") "Tasks")
           "* TODO %?\nSCHEDULED:%^T  %U\n  %a\n  %i" :empty-lines 1)
          ("tD" "Inbox Deadline" entry (file ,(concat org_agenda_dir "inbox.org") "Tasks")
           "* TODO %? \nDEADLINE: %^T\n %U" :empty-lines 1)
          ("tw" "Wait deadline" entry (file+olp ,(concat org_agenda_dir "waiting.org") "Waiting For")
           "* WAIT %? From _%^{Delegated To}_ \nDEADLINE: %^T\n %U\n %a" :empty-lines 1)

          ;; Catchall for faster capture "SPC-x-x"
          ("x" "Inbox" entry (file+olp ,(concat org_agenda_dir "inbox.org") "Everything/Notes")
           "* %? \n %U\n %a" :empty-lines 1)

          ;; Events
          ("e" "Event" entry (file ,(concat org_agenda_dir "gcal/events.org"))
           "* %?\n %U":empty-lines 1)

          ;; Reading Lists
          ("r" "Reading List")
          ("ra" "Article" entry
           (file+olp ,(concat org_agenda_dir "reading_list.org") "Journal Article")
           "* RD %? \n%U\n%a" :empty-lines 1)
          ("rb" "Books" entry
           (file+olp ,(concat org_agenda_dir "reading_list.org") "Books")
           "* RD %? \n%U\n%a" :empty-lines 1)

          ;; Birthdays
          ("b" "Birthdays")
          ("br" "Relatives/Family" entry
           (file+olp ,(concat org_agenda_dir "birthdays.org") "Relatives")
           "* %^{Who?} \n%^{Birthday}t\n%U" :empty-lines 1 :immediate-finish 1)
          ("ba" "Acquintances" entry
           (file+olp ,(concat org_agenda_dir "birthdays.org") "Acquintances")
           "* %^{Who?} \n%^{Birthday}t\n%U" :empty-lines 1 :immediate-finish 1)
          ("bf" "Friends" entry
           (file+olp ,(concat org_agenda_dir "birthdays.org") "Friends")
           "* %^{Who?} \n%^{Birthday}t\n%U" :empty-lines 1 :immediate-finish 1)
          ("bo" "Others" entry
           (file+olp ,(concat org_agenda_dir "birthdays.org") "Others")
           "* %^{Who?} \n%^{Birthday}t\n%U" :empty-lines 1 :immediate-finish 1)

          ;; workflow
          ("m" "Meeting" entry
           (file+olp+datetree ,(concat org_file_dir "meetings.org") "Active")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume :empty-lines 1)
          ("E" "Emails")
          ("Er" "Read Later" entry
           (file+olp ,(concat org_agenda_dir "mail.org") "Read Later")
           (file ,(concat doom-private-dir "orgtemplates/mailreadlater.org"))
           :empty-lines 1 :immediate-finish t)
          ("Ef" "Follow Up" entry (file+olp ,(concat org_agenda_dir "mail.org") "Follow Up")
           (file ,(concat doom-private-dir "orgtemplates/mailfollowup.org"))
           :empty-lines 1 :immediate-finish t)
          ("Es" "Send Mail" entry
           (file+olp ,(concat org_agenda_dir "mail.org") "Compose Mail")
           (file ,(concat doom-private-dir "orgtemplates/mailsendmail.org"))
           :empty-lines 1 :immediate-finish t)

          ;; Tracking
          ("M" "Metrics Capture")
          ("Mw" "Weight" table-line
           (file+headline ,(concat org_file_dir "weight.org") "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :immediate-finish t)
          ("Mp" "PE" table-line
           (file+headline ,(concat org_agenda_dir "pe.org") "Measurements")
           "| %U | %^{BPEL} | %^{EG} | %^{NBPEL} | %^{BPFSL} |"
           :immediate-finish t)
          ("Ml" "Lead Managements" table-line
           (file+headline "~/Videos/Intergender Dynamic/Occam's Razor - Ultimate Seduction System/Template For Managing Leads/Template for Managing Leads.org" "Leads")
           "| %U | %^{Girl Name} | %^{Date Time}T | %^{Had Sex?} | %^{Repeat?} | %^{Source (Daygame, Nightgame, Onlinegame, Else)} | %^{Description} | %^{Next Actions} |"
           :immediate-finish t)

          ;; Journal
          ("j" "Journal Entries")
          ("jj" "Journal Entry" entry
           (function rz/org-journal-find-location)
           "\n** %<%I:%M %p> - %? :journal:\n" :empty-lines 1)
          ("js" "Scheduled Journal" entry
           (function rz/org-journal-date-location)
           "* TODO %?\n <%(princ org-journal--date-location-scheduled-time)>\n"
           :empty-lines 1)
          ("jm" "Morning Journal entry" entry
           (function rz/org-journal-find-location)
           (file ,(concat doom-private-dir "orgtemplates/morningroutine.org"))
           :empty-lines 1 :jump-to-captured t)
          ("jn" "Night Journal entry" entry
           (function rz/org-journal-find-location)
           (file ,(concat doom-private-dir "orgtemplates/nightroutine.org"))
           :empty-lines 1 :jump-to-captured t)
          ("jw" "Weekly Review" entry
           (file ,(concat org_journal_weekly_dir "2022.org"))
           (file ,(concat doom-private-dir "orgtemplates/weeklyreview.org"))
           :empty-lines 1 :jump-to-captured t)

          ;; Cookbook
          ("c" "Cookbook")
          ("cc" "Web Fetch" entry (file "~/org/cookbook.org")
           "%(org-chef-get-recipe-from-url)"
           :empty-lines 1)
          ("cm" "Manual Cookbook" entry (file ,(concat org_file_dir "cookbook.org"))
           "* %^{Recipe title: }\n:PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")

          ;; Protocol
          ("Q" "[pro] Web Quote" entry (file+olp ,(concat org_agenda_dir "inbox.org") "Web Quote")
           "* %^{Quote from/Who said this}\n:PROPERTIES:\n:SOURCE: %:annotation\n:CREATED_AT: %U\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%^{What do you think about this quote}"
           :prepend t :empty-lines 1
           :immediate-finish t)
          ("L" "[pro] Web Link" entry (file+olp ,(concat org_agenda_dir "reading_list.org") "Web")
           "* RD [[%:link][%:description]]\n%U\n%^{What is the gist of it} "
           :prepend t :empty-lines 1
           :immediate-finish t)
          ("W" "[pro] Web Reading List" entry
           (file+olp ,(concat org_agenda_dir "reading_list.org") "Web")
           "* RD %? \n%U\n%a" :empty-lines 1))))

(use-package org-superstar
  :straight t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t
        org-superstar-todo-bullet-alist
          '(("TODO" . 61708)
            ("NEXT" . 61469)
            ("PROG" . 61729)
            ("PROJ" . 61729)
            ("WAIT" . 62092)
            ("CANCEL" . 61532)
            ("DONE" . 61533)
            ("RD" . 61708)
            ("RDING" . 61469)
            ("TMPDROP" . 62092)
            ("DROP" . 61532)
            ("FNSHED" . 61533))))

;;(add-hook 'org-mode-hook #'org-superstar-mode)

;; Setting up org-capture to run on its own frame
(defun rz/delete-capture-frame (&rest _)
  "Delete frame with its name frame-parameter set to \"capture\"."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))
(advice-add 'org-capture-finalize :after #'rz/delete-capture-frame)

(defun rz/org-capture-frame ()
  "Run org-capture in its own frame."
  (interactive)
  (require 'cl-lib)
  (select-frame-by-name "capture")
  (delete-other-windows)
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
    (condition-case err
        (org-capture)
      ;; "q" signals (error "Abort") in `org-capture'
      ;; delete the newly created frame in this scenario.
      (user-error (when (string= (cadr err) "Abort")
                    (delete-frame))))))

(defun rz/org-capture-inbox-frame ()
  "Run org-capture in its own frame."
  (interactive)
  (require 'cl-lib)
  (select-frame-by-name "capture-inbox")
  (delete-other-windows)
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
    (condition-case err
        (org-capture "" "x")
      ;; "q" signals (error "Abort") in `org-capture'
      ;; delete the newly created frame in this scenario.
      (user-error (when (string= (cadr err) "Abort")
                    (delete-frame))))))

(defun rz/org-capture-journal-frame ()
  "Run org-capture in its own frame."
  (interactive)
  (require 'cl-lib)
  (select-frame-by-name "capture-journal")
  (delete-other-windows)
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
    (condition-case err
        (org-capture "" "jj")
      ;; "q" signals (error "Abort") in `org-capture'
      ;; delete the newly created frame in this scenario.
      (user-error (when (string= (cadr err) "Abort")
                    (delete-frame))))))

(defun rz/org-capture-task-frame ()
  "Run org-capture in its own frame."
  (interactive)
  (require 'cl-lib)
  (select-frame-by-name "capture-task")
  (delete-other-windows)
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
    (condition-case err
        (org-capture "" "tt")
      ;; "q" signals (error "Abort") in `org-capture'
      ;; delete the newly created frame in this scenario.
      (user-error (when (string= (cadr err) "Abort")
                    (delete-frame))))))
