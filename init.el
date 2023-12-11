;;; init.el --- A no-frills Emacs config -*- lexical-binding: t; -*-

;; Copyright © 2023

;; Author: Ben MacLaurin <benmaclaurin@icloud.com>
;; URL: https://github.com/ben-maclaurin/emacs.d
;; Package-Requires: ((emacs "29"))
;; Created: 2023-12-10

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A no-frills Emacs config.
;; Expects macOS


;; Package
(require 'package)

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)) ; enable use of use-package
(setq use-package-always-ensure t)


;; Emacs
(setq custom-file (locate-user-emacs-file "custom.el"))
(setq warning-minimum-level :emergency)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/")) ;; set load path


;; Startup
(setq initial-major-mode 'org-mode) ; mainly for scratch buffer
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq
 initial-scratch-message
 "If today were the last day of my life, would I want to do what I am about to do today?")


;; UI
(menu-bar-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(context-menu-mode 1)
(blink-cursor-mode 0)
(global-hl-line-mode)

(setq-default line-spacing 5)

(set-frame-font "EK Modena Mono-14" nil t)

(set-frame-parameter nil 'internal-border-width 50)
(add-to-list 'default-frame-alist '(internal-border-width . 50))
(set-frame-parameter nil 'border-width 0)
(set-fringe-style 0)


;; Darwin
(setq mac-command-modifier 'meta)

(use-package ns-auto-titlebar)
(when (eq system-type 'darwin)
  (ns-auto-titlebar-mode))


;; Convenience
(setq next-line-add-newlines t)

(global-set-key
 (kbd "M-n")
 (lambda ()
   (interactive)
   (next-line)))

(global-set-key
 (kbd "M-p")
 (lambda ()
   (interactive)
   (previous-line)))

(global-set-key (kbd "M-g") 'keyboard-quit)

(global-set-key (kbd "M-c") 'kill-ring-save)

(delete-selection-mode 1)


;; Org
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-emphasis-markers t)


;; No backups, no auto-save
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-auto-revert-mode 1) ; listen for local file change


;; Packages
(use-package
 expand-region
 :init (require 'expand-region)
 :bind
 (("C-l" . er/expand-region)
  ("C-;" . er/contract-region)
  ("M-l" . er/expand-region)))

(use-package prettier)

(use-package deadgrep)

(use-package vertico :config (vertico-mode 1) (vertico-mouse-mode 1))

(use-package magit)

(use-package
 company
 :bind (("C-<return>" . company-complete))
 :config
 (setq company-idle-delay nil)
 (global-company-mode))

(use-package
 orderless
 :ensure t
 :custom (completion-styles '(orderless basic))
 (completion-category-overrides
  '((file (styles basic partial-completion)))))

(use-package
 consult
 :bind (("C-x b" . consult-buffer) ("M-s" . consult-fd))
 :hook (completion-list-mode . consult-preview-at-point-mode)
 :init
 (setq
  register-preview-delay 0.5
  register-preview-function #'consult-register-format)
 (advice-add #'register-preview :override #'consult-register-window)
 (setq
  xref-show-xrefs-function #'consult-xref
  xref-show-definitions-function #'consult-xref)
 :config
 (consult-customize
  consult-theme
  :preview-key
  '(:debounce 0.2 any)
  consult-ripgrep
  consult-git-grep
  consult-grep
  consult-bookmark
  consult-recent-file
  consult-xref
  consult--source-bookmark
  consult--source-file-register
  consult--source-recent-file
  consult--source-project-recent-file
  :preview-key '(:debounce 0.4 any)))

(use-package pyvenv)

(use-package
 marginalia
 :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
 :init (marginalia-mode))

(use-package elisp-autofmt)
(setq elisp-autofmt-python-bin "python3")

(use-package denote)
(setq denote-directory
      "~/Library/Mobile Documents/com~apple~CloudDocs/Life/")

(use-package helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)


;; Theme
(use-package stimmung-themes)

(setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
        (border-mode-line-inactive bg-mode-line-inactive)
        (bg-mode-line-active bg-mode-line-inactive)
	(bg-main bg-dim)))

(load-theme 'modus-operandi)


;; Custom functions
(defun b-quick-save-file ()
  "Quickly save a file"
  (interactive)
  (write-file
   (concat "~/desktop/" (concat (current-time-string) ".org"))))

(defun b-random-file ()
  "Open a random file from the current directory"
  (interactive)
  (let* ((files (directory-files default-directory t))
         (random-file (nth (random (length files)) files)))
    (while (file-directory-p random-file)
      (setq random-file (nth (random (length files)) files)))
    (find-file random-file)
    (edit-in)))

(defun b-edit-in-intellij ()
  "Open the current file in Intellij"
  (interactive)
  (when buffer-file-name
    (shell-command
     (concat
      "open -a 'IntelliJ IDEA.app' "
      (shell-quote-argument buffer-file-name)))))

(global-set-key (kbd "C-x j") 'b-edit-in-intellij)


;; 
;; Mail
;; This code is borrowed from https://macowners.club/posts/email-emacs-mu4e-macos/
;;
(use-package
 mu4e
 :load-path "/opt/homebrew/Cellar/mu/1.10.8/share/emacs/site-lisp/mu/mu4e/")

(require 'smtpmail)

(setq mu4e-mu-binary (executable-find "mu"))
(setq mu4e-maildir "~/.maildir")
(setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
(setq mu4e-update-interval 60)
(setq mu4e-attachment-dir "~/Desktop")
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-user-mail-address-list '("ben.maclaurin@icloud.com"))

(setq mu4e-maildir-shortcuts
      '(("/icloud/INBOX" . ?i) ("/icloud/Sent Messages" . ?I)))

(setq mu4e-bookmarks nil)

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "icloud"
          :enter-func
          (lambda ()
            (mu4e-message "Enter ben.maclaurin@icloud.com context"))
          :leave-func
          (lambda ()
            (mu4e-message "Leave ben.maclaurin@icloud.com context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches
               msg
               :to "ben.maclaurin@icloud.com")))
          :vars
          '((user-mail-address . "ben.maclaurin@icloud.com")
            (user-full-name . "Ben MacLaurin")
            (mu4e-drafts-folder . "/icloud/Drafts")
            (mu4e-refile-folder . "/icloud/Archive")
            (mu4e-sent-folder . "/icloud/Sent Messages")
            (mu4e-trash-folder . "/icloud/Deleted Messages")))))

(setq mu4e-context-policy 'pick-first)

(setq mu4e-compose-context-policy 'ask)

(require 'epa-file)
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)
(auth-source-forget-all-cached)
(setq message-kill-buffer-on-exit t)

(setq
 send-mail-function 'sendmail-send-it
 message-send-mail-function 'sendmail-send-it)

(setq sendmail-program (executable-find "msmtp"))
(setq message-sendmail-envelope-from 'header)

(defun timu/set-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let* ((from
                (save-restriction
                  (message-narrow-to-headers)
                  (message-fetch-field "from")))
               (account
                (cond
                 ((string-match "ben.maclaurin@icloud.com" from)
                  "icloud"))))
          (setq message-sendmail-extra-arguments
                (list ' "-a" account))))))

(add-hook
 'mu4e-compose-mode-hook
 (defun timu/add-cc-and-bcc ()
   (save-excursion (message-add-header "Cc:\n"))
   (save-excursion (message-add-header "Bcc:\n"))))

(add-hook 'mu4e-compose-mode-hook 'company-mode)


;; YNAB
;; package repo: https://github.com/ben-maclaurin/ynab-emacs
(require 'ynab)
(setq ynab-budget-id "64dfafd8-500e-4383-8f81-1822475830ec")
(global-set-key (kbd "C-x y") 'ynab-budget)

