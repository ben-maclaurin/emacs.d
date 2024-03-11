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


;; UI
(menu-bar-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(context-menu-mode 1)
(blink-cursor-mode 0)

(setq-default line-spacing 5)

(set-frame-font "Berkeley Mono Variable-14" nil t)


;; Darwin
(setq mac-command-modifier 'meta)

(use-package ns-auto-titlebar)
(when (eq system-type 'darwin)
  (ns-auto-titlebar-mode))


;; Convenience
(setq next-line-add-newlines t)

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
  ("C-;" . er/contract-region))

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
 :bind (("C-x b" . consult-buffer) ("M-p" . consult-fd) ("M-s" . consult-ripgrep) ("C-s"))
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
(load-theme 'modus-vivendi)
(require 'ef-themes)

(defun b-random-file ()
  "Open a random file from the current directory"
  (interactive)
  (let* ((files (directory-files default-directory t))
         (random-file (nth (random (length files)) files)))
    (while (file-directory-p random-file)
      (setq random-file (nth (random (length files)) files)))
    (find-file random-file)
    (edit-in)))


;; YNAB
;; package repo: https://github.com/ben-maclaurin/ynab-emacs
(require 'ynab)
(setq ynab-budget-id "64dfafd8-500e-4383-8f81-1822475830ec")
(setq ynab-api-key
      (string-trim
       (shell-command-to-string
        "security find-generic-password -s ynab-api-key -a ben -w")))

(global-set-key (kbd "C-x y") 'ynab-budget)

;; vterm
(use-package vterm)
(setq vterm-timer-delay 0)

(setq dired-dwim-target t)

(use-package yasnippet)
(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(use-package markdown-mode
  :hook (markdown-mode . lsp)
  :config
  (require 'lsp-marksman))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package forge)

;; LSP-Bridge
(global-set-key (kbd "M-.") 'lsp-bridge-find-def)
(global-set-key (kbd "M-,") 'lsp-bridge-find-def-return)
(global-set-key (kbd "M-;") 'lsp-bridge-find-references)
(global-set-key (kbd "C-i") 'lsp-bridge-popup-documentation)
(global-set-key (kbd "C-.") 'lsp-bridge-code-action)

(global-set-key (kbd "M-/") 'comment-dwim)


(global-display-line-numbers-mode t)


(defun b-select-current-line-or-expand-down ()
  "Select the current line or, if already selecting, extend the selection down a line."
  (interactive)
  (if (use-region-p)
      (progn
        (goto-char (region-end))
        (forward-line 1)
        (end-of-line))
    (progn
      (beginning-of-line) ; Move to the beginning of the current line
      (set-mark (point)) ; Set the mark at the current position
      (end-of-line)))) ; Move to the end of the current line and extend the selection

(global-set-key (kbd "M-l") 'b-select-current-line-or-expand-down)

(defvar my-lsp-bridge-doc-timer nil
  "Timer to trigger showing lsp-bridge documentation after a hover delay.")

(defun my-cancel-lsp-bridge-doc-timer ()
  "Cancel the lsp-bridge doc timer if it's active."
  (when my-lsp-bridge-doc-timer
    (cancel-timer my-lsp-bridge-doc-timer)
    (setq my-lsp-bridge-doc-timer nil)))

(defun my-lsp-bridge-hover-doc ()
  "Show lsp-bridge documentation if lsp-bridge-mode is enabled and point is idle."
  (my-cancel-lsp-bridge-doc-timer) ; Ensure any previous timer is cancelled
  (setq my-lsp-bridge-doc-timer
        (run-with-idle-timer
         1 nil ; Wait for 1 second of idle time
         (lambda ()
           (when (and (bound-and-true-p lsp-bridge-mode) ; Check if lsp-bridge-mode is active
                      (not (window-minibuffer-p))) ; Ensure we're not in the minibuffer
             (lsp-bridge-popup-documentation))))))

(defun my-setup-lsp-bridge-doc-on-hover ()
  "Setup hover documentation for lsp-bridge."
  (add-hook 'post-command-hook #'my-lsp-bridge-hover-doc))

(defun my-teardown-lsp-bridge-doc-on-hover ()
  "Tear down hover documentation setup for lsp-bridge."
  (remove-hook 'post-command-hook #'my-lsp-bridge-hover-doc)
  (my-cancel-lsp-bridge-doc-timer))

(add-hook 'lsp-bridge-mode-hook #'my-setup-lsp-bridge-doc-on-hover)



