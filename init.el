(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(setq initial-major-mode 'org-mode)

(setq custom-file (locate-user-emacs-file "custom.el"))

(menu-bar-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(context-menu-mode 1)

(setq-default line-spacing 5)

(setq inhibit-startup-message t)
(setq warning-minimum-level :emergency)

(setq mac-command-modifier 'meta)
(setq next-line-add-newlines t)

(global-set-key (kbd "M-n")
		(lambda ()
		  (interactive)
		  (next-line)))
(global-set-key (kbd "M-p")
		(lambda ()
		  (interactive)
		  (previous-line)))

(setq initial-scratch-message nil)

(use-package expand-region
  :init (require 'expand-region):bind
  (("C-l" . er/expand-region)
   ("C-;" . er/contract-region)
   ("M-l" . er/expand-region)))

;; (use-package avy
;;   :bind (("C-i" . avy-goto-char-timer)
;; 	 ("M-i" . avy-goto-char-timer)))

(delete-selection-mode 1)

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-emphasis-markers t)

(global-visual-line-mode 1)		

(set-frame-font "Lexend-15" nil t)

(blink-cursor-mode 0)

;; (use-package treesit-auto
;;   :config
;;   (global-treesit-auto-mode))

(use-package prettier)

(use-package deadgrep)

(use-package projectile)
(setq projectile-project-search-path '("~/Developer/" "~/.emacs.d/"))

(use-package vertico
  :config (vertico-mode 1)(vertico-mouse-mode 1))

(use-package magit)

(use-package atomic-chrome)
(require 'atomic-chrome)
(atomic-chrome-start-server)

(use-package company
  :bind (("C-<return>" . company-complete)):config
  (setq company-idle-delay nil)
  (global-company-mode))

(use-package web-mode)

(defun ben-save-and-switch-to-web-mode ()
  (let ((original-mode major-mode))
    (web-mode)
    (save-buffer)
    (funcall original-mode)))

(add-hook 'after-save-hook 'ben-save-and-switch-to-web-mode)
(add-hook 'find-file-hook 'ben-save-and-switch-to-web-mode)

(defun ben-prettier ()
  (when (or (string-equal "ts"
			  (file-name-extension buffer-file-name))
	    (string-equal "tsx"
			  (file-name-extension buffer-file-name))
	    (string-equal "js"
			  (file-name-extension buffer-file-name))
	    (string-equal "jsx"
			  (file-name-extension buffer-file-name)))
    (prettier-prettify)))

(add-hook 'before-save-hook 'ben-prettier)

(setq make-backup-files nil)

;; (setq-default line-spacing 6)

(use-package autothemer)

(use-package pulsar
  :config (require 'pulsar)(setq pulsar-pulse t)(setq pulsar-delay 0.055)(setq pulsar-iterations 10)(setq pulsar-face 'pulsar-magenta)(setq pulsar-highlight-face 'pulsar-yellow)(pulsar-global-mode 1)(dolist (hook '(org-mode-hook emacs-lisp-mode-hook))
																									 (add-hook hook #'pulsar-mode))(let ((map global-map))
																									 (define-key map (kbd "C-c h p") #'pulsar-pulse-line)
																									 (define-key map (kbd "C-c h h") #'pulsar-highlight-line)))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless basic))(completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :bind (("C-x b" . consult-buffer)
	 ("M-s" . consult-fd)):hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init (setq register-preview-delay 0.5 register-preview-function
	      #'consult-register-format)(advice-add #'register-preview :override #'consult-register-window)(setq xref-show-xrefs-function #'consult-xref
	      xref-show-definitions-function #'consult-xref):config
  (consult-customize consult-theme
		     :preview-key '(:debounce 0.2 any)consult-ripgrep
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

(use-package ns-auto-titlebar)
(when (eq system-type 'darwin)
  (ns-auto-titlebar-mode))

(setq-default mode-line-format nil)

(global-set-key (kbd "M-g")
		'keyboard-quit)

(global-set-key (kbd "M-c")
		'kill-ring-save)

;; (use-package flexoki-themes
;;   :ensure t  ;; or :straight t if using straight.el
;;   :config (load-theme 'flexoki-themes-dark t))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar):bind
  ("C-x f" . dired-sidebar-toggle-sidebar))

(use-package pyvenv)

(defun convert-binary-to-decimal ()
  (interactive)
  (let* ((binary-string (buffer-substring-no-properties (region-beginning)
							(region-end)))
	 (clean-binary-string (replace-regexp-in-string "[^01]" "" binary-string))
	 (fixed-binary-string (if (> (length clean-binary-string) 8)
				  (substring clean-binary-string
					     (- (length clean-binary-string)
						8))
				clean-binary-string))
	 (is-negative (string-prefix-p "1" fixed-binary-string))
	 (unsigned-binary (if is-negative
			      (substring fixed-binary-string 1)
			    fixed-binary-string))
	 (unsigned-decimal (string-to-number unsigned-binary 2))
	 (sign-adjustment (if is-negative
			      (expt 2
				    (length unsigned-binary))
			    0))
	 (decimal-number (- unsigned-decimal sign-adjustment)))
    (message "Decimal (two's complement, 8-bit): %d"
	     decimal-number)))

(defun insert-binary-template ()
  (interactive)
  (insert (format "%s" "0 0 0 0 0 0 0 0 0")))

(defun quick-save-file ()
  (interactive)
  (write-file (concat "~/desktop/"
		      (concat (current-time-string)
			      ".org"))))

;; (global-display-line-numbers-mode)

(use-package zig-mode)

;; (load-theme 'modus-operandi)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle)):init
  (marginalia-mode))

(defun open-random-file ()
  "Open a random file from the current directory."
  (interactive)
  (let* ((files (directory-files default-directory t))
	 (random-file (nth (random (length files))
			   files)))
    (while (file-directory-p random-file)
      (setq random-file (nth (random (length files))
			     files)))
    (find-file random-file)
    (edit-in)))

(use-package olivetti
  :init (require 'olivetti))

(setq initial-scratch-message "If today were the last day of my life, would I want to do what I am about to do today?")

(setq elisp-autofmt-python-bin "python3")


(setq denote-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Life/")

(defun edit-in-cot ()
  "Open the current file in Bike.app on macOS."
  (interactive)
  (when buffer-file-name
    (shell-command (concat "open -a CotEditor.app " (shell-quote-argument buffer-file-name)))))

(defun edit-in-intellij ()
  "Open the current file in Bike.app on macOS."
  (interactive)
  (when buffer-file-name
    (shell-command (concat "open -a 'IntelliJ IDEA.app' " (shell-quote-argument buffer-file-name)))))

(defun edit-in-safari ()
  "Open the current file in Bike.app on macOS."
  (interactive)
  (when buffer-file-name
    (shell-command (concat "open -a 'Safari.app' " (shell-quote-argument buffer-file-name)))))

(defun edit-in ()
  "Open the current file in Bike if in org-mode, in Safari if it's an HTML file, otherwise in IntelliJ."
  (interactive)
  (if (eq major-mode 'org-mode)
      (edit-in-cot)
    (if (and (buffer-file-name)
             (string-match "\\.html\\'" (buffer-file-name)))
        (edit-in-safari)
      (edit-in-intellij))))

(global-set-key (kbd "C-x j") 'edit-in)

(global-auto-revert-mode 1)

;; (setq auto-revert-avoid-polling t)
(keymap-global-set "<down-mouse-4>" 'strokes-do-stroke)

(use-package helpful)

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)

(defun htmlfontify-org-files-in-current-dir ()
  "Htmlfontify all .org files in the current directory, retaining styles, and add custom link tags."
  (interactive)
  (let ((dir (expand-file-name default-directory)))  ; Use current directory
    (dolist (file (directory-files dir t "\\.org\\'"))  ; Filter for .org files
      (when (file-regular-p file)
        (with-current-buffer (find-file-noselect file)  ; Open file in buffer
          (htmlfontify-buffer)

          ;; Insert custom link tags at the beginning of the HTML buffer.
          (goto-char (point-min))
          (insert "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">\n")
          (insert "<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>\n")
          (insert "<link href=\"https://fonts.googleapis.com/css2?family=Lexend&display=swap\" rel=\"stylesheet\">\n")

          (write-file (concat file ".html"))
          (kill-buffer))))))  ; Close the buffer

(defun fill-org-paragraphs-in-directory ()
  "Open all .org files in the current directory, select all text, and run org-fill-paragraph."
  (interactive)
  (let ((files (directory-files "." t "\\.org\\'")))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (while (not (eobp))
          (org-fill-paragraph nil t)
          (forward-paragraph))))))

