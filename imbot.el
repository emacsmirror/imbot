;;; imbot.el --- Automatic system input method switcher -*- lexical-binding: t; -*-

;; URL: https://github.com/QiangF/imbot
;; Created: July 24th, 2020
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))
;; Version: 3.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; imbot is inspired by https://github.com/laishulu/emacs-smart-input-source

(require 'seq)

(defvar imbot--active-saved nil
  "Buffer local input method state, changes only at manual input method toggling.")

(make-variable-buffer-local 'imbot--active-saved)

(defvar imbot--im-config (if (and (string= (window-system) "w32")
                                  (fboundp 'w32-get-ime-open-status))
                             'imbot--windows
                           'imbot--fcitx5)
  "User config file with the definition of `imbot--activate-force, `imbot--deactivate.")

(require `,imbot--im-config)

(defun imbot--update-cursor ()
  "Set cursor color according to input method state."
  (cancel-timer imbot--update-cursor-timer)
  (if imbot--active-saved
      (set-cursor-color "green")
    (set-cursor-color "white"))
  (redisplay t))

;; disable imbot-mode before looking up key definition start with imbot--prefix-override-keys
(defvar imbot--prefix-override-keys
  '("C-c" "C-x" "<f1>")
  "Prefix keys not handled by input method, which disable input method temperarily.")

(defvar imbot--prefix-override-map-alist nil
  "An `emulation-mode-map-alists keymap.")

(let ((keymap (make-sparse-keymap)))
  (dolist (prefix imbot--prefix-override-keys)
    (define-key keymap (kbd prefix)
                #'imbot--prefix-override-handler))
  ;; better to set imbot--active-checked set for the window manager with an input method toggle shortcut
  (setq imbot--prefix-override-map-alist `((imbot-mode . ,keymap))))

(defun imbot--prefix-override-add (&optional _args)
  "Setup `emulation-mode-map-alist."
  (add-to-list 'emulation-mode-map-alists 'imbot--prefix-override-map-alist))

(defun imbot--prefix-override-remove (&optional _args)
  "Unset `emulation-mode-map-alist."
  (setq emulation-mode-map-alists
        (delq 'imbot--prefix-override-map-alist emulation-mode-map-alists)))

(defvar imbot--prefix-reinstate-triggers
  '(yas-minor-mode eaf-mode)
  "Handle modes that mess `emulation-mode-map-alists, add evil-local-mode if you use evil.")

(defun imbot--prefix-override-handler (arg)
  "Prefix key handler with ARG."
  (interactive "P")
  ;; pre-command-hook is run before this function
  (let* ((keys (this-command-keys))
         (last-command-before-prefix real-last-command))
    ;; remove prefix override to avoid recursion, will be added in post-command-hook
    (imbot--prefix-override-remove)
    (imbot--deactivate-force)
    ;; deactivate imbot in post-command-hook, run after the first key event
    ;; Restore the prefix arg
    (setq prefix-arg arg)
    ;; binding in `emulation-mode-map-alists has no real-this-command, no real-last-command
    (setq last-command 'imbot--prefix-override-handler)
    ;; preserve last-command
    (setq this-command real-last-command)
    (prefix-command-preserve-state)
    ;; Push the key back on the event queue
    (setq unread-command-events
          (append (mapcar (lambda (e) `(t . ,e)) (listify-key-sequence keys))
                  unread-command-events))))

(defvar imbot--overlay nil
  "Inline english overlay.")

(defvar imbot--inline-cursor '(hbar . 4)
  "Inline english cursor.")

(defface imbot--inline-face '((t (:underline t :box nil)))
  "Face to show inline english (input method temperarily disabled) is active."
  :group 'imbot)

(set-face-attribute
 'imbot--inline-face nil
 :weight 'bold
 ;; :foreground (face-attribute 'font-lock-constant-face :foreground)
 :inverse-video nil)

(defun imbot--english-region-p ()
  "Buffer is in `prog-mode or `conf-mode, and buffer string is not in a string or comment."
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun imbot--delete-overlay ()
  (delete-overlay imbot--overlay)
  (setq cursor-type my-default-cursor-type)
  (setq imbot--overlay nil))

(defun imbot--english-context-p ()
  "Return t if English should be inputed at cursor point."
  (unless (eq last-command 'imbot--inline-english-deactivate)
    (let* ((line-beginning (line-beginning-position))
           (point (point))
           (overlay-active (overlayp imbot--overlay))
           (english-context
            (or
             ;; 中文后面紧接1个空格切换到英文输入
             ;; \cC represents any character of category “C”, according to “M-x describe-categories”
             (looking-back "\\cC " (max line-beginning (- point 2)))
             (string-match "^\\s-*[0-9]+$" (buffer-substring-no-properties line-beginning point))
             (looking-at-p "^\\*")    ; org heading
             (looking-back "[a-zA-Z\\-\\*]" (max line-beginning (1- point))))))
      ;; (message "english context: %s, imbot--active-saved: %s" english-context imbot--active-saved)
      (if overlay-active
          (if (and english-context imbot--active-saved)
              (progn (move-overlay imbot--overlay line-beginning (line-end-position))
                     (message "Activate input method with [return]. Quit with [C-g]"))
            (imbot--delete-overlay))
        (when english-context
          (setq imbot--overlay (make-overlay line-beginning (line-end-position) nil t t))
          (setq cursor-type imbot--inline-cursor)
          (overlay-put imbot--overlay 'face 'imbot--inline-face)
          (overlay-put imbot--overlay 'keymap
                       (let ((keymap (make-sparse-keymap)))
                         (define-key keymap (kbd "C-g")
                                     #'imbot--inline-english-quit)
                         (define-key keymap (kbd "RET")
                                     #'imbot--inline-english-deactivate)
                         (define-key keymap (kbd "<return>")
                                     #'imbot--inline-english-deactivate)
                         keymap))))
      english-context)))

(defun imbot--inline-english-deactivate ()
  "Deactivate the inline english overlay."
  (interactive)
  (when (overlayp imbot--overlay)
    (imbot--delete-overlay))
  (imbot--activate-force))

(defun imbot--inline-english-quit ()
  "Quit the inline english overlay."
  (interactive)
  (when imbot--overlay
    (imbot--delete-overlay)
    (setq imbot--active-saved nil)
    (setq this-command 'imbot--inline-english-deactivate)))

(defun imbot--english-p ()
  "Check context."
  ;; (imbot--track-state "in english context checking!")
  (unless (equal major-mode 'dired-mode)
    (or (imbot--english-region-p)
        (imbot--english-context-p))))

(defvar god-local-mode nil)

(defvar hydra-curr-map nil)

(defvar imbot--suppression-watch-list
  '(god-local-mode hydra-curr-map)
  "Enable suppression if any variables in this list is t, add evil-normal-state-minor-mode
evil-visual-state-minor-mode evil-motion-state-minor-mode if evil is used")

(defun imbot--in-key-seq-p ()
  "This-command becomes non nil after prefix sequence completion."
  (or
   (equal last-command 'imbot--prefix-override-handler)
   ;; use C-g to disable repeat popup
   (and repeat-mode repeat-in-progress)
   (memq last-command '(er/expand-region er/contract-region))))

(defvar imbot--suppression-predicates
  (list #'imbot--english-p #'imbot--in-key-seq-p)
  "Conditions in which input method should be suppressed, in order of priority.")

(defun imbot--check-supression-state ()
  "expensive check"
  (or (eval `(or ,@imbot--suppression-watch-list))
      (seq-find 'funcall imbot--suppression-predicates nil)))

(defvar imbot--update-cursor-timer (timer--create))
;; won't work in windows if per app input status is on
;; the toggle with w32-set-ime-open-status will be undone
;; try im-select.exe?
(defun imbot-toggle ()
  (interactive)
  (if imbot--active-saved
      (progn
        (imbot--deactivate-force)
        (setq imbot--active-saved nil))
    (progn
      (imbot--activate-force)
      (setq imbot--active-saved t)))
  (unless (equal major-mode 'exwm-mode)
    (setq imbot--update-cursor-timer (run-with-idle-timer 0.3 nil 'imbot--update-cursor))))

(defun imbot--post-command-function ()
  "Restore input state."
  ;; When an editing command returns to the editor command loop, the buffer is still the original buffer,
  ;; buffer change after Emacs automatically calls set-buffer on the buffer shown in the selected window.
  ;; unless (equal 'real-this-command 'self-insert-command)
  (unless (or ;; (eq real-this-command 'imbot-toggle)
           (eq real-this-command 'imbot--inline-english-quit)
           (eq real-this-command 'imbot--inline-english-deactivate))
    ;; hook not run in command after imbot-toggle?
    ;; in the timer, this-command is now last-command?
    (run-with-timer
     0 nil
     ;; in command that changes buffer, like winner-undo, the timer is run with the target window
     (lambda (last-command-in)
       (let ((last-command last-command-in))
         ;; (notify "post command hook" (format "real this %s command in %s real last %s last %s"
         ;;                                     real-this-command
         ;;                                     last-command-in
         ;;                                     real-last-command
         ;;                                     last-command))
         (when imbot--active-saved
           (if (imbot--check-supression-state)
               (imbot--deactivate-force)
             (imbot--activate-force)))
         ;; put this on an idle timer?
         (unless (equal last-command 'imbot--prefix-override-handler)
           (when imbot-mode (imbot--prefix-override-add)))
         (imbot--update-cursor)))
     last-command)))

(defvar imbot-post-command-hook-list '(post-command-hook)
  "List of hook names to add `imbot--post-command-function into.")

(defun imbot--hook-handler (add-or-remove)
  "Setup hooks, ADD-OR-REMOVE."
  ;; (funcall add-or-remove 'minibuffer-setup-hook 'imbot--deactivate-force)
  (funcall add-or-remove 'minibuffer-exit-hook 'imbot--post-command-function)
  ;; add to "global" post-command-hook
  (dolist (hook-name imbot-post-command-hook-list)
    (funcall add-or-remove hook-name #'imbot--post-command-function)))

(defun imbot--non-interactive (orig-func &rest args)
  (let ((pre-command-hook nil)
        (post-command-hook nil))
    (apply orig-func args)))

;;;###autoload
(define-minor-mode imbot-mode
  "Input method managing bot."
  :global t
  :init-value nil
  (if imbot-mode
      (progn
        (advice-add #'execute-kbd-macro :around #'imbot--non-interactive)
        (imbot--hook-handler 'add-hook)
        (imbot--prefix-override-add)
        ;; (debug-watch 'imbot-mode)
        (dolist (trigger imbot--prefix-reinstate-triggers)
          (advice-add trigger :after #'imbot--prefix-override-add)))
    (advice-remove #'execute-kbd-macro #'imbot--non-interactive)
    (imbot--hook-handler 'remove-hook)
    (imbot--prefix-override-remove)
    (dolist (trigger imbot--prefix-reinstate-triggers)
      (advice-remove trigger #'imbot--prefix-override-add))))

(provide 'imbot)
;;; imbot.el ends here
