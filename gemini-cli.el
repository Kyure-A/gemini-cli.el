;;; gemini-cli.el --- Emacs integration for the Gemini CLI

;; Copyright (C) 2024 Kyure-A
;;
;; Author: Kyure-A <kyure.a@gmail.com>
;; Keywords: ai, tools, convenience
;; URL: https://github.com/Kyure-A/gemini-cli.el
;; Package-Requires: ((emacs "30.1") (transient "0.4.0") (eat "0.8"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides an Emacs integration for the Google Gemini CLI.
;; It allows you to interact with the Gemini AI from within Emacs,
;; providing a seamless workflow for coding, writing, and more.
;;
;; To get started, run `M-x gemini-cli-start`.
;;
;;; Code:

(require 'ansi-color)
(require 'transient)

;;;;; Customization Variables

(defgroup gemini-cli nil
  "Emacs integration for the Gemini CLI."
  :group 'applications)

(defcustom gemini-cli-start-command '("npx" "https://github.com/google-gemini/gemini-cli")
  "The base command and arguments to launch the Gemini CLI."
  :type '(repeat string)
  :group 'gemini-cli)

(defcustom gemini-cli-model nil
  "The Gemini model to use (e.g., \"gemini-1.5-pro\")."
  :type '(string :tag "Model Name")
  :group 'gemini-cli)

(defcustom gemini-cli-yolo-mode nil
  "If non-nil, run in YOLO mode, automatically accepting all actions."
  :type 'boolean
  :group 'gemini-cli)

(defcustom gemini-cli-keymap-prefix (kbd "C-c g")
  "Prefix key for `gemini-cli` commands."
  :type 'key-sequence
  :group 'gemini-cli)

(defcustom gemini-cli-start-key (kbd "s")
  "Key to bind `gemini-cli-start` under `gemini-cli-keymap-prefix`."
  :type 'key-sequence
  :group 'gemini-cli)

(defcustom gemini-cli-stop-key (kbd "q")
  "Key to bind `gemini-cli-stop` under `gemini-cli-keymap-prefix`."
  :type 'key-sequence
  :group 'gemini-cli)

(defcustom gemini-cli-restart-key (kbd "r")
  "Key to bind `gemini-cli-restart` under `gemini-cli-keymap-prefix`."
  :type 'key-sequence
  :group 'gemini-cli)

(defcustom gemini-cli-send-buffer-key (kbd "b")
  "Key to bind `gemini-cli-send-buffer-as-context` under `gemini-cli-keymap-prefix`."
  :type 'key-sequence
  :group 'gemini-cli)

(defcustom gemini-cli-send-region-key (kbd "R")
  "Key to bind `gemini-cli-send-region-as-context` under `gemini-cli-keymap-prefix`."
  :type 'key-sequence
  :group 'gemini-cli)

;;;;; Core Process Management

(defvar-local gemini-cli--process nil
  "The Gemini CLI process object for the current buffer.")

(defvar-local gemini-cli--in-code-block nil
  "Non-nil if currently inside a code block.")

(defvar-local gemini-cli--code-block-lang nil
  "The language of the current code block.")

(defun gemini-cli--format-output (start end)
  "Format the output in the buffer between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^\s-*\(```\)\(.*\)?$" nil t)
      (let* ((fence-start (match-beginning 1))
             (fence-end (match-end 1))
             (lang (if (match-string 2) (string-trim (match-string 2)) nil)))
        (if gemini-cli--in-code-block
            ;; End of code block
            (progn
              (setq gemini-cli--in-code-block nil)
              (setq gemini-cli--code-block-lang nil)
              (put-text-property fence-start fence-end 'face 'font-lock-comment-face))
          ;; Start of code block
          (setq gemini-cli--in-code-block t)
          (setq gemini-cli--code-block-lang lang)
          (put-text-property fence-start fence-end 'face 'font-lock-comment-face))))

    ;; Apply font-lock to code blocks
    (when gemini-cli--in-code-block
      (let ((mode (cond
                   ((string= gemini-cli--code-block-lang "elisp") 'emacs-lisp-mode)
                   ((string= gemini-cli--code-block-lang "python") 'python-mode)
                   ((string= gemini-cli--code-block-lang "js") 'js-mode)
                   ((string= gemini-cli--code-block-lang "json") 'json-mode)
                   ((string= gemini-cli--code-block-lang "sh") 'sh-mode)
                   ((string= gemini-cli--code-block-lang "bash") 'sh-mode)
                   ((string= gemini-cli--code-block-lang "diff") 'diff-mode)
                   (t nil))))
        (when mode
          (font-lock-fontify-region start end nil mode)))))

(defun gemini-cli--process-filter (proc string)
  "Process filter to handle output from the Gemini CLI."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (old-point (point)))
        (goto-char (process-mark proc))
        (insert string)
        (ansi-color-apply-on-region (process-mark proc) (point))
        (gemini-cli--format-output old-point (point))
        (set-marker (process-mark proc) (point))))))

(defun gemini-cli--process-sentinel (proc msg)
  "Process sentinel to handle process termination."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (message "Gemini CLI process terminated: %s" msg)
      (setq-local gemini-cli--process nil)
      (setq-local gemini-cli--in-code-block nil)
      (setq-local gemini-cli--code-block-lang nil))))

(defun gemini-cli-send-input (input)
  "Send INPUT to the Gemini CLI process."
  (if (and (local-variable-p 'gemini-cli--process) gemini-cli--process (process-live-p gemini-cli--process))
      (progn
        (process-send-string gemini-cli--process (concat input "
"))
        (with-current-buffer (process-buffer gemini-cli--process)
          (add-to-history 'gemini-cli--input-history input)))
    (error "Gemini CLI is not running. Run `gemini-cli-start` first")))

;;;;; UI Functions

(defvar-local gemini-cli--input-history nil
  "History of sent inputs.")

(defconst gemini-cli--slash-commands
  '("/help" "/tools" "/memory show" "/mcp")
  "A list of common Gemini CLI slash commands.")

(defun gemini-cli-read-slash-command ()
  "Read a slash command from the user with completion."
  (interactive)
  (let ((command (completing-read "Slash command: " gemini-cli--slash-commands nil t)))
    (gemini-cli-send-input command)))

(defun gemini-cli-read-prompt ()
  "Read a prompt from the user and send it."
  (interactive)
  (let ((prompt (read-string "Prompt: " nil 'gemini-cli--input-history)))
    (gemini-cli-send-input prompt)))

(defun gemini-cli-read-at-command ()
  "Read a file path to add to the prompt."
  (interactive)
  (let ((file (read-file-name "File to add: ")))
    (gemini-cli-send-input (concat "@" file))))

(defun gemini-cli--write-to-temp-file (content)
  "Write CONTENT to a temporary file and return its path."
  (let* ((temp-dir (make-temp-file "gemini-cli-" t))
         (temp-file (expand-file-name "context.txt" temp-dir)))
    (make-directory temp-dir t)
    (with-temp-file temp-file
      (insert content))
    temp-file))

(defun gemini-cli-send-buffer-as-context ()
  "Send the content of the current buffer as context to the Gemini CLI."
  (interactive)
  (let* ((content (buffer-string))
         (temp-file (gemini-cli--write-to-temp-file content)))
    (gemini-cli-send-input (concat "@" temp-file))
    (message "Sent buffer content as context from %s" temp-file)))

(defun gemini-cli-send-region-as-context ()
  "Send the content of the active region as context to the Gemini CLI."
  (interactive)
  (if (use-region-p)
      (let* ((content (buffer-substring (region-beginning) (region-end)))
             (temp-file (gemini-cli--write-to-temp-file content)))
        (gemini-cli-send-input (concat "@" temp-file))
        (message "Sent region content as context from %s" temp-file))
    (error "No region active. Mark a region first.")))

;;;###autoload
(transient-define-prefix gemini-cli ()
  "Transient command for interacting with the Gemini CLI."
  ["Gemini CLI"
   ("p" "Prompt" gemini-cli-read-prompt)
   ("s" "Slash Command" gemini-cli-read-slash-command)
   ("@" "Add File" gemini-cli-read-at-command)
   ("b" "Send Buffer as Context" gemini-cli-send-buffer-as-context)
   ("r" "Send Region as Context" gemini-cli-send-region-as-context)]
  ["Process"
   ("R" "Restart" gemini-cli-restart)
   ("q" "Quit" gemini-cli-stop)])

;;;;; Major Mode and Entry Points

(define-derived-mode gemini-cli-mode comint-mode "Gemini-CLI"
  "Major mode for interacting with the Gemini CLI.
\{gemini-cli-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (setq-local comint-prompt-regexp "^> ")
  (setq-local comint-process-echoes t)
  (setq-local comint-get-old-input (lambda () "")))

;;;###autoload
(defun gemini-cli-start ()
  "Start the Gemini CLI process and open its interaction buffer."
  (interactive)
  (let* ((buf-name "*gemini-cli*")
         (buffer (get-buffer-create buf-name))
         (command (append gemini-cli-start-command
                          (when gemini-cli-model (list "--model" gemini-cli-model))
                          (when gemini-cli-yolo-mode (list "--yolo")))))
    (with-current-buffer buffer
      (gemini-cli-mode)
      (when (and (local-variable-p 'gemini-cli--process) gemini-cli--process (process-live-p gemini-cli--process))
        (error "Gemini CLI is already running in %s" buf-name))
      (setq-local gemini-cli--process (apply #'start-process "gemini-cli" buffer command))
      (set-process-filter gemini-cli--process #'gemini-cli--process-filter)
      (set-process-sentinel gemini-cli--process #'gemini-cli--process-sentinel)
      (set-buffer-process buffer gemini-cli--process))
    (display-buffer buffer)))

;;;###autoload
(defun gemini-cli-stop ()
  "Stop the Gemini CLI process."
  (interactive)
  (let ((process (when (get-buffer "*gemini-cli*")
                   (with-current-buffer "*gemini-cli*"
                     gemini-cli--process))))
    (if (and process (process-live-p process))
        (progn
          (quit-process process)
          (message "Gemini CLI stopped."))
      (message "Gemini CLI is not running."))))

(defun gemini-cli-restart ()
  "Restart the Gemini CLI process."
  (interactive)
  (gemini-cli-stop)
  (sleep-for 0.1)
  (gemini-cli-start))

;;;;; Keybindings

(define-key global-map gemini-cli-keymap-prefix 'gemini-cli)
(define-key (lookup-key global-map gemini-cli-keymap-prefix) gemini-cli-start-key 'gemini-cli-start)
(define-key (lookup-key global-map gemini-cli-keymap-prefix) gemini-cli-stop-key 'gemini-cli-stop)
(define-key (lookup-key global-map gemini-cli-keymap-prefix) gemini-cli-restart-key 'gemini-cli-restart)
(define-key (lookup-key global-map gemini-cli-keymap-prefix) gemini-cli-send-buffer-key 'gemini-cli-send-buffer-as-context)
(define-key (lookup-key global-map gemini-cli-keymap-prefix) gemini-cli-send-region-key 'gemini-cli-send-region-as-context)

(provide 'gemini-cli)


;;; gemini-cli.el ends here
