;;; type-fastly.el --- a minor mode to fake your code presentations

(make-variable-buffer-local
 (defvar type-fastly-text "" "The text left to type fastly."))

(defvar fastly-inserting " >")
(defvar fastly-finished  " $")

(defun get-file-string (file)
  "Read the contents of a file and return as a string."
  (with-current-buffer (find-file-noselect file)
    (buffer-string)))

(defun read-file (name)
  "Prompts for file name and returns the file as a string."
  (interactive `(,(ido-read-file-name "Find file: ")))
  (setq type-fastly-text
    (get-file-string (expand-file-name name))))

(defun first-char (string) (string-to-char string))
(defun rest-char (string) (substring-no-properties string 1 nil))

(defun fastly-insert-char ()
  "Insert the next character in type-fastly-text."
  (interactive)
  (if (not (= (length type-fastly-text) 0))
      (progn
        (insert-char (first-char type-fastly-text) 1)
        (setq type-fastly-text (rest-char type-fastly-text))))
  (fastly-update-modeline))

(defun fastly-backward-delete-char ()
  "Delete the last character typed fastly, adding back the
character to type-fastly-text."
  (interactive)
  (if (not (bobp)) ;; if not at beginning of buffer
      (progn
        (let ((last-char (string (preceding-char))))
          (delete-backward-char 1)
          (setq type-fastly-text (concat last-char type-fastly-text)))))
  (fastly-update-modeline))

(defun fastly-update-modeline ()
  "Update the modeline character"
  (defun update-modeline (new-symbol)
      (setcar (cdr (assq 'type-fastly-mode minor-mode-alist)) new-symbol))
  (if (= (length type-fastly-text) 0)
      (update-modeline fastly-finished)
      (update-modeline fastly-inserting)))

;;;###autoload
(define-minor-mode type-fastly-mode
  "Fake your code presentations."
  :lighter fastly-inserting
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap self-insert-command] 'fastly-insert-char)
            (define-key map (kbd "<backspace>") 'fastly-backward-delete-char)
            (define-key map (kbd "<S-escape>") 'type-fastly-mode)
            map)
  (if type-fastly-mode
      (progn
        (call-interactively 'read-file)
        (fastly-update-modeline))))

(global-set-key (kbd "<f8>") 'type-fastly-mode) ; for debugging

(provide 'type-fastly)
