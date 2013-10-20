;;; type-fastly.el --- a minor mode to fake your code presentations

(make-variable-buffer-local
 (defvar type-fastly-text "" "The text left to type fastly."))

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
        (setq type-fastly-text (rest-char type-fastly-text)))
      (message "Done.")))

;;;###autoload
(define-minor-mode type-fastly-mode
  "Fake your code presentations."
  :lighter " *"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap self-insert-command] 'fastly-insert-char)
            (define-key map (kbd "<S-escape>") 'type-fastly-mode)
            map)
  (if type-fastly-mode
      (call-interactively 'read-file)))

(global-set-key (kbd "<f8>") 'type-fastly-mode) ; for debugging

(provide 'type-fastly)
