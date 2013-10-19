;;; type-fastly.el --- a minor mode to fake your code presentations

(defun get-file-string (file)
  "Read the contents of a file and return as a string."
  (with-current-buffer (find-file-noselect file)
    (buffer-string)))

(defun read-file (name)
  "Prompts for file name and returns the file as a string."
  (interactive `(,(ido-read-file-name "Find file: ")))
  (get-file-string (expand-file-name name)))

(defvar text "hello there!")

(defun first-char (string) (string-to-char string))
(defun rest-char (string) (substring string 1 nil))
(defun fastly-insert-char ()
  (interactive)
  (if (not (= (length text) 0))
      (progn (insert-char (first-char text) 1) (setq text (rest-char text)))
      (progn (message "Done!"))))

(define-minor-mode type-fastly-mode
  "Fake your code presentations."
  :lighter " f"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap self-insert-command] 'fastly-insert-char)
            (define-key map (kbd "+") 'type-fastly-mode)
            map))

(global-set-key (kbd "<f8>") 'type-fastly-mode) ; for debugging

(message "Reloaded type-fastly.el")
