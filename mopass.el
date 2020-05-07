(require 'json)
(require 'subr-x)

(defvar mopass--filename (expand-file-name "~/.gnupg/passwords.gpg"))
(defvar mopass-dir  (expand-file-name "~/.gnupg"))

(defun mopass--get-password-and-run-callback (callback &optional override-file)
  "Get password and run callback"
  (let* ((mopass--filename (if override-file
                               override-file
                             mopass--filename))
         (keys (with-temp-buffer
                 (insert-file-contents mopass--filename)
                 (search-forward "[")
                 (beginning-of-line)
                 (buffer-string)
                 (json-read))))
    (let ((keys-helm-source
           `((name . "Select a key: ")
             (candidates . ,(mapcar #'(lambda (element)
                                        `(,(string-join
                                            `(,(cdr (assoc 'name element))
                                              ,(cdr (assoc 'username element))) " - ") . ,element))
                                    keys))
             (action . callback))))
      (helm :sources '(keys-helm-source)))))

(defun mopass-copy-password-to-kill-ring (arg)
  "Get password and copy it to kill-ring. With `arg' let
you choose the passwords file"
  (interactive "P")
  (let ((filename
         (if arg
             (read-file-name "Select passwords file: " mopass-dir))))
    (mopass--get-password-and-run-callback
     'mopass--parse-password-and-copy-to-kill-ring
     filename)))

(defun mopass-insert-password (arg)
  "Get password and insert it in the current position. With `arg' let
you choose the passwords file"
  (interactive "P")
  (let ((filename
         (if arg
             (read-file-name "Select passwords file: " mopass-dir))))
    (mopass--get-password-and-run-callback
     'mopass--parse-password-and-insert
     filename)))

(defun mopass--parse-password-and-copy-to-kill-ring (candidate)
  (kill-new (cdr (assoc 'password candidate))))

(defun mopass--parse-password-and-insert (candidate)
  (insert (cdr (assoc 'password candidate))))

(defalias 'mopass 'mopass-copy-password-to-kill-ring)
