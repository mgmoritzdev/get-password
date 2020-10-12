(require 'json)
(require 'subr-x)
(require 'helm)
(require 'ivy)

(defvar mopass--filename (expand-file-name "~/.gnupg/passwords.gpg"))
(defvar mopass-dir  (expand-file-name "~/.gnupg"))

(defun mopass--get-password-by-name (name &optional override-file)
  "Return the password the exact matching name"
  (let* ((mopass--filename (if override-file
                               override-file
                             mopass--filename))
         (keys (with-temp-buffer
                 (insert-file-contents mopass--filename)
                 (search-forward "[")
                 (beginning-of-line)
                 (buffer-string)
                 (json-read))))
    (alist-get 'password (elt (cl-remove-if-not
                               (lambda (element)
                                 (equal (cdr (assoc 'name element)) name))
                               keys) 0))))

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


(defun mopass--get-passwords (&optional override-file)
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
    keys))

(defun mopass--candidates-function (str pred _)
  (let* ((candidates (mopass--get-passwords))
         (keys (mapcar #'(lambda (element)
                           `(,(string-join
                               `(,(cdr (assoc 'name element))
                                 ,(cdr (assoc 'username element))) " - ") . ,element))
                       candidates)))

    (let ((cand-names (mapcar 'car keys))
          (cand-targets (mapcar 'cdr keys)))
      (cl-mapcar (lambda (s p) (propertize s 'property p))
                 cand-names
                 cand-targets))))

(defun mopass-ivy-execute (cand)
  (cdr (assoc 'password cand)))

(defun mopass-ivy ()
  (interactive)
  (ivy-read "Remmina connections: "
            #'mopass--candidates-function
            :action 'mopass-ivy-execute))

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
(defalias 'mopass-get 'mopass--get-password-by-name)
