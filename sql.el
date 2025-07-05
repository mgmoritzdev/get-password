;; Usage in org-babel
;; Given the encrypted file sql-credentials.gpg:
;; [
;;   {
;;     "my-postgres": {
;;       "sql-product": "postgres",
;;       "sql-server": "123.123.123.123",
;;       "sql-user": "pguser",
;;       "sql-password": "pgpass",
;;       "sql-database": "pgname"
;;     }
;;   },
;;   {
;;     "my-sql-server": {
;;       "sql-product": "mssql",
;;       "sql-server": "123.123.123.124",
;;       "sql-user": "msuser",
;;       "sql-password": "mspass",
;;       "sql-database": "msname"
;;     }
;;   }
;; ]
;; * My PostgreSQL Babel Block
;; :PROPERTIES:
;; :header-args: :results table
;; :header-args+: :config (sqlcreds-setconfig 'config 'my-postgres)
;; :header-args+: :engine (sqlcreds-get-property config 'sql-product)
;; :header-args+: :dbhost (sqlcreds-get-property config 'sql-server)
;; :header-args+: :dbport (sqlcreds-get-property config 'sql-port)
;; :header-args+: :database (sqlcreds-get-property config 'sql-database)
;; :header-args+: :dbuser (sqlcreds-get-property config 'sql-user)
;; :header-args+: :dbpassword (sqlcreds-get-property config 'sql-password)
;; :END:
;;
;; #+BEGIN_SRC sql
;;   SELECT 1 as hello;
;; #+END_SRC
;;
;; * My PostgreSQL Babel Block
;; :PROPERTIES:
;; :header-args: :results table
;; :header-args+: :config (sqlcreds-setconfig 'config 'my-sql-server)
;; :header-args+: :engine (sqlcreds-get-property config 'sql-product)
;; :header-args+: :dbhost (sqlcreds-get-property config 'sql-server)
;; :header-args+: :dbport (sqlcreds-get-property config 'sql-port)
;; :header-args+: :database (sqlcreds-get-property config 'sql-database)
;; :header-args+: :dbuser (sqlcreds-get-property config 'sql-user)
;; :header-args+: :dbpassword (sqlcreds-get-property config 'sql-password)
;; :END:
;;
;; #+BEGIN_SRC sql
;;   SELECT 1 as hello;
;; #+END_SRC

(require 'json)
(defvar sqlcreds--file  (expand-file-name "~/.gnupg/sql-credentials.gpg"))

(defun sqlcreds-get-config (config-name)
  "Get sql credentials by name"
  (with-temp-buffer
    (insert-file-contents sqlcreds--file)
    (search-forward "[")
    (beginning-of-line)
    (buffer-string)
    (cdr (assoc config-name (cl-map 'list
                                    (lambda (item)
                                      `(,(caar item) . ,(cdar item)))
                                    (json-read))))))

(defun moritz/decrypt-keys(key)
  (interactive)
  (let ((default-directory "~/.gnupg"))
    (if (file-exists-p "decrypt.sh")
        (shell-command (format "./decrypt.sh %s" key) "decrypt-output")
      (error "decrypt script is missing"))))

(defun sqlcreds--require-certs (config-variable connection-name)
  (if (and (sqlcreds-get-property (eval config-variable) 'sql-sslmode)
           (not (file-exists-p (concat "/tmp/" (symbol-name connection-name)))))
      (moritz/decrypt-keys connection-name)))


(defun sqlcreds-get-property (config property)
  (cdr (assoc property config)))

(defun sqlcreds-get-property-by (connection-name property)
  (let ((config (sqlcreds-get-config connection-name)))
    (cdr (assoc property config))))

(defun sqlcreds-setconfig (config-variable connection-name)
  (if (local-variable-p config-variable)
      (set config-variable
           (sqlcreds-get-config connection-name))
    (set (make-local-variable config-variable)
         (sqlcreds-get-config connection-name)))
  (sqlcreds--require-certs config-variable connection-name))

;; (set (make-local-variable 'config) (sqlcreds-get-config 'cdp-dev))
