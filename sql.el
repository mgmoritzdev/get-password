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
;; :header-args+: :engine (sql-get-config-prop config 'sql-product)
;; :header-args+: :dbhost (sql-get-config-prop config 'sql-server)
;; :header-args+: :dbport (sql-get-config-prop config 'sql-port)
;; :header-args+: :database (sql-get-config-prop config 'sql-database)
;; :header-args+: :dbuser (sql-get-config-prop config 'sql-user)
;; :header-args+: :dbpassword (sql-get-config-prop config 'sql-password)
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
;; :header-args+: :engine (sql-get-config-prop config 'sql-product)
;; :header-args+: :dbhost (sql-get-config-prop config 'sql-server)
;; :header-args+: :dbport (sql-get-config-prop config 'sql-port)
;; :header-args+: :database (sql-get-config-prop config 'sql-database)
;; :header-args+: :dbuser (sql-get-config-prop config 'sql-user)
;; :header-args+: :dbpassword (sql-get-config-prop config 'sql-password)
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

(defun sql-get-config-prop (config property)
  (cdr (assoc property config)))

(defun sqlcreds-setconfig (variable config)
  (set (make-local-variable variable) (sqlcreds-get-config config)))
