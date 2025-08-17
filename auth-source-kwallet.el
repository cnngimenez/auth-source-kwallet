;;; auth-source-kwallet.el --- KWallet integration for auth-source -*- lexical-binding: t; -*-

;;; Copyright (C) 2020 Ekaterina Vaartis
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Author: Ekaterina Vaartis <vaartis@kotobank.ch>
;;; Created: 13 Dec 2020
;;; URL: https://github.com/vaartis/auth-source-kwallet

;;; Package-Requires: ((emacs "27.1"))

;;; Version: 0.0.2

;;; Commentary:
;; This package adds kwallet support to auth-source by calling
;; kwallet-query from the command line.

;;; Code:

(require 'auth-source)

(defgroup auth-source-kwallet nil
  "KWallet auth source settings."
  :group 'external
  :tag "auth-source-kwallet"
  :prefix "kwallet-")

(defcustom auth-source-kwallet-wallet "Passwords"
  "KWallet wallet to use."
  :type 'string
  :group 'auth-source-kwallet)

(defcustom auth-source-kwallet-folder "Passwords"
  "KWallet folder to use."
  :type 'string
  :group 'auth-source-kwallet)

(defcustom auth-source-kwallet-key-separator "@"
  "Separator to use between the user and the host for KWallet."
  :type 'string
  :group 'auth-source-kwallet)

(defcustom auth-source-kwallet-executable "kwallet-query"
  "Executable used to query kwallet."
  :type 'string
  :group 'auth-source-kwallet)

(defun auth-source-kwallet-error-output-p (str)
  "True if STR is an error output from KWallet.
STR can be a list (EXIT-STATUS OUTPUT) or an output error string."
  (or (and (listp str) (not (zerop (car str))))
      (and (stringp str) (or
                          (string-match-p "^The folder .* does not exist!.*" str)
                          (string-match-p "^Failed to read entry .*" str)
                          (string-match-p "^Wallet .* not found$" str)))))

(defun auth-source-kwallet-call-subprocess (&optional user host label list folder wallet)
  "Call kwallet-query executable.

Call kwallet-query executable and return a list such as (EXIT-STATUS OUTPUT),
with the program exit status and its answer unprocessed respectivelly.  Beware
that when kwallet-query returns a non-zero exit status (an error ocurred), its
output may contain an error message.

If USER and HOST are provided, use it to search for the query.  See
`auth-source-kwallet-key-separator' to modify the user-host separator.  If LABEL
is provided, it takes precedence and is searchead literaly without modification.
Label searching is useful for LUKS devices or other non host, FTP, or Web-sites
 user@host passwords.

If LIST is t, it sends the list parameter to the program.  In this case, USER,
HOST, and LABEL arguments are ignored.

The WALLET argument is used if not nil.  Else, `auth-source-kwallet-wallet' is
used instead by default.

The FOLDER argument is used if provided.  Else, `auth-source-kwallet-folder' is
used by default.

Examples:
- List entries in \"My Folder\" folder:
\\=(auth-source-kwallet-call-subprocess nil nil nil t nil \"My Folder\")

- Get the entry with label \"6c4c880d-98aa-452a-a1a3-13e73f044d3a\" (a disk
  UUID partition) at \"SolidLuks\" folder:
\\=(auth-source-kwallet-call-subprocess nil nil
\"6c4c880d-98aa-452a-a1a3-13e73f044d3a\" nil nil \"SolidLuks\")

This function warns if the executable is not found, and returns nil."
  (if (executable-find auth-source-kwallet-executable)
      (let ((output-buffer (generate-new-buffer "*kwallet-output*")))
        (unwind-protect
            (let ((exit-status (call-process auth-source-kwallet-executable
                                             nil output-buffer nil
                                             (or wallet auth-source-kwallet-wallet)
                                             "-f" (or folder auth-source-kwallet-folder)
                                             (if list "-l" "-r")
                                             ;; There's no way to remove an argument (except using macros) when using "-l".
                                             ;; The only way is by repeating the same argument ("-l").
                                             (if list "-l"
                                               (or label
                                                   (concat user auth-source-kwallet-key-separator host))))))
              (list exit-status
                    (with-current-buffer output-buffer
                      (string-trim (buffer-string)))))
              (kill-buffer output-buffer)))
    ;; else, if not executable was found, return nil and show a warning
    (warn (format "`auth-source-kwallet': Could not find executable '%s' to query KWallet"
                  auth-source-kwallet-executable))))

(defun auth-source-kwallet-list (&optional folder wallet)
  "Return a list of elements stored in kwallet.
If FOLDER is nil, use `auth-source-kwallet-folder'.  If it is \"\" then return a
list of folders.

If WALLET is nil, use `auth-source-kwallet-wallet', else use the specified
wallet."
  (let ((str-result (auth-source-kwallet-call-subprocess nil nil nil t folder wallet)))
    (if (auth-source-kwallet-error-output-p str-result)
        (progn ;; An error ocurred, report it.
          (warn "`auth-source-kwallet': KWallet-query program returned with status error %s and output \"%s\"."
                (car str-result) (cadr str-result))
          nil)
      (split-string (cadr str-result)))))

(defun auth-source-kwallet-read (label &optional folder wallet)
  "Read a secret from kwallet.
LABEL is the label to search in kwallet.
FOLDER and WALLET are optional.  If not provided, `auth-source-kwallet-folder'
and `auth-source-kwallet-wallet' are used respectively."
  (let ((str-result (auth-source-kwallet-call-subprocess nil nil label nil folder wallet)))
    (if (auth-source-kwallet-error-output-p str-result)
        (progn (warn "`auth-source-kwallet': KWallet-query program returned with status error %s and output \"%s\"."
                     (car str-result) (cadr str-result))
               nil)
      (condition-case nil
          (json-parse-string (cadr str-result) :object-type 'alist)
        (json-parse-error (cadr str-result))
        (json-trailing-content (cadr str-result))))))

(cl-defun auth-source-kwallet--kwallet-search (&rest spec
                                                &key _backend _type host user _port
                                                folder wallet label list
                                                &allow-other-keys)
  "Search KWallet for the specified user and host.
SPEC, BACKEND, TYPE, HOST, USER and PORT are as required by auth-source.
Other key parameters are FOLDER, WALLET, and LABEL.  These are kwallet specific
parameters.

If FOLDER is not provided, then `auth-source-kwallet-folder' is used.
Similarly, if WALLET is nil or not provided, `auth-source-kwallet-wallet' is
used.

HOST and USER are used to compose the kwallet search query.  If LABEL is
provided, it is used instead.

Listing the folders is possible when LIST is t.  In such case, WALLET is the
only valid keys."
  (let ((got-secret (if list
                        (auth-source-kwallet-list folder wallet)
                      (auth-source-kwallet-read (or label
                                                    (concat user
                                                            auth-source-kwallet-key-separator
                                                            host))
                                                folder wallet))))
    (list (list :user (or user label)
                :secret got-secret))))

(defun auth-source-kwallet--kwallet-backend-parse (entry)
  "Parse the entry to check if this is a kwallet entry.
ENTRY is as required by auth-source."
  (when (eq entry 'kwallet)
    (auth-source-backend-parse-parameters
     entry
     (auth-source-backend
      :source "KWallet"
      :type 'kwallet
      :search-function #'auth-source-kwallet--kwallet-search))))

;;;###autoload
(defun auth-source-kwallet-enable ()
  "Enable the kwallet auth source."

  ;; (advice-add 'auth-source-backend-parse
  ;;             :before-until
  ;;             #'auth-source-kwallet--kwallet-backend-parse)
  ;;
  ;; In auth-source.el, their authors add a new parser by using the
  ;; `auth-source-backend-parser-functions' hook, instead of advising the
  ;; `auth-source-backend-parse'.  Just to be sure, we do the same as them :).
  
  (add-hook 'auth-source-backend-parser-functions #'auth-source-kwallet--kwallet-backend-parse)
  (add-to-list 'auth-sources 'kwallet)
  (auth-source-forget-all-cached))

(provide 'auth-source-kwallet)

;;; auth-source-kwallet.el ends here
