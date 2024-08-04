;;; auth-source-kwallet.el --- KWallet integration for auth-source -*- lexical-binding: t; -*-

;;; Copyright (C) 2020 Ekaterina Vaartis
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Author: Ekaterina Vaartis <vaartis@kotobank.ch>
;;; Created: 13 Dec 2020
;;; URL: https://github.com/vaartis/auth-source-kwallet

;;; Package-Requires: ((emacs "27.1"))

;;; Version: 1.0.0

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

(defun auth-source-kwallet-list (&optional folder wallet)
  "Return a list of elements stored in kwallet.
If FOLDER is nil, use `auth-source-kwallet-folder'.  If it is \"\" then return a
list of folders.

If WALLET is nil, use `auth-source-kwallet-wallet', else use the specified
wallet."
  (split-string
   (shell-command-to-string
    (format "%s %s -f %s -l"
            (executable-find auth-source-kwallet-executable)
            (shell-quote-argument
             (or wallet auth-source-kwallet-wallet))
            (shell-quote-argument
             (or folder auth-source-kwallet-folder))))))

(defun auth-source-kwallet-read (label &optional folder wallet)
  "Read a secret from kwallet.
LABEL is the label to search in kwallet.
FOLDER and WALLET are optional.  If not provided, `auth-source-kwallet-folder'
and `auth-source-kwallet-wallet' are used respectively."
  (let ((str-result (string-trim
                     (shell-command-to-string (format "%s %s -f %s -r %s"
                                                      auth-source-kwallet-executable
                                                      (shell-quote-argument
                                                       (or wallet auth-source-kwallet-wallet))
                                                      (shell-quote-argument
                                                       (or folder auth-source-kwallet-folder))
                                                      (shell-quote-argument label))))))
    (condition-case nil
        (json-parse-string str-result :object-type 'alist)
      (json-parse-error str-result)
      (json-trailing-content str-result))))

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
  (if (executable-find auth-source-kwallet-executable)
      (let ((got-secret (if list
                            (auth-source-kwallet-list folder wallet)
                          (auth-source-kwallet-read (or label
                                                        (concat user
                                                                auth-source-kwallet-key-separator
                                                                host))
                                                    folder wallet))))
        (list (list :user (or user label)
                    :secret got-secret)))
    ;; If not executable was found, return nil and show a warning
    (warn (format "`auth-source-kwallet': Could not find executable '%s' to query KWallet"
                  auth-source-kwallet-executable))))

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

  (advice-add 'auth-source-backend-parse
              :before-until
              #'auth-source-kwallet--kwallet-backend-parse)
  (add-to-list 'auth-sources 'kwallet)
  (auth-source-forget-all-cached))

(provide 'auth-source-kwallet)

;;; auth-source-kwallet.el ends here
