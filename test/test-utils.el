;;; test-utils.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  Kien Nguyen <kien.n.quang <at> gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'ert)
(load "bootstrap")
(require 'research)

(defun research-test--resolved-promise (value)
  "Return a resolved promise containing VALUE."
  (let ((promise (aio-promise)))
    (aio-resolve promise (-const value))
    promise))

(ert-deftest research-azure-cli-request-sends-and-caches-bearer-token ()
  (let ((research--azure-cli-token nil)
        (token-requests 0)
        command
        request-auths
        request-headers)
    (cl-letf (((symbol-function 'executable-find) (-const "az"))
              ((symbol-function 'research--exec)
               (lambda (&rest args)
                 (cl-incf token-requests)
                 (setq command args)
                 (research-test--resolved-promise
                  "{\"accessToken\":\"entra-token\",\"expires_on\":4102444800}")))
              ((symbol-function 'ghub-request)
               (lambda (_method _resource _params &rest args)
                 (push (plist-get args :auth) request-auths)
                 (push (plist-get args :headers) request-headers)
                 (funcall (plist-get args :callback) '(:count 0)))))
      (dotimes (_ 2)
        (should
         (equal
          (aio-wait-for
           (research--request "GET" "/_apis/projects"
                              :auth 'azure-cli
                              :host "dev.azure.com/fabrikam"
                              :forge 'azdev))
          '(:count 0))))
      (should (= token-requests 1))
      (should
       (equal command
              '("az" "account" "get-access-token"
                "--resource" "https://app.vssps.visualstudio.com/"
                "--output" "json")))
      (should (-all? (-partial #'eq 'none) request-auths))
      (should
       (-all?
        (lambda (headers)
          (equal (cdr (assoc "Authorization" headers))
                 "Bearer entra-token"))
        request-headers))
      (should
       (-all?
        (lambda (headers)
          (equal (cdr (assoc "X-TFS-FedAuthRedirect" headers))
                 "Suppress"))
        request-headers)))))

(ert-deftest research-azure-cli-request-refreshes-once-after-401 ()
  (let ((research--azure-cli-token nil)
        (attempts 0)
        (token-requests 0)
        authorization)
    (cl-letf (((symbol-function 'executable-find) (-const "az"))
              ((symbol-function 'research--exec)
               (lambda (&rest _)
                 (cl-incf token-requests)
                 (research-test--resolved-promise
                  (format
                   "{\"accessToken\":\"token-%d\",\"expires_on\":4102444800}"
                   token-requests))))
              ((symbol-function 'ghub-request)
               (lambda (_method _resource _params &rest args)
                 (cl-incf attempts)
                 (push
                  (cdr (assoc "Authorization"
                              (plist-get args :headers)))
                  authorization)
                 (if (= attempts 1)
                     (funcall (plist-get args :errorback)
                              '(error http 401 nil) nil nil nil)
                   (funcall (plist-get args :callback) '(:count 0))))))
      (should
       (equal
        (aio-wait-for
         (research--request "GET" "/_apis/projects"
                            :auth 'azure-cli
                            :host "dev.azure.com/fabrikam"
                            :forge 'azdev))
        '(:count 0)))
      (should (= attempts 2))
      (should (= token-requests 2))
      (should
       (equal (nreverse authorization)
              '("Bearer token-1" "Bearer token-2"))))))

(ert-deftest research-azure-cli-request-errors-after-second-401 ()
  (let ((research--azure-cli-token nil)
        (attempts 0)
        (token-requests 0))
    (cl-letf (((symbol-function 'executable-find) (-const "az"))
              ((symbol-function 'research--exec)
               (lambda (&rest _)
                 (cl-incf token-requests)
                 (research-test--resolved-promise
                  (format
                   "{\"accessToken\":\"token-%d\",\"expires_on\":4102444800}"
                   token-requests))))
              ((symbol-function 'ghub-request)
               (lambda (_method _resource _params &rest args)
                 (cl-incf attempts)
                 (funcall (plist-get args :errorback)
                          '(error http 401 nil) nil nil nil))))
      (should-error
       (aio-wait-for
        (research--request "GET" "/_apis/projects"
                           :auth 'azure-cli
                           :host "dev.azure.com/fabrikam"
                           :forge 'azdev))
       :type 'research-auth-error)
      (should (= attempts 2))
      (should (= token-requests 2)))))

(ert-deftest research-azure-cli-request-errors-without-az ()
  (let ((research--azure-cli-token nil))
    (cl-letf (((symbol-function 'executable-find) (-const nil)))
      (should-error
       (aio-wait-for
        (research--request "GET" "/_apis/projects"
                           :auth 'azure-cli
                           :host "dev.azure.com/fabrikam"
                           :forge 'azdev))
       :type 'research-auth-error))))

(ert-deftest research-azure-cli-request-rejects-non-ado-forges ()
  (should-error
   (aio-wait-for
    (research--request "GET" "/search/repositories"
                       :auth 'azure-cli
                       :host "api.github.com"
                       :forge 'github))
   :type 'research-auth-error))

(ert-deftest research-ado-request-errors-on-403-without-response-body ()
  (let ((attempts 0)
        logged-message
        signaled-message)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq logged-message
                       (apply #'format-message format-string args))))
              ((symbol-function 'ghub-request)
               (lambda (_method _resource _params &rest args)
                 (cl-incf attempts)
                 (funcall (plist-get args :errorback)
                          '(error http 403 "sensitive-response-body")
                          nil nil nil))))
      (condition-case err
          (aio-wait-for
           (research--request "GET" "/_apis/projects"
                              :auth 'token
                              :host "dev.azure.com/fabrikam"
                              :forge 'azdev))
        (research-auth-error
         (setq signaled-message (error-message-string err))))
      (should (= attempts 1))
      (should (string-match-p "403" logged-message))
      (should (string-match-p "403" signaled-message))
      (should-not (string-match-p "sensitive-response-body"
                                  logged-message))
      (should-not (string-match-p "sensitive-response-body"
                                  signaled-message)))))

(ert-deftest research-pat-request-preserves-ghub-authentication ()
  (let (request-auth request-headers)
    (cl-letf (((symbol-function 'ghub-request)
               (lambda (_method _resource _params &rest args)
                 (setq request-auth (plist-get args :auth)
                       request-headers (plist-get args :headers))
                 (funcall (plist-get args :callback) '(:count 0)))))
      (should
       (equal
        (aio-wait-for
         (research--request "GET" "/_apis/projects"
                            :auth 'token
                            :host "dev.azure.com/fabrikam"
                            :forge 'azdev))
        '(:count 0)))
      (should (eq request-auth 'token))
      (should-not (assoc "Authorization" request-headers))
      (should
       (equal (cdr (assoc "X-TFS-FedAuthRedirect" request-headers))
              "Suppress")))))

(ert-deftest research-github-request-uses-token-authentication-by-default ()
  (let ((research-default-auth-method 'azure-cli)
        (research--azure-cli-token nil)
        request-auth
        request-headers)
    (cl-letf (((symbol-function 'executable-find) (-const nil))
              ((symbol-function 'ghub-request)
               (lambda (_method _resource _params &rest args)
                 (setq request-auth (plist-get args :auth)
                       request-headers (plist-get args :headers))
                 (funcall (plist-get args :callback) '(:count 0)))))
      (should
       (equal
        (aio-wait-for
         (research--request "GET" "/search/repositories"
                            :host "api.github.com"
                            :forge 'github))
        '(:count 0)))
      (should (eq request-auth 'token))
      (should-not (assoc "Authorization" request-headers)))))

(ert-deftest research-init-warns-when-azure-cli-is-unavailable ()
  (let ((research-default-auth-method 'azure-cli)
        (research-recipes-file (make-temp-name
                                (expand-file-name "research-test-"
                                                  temporary-file-directory)))
        (research-cols-file (make-temp-name
                             (expand-file-name "research-test-"
                                               temporary-file-directory)))
        warning)
    (cl-letf (((symbol-function 'executable-find) (-const nil))
              ((symbol-function 'display-warning)
               (lambda (_type message &rest _)
                 (setq warning message))))
      (research-init)
      (should
       (string-match-p "Azure CLI executable `az` is unavailable"
                       warning)))))

(ert-deftest research-init-does-not-warn-for-token-auth ()
  (let ((research-default-auth-method 'token)
        (research-recipes-file (make-temp-name
                                (expand-file-name "research-test-"
                                                  temporary-file-directory)))
        (research-cols-file (make-temp-name
                             (expand-file-name "research-test-"
                                               temporary-file-directory)))
        warned)
    (cl-letf (((symbol-function 'executable-find) (-const nil))
              ((symbol-function 'display-warning)
               (lambda (&rest _) (setq warned t))))
      (research-init)
      (should-not warned))))

(ert-deftest research-init-does-not-validate-authentication ()
  (let ((research-default-auth-method 'cookie)
        (research-recipes-file (make-temp-name
                                (expand-file-name "research-test-"
                                                  temporary-file-directory)))
        (research-cols-file (make-temp-name
                             (expand-file-name "research-test-"
                                               temporary-file-directory))))
    (research-init)))

(ert-deftest research-request-rejects-unsupported-authentication-at-use ()
  (let ((research-default-auth-method 'cookie))
    (should-error
     (aio-wait-for
      (research--request "GET" "/_apis/projects"
                         :host "dev.azure.com/fabrikam"
                         :forge 'azdev))
     :type 'research-auth-error)))

(ert-deftest research-byte-compilation-test ()
  (seq-doseq (library (-filter
                       (lambda (file)
                         (and (f-ext? file "el")
                              (not (string-match-p "test" file))
                              (not (string-match-p "obsolete" file))))
                       (append (when (or load-file-name buffer-file-name)
                                 (f-files (f-parent (f-dirname (or load-file-name buffer-file-name)))))
                               (f-files default-directory))))
    (let ((byte-compile-error-on-warn t))
      (message "Testing file %s" library)
      (should (byte-compile-file (save-excursion
                                   (find-library library)
                                   (buffer-file-name)))))))

;;; test-utils.el ends here
