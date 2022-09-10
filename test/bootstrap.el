;;; bootstrap.el ---                             -*- lexical-binding: t; -*-

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
(require 'package)

(let* ((package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu-devel"    . "https://elpa.gnu.org/devel/")
                           ("gnu-stable"   . "https://elpa.gnu.org/packages/")
                           ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))
       (no-byte-compile t)
       (user-emacs-directory (expand-file-name (make-temp-name ".emacs.d")
                                               "~"))
       (package-user-dir (expand-file-name (make-temp-name "tmp-elpa")
                                           user-emacs-directory))
       (custom-file (expand-file-name "custom.el" package-user-dir))
       (deps '( aio helm dash f compat
                (ghub :fetcher github :repo "kiennq/ghub" :files ("lisp/*.el" "docs/ghub.texi")))))
  (package-refresh-contents)
  (package-initialize)

  ;; bootstrap quelpa
  (unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))

  (mapc (lambda (pkg)
          (if (listp pkg)
              (quelpa pkg)
            (package-install pkg))
          (require (if (listp pkg) (car pkg) pkg)))
        deps)
  (add-hook 'kill-emacs-hook `(lambda ()
                                (delete-directory ,user-emacs-directory t))))
