;;; research.el --- research integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Park Jiin; Kien Nguyen
;; Maintainer: Park Jiin; Kien Nguyen
;; Version: 3.1
;; Keywords: research, source
;; Package-Requires: ((emacs "26.1") (aio "1.0") (dash "2.19.1") (ghub))

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; This package only works in Windows (or WSL)
;;; Code:

(require 'pulse)
(require 'aio)
(require 'cl-lib)
(require 'files)
(require 'browse-url)
(require 'url)
(require 'rx)
(require 'pulse)
(require 'dash)
(require 'ghub)
(eval-when-compile
  (require 'subr-x))

(defgroup research nil
  "reSearch group."
  :group 'source-grep
  :prefix "research-")

(defcustom research-pulse-at-cursor t
  "If non-nil, pulse at point after jumping."
  :group 'research
  :type 'boolean)

(defconst research--cache-path (expand-file-name ".cache/research"
                                                 user-emacs-directory)
  "Path to store research's cache files.")

(defcustom research-recipes-file (expand-file-name "repos" research--cache-path)
  "File to save the repo's recipes."
  :group 'research
  :type 'file)

(defcustom research-cols-file (expand-file-name "collections" research--cache-path)
  "File to cache the available repos."
  :group 'research
  :type 'file)

(defcustom research-default-auth-method 'token
  "Default method for authentication."
  :group 'research
  :type `(choice (const :tag "Use token for authentication." token)
                 (const :tag "Use imported cookies for authenitcation." cookie)))

(defvar research-debug nil "Enable debug mode.")
(defvar research--conn nil "ReSearchCLI jsonrpc connection.")

(declare-function evil-set-command-property "ext:evil-common")

(defmacro research-destruct (&rest structs)
  "Generate methods that allow to use STRUCTS in dash bindings."
  (with-case-table ascii-case-table
    (->> structs
         (-map (lambda (struct)
                 (cl-list*
                  `(defun ,(intern (format "dash-expand:&%s" struct)) (key source)
                     (let ((f (intern (format "%s-%s"
                                              ,(symbol-name struct)
                                              (substring (symbol-name key) 1)))))
                     `(,f ,source))))))
         (macroexp-progn))))

(eval-and-compile
  (research-destruct url))

;; AzureDevOps
(cl-defmethod ghub--username (_host (_forge (eql 'azdev)))
  ""
  "azdev")

(cl-defmethod ghub--auth (host auth username (forge (eql 'azdev)))
  "Authentication header for AzDev."
  (unless username
    (setq username (ghub--username host forge)))
  (if (eq auth 'basic)
      (cons "Authorization" (ghub--basic-auth host username))
    (cons "Authorization"
          (concat "Basic "
                  (base64-encode-string
                   (concat ":" (ghub--token host username auth nil forge))
                   t)))))

;; GitHub Codesearch
(cl-defmethod ghub--username (_host (_forge (eql 'cs-github)))
  ""
  "cs-github")

(aio-defun research--url-get-available (url)
  "Get first valid sub-domain of URL."
  (-let* ((urlobj (url-generic-parse-url url))
          ((&url :type :host) urlobj)
          (check-url (aio-lambda (url)
                       (-> (list (aio-url-retrieve url)
                                 (aio-sleep 1 '((:error 408))))
                           (aio-make-select)
                           (aio-select)
                           (aio-await)
                           (aio-await))))
          (((&plist :error) . _) (->> (format "%s://%s" type host)
                                      (funcall check-url)
                                      (aio-await))))
    (while error
      (setq host (-some->> (string-search "." host) (+ 1) (substring host)))
      (setq error (-> (funcall check-url (format "%s://%s" type host))
                      (aio-await)
                      (car)
                      (plist-get :error))))
    (when host
      (setf (url-host urlobj) host)
      (url-recreate-url urlobj))))

(cl-defmethod ghub--auth (host (_auth (eql 'cookie)) _user _forge)
  "Authentication header with HOST using cookie."
  (when-let* ((pred1 url-setup-done)
              (default-exp "12/31/2099")
              ;; (url (format "https://%s" host))
              (url (format "https://%s" host))
              (urlobj (url-generic-parse-url url))
              (host (url-host urlobj))
              (domain (url-domain urlobj))
              (path (let ((raw-path (car (url-path-and-query urlobj))))
                      (if (> (length raw-path) 0) raw-path "/")))
              (pred2 (not (url-cookie-retrieve domain path 'secure)))
              (url (or (aio-wait-for (research--url-get-available url)) url)))
    (read-from-minibuffer
     (format "The current cookies need refresh. Press ENTER to open %s for verification." url))
    (browse-url url)
    (let* ((ghub-json-object-type 'plist)
           (ghub-json-array-type 'array)
           (ghub-json-null-object nil)
           (ghub-json-false-object nil))
      (->> (ghub--json-parse-string
            (read-from-minibuffer
             "Login and enter the json cookies (via Cookie-Editor) from the opened website: "))
           (mapc (-lambda ((&plist :name :value :expirationDate :domain :path :secure))
                   (url-cookie-store name value
                                     (pcase expirationDate
                                       ((pred numberp)
                                        (format-time-string "%FT%T%z" (seconds-to-time expirationDate)))
                                       (_ (format "%s" expirationDate)))
                                     domain path secure)))))
    ;; Mark the host cookies have been imported
    (url-cookie-store "__imported_" "1" default-exp domain path 'secure)
    (setq url-cookies-changed-since-last-save t)
    (url-cookie-write-file))
  '("Authorization" . ""))

(cl-defun research--comp-read (prompt collection
                                      &key
                                      category
                                      predicate
                                      initial-input
                                      default
                                      require-match
                                      history)
  "`completing-read' thats return `cdr' in case collection is an alist.
HISTORY is the variable that holds input history.
PROMPT COLLECTION CATEGORY PREDICATE INITIAL-INPUT DEFAULT REQUIRE-MATCH."
  (let* ((collection (delete-dups (append collection (symbol-value history))))
         (result (completing-read prompt
                                  (lambda (str pred action)
                                    (cond
                                     ((equal action 'metadata)
                                      `(metadata (category . ,category)))
                                     (t
                                      (complete-with-action action collection str pred))))
                                  predicate require-match initial-input
                                  history default)))
    (or (cdr (assoc result collection))
        result)))

(defsubst research--debug (msg &optional erase? popup?)
  "Log debug MSG to <*reSearch debug*> buffer.
ERASE? will clear the log buffer, and POPUP? wil switch to it."
  (when research-debug
    (with-current-buffer (get-buffer-create "*reSearch debug*")
      (if erase? (erase-buffer))
      (insert (format "%s\n" msg))
      (if popup? (pop-to-buffer (current-buffer))))))

(defun research--restore (file)
  "Restore saved objected from FILE."
  (when (file-exists-p file)
    (car (read-from-string
          (let ((coding-system-for-read 'utf-8-auto-dos))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (setq buffer-file-coding-system 'no-conversion)
              (insert-file-contents file)
              (buffer-substring-no-properties (point-min) (point-max))))))))

(defun research--save (file obj)
  "Save OBJ to FILE."
  (let ((print-length nil)
        (print-level nil)
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory file) t)
    (write-region (prin1-to-string obj) nil file nil :silent)
    obj))

(cl-defgeneric research--refresh-auth (_host _auth _forge)
  "Try to re-authenticate for HOST with AUTH method of FORGE."
  nil)

(cl-defmethod research--refresh-auth (host (_auth (eql 'cookie)) _forge)
  "Try to re-authenticate via cookie for HOST of FORGE."
  (when-let* ((url (format "https://%s" host))
              (urlobj (url-generic-parse-url url))
              (host (url-host urlobj))
              (path (let ((raw-path (car (url-path-and-query urlobj))))
                      (if (> (length raw-path) 0) raw-path "/")))
              (domain (url-domain urlobj))
              (has-domain? (url-cookie-retrieve domain path 'secure)))
    (url-cookie-delete-cookies domain)
    t))

;; TODO: create a class/truct so that we can use method specialization defmethod
(cl-defun research--request (method resource
                                    &key query payload headers reader auth host forge)
  "Wrapper of `ghub-request' in async form."
  (let ((promise (aio-promise))
        (ghub-json-object-type 'plist)
        (ghub-json-array-type 'array)
        (ghub-json-null-object nil)
        (ghub-json-false-object nil)
        (auth (or auth research-default-auth-method 'token)))
    (when (> 0 (prefix-numeric-value current-prefix-arg))
      (research--refresh-auth host auth forge))
    (ghub-request method resource nil
                  :query query
                  :payload payload
                  :headers headers
                  :reader reader
                  :auth auth
                  :host host
                  :forge forge
                  :callback (lambda (result &rest _) (aio-resolve promise (-const result)))
                  :errorback
                  (lambda (err _header _status req &rest _)
                    (let* ((err-type (cl-second err))
                           (err-code (cl-third err))
                           (err-detail (cl-fourth err))
                           (err (if (eq err-type 'http)
                                    (list 'http-error err-code
                                          (nth 2 (assq err-code url-http-codes))
                                          (when req (url-filename (ghub--req-url req)))
                                          err-detail)
                                  err)))
                      (message "%s::%s" (propertize "Research" 'face 'error) err)
                      (pcase err-code
                        ((or 401 404 500)
                         (if (research--refresh-auth host auth forge)
                             (aio-with-async
                               (aio-resolve
                                promise
                                (-const (aio-await (research--request
                                                    method resource
                                                    :query query
                                                    :payload payload
                                                    :headers headers
                                                    :reader reader
                                                    :auth auth
                                                    :host host
                                                    :forge forge)))))
                           (aio-resolve promise (-const nil))))
                        (_ (aio-resolve promise (-const nil)))))))
    promise))

(aio-defun research--shell (command)
  "Asynchronously execute shell command COMMAND and return its output string."
  (-let ((promise (aio-promise))
         (buf (generate-new-buffer " *research-shell-command*")))
    (set-process-sentinel
     (start-process-shell-command "research-shell-command" buf command)
     (lambda (proc _signal)
       (when (memq (process-status proc) '(exit signal))
         (with-current-buffer buf
           (let ((data (buffer-string)))
             (aio-resolve promise
                          (-const (when (> (length data) 0) (substring data 0 -1))))))
         (kill-buffer buf))))
    (aio-await promise)))

(defsubst research--encode-url (url)
  "Call `url-encode-url' for URL."
  (pcase url
    ((pred stringp) (url-encode-url url))
    (_ url)))

(cl-defstruct research--rcp
  "Recipe for the repository."
  (id)
  (org)
  (repo))

(cl-defstruct (research--az-rcp (:constructor research--az-rcp-new (&key org project repo))
                                (:include research--rcp (id (substring-no-properties
                                                             (format "AzDev/%s/%s/%s" org
                                                                     (or project "_")
                                                                     (or repo "_"))))))
  (project))

(cl-defstruct (research--gh-rcp (:constructor research--gh-rcp-new (&key org repo))
                                (:include research--rcp (id (substring-no-properties
                                                             (format "GitHub/%s/%s" org
                                                                     (or repo "_")))))))

(cl-defstruct research--repo
  "Base type for the repository that we search on."
  (name nil :documentation "Display name." :read-only t)
  ;; rcp can contain empty data, dont rely on it
  (rcp nil :read-only t)
  (root nil :documentation "The path to repo's code.")
  (skip-calc-pos t))

(cl-defstruct (research--az-repo (:constructor research--az-repo-new)
                                 (:include research--repo))
  (branch))

(cl-defstruct (research--gh-repo (:constructor research--gh-repo-new)
                                 (:include research--repo)))

(eval-and-compile
  (research-destruct research--rcp
                     research--az-rcp
                     research--gh-rcp
                     research--repo
                     research--az-repo
                     research--gh-repo))

(defvar research--rcps nil
  "Repo's recipres.")

(defvar research--collections nil
  "List of cached `research--repo'.")

(cl-defgeneric research--get-collections (repo-rcp)
  "Get collections of REPO-RCP.")

(cl-defmethod research--get-collections ((repo-rcp research--az-rcp))
  ""
  (aio-with-async
    (-let* (((&research--az-rcp :id rcp-id :org :project :repo) repo-rcp)
            (col (aio-await
                  (research--request "GET" (format "/%s/_apis/search/status/repositories/%s"
                                                   (research--encode-url project)
                                                   (research--encode-url repo))
                                     :host (format "almsearch.dev.azure.com/%s" (research--encode-url org))
                                     :forge 'azdev)))
            ((&plist :indexedBranches indexed-branches) col))
      (nconc
       `(,(let ((rcp (research--az-rcp-new :org org)))
            (research--az-repo-new
             :name (substring-no-properties (research--az-rcp-id rcp))
             :rcp rcp
             :skip-calc-pos 'undefined))
         ,(let ((rcp (research--az-rcp-new :org org :project project)))
            (research--az-repo-new
             :name (substring-no-properties (research--az-rcp-id rcp))
             :rcp rcp
             :skip-calc-pos 'undefined)))
       (when (> (length indexed-branches) 1)
         `(,(research--az-repo-new
             :name (substring-no-properties (format "%s" rcp-id))
             :rcp repo-rcp
             :skip-calc-pos 'undefined)))
       (mapcar (-lambda ((&plist :name branch))
                 (research--az-repo-new
                  :name (substring-no-properties (format "%s/%s" rcp-id branch))
                  :rcp repo-rcp
                  :skip-calc-pos 'undefined
                  :branch branch))
               indexed-branches)))))

(cl-defmethod research--get-collections ((repo-rcp research--gh-rcp))
  ""
  (aio-with-async
    (-let* (((&research--gh-rcp :id rcp-id :org :repo) repo-rcp)
            (col (aio-await
                  (research--request "GET" "/search/repositories"
                                     :headers '(("Accept" . "application/vnd.github.v3+json"))
                                     :query `((q . ,(format "repo:%s/%s fork:true" org repo)))
                                     :host "api.github.com"
                                     :forge 'github)))
            ((&plist :items) col))
      (mapcar (-lambda ((&plist :id _))
                (research--gh-repo-new
                 :name (substring-no-properties (format "%s" rcp-id))
                 :skip-calc-pos 'undefined
                 :rcp repo-rcp))
              items))))

;; (cl-defmethod research--get-collections ((repo-rcp research--gh-rcp))
;;   (aio-with-async
;;     (-when-let* (((&research--gh-rcp :org :repo) repo-rcp)
;;                  (col (aio-await
;;                        (research--request "GET" (format "/api/repos/%s/%s"
;;                                                         (research--encode-url org)
;;                                                         (research--encode-url repo))
;;                                           :auth 'cookie
;;                                           :host "cs.github.com"
;;                                           :forge 'cs-github)))
;;                  ((&plist :RepoID id) col))
;;       (list
;;        (research--gh-repo-new
;;         :name (format "GitHub/%s/%s" org repo)
;;         :id id
;;         :skip-calc-pos 'undefined
;;         :rcp repo-rcp)))))

(defvar research--inuse-collections nil)

;;;###autoload
(defun research-set-collection (&optional option)
  "Set a collection as query list target.
Optionally, with OPTION add a collection instead.
- `\\[universal-argument] \\[universal-argument] \\[research-set-collection]' will add new collection
from refreshed collections instead of cached one.
- `\\[universal-argument] \\[research-set-collection]' will add new collection
into query list target."
  (interactive "p")
  (aio-with-async
    (let* ((force (equal option 16))
           (add-new (equal option 4))
           (collections (or (and (not force) research--collections)
                            (setq research--collections
                                  (research--save
                                   research-cols-file
                                   (mapcar (lambda (col)
                                             `(,(research--repo-name col) . ,col))
                                           (let ((repos (mapcar
                                                         (aio-lambda (rcp)
                                                           (aio-await (research--get-collections rcp)))
                                                         research--rcps)))
                                             (aio-await (aio-all repos))
                                             (apply #'nconc (mapcar #'aio-wait-for repos)))))))))
      (setq research--inuse-collections
            (append
             (when add-new research--inuse-collections)
             `(,(research--comp-read
                 (format "%s Collection [%s]: "
                         (cond (add-new "Add")
                               (t ""))
                         (mapcar #'research--repo-name research--inuse-collections))
                 (if (vectorp collections) (append collections nil) collections)
                 :require-match t
                 :initial-input
                 (when (and (not research--collections)
                            (executable-find "SourceControl.Git.ShellAdapter"))
                   (aio-await (research--shell "SourceControl.Git.ShellAdapter GetOfficialBranch"))))))))))

(defvar research--repo-orgs nil)

(aio-defun research--add-recipe (recipe)
  "Add repository with RECIPE into search collections."
  (research--save research-recipes-file
                  (setq research--rcps (add-to-list 'research--rcps recipe)))
  (research--save research-cols-file
                  (delete-dups
                   (setq research--collections
                         (nconc research--collections
                                (mapcar (lambda (col) `(,(research--repo-name col) . ,col))
                                        (aio-await (research--get-collections recipe))))))))

(cl-defgeneric research--add-repo (type)
  "Add a repository of type TYPE into search collections.")

(cl-defmethod research--add-repo ((_type (eql 'azdev)))
  (aio-with-async
    (let* ((org (research--comp-read "Org: " nil :history 'research--repo-orgs))
           (project (research--comp-read
                     "Project: "
                     (mapcar (-rpartial #'plist-get :name)
                             (-> (aio-await (research--request
                                             "GET" "/_apis/projects"
                                             :host (format "dev.azure.com/%s" (research--encode-url org))
                                             :forge 'azdev))
                                 (plist-get :value)))))
           (repo (research--comp-read
                  "Repository: "
                  (mapcar (-rpartial #'plist-get :name)
                          (-> (aio-await (research--request
                                          "GET" (format "/%s/_apis/git/repositories"
                                                        (research--encode-url project))
                                          :host (format "dev.azure.com/%s" (research--encode-url org))
                                          :forge 'azdev))
                              (plist-get :value))))))
      (aio-await (research--add-recipe
                  (research--az-rcp-new :org org :project project :repo repo))))))

(cl-defmethod research--add-repo ((_type (eql 'github)))
  (aio-with-async
    (let* ((org (research--comp-read "Org: " nil :history 'research--repo-orgs))
           (repo (research--comp-read
                  "Repository: "
                  (mapcar (-rpartial #'plist-get :name)
                          (-> (aio-await (research--request
                                          "GET" "/search/repositories"
                                          :query `((q . ,(format "user:%s fork:true" org))
                                                   (per_page . 100))
                                          :headers '(("Accept" . "application/vnd.github.v3+json"))
                                          :host "api.github.com"))
                              (plist-get :items))))))
    (aio-await (research--add-recipe
                (research--gh-rcp-new :org org :repo repo))))))

;;;###autoload
(defun research-add-repo (type)
  "Add a repository of TYPE into search collections."
  (interactive (list (research--comp-read "Type: " `(("Azure DevOps"    . azdev)
                                                     ("GitHub"          . github))
                                          :require-match t)))
  (research--add-repo type))

;;;###autoload
(defun research-remove-repo (repo)
  "Remove recipe REPO from search collections."
  (interactive (list (research--comp-read "Repo: "
                                          (mapcar (lambda (rcp) `(,(research--rcp-id rcp) . ,rcp))
                                                  research--rcps)
                                          :require-match t)))
  (delete repo research--rcps))

;; retain query history for its own buffer
(defvar research--query-history nil)

(defun research-clear-query-history ()
  "Clear query history."
  (interactive)
  (setq research--query-history nil))

(with-eval-after-load 'evil
  (evil-set-command-property 'research-query :jump t)
  (evil-set-command-property 'research-next-in-buffer :jump t))

(cl-defstruct research--code-result
  "The code result"
  (path nil :documentation "Path to current result.")
  (url nil :documentation "URL to the current result.")
  (id nil :documentation "Version id.")
  (org)
  (repo)
  (repo-metadata)
  (matches nil :type list :documentation "List of offsets"))

(cl-defstruct (research--az-code-result (:constructor research--az-code-result-new)
                                        (:include research--code-result))
  (project)
  (content-id nil)
  (type nil))

(cl-defstruct (research--gh-code-result (:constructor research--gh-code-result-new)
                                        (:include research--code-result)))

(eval-and-compile
  (research-destruct research--code-result
                     research--az-code-result
                     research--gh-code-result))

(defun research--az-get-info (info)
  "Return info description of INFO code."
  (pcase info
    (0 nil)
    (1 "Account is being reindexed")
    (2 "Account indexing has not started.");
    (3 "Invalid Request.");
    (4 "Prefix wildcard query not supported.");
    (5 "MultiWords with code facet not supported.");
    (6 "Account is being onboarded.");
    (7 "Account is being onboarded or reindexed.");
    (8 "Top value trimmed to maxresult allowed.");
    (9 "Branches are being indexed. You may see partial search results from those branches.");
    (10 "Faceting not enabled.");
    (11 "Work items not accessible.");
    (15 nil)
    (16 "Your search may be returning no results because of a scale issue with wildcard searches on the reSearch backend. You may be able to fix this by prefixing your wildcard with more characters to limit the number of possible terms it could expand into.");
    (17 nil)
    (19 "Phrase queries with code type filters not supported.");
    (20 "Wildcard queries with code type filters not supported.");
    (24 "Wildcard queries may return partial results. Current ADO limit is 100 results. Try prefixing wildcard with more characters to limit the number of results.");
    (31 "Prefix sub-string too short.");
    ((or 32 33) "Search queries with 2 or more wildcards (including a infix wildcard) like *ab*cd*, *abc*de* are not supported.")
    (34 "Substring search queries (double wildcard) in a code element filter like class:*abc*, struct:*qwe* are not supported.");
    (35 "Substring search queries (double wildcard) with '?' or a combination of '?' and '*' like *abc? , ?abc? , ?abc* is not supported. You can try using '*' instead of '?'")
    (_ (format "Unexpected info code of `%s'. Search directly on ADO might reveal the issue." info))))

(cl-defgeneric research--query (repo query page max-result)
  "Query QUERY to repository REPO with page PAGE.
Return at most MAX-RESULT items.")

(cl-defmethod research--query ((collection research--az-repo) query page max-result)
  ""
  (aio-with-async
    (-let* (((&research--az-repo
              :branch
              :rcp (&research--az-rcp :org :project rcp-proj :repo rcp-repo))
             collection)
            (res (aio-await (research--request
                             "POST" "/_apis/search/advancedCodeSearchResults"
                             :query '((api-version . "6.0-preview.1"))
                             :payload `( :$top ,max-result
                                         :$skip ,(* (max 0 (1- page)) max-result)
                                         :searchText ,query
                                         :filters ,(append
                                                    (when rcp-proj `(:project [,rcp-proj]))
                                                    (when rcp-repo `(:repository [,rcp-repo]))
                                                    (when branch `(:branch [,branch]))))
                             :host (format "almsearch.dev.azure.com/%s" (research--encode-url org))
                             :forge 'azdev)))
            ((&plist :results :infoCode info) res)
            (files (mapcar (-lambda ((&plist :path
                                             :contentId content-id
                                             :versions [(&plist :branchName branch
                                                                :changeId change-id)]
                                             :repository (&plist :name repo)
                                             :project (&plist :name proj)
                                             :matches :repository))
                             (research--az-code-result-new
                              :path path
                              :url (format "https://dev.azure.com/%s/%s/_git/%s?path=%s&version=GB%s"
                                           (research--encode-url org)
                                           (research--encode-url proj)
                                           (research--encode-url repo)
                                           (research--encode-url path)
                                           (research--encode-url branch))
                              :id change-id
                              :content-id content-id
                              :org org
                              :project proj
                              :repo repo
                              :repo-metadata collection
                              :matches (mapcar (-rpartial #'plist-get :charOffset)
                                               (plist-get matches :content))
                              :type (plist-get repository :type)))
                           results)))
      (research--show-info info)
      files)))

(cl-defstruct (research--frag-pos (:constructor research--frag-pos-new))
  (fragment nil :type string)
  (offset nil :type number))

(eval-and-compile
  (research-destruct research--frag-pos))

;; (cl-defmethod research--query ((collection research--gh-repo) query page max-result)
;;   (aio-with-async
;;     (-let* (((&research--gh-repo
;;               :id repo-id
;;               :rcp (&research--gh-rcp :org :repo))
;;              collection)
;;             (res (aio-await (research--request
;;                              "GET" "/search/code"
;;                              :headers '(("Accept" . "application/vnd.github.v3.text-match+json"))
;;                              :query `((q . ,(format "repo:%s/%s %s" org repo query))
;;                                       (per_page . ,max-result)
;;                                       (page . ,page))
;;                              :host "api.github.com"
;;                              :forge 'github)))
;;             ((&plist :items) res))
;;       (--remove (not it)
;;                 (mapcar
;;                  (-lambda ((&plist :path :sha :html_url url
;;                                    :repository (&plist :id cur-id)
;;                                    :text_matches matches))
;;                    (when (equal repo-id cur-id)
;;                      (research--gh-code-result-new
;;                       :path path
;;                       :url url
;;                       :id sha
;;                       :repo collection
;;                       :matches (apply #'nconc
;;                                       (mapcar (-lambda ((&plist :fragment :matches))
;;                                                 (mapcar (-lambda ((&plist :indices [start _]))
;;                                                           (research--frag-pos-new
;;                                                            :fragment fragment
;;                                                            :offset start))
;;                                                         matches))
;;                                               matches)))))
;;                  items)))))

(cl-defmethod research--query ((collection research--gh-repo) query page _max-result)
  ""
  (aio-with-async
    (-let* (((&research--gh-repo
              :rcp (&research--gh-rcp :org org :repo rcp-repo))
             collection)
            (url-mime-accept-string "application/json")
            (res (aio-await (research--request
                             "GET" "/search"
                             :query `((q . ,(format "repo:%s/%s %s" org rcp-repo query))
                                      (type . "code")
                                      (p . ,page))
                             :auth 'cookie
                             :host "github.com"
                             :forge 'cs-github)))
            ((&plist :payload (&plist :results)) res))
      (--remove (not it)
                (mapcar
                 (-lambda ((&plist :path :blob_sha :commit_sha
                                   :repo_nwo
                                   :repo_id
                                   :term_matches))
                   (research--gh-code-result-new
                    :path path
                    :url (format "https://github.com/%s/blob/%s/%s"
                                 repo_nwo
                                 (research--encode-url commit_sha)
                                 (research--encode-url path))
                    :org org
                    :id blob_sha
                    :repo repo_id
                    :repo-metadata collection
                    :matches (mapcar (-rpartial #'plist-get :start) term_matches)))
                 results)))))

;;;###autoload
(cl-defun research-query (&key query page prefix hint)
  "Query QUERY to research server with page PAGE and query prefix PREFIX.
The PAGE can be input using prefix arg, negative value will force re-authentication.
The HINT will be used when there's no query specified."
  (interactive)
  (aio-with-async
    (-let* ((prefix-val (prefix-numeric-value current-prefix-arg))
            (query (cond
                    (query (let ((query (concat prefix query)))
                             (push query research--query-history)
                             query))
                    ((and current-prefix-arg (<= 0 prefix-val)) (car research--query-history))
                    (t (research--comp-read "Query: " nil
                                            :initial-input (concat prefix
                                                                   (or hint (thing-at-point 'symbol)))
                                            :history 'research--query-history))))
            (cols (or research--inuse-collections
                      (aio-await (research-set-collection))))
            (page (or page prefix-val))
            (results (aio-await (research--query-1 cols query page)))
            ((promise result) (aio-await (research--show-query-result results))))
      (while t
        (aio-await (apply #'research--jump-to-result result))
        (setq result (car (aio-chain promise)))))))

(defvar research--query-results nil)
;; TODO: Store the current search result in an alist so that we can navigate to next occurrence.
;; That list should be lazily evaluated the true pos when open the buffer.

(aio-defun research--query-1 (collections query page &optional max-result)
  ""
  (let ((files (mapcar (aio-lambda (col)
                         (aio-await (research--query col query page
                                                     (or max-result 200))))
                       collections)))
    (aio-await (aio-all files))
    (setq research--query-results
          (apply #'nconc (mapcar #'aio-wait-for files)))))

(defun research--show-info (info)
  "Show INFO."
  (when-let ((msg (research--az-get-info info)))
    (run-at-time 0.2 nil
                 (lambda ()
                   (message
                    "%s%s"
                    (propertize "Info: " 'face 'warning)
                    msg)))))

(cl-defgeneric research--buf-pos (pos)
  "Calculate the position in current buffer from POS.")

(defvar-local research--skip-calc-pos? nil)
(cl-defmethod research--buf-pos ((pos number))
  (if buffer-file-name
      (if research--skip-calc-pos?
          (+ pos 1)
        (let ((name buffer-file-name)
              (inhibit-eol-conversion t)
              (r-count 0)
              (pos (max pos (point-min))))
          (with-temp-buffer
            (insert-file-contents name)
            (save-match-data
              (goto-char (point-min))
              (while (search-forward "\r" pos 'bound)
                (cl-incf r-count)))
            (- pos r-count -1))))
    (+ (filepos-to-bufferpos pos) 1)))

(cl-defmethod research--buf-pos ((pos research--frag-pos))
  (-let* (((&research--frag-pos :fragment :offset) pos)
          (r-count 0)
          (offset (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (setq buffer-file-coding-system 'no-conversion)
                    (insert fragment)
                    (save-match-data
                      (goto-char (point-min))
                      (while (search-forward "\r" offset 'bound)
                        (cl-incf r-count)))
                    (- offset r-count))))
    (goto-char (point-min))
    (search-forward (decode-coding-string fragment 'utf-8-auto-dos 'nocopy) nil 'noerror)
    (when-let ((m (match-beginning 0)))
      (+ m offset))))

(defvar-local research--current-buffer-result nil
  "Store reSearch result of current buffer.
It's a plist of (:re research--code-result :idx :skip-calc-pos).")
;; Make this result permanent-local not cleared when change major mode.
(put 'research--current-buffer-result 'permanent-local t)

(cl-defun research--save-current-buffer-result (buf result
                                                    &key
                                                    pos skip-calc-pos)
  "BUF RESULT POS SKIP-CALC-POS."
  (with-current-buffer buf
    (setq research--current-buffer-result
          `( :re ,result
             :idx ,(cl-position pos (research--code-result-matches result)))
          research--skip-calc-pos? skip-calc-pos)))

(declare-function ivy-read "ext:ivy")
(declare-function helm "ext:helm")
(declare-function helm-build-sync-source "ext:helm")
(declare-function helm-make-actions "ext:helm")

(defun research--show-query-result (results)
  "Show completion windows for RESULTS and return promise."
  (research--debug results)
  (-let [(action-fn . promise) (aio-make-callback)]
    ;; `run-at-time' so this will not block
    (run-at-time
     0 nil
     (lambda (collection action-fn)
       (cond
         ((bound-and-true-p ivy-mode)
          (ivy-read (format "pattern [%s]: " (car research--query-history))
                    collection
                    :require-match t
                    :action
                    `(1
                      ("o" (lambda (re) (funcall ,action-fn `(,(cdr re) local)))
                           "Open local")
                      ("r" (lambda (re) (funcall ,action-fn `(,(cdr re) remote)))
                           "Open remote file")
                      ("f" (lambda (re) (funcall ,action-fn `(,(cdr re) remote-force)))
                           "Open remote force")
                      ("b" (lambda (re) (funcall ,action-fn `(,(cdr re) web)))
                           "Open in browser"))
                    :caller 'research-results))
         ((bound-and-true-p helm-mode)
          (helm
           :sources (helm-build-sync-source
                     "ReSearch results:"
                     :candidates collection
                     :action (helm-make-actions
                              "Open local" (lambda (re) (funcall action-fn `(,re local)))
                              "Open remote file" (lambda (re) (funcall action-fn `(,re remote)))
                              "Open remote force" (lambda (re) (funcall action-fn `(,re remote-force)))
                              "Open in browser" (lambda (re) (funcall action-fn `(,re web)))))
           :prompt (format "pattern [%s]: " (car research--query-history))
           :buffer "*helm reSearch*"))
         (t
          (let ((re (research--comp-read (format "pattern [%s]: " (car research--query-history))
                                         collection
                                         :category 'research
                                         :require-match t)))
            (funcall action-fn `(,re local))))))
     (--map (cons (research--code-result-path it) it) results)
     action-fn)
    promise))

(aio-defun research--jump-to-result (result &optional type)
  "Jump to RESULT regarding to TYPE as `local', `remote', `remote-force' or `web'."
  (-let* (((&research--code-result :path :url :repo-metadata :matches) result)
          (pos (if (> (length matches) 0) (elt matches 0) 0))
          skip-calc-pos?
          (buf
           (pcase type
            ('remote
             (aio-await (research--load-file-1 result)))
            ('remote-force
             (aio-await (research--load-file-1 result 'force)))
            ('web (browse-url url))
            (_
             (let ((local-path (aio-await
                                (research--convert-to-local-path path repo-metadata))))
               (if (file-exists-p local-path)
                   (progn
                     (setq skip-calc-pos? (research--get-skip-calc-pos repo-metadata))
                     (find-file-noselect local-path))
                 (aio-await (research--load-file-1 result))))))))
    (when (or (bufferp buf) (stringp buf))
      (research--save-current-buffer-result buf result
                                            :pos pos
                                            :skip-calc-pos skip-calc-pos?)
      (research--jump-to-pos buf pos))))

;;;###autoload
(defun research-next-in-buffer (step)
  "Jump to next STEP result in current buffer."
  (interactive "p")
  (when research--current-buffer-result
    (when-let* ((matches (research--code-result-matches
                          (plist-get research--current-buffer-result :re)))
                (not-empty (> (length matches) 0))
                (next (% (+ (or (plist-get research--current-buffer-result :idx) 0) step)
                         (length matches))))
      (research--jump-to-pos (current-buffer) (elt matches next))
      (plist-put research--current-buffer-result :idx next))))

(defsubst research--set-major-mode (&optional file-name buffer)
  "Set major mode based on FILE-NAME or BUFFER information."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((buffer-file-name (or file-name (buffer-name))))
      (set-auto-mode t))))

(cl-defgeneric research--load-file (file)
  "Load the remote FILE.")

(cl-defmethod research--load-file ((file research--az-code-result))
  ""
  (aio-with-async
    (-let [(&research--az-code-result :type :id :content-id :path
                                      :org :project :repo)
           file]
      (pcase type
        ("git"
         (aio-await (research--request
                     "GET" (format "/%s/_apis/git/repositories/%s/items"
                                   (research--encode-url project)
                                   (research--encode-url repo))
                     :query `((api-version . "6.0-preview.1")
                              (versionType . "commit")
                              (version . ,id)
                              (scopePath . ,path))
                     :reader (lambda (&rest _) (buffer-substring-no-properties
                                                (point) (point-max)))
                     :host (format "dev.azure.com/%s" (research--encode-url org))
                     :forge 'azdev)))
        ("custom"
         (-> (aio-await (research--request
                         "GET" "/_apis/search/customCode"
                         :query `((api-version . "6.0-preview.1")
                                  (projectName . ,project)
                                  (repositoryName . ,repo)
                                  (branchName . ,content-id)
                                  (filePath . ,path))
                         :host (format "almsearch.dev.azure.com/%s" org)
                         :forge 'azdev))
             (plist-get :value)))))))

(cl-defmethod research--load-file ((file research--gh-code-result))
  ""
  (aio-with-async
    (-let [(&research--gh-code-result :id :repo) file]
      (-some-> (aio-await (research--request
                           "GET" (format "/repositories/%s/git/blobs/%s"
                                         (research--encode-url repo)
                                         (research--encode-url id))
                           :host "api.github.com"
                           :forge 'github))
        (plist-get :content)
        (base64-decode-string)))))

(aio-defun research--load-file-1 (file &optional force)
  "Load the remote FILE.
Optionally open ignore cache with FORCE."
  (-let* (((&research--code-result :repo-metadata (&research--repo :name)
                                   :path :id)
           file)
          (file-name (string-join `(,temporary-file-directory
                                    ,name
                                    ,(concat
                                      (file-name-sans-extension path)
                                      "_"
                                      (if (> (length id) 5) (substring id 0 5) id)
                                      (let ((ext (file-name-extension path)))
                                        (if ext (concat "." ext) ""))))
                                  "/" )))
    (if (and (file-exists-p file-name) (not force))
        (find-file-noselect file-name)
      (when-let ((file-content (aio-await (research--load-file file))))
        (make-directory (file-name-directory file-name) t)
        (with-current-buffer (find-file-noselect file-name)
          (setq buffer-read-only nil)
          (erase-buffer)
          ;; Need to convert to unibyte and no-conversion to insert binary data
          (let ((coding-system-for-write 'no-conversion))
            (set-buffer-multibyte nil)
            (setq buffer-file-coding-system 'no-conversion)
            (insert file-content)
            (save-buffer))
          (let ((coding-system-for-read 'utf-8-auto-dos))
            (revert-buffer t t))
          (setq buffer-read-only t)
          (current-buffer))))))

(aio-defun research--get-git-toplevel ()
  "Get top folder of current git repo."
  (if (executable-find "git")
      (or (aio-await (research--shell "git rev-parse --show-toplevel"))
          default-directory)
    default-directory))

(aio-defun research--get-project-root (repo)
  "Get project root for REPO."
  (let ((root (read-directory-name
               (format "root [%s]: " (research--repo-name repo))
               (aio-await (research--get-git-toplevel)))))
    (setf (research--repo-root repo) root)))

;;;###autoload
(defun research-get-project-root ()
  "Get current project root."
  (interactive)
  (mapc (aio-lambda (collection)
          (aio-await (research--get-project-root collection)))
        research--inuse-collections))

(aio-defun research--convert-to-local-path (path repo)
  "Convert PATH to local path for REPO."
  (concat (or (research--repo-root repo)
              (aio-await (research--get-project-root repo)))
          "/"
          path))

(defun research--get-skip-calc-pos (repo)
  "Ask for skip calculating true position for REPO."
  (when (equal (research--repo-skip-calc-pos repo) 'undefined)
    (setf (research--repo-skip-calc-pos repo)
          (y-or-n-p (format "Skip calculating true position [%s]? "
                                 (research--repo-name repo)))))
  (research--repo-skip-calc-pos repo))

(defun research--jump-to-pos (buffer raw-pos)
  "In buffer BUFFER, jump to true position of RAW-POS."
  (switch-to-buffer buffer)
  (let* ((raw-pos (if (stringp raw-pos) (string-to-number raw-pos) raw-pos))
         (pos (if (equal raw-pos 0) (point) (research--buf-pos raw-pos))))
    (goto-char pos)
    (recenter)
    (when research-pulse-at-cursor
      (pulse-momentary-highlight-one-line pos))))

(defun research--load-local-file (path pos)
  "PATH POS."
  (when (file-exists-p path)
    (research--jump-to-pos (find-file path) pos)))

;;;###autoload
(defun research-init ()
  "Init reSearch middleware process."
  (interactive)
  (research-exit)
  (setq research--rcps (or (research--restore research-recipes-file)
                           research--rcps))
  (setq research--collections (research--restore research-cols-file)))

(defun research-exit ()
  "Kill reSearch middleware process."
  (interactive)
  (setq research--inuse-collections nil))

;;;###autoload
(define-minor-mode research-mode
  "Minor mode for reSearch."
  :global t
  :keymap (make-sparse-keymap)
  :lighter " reSearch"
  (if research-mode
      (research-init)
    (research-exit)))

;;; Utilities commands

(cl-defgeneric research--buffer-position-url (code-result line)
  "Return the current buffer file url with LINE information included.")

(cl-defmethod research--buffer-position-url ((re research--az-code-result) line)
  (format "%1$s&line=%2$s&lineEnd=%2$s&lineStartColumn=1&lineEndColumn=1"
          (research--code-result-url re)
          line))

(cl-defmethod research--buffer-position-url ((re research--gh-code-result) line)
  (format "%s#L%s" (research--code-result-url re) line))

;;;###autoload
(defun research-open-buffer-url ()
  "Open buffer result url in browser or offer to open current file by reSearch."
  (interactive)
  (cond
   (research--current-buffer-result
    (browse-url
     (-> (plist-get research--current-buffer-result :re)
         (research--buffer-position-url (format-mode-line "%l")))))
   (buffer-file-name
    (aio-with-async
      (-let* ((path (-some->> research--inuse-collections
                      (--map (or (research--repo-root it)
                                 (aio-wait-for (research--get-project-root it))))
                      (--first (string-prefix-p it buffer-file-name))
                      (funcall (lambda (s)
                                 (substring buffer-file-name (length s))))))
              (query (if path (format "path:%s" path)
                       (format "file:%s" (file-name-nondirectory buffer-file-name))))
              (results (aio-await (research-query :query query :page 1)))
              ((promise result) (aio-await (research--show-query-result results))))
        (push query research--query-history)
        (while t
          (aio-await (apply #'research--jump-to-result result))
          (browse-url
           (-> (plist-get research--current-buffer-result :re)
               (research--buffer-position-url (format-mode-line "%l"))))
          (-setq (result) (aio-chain promise))))))
   (t
    (message "No buffer url found! Please open this buffer by reSearch."))))

;;;###autoload
(defun research-browse-folder ()
  "Browse `thing-at-point' in current folder."
  (interactive)
  (cond
   (research--current-buffer-result
    (let ((thing (thing-at-point 'symbol))
          (dir (file-name-directory
                (research--code-result-path
                 (plist-get research--current-buffer-result :re)))))
      (research-query :query (if thing nil dir)
                      :prefix "path:"
                      :hint (concat dir " " thing))))
   (t
    (message "No buffer reSearch result found! Please open this buffer by reSearch."))))

;;;###autoload
(defun research-query-def ()
  "Search definition of `thing-at-point'."
  (interactive)
  (research-query :query (thing-at-point 'symbol) :prefix "def:"))

(provide 'research)

;;; research.el ends here
