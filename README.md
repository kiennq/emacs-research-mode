Code search integration for `Emacs`
=================================

`research.el` is an `Emacs`'s plugin that can be used for code searching on `Azure DevOps` and `GitHub` repo.
Support for other forges integration are now under development.

### News
- 12/23/2021: Add support for `GitHub` code search

# How to install
Via [`Quelpa`](https://github.com/quelpa/quelpa)

``` emacs-lisp
(quelpa '(ghub :fetcher github :repo "kiennq/ghub"))
(quelpa '(research :fetcher git
                   :url "git@github.com:kiennq/emacs-research-mode.git"))
```

You will need personal access tokens (PATs) to query for the code search result.
After getting your PATs, you can save them into `~/.authinfo` file (or `~/.authinfo.gpg` as encryted file).
The user name should be set to:
- `azdev^token` for `Azure DevOps`
- `<account>^token` for others

For example:

``` conf
machine almsearch.dev.azure.com/<org> login azdev^token password <PAT>
machine dev.azure.com/<org> login azdev^token password <PAT>
machine api.github.com login <account>^token password <PAT>
```

After that, you can start playing around with this plugin.

# Sample setup

``` emacs-lisp
(use-package research
  :quelpa (research
           :fetcher git
           :url "git@github.com:kiennq/emacs-research-mode.git")
  :defer t
  :diminish
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'umbrella-keybind-mode
      (kbd "SPC s r") #'research-query
      (kbd "SPC s e") #'research-add-repo
      (kbd "SPC s d") #'research-query-def
      (kbd "SPC s S") #'research-set-default-scope
      (kbd "SPC s c") #'research-set-collection)
    (evil-define-key 'normal 'research-mode
      (kbd "SPC s n") #'research-next-in-buffer
      (kbd "SPC s w") #'research-open-buffer-url
      (kbd "SPC s f") #'research-browse-folder))
  :config
  (research-mode +1))
```

# For development
`research.el` has some extension functions that can be used to add support for other forges.

``` emacs-lisp
(cl-defgeneric research--get-collections ((repo-rcp research--rcp))
  "Get collections of REPO-RCP.")

(cl-defgeneric research--add-repo (type)
  "Add a repository of type TYPE into search collections.")

(cl-defgeneric research--query ((repo research--repo) query page max-result)
  "Query QUERY to repository REPO with page PAGE.
Return at most MAX-RESULT items.")

(cl-defgeneric research--load-file ((file research--code-result))
  "Load the remote FILE.")

;; utilities.
(cl-defgeneric research--buffer-position-url (code-result line)
  "Return the current buffer file url with LINE information included.")
```

It also contains few structs that can be used to store the return data from them.

``` emacs-lisp
(cl-defstruct research--rcp
  "Recipe for the repository."
  (org)
  (repo))

(cl-defstruct research--repo
  "Base type for the repository that we search on."
  (name nil :document "Display name." :read-only t)
  (id)
  (rcp nil :read-only t)
  (root nil :document "The path to repo's code.")
  (skip-calc-pos 'none))

(cl-defstruct research--code-result
  "The code result"
  (path nil :document "Path to current result.")
  (url nil :document "URL to the current result.")
  (id nil :document "Version id.")
  (repo nil)
  (matches nil :type list :document "List of offsets"))
```

When adding support for a new forge, we should create a new type that inherits the listed base type, then create a new implementation for the generic functions.
