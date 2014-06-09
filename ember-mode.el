;;; ember-mode.el --- Ember navigation mode for emacs

;; This is a proof of concept for ember-mode.  ember-mode helps you 
;; navigate through the files in your emberjs project.  A bunch of
;; bindings have been created to quickly jump to the relevant sources
;; given that you're visiting a recognised file (*) in the ember project.
;;
;; In the current state, you can quickly jump to the:
;; - model
;; - controller
;; - route
;; - router
;; - view
;; - component
;; - template
;;
;; ember-mode is currently geared towards ember-cli, however the
;; folder structure is similar in similar build systems for ember so
;; it will probably work there as well.
;;
;;
;; (*) There is a base implementation for the file recognition, but it
;;     needs improvement so you can always jump back from a found file.
;;     Some (somewhat) less common files are not recognised yet.


;;;;;;;;;;;;;;;;
;;;; MIT License

;; Copyright (C) 2014 Aad Versteden
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;;;;;;;;;;;;;;
;;;; Accounting

;; Version 0.1
;; Author: Aad Versteden <madnificent@gmail.com>
;; Keywords: ember ember.js emberjs
;; License: MIT


(require 'cl)

;;;;;;;;;;;;
;;;; plurals
;;
;; This should really be replaced by a Snowball or Porter2 stemmer.
;; It seems to be good enough for a proof of concept of ember-mode.

(defvar *pluralization-irregular-nouns* '(("child" . "children") ("woman" . "women") ("man" . "men") ("mouse" . "mice") ("goose" . "geese"))
  "alist containing the singular first and the plural next of some words.")

(defun pluralize-noun (noun)
  "Pluralizes NOUN."
  (save-match-data
    (cond ((find noun *pluralization-irregular-nouns* :key #'car :test #'string=)
           (cdr (find noun *pluralization-irregular-nouns* :key #'car :test #'string=)))
          ((string-match-p "[yo]$" noun)
           (message "Don't know how to translate %s" noun)
           noun)
          ((or (string-match "ch$" noun)
               (string-match "[xs]$" noun))
           (concat noun "es"))
          ((string-match "^\\(.*\\)fe?$" noun)
           (concat (match-string 1 noun) "ves"))
          (t (concat noun "s")))))

(defun singularize-noun (noun)
  "Singularizes NOUN."
  (save-match-data
    (cond ((find noun *pluralization-irregular-nouns* :key #'cdr :test #'string=)
           (car (find noun *pluralization-irregular-nouns* :key #'cdr :test #'string=)))
          ((string-match "^\\(.*ch\\)es$" noun)
           (match-string 1 noun))
          ((string-match "^\\(.*[xs]\\)es$" noun)
           (match-string 1 noun))
          ((string-match "^\\(.*\\)ves$" noun)
           (concat (match-string 1 noun) "f")) ;; this is just a wild guess, it might as well be fe
          ((string-match "^\\(.*\\)s$" noun)
           (match-string 1 noun))
          (t noun))))
           

;;;;;;;;;;;;
;;;; emberjs
(defun relative-ember-source-path (base-class base-type target-kind)
  "Supplies a list of plausible paths to an ember source file given
its core components.  The paths are returned as a list of strings,
starting from the app's root.

Sources are specified in ember by a few orthogonal factors:
- BASE-CLASS :: The base class of the element we're talking about.
    For instance:
      - A UserRoute would have a base class of User.
      - A UserModel would have a base class of User.
      - A LoginRoute would have a base class of Login.
- BASE-TYPE :: The type of the class we're talking about.
    For instance:
      - A UserRoute would have a base type of Route.
      - A UserModel would have a base type of Model.
      - A LoginRoute would have a base type of Route.
      - The template of a UserRoute would be app/templates/user.hbs
        but this could also be specified as a base-type of 'user'
        and a target-kind of 'template'.
    Possible values are:
      - router
      - route
      - model
      - controller
      - view
      - component
      - template
      - index (the index template)
      - (blank)
- TARGET-KIND :: The target kind is the kind of source file you
    expect to receive.  This is either 'source', 'template', or blank.
    For instance:
      - The coffeescript file for a UserRoute would be 'source'
      - The handlebars file for a UserController would be 'template'
      - The UserComponent's handlebars file would be 'template'
      - The UserComponent's coffeescript file would be 'source'
    Possible values are:
      - template
      - source
      - (blank)"
  (let (matchers)
    (cl-macrolet
        ((for-each-js-ext (&body body)
           "expect users to supply a body which is to be executed for each
            known javascript file extension.  collects the results in a list."
           `(loop for ext in '(".coffee" ".js")
              collect (progn ,@body)))
         (for-each-hbs-ext (&body body)
           "expect users to supply a body which is to be executed for each
            known handlebars file extension.  collects the results in a list."
           `(loop for ext in '(".hbs" ".handlebars")
              collect (progn ,@body)))
         (define-matchers (&body matchers)
           "expect users to supply a set of matches as
              (regex-for-base-type regex-for-target-kind match-lambda)
            in which the first two are strings and the latter is the body
            of a lambda function which is executed with the bindings
            class, type and target for base-class, base-typ and target-kind
            respectively."
           `(progn
              ,@(loop for matcher in matchers collect
                      `(setf matchers
                             (append matchers 
                                     (list (list ,(first matcher) ,(second matcher)
                                                 (lambda (class type target) ,@(cddr matcher))))))))))
      (define-matchers
        ;; BEGIN contains the definition for each matcher
        ;; the first two columns are a regexp, the rest is executed as code
        ;; base-type | target-kind | concatenation lambda body
        ("router"      ".*"          (for-each-js-ext (concat "app/router" ext)))
        ("^route$"     "source"      (for-each-js-ext (concat "app/routes/" class ext)))
        ("model"       "source"      (for-each-js-ext (concat "app/models/" class ext)))
        ("view"        "source"      (for-each-js-ext (concat "app/views/"  class ext)))
        ("component"   "source"      (for-each-js-ext (concat "app/components/" class ext)))
        ("controller"  "source"      (for-each-js-ext (concat "app/controllers/" class ext)))
        ("template"    ".*"          (for-each-hbs-ext (concat "app/templates/" class extension)))
        ("index"       ".*"          (for-each-hbs-ext (concat "app/templates/" class "/index" ext)))
        ("component"   "template"    (for-each-hbs-ext (concat "app/templates/components/" class ext)))
        (".*"          "template"    (for-each-hbs-ext (concat "app/templates/" class ext)))
        ;; END contains the definition of each matcher
        ))
    (loop for (base-type-regexp target-kind-regexp functor) in matchers
          if (and (string-match base-type-regexp base-type)
                  (string-match target-kind-regexp target-kind))
          return (funcall functor base-class base-type target-kind))))

(defun ember-current-project-root ()
  "Returns the root folder of the current ember project."
  ;; for the current implementation this basically walks up the tree until
  ;; it sees an app folder and assumes the folder containing the app folder
  ;; is the root of the ember project.
  (locate-dominating-file (or load-file-name buffer-file-name) "app"))

(defun join-strings (list join-string)
  "Joins the list of strings by the supplied separator"
  (apply #'concat (rest (loop for str in list append (list join-string str)))))

(defun relative-directory-components (file)
  "Returns the components of which the relative directory of file consists."
  ;; I assume split-string will work on non-unix systems because the function
  ;; `convert-standard-filename` exists and I like to think in a world with
  ;; consistent definitions.
  ;; note: by using `directory-file-name` we strip the last empty component
  (split-string (directory-file-name (file-name-directory file)) "/"))

(defun ember-current-file-components ()
  "returns a list containing the components upon which make up this ember
source file.

the components are defined in `relative-ember-source-path`.  this function
returns the base-class, the base-type and the target-kind of the current
file."
  ;; in this, the base type can normally be derived from the top level
  ;; folder (which is the one right underneath app).  the extension of
  ;; the file con be used to determine the type of the file.  anything
  ;; in between can be considered to be the class of the file (in most
  ;; cases).
  (let* ((relative-path (file-relative-name (or load-file-name buffer-file-name)
                                            (ember-current-project-root)))
         (rep-dir-components (relative-directory-components relative-path)))
    (let ((extension (file-name-extension relative-path))
          (maybe-base-type (second rep-dir-components))
          (maybe-base-class (join-strings (append (cddr rep-dir-components)
                                                  (list (file-name-base relative-path)))
                                          "/")))
      ;; (list maybe-base-class maybe-base-type extension)
      (let ((kind (if (or (string= extension "coffee")
                          (string= extension "js"))
                      "source" "template")))
        ;; todo: discover index
        (save-excursion
          (cond ((and (string= maybe-base-type "templates")
                      (string-match-p "^components/\\(.*\\)$" maybe-base-class))
                 (list (substring maybe-base-class (length "components/"))
                       "component"
                       "template"))
                (t (list maybe-base-class (singularize-noun maybe-base-type) kind))))))))
          
(cl-defun ember-open-file-by-type (type &optional (assume-js t))
  "Opens an ember file for a given kind"
  (destructuring-bind (base-class base-type target-kind)
      (ember-current-file-components)
    (ember-generic-open-file base-class type (if assume-js "source" target-kind))))

(defun ember-open-file-by-kind (kind)
  "Opens an ember file for a given kind"
  (destructuring-bind (base-class base-type target-kind)
      (ember-current-file-components)
    (ember-generic-open-file base-class base-type kind)))

(defun ember-generic-open-file (base-class base-type target-kind)
  "Tries to open the ember file specified by BASE-CLASS, BASE-TYPE and TARGET-KIND.
   If no such file was found, it tries to find related files or requests the user
   if the file should be created."
  (let ((ember-root (ember-current-project-root))
        (file-list
         ;; pick the files and their alternatives, so we have a good list
         ;; to search for an existing file.
         (append (relative-ember-source-path base-class base-type target-kind)
                 (relative-ember-source-path (pluralize-noun base-class) base-type target-kind)
                 (relative-ember-source-path (singularize-noun base-class) base-type target-kind))))
    (block found-file
      (loop for relative-file in file-list
            for absolute-file = (concat ember-root relative-file)
            if (file-exists-p absolute-file)
            do 
               (find-file absolute-file)
               (return-from found-file absolute-file))
      (let* ((rel-file (first file-list))
             (abs-file (concat ember-root rel-file)))
        (when (y-or-n-p (format "File not found.  Create [%s]?" rel-file))
          (find-file abs-file)
          (return-from found-file abs-file))))))

(defun open-ember-component ()
  (interactive)
  (ember-open-file-by-type "component"))

(defun open-ember-router ()
  (interactive)
  (ember-open-file-by-type "router"))

(defun open-ember-controller ()
  (interactive)
  (ember-open-file-by-type "controller"))

(defun open-ember-model ()
  (interactive)
  (ember-open-file-by-type "model"))

(defun open-ember-route ()
  (interactive)
  (ember-open-file-by-type "route"))

(defun open-ember-template ()
  (interactive)
  (ember-open-file-by-kind "template"))

(defun open-ember-javascript ()
  (interactive)
  (ember-open-file-by-kind "source"))

(defun open-ember-view ()
  (interactive)
  (ember-open-file-by-type "view"))


(defvar ember-mode-keymap (make-sparse-keymap)
  "Keymap for ember-mode.")

(define-key ember-mode-keymap (kbd "C-c c f p") #'open-ember-component)
(define-key ember-mode-keymap (kbd "C-c c f o") #'open-ember-router)
(define-key ember-mode-keymap (kbd "C-c c f c") #'open-ember-controller)
(define-key ember-mode-keymap (kbd "C-c c f m") #'open-ember-model)
(define-key ember-mode-keymap (kbd "C-c c f r") #'open-ember-route)
(define-key ember-mode-keymap (kbd "C-c c f t") #'open-ember-template)
(define-key ember-mode-keymap (kbd "C-c c f s") #'open-ember-javascript)
(define-key ember-mode-keymap (kbd "C-c c f v") #'open-ember-view)

(define-minor-mode ember-mode
  "Mode for navigating around ember applications"
  nil " [EM]" ember-mode-keymap
  :global t)

(provide 'ember-mode)
