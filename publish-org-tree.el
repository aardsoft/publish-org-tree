(require 'org)
(require 'org-table)
(require 'ox-publish)
;; sensible defaults for some emacs variables
(setq backup-inhibited t
      org-export-allow-bind-keywords t
      auto-save-default nil)


(defvar org-page-toc-level 3)
(defvar org-page-toc-in-sidebar t)
(defvar org-page-template)
(defvar org-page-stylesheets '())
(defvar org-page-default-stylesheets nil)
(defvar org-page-absolute-stylesheets nil)
(defvar org-page-relative-stylesheets nil)
;; undocumented for now
(defvar org-page-shortcuts nil)
(defvar org-page-shortcuts-content '())
(defvar org-page-shortcuts-include '())
;;

;; helper functions also used for variable init
(defun rel-to-project(path)
  (file-relative-name path (plist-get settings :project-directory)))

(defun abs-to-project(path)
  (if (file-name-absolute-p path) path
    (expand-file-name (concat (plist-get settings :project-directory) "/" path))))

;; org-mode export defaults
(setq org-confirm-babel-evaluate nil
      org-html-htmlize-output-type 'css
      org-html-validation-link nil
      org-publish-use-timestamps-flag nil
      org-export-with-sub-superscripts '{})

(load "ox-thtml.el")

(split-string "foo" " ")

(defvar settings)
(defun configure-setting(input &optional output)
  (let ((out (or output '())))
    (if (null input)
        output
      (let* ((key-sym (car input))
             (key (substring (prin1-to-string key-sym) 1))
             (env-key (replace-regexp-in-string "-" "_" key))
             (env (getenv env-key))
             (value (cadr input)))
        (if env
            (progn
              (princ (format "%s: %s\n" key env))
              (let ((type (type-of value)))
                (cond
                 ((eq type 'string)
                  (setq out (plist-put out key-sym env)))
                 ((eq type 'symbol)
                  (setq out (plist-put out key-sym (intern env))))
                 ((eq type 'cons)
                  (setq out (plist-put out key-sym (split-string env " "))))
                 (t (princ (format "Unknown value type encountered: %s, skipping\n" type))))))
          (progn
            (princ (format "*%s: %s\n" key value))
            (setq out (plist-put out key-sym value)))))
      (configure-setting (cddr input) out))))

;; all default settings configured here can be set through environment variables
(setq default-settings '(
                         :default-page-template "page.html"
                         :org-publish-function org-html-publish-to-html
                         :project-directory "."
                         :project-name "default"
                         :publish-directory ""
                         :static-files (css eot gif jpg js json less mp3 ogg pdf png scss svg ttf txt woff woff2 yml zip ps1 sh)
                         :style-directory ""
                         :template-directory ""
                         ))

(princ (format "*** Configuration summary ***

Configuration variables (defaults marked with *):

"))

(setq settings (configure-setting default-settings)
      abs-template-directory nil)

(let ((template-directory (plist-get settings :template-directory)))
  (when (and template-directory (> (length template-directory) 0))
    (setq abs-template-directory (abs-to-project template-directory))))

(setq default-search-path (list (expand-file-name (plist-get settings :project-directory)) abs-template-directory))

(princ (format "Search path: %s" default-search-path))

;; minimal export modules
(setq static-modules `(("static" :base-directory ,(plist-get settings :project-directory))))
(setq dynamic-modules `(("html" :base-directory ,(plist-get settings :project-directory))))

;; read the styles also relative to the project directory. Note that the setting
;; itself is not touched.
(let ((style-directory (plist-get settings :style-directory)))
  (when (> (length style-directory) 0)
    (push `("style" :base-directory ,(abs-to-project style-directory)) static-modules)))

(let ((sitemap (getenv "sitemap")))
  (when (and sitemap (> (length sitemap) 0))
    (push `("sitemap"
            :base-directory ,(plist-get settings :project-directory)
            :html-template (lambda (contents info)
                             (let* ((input-buffer (plist-get info :input-buffer))
                                    (template-path (lookup-template-name input-buffer)))
                               (if (and (string= input-buffer "sitemap.org") template-path (file-exists-p template-path))
                                   (let ((template (templated-html-load-template template-path)))
                                     (funcall template contents info))
                                 (progn
                                   (message (format "Warning: not using template for %s, %s not found." input-buffer template-path))
                                   (org-html-template contents info)))))
            :auto-sitemap t)
          dynamic-modules)))

;; defaults for exporting static files
(setq static-export-defaults `(:base-extension ,(mapconcat (lambda (in) (if (stringp in) in (symbol-name in))) (plist-get settings :static-files) "\\|")
                                               :publishing-directory ,(plist-get settings :publish-directory)
                                               :recursive t
                                               :publishing-function org-publish-attachment))

;; defaults for exporting dynamic files
(setq dynamic-export-defaults `(:base-extension "org"
                                        ;:include ,(mapcar 'symbol-name pre-includes)
                                        ;:exclude ,(regexp-opt (mapcar 'symbol-name '(${pre_excludes:-})))
                                                :publishing-directory ,(plist-get settings :publish-directory)
                                                :html-doctype "html5"
                                                :recursive t
                                                :section-numbers nil
                                                :publishing-function ,(plist-get settings :org-publish-function)
                                                :headline-levels 3
                                                :auto-preamble this
                                                :auto-sitemap  nil
                                                :html-template (lambda (contents info)
                                                                 (let* ((input-buffer (plist-get info :input-buffer))
                                                                        (template-path (lookup-template-name input-buffer)))
                                                                   (if (and template-path (file-exists-p template-path))
                                                                       (let ((template (templated-html-load-template template-path)))
                                                                         (funcall template contents info))
                                                                     (progn
                                                                       (message (format "Warning: not using template for %s, %s not found." input-buffer template-path))
                                                                       (org-html-template contents info)))))
                                                :with-email nil
                                                :with-date nil
                                                :with-toc nil))

;; function definitions
(defun lookup-template-name (input-buffer)
  "Return an absolute template path"
  (let ((template-name
         (cond ((boundp 'org-page-template)
                org-page-template)
               ((string= input-buffer "sidebar.org")
                "sidebar-template.html")
               ((string= input-buffer "sitemap.org")
                "sitemap-template.html")
               (t (plist-get settings :default-page-template)))))
    (expand-file-name template-name abs-template-directory)))

(defun build-org-publish-project-alist ()
  "Generate org-publish-project-alist.

In the generated list all static exports will come first, followed by all
dynamic exports. Processing is in reverse order of the configuration, i.e.,
the two default modules come last. It is assumed that nothing depends on the
default modules, while the default modules may depend on others (like sidebar,
sitemap) - this processing makes sure dependencies are available.

If a module depends on one of the default modules it manually needs to be
placed at the right position."
  (let* ((component-list ())
         (export-alist ())
         (project-name (plist-get settings :project-name)))
    ;; add dynamic export modules
    (mapc
     (lambda (module)
       (let* ((module-name (car module))
              (component-name (concat project-name "-" module-name))
              (component-variable (intern (concat module-name "-export-plist")))
              (plist
               (if (boundp component-variable) (org-combine-plists dynamic-export-defaults (cdr module) (symbol-value component-variable))
                 (org-combine-plists dynamic-export-defaults (cdr module)))))
         (push (push component-name plist) export-alist)
         (push component-name component-list)))
     (nreverse dynamic-modules))
    ;; add static export modules
    (mapc
     (lambda (module)
       (let* ((module-name (car module))
              (component-name (concat project-name "-" module-name))
              (component-variable (intern (concat module-name "-export-plist")))
              (plist
               (if (boundp component-variable) (org-combine-plists static-export-defaults (cdr module) (symbol-value component-variable))
                 (org-combine-plists static-export-defaults (cdr module)))))
         (push (push component-name plist) export-alist)
         (push component-name component-list)))
     (nreverse static-modules))
    ;; finally push the component list onto the alist and return
    (push (list project-name :components component-list) export-alist)
    ))

(defun insert-from-file (file &optional search-path)
  (let* ((path (or search-path default-search-path))
         (filepath (expand-file-name file (car path))))
    (if (file-exists-p filepath)
        (with-temp-buffer
          (insert-file-contents filepath)
          (buffer-string))
      (insert-from-file file (cdr path)))))

(defun add-stylesheets (stylesheets info)
  ""
  (mapconcat (lambda (stylesheet) (format "<link rel=\"stylesheet\" href=\"%s\" type=\"text/css\"/>" stylesheet)) stylesheets "\n"))

(defun add-relative-stylesheets (stylesheets info)
  ""
  (mapconcat (lambda (stylesheet)
               (let ((relative-stylesheet (file-relative-name
                                           (concat (plist-get info :publishing-directory) stylesheet)
                                           (file-name-directory (plist-get info :output-file)))))
                 (format "<link rel=\"stylesheet\" href=\"%s\" type=\"text/css\"/>" relative-stylesheet))) stylesheets "\n"))

(defun publish-project ()
  "Generate org-publish-project-alist from current configuration, and start an export."
  (let ((project-name (plist-get settings :project-name)))
    (message (format "Publishing project '%s' to %s"
                     project-name
                     (plist-get settings :publish-directory)))
    (setq org-publish-project-alist (build-org-publish-project-alist))
    (message "Elements in export alist:")
    (mapc (lambda (list)
            (message (format "- %s" (car list))))
          org-publish-project-alist)
    (org-publish-project project-name t)))

;; load user configuration, if available.
;; To change flags in the static module without overriding variables something
;; like this would work in one of those files:
;; (setq static-export-plist '(:recursive nil))
(let ((project-name (plist-get settings :project-name)))
  (mapc (lambda (directory)
          (let ((file-name (expand-file-name "export-settings.user" directory)))
            (if (file-exists-p file-name)
                (load-file file-name))))
        `(project-name ,(concat project-name "/../"))))
