(require 'org)
(require 'org-table)
(require 'ox-publish)
;; sensible defaults for some emacs variables
(setq backup-inhibited t
      auto-save-default nil)

;; helper functions also used for variable init
(defun rel-to-project(path)
  (file-relative-name path project-directory))

(defun abs-to-project(path)
  (if (file-name-absolute-p path) path
    (expand-file-name (concat project-directory "/" path))))

;; org-mode export defaults
(setq org-confirm-babel-evaluate nil
      org-html-validation-link nil
      org-export-with-sub-superscripts '{})

(load "ox-thtml.el")

(setq defaults '(
                 (:static-files (css js png jpg gif pdf mp3 ogg zip))
                 (:org-publish-function org-html-publish-to-templated-html)
                 ))

(setq project-name (getenv "project_name")
      project-directory (getenv "project_directory")
      publish-directory (getenv "publish_directory")
      template-directory (getenv "template_directory")
      debug-publish (getenv "debug_publish")
      static-files '(css js png jpg gif pdf mp3 ogg zip)
      abs-template-directory nil
      pre-org-publish-function 'org-html-publish-to-templated-html
      )

(when (and template-directory (> (length template-directory) 0))
  (setq abs-template-directory (abs-to-project template-directory)))

(unless (and project-name (> (length project-name) 0))
  (setq project-name "default"))

(message (format "*** Configuration summary ***

Project directory: %s
Template directory: %s
Template directory (abs): %s

"
project-directory
template-directory
abs-template-directory
))

;; minimal export modules
(setq static-modules `(("static" :base-directory ,project-directory)))
(setq dynamic-modules `(("html" :base-directory ,project-directory)))

(let ((style-dir (getenv "style_dir")))
  (when (and style-dir (> (length style-dir) 0))
    (push `("style" :base-directory ,style-dir) static-modules)))

;; TODO:
;; - add variables and default config for two pre-styles, sidebar and sitemap
;; any additional modules should be defined in user files

;; defaults for exporting static files
(setq static-export-defaults `(:base-extension ,(mapconcat 'symbol-name static-files "\\|")
                                               :publishing-directory ,publish-directory
                                               :recursive t
                                               :publishing-function org-publish-attachment))

;; defaults for exporting dynamic files
(setq dynamic-export-defaults `(:base-extension "org"
                                                ;:include ,(mapcar 'symbol-name pre-includes)
                                                ;:exclude ,(regexp-opt (mapcar 'symbol-name '(${pre_excludes:-})))
                                                :publishing-directory ,publish-directory
                                                :html-doctype "html5"
                                                :recursive t
                                                :section-numbers nil
                                                :publishing-function org-html-publish-to-templated-html
                                                :headline-levels 3
                                                :auto-preamble this
                                                :auto-sitemap  t
                                                :sitemap-filename "sitemap.org"
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
         (cond ((string= input-buffer "sitemap.org")
                "sidebar-template.html")
               (t "page.html"))))
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
         (export-alist ()))
    ;; add dynamic export modules
    (mapc
     (lambda (module)
       (let* ((module-name (car module))
              (component-name (concat project-name "-dynamic"))
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
;(build-org-publish-project-alist)

(defun publish-project ()
  "Generate org-publish-project-alist from current configuration, and start an export."
  (message (format "Publishing project '%s' to %s" project-name publish-directory))
  (setq org-publish-project-alist (build-org-publish-project-alist))
  (when debug-publish
    (message (prin1-to-string org-publish-project-alist)))
  (message "Elements in export alist:")
  (mapc (lambda (list)
          (message (format "- %s" (car list))))
        org-publish-project-alist)
  (org-publish-project project-name t))

;; load user configuration, if available.
;; To change flags in the static module without overriding variables something
;; like this would work in one of those files:
;; (setq static-export-plist '(:recursive nil))
(mapc (lambda (directory)
        (let ((file-name (expand-file-name "export-settings.user" directory)))
          (if (file-exists-p file-name)
              (load-file file-name))))
      `(project-name ,(concat project-name "/../")))
