(uiop:define-package #:com.andrewsoutar.figment/vulkan-bind
  (:use #:cl #:alexandria #:cl-ppcre #:cffi)
  (:import-from #:cxml)
  (:import-from #:cxml-dom)
  (:export #:gen-vulkan-bindings))
(cl:in-package #:com.andrewsoutar.figment/vulkan-bind)

(defun child-elems (node)
  "Gets a list of all the children of NODE which are ELEMENTs; ignores all others"
  (coerce (remove-if-not #'dom:element-p (dom:child-nodes node)) 'list))

(defun get-attribute (element name)
  "Gets the value of the attribute NAME of ELEMENT, or NIL if it's not present"
  (when-let ((attr (dom:get-attribute-node element name)))
    (dom:value attr)))

(defparameter *vulkan-file* #p"/usr/share/vulkan/registry/vk.xml")

(defmacro gen-vulkan-bindings ((&optional (file *vulkan-file*)) &body names)
  (destructuring-bind (structure-type) names
    (let ((root (dom:document-element (cxml:parse-file file (cxml-dom:make-dom-builder)))))
      (assert (equal (dom:tag-name root) "registry"))
      (let ((structure-type-el (find-if (lambda (el) (and (equal (dom:tag-name el) "enums")
                                                          (equal (get-attribute el "name") "VkStructureType")))
                                        (child-elems root))))
        `(defcenum ,structure-type
           ,@(mapcar
              (lambda (enum)
                (assert (equal (dom:tag-name enum) "enum"))
                (or
                 (register-groups-bind (name) ("^VK_STRUCTURE_TYPE_(.*)$" (get-attribute enum "name"))
                   `(,(intern (nsubstitute #\- #\_ name) :keyword) ,(parse-integer (get-attribute enum "value"))))
                 (error "Unable to parse element ~A" enum)))
              (child-elems structure-type-el)))))))
