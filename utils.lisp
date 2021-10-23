(uiop:define-package #:com.andrewsoutar.figment/utils
  (:use #:cl)
  (:export #:with-cleanups #:nest))
(cl:in-package #:com.andrewsoutar.figment/utils)

(defmacro with-cleanups ((&rest bindings) &body body)
  (if bindings
      (destructuring-bind (cleanup value &optional name) (reverse (first bindings))
        (let ((temp (gensym (format nil "~A-TEMP" name)))
              (cleanup-fun (gensym (format nil "~A-CLEANUP" name))))
          `(let* ((,temp ,value)
                  (,cleanup-fun ,cleanup)
                  ,@(when name `((,name ,temp))))
             (unwind-protect (with-cleanups ,(rest bindings) ,@body)
               (funcall ,cleanup-fun ,temp)))))
      `(locally ,@body)))

(defmacro nest (&body body)
  (destructuring-bind (first &rest rest) body
    (if rest `(,@first (nest ,@rest)) first)))
