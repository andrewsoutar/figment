(uiop:define-package #:com.andrewsoutar.figment/vulkan-bind
  (:use #:cl #:alexandria #:cl-ppcre #:cffi #:com.andrewsoutar.matcher)
  (:local-nicknames (#:m #:com.andrewsoutar.matcher/matchers))
  (:import-from #:cxml)
  (:import-from #:cxml-dom)
  (:export #:load-registry #:gen-vulkan-bindings))
(cl:in-package #:com.andrewsoutar.figment/vulkan-bind)

(defun extract-contents (node)
  "Extracts the text contents of NODE (including whitespace), ignoring any tags"
  (apply 'concatenate 'string
         (map 'list (lambda (node)
                      (cond ((dom:comment-p node) "")
                            ((dom:text-node-p node) (dom:data node))
                            ((dom:element-p node)
                             (unless (equalp (dom:tag-name node) "comment") (extract-contents node)))
                            (t (cerror "Skip" "Unrecognized node: ~A" node))))
              (dom:child-nodes node))))

(defun child-elems (node)
  "Gets a list of all the children of NODE which are ELEMENTs; ignores all others"
  (coerce (remove-if-not #'dom:element-p (dom:child-nodes node)) 'list))

(defun get-attribute (element name)
  "Gets the value of the attribute NAME of ELEMENT, or NIL if it's not present"
  (when-let ((attr (dom:get-attribute-node element name)))
    (dom:value attr)))


(defun c-tokenize (str &optional (start 0))
  (labels ((match (matcher &aux (mstr (string matcher)) (end (+ start (length mstr))))
             (and (<= end (length str)) (string= str mstr :start1 start :end1 end) (setf start end)))
           (num (radix &rest keys &aux temp)
             (when (and (< start (length str)) (digit-char-p (char str start) radix))
               (setf (values temp start) (apply #'parse-integer str :start start :radix radix :junk-allowed t keys))))
           (parse-char ()
             (or (unless (match "\\") (char str (prog1 start (incf start))))
                 (cdr (assoc-if #'match '(("'".#\') ("\"".#\") ("?".#\?) ("\\".#\\) ("a".#\Bel) ("b".#\Backspace)
                                          ("f".#\Page) ("n".#\Newline) ("r".#\Return) ("t".#\Tab) ("v".#\Vt))))
                 (let ((hex (match "x"))) (code-char (num (if hex 16 8) :end (unless hex (+ start 3))))))))
    (do* ((tokens ()) (allow-octal t t)) ((or (null start) (>= start (length str))) (nreverse tokens))
     retry
      (let* ((old-start start) (radix (cond ((match "0x") 16) ((and allow-octal (match "0")) 8) (10)))
             (num (num radix)) frac frac-bias exp (kind :int))
        (when (match ".")
          (when (= radix 8) (setf allow-octal nil start old-start) (go retry))
          (setf frac-bias (expt radix (- start (progn (setf frac (num radix)) start)))))
        (unless (or num frac) (go skip-number))
        (when (find-if #'match (if (= radix 16) "pP" "eE"))
          (setf exp (* (cond ((match "+") 1) ((match "-") -1) (1)) (num 10))))
        (when (or frac-bias exp)
          (setf kind (cond ((find-if #'match "fF") :float) ((find-if #'match "lL") :long-double) (:double))))
        (when (or num frac)
          (push `(,kind ,(* (expt radix (or exp 0)) (+ (or num 0) (if frac (* frac frac-bias) 0)))) tokens)
          (go again))
        (setf start old-start))
     skip-number
      (cond ((find-if #'match #(#\Space #\Tab #\Newline #\Vt #\Page)))
            ((when-let (punc (or (find-if #'match #("<<=" ">>=" "->" "++" "--" "<<" ">>" "<=" ">=" "==" "!=" "&&" "||"
                                                    "..." "*=" "/=" "%=" "+=" "-=" "&=" "^=" "|=" "##"))
                                 (find-if #'match "[](){}.&*+-~!/%<>^|?:;=,#")))
               (push `(:punc ,(string punc)) tokens)))
            ((when-let (seq-kind (find-if #'match #("'" "L'" "u'" "U'" "\"" "u8\"" "u\"" "U\"" "L\"")))
               (do (c (lit (make-array '(16) :element-type 'character :adjustable t :fill-pointer 0)))
                   ((char= (setf c (char str start)) (char seq-kind (1- (length seq-kind))))
                    (incf start) (push `(:literal ,seq-kind ,lit) tokens))
                 (vector-push-extend (parse-char) lit))))
            ((or (alpha-char-p (char str start)) (eql (char str start) #\_))
             (flet ((sym-char (c) (or (alphanumericp c) (eql c #\_))))
               (push `(:ident ,(subseq str start (setf start (position-if-not #'sym-char str :start start))))
                     tokens)))
            (t (error "Invalid character while tokenizing: '~C'" (char str start))))
     again)))

(defun parse-type-decl (decl)
  "Parse a C type declaration"
  (let ((tokens (c-tokenize decl)))
    ;; This doesn't (yet) support function pointers. It only supports
    ;; const as a qualifier. It only supports the declaration of a
    ;; single identifier.
    (macrolet ((maybe-tokens (&body body)
                 (with-gensyms (old-tokens done)
                   `(let ((,old-tokens tokens) ,done)
                      (unwind-protect (multiple-value-prog1 (block nil ,@body) (setf ,done t))
                        (unless ,done (setf tokens ,old-tokens))))))
               (match-token-or-fail (&body matchers)
                 `(match-case (pop tokens) ,@matchers (t (throw 'parse-fail nil)))))
      (labels ((parse-qualifier () (maybe-tokens (match-token-or-fail ((:ident "const") :const))))
               (parse-expression ()
                 (maybe-tokens
                  (match-token-or-fail
                   (((m:or :int :float :double :long-double :literal) lit) lit)
                   ((:ident var) `(:var ,var)))))
               (parse-declaration ()
                 (maybe-tokens
                  (multiple-value-bind (name type-thunk)
                      (match-token-or-fail
                       ((:punc "*")
                        (let ((quals (make-array '(1) :adjustable t :fill-pointer 0)))
                          (catch 'parse-fail (loop (vector-push-extend (parse-qualifier) quals)))
                          (multiple-value-bind (name inner-decl) (parse-declaration)
                            (return (values name (lambda (type) (funcall inner-decl `(:pointer ,type))))))))
                       ((:punc "(") (multiple-value-prog1 (parse-declaration) (match-token-or-fail ((:ident ")")))))
                       ((:ident name) (values name #'identity)))
                    (catch 'parse-fail
                      (loop
                        (maybe-tokens
                         (match-token-or-fail
                          ((:punc "[")
                           (let ((len (prog1 (parse-expression) (match-token-or-fail ((:punc "]")))))
                                 (old-type-thunk type-thunk))
                             (setf type-thunk (lambda (type) (funcall old-type-thunk `(:array ,type ,len))))))))))
                    (values name type-thunk)))))
        (let ((type (loop (match-token-or-fail
                           ((:ident "const"))
                           ((:ident type) (return type))))))
          (multiple-value-bind (name type-thunk) (parse-declaration)
            (unless (endp tokens) (throw 'parse-fail nil))
            (return-from parse-type-decl `(,name ,(funcall type-thunk type)))))))))

(defun eval-c-expr (expr const-table)
  (match-ecase expr
    ((:var var) (parse-integer (get-attribute (gethash var const-table) "value")))
    ((m:type atom) expr)))

(defun vulkanize-type-name (name)
  (do ((copy (nstring-downcase (remove #\% (string name))))
       (pos 0 (1+ pos)))
      ((>= pos (length copy)) (concatenate 'string "Vk" (delete #\- copy)))
    (when (and (alpha-char-p (char copy pos)) (or (zerop pos) (not (alpha-char-p (char copy (1- pos))))))
      (setf (char copy pos) (char-upcase (char copy pos))))))
(defun vulkanize-func-name (name)
  ;; Bit of a hack
  (let ((type-like-name (vulkanize-type-name name)))
    (setf (char type-like-name 0) #\v)
    type-like-name))
(defun vulkanize-enum-value (name)
  (concatenate 'string "VK_" (substitute #\_ #\- (remove #\% (string name)))))
(defun unvulkanize-field-name (name type package &aux (start 0))
  (do () ((not (eql (car (ensure-list type)) :pointer)))
    (assert (eql (char name start) #\p))
    (incf start)
    (setf type (cadr (ensure-list type))))
  (do* ((start start end)
        (end #1=(position-if #'upper-case-p name :start (1+ start)) #1#)
        (temp #2=(string-upcase (subseq name start end)) (concatenate 'string temp "-" #2#)))
       ((null end) (if package (intern temp package) (make-symbol temp)))))

(defparameter *vulkan-file* #p"/usr/share/vulkan/registry/vk.xml")
(defvar *cache* (make-hash-table :test 'equal))

(defun %load-registry (&optional (file *vulkan-file*))
  (multiple-value-bind (root truename)
      (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
        (values (dom:document-element (cxml:parse-stream stream (cxml-dom:make-dom-builder)))
                (progn (close stream) (truename stream))))
    (assert (equal (dom:tag-name root) "registry"))
    (let ((type-table (make-hash-table :test 'equal))
          (const-table (make-hash-table :test 'equal))
          (func-table (make-hash-table :test 'equal)))
      (dolist (toplevel-child (child-elems root))
        (match-case (dom:tag-name toplevel-child)
          ("types"
           (dolist (type-elem (child-elems toplevel-child))
             (unless (or (not (equal (dom:tag-name type-elem) "type"))
                         (member (get-attribute type-elem "category") '(nil "include" "define") :test #'equal))
               (let ((name (or (get-attribute type-elem "name")
                               (extract-contents (find "name" (child-elems type-elem)
                                                       :key #'dom:tag-name :test #'equal)))))
                 (assert (null (shiftf (first (ensure-gethash name type-table (list nil nil))) type-elem)))))))
          ("enums"
           (when-let (name (get-attribute toplevel-child "name"))
             (assert (null (shiftf (second (ensure-gethash name type-table (list nil nil))) toplevel-child))))
           (dolist (enum-elem (child-elems toplevel-child))
             (when (equal (dom:tag-name enum-elem) "enum")
               (let ((name (get-attribute enum-elem "name")))
                 (assert name)
                 (assert (null (shiftf (gethash name const-table) enum-elem)))))))
          ("commands"
           (dolist (command-elem (child-elems toplevel-child))
             (assert (equal (dom:tag-name command-elem) "command"))
             (let ((name (or (get-attribute command-elem "name")
                             (let ((proto (first (child-elems command-elem))))
                               (assert (equal (dom:tag-name proto) "proto"))
                               (extract-contents (find "name" (child-elems proto)
                                                       :key #'dom:tag-name :test #'equal))))))
               (assert (null (shiftf (gethash name func-table) command-elem))))
             ))))
      (setf (gethash file *cache*)
            (setf (gethash truename *cache*) (list type-table const-table func-table))))
    truename))
(defmacro load-registry (&optional (file '*vulkan-file*))
  `(eval-when (:compile-toplevel :execute)
     (%load-registry ,file)))

(defmacro define-vulkan-func (kind (name vulkan-name) return-type &body args)
  (multiple-value-bind (var get-pointer-expr)
      (ecase kind
        (:global (values nil `(get-instance-proc-addr (null-pointer) ,vulkan-name)))
        (:instance (values '*instance* `(get-instance-proc-addr (aref *instance*) ,vulkan-name)))
        (:device (values '*device* `(get-device-proc-addr (aref *device*) ,vulkan-name))))
    `(defun ,name ,(mapcar #'first args)
       ,(let* ((loopy (cons nil nil))
               (body `(let ((pfn ,get-pointer-expr))
                        (eval `(defun ,',name ,',(mapcar #'first args)
                                 (if (eql ,',var ',,var)
                                     (foreign-funcall-pointer ,pfn () ,@',(mapcan #'reverse args) ,',return-type)
                                     ,',loopy)))
                        (,name ,@(mapcar #'first args)))))
          (setf (car loopy) (car body)
                (cdr loopy) (cdr body))
          loopy))))

(defmacro gen-vulkan-bindings ((&optional (file *vulkan-file*)) &body body)
  (multiple-value-bind (structs enums functions)
      (loop for (kind . names) in body
            when (eql kind :structs)
              append names into structs
            when (eql kind :enums)
              append names into enums
            when (eql kind :functions)
              append names into functions
            finally (return (values structs enums functions)))
    (destructuring-bind (type-table const-table func-table) (gethash file *cache*)
      (labels ((normalize-type (type)
                 (match-ecase type
                   ((:pointer &rest) :pointer)
                   ((:array inner len) `(:array ,(normalize-type inner) ,(eval-c-expr len const-table)))
                   ("void" :void)
                   ("char" :char)
                   ("float" :float)
                   ("int32_t" :int32)
                   ("uint32_t" :uint32)
                   ("size_t" :size)
                   ("VkBool32" `(:boolean :uint32))
                   ((m:type string) (let ((elem (first (gethash type type-table))))
                                      (match-ecase (get-attribute elem "category")
                                        ((m:or "enum" "bitmask")
                                         (or (find type enums :key #'vulkanize-type-name :test #'equal) :int))
                                        ("struct"
                                         `(:struct ,(or (find type structs :key #'vulkanize-type-name :test #'equal)
                                                        (error "Struct ~A does not have a name!" type))))
                                        ("handle"
                                         (match-ecase (extract-contents elem)
                                           ((m:equal (format nil "VK_DEFINE_HANDLE(~A)" type)) :pointer)
                                           ((m:equal (format nil "VK_DEFINE_NON_DISPATCHABLE_HANDLE(~A)" type))
                                            :uint64)))))))))
        `(progn
           ,@(mapcar
              (lambda (enum-name)
                (let* ((type-name (vulkanize-type-name enum-name))
                       (prefix (concatenate 'string (vulkanize-enum-value enum-name) "_"))
                       (enums-el (second (gethash type-name type-table))))
                  `(defcenum ,enum-name
                     ,@(mapcar (lambda (value)
                                 (assert (equal (dom:tag-name value) "enum"))
                                 (let ((val-name (get-attribute value "name")))
                                   (assert (string= prefix val-name :end2 (length prefix)))
                                   `(,(intern (nsubstitute #\- #\_ (subseq val-name (length prefix))) :keyword)
                                     ,(parse-integer (get-attribute value "value")))))
                               (child-elems enums-el)))))
              enums)
           ,@(mapcar
              (lambda (struct-name)
                (let* ((type-name (vulkanize-type-name struct-name))
                       (struct-el (first (gethash type-name type-table))))
                  `(defcstruct ,struct-name
                     ,@(mapcar (lambda (member)
                                 (assert (equal (dom:tag-name member) "member"))
                                 (destructuring-bind (name type)
                                     (parse-type-decl (extract-contents member))
                                   `(,(unvulkanize-field-name name type :keyword)
                                     ,(normalize-type type))))
                               (child-elems struct-el)))))
              structs)
           ,@(labels ((function-kind (name params)
                        ;; vkspec 4.1
                        (if (member name '("vkEnumerateInstanceVersion" "vkEnumerateInstanceExtensionProperties"
                                           "vkEnumerateInstanceLayerProperties" "vkCreateInstance")
                                    :test #'equal)
                            :global
                            (destructuring-bind (name type) (parse-type-decl (extract-contents (first params)))
                              (declare (ignore name))
                              (function-kind-for-type type))))
                      (function-kind-for-type (type)
                        (match-ecase type
                          ("VkDevice" :device)
                          ("VkInstance" :instance)
                          ((m:type string)
                           (function-kind-for-type (get-attribute (first (gethash type type-table)) "parent"))))))
               (mapcar
                (lambda (func-name)
                  (let* ((vk-name (vulkanize-func-name func-name))
                         (func-el (gethash vk-name func-table)))
                    (destructuring-bind (proto &rest params) (child-elems func-el)
                      (assert (equal (dom:tag-name proto) "proto"))
                      (assert (every (lambda (x) (equal (dom:tag-name x) "param")) params))
                      (destructuring-bind (name return-type) (parse-type-decl (extract-contents proto))
                        (assert (equal name vk-name))
                        `(define-vulkan-func ,(function-kind vk-name params) (,func-name ,vk-name)
                             ,(normalize-type return-type)
                           ,@(mapcar (lambda (param)
                                       (destructuring-bind (name type) (parse-type-decl (extract-contents param))
                                         `(,(unvulkanize-field-name name type nil) ,(normalize-type type))))
                                     params))))))
                functions)))))))


;;; FIXME this stuff probably shouldn't be here
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library libvulkan
    (:linux "libvulkan.so.1")
    (t (:default "libvulkan"))))
(use-foreign-library libvulkan)

(defvar *instance*)
(defvar *device*)

;;; Hack? Might be better to use the "proper" types for instance & device
(defcfun (get-instance-proc-addr "vkGetInstanceProcAddr" :library libvulkan) :pointer
  (instance :pointer)
  (name :string))
(define-vulkan-func :instance (get-device-proc-addr "vkGetDeviceProcAddr") :pointer
  (device :pointer)
  (name :string))
