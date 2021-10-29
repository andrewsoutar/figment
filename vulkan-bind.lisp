(uiop:define-package #:com.andrewsoutar.figment/vulkan-bind
  (:use #:cl #:alexandria #:cl-ppcre #:cffi #:com.andrewsoutar.matcher)
  (:local-nicknames (#:m #:com.andrewsoutar.matcher/matchers))
  (:import-from #:cxml)
  (:import-from #:cxml-dom)
  (:export #:load-registry #:gen-bindings))
(cl:in-package #:com.andrewsoutar.figment/vulkan-bind)

(defun extract-contents (node)
  "Extracts the text contents of NODE (including whitespace) to a list of strings, ignoring any tags"
  (flatten
   (map 'list (lambda (node)
                (cond ((dom:comment-p node) "")
                      ((dom:text-node-p node) (dom:data node))
                      ((dom:element-p node)
                       (unless (equalp (dom:tag-name node) "comment") (extract-contents node)))
                      (t (cerror "Skip" "Unrecognized node: ~A" node))))
        (dom:child-nodes node))))
(defun extract-contents-str (node)
  "Extracts the text contents of NODE (including whitespace) to a single string, ignoring any tags"
  (apply #'concatenate 'string (extract-contents node)))

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
      (let* ((old-start start) (radix (cond ((match "0x") 16) ((and allow-octal (eql (char str start) #\0)) 8) (10)))
             (num (num radix)) frac frac-bias exp (kind :int))
        (when (match ".")
          (when (= radix 8) (setf allow-octal nil start old-start) (go retry))
          (setf frac-bias (expt radix (- start (progn (setf frac (num radix)) start)))))
        (unless (or num frac) (setf start old-start) (go skip-number))
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

(defvar *parser-tokens*)
(macrolet ((maybe-tokens (&body body)
             (with-gensyms (new-tokens)
               `(let (,new-tokens)
                  (multiple-value-prog1
                      (let ((*parser-tokens* *parser-tokens*))
                        (multiple-value-prog1 (block nil ,@body)
                          (setf ,new-tokens *parser-tokens*)))
                    (setf *parser-tokens* ,new-tokens)))))
           (match-token-or-fail (&body matchers)
             `(match-case (pop *parser-tokens*) ,@matchers (t (throw 'parse-fail nil)))))
  (defun parse-expression (&optional (input nil input-p))
    (when input-p
      (return-from parse-expression (let ((*parser-tokens* (mapcan #'c-tokenize input))) (parse-expression))))
    (maybe-tokens
     (match-token-or-fail
      (((m:or :int :float :double :long-double :literal) lit) lit)
      ((:ident var) `(:var ,var))
      ((:punc "-") `(- ,(parse-expression))))))
  (defun parse-type-decl (&optional (input nil input-p))
    "Parse a C type declaration"
    ;; This doesn't (yet) support function pointers. It only supports
    ;; const as a qualifier. It only supports the declaration of a
    ;; single identifier.
    (when input-p
      (return-from parse-type-decl (let ((*parser-tokens* (mapcan #'c-tokenize input))) (parse-type-decl))))
    (labels ((parse-qualifier () (maybe-tokens (match-token-or-fail ((:ident "const") :const))))
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
                         ((:ident "struct") (match-token-or-fail ((:ident struct-name) (return struct-name))))
                         ((:ident type) (return type))))))
        (multiple-value-bind (name type-thunk) (parse-declaration)
          (unless (endp *parser-tokens*) (throw 'parse-fail nil))
          (return-from parse-type-decl `(,name ,(funcall type-thunk type))))))))

(defun eval-c-expr (expr const-table)
  (match-ecase expr
    (('- expr) (- (eval-c-expr expr const-table)))
    ((:var var) (parse-integer (get-attribute (gethash var const-table) "value")))
    ((m:type atom) expr)))

(defun vulkan-tag-split (name tag-table)
  (let* ((name-string (string name))
         (dash (position #\- name-string :from-end t))
         (maybe-tagless-name (subseq name-string 0 dash))
         (maybe-tag (when dash (subseq name-string (1+ dash)))))
    (if (and maybe-tag (gethash maybe-tag tag-table))
        (values maybe-tagless-name maybe-tag)
        (values name-string ""))))
(defun vulkanize-type-name (name tag-table)
  (multiple-value-bind (name tag) (vulkan-tag-split name tag-table)
    (do ((copy (nstring-downcase (remove #\% name)))
         (pos 0 (1+ pos)))
        ((>= pos (length copy)) (concatenate 'string "Vk" (delete #\- copy) tag))
      (when (and (alpha-char-p (char copy pos)) (or (zerop pos) (not (alpha-char-p (char copy (1- pos))))))
        (setf (char copy pos) (char-upcase (char copy pos)))))))
(defun unvulkanize-type-name (name tag-table package)
  (assert (string= name "Vk" :end1 2))
  (do ((pos 2 (1+ pos))
       (temp (make-array '(16) :element-type 'character :adjustable t :fill-pointer 0)))
      ((>= pos (length name))
       (if package (intern (coerce temp 'string) package) (make-symbol (coerce temp 'string))))
    (if (gethash (subseq name pos) tag-table)
        (setf temp (format nil "~A-~A" (coerce temp 'string) (subseq name pos))
              pos (length name))
        (progn
          (when (and (>= pos 3) (not (lower-case-p (char name pos))) (alpha-char-p (char name (1- pos))))
            (vector-push-extend #\- temp))
          (vector-push-extend (char-upcase (char name pos)) temp)))))
(defun unvulkanize-func-name (name tag-table package)
  (let ((copy (copy-sequence 'string name)))
    (assert (eql (char copy 0) #\v))
    (setf (char copy 0) #\V)
    (unvulkanize-type-name copy tag-table package)))
(defun vulkanize-func-name (name tag-table)
  ;; Bit of a hack
  (let ((type-like-name (vulkanize-type-name name tag-table)))
    (setf (char type-like-name 0) #\v)
    type-like-name))
(defun vulkanize-enum-value (name tag-table)
  (setf (values name) (vulkan-tag-split name tag-table))
  (setf name (string name))
  (let ((suffix "-FLAG-BITS"))
    (when (and (>= (length name) (length suffix)) (string= name suffix :start1 (- (length name) (length suffix))))
      (setf name (subseq name 0 (- (length name) (length suffix))))))
  (concatenate 'string "VK_" (substitute #\_ #\- (remove #\% (string name)))))
(defun vulkanize-flag-value (name tag-table)
  (setf (values name) (vulkan-tag-split name tag-table))
  (setf name (string name))
  (let ((suffix "-FLAGS"))
    (assert (string= name suffix :start1 (- (length name) (length suffix))))
    (concatenate 'string "VK_" (substitute #\_ #\- (delete #\% (subseq name 0 (- (length name) (length suffix))))))))
(defun unvulkanize-field-name (name type package &aux (start 0))
  (do () ((not (and (eql (car (ensure-list type)) :pointer) (eql (char name start) #\p))))
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
    (let ((tag-table (make-hash-table :test 'equal))
          (type-table (make-hash-table :test 'equal))
          (const-table (make-hash-table :test 'equal))
          (func-table (make-hash-table :test 'equal))
          (feat-table (make-hash-table :test 'equal))
          (ext-table (make-hash-table :test 'equal)))
      (dolist (toplevel-child (child-elems root))
        (match-case (dom:tag-name toplevel-child)
          ("tags"
           (dolist (tag-elem (child-elems toplevel-child))
             (assert (equal (dom:tag-name tag-elem) "tag"))
             (let ((name (get-attribute tag-elem "name")))
               (assert name)
               (assert (null (shiftf (gethash name tag-table) t))))))
          ("types"
           (dolist (type-elem (child-elems toplevel-child))
             (unless (or (not (equal (dom:tag-name type-elem) "type"))
                         (member (get-attribute type-elem "category") '(nil "include" "define") :test #'equal))
               (let ((name (or (get-attribute type-elem "name")
                               (extract-contents-str (find "name" (child-elems type-elem)
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
                               (extract-contents-str (find "name" (child-elems proto)
                                                           :key #'dom:tag-name :test #'equal))))))
               (assert (null (shiftf (gethash name func-table) command-elem))))))
          ("feature"
           (let ((name (get-attribute toplevel-child "name")))
             (assert name)
             (assert (null (shiftf (gethash name feat-table) toplevel-child)))))
          ("extensions"
           (dolist (extension-elem (child-elems toplevel-child))
             (assert (equal (dom:tag-name extension-elem) "extension"))
             (let ((name (get-attribute extension-elem "name")))
               (assert name)
               (assert (null (shiftf (gethash name ext-table) extension-elem))))))))
      (setf (gethash file *cache*)
            (setf (gethash truename *cache*)
                  (list tag-table type-table const-table func-table feat-table ext-table))))
    truename))
(defmacro load-registry (&optional (file '*vulkan-file*))
  `(eval-when (:compile-toplevel :execute)
     (%load-registry ,file)))

(defmacro define-vulkan-func (kind (name vulkan-name &optional wrap) return-type &body args)
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
                                     (let ((result (foreign-funcall-pointer ,pfn () ,@',(mapcan #'reverse args)
                                                                            ,',return-type)))
                                       ,',(or wrap 'result))
                                     ,',loopy)))
                        (,name ,@(mapcar #'first args)))))
          (setf (car loopy) (car body)
                (cdr loopy) (cdr body))
          loopy))))

(defun gen-vulkan-bindings (tag-table type-table const-table func-table feat-table ext-table &rest body)
  (declare (ignore feat-table ext-table))
  (multiple-value-bind (imported-types types functions)
      (loop for (kind . names) in body
            when (eql kind :imported-types)
              append names into imported-types
            when (eql kind :types)
              append names into types
            when (eql kind :functions)
              append names into functions
            finally (return (values imported-types types functions)))
    (labels ((normalize-type (type &optional require-type-func)
               (match-ecase type
                 ((:pointer &rest) :pointer)
                 ((:array inner len) `(:array ,(normalize-type inner) ,(eval-c-expr len const-table)))
                 ("void" :void)
                 ("char" :char)
                 ("float" :float)
                 ("uint8_t" :uint8)
                 ("int32_t" :int32)
                 ("uint32_t" :uint32)
                 ("uint64_t" :uint64)
                 ("size_t" :size)
                 ("VkBool32" `(:boolean :uint32))
                 ((m:or "xcb_window_t" "xcb_visualid_t") :uint32)
                 ((m:type string)
                  (let ((sym (or (find type types :key (lambda (x) (vulkanize-type-name x tag-table))
                                                  :test #'equal)
                                 (find type imported-types :key (lambda (x) (vulkanize-type-name x tag-table))
                                                           :test #'equal)))
                        (elem (first (gethash type type-table))))
                    (when (and sym require-type-func) (funcall require-type-func sym))
                    (match-ecase (get-attribute elem "category")
                      ;; FIXME I don't think we necessarily know that it's supposed to be a uint32
                      ("basetype" (assert sym) sym)
                      ("funcpointer" :pointer)
                      ((m:or "enum" "bitmask") (or sym :uint32))
                      ("struct" `(:struct ,(or sym (error "Struct ~A does not have a name!" type))))
                      ("union" `(:union ,(or sym (error "Union ~A does not have a name!" type))))
                      ("handle" (assert sym) sym))))))
             (enumlike-body (elem prefix)
               (mapcar (lambda (value)
                         (assert (equal (dom:tag-name value) "enum"))
                         (let ((val-name (get-attribute value "name")))
                           (assert (string= prefix val-name :end2 (length prefix)))
                           `(,(intern (nsubstitute #\- #\_ (subseq val-name (length prefix))) :keyword)
                             ,(let ((val-str (get-attribute value "value"))
                                    (bitpos (get-attribute value "bitpos")))
                                (if val-str
                                    (progn (assert (null bitpos))
                                           (eval-c-expr (parse-expression (list val-str)) const-table))
                                    (ash 1 (eval-c-expr (parse-expression (list bitpos)) const-table)))))))
                       (remove-if (lambda (el) (or (member (dom:tag-name el) '("comment" "unused") :test #'equal)
                                                   (member (get-attribute el "name")
                                                           '("VK_STENCIL_FRONT_AND_BACK"
                                                             "VK_COLORSPACE_SRGB_NONLINEAR_KHR")
                                                           :test #'equal)))
                                  (child-elems elem)))))
      `(progn
         ,@(let ((type-defn-forms ())
                 (type-status (make-hash-table)))
             (labels ((require-type (type-name)
                        (ecase (gethash type-name type-status)
                          ((nil))
                          ((:pending) (error "Type cycle detected: ~A" type-name))
                          ((:done) (return-from require-type)))
                        (setf (gethash type-name type-status) :pending)
                        (match-let* ((vk-name (vulkanize-type-name type-name tag-table))
                                     ((type-elem enums-elem) (gethash vk-name type-table)))
                          (match-ecase (get-attribute type-elem "category")
                            ("basetype"
                             (match-ecase (mapcan #'c-tokenize (extract-contents type-elem))
                               (((:ident "typedef") (:ident type) (:ident name) (:punc ";"))
                                (assert (equal name vk-name))
                                (push `(defctype ,type-name ,(normalize-type type #'require-type))
                                      type-defn-forms))))
                            ("handle"
                             (let ((base-type
                                     (match-ecase (extract-contents-str type-elem)
                                       ((m:equal (format nil "VK_DEFINE_HANDLE(~A)" vk-name)) :pointer)
                                       ((m:equal (format nil "VK_DEFINE_NON_DISPATCHABLE_HANDLE(~A)" vk-name))
                                        :uint64))))
                               (push `(defctype ,type-name ,base-type) type-defn-forms)))
                            ("enum"
                             (push
                              `(defcenum (,type-name :uint32)
                                 ,@(enumlike-body
                                    enums-elem
                                    (if (equal vk-name "VkResult")
                                        "VK_"
                                        (concatenate 'string (vulkanize-enum-value type-name tag-table) "_"))))
                              type-defn-forms))
                            ("bitmask"
                             (assert (equal (extract-contents-str type-elem)
                                            (format nil "typedef VkFlags ~A;" vk-name)))
                             (let* ((bits-enum (get-attribute type-elem "requires"))
                                    (bits-elem (when bits-enum (second (gethash bits-enum type-table)))))
                               (when bits-enum (assert bits-elem))
                               (push
                                `(defbitfield (,type-name :uint32)
                                   ,@(when bits-elem
                                       (enumlike-body
                                        bits-elem
                                        (concatenate 'string
                                                     (vulkanize-flag-value type-name tag-table) "_"))))
                                type-defn-forms)))
                            ((m:and category (m:or "struct" "union"))
                             (push
                              `(,(match-ecase category ("struct" 'defcstruct) ("union" 'defcunion)) ,type-name
                                ,@(mapcar (lambda (member)
                                            (assert (equal (dom:tag-name member) "member"))
                                            (destructuring-bind (name type)
                                                (parse-type-decl (extract-contents member))
                                              `(,(unvulkanize-field-name name type :keyword)
                                                ,(normalize-type type #'require-type))))
                                          (remove "comment" (child-elems type-elem)
                                                  :key #'dom:tag-name :test #'equal)))
                              type-defn-forms))))
                        (setf (gethash type-name type-status) :done)))
               (map () #'require-type types))
             (nreverse type-defn-forms))
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
                         (function-kind-for-type (get-attribute (first (gethash type type-table)) "parent")))))
                    (parse-codes-list (str)
                      (do* ((result ())
                            (start 0 (1+ (or comma-pos (return (nreverse result)))))
                            (comma-pos #1=(position #\, str :start start) #1#))
                           (nil)
                        (let* ((substr (subseq str start comma-pos))
                               (el (gethash substr const-table))
                               ;; HACK we should eventually always
                               ;; find constants! This is only
                               ;; needed because we're not parsing
                               ;; extensions yet
                               (value (when el (parse-integer (get-attribute el "value")))))
                          (when value
                            (push (cons substr value) result)))))
                    (make-result-wrapper (func-el)
                      `(ecase result
                         ,@(mapcar
                            (lambda (success-code)
                              `((,(cdr success-code)) ,(intern (subseq (car success-code) 3) :keyword)))
                            (parse-codes-list (get-attribute func-el "successcodes")))
                         ,@(mapcar
                            (lambda (error-code)
                              `((,(cdr error-code)) (error "~A" ',(car error-code))))
                            (parse-codes-list (get-attribute func-el "errorcodes"))))))
             (mapcar
              (lambda (func-name)
                (let* ((vk-name (vulkanize-func-name func-name tag-table))
                       (func-el (gethash vk-name func-table)))
                  (destructuring-bind (proto &rest params) (child-elems func-el)
                    (setf params (remove "implicitexternsyncparams" params :key #'dom:tag-name :test #'equal))
                    (assert (equal (dom:tag-name proto) "proto"))
                    (assert (every (lambda (x) (equal (dom:tag-name x) "param")) params))
                    (destructuring-bind (name return-type) (parse-type-decl (extract-contents proto))
                      (let ((normalized-return-type (normalize-type return-type)))
                        (assert (equal name vk-name))
                        `(define-vulkan-func ,(function-kind vk-name params)
                             (,func-name ,vk-name ,@(when (equal return-type "VkResult")
                                                      (setf normalized-return-type :int)
                                                      `(,(make-result-wrapper func-el))))
                             ,normalized-return-type
                           ,@(mapcar (lambda (param)
                                       (destructuring-bind (name type) (parse-type-decl (extract-contents param))
                                         `(,(unvulkanize-field-name name type nil) ,(normalize-type type))))
                                     params)))))))
              functions))))))

(defmacro gen-bindings ((package-name &optional (file *vulkan-file*)) &body features-and-extensions)
  (multiple-value-bind (features extensions)
      (loop for (kind . names) in features-and-extensions
            when (eql kind :features)
              append names into features
            when (eql kind :extensions)
              append names into extensions
            finally (return (values features extensions)))
    (destructuring-bind (tag-table type-table const-table func-table feat-table ext-table) (gethash file *cache*)
      (let ((package (or (find-package package-name) (make-package package-name)))
            (syms ()))
        `(progn
           ,(apply #'gen-vulkan-bindings tag-table type-table const-table func-table feat-table ext-table
                   (mapcan
                    (lambda (require-elem)
                      (assert (equal (dom:tag-name require-elem) "require"))
                      (mapcan (lambda (required-thing)
                                (let ((name (get-attribute required-thing "name")))
                                  (match-ecase (dom:tag-name required-thing)
                                    ("type"
                                     (when-let (elem (first (gethash name type-table)))
                                       (unless (or (equal (get-attribute elem "category") "define")
                                                   (and (>= (length name) 4) (string= name "PFN_" :end1 4)))
                                         (let ((type-name (unvulkanize-type-name name tag-table package)))
                                           (push type-name syms)
                                           `((:types ,type-name))))))
                                    ("enum")
                                    ("command" (let ((type-name (unvulkanize-func-name name tag-table package)))
                                                 (push type-name syms)
                                                 `((:functions ,type-name))))
                                    ("comment"))))
                              (child-elems require-elem)))
                    (mappend #'child-elems
                             (nconc (mapcar (lambda (feature) (gethash feature feat-table)) features)
                                    (mapcar (lambda (extension) (gethash extension ext-table)) extensions)))))
           (eval-when (:compile-toplevel :load-toplevel :execute) (export ',(nreverse syms) ',package-name))
           (find-package ',(package-name package)))))))


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
