(uiop:define-package #:com.andrewsoutar.figment/vulkan-bind
  (:use #:cl #:alexandria #:cl-ppcre #:cffi #:com.andrewsoutar.matcher)
  (:local-nicknames (#:m #:com.andrewsoutar.matcher/matchers))
  (:import-from #:cxml)
  (:import-from #:cxml-dom)
  (:export #:gen-vulkan-bindings))
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

(defun parse-type-decl (decl type-translate-fun)
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
                 (maybe-tokens (match-token-or-fail (((m:or :int :float :double :long-double :literal) lit) lit))))
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
                           ((:ident "void") (return :void))
                           ((:ident "uint32_t") (return :uint32))
                           ((:ident type) (return (funcall type-translate-fun type)))))))
          (multiple-value-bind (name type-thunk) (parse-declaration)
            (unless (endp tokens) (throw 'parse-fail nil))
            (return-from parse-type-decl `(,name ,(funcall type-thunk type)))))))))

(defparameter *vulkan-file* #p"/usr/share/vulkan/registry/vk.xml")

(defmacro gen-vulkan-bindings ((&optional (file *vulkan-file*)) &body names)
  (destructuring-bind (structure-type instance-create-info flags) names
    (let ((root (dom:document-element (cxml:parse-file file (cxml-dom:make-dom-builder)))))
      (assert (equal (dom:tag-name root) "registry"))
      (let ((structure-type-el (find-if (lambda (el) (and (equal (dom:tag-name el) "enums")
                                                          (equal (get-attribute el "name") "VkStructureType")))
                                        (child-elems root)))
            (instance-create-info-el
              (find-if (lambda (el) (and (equal (dom:tag-name el) "type")
                                         (equal (get-attribute el "category") "struct")
                                         (equal (get-attribute el "name") "VkInstanceCreateInfo")))
                       (child-elems (find "types" (child-elems root) :key #'dom:tag-name :test #'equal)))))
        `(progn
           (defcenum ,structure-type
             ,@(mapcar
                (lambda (enum)
                  (assert (equal (dom:tag-name enum) "enum"))
                  (or
                   (register-groups-bind (name) ("^VK_STRUCTURE_TYPE_(.*)$" (get-attribute enum "name"))
                     `(,(intern (nsubstitute #\- #\_ name) :keyword) ,(parse-integer (get-attribute enum "value"))))
                   (error "Unable to parse element ~A" enum)))
                (child-elems structure-type-el)))
           (defcstruct ,instance-create-info
             ,@(mapcar
                (lambda (member)
                  (assert (equal (dom:tag-name member) "member"))
                  (parse-type-decl (extract-contents member)
                                   ;; HACK
                                   (lambda (type)
                                     (match-ecase type
                                       ("VkStructureType" structure-type)
                                       ("VkInstanceCreateFlags" flags)
                                       ;; screw it
                                       (t :void)))))
                (child-elems instance-create-info-el))))))))
