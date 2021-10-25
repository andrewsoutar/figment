(uiop:define-package #:com.andrewsoutar.figment/x11
  (:use #:cl #:alexandria #:cffi #:com.andrewsoutar.just-enough-x11)
  (:use #:com.andrewsoutar.figment/utils #:com.andrewsoutar.figment/vulkan-bind))
(cl:in-package #:com.andrewsoutar.figment/x11)

(define-from-xml "xproto"
  create-window map-window)


;;; XCB stuff - this should eventually end up in just-enough-x11
(defcfun xcb-connect :pointer
  (display :string)
  (screen (:pointer :int)))
(defcfun xcb-disconnect :void
  (connection :pointer))

(defcfun xcb-flush :int
  (connection :pointer))

(defcfun xcb-get-setup :pointer
  (connection :pointer))

(defcfun xcb-generate-id :uint32
  (connection :pointer))


;;; Vulkan stuff
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library libvulkan
    (:linux "libvulkan.so.1")
    (t (:default "libvulkan"))))
(use-foreign-library libvulkan)

(defctype flags :uint32)
(defctype vk-result :int)

(defctype vk-dispatchable :pointer)
(defctype vk-non-dispatchable :uint64)

(defcstruct offset-2d (x :int32) (y :int32))
(defcstruct offset-3d (x :int32) (y :int32) (z :int32))
(defcstruct extent-2d (width :uint32) (height :uint32))
(defcstruct extent-3d (width :uint32) (height :uint32) (depth :uint32))
(defcstruct rect-2d
  (offset (:struct offset-2d))
  (extent (:struct extent-2d)))

(gen-vulkan-bindings ()
  :special (flags)
  :enums (structure-type)
  :structs (instance-create-info))

(defvar *instance*)
(defvar *device*)
(macrolet ((frob (name var getter)
             `(defmacro ,name ((name vulkan-name) return-type &body lambda-list)
                `(defun ,name ,(mapcar #'first lambda-list)
                   ,(let* ((loopy (cons nil nil))
                           (body
                             `(let ((pfn ,,getter))
                                (eval `(defun ,',name ,',(mapcar #'first lambda-list)
                                         (if (eql ,,'',var ,,',var)
                                             (foreign-funcall-pointer ,pfn ()
                                                                      ,@',(mapcan #'reverse lambda-list)
                                                                      ,',return-type)
                                             ,',loopy)))
                                (,name ,@(mapcar #'first lambda-list)))))
                      (setf (car loopy) (car body)
                            (cdr loopy) (cdr body))
                      loopy)))))
  (frob define-vulkan-global-fun nil `(%vk-get-instance-proc-addr (null-pointer) ,vulkan-name))
  (frob define-vulkan-instance-fun *instance* `(%vk-get-instance-proc-addr (aref *instance*) ,vulkan-name))
  (frob define-vulkan-device-fun *device* `(%vk-get-device-proc-addr (aref *device*) ,vulkan-name)))

(defctype instance vk-dispatchable)
(defcfun (%vk-get-instance-proc-addr "vkGetInstanceProcAddr" :library libvulkan) :pointer
  (instance instance)
  (name :string))

(defctype device vk-dispatchable)
(define-vulkan-instance-fun (%vk-get-device-proc-addr "vkGetDeviceProcAddr") :pointer
  (device device)
  (name :string))


(define-vulkan-global-fun (%create-instance "vkCreateInstance") vk-result
  (create-info (:pointer (:struct instance-create-info)))
  (allocator :pointer)
  (instance (:pointer instance)))
(define-vulkan-instance-fun (%destroy-instance "vkDestroyInstance") :void
  (instance instance)
  (allocator :pointer))
(defun create-instance (layers extensions)
  (declare (type simple-vector layers extensions))
  (with-foreign-objects ((instance 'instance)
                         (create-info '(:struct instance-create-info))
                         (layers-arr :pointer (length layers))
                         (extensions-arr :pointer (length extensions)))
    (dotimes (i (length layers)) (setf (mem-aref layers-arr :pointer i) (null-pointer)))
    (dotimes (i (length extensions)) (setf (mem-aref extensions-arr :pointer i) (null-pointer)))
    (setf (mem-ref create-info '(:struct instance-create-info))
          (list :s-type :instance-create-info :next (null-pointer) :flags 0 :application-info (null-pointer)
                :enabled-layer-count (length layers) :enabled-layer-names layers-arr
                :enabled-extension-count (length extensions) :enabled-extension-names extensions-arr))
    (unwind-protect
         (progn
           (dotimes (i (length layers))
             (setf (mem-aref layers-arr :pointer i) (foreign-string-alloc (svref layers i))))
           (dotimes (i (length extensions))
             (setf (mem-aref extensions-arr :pointer i) (foreign-string-alloc (svref extensions i))))
           (unless (zerop (%create-instance create-info (null-pointer) instance))
             (error "vkCreateInstance"))
           (make-array () :initial-element (mem-ref instance 'instance)))
      (dotimes (i (length layers))
        (let ((ptr (mem-aref layers-arr :pointer i)))
          (unless (null-pointer-p ptr)
            (foreign-string-free ptr))))
      (dotimes (i (length extensions))
        (let ((ptr (mem-aref extensions-arr :pointer i)))
          (unless (null-pointer-p ptr)
            (foreign-string-free ptr)))))))

(defmacro with-instance ((layers extensions) &body body)
  `(with-cleanups ((*instance* (create-instance ,layers ,extensions)
                               (lambda (i) (%destroy-instance (aref i) (null-pointer)))))
     ,@body))

#+nil
(defmacro define-vulkan-object ((name kind &key create-structure-type suffix
                                             (actual-object-name name) (param 'instance))
                                &body create-info-fields)
  `(progn
     ,@(unless (eql kind :none)
         `((defctype ,actual-object-name
               ,(ecase kind
                  (:dispatchable :pointer)
                  (:non-dispatchable :uint64)))
           (defvkfun (,(intern (format nil "DESTROY-~A" name) (symbol-package name))
                      ,(format nil "vkDestroy~A~@[~A~]" (delete #\- (string-capitalize actual-object-name)) suffix))
                     :void
             ,@(when param `((,param ,param)))
             (,name ,actual-object-name)
             (allocator :pointer))))
     ,@(when create-structure-type
         (let ((create-name (intern (format nil "%CREATE-~A" name) (symbol-package name)))
               (create-info-name (intern (format nil "%~A-CREATE-INFO" name) (symbol-package name))))
           `((defcstruct ,create-info-name
               (stype structure-type)
               (next :pointer)
               ,@create-info-fields)
             (defvkfun (,create-name
                        ,(format nil "vkCreate~A~@[~A~]" (delete #\- (string-capitalize name)) suffix))
                       vk-result
               ,@(when param `((,param ,param)))
               (create-info (:pointer (:struct ,create-info-name)))
               (allocator :pointer)
               (,name (:pointer ,actual-object-name)))
             (defun ,(intern (format nil "CREATE-~A" name) (symbol-package name))
                 (,@(when param `(,param)) ,@(mapcar #'first create-info-fields))
               ,(with-gensyms (object create-info)
                  `(with-foreign-objects ((,object ',actual-object-name)
                                          (,create-info '(:struct ,create-info-name)))
                     (setf (mem-ref ,create-info '(:struct ,create-info-name))
                           (list 'stype ',create-structure-type
                                 'next (null-pointer)
                                 ,@(mapcan (lambda (ci) `(',(car ci) ,(car ci))) create-info-fields)))
                     (assert (zerop (let ,(when (null param) `((*instance* (null-pointer))))
                                      (,create-name ,@(when param `(,param)) ,create-info (null-pointer) ,object))))
                     (mem-ref ,object ',actual-object-name)))))))))

#+nil
(define-vulkan-object (instance :dispatchable :create-structure-type 1 :param nil)
  (flags flags)
  (application-info :pointer)
  (enabled-layer-count :uint32)
  (enabled-layers (:pointer :string))
  (enabled-extension-count :uint32)
  (enabled-extensions (:pointer :string)))

#+nil
(define-vulkan-object (surface :non-dispatchable :suffix "KHR"))
#+nil
(define-vulkan-object (xcb-surface :none :create-structure-type 1000005000 :suffix "KHR" :actual-object-name surface)
  (flags flags)
  (connection :pointer)
  (window :uint32))

(defctype surface vk-non-dispatchable)
(define-vulkan-instance-fun (%destroy-surface "vkDestroySurfaceKHR") :void
  (instance instance)
  (surface surface)
  (allocator :pointer))
(defun destroy-surface (surface)
  (%destroy-surface (aref *instance*) surface (null-pointer)))

(defcstruct %xcb-surface-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (connection :pointer)
  (window :uint32))
(define-vulkan-instance-fun (%create-xcb-surface "vkCreateXcbSurfaceKHR") vk-result
  (instance instance)
  (create-info (:pointer (:struct %xcb-surface-create-info)))
  (allocator :pointer)
  (surface (:pointer surface)))
(defun create-xcb-surface (create-info)
  (with-foreign-object (surface 'surface)
    (unless (zerop (%create-xcb-surface (aref *instance*) create-info (null-pointer) surface))
      (error "vkCreateSurfaceKHR"))
    (mem-ref surface 'surface)))


(defmacro with-enumerated-objects ((count array type) (wrapped-fun &rest parameters) &body body)
  (with-gensyms (block count-ptr retry result)
    `(block ,block
       (with-foreign-object (,count-ptr :uint32)
         (tagbody
            ,retry
            (unless (zerop (,wrapped-fun ,@parameters ,count-ptr (null-pointer)))
              (error "Error from enumerator ~A" ',wrapped-fun))
            (with-foreign-object (,array ',type (mem-ref ,count-ptr :uint32))
              (let ((,result (,wrapped-fun ,@parameters ,count-ptr ,array)))
                (case ,result
                  (0 (return-from ,block (let ((,count (mem-ref ,count-ptr :uint32))) ,@body)))
                  (5 (go ,retry))
                  (t (error "Error from enumerator ~A: ~A" ',wrapped-fun ,result))))))))))

(defctype physical-device vk-dispatchable)
(define-vulkan-instance-fun (enumerate-physical-devices "vkEnumeratePhysicalDevices") vk-result
  (instance instance)
  (phys-dev-count (:pointer :uint32))
  (phys-devs (:pointer physical-device)))

(defcstruct queue-family-properties
  (queue-flags flags)
  (queue-count :uint32)
  (timestamp-valid-bits :uint32)
  (min-image-transfer-granularity (:struct extent-3d)))
(define-vulkan-instance-fun (%get-physical-device-queue-family-properties
                             "vkGetPhysicalDeviceQueueFamilyProperties")
                            :void
  (physical-device physical-device)
  (count (:pointer :uint32))
  (array (:pointer (:struct queue-family-properties))))
(defun get-physical-device-queue-family-properties (physical-device count array)
  (%get-physical-device-queue-family-properties physical-device count array)
  0)


(define-vulkan-instance-fun (%get-physical-device-surface-support
                             "vkGetPhysicalDeviceSurfaceSupportKHR")
                            vk-result
  (physical-device physical-device)
  (queue-family-index :uint32)
  (surface surface)
  (supported (:pointer :uint32)))
(defun get-physical-device-surface-support (physical-device queue-family-index surface)
  (with-foreign-object (supported :uint32)
    (unless (zerop (%get-physical-device-surface-support physical-device queue-family-index surface supported))
      (error "vkGetPhysicalDeviceSurfaceSupportKHR"))
    (= 1 (mem-ref supported :uint32))))

(defcstruct extension-properties
  (name (:array :char 256))             ;VK_MAX_EXTENSION_NAME_SIZE
  (spec-version :uint32))
(define-vulkan-instance-fun (enumerate-device-extension-properties
                             "vkEnumerateDeviceExtensionProperties")
                            vk-result
  (physical-device physical-device)
  (layer-name :string)
  (property-count (:pointer :uint32))
  (properties (:pointer (:struct extension-properties))))

(defcstruct surface-format
  (format :int)
  (color-space :int))
(define-vulkan-instance-fun (get-physical-device-surface-formats
                             "vkGetPhysicalDeviceSurfaceFormatsKHR")
                            vk-result
  (device physical-device)
  (surface surface)
  (surface-format-count (:pointer :uint32))
  (surface-formats (:pointer (:struct surface-format))))


(defmacro do-foreign-array ((var array type length &key pointerp) &body body)
  (multiple-value-bind (index-var elem-var)
      (if (consp var) (values-list var) (values (gensym "I") var))
    (once-only (array)
      `(dotimes (,index-var ,length)
         (let ((,elem-var (,(if pointerp 'mem-aptr 'mem-aref) ,array ',type ,index-var)))
           (tagbody ,@body))))))


(defparameter *image-format* 50)
(defparameter *image-color-space* 0)

(defun find-device-queue (surface)
  (with-enumerated-objects (pd-count pd-array physical-device)
      (enumerate-physical-devices (aref *instance*))
    (do-foreign-array (physical-device pd-array physical-device pd-count)
      (with-enumerated-objects (ext-count ext-array (:struct extension-properties))
          (enumerate-device-extension-properties physical-device (null-pointer))
        (unless (every (lambda (ext)
                         (do-foreign-array (ext-ptr ext-array (:struct extension-properties) ext-count :pointerp t)
                           (when (string= ext (foreign-string-to-lisp
                                               (foreign-slot-pointer ext-ptr '(:struct extension-properties) 'name)
                                               :max-chars 256))
                             (return t))))
                       #("VK_KHR_swapchain"))
          (go skip)))
      (with-enumerated-objects (fmt-count fmt-array (:struct surface-format))
          (get-physical-device-surface-formats physical-device surface)
        (unless (do-foreign-array (fmt fmt-array (:struct surface-format) fmt-count)
                  (when (and (eql (getf fmt 'format) *image-format*)
                             (eql (getf fmt 'color-space) *image-color-space*))
                    (return t)))
          (go skip)))
      (with-enumerated-objects (qf-count qf-array (:struct queue-family-properties))
          (get-physical-device-queue-family-properties physical-device)
        (do-foreign-array ((j qf-ptr) qf-array (:struct queue-family-properties) qf-count :pointerp t)
          (when (and (logand (foreign-slot-value qf-ptr '(:struct queue-family-properties) 'queue-flags)
                             #x00000001 ; VK_QUEUE_GRAPHICS_BIT
                             )
                     (get-physical-device-surface-support physical-device j surface))
            (return-from find-device-queue (values physical-device j)))))
      skip))
  (error "No supported devices found"))


(defctype device vk-dispatchable)

(defcstruct device-queue-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (queue-family-index :uint32)
  (queue-count :uint32)
  (queue-priorities (:pointer :float)))
(defcstruct device-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (queue-create-info-count :uint32)
  (queue-create-infos (:pointer (:struct device-queue-create-info)))
  (enabled-layer-count :uint32)
  (enabled-layer-names (:pointer :string))
  (enabled-extension-count :uint32)
  (enabled-extension-names (:pointer :string))
  (enabled-features :pointer))
(define-vulkan-instance-fun (%create-device "vkCreateDevice") vk-result
  (physical-device physical-device)
  (create-info (:pointer (:struct device-create-info)))
  (allocator :pointer)
  (device (:pointer device)))

(define-vulkan-device-fun (%destroy-device "vkDestroyDevice") :void
  (device device)
  (allocator :pointer))

(defun create-device (physical-device queue &key (extensions #()) (queue-priority 1.0f0))
  (declare (type simple-vector extensions))
  (with-foreign-objects ((device 'device)
                         (device-create-info '(:struct device-create-info))
                         (extension-names :pointer (length extensions))
                         (queue-create-info '(:struct device-queue-create-info))
                         (priorities :float 1))
    (dotimes (i (length extensions)) (setf (mem-aref extension-names :pointer i) (null-pointer)))
    (unwind-protect
         (progn
           (dotimes (i (length extensions))
             (setf (mem-aref extension-names :pointer i) (foreign-string-alloc (svref extensions i))))
           (setf (mem-ref device-create-info '(:struct device-create-info))
                 (list 'stype :device-create-info 'next (null-pointer) 'flags 0
                       'queue-create-info-count 1 'queue-create-infos queue-create-info
                       'enabled-layer-count 0 'enabled-layer-names (null-pointer)
                       'enabled-extension-count (length extensions) 'enabled-extension-names extension-names
                       'enabled-features (null-pointer)))
           (setf (mem-ref queue-create-info '(:struct device-queue-create-info))
                 (list 'stype :device-queue-create-info 'next (null-pointer) 'flags 0
                       'queue-family-index queue 'queue-count 1 'queue-priorities priorities))
           (setf (mem-aref priorities :float 0) queue-priority)
           (unless (zerop (%create-device physical-device device-create-info (null-pointer) device))
             (error "vkCreateDevice"))
           (mem-ref device 'device))
      (dotimes (i (length extensions))
        (let ((ptr (mem-aref extension-names :pointer i)))
          (unless (null-pointer-p ptr) (foreign-string-free ptr)))))))
(defmacro with-device (device &body body)
  `(with-cleanups ((*device* (make-array () :initial-element ,device)
                             (lambda (d) (%destroy-device (aref d) (null-pointer)))))
     ,@body))


(defctype queue vk-dispatchable)
(define-vulkan-device-fun (%get-device-queue "vkGetDeviceQueue") :void
  (device device)
  (queue-family-index :uint32)
  (queue-index :uint32)
  (queue (:pointer queue)))

(defun get-device-queue (device queue-family-index queue-index)
  (with-foreign-object (queue 'queue)
    (%get-device-queue device queue-family-index queue-index queue)
    (mem-ref queue 'queue)))


(defcstruct surface-capabilities
  (min-image-count :uint32)
  (max-image-count :uint32)
  (current-extent (:struct extent-2d))
  (min-image-extent (:struct extent-2d))
  (max-image-extent (:struct extent-2d))
  (max-image-array-layers :uint32)
  (supported-transforms :int)
  (current-transform :int)
  (supported-composite-alpha :int)
  (supported-usage-flags flags))

(define-vulkan-instance-fun (get-physical-device-surface-capabilities
                             "vkGetPhysicalDeviceSurfaceCapabilitiesKHR")
                            vk-result
  (physical-device physical-device)
  (surface surface)
  (surface-capabilities (:pointer (:struct surface-capabilities))))


(defctype swapchain vk-non-dispatchable)

(defcstruct swapchain-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (surface surface)
  (min-image-count :uint32)
  (image-format :int)
  (image-color-space :int)
  (image-extent (:struct extent-2d))
  (image-array-layers :uint32)
  (image-usage flags)
  (image-sharing-mode :int)
  (queue-family-index-count :uint32)
  (queue-family-indices (:pointer :uint32))
  (pre-transform :int)
  (composite-alpha :int)
  (present-mode :int)
  (clipped :uint32)
  (old-swapchain swapchain))
(define-vulkan-device-fun (%create-swapchain "vkCreateSwapchainKHR") vk-result
  (device device)
  (create-info (:pointer (:struct swapchain-create-info)))
  (allocator :pointer)
  (swapchain (:pointer swapchain)))


(defctype image vk-non-dispatchable)
(define-vulkan-device-fun (get-swapchain-images "vkGetSwapchainImagesKHR") vk-result
  (device device)
  (swapchain swapchain)
  (swapchain-image-count (:pointer :uint32))
  (swapchain-images (:pointer image)))


(defun create-swapchain (surface &key min-image-count)
  (with-foreign-objects ((swapchain 'swapchain)
                         (create-info '(:struct swapchain-create-info)))
    (setf (mem-ref create-info '(:struct swapchain-create-info))
          (list 'stype 1000001000 'next (null-pointer) 'flags 0
                'surface surface 'min-image-count min-image-count
                'image-format *image-format* 'image-color-space *image-color-space*
                'image-extent '(width 600 height 400) 'image-array-layers 1
                'image-usage #x00000010 ; VK_IMAGE_USE_COLOR_ATTACHMENT_BIT
                'image-sharing-mode 0
                'queue-family-index-count 0 'queue-family-indices (null-pointer)
                'pre-transform #x00000001  ; VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
                'composite-alpha #x00000001 ; VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
                'present-mode 2         ; VK_PRESENT_MODE_FIFO_KHR
                'clipped 0 'old-swapchain 0))
    (unless (zerop (%create-swapchain (aref *device*) create-info (null-pointer) swapchain))
      (error "vkCreateSwapchainKHR"))
    (mem-ref swapchain 'swapchain)))

(define-vulkan-device-fun (%destroy-swapchain "vkDestroySwapchainKHR") :void
  (device device)
  (swapchain swapchain)
  (allocator :pointer))
(defun destroy-swapchain (swapchain)
  (%destroy-swapchain (aref *device*) swapchain (null-pointer)))


(defctype image-view vk-non-dispatchable)

(defcstruct component-mapping (r :int) (g :int) (b :int) (a :int))
(defcstruct image-subresource-range
  (aspect-mask flags)
  (base-mip-level :uint32)
  (level-count :uint32)
  (base-array-layer :uint32)
  (layer-count :uint32))
(defcstruct image-view-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (image image)
  (view-type :int)
  (format :int)
  (components (:struct component-mapping))
  (subresource-range (:struct image-subresource-range)))

(define-vulkan-device-fun (%create-image-view "vkCreateImageView") vk-result
  (device device)
  (create-info (:pointer (:struct image-view-create-info)))
  (allocator :pointer)
  (view (:pointer image-view)))
(defun create-image-view (image)
  (with-foreign-objects ((view 'image-view)
                         (create-info '(:struct image-view-create-info)))
    (setf (mem-ref create-info '(:struct image-view-create-info))
          (list 'stype :image-view-create-info 'next (null-pointer) 'flags 0
                'image image 'view-type 1 ; VK_IMAGE_VIEW_TYPE_2D
                'format *image-format*
                'components '(r 0 b 0 g 0 a 0)
                'subresource-range '(aspect-mask #x00000001 base-mip-level 0 level-count 1
                                     base-array-layer 0 layer-count 1)))
    (unless (zerop (%create-image-view (aref *device*) create-info (null-pointer) view))
      (error "vkCreateImageView"))
    (mem-ref view 'image-view)))

(define-vulkan-device-fun (%destroy-image-view "vkDestroyImageView") :void
  (device device)
  (image-view image-view)
  (allocator :pointer))
(defun destroy-image-view (image-view)
  (%destroy-image-view (aref *device*) image-view (null-pointer)))


(defparameter *vert-shader*
  ;; Compiled with glslc from shader.vert
  (coerce #(3 2 35 7 0 0 1 0 10 0 13 0 40 0 0 0 0 0 0 0 17 0 2 0 1 0 0 0 11 0 6
            0 1 0 0 0 71 76 83 76 46 115 116 100 46 52 53 48 0 0 0 0 14 0 3 0 0
            0 0 0 1 0 0 0 15 0 7 0 0 0 0 0 4 0 0 0 109 97 105 110 0 0 0 0 25 0 0
            0 29 0 0 0 3 0 3 0 2 0 0 0 194 1 0 0 4 0 10 0 71 76 95 71 79 79 71
            76 69 95 99 112 112 95 115 116 121 108 101 95 108 105 110 101 95 100
            105 114 101 99 116 105 118 101 0 0 4 0 8 0 71 76 95 71 79 79 71 76
            69 95 105 110 99 108 117 100 101 95 100 105 114 101 99 116 105 118
            101 0 5 0 4 0 4 0 0 0 109 97 105 110 0 0 0 0 5 0 5 0 12 0 0 0 112
            111 115 105 116 105 111 110 115 0 0 0 5 0 6 0 23 0 0 0 103 108 95 80
            101 114 86 101 114 116 101 120 0 0 0 0 6 0 6 0 23 0 0 0 0 0 0 0 103
            108 95 80 111 115 105 116 105 111 110 0 6 0 7 0 23 0 0 0 1 0 0 0 103
            108 95 80 111 105 110 116 83 105 122 101 0 0 0 0 6 0 7 0 23 0 0 0 2
            0 0 0 103 108 95 67 108 105 112 68 105 115 116 97 110 99 101 0 6 0 7
            0 23 0 0 0 3 0 0 0 103 108 95 67 117 108 108 68 105 115 116 97 110
            99 101 0 5 0 3 0 25 0 0 0 0 0 0 0 5 0 6 0 29 0 0 0 103 108 95 86 101
            114 116 101 120 73 110 100 101 120 0 0 72 0 5 0 23 0 0 0 0 0 0 0 11
            0 0 0 0 0 0 0 72 0 5 0 23 0 0 0 1 0 0 0 11 0 0 0 1 0 0 0 72 0 5 0 23
            0 0 0 2 0 0 0 11 0 0 0 3 0 0 0 72 0 5 0 23 0 0 0 3 0 0 0 11 0 0 0 4
            0 0 0 71 0 3 0 23 0 0 0 2 0 0 0 71 0 4 0 29 0 0 0 11 0 0 0 42 0 0 0
            19 0 2 0 2 0 0 0 33 0 3 0 3 0 0 0 2 0 0 0 22 0 3 0 6 0 0 0 32 0 0 0
            23 0 4 0 7 0 0 0 6 0 0 0 2 0 0 0 21 0 4 0 8 0 0 0 32 0 0 0 0 0 0 0
            43 0 4 0 8 0 0 0 9 0 0 0 3 0 0 0 28 0 4 0 10 0 0 0 7 0 0 0 9 0 0 0
            32 0 4 0 11 0 0 0 6 0 0 0 10 0 0 0 59 0 4 0 11 0 0 0 12 0 0 0 6 0 0
            0 43 0 4 0 6 0 0 0 13 0 0 0 0 0 0 0 43 0 4 0 6 0 0 0 14 0 0 0 0 0 0
            191 44 0 5 0 7 0 0 0 15 0 0 0 13 0 0 0 14 0 0 0 43 0 4 0 6 0 0 0 16
            0 0 0 0 0 0 63 44 0 5 0 7 0 0 0 17 0 0 0 16 0 0 0 16 0 0 0 44 0 5 0
            7 0 0 0 18 0 0 0 14 0 0 0 16 0 0 0 44 0 6 0 10 0 0 0 19 0 0 0 15 0 0
            0 17 0 0 0 18 0 0 0 23 0 4 0 20 0 0 0 6 0 0 0 4 0 0 0 43 0 4 0 8 0 0
            0 21 0 0 0 1 0 0 0 28 0 4 0 22 0 0 0 6 0 0 0 21 0 0 0 30 0 6 0 23 0
            0 0 20 0 0 0 6 0 0 0 22 0 0 0 22 0 0 0 32 0 4 0 24 0 0 0 3 0 0 0 23
            0 0 0 59 0 4 0 24 0 0 0 25 0 0 0 3 0 0 0 21 0 4 0 26 0 0 0 32 0 0 0
            1 0 0 0 43 0 4 0 26 0 0 0 27 0 0 0 0 0 0 0 32 0 4 0 28 0 0 0 1 0 0 0
            26 0 0 0 59 0 4 0 28 0 0 0 29 0 0 0 1 0 0 0 32 0 4 0 31 0 0 0 6 0 0
            0 7 0 0 0 43 0 4 0 6 0 0 0 34 0 0 0 0 0 128 63 32 0 4 0 38 0 0 0 3 0
            0 0 20 0 0 0 54 0 5 0 2 0 0 0 4 0 0 0 0 0 0 0 3 0 0 0 248 0 2 0 5 0
            0 0 62 0 3 0 12 0 0 0 19 0 0 0 61 0 4 0 26 0 0 0 30 0 0 0 29 0 0 0
            65 0 5 0 31 0 0 0 32 0 0 0 12 0 0 0 30 0 0 0 61 0 4 0 7 0 0 0 33 0 0
            0 32 0 0 0 81 0 5 0 6 0 0 0 35 0 0 0 33 0 0 0 0 0 0 0 81 0 5 0 6 0 0
            0 36 0 0 0 33 0 0 0 1 0 0 0 80 0 7 0 20 0 0 0 37 0 0 0 35 0 0 0 36 0
            0 0 13 0 0 0 34 0 0 0 65 0 5 0 38 0 0 0 39 0 0 0 25 0 0 0 27 0 0 0
            62 0 3 0 39 0 0 0 37 0 0 0 253 0 1 0 56 0 1 0)
          '(simple-array (unsigned-byte 8) 1)))
(defparameter *frag-shader*
  ;; Compiled with glslc from shader.frag
  (coerce #(3 2 35 7 0 0 1 0 10 0 13 0 13 0 0 0 0 0 0 0 17 0 2 0 1 0 0 0 11 0 6
            0 1 0 0 0 71 76 83 76 46 115 116 100 46 52 53 48 0 0 0 0 14 0 3 0 0
            0 0 0 1 0 0 0 15 0 6 0 4 0 0 0 4 0 0 0 109 97 105 110 0 0 0 0 9 0 0
            0 16 0 3 0 4 0 0 0 7 0 0 0 3 0 3 0 2 0 0 0 194 1 0 0 4 0 10 0 71 76
            95 71 79 79 71 76 69 95 99 112 112 95 115 116 121 108 101 95 108 105
            110 101 95 100 105 114 101 99 116 105 118 101 0 0 4 0 8 0 71 76 95
            71 79 79 71 76 69 95 105 110 99 108 117 100 101 95 100 105 114 101
            99 116 105 118 101 0 5 0 4 0 4 0 0 0 109 97 105 110 0 0 0 0 5 0 5 0
            9 0 0 0 111 117 116 67 111 108 111 114 0 0 0 0 71 0 4 0 9 0 0 0 30 0
            0 0 0 0 0 0 19 0 2 0 2 0 0 0 33 0 3 0 3 0 0 0 2 0 0 0 22 0 3 0 6 0 0
            0 32 0 0 0 23 0 4 0 7 0 0 0 6 0 0 0 4 0 0 0 32 0 4 0 8 0 0 0 3 0 0 0
            7 0 0 0 59 0 4 0 8 0 0 0 9 0 0 0 3 0 0 0 43 0 4 0 6 0 0 0 10 0 0 0 0
            0 128 63 43 0 4 0 6 0 0 0 11 0 0 0 0 0 0 0 44 0 7 0 7 0 0 0 12 0 0 0
            10 0 0 0 11 0 0 0 11 0 0 0 10 0 0 0 54 0 5 0 2 0 0 0 4 0 0 0 0 0 0 0
            3 0 0 0 248 0 2 0 5 0 0 0 62 0 3 0 9 0 0 0 12 0 0 0 253 0 1 0 56 0 1
            0)
          '(simple-array (unsigned-byte 8) 1)))


(defctype shader-module vk-non-dispatchable)

(defcstruct shader-module-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (code-size :size)
  (code (:pointer :uint32)))
(define-vulkan-device-fun (%create-shader-module "vkCreateShaderModule") vk-result
  (device device)
  (create-info (:pointer (:struct shader-module-create-info)))
  (allocator :pointer)
  (shader-module (:pointer shader-module)))
(defun create-shader-module (code)
  (with-foreign-objects ((shader-module 'shader-module)
                         (create-info '(:struct shader-module-create-info)))
    (with-pointer-to-vector-data (code-ptr code)
      (setf (mem-ref create-info '(:struct shader-module-create-info))
            (list 'stype :shader-module-create-info 'next (null-pointer) 'flags 0
                  'code-size (length code) 'code code-ptr))
      (unless (zerop (%create-shader-module (aref *device*) create-info (null-pointer) shader-module))
        (error "vkCreateShaderModule"))
      (mem-ref shader-module 'shader-module))))

(define-vulkan-device-fun (%destroy-shader-module "vkDestroyShaderModule") :void
  (device device)
  (shader-module shader-module)
  (allocator :pointer))
(defun destroy-shader-module (shader-module)
  (%destroy-shader-module (aref *device*) shader-module (null-pointer)))


(defctype pipeline-layout vk-non-dispatchable)

(defcstruct pipeline-layout-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (set-layout-count :uint32)
  (set-layouts :pointer)
  (push-constant-range-count :uint32)
  (push-constant-ranges :pointer))
(define-vulkan-device-fun (%create-pipeline-layout "vkCreatePipelineLayout") vk-result
  (device device)
  (create-info (:pointer (:struct pipeline-layout-create-info)))
  (allocator :pointer)
  (pipeline-layout (:pointer pipeline-layout)))
(defun create-pipeline-layout ()
  (with-foreign-objects ((layout 'pipeline-layout)
                         (create-info '(:struct pipeline-layout-create-info)))
    (setf (mem-ref create-info '(:struct pipeline-layout-create-info))
          (list 'stype :pipeline-layout-create-info 'next (null-pointer) 'flags 0
                'set-layout-count 0 'set-layouts (null-pointer)
                'push-constant-range-count 0 'push-constant-ranges (null-pointer)))
    (unless (zerop (%create-pipeline-layout (aref *device*) create-info (null-pointer) layout))
      (error "vkCreatePipelineLayout"))
    (mem-ref layout 'pipeline-layout)))

(define-vulkan-device-fun (%destroy-pipeline-layout "vkDestroyPipelineLayout") :void
  (device device)
  (pipeline-layout pipeline-layout)
  (allocator :pointer))
(defun destroy-pipeline-layout (pipeline-layout)
  (%destroy-pipeline-layout (aref *device*) pipeline-layout (null-pointer)))


(defctype render-pass vk-non-dispatchable)

(defcstruct attachment-reference
  (attachment :uint32)
  (layout :int))
(defcstruct attachment-description
  (flags flags)
  (format :int)
  (samples :int)
  (load-op :int)
  (store-op :int)
  (stencil-load-op :int)
  (stencil-store-op :int)
  (initial-layout :int)
  (final-layout :int))
(defcstruct subpass-description
  (flags flags)
  (pipeline-bind-point :int)
  (input-attachment-count :uint32)
  (input-attachments (:pointer (:struct attachment-reference)))
  (color-attachment-count :uint32)
  (color-attachments (:pointer (:struct attachment-reference)))
  (resolve-attachments (:pointer (:struct attachment-reference)))
  (depth-stencil-attachments (:pointer (:struct attachment-reference)))
  (preserve-attachment-count :uint32)
  (preserve-attachments (:pointer :uint32)))
(defcstruct render-pass-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (attachment-count :uint32)
  (attachments (:pointer (:struct attachment-description)))
  (subpass-count :uint32)
  (subpasses :pointer)
  (dependency-count :uint32)
  (dependencies :pointer))
(define-vulkan-device-fun (%create-render-pass "vkCreateRenderPass") vk-result
  (device device)
  (create-info (:pointer (:struct render-pass-create-info)))
  (allocator :pointer)
  (render-pass (:pointer render-pass)))
(defun create-render-pass (attachments subpass)
  (declare (type (vector list) attachments)
           (type list subpass))
  (with-foreign-objects ((render-pass 'render-pass)
                         (create-info '(:struct render-pass-create-info))
                         (attachments-ptr '(:struct attachment-description) (length attachments))
                         (subpass-ptr '(:struct subpass-description)))
    (dotimes (i (length attachments))
      (setf (mem-aref attachments-ptr '(:struct attachment-description) i) (aref attachments i)))
    (setf (mem-ref subpass-ptr '(:struct subpass-description)) subpass)
    (setf (mem-ref create-info '(:struct render-pass-create-info))
          (list 'stype :render-pass-create-info 'next (null-pointer) 'flags 0
                'attachment-count (length attachments) 'attachments attachments-ptr
                'subpass-count 1 'subpasses subpass-ptr
                'dependency-count 0 'dependencies (null-pointer)))
    (unless (zerop (%create-render-pass (aref *device*) create-info (null-pointer) render-pass))
      (error "vkCreateRenderPass"))
    (mem-ref render-pass 'render-pass)))

(define-vulkan-device-fun (%destroy-render-pass "vkDestroyRenderPass") :void
  (device device)
  (render-pass render-pass)
  (allocator :pointer))
(defun destroy-render-pass (render-pass)
  (%destroy-render-pass (aref *device*) render-pass (null-pointer)))


(defctype pipeline vk-non-dispatchable)
(define-vulkan-device-fun (%destroy-pipeline "vkDestroyPipeline") :void
  (device device)
  (pipeline pipeline)
  (allocator :pointer))
(defun destroy-pipeline (pipeline)
  (%destroy-pipeline (aref *device*) pipeline (null-pointer)))

(defcstruct pipeline-shader-stage-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (stage :int)
  (module shader-module)
  (name :string)
  (specialization-info :pointer))

(defcstruct pipeline-vertex-input-state-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (vertex-binding-description-count :uint32)
  (vertex-binding-descriptions :pointer)
  (vertex-attribute-description-count :uint32)
  (vertex-attribute-descriptions :pointer))

(defcstruct pipeline-input-assembly-state-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (topology :int)
  (primitive-restart-enable :uint32))

(defcstruct viewport
  (x :float) (y :float)
  (width :float) (height :float)
  (min-depth :float) (max-depth :float))
(defcstruct pipeline-viewport-state-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (viewport-count :uint32)
  (viewports (:pointer (:struct viewport)))
  (scissor-count :uint32)
  (scissors (:pointer (:struct rect-2d))))

(defcstruct pipeline-rasterization-state-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (depth-clamp-enable :uint32)
  (rasterizer-discard-enable :uint32)
  (polygon-mode :int)
  (cull-mode flags)
  (front-face :int)
  (depth-bias-enable :uint32)
  (depth-bias-constant-factor :float)
  (depth-bias-clamp :float)
  (depth-bias-slope-factor :float)
  (line-width :float))

(defcstruct pipeline-multisample-state-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (rasterization-samples :int)
  (sample-shading-enable :uint32)
  (min-sample-shading :float)
  (sample-mask :pointer)
  (alpha-to-converge-enable :uint32)
  (alpha-to-one-enable :uint32))

(defcstruct pipeline-color-blend-attachment-state
  (blend-enable :uint32)
  (src-color-blend-factor :int)
  (dst-color-blend-factor :int)
  (color-blend-op :int)
  (src-alpha-blend-factor :int)
  (dst-alpha-blend-factor :int)
  (alpha-blend-op :int)
  (color-write-mask flags))
(defcstruct pipeline-color-blend-state-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (logic-op-enable :uint32)
  (logic-op :int)
  (attachment-count :uint32)
  (attachments :pointer)
  (blend-constants (:array :float 4)))

(defcstruct graphics-pipeline-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (stage-count :uint32)
  (stages (:pointer (:struct pipeline-shader-stage-create-info)))
  (vertex-input-state (:pointer (:struct pipeline-vertex-input-state-create-info)))
  (input-assembly-state (:pointer (:struct pipeline-input-assembly-state-create-info)))
  (tessellation-state (:pointer #+nil (:struct pipeline-tessellation-state-create-info)))
  (viewport-state (:pointer (:struct pipeline-viewport-state-create-info)))
  (rasterization-state (:pointer (:struct pipeline-rasterization-state-create-info)))
  (multisample-state (:pointer (:struct pipeline-multisample-state-create-info)))
  (depth-stencil-state (:pointer #+nil (:struct pipeline-depth-stencil-state-create-info)))
  (color-blend-state (:pointer (:struct pipeline-color-blend-state-create-info)))
  (dynamic-state (:pointer #+nil (:struct pipeline-dynamic-state-create-info)))
  (layout pipeline-layout)
  (render-pass render-pass)
  (subpass :uint32)
  (base-pipeline-handle pipeline)
  (base-pipeline-index :uint32))

(define-vulkan-device-fun (%create-graphics-pipelines "vkCreateGraphicsPipelines") vk-result
  (device device)
  (pipeline-cache vk-non-dispatchable #+nil pipeline-cache)
  (create-info-count :uint32)
  (create-infos (:pointer (:struct graphics-pipeline-create-info)))
  (allocator :pointer)
  (pipelines (:pointer pipeline)))

(defun create-graphics-pipeline (vertex-module fragment-module layout render-pass)
  (with-foreign-objects ((pipeline 'pipeline)
                         (create-info '(:struct graphics-pipeline-create-info))
                         (stages-info '(:struct pipeline-shader-stage-create-info) 2)
                         (vertex-input-state '(:struct pipeline-vertex-input-state-create-info))
                         (input-assembly-state '(:struct pipeline-input-assembly-state-create-info))
                         (viewport '(:struct viewport) 1)
                         (scissor '(:struct rect-2d) 1)
                         (viewport-state '(:struct pipeline-viewport-state-create-info))
                         (rasterization-state '(:struct pipeline-rasterization-state-create-info))
                         (multisample-state '(:struct pipeline-multisample-state-create-info))
                         (attachments '(:struct pipeline-color-blend-attachment-state) 1)
                         (color-blend-state '(:struct pipeline-color-blend-state-create-info)))
    (with-foreign-string (str-main "main")
      (setf (mem-aref stages-info '(:struct pipeline-shader-stage-create-info) 0)
            (list 'stype :pipeline-shader-stage-create-info 'next (null-pointer) 'flags 0
                  'stage #x00000001     ; VK_SHADER_STAGE_VERTEX_BIT
                  'module vertex-module 'name str-main
                  'specialization-info (null-pointer)))
      (setf (mem-aref stages-info '(:struct pipeline-shader-stage-create-info) 1)
            (list 'stype :pipeline-shader-stage-create-info 'next (null-pointer) 'flags 0
                  'stage #x00000010     ; VK_SHADER_STAGE_FRAGMENT_BIT
                  'module fragment-module 'name str-main
                  'specialization-info (null-pointer)))

      (setf (mem-ref vertex-input-state '(:struct pipeline-vertex-input-state-create-info))
            (list 'stype :pipeline-vertex-input-state-create-info 'next (null-pointer) 'flags 0
                  'vertex-binding-description-count 0
                  'vertex-binding-descriptions (null-pointer)
                  'vertex-attribute-description-count 0
                  'vertex-attribute-descriptions (null-pointer)))
      (setf (mem-ref input-assembly-state '(:struct pipeline-input-assembly-state-create-info))
            (list 'stype :pipeline-input-assembly-state-create-info 'next (null-pointer) 'flags 0
                  'topology 3    ; VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                  'primitive-restart-enable 0))

      (setf (mem-aref viewport '(:struct viewport) 0)
            '(x 0f0 y 0f0 width 600f0 height 400f0 min-depth 0.0f0 max-depth 1.0f0))
      (setf (mem-aref scissor '(:struct rect-2d) 0)
            '(offset (x 0 y 0) extent (width 600 height 400)))
      (setf (mem-ref viewport-state '(:struct pipeline-viewport-state-create-info))
            (list 'stype :pipeline-viewport-state-create-info 'next (null-pointer) 'flags 0
                  'viewport-count 1 'viewports viewport
                  'scissor-count 1 'scissors scissor))

      (setf (mem-ref rasterization-state '(:struct pipeline-rasterization-state-create-info))
            (list 'stype :pipeline-rasterization-state-create-info 'next (null-pointer) 'flags 0
                  'depth-clamp-enable 0 'rasterizer-discard-enable 0
                  'polygon-mode 0    ; VK_POLYGON_MODE_FILL
                  'cull-mode 0       ; VK_CULL_MODE_NONE
                  'front-face 0      ; VK_FRONT_FACE_COUNTER_CLOCKWISE
                  'depth-bias-enable 0 'depth-bias-constant-factor 0.0f0
                  'depth-bias-clamp 0.0f0 'depth-bias-slope-factor 0.0f0
                  'line-width 1.0f0))

      (setf (mem-ref multisample-state '(:struct pipeline-multisample-state-create-info))
            (list 'stype :pipeline-multisample-state-create-info 'next (null-pointer) 'flags 0
                  'rasterization-samples #x00000001 ; VK_SAMPLE_COUNT_1_BIT
                  'sample-shading-enable 0 'min-sample-shading 1.0f0
                  'sample-mask (null-pointer)
                  'alpha-to-converge-enable 0 'alpha-to-one-enable 0))

      (setf (mem-aref attachments '(:struct pipeline-color-blend-attachment-state) 0)
            '(blend-enable 0 src-color-blend-factor 1 dst-color-blend-factor 0 color-blend-op 0
              src-alpha-blend-factor 1 dst-alpha-blend-factor 0 alpha-blend-op 0 color-write-mask 15))
      (setf (mem-ref color-blend-state '(:struct pipeline-color-blend-state-create-info))
            (list 'stype :pipeline-color-blend-state-create-info 'next (null-pointer) 'flags 0
                  'logic-op-enable 0 'logic-op 0
                  'attachment-count 1 'attachments attachments))
      (setf (foreign-slot-value color-blend-state '(:struct pipeline-color-blend-state-create-info) 'blend-constants)
            #(0.0f0 0.0f0 0.0f0 0.0f0))

      (setf (mem-ref create-info '(:struct graphics-pipeline-create-info))
            (list* 'stype :graphics-pipeline-create-info 'next (null-pointer) 'flags 0
                   'stage-count 2 'stages stages-info
                   'tessellation-state (null-pointer)
                   'depth-stencil-state (null-pointer)
                   'dynamic-state (null-pointer)
                   'layout layout
                   'render-pass render-pass 'subpass 0
                   'base-pipeline-handle 0 'base-pipeline-index #xffffffff
                   #.(cons 'list (mapcan (lambda (x) `(',x ,x))
                                         '(vertex-input-state input-assembly-state viewport-state
                                           rasterization-state multisample-state color-blend-state)))))

      (unless (zerop (%create-graphics-pipelines (aref *device*) 0 1 create-info (null-pointer) pipeline))
        (error "vkCreateGraphicsPipelines"))
      (mem-ref pipeline 'pipeline))))


(defctype framebuffer vk-non-dispatchable)

(defcstruct framebuffer-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (render-pass render-pass)
  (attachment-count :uint32)
  (attachments (:pointer image-view))
  (width :uint32)
  (height :uint32)
  (layers :uint32))
(define-vulkan-device-fun (%create-framebuffer "vkCreateFramebuffer") vk-result
  (device device)
  (create-info (:pointer (:struct framebuffer-create-info)))
  (allocator :pointer)
  (framebuffer (:pointer framebuffer)))
(defun create-framebuffer (render-pass image-view)
  (with-foreign-objects ((framebuffer 'framebuffer)
                         (create-info '(:struct framebuffer-create-info))
                         (attachment 'image-view 1))
    (setf (mem-ref attachment 'image-view) image-view)
    (setf (mem-ref create-info '(:struct framebuffer-create-info))
          (list 'stype :framebuffer-create-info 'next (null-pointer) 'flags 0
                'render-pass render-pass
                'attachment-count 1 'attachments attachment
                'width 600 'height 400 'layers 1))
    (unless (zerop (%create-framebuffer (aref *device*) create-info (null-pointer) framebuffer))
      (error "vkCreateFramebuffer"))
    (mem-ref framebuffer 'framebuffer)))

(define-vulkan-device-fun (%destroy-framebuffer "vkDestroyFramebuffer") :void
  (device device)
  (framebuffer framebuffer)
  (allocator :pointer))
(defun destroy-framebuffer (framebuffer)
  (%destroy-framebuffer (aref *device*) framebuffer (null-pointer)))


(defctype command-pool vk-non-dispatchable)

(defcstruct command-pool-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (queue-family-index :uint32))
(define-vulkan-device-fun (%create-command-pool "vkCreateCommandPool") vk-result
  (device device)
  (create-info (:pointer (:struct command-pool-create-info)))
  (allocator :pointer)
  (command-pool (:pointer command-pool)))
(defun create-command-pool (queue-family-index)
  (with-foreign-objects ((command-pool 'command-pool)
                         (create-info '(:struct command-pool-create-info)))
    (setf (mem-ref create-info '(:struct command-pool-create-info))
          (list 'stype :command-pool-create-info 'next (null-pointer) 'flags 0
                'queue-family-index queue-family-index))
    (unless (zerop (%create-command-pool (aref *device*) create-info (null-pointer) command-pool))
      (error "vkCreateCommandPool"))
    (mem-ref command-pool 'command-pool)))

(define-vulkan-device-fun (%destroy-command-pool "vkDestroyCommandPool") :void
  (device device)
  (command-pool command-pool)
  (allocator :pointer))
(defun destroy-command-pool (command-pool)
  (%destroy-command-pool (aref *device*) command-pool (null-pointer)))


(defctype command-buffer vk-dispatchable)

(defcstruct command-buffer-allocate-info
  (stype structure-type)
  (next :pointer)
  (command-pool command-pool)
  (level :int)
  (command-buffer-count :uint32))
(define-vulkan-device-fun (%allocate-command-buffers "vkAllocateCommandBuffers") vk-result
  (device device)
  (allocate-info (:pointer (:struct command-buffer-allocate-info)))
  (command-buffers (:pointer command-buffer)))
(defun allocate-command-buffers (command-buffers n-command-buffers command-pool)
  (with-foreign-object (allocate-info '(:struct command-buffer-allocate-info))
    (setf (mem-ref allocate-info '(:struct command-buffer-allocate-info))
          (list 'stype :command-buffer-allocate-info 'next (null-pointer)
                'command-pool command-pool 'level 0 'command-buffer-count n-command-buffers))
    (unless (zerop (%allocate-command-buffers (aref *device*) allocate-info command-buffers))
      (error "vkAllocateCommandBuffers"))))

(define-vulkan-device-fun (%free-command-buffers "vkFreeCommandBuffers") :void
  (device device)
  (command-pool command-pool)
  (command-buffer-count :uint32)
  (command-buffers (:pointer command-buffer)))
(defun free-command-buffers (command-pool command-buffers n-command-buffers)
  (%free-command-buffers (aref *device*) command-pool n-command-buffers command-buffers))


(defmacro defvk (kind (name vk-name) &body lambda-list)
  (let ((%name (intern (format nil "%~A" name) (symbol-package name)))
        (lambda-list (mapcar (lambda (ll) (if (consp ll) ll (list ll ll))) lambda-list)))
    `(progn (,(ecase kind
                ((:global) 'define-vulkan-global-fun)
                ((:instance) 'define-vulkan-instance-fun)
                ((:device) 'define-vulkan-device-fun))
             (,%name ,vk-name) vk-result ,@lambda-list)
            (defun ,name ,(mapcar #'first lambda-list)
              (unless (zerop (,%name ,@(mapcar #'first lambda-list)))
                (error "~A" ',vk-name))))))

(defcstruct command-buffer-begin-info
  (stype structure-type)
  (next :pointer)
  (flags flags)
  (inheritance-info :pointer))
(defvk :device (%begin-command-buffer "vkBeginCommandBuffer")
  command-buffer
  (begin-info (:pointer (:struct command-buffer-begin-info))))
(defun begin-command-buffer (command-buffer)
  (with-foreign-object (begin-info '(:struct command-buffer-begin-info))
    (setf (mem-ref begin-info '(:struct command-buffer-begin-info))
          (list 'stype :command-buffer-begin-info 'next (null-pointer) 'flags 0 'inheritance-info (null-pointer)))
    (%begin-command-buffer command-buffer begin-info)))

(defvk :device (end-command-buffer "vkEndCommandBuffer")
  command-buffer)

(defcunion clear-color-value
  (float32 (:array :float 4))
  (int32 (:array :int32 4))
  (uint32 (:array :uint32 4)))
(defcunion clear-depth-stencil-value
  (depth :float)
  (stencil :uint32))
(defcunion clear-value
  (color (:union clear-color-value))
  (depth-stencil (:union clear-depth-stencil-value)))
(defcstruct render-pass-begin-info
  (stype structure-type)
  (next :pointer)
  (render-pass render-pass)
  (framebuffer framebuffer)
  (render-area (:struct rect-2d))
  (clear-value-count :uint32)
  (clear-values (:pointer (:union clear-value))))
(define-vulkan-device-fun (cmd-begin-render-pass "vkCmdBeginRenderPass") :void
  (command-buffer command-buffer)
  (render-pass-begin (:pointer (:struct render-pass-begin-info)))
  (contents :int))

(define-vulkan-device-fun (cmd-bind-pipeline "vkCmdBindPipeline") :void
  (command-buffer command-buffer)
  (pipeline-bind-point :int)
  (pipeline pipeline))

(define-vulkan-device-fun (cmd-draw "vkCmdDraw") :void
  (command-buffer command-buffer)
  (vertex-count :uint32)
  (instance-count :uint32)
  (first-vertex :uint32)
  (first-instance :uint32))

(define-vulkan-device-fun (cmd-end-render-pass "vkCmdEndRenderPass") :void
  (command-buffer command-buffer))


(defctype semaphore vk-non-dispatchable)

(defcstruct semaphore-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags))
(defvk :device (%create-semaphore "vkCreateSemaphore")
  device
  (create-info (:pointer (:struct semaphore-create-info)))
  (allocator :pointer)
  (semaphore (:pointer semaphore)))
(defun create-semaphore ()
  (with-foreign-objects ((semaphore 'semaphore)
                         (create-info '(:struct semaphore-create-info)))
    (setf (mem-ref create-info '(:struct semaphore-create-info))
          (list 'stype :semaphore-create-info 'next (null-pointer) 'flags 0))
    (%create-semaphore (aref *device*) create-info (null-pointer) semaphore)
    (mem-ref semaphore 'semaphore)))

(define-vulkan-device-fun (%destroy-semaphore "vkDestroySemaphore") :void
  (device device)
  (semaphore semaphore)
  (allocator :pointer))
(defun destroy-semaphore (semaphore)
  (%destroy-semaphore (aref *device*) semaphore (null-pointer)))


(defctype fence vk-non-dispatchable)

(defcstruct fence-create-info
  (stype structure-type)
  (next :pointer)
  (flags flags))
(defvk :device (%create-fence "vkCreateFence")
  device
  (create-info (:pointer (:struct fence-create-info)))
  (allocator :pointer)
  (fence (:pointer fence)))
(defun create-fence ()
  (with-foreign-objects ((fence 'fence)
                         (create-info '(:struct fence-create-info)))
    (setf (mem-ref create-info '(:struct fence-create-info))
          (list 'stype :fence-create-info 'next (null-pointer) 'flags 0))
    (%create-fence (aref *device*) create-info (null-pointer) fence)
    (mem-ref fence 'fence)))

(define-vulkan-device-fun (%destroy-fence "vkDestroyFence") :void
  (device device)
  (fence fence)
  (allocator :pointer))
(defun destroy-fence (fence)
  (%destroy-fence (aref *device*) fence (null-pointer)))

(defvk :device (wait-for-fences "vkWaitForFences")
  device
  (fence-count :uint32)
  (fences (:pointer fence))
  (wait-all :uint32)
  (timeout :uint64))

(defvk :device (reset-fences "vkResetFences")
  device
  (fence-count :uint32)
  (fences (:pointer fence)))


(defvk :device (acquire-next-image "vkAcquireNextImageKHR")
  device swapchain (timeout :uint64) semaphore fence
  (image-index (:pointer :uint32)))


(defcstruct submit-info
  (stype structure-type)
  (next :pointer)
  (wait-semaphore-count :uint32)
  (wait-semaphores (:pointer semaphore))
  (wait-dst-stage-mask (:pointer flags))
  (command-buffer-count :uint32)
  (command-buffers (:pointer command-buffer))
  (signal-semaphore-count :uint32)
  (signal-semaphores (:pointer semaphore)))
(defvk :device (queue-submit "vkQueueSubmit")
  queue
  (submit-count :uint32)
  (submits :pointer)
  fence)


(defcstruct present-info
  (stype structure-type)
  (next :pointer)
  (wait-semaphore-count :uint32)
  (wait-semaphores (:pointer semaphore))
  (swapchain-count :uint32)
  (swapchains (:pointer swapchain))
  (image-indices (:pointer :uint32))
  (results (:pointer vk-result)))
(defvk :device (queue-present "vkQueuePresentKHR")
  queue (present-info (:pointer (:struct present-info))))


(defvk :device (queue-wait-idle "vkQueueWaitIdle") queue)


(defun main ()
  (nest
    (with-cleanups ((conn (xcb-connect (null-pointer) (null-pointer)) #'xcb-disconnect)))
    (let ((win (xcb-generate-id conn)))
      (let* ((setup (xcb-get-setup conn))
             (setup-len (* 4 (sb-sys:sap-ref-16 setup 6)))
             (vends-len-pad (* (ceiling (sb-sys:sap-ref-16 setup 24) 4) 4))
             (formats-len (* 8 (sb-sys:sap-ref-8 setup 29)))
             (setup-screen-offs (+ 40 vends-len-pad formats-len))
             (root (when (<= (+ setup-screen-offs 36) setup-len)
                     (sb-sys:sap-ref-32 setup (+ setup-screen-offs 0))))
             (visual (when (<= (+ setup-screen-offs 36) setup-len)
                       (sb-sys:sap-ref-32 setup (+ setup-screen-offs 32)))))
        (assert root)
        (create-window conn :copy-from-parent win root 0 0 600 400 10 :input-output visual)
        (unless (= (xcb-flush conn) 1)
          (error "xcb_flush"))))
    (with-instance (#("VK_LAYER_KHRONOS_validation") #("VK_KHR_surface" "VK_KHR_xcb_surface")))
    (with-cleanups ((surface (with-foreign-object (create-info '(:struct %xcb-surface-create-info))
                               (setf (mem-ref create-info '(:struct %xcb-surface-create-info))
                                     (list 'stype 1000005000 'next (null-pointer)
                                           'flags 0 'connection conn 'window win))
                               (create-xcb-surface create-info))
                             #'destroy-surface)))
    (multiple-value-bind (physical-device queue-family)
        (find-device-queue surface)
      (unless physical-device
        (error "No supported GPUs found :(")))
    (with-device (create-device physical-device queue-family :extensions #("VK_KHR_swapchain") :queue-priority 1.0f0))
    (let ((queue (get-device-queue (aref *device*) queue-family 0))))
    (with-cleanups ((swapchain (multiple-value-bind (min-image-count)
                                   (with-foreign-object (capabilities-p '(:struct surface-capabilities))
                                     (unless (zerop (get-physical-device-surface-capabilities
                                                     physical-device surface capabilities-p))
                                       (error "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"))
                                     (let ((capabilities (mem-ref capabilities-p '(:struct surface-capabilities))))
                                       (values (getf capabilities 'min-image-count))))
                                 (create-swapchain surface :min-image-count (1+ min-image-count)))
                               #'destroy-swapchain)))
    (let ((images (with-enumerated-objects (count array image) (get-swapchain-images (aref *device*) swapchain)
                    (let ((a (make-array count)))
                      (dotimes (i count) (setf (aref a i) (mem-aref array 'image i)))
                      a)))))
    (with-cleanups ((image-views (make-array (length images) :initial-element nil)
                                 (curry #'map () (conjoin #'identity #'destroy-image-view))))
      (map-into image-views #'create-image-view images))
    (with-cleanups ((vert-shader-mod (create-shader-module *vert-shader*) #'destroy-shader-module)
                    (frag-shader-mod (create-shader-module *frag-shader*) #'destroy-shader-module)
                    (pipeline-layout (create-pipeline-layout) #'destroy-pipeline-layout)
                    (render-pass (with-foreign-object (attach-ref '(:struct attachment-reference))
                                   (setf (mem-ref attach-ref '(:struct attachment-reference))
                                         '(attachment 0 layout 2))
                                   (create-render-pass
                                    (vector (list 'flags 0 'format *image-format* 'samples #x00000001
                                                  'load-op 1 'store-op 0 'stencil-load-op 2 'stencil-store-op 1
                                                  'initial-layout 0 'final-layout 1000001002))
                                    (list 'flags 0 'pipeline-bind-point 0
                                          'input-attachment-count 0 'input-attachments (null-pointer)
                                          'color-attachment-count 1 'color-attachments attach-ref
                                          'resolve-attachments (null-pointer)
                                          'depth-stencil-attachments (null-pointer)
                                          'preserve-attachment-count 0 'preserve-attachments (null-pointer))))
                                 #'destroy-render-pass)
                    (graphics-pipeline (create-graphics-pipeline vert-shader-mod frag-shader-mod
                                                                 pipeline-layout render-pass)
                                       #'destroy-pipeline)))
    (with-cleanups ((framebuffers (make-array (length image-views) :initial-element nil)
                                  (curry #'map () (conjoin #'identity #'destroy-framebuffer))))
      (map-into framebuffers (curry #'create-framebuffer render-pass) image-views))
    (with-cleanups ((cmd-pool (create-command-pool queue-family) #'destroy-command-pool)))
    (with-foreign-object (cmd-bufs 'command-buffer (length framebuffers)))
    (with-cleanups (((allocate-command-buffers cmd-bufs (length framebuffers) cmd-pool)
                     (lambda (ign)
                       (declare (ignore ign))
                       (free-command-buffers cmd-pool cmd-bufs (length framebuffers)))))
      (do-foreign-array ((i cb) cmd-bufs command-buffer (length framebuffers))
        (begin-command-buffer cb)
        (with-foreign-objects ((begin-info '(:struct render-pass-begin-info))
                               (clear-values '(:union clear-value) 1))
          (setf (foreign-slot-value (foreign-slot-pointer clear-values '(:union clear-value) 'color)
                                    '(:union clear-color-value) 'float32)
                #(1.0f0 1.0f0 1.0f0 1.0f0))
          (setf (mem-ref begin-info '(:struct render-pass-begin-info))
                (list 'stype :render-pass-begin-info 'next (null-pointer)
                      'render-pass render-pass 'framebuffer (aref framebuffers i)
                      'render-area '(offset (x 0 y 0) extent (width 600 height 400))
                      'clear-value-count 1 'clear-values clear-values))
          (cmd-begin-render-pass cb begin-info 0))
        (cmd-bind-pipeline cb 0 graphics-pipeline)
        (cmd-draw cb 3 1 0 0)
        (cmd-end-render-pass cb)
        (end-command-buffer cb)))
    (with-cleanups ((image-available-sem (create-semaphore) #'destroy-semaphore)
                    (render-finished-sem (create-semaphore) #'destroy-semaphore)
                    (fence (create-fence) #'destroy-fence)))
    (with-cleanups ((queue #'queue-wait-idle)))
    (progn (map-window conn win)
           (unless (= (xcb-flush conn) 1)
             (error "xcb_flush")))
    (loop
      (let ((image-index (with-foreign-object (img :uint32)
                           (acquire-next-image (aref *device*) swapchain (1- (ash 1 64))
                                               image-available-sem 0 img)
                           (mem-ref img :uint32))))
        (with-foreign-objects ((submit-info '(:struct submit-info) 1)
                               (wait-semaphores 'semaphore 1)
                               (wait-masks 'flags 1)
                               (signal-semaphores 'semaphore 1))
          (setf (mem-aref wait-semaphores 'semaphore 0) image-available-sem)
          (setf (mem-aref wait-masks 'flags 0) #x00000001)
          (setf (mem-aref signal-semaphores 'semaphore 0) render-finished-sem)
          (setf (mem-aref submit-info '(:struct submit-info) 0)
                (list 'stype :submit-info 'next (null-pointer)
                      'wait-semaphore-count 1 'wait-semaphores wait-semaphores 'wait-dst-stage-mask wait-masks
                      'command-buffer-count 1 'command-buffers (mem-aptr cmd-bufs 'command-buffer image-index)
                      'signal-semaphore-count 1 'signal-semaphores signal-semaphores))
          (queue-submit queue 1 submit-info fence))
        (with-foreign-objects ((present-info '(:struct present-info))
                               (wait-semaphores 'semaphore 1)
                               (swapchains 'swapchain 1)
                               (indices :uint32 1))
          (setf (mem-aref wait-semaphores 'semaphore 0) render-finished-sem)
          (setf (mem-aref swapchains 'swapchain 0) swapchain)
          (setf (mem-aref indices :uint32 0) image-index)
          (setf (mem-ref present-info '(:struct present-info))
                (list 'stype 1000001001 'next (null-pointer)
                      'wait-semaphore-count 1 'wait-semaphores wait-semaphores
                      'swapchain-count 1 'swapchains swapchains
                      'image-indices indices 'results (null-pointer)))
          (queue-present queue present-info)))
      (unless (= (xcb-flush conn) 1)
        (error "xcb_flush"))
      (with-foreign-object (fences 'fence 1)
        (setf (mem-aref fences 'fence 0) fence)
        (wait-for-fences (aref *device*) 1 fences 0 (1- (ash 1 64)))
        (reset-fences (aref *device*) 1 fences)))
    #+nil
    (progn
      (map-window conn win)
      (unless (= (xcb-flush conn) 1)
        (error "xcb_flush"))
      (sleep 100))))
