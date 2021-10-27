(uiop:define-package #:com.andrewsoutar.figment/x11
  (:use #:cl #:alexandria #:cffi #:com.andrewsoutar.just-enough-x11)
  (:use #:com.andrewsoutar.figment/utils #:com.andrewsoutar.figment/vulkan-bind)
  (:import-from #:com.andrewsoutar.figment/vulkan-bind
                #:*instance* #:*device* #:get-instance-proc-addr #:get-device-proc-addr))
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
(load-registry)

(defctype flags :uint32)
(defctype vk-result :int)

(defctype vk-dispatchable :pointer)
(defctype vk-non-dispatchable :uint64)

(gen-vulkan-bindings ()
  (:types structure-type
          offset-2d offset-3d extent-2d extent-3d rect-2d
          instance-create-info queue-family-properties extension-properties
          device-queue-create-info device-create-info
          component-mapping image-subresource-range image-view-create-info
          shader-module-create-info pipeline-layout-create-info
          attachment-reference attachment-description subpass-description render-pass-create-info
          pipeline-shader-stage-create-info pipeline-vertex-input-state-create-info
          pipeline-input-assembly-state-create-info viewport pipeline-viewport-state-create-info
          pipeline-rasterization-state-create-info pipeline-multisample-state-create-info
          pipeline-color-blend-attachment-state pipeline-color-blend-state-create-info
          graphics-pipeline-create-info framebuffer-create-info command-pool-create-info
          command-buffer-allocate-info command-buffer-begin-info render-pass-begin-info
          semaphore-create-info fence-create-info submit-info
          xcb-surface-create-info-khr surface-format-khr surface-capabilities-khr
          swapchain-create-info-khr present-info-khr)
  (:functions %create-instance %destroy-instance
              enumerate-physical-devices
              %get-physical-device-queue-family-properties
              enumerate-device-extension-properties
              %create-device %destroy-device %get-device-queue queue-submit queue-wait-idle queue-present-khr
              %create-image-view %destroy-image-view
              %create-shader-module %destroy-shader-module
              %create-pipeline-layout %destroy-pipeline-layout
              %create-render-pass %destroy-render-pass
              %destroy-pipeline %create-graphics-pipelines
              %create-framebuffer %destroy-framebuffer
              %create-command-pool %destroy-command-pool
              %allocate-command-buffers %free-command-buffers
              cmd-begin-render-pass cmd-bind-pipeline cmd-draw cmd-end-render-pass
              %create-semaphore %destroy-semaphore
              %create-fence %destroy-fence wait-for-fences reset-fences
              %destroy-surface-khr %create-xcb-surface-khr
              %get-physical-device-surface-support-khr
              get-physical-device-surface-formats-khr
              get-physical-device-surface-capabilities-khr
              %create-swapchain-khr %destroy-swapchain-khr get-swapchain-images-khr acquire-next-image-khr
              %begin-command-buffer end-command-buffer))


(defctype instance vk-dispatchable)

(defctype device vk-dispatchable)

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
           (%create-instance create-info (null-pointer) instance)
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

(defctype surface vk-non-dispatchable)

(defun destroy-surface (surface)
  (%destroy-surface-khr (aref *instance*) surface (null-pointer)))

(defun create-xcb-surface (create-info)
  (with-foreign-object (surface 'surface)
    (%create-xcb-surface-khr (aref *instance*) create-info (null-pointer) surface)
    (mem-ref surface 'surface)))


(defmacro with-enumerated-objects ((count array type) (wrapped-fun &rest parameters) &body body)
  (with-gensyms (block count-ptr retry result)
    `(block ,block
       (with-foreign-object (,count-ptr :uint32)
         (tagbody
            ,retry
            (unless (eql (,wrapped-fun ,@parameters ,count-ptr (null-pointer)) :success)
              (error "Error from enumerator ~A" ',wrapped-fun))
            (with-foreign-object (,array ',type (mem-ref ,count-ptr :uint32))
              (let ((,result (,wrapped-fun ,@parameters ,count-ptr ,array)))
                (case ,result
                  (:success (return-from ,block (let ((,count (mem-ref ,count-ptr :uint32))) ,@body)))
                  (:incomplete (go ,retry))
                  (t (error "Error from enumerator ~A: ~A" ',wrapped-fun ,result))))))))))

(defctype physical-device vk-dispatchable)

(defun get-physical-device-queue-family-properties (physical-device count array)
  (%get-physical-device-queue-family-properties physical-device count array)
  :success)


(defun get-physical-device-surface-support (physical-device queue-family-index surface)
  (with-foreign-object (supported :uint32)
    (%get-physical-device-surface-support-khr physical-device queue-family-index surface supported)
    (= 1 (mem-ref supported :uint32))))


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
                                               (foreign-slot-pointer ext-ptr '(:struct extension-properties)
                                                                     :extension-name)
                                               :max-chars 256))
                             (return t))))
                       #("VK_KHR_swapchain"))
          (go skip)))
      (with-enumerated-objects (fmt-count fmt-array (:struct surface-format-khr))
          (get-physical-device-surface-formats-khr physical-device surface)
        (unless (do-foreign-array (fmt fmt-array (:struct surface-format-khr) fmt-count)
                  (when (and (eql (getf fmt :format) *image-format*)
                             (eql (getf fmt :color-space) *image-color-space*))
                    (return t)))
          (go skip)))
      (with-enumerated-objects (qf-count qf-array (:struct queue-family-properties))
          (get-physical-device-queue-family-properties physical-device)
        (do-foreign-array ((j qf-ptr) qf-array (:struct queue-family-properties) qf-count :pointerp t)
          (when (and (logand (foreign-slot-value qf-ptr '(:struct queue-family-properties) :queue-flags)
                             #x00000001 ; VK_QUEUE_GRAPHICS_BIT
                             )
                     (get-physical-device-surface-support physical-device j surface))
            (return-from find-device-queue (values physical-device j)))))
      skip))
  (error "No supported devices found"))


(defctype device vk-dispatchable)

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
                 (list :s-type :device-create-info :next (null-pointer) :flags 0
                       :queue-create-info-count 1 :queue-create-infos queue-create-info
                       :enabled-layer-count 0 :enabled-layer-names (null-pointer)
                       :enabled-extension-count (length extensions) :enabled-extension-names extension-names
                       :enabled-features (null-pointer)))
           (setf (mem-ref queue-create-info '(:struct device-queue-create-info))
                 (list :s-type :device-queue-create-info :next (null-pointer) :flags 0
                       :queue-family-index queue :queue-count 1 :queue-priorities priorities))
           (setf (mem-aref priorities :float 0) queue-priority)
           (%create-device physical-device device-create-info (null-pointer) device)
           (mem-ref device 'device))
      (dotimes (i (length extensions))
        (let ((ptr (mem-aref extension-names :pointer i)))
          (unless (null-pointer-p ptr) (foreign-string-free ptr)))))))
(defmacro with-device (device &body body)
  `(with-cleanups ((*device* (make-array () :initial-element ,device)
                             (lambda (d) (%destroy-device (aref d) (null-pointer)))))
     ,@body))


(defctype queue vk-dispatchable)

(defun get-device-queue (device queue-family-index queue-index)
  (with-foreign-object (queue 'queue)
    (%get-device-queue device queue-family-index queue-index queue)
    (mem-ref queue 'queue)))



(defctype swapchain vk-non-dispatchable)


(defctype image vk-non-dispatchable)

(defun create-swapchain (surface &key min-image-count)
  (with-foreign-objects ((swapchain 'swapchain)
                         (create-info '(:struct swapchain-create-info-khr)))
    (setf (mem-ref create-info '(:struct swapchain-create-info-khr))
          (list :s-type 1000001000 :next (null-pointer) :flags 0
                :surface surface :min-image-count min-image-count
                :image-format *image-format* :image-color-space *image-color-space*
                :image-extent '(:width 600 :height 400) :image-array-layers 1
                :image-usage #x00000010 ; VK_IMAGE_USE_COLOR_ATTACHMENT_BIT
                :image-sharing-mode 0
                :queue-family-index-count 0 :queue-family-indices (null-pointer)
                :pre-transform #x00000001  ; VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
                :composite-alpha #x00000001 ; VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
                :present-mode 2         ; VK_PRESENT_MODE_FIFO_KHR
                :clipped 0 :old-swapchain 0))
    (%create-swapchain-khr (aref *device*) create-info (null-pointer) swapchain)
    (mem-ref swapchain 'swapchain)))

(defun destroy-swapchain (swapchain)
  (%destroy-swapchain-khr (aref *device*) swapchain (null-pointer)))


(defctype image-view vk-non-dispatchable)

(defun create-image-view (image)
  (with-foreign-objects ((view 'image-view)
                         (create-info '(:struct image-view-create-info)))
    (setf (mem-ref create-info '(:struct image-view-create-info))
          (list :s-type :image-view-create-info :next (null-pointer) :flags 0
                :image image :view-type 1 ; VK_IMAGE_VIEW_TYPE_2D
                :format *image-format*
                :components '(:r 0 :b 0 :g 0 :a 0)
                :subresource-range '(:aspect-mask #x00000001 :base-mip-level 0 :level-count 1
                                     :base-array-layer 0 :layer-count 1)))
    (%create-image-view (aref *device*) create-info (null-pointer) view)
    (mem-ref view 'image-view)))

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

(defun create-shader-module (code)
  (with-foreign-objects ((shader-module 'shader-module)
                         (create-info '(:struct shader-module-create-info)))
    (with-pointer-to-vector-data (code-ptr code)
      (setf (mem-ref create-info '(:struct shader-module-create-info))
            (list :s-type :shader-module-create-info :next (null-pointer) :flags 0
                  :code-size (length code) :code code-ptr))
      (%create-shader-module (aref *device*) create-info (null-pointer) shader-module)
      (mem-ref shader-module 'shader-module))))

(defun destroy-shader-module (shader-module)
  (%destroy-shader-module (aref *device*) shader-module (null-pointer)))


(defctype pipeline-layout vk-non-dispatchable)

(defun create-pipeline-layout ()
  (with-foreign-objects ((layout 'pipeline-layout)
                         (create-info '(:struct pipeline-layout-create-info)))
    (setf (mem-ref create-info '(:struct pipeline-layout-create-info))
          (list :s-type :pipeline-layout-create-info :next (null-pointer) :flags 0
                :set-layout-count 0 :set-layouts (null-pointer)
                :push-constant-range-count 0 :push-constant-ranges (null-pointer)))
    (%create-pipeline-layout (aref *device*) create-info (null-pointer) layout)
    (mem-ref layout 'pipeline-layout)))

(defun destroy-pipeline-layout (pipeline-layout)
  (%destroy-pipeline-layout (aref *device*) pipeline-layout (null-pointer)))


(defctype render-pass vk-non-dispatchable)

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
          (list :s-type :render-pass-create-info :next (null-pointer) :flags 0
                :attachment-count (length attachments) :attachments attachments-ptr
                :subpass-count 1 :subpasses subpass-ptr
                :dependency-count 0 :dependencies (null-pointer)))
    (%create-render-pass (aref *device*) create-info (null-pointer) render-pass)
    (mem-ref render-pass 'render-pass)))

(defun destroy-render-pass (render-pass)
  (%destroy-render-pass (aref *device*) render-pass (null-pointer)))


(defctype pipeline vk-non-dispatchable)

(defun destroy-pipeline (pipeline)
  (%destroy-pipeline (aref *device*) pipeline (null-pointer)))


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
            (list :s-type :pipeline-shader-stage-create-info :next (null-pointer) :flags 0
                  :stage #x00000001     ; VK_SHADER_STAGE_VERTEX_BIT
                  :module vertex-module :name str-main
                  :specialization-info (null-pointer)))
      (setf (mem-aref stages-info '(:struct pipeline-shader-stage-create-info) 1)
            (list :s-type :pipeline-shader-stage-create-info :next (null-pointer) :flags 0
                  :stage #x00000010     ; VK_SHADER_STAGE_FRAGMENT_BIT
                  :module fragment-module :name str-main
                  :specialization-info (null-pointer)))

      (setf (mem-ref vertex-input-state '(:struct pipeline-vertex-input-state-create-info))
            (list :s-type :pipeline-vertex-input-state-create-info :next (null-pointer) :flags 0
                  :vertex-binding-description-count 0
                  :vertex-binding-descriptions (null-pointer)
                  :vertex-attribute-description-count 0
                  :vertex-attribute-descriptions (null-pointer)))
      (setf (mem-ref input-assembly-state '(:struct pipeline-input-assembly-state-create-info))
            (list :s-type :pipeline-input-assembly-state-create-info :next (null-pointer) :flags 0
                  :topology 3))  ; VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
      (setf (foreign-slot-value input-assembly-state '(:struct pipeline-input-assembly-state-create-info)
                                :primitive-restart-enable)
            nil)

      (setf (mem-aref viewport '(:struct viewport) 0)
            '(:x 0f0 :y 0f0 :width 600f0 :height 400f0 :min-depth 0.0f0 :max-depth 1.0f0))
      (setf (mem-aref scissor '(:struct rect-2d) 0)
            '(:offset (:x 0 :y 0) :extent (:width 600 :height 400)))
      (setf (mem-ref viewport-state '(:struct pipeline-viewport-state-create-info))
            (list :s-type :pipeline-viewport-state-create-info :next (null-pointer) :flags 0
                  :viewport-count 1 :viewports viewport
                  :scissor-count 1 :scissors scissor))

      (setf (mem-ref rasterization-state '(:struct pipeline-rasterization-state-create-info))
            (list :s-type :pipeline-rasterization-state-create-info :next (null-pointer) :flags 0
                  :polygon-mode 0    ; VK_POLYGON_MODE_FILL
                  :cull-mode 0       ; VK_CULL_MODE_NONE
                  :front-face 0      ; VK_FRONT_FACE_COUNTER_CLOCKWISE
                  :depth-bias-constant-factor 0.0f0
                  :depth-bias-clamp 0.0f0 :depth-bias-slope-factor 0.0f0
                  :line-width 1.0f0))
      (setf (foreign-slot-value rasterization-state '(:struct pipeline-rasterization-state-create-info)
                                :depth-clamp-enable)
            nil)
      (setf (foreign-slot-value rasterization-state '(:struct pipeline-rasterization-state-create-info)
                                :rasterizer-discard-enable)
            nil)
      (setf (foreign-slot-value rasterization-state '(:struct pipeline-rasterization-state-create-info)
                                :depth-bias-enable)
            nil)

      (setf (mem-ref multisample-state '(:struct pipeline-multisample-state-create-info))
            (list :s-type :pipeline-multisample-state-create-info :next (null-pointer) :flags 0
                  :rasterization-samples #x00000001 ; VK_SAMPLE_COUNT_1_BIT
                  :min-sample-shading 1.0f0
                  :sample-mask (null-pointer)
                  :alpha-to-coverage-enable nil :alpha-to-one-enable nil))
      (setf (foreign-slot-value multisample-state '(:struct pipeline-multisample-state-create-info)
                                :sample-shading-enable)
            nil)
      (setf (foreign-slot-value multisample-state '(:struct pipeline-multisample-state-create-info)
                                :alpha-to-coverage-enable)
            nil)
      (setf (foreign-slot-value multisample-state '(:struct pipeline-multisample-state-create-info)
                                :alpha-to-one-enable)
            nil)

      (setf (mem-aref attachments '(:struct pipeline-color-blend-attachment-state) 0)
            '(:src-color-blend-factor 1 :dst-color-blend-factor 0 :color-blend-op 0
              :src-alpha-blend-factor 1 :dst-alpha-blend-factor 0 :alpha-blend-op 0 :color-write-mask 15))
      (setf (foreign-slot-value attachments '(:struct pipeline-color-blend-attachment-state) :blend-enable) nil)
      (setf (mem-ref color-blend-state '(:struct pipeline-color-blend-state-create-info))
            (list :s-type :pipeline-color-blend-state-create-info :next (null-pointer) :flags 0
                  :logic-op-enable nil :logic-op 0
                  :attachment-count 1 :attachments attachments))
      (setf (foreign-slot-value color-blend-state '(:struct pipeline-color-blend-state-create-info) :logic-op-enable)
            nil)
      (setf (foreign-slot-value color-blend-state '(:struct pipeline-color-blend-state-create-info) :blend-constants)
            #(0.0f0 0.0f0 0.0f0 0.0f0))

      (setf (mem-ref create-info '(:struct graphics-pipeline-create-info))
            (list* :s-type :graphics-pipeline-create-info :next (null-pointer) :flags 0
                   :stage-count 2 :stages stages-info
                   :tessellation-state (null-pointer)
                   :depth-stencil-state (null-pointer)
                   :dynamic-state (null-pointer)
                   :layout layout
                   :render-pass render-pass :subpass 0
                   :base-pipeline-handle 0 :base-pipeline-index -1
                   #.(cons 'list (mapcan (lambda (x) `(,(intern (symbol-name x) :keyword) ,x))
                                         '(vertex-input-state input-assembly-state viewport-state
                                           rasterization-state multisample-state color-blend-state)))))

      (%create-graphics-pipelines (aref *device*) 0 1 create-info (null-pointer) pipeline)
      (mem-ref pipeline 'pipeline))))


(defctype framebuffer vk-non-dispatchable)

(defun create-framebuffer (render-pass image-view)
  (with-foreign-objects ((framebuffer 'framebuffer)
                         (create-info '(:struct framebuffer-create-info))
                         (attachment 'image-view 1))
    (setf (mem-ref attachment 'image-view) image-view)
    (setf (mem-ref create-info '(:struct framebuffer-create-info))
          (list :s-type :framebuffer-create-info :next (null-pointer) :flags 0
                :render-pass render-pass
                :attachment-count 1 :attachments attachment
                :width 600 :height 400 :layers 1))
    (%create-framebuffer (aref *device*) create-info (null-pointer) framebuffer)
    (mem-ref framebuffer 'framebuffer)))

(defun destroy-framebuffer (framebuffer)
  (%destroy-framebuffer (aref *device*) framebuffer (null-pointer)))


(defctype command-pool vk-non-dispatchable)

(defun create-command-pool (queue-family-index)
  (with-foreign-objects ((command-pool 'command-pool)
                         (create-info '(:struct command-pool-create-info)))
    (setf (mem-ref create-info '(:struct command-pool-create-info))
          (list :s-type :command-pool-create-info :next (null-pointer) :flags 0
                :queue-family-index queue-family-index))
    (%create-command-pool (aref *device*) create-info (null-pointer) command-pool)
    (mem-ref command-pool 'command-pool)))

(defun destroy-command-pool (command-pool)
  (%destroy-command-pool (aref *device*) command-pool (null-pointer)))


(defctype command-buffer vk-dispatchable)

(defun allocate-command-buffers (command-buffers n-command-buffers command-pool)
  (with-foreign-object (allocate-info '(:struct command-buffer-allocate-info))
    (setf (mem-ref allocate-info '(:struct command-buffer-allocate-info))
          (list :s-type :command-buffer-allocate-info :next (null-pointer)
                :command-pool command-pool :level 0 :command-buffer-count n-command-buffers))
    (%allocate-command-buffers (aref *device*) allocate-info command-buffers)))

(defun free-command-buffers (command-pool command-buffers n-command-buffers)
  (%free-command-buffers (aref *device*) command-pool n-command-buffers command-buffers))


(defun begin-command-buffer (command-buffer)
  (with-foreign-object (begin-info '(:struct command-buffer-begin-info))
    (setf (mem-ref begin-info '(:struct command-buffer-begin-info))
          (list :s-type :command-buffer-begin-info :next (null-pointer) :flags 0 :inheritance-info (null-pointer)))
    (%begin-command-buffer command-buffer begin-info)))

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


(defctype semaphore vk-non-dispatchable)

(defun create-semaphore ()
  (with-foreign-objects ((semaphore 'semaphore)
                         (create-info '(:struct semaphore-create-info)))
    (setf (mem-ref create-info '(:struct semaphore-create-info))
          (list :s-type :semaphore-create-info :next (null-pointer) :flags 0))
    (%create-semaphore (aref *device*) create-info (null-pointer) semaphore)
    (mem-ref semaphore 'semaphore)))

(defun destroy-semaphore (semaphore)
  (%destroy-semaphore (aref *device*) semaphore (null-pointer)))


(defctype fence vk-non-dispatchable)

(defun create-fence ()
  (with-foreign-objects ((fence 'fence)
                         (create-info '(:struct fence-create-info)))
    (setf (mem-ref create-info '(:struct fence-create-info))
          (list :s-type :fence-create-info :next (null-pointer) :flags 0))
    (%create-fence (aref *device*) create-info (null-pointer) fence)
    (mem-ref fence 'fence)))

(defun destroy-fence (fence)
  (%destroy-fence (aref *device*) fence (null-pointer)))


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
    (with-cleanups ((surface (with-foreign-object (create-info '(:struct xcb-surface-create-info-khr))
                               (setf (mem-ref create-info '(:struct xcb-surface-create-info-khr))
                                     (list :s-type 1000005000 :next (null-pointer)
                                           :flags 0 :connection conn :window win))
                               (create-xcb-surface create-info))
                             #'destroy-surface)))
    (multiple-value-bind (physical-device queue-family)
        (find-device-queue surface)
      (unless physical-device
        (error "No supported GPUs found :(")))
    (with-device (create-device physical-device queue-family :extensions #("VK_KHR_swapchain") :queue-priority 1.0f0))
    (let ((queue (get-device-queue (aref *device*) queue-family 0))))
    (with-cleanups ((swapchain (multiple-value-bind (min-image-count)
                                   (with-foreign-object (capabilities-p '(:struct surface-capabilities-khr))
                                     (get-physical-device-surface-capabilities-khr physical-device surface
                                                                                   capabilities-p)
                                     (let ((capabilities
                                             (mem-ref capabilities-p '(:struct surface-capabilities-khr))))
                                       (values (getf capabilities :min-image-count))))
                                 (create-swapchain surface :min-image-count (1+ min-image-count)))
                               #'destroy-swapchain)))
    (let ((images (with-enumerated-objects (count array image) (get-swapchain-images-khr (aref *device*) swapchain)
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
                                         '(:attachment 0 :layout 2))
                                   (create-render-pass
                                    (vector (list :flags 0 :format *image-format* :samples #x00000001
                                                  :load-op 1 :store-op 0 :stencil-load-op 2 :stencil-store-op 1
                                                  :initial-layout 0 :final-layout 1000001002))
                                    (list :flags 0 :pipeline-bind-point 0
                                          :input-attachment-count 0 :input-attachments (null-pointer)
                                          :color-attachment-count 1 :color-attachments attach-ref
                                          :resolve-attachments (null-pointer)
                                          :depth-stencil-attachment (null-pointer)
                                          :preserve-attachment-count 0 :preserve-attachments (null-pointer))))
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
                (list :s-type :render-pass-begin-info :next (null-pointer)
                      :render-pass render-pass :framebuffer (aref framebuffers i)
                      :render-area '(:offset (:x 0 :y 0) :extent (:width 600 :height 400))
                      :clear-value-count 1 :clear-values clear-values))
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
                           (acquire-next-image-khr (aref *device*) swapchain (1- (ash 1 64))
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
                (list :s-type :submit-info :next (null-pointer)
                      :wait-semaphore-count 1 :wait-semaphores wait-semaphores :wait-dst-stage-mask wait-masks
                      :command-buffer-count 1 :command-buffers (mem-aptr cmd-bufs 'command-buffer image-index)
                      :signal-semaphore-count 1 :signal-semaphores signal-semaphores))
          (queue-submit queue 1 submit-info fence))
        (with-foreign-objects ((present-info '(:struct present-info-khr))
                               (wait-semaphores 'semaphore 1)
                               (swapchains 'swapchain 1)
                               (indices :uint32 1))
          (setf (mem-aref wait-semaphores 'semaphore 0) render-finished-sem)
          (setf (mem-aref swapchains 'swapchain 0) swapchain)
          (setf (mem-aref indices :uint32 0) image-index)
          (setf (mem-ref present-info '(:struct present-info-khr))
                (list :s-type 1000001001 :next (null-pointer)
                      :wait-semaphore-count 1 :wait-semaphores wait-semaphores
                      :swapchain-count 1 :swapchains swapchains
                      :image-indices indices :results (null-pointer)))
          (queue-present-khr queue present-info)))
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
