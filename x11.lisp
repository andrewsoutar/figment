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
(uiop:define-package #:com.andrewsoutar.figment/x11/vk
  (:documentation "Vulkan bindings")
  (:use))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (uiop:add-package-local-nickname '#:vk '#:com.andrewsoutar.figment/x11/vk '#.(package-name *package*)))
(gen-bindings (#:vk)
  (:features "VK_VERSION_1_0")
  (:extensions "VK_KHR_surface" "VK_KHR_swapchain" "VK_KHR_xcb_surface"))


(defun create-instance (layers extensions)
  (declare (type simple-vector layers extensions))
  (with-foreign-objects ((instance 'vk:instance)
                         (create-info '(:struct vk:instance-create-info))
                         (layers-arr :pointer (length layers))
                         (extensions-arr :pointer (length extensions)))
    (dotimes (i (length layers)) (setf (mem-aref layers-arr :pointer i) (null-pointer)))
    (dotimes (i (length extensions)) (setf (mem-aref extensions-arr :pointer i) (null-pointer)))
    (setf (mem-ref create-info '(:struct vk:instance-create-info))
          (list :s-type :instance-create-info :next (null-pointer) :flags () :application-info (null-pointer)
                :enabled-layer-count (length layers) :enabled-layer-names layers-arr
                :enabled-extension-count (length extensions) :enabled-extension-names extensions-arr))
    (unwind-protect
         (progn
           (dotimes (i (length layers))
             (setf (mem-aref layers-arr :pointer i) (foreign-string-alloc (svref layers i))))
           (dotimes (i (length extensions))
             (setf (mem-aref extensions-arr :pointer i) (foreign-string-alloc (svref extensions i))))
           (vk:create-instance create-info (null-pointer) instance)
           (make-array () :initial-element (mem-ref instance 'vk:instance)))
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
                               (lambda (i) (vk:destroy-instance (aref i) (null-pointer)))))
     ,@body))

(defun destroy-surface (surface)
  (vk:destroy-surface-khr (aref *instance*) surface (null-pointer)))

(defun create-xcb-surface (create-info)
  (with-foreign-object (surface 'vk:surface-khr)
    (vk:create-xcb-surface-khr (aref *instance*) create-info (null-pointer) surface)
    (mem-ref surface 'vk:surface-khr)))


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

(defun get-physical-device-queue-family-properties (physical-device count array)
  (vk:get-physical-device-queue-family-properties physical-device count array)
  :success)


(defun get-physical-device-surface-support (physical-device queue-family-index surface)
  (with-foreign-object (supported :uint32)
    (vk:get-physical-device-surface-support-khr physical-device queue-family-index surface supported)
    (= 1 (mem-ref supported :uint32))))


(defmacro do-foreign-array ((var array type length &key pointerp) &body body)
  (multiple-value-bind (index-var elem-var)
      (if (consp var) (values-list var) (values (gensym "I") var))
    (once-only (array)
      `(dotimes (,index-var ,length)
         (let ((,elem-var (,(if pointerp 'mem-aptr 'mem-aref) ,array ',type ,index-var)))
           (tagbody ,@body))))))


(defparameter *image-format* :B8G8R8A8-srgb)
(defparameter *image-color-space* :srgb-nonlinear-khr)

(defun find-device-queue (surface)
  (with-enumerated-objects (pd-count pd-array vk:physical-device)
      (vk:enumerate-physical-devices (aref *instance*))
    (do-foreign-array (physical-device pd-array vk:physical-device pd-count)
      (with-enumerated-objects (ext-count ext-array (:struct vk:extension-properties))
          (vk:enumerate-device-extension-properties physical-device (null-pointer))
        (unless (every (lambda (ext)
                         (do-foreign-array (ext-ptr ext-array (:struct vk:extension-properties) ext-count :pointerp t)
                           (when (string= ext (foreign-string-to-lisp
                                               (foreign-slot-pointer ext-ptr '(:struct vk:extension-properties)
                                                                     :extension-name)
                                               :max-chars 256))
                             (return t))))
                       #("VK_KHR_swapchain"))
          (go skip)))
      (with-enumerated-objects (fmt-count fmt-array (:struct vk:surface-format-khr))
          (vk:get-physical-device-surface-formats-khr physical-device surface)
        (unless (do-foreign-array (fmt fmt-array (:struct vk:surface-format-khr) fmt-count)
                  (when (and (eql (getf fmt :format) *image-format*)
                             (eql (getf fmt :color-space) *image-color-space*))
                    (return t)))
          (go skip)))
      (with-enumerated-objects (qf-count qf-array (:struct vk:queue-family-properties))
          (get-physical-device-queue-family-properties physical-device)
        (do-foreign-array ((j qf-ptr) qf-array (:struct vk:queue-family-properties) qf-count :pointerp t)
          (when (and (member :graphics-bit
                             (foreign-slot-value qf-ptr '(:struct vk:queue-family-properties) :queue-flags))
                     (get-physical-device-surface-support physical-device j surface))
            (return-from find-device-queue (values physical-device j)))))
      skip))
  (error "No supported devices found"))


(defun create-device (physical-device queue &key (extensions #()) (queue-priority 1.0f0))
  (declare (type simple-vector extensions))
  (with-foreign-objects ((device 'vk:device)
                         (device-create-info '(:struct vk:device-create-info))
                         (extension-names :pointer (length extensions))
                         (queue-create-info '(:struct vk:device-queue-create-info))
                         (priorities :float 1))
    (dotimes (i (length extensions)) (setf (mem-aref extension-names :pointer i) (null-pointer)))
    (unwind-protect
         (progn
           (dotimes (i (length extensions))
             (setf (mem-aref extension-names :pointer i) (foreign-string-alloc (svref extensions i))))
           (setf (mem-ref device-create-info '(:struct vk:device-create-info))
                 (list :s-type :device-create-info :next (null-pointer) :flags ()
                       :queue-create-info-count 1 :queue-create-infos queue-create-info
                       :enabled-layer-count 0 :enabled-layer-names (null-pointer)
                       :enabled-extension-count (length extensions) :enabled-extension-names extension-names
                       :enabled-features (null-pointer)))
           (setf (mem-ref queue-create-info '(:struct vk:device-queue-create-info))
                 (list :s-type :device-queue-create-info :next (null-pointer) :flags ()
                       :queue-family-index queue :queue-count 1 :queue-priorities priorities))
           (setf (mem-aref priorities :float 0) queue-priority)
           (vk:create-device physical-device device-create-info (null-pointer) device)
           (mem-ref device 'vk:device))
      (dotimes (i (length extensions))
        (let ((ptr (mem-aref extension-names :pointer i)))
          (unless (null-pointer-p ptr) (foreign-string-free ptr)))))))
(defmacro with-device (device &body body)
  `(with-cleanups ((*device* (make-array () :initial-element ,device)
                             (lambda (d) (vk:destroy-device (aref d) (null-pointer)))))
     ,@body))


(defun get-device-queue (device queue-family-index queue-index)
  (with-foreign-object (queue 'vk:queue)
    (vk:get-device-queue device queue-family-index queue-index queue)
    (mem-ref queue 'vk:queue)))



(defun create-swapchain (surface &key min-image-count)
  (with-foreign-objects ((swapchain 'vk:swapchain-khr)
                         (create-info '(:struct vk:swapchain-create-info-khr)))
    (setf (mem-ref create-info '(:struct vk:swapchain-create-info-khr))
          (list :s-type 1000001000 :next (null-pointer) :flags ()
                :surface surface :min-image-count min-image-count
                :image-format *image-format* :image-color-space *image-color-space*
                :image-extent '(:width 600 :height 400) :image-array-layers 1
                :image-usage '(:color-attachment-bit)
                :image-sharing-mode :exclusive
                :queue-family-index-count 0 :queue-family-indices (null-pointer)
                :pre-transform :identity-bit-khr
                :composite-alpha :opaque-bit-khr
                :present-mode :fifo-khr
                :clipped nil :old-swapchain 0))
    (vk:create-swapchain-khr (aref *device*) create-info (null-pointer) swapchain)
    (mem-ref swapchain 'vk:swapchain-khr)))

(defun destroy-swapchain (swapchain)
  (vk:destroy-swapchain-khr (aref *device*) swapchain (null-pointer)))


(defun create-image-view (image)
  (with-foreign-objects ((view 'vk:image-view)
                         (create-info '(:struct vk:image-view-create-info)))
    (setf (mem-ref create-info '(:struct vk:image-view-create-info))
          (list :s-type :image-view-create-info :next (null-pointer) :flags ()
                :image image :view-type :2d
                :format *image-format*
                :components '(:r 0 :b 0 :g 0 :a 0)
                :subresource-range '(:aspect-mask (:color-bit) :base-mip-level 0 :level-count 1
                                     :base-array-layer 0 :layer-count 1)))
    (vk:create-image-view (aref *device*) create-info (null-pointer) view)
    (mem-ref view 'vk:image-view)))

(defun destroy-image-view (image-view)
  (vk:destroy-image-view (aref *device*) image-view (null-pointer)))


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


(defun create-shader-module (code)
  (with-foreign-objects ((shader-module 'vk:shader-module)
                         (create-info '(:struct vk:shader-module-create-info)))
    (with-pointer-to-vector-data (code-ptr code)
      (setf (mem-ref create-info '(:struct vk:shader-module-create-info))
            (list :s-type :shader-module-create-info :next (null-pointer) :flags ()
                  :code-size (length code) :code code-ptr))
      (vk:create-shader-module (aref *device*) create-info (null-pointer) shader-module)
      (mem-ref shader-module 'vk:shader-module))))

(defun destroy-shader-module (shader-module)
  (vk:destroy-shader-module (aref *device*) shader-module (null-pointer)))


(defun create-pipeline-layout ()
  (with-foreign-objects ((layout 'vk:pipeline-layout)
                         (create-info '(:struct vk:pipeline-layout-create-info)))
    (setf (mem-ref create-info '(:struct vk:pipeline-layout-create-info))
          (list :s-type :pipeline-layout-create-info :next (null-pointer) :flags ()
                :set-layout-count 0 :set-layouts (null-pointer)
                :push-constant-range-count 0 :push-constant-ranges (null-pointer)))
    (vk:create-pipeline-layout (aref *device*) create-info (null-pointer) layout)
    (mem-ref layout 'vk:pipeline-layout)))

(defun destroy-pipeline-layout (pipeline-layout)
  (vk:destroy-pipeline-layout (aref *device*) pipeline-layout (null-pointer)))


(defun create-render-pass (attachments subpass)
  (declare (type (vector list) attachments)
           (type list subpass))
  (with-foreign-objects ((render-pass 'vk:render-pass)
                         (create-info '(:struct vk:render-pass-create-info))
                         (attachments-ptr '(:struct vk:attachment-description) (length attachments))
                         (subpass-ptr '(:struct vk:subpass-description)))
    (dotimes (i (length attachments))
      (setf (mem-aref attachments-ptr '(:struct vk:attachment-description) i) (aref attachments i)))
    (setf (mem-ref subpass-ptr '(:struct vk:subpass-description)) subpass)
    (setf (mem-ref create-info '(:struct vk:render-pass-create-info))
          (list :s-type :render-pass-create-info :next (null-pointer) :flags ()
                :attachment-count (length attachments) :attachments attachments-ptr
                :subpass-count 1 :subpasses subpass-ptr
                :dependency-count 0 :dependencies (null-pointer)))
    (vk:create-render-pass (aref *device*) create-info (null-pointer) render-pass)
    (mem-ref render-pass 'vk:render-pass)))

(defun destroy-render-pass (render-pass)
  (vk:destroy-render-pass (aref *device*) render-pass (null-pointer)))


(defun destroy-pipeline (pipeline)
  (vk:destroy-pipeline (aref *device*) pipeline (null-pointer)))


(defun create-graphics-pipeline (vertex-module fragment-module layout render-pass)
  (with-foreign-objects ((pipeline 'vk:pipeline)
                         (create-info '(:struct vk:graphics-pipeline-create-info))
                         (stages-info '(:struct vk:pipeline-shader-stage-create-info) 2)
                         (vertex-input-state '(:struct vk:pipeline-vertex-input-state-create-info))
                         (input-assembly-state '(:struct vk:pipeline-input-assembly-state-create-info))
                         (viewport '(:struct vk:viewport) 1)
                         (scissor '(:struct vk:rect-2d) 1)
                         (viewport-state '(:struct vk:pipeline-viewport-state-create-info))
                         (rasterization-state '(:struct vk:pipeline-rasterization-state-create-info))
                         (multisample-state '(:struct vk:pipeline-multisample-state-create-info))
                         (attachments '(:struct vk:pipeline-color-blend-attachment-state) 1)
                         (color-blend-state '(:struct vk:pipeline-color-blend-state-create-info)))
    (with-foreign-string (str-main "main")
      (setf (mem-aref stages-info '(:struct vk:pipeline-shader-stage-create-info) 0)
            (list :s-type :pipeline-shader-stage-create-info :next (null-pointer) :flags ()
                  :stage :vertex-bit
                  :module vertex-module :name str-main
                  :specialization-info (null-pointer)))
      (setf (mem-aref stages-info '(:struct vk:pipeline-shader-stage-create-info) 1)
            (list :s-type :pipeline-shader-stage-create-info :next (null-pointer) :flags ()
                  :stage :fragment-bit
                  :module fragment-module :name str-main
                  :specialization-info (null-pointer)))

      (setf (mem-ref vertex-input-state '(:struct vk:pipeline-vertex-input-state-create-info))
            (list :s-type :pipeline-vertex-input-state-create-info :next (null-pointer) :flags ()
                  :vertex-binding-description-count 0
                  :vertex-binding-descriptions (null-pointer)
                  :vertex-attribute-description-count 0
                  :vertex-attribute-descriptions (null-pointer)))
      (setf (mem-ref input-assembly-state '(:struct vk:pipeline-input-assembly-state-create-info))
            (list :s-type :pipeline-input-assembly-state-create-info :next (null-pointer) :flags ()
                  :topology :triangle-list))
      (setf (foreign-slot-value input-assembly-state '(:struct vk:pipeline-input-assembly-state-create-info)
                                :primitive-restart-enable)
            nil)

      (setf (mem-aref viewport '(:struct vk:viewport) 0)
            '(:x 0f0 :y 0f0 :width 600f0 :height 400f0 :min-depth 0.0f0 :max-depth 1.0f0))
      (setf (mem-aref scissor '(:struct vk:rect-2d) 0)
            '(:offset (:x 0 :y 0) :extent (:width 600 :height 400)))
      (setf (mem-ref viewport-state '(:struct vk:pipeline-viewport-state-create-info))
            (list :s-type :pipeline-viewport-state-create-info :next (null-pointer) :flags ()
                  :viewport-count 1 :viewports viewport
                  :scissor-count 1 :scissors scissor))

      (setf (mem-ref rasterization-state '(:struct vk:pipeline-rasterization-state-create-info))
            (list :s-type :pipeline-rasterization-state-create-info :next (null-pointer) :flags ()
                  :polygon-mode :fill
                  :cull-mode nil
                  :front-face :counter-clockwise
                  :depth-bias-constant-factor 0.0f0
                  :depth-bias-clamp 0.0f0 :depth-bias-slope-factor 0.0f0
                  :line-width 1.0f0))
      (setf (foreign-slot-value rasterization-state '(:struct vk:pipeline-rasterization-state-create-info)
                                :depth-clamp-enable)
            nil)
      (setf (foreign-slot-value rasterization-state '(:struct vk:pipeline-rasterization-state-create-info)
                                :rasterizer-discard-enable)
            nil)
      (setf (foreign-slot-value rasterization-state '(:struct vk:pipeline-rasterization-state-create-info)
                                :depth-bias-enable)
            nil)

      (setf (mem-ref multisample-state '(:struct vk:pipeline-multisample-state-create-info))
            (list :s-type :pipeline-multisample-state-create-info :next (null-pointer) :flags ()
                  :rasterization-samples :1-bit
                  :min-sample-shading 1.0f0
                  :sample-mask (null-pointer)
                  :alpha-to-coverage-enable nil :alpha-to-one-enable nil))
      (setf (foreign-slot-value multisample-state '(:struct vk:pipeline-multisample-state-create-info)
                                :sample-shading-enable)
            nil)
      (setf (foreign-slot-value multisample-state '(:struct vk:pipeline-multisample-state-create-info)
                                :alpha-to-coverage-enable)
            nil)
      (setf (foreign-slot-value multisample-state '(:struct vk:pipeline-multisample-state-create-info)
                                :alpha-to-one-enable)
            nil)

      (setf (mem-aref attachments '(:struct vk:pipeline-color-blend-attachment-state) 0)
            '(:src-color-blend-factor :one :dst-color-blend-factor :zero :color-blend-op :add
              :src-alpha-blend-factor :one :dst-alpha-blend-factor :zero :alpha-blend-op :add
              :color-write-mask (:r-bit :g-bit :b-bit :a-bit)))
      (setf (foreign-slot-value attachments '(:struct vk:pipeline-color-blend-attachment-state) :blend-enable) nil)
      (setf (mem-ref color-blend-state '(:struct vk:pipeline-color-blend-state-create-info))
            (list :s-type :pipeline-color-blend-state-create-info :next (null-pointer) :flags ()
                  :logic-op-enable nil
                  :attachment-count 1 :attachments attachments))
      (setf (foreign-slot-value color-blend-state '(:struct vk:pipeline-color-blend-state-create-info)
                                :logic-op-enable)
            nil)
      (setf (foreign-slot-value color-blend-state '(:struct vk:pipeline-color-blend-state-create-info)
                                :blend-constants)
            #(0.0f0 0.0f0 0.0f0 0.0f0))

      (setf (mem-ref create-info '(:struct vk:graphics-pipeline-create-info))
            (list* :s-type :graphics-pipeline-create-info :next (null-pointer) :flags ()
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

      (vk:create-graphics-pipelines (aref *device*) 0 1 create-info (null-pointer) pipeline)
      (mem-ref pipeline 'vk:pipeline))))


(defun create-framebuffer (render-pass image-view)
  (with-foreign-objects ((framebuffer 'vk:framebuffer)
                         (create-info '(:struct vk:framebuffer-create-info))
                         (attachment 'vk:image-view 1))
    (setf (mem-ref attachment 'vk:image-view) image-view)
    (setf (mem-ref create-info '(:struct vk:framebuffer-create-info))
          (list :s-type :framebuffer-create-info :next (null-pointer) :flags ()
                :render-pass render-pass
                :attachment-count 1 :attachments attachment
                :width 600 :height 400 :layers 1))
    (vk:create-framebuffer (aref *device*) create-info (null-pointer) framebuffer)
    (mem-ref framebuffer 'vk:framebuffer)))

(defun destroy-framebuffer (framebuffer)
  (vk:destroy-framebuffer (aref *device*) framebuffer (null-pointer)))


(defun create-command-pool (queue-family-index)
  (with-foreign-objects ((command-pool 'vk:command-pool)
                         (create-info '(:struct vk:command-pool-create-info)))
    (setf (mem-ref create-info '(:struct vk:command-pool-create-info))
          (list :s-type :command-pool-create-info :next (null-pointer) :flags ()
                :queue-family-index queue-family-index))
    (vk:create-command-pool (aref *device*) create-info (null-pointer) command-pool)
    (mem-ref command-pool 'vk:command-pool)))

(defun destroy-command-pool (command-pool)
  (vk:destroy-command-pool (aref *device*) command-pool (null-pointer)))


(defun allocate-command-buffers (command-buffers n-command-buffers command-pool)
  (with-foreign-object (allocate-info '(:struct vk:command-buffer-allocate-info))
    (setf (mem-ref allocate-info '(:struct vk:command-buffer-allocate-info))
          (list :s-type :command-buffer-allocate-info :next (null-pointer)
                :command-pool command-pool :level :primary :command-buffer-count n-command-buffers))
    (vk:allocate-command-buffers (aref *device*) allocate-info command-buffers)))

(defun free-command-buffers (command-pool command-buffers n-command-buffers)
  (vk:free-command-buffers (aref *device*) command-pool n-command-buffers command-buffers))


(defun begin-command-buffer (command-buffer)
  (with-foreign-object (begin-info '(:struct vk:command-buffer-begin-info))
    (setf (mem-ref begin-info '(:struct vk:command-buffer-begin-info))
          (list :s-type :command-buffer-begin-info :next (null-pointer) :flags () :inheritance-info (null-pointer)))
    (vk:begin-command-buffer command-buffer begin-info)))


(defun create-semaphore ()
  (with-foreign-objects ((semaphore 'vk:semaphore)
                         (create-info '(:struct vk:semaphore-create-info)))
    (setf (mem-ref create-info '(:struct vk:semaphore-create-info))
          (list :s-type :semaphore-create-info :next (null-pointer) :flags ()))
    (vk:create-semaphore (aref *device*) create-info (null-pointer) semaphore)
    (mem-ref semaphore 'vk:semaphore)))

(defun destroy-semaphore (semaphore)
  (vk:destroy-semaphore (aref *device*) semaphore (null-pointer)))


(defun create-fence ()
  (with-foreign-objects ((fence 'vk:fence)
                         (create-info '(:struct vk:fence-create-info)))
    (setf (mem-ref create-info '(:struct vk:fence-create-info))
          (list :s-type :fence-create-info :next (null-pointer) :flags ()))
    (vk:create-fence (aref *device*) create-info (null-pointer) fence)
    (mem-ref fence 'vk:fence)))

(defun destroy-fence (fence)
  (vk:destroy-fence (aref *device*) fence (null-pointer)))


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
    (with-cleanups ((surface (with-foreign-object (create-info '(:struct vk:xcb-surface-create-info-khr))
                               (setf (mem-ref create-info '(:struct vk:xcb-surface-create-info-khr))
                                     (list :s-type 1000005000 :next (null-pointer)
                                           :flags () :connection conn :window win))
                               (create-xcb-surface create-info))
                             #'destroy-surface)))
    (multiple-value-bind (physical-device queue-family)
        (find-device-queue surface)
      (unless physical-device
        (error "No supported GPUs found :(")))
    (with-device (create-device physical-device queue-family :extensions #("VK_KHR_swapchain") :queue-priority 1.0f0))
    (let ((queue (get-device-queue (aref *device*) queue-family 0))))
    (with-cleanups ((swapchain (multiple-value-bind (min-image-count)
                                   (with-foreign-object (capabilities-p '(:struct vk:surface-capabilities-khr))
                                     (vk:get-physical-device-surface-capabilities-khr physical-device surface
                                                                                      capabilities-p)
                                     (let ((capabilities
                                             (mem-ref capabilities-p '(:struct vk:surface-capabilities-khr))))
                                       (values (getf capabilities :min-image-count))))
                                 (create-swapchain surface :min-image-count (1+ min-image-count)))
                               #'destroy-swapchain)))
    (let ((images (with-enumerated-objects (count array vk:image)
                      (vk:get-swapchain-images-khr (aref *device*) swapchain)
                    (let ((a (make-array count)))
                      (dotimes (i count) (setf (aref a i) (mem-aref array 'vk:image i)))
                      a)))))
    (with-cleanups ((image-views (make-array (length images) :initial-element nil)
                                 (curry #'map () (conjoin #'identity #'destroy-image-view))))
      (map-into image-views #'create-image-view images))
    (with-cleanups ((vert-shader-mod (create-shader-module *vert-shader*) #'destroy-shader-module)
                    (frag-shader-mod (create-shader-module *frag-shader*) #'destroy-shader-module)
                    (pipeline-layout (create-pipeline-layout) #'destroy-pipeline-layout)
                    (render-pass (with-foreign-object (attach-ref '(:struct vk:attachment-reference))
                                   (setf (mem-ref attach-ref '(:struct vk:attachment-reference))
                                         '(:attachment 0 :layout :color-attachment-optimal))
                                   (create-render-pass
                                    (vector (list :flags () :format *image-format* :samples :1-bit
                                                  :load-op :clear :store-op :store
                                                  :stencil-load-op :dont-care :stencil-store-op :dont-care
                                                  :initial-layout :undefined :final-layout 1000001002))
                                    (list :flags () :pipeline-bind-point :graphics
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
    (with-foreign-object (cmd-bufs 'vk:command-buffer (length framebuffers)))
    (with-cleanups (((allocate-command-buffers cmd-bufs (length framebuffers) cmd-pool)
                     (lambda (ign)
                       (declare (ignore ign))
                       (free-command-buffers cmd-pool cmd-bufs (length framebuffers)))))
      (do-foreign-array ((i cb) cmd-bufs vk:command-buffer (length framebuffers))
        (begin-command-buffer cb)
        (with-foreign-objects ((begin-info '(:struct vk:render-pass-begin-info))
                               (clear-values '(:union vk:clear-value) 1))
          (setf (foreign-slot-value (foreign-slot-pointer clear-values '(:union vk:clear-value) :color)
                                    '(:union vk:clear-color-value) :float32)
                #(1.0f0 1.0f0 1.0f0 1.0f0))
          (setf (mem-ref begin-info '(:struct vk:render-pass-begin-info))
                (list :s-type :render-pass-begin-info :next (null-pointer)
                      :render-pass render-pass :framebuffer (aref framebuffers i)
                      :render-area '(:offset (:x 0 :y 0) :extent (:width 600 :height 400))
                      :clear-value-count 1 :clear-values clear-values))
          (vk:cmd-begin-render-pass cb begin-info :inline))
        (vk:cmd-bind-pipeline cb :graphics graphics-pipeline)
        (vk:cmd-draw cb 3 1 0 0)
        (vk:cmd-end-render-pass cb)
        (vk:end-command-buffer cb)))
    (with-cleanups ((image-available-sem (create-semaphore) #'destroy-semaphore)
                    (render-finished-sem (create-semaphore) #'destroy-semaphore)
                    (fence (create-fence) #'destroy-fence)))
    (with-cleanups ((queue #'vk:queue-wait-idle)))
    (progn (map-window conn win)
           (unless (= (xcb-flush conn) 1)
             (error "xcb_flush")))
    (loop
      (let ((image-index (with-foreign-object (img :uint32)
                           (vk:acquire-next-image-khr (aref *device*) swapchain (1- (ash 1 64))
                                                      image-available-sem 0 img)
                           (mem-ref img :uint32))))
        (with-foreign-objects ((submit-info '(:struct vk:submit-info) 1)
                               (wait-semaphores 'vk:semaphore 1)
                               (wait-masks 'vk:pipeline-stage-flags 1)
                               (signal-semaphores 'vk:semaphore 1))
          (setf (mem-aref wait-semaphores 'vk:semaphore 0) image-available-sem)
          (setf (mem-aref wait-masks 'vk:pipeline-stage-flags 0) :top-of-pipe-bit)
          (setf (mem-aref signal-semaphores 'vk:semaphore 0) render-finished-sem)
          (setf (mem-aref submit-info '(:struct vk:submit-info) 0)
                (list :s-type :submit-info :next (null-pointer)
                      :wait-semaphore-count 1 :wait-semaphores wait-semaphores :wait-dst-stage-mask wait-masks
                      :command-buffer-count 1 :command-buffers (mem-aptr cmd-bufs 'vk:command-buffer image-index)
                      :signal-semaphore-count 1 :signal-semaphores signal-semaphores))
          (vk:queue-submit queue 1 submit-info fence))
        (with-foreign-objects ((present-info '(:struct vk:present-info-khr))
                               (wait-semaphores 'vk:semaphore 1)
                               (swapchains 'vk:swapchain-khr 1)
                               (indices :uint32 1))
          (setf (mem-aref wait-semaphores 'vk:semaphore 0) render-finished-sem)
          (setf (mem-aref swapchains 'vk:swapchain-khr 0) swapchain)
          (setf (mem-aref indices :uint32 0) image-index)
          (setf (mem-ref present-info '(:struct vk:present-info-khr))
                (list :s-type 1000001001 :next (null-pointer)
                      :wait-semaphore-count 1 :wait-semaphores wait-semaphores
                      :swapchain-count 1 :swapchains swapchains
                      :image-indices indices :results (null-pointer)))
          (vk:queue-present-khr queue present-info)))
      (unless (= (xcb-flush conn) 1)
        (error "xcb_flush"))
      (with-foreign-object (fences 'vk:fence 1)
        (setf (mem-aref fences 'vk:fence 0) fence)
        (vk:wait-for-fences (aref *device*) 1 fences nil (1- (ash 1 64)))
        (vk:reset-fences (aref *device*) 1 fences)))))
