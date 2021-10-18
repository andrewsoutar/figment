(uiop:define-package #:com.andrewsoutar.figment/x11
  (:use #:cl #:cffi #:com.andrewsoutar.just-enough-x11)
  (:use #:com.andrewsoutar.figment/utils))
(cl:in-package #:com.andrewsoutar.figment/x11)

(define-from-xml "xproto"
  map-window)

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


(defun xcb-create-window (connection depth wid parent x y width height border-width class visual
                          &key background-pixmap background-pixel border-pixmap border-pixel
                            bit-gravity win-gravity backing-store backing-planes backing-pixel
                            (override-redirect nil override-redirect-p) (save-under nil save-under-p)
                            (event-mask nil event-mask-p) (do-not-propagate-mask nil do-not-propagate-mask-p)
                            colormap cursor)
  (declare (type foreign-pointer connection)
           (type (or (member :copy-from-parent) (unsigned-byte 8)) depth)
           (type (unsigned-byte 29) wid)
           (type (unsigned-byte 29) parent)
           (type (signed-byte 16) x y)
           (type (unsigned-byte 16) width height border-width)
           (type (member :copy-from-parent :input-output :input-only) class)
           (type (unsigned-byte 29) visual)
           (type (or null (unsigned-byte 29)) background-pixmap)
           (type (or null (unsigned-byte 32)) background-pixel)
           (type (or null (unsigned-byte 29)) border-pixmap)
           (type (or null (unsigned-byte 32)) border-pixel)
           (type (or null (member :forget :north-west :north :north-east :west :center
                                  :east :south-west :south :south-east :static))
                 bit-gravity)
           (type (or null (member :unmap :north-west :north :north-east :west :center
                                  :east :south-west :south :south-east :static))
                 win-gravity)
           (type (or null (member :not-useful :when-mapped :always)) backing-store)
           (type (or null (unsigned-byte 32)) backing-planes)
           (type (or null (unsigned-byte 32)) backing-pixel)
           (type list event-mask)
           (type list do-not-propagate-mask)
           (type (or null (unsigned-byte 32)) colormap)
           (type (or null (unsigned-byte 32)) cursor))
  ;; SBCL can't stack-allocate variable-length aliens, so if we used
  ;; with-foreign-* it would fall back to a C-heap-allocation. Since
  ;; this array is going to be garbage immediately, it's probably more
  ;; efficient to do a (cheap) lisp-allocation and let the
  ;; generational GC do its job.
  (macrolet
      ((do-keywords (head mac)
         (let ((keywords
                 '(background-pixmap
                   background-pixel
                   border-pixmap
                   border-pixel
                   (bit-gravity (position bit-gravity #(:forget :north-west :north :north-east :west :center
                                                        :east :south-west :south :south-east :static)))
                   (win-gravity (position win-gravity #(:unmap :north-west :north :north-east :west :center
                                                        :east :south-west :south :south-east :static)))
                   (backing-store (position backing-store #(:not-useful :when-mapped :always)))
                   backing-planes
                   backing-pixel
                   (override-redirect-p (if override-redirect 1 0))
                   (save-under-p (if save-under 1 0))
                   (event-mask-p
                    (reduce #'logior event-mask :key
                     (lambda (event)
                       (ash 1 (or (position event #(:key-press :key-release :button-press :button-release
                                                    :enter-window :leave-window :pointer-motion
                                                    :pointer-motion-hint :button-1-motion :button-2-motion
                                                    :button-3-motion :button-4-motion :button-5-motion
                                                    :button-motion :keymap-state :exposure
                                                    :visibility-change :structure-notify :resize-redirect
                                                    :substructure-notify :substructure-redirect :focus-change
                                                    :property-change :colormap-change :owner-grab-button))
                                  (error "event-mask: ~A" event))))))
                   (do-not-propagate-mask-p
                       (reduce #'logior do-not-propagate-mask :key
                        (lambda (event)
                          (ash 1 (or (position event #(:key-press :key-release :button-press :button-release
                                                       nil nil :pointer-motion nil :button-1-motion
                                                       :button-2-motion :button-3-motion :button-4-motion
                                                       :button-5-motion :button-motion))
                                     (error "do-not-propagate-mask: ~A" event))))))
                   colormap
                   cursor)))
           `(,head
             ,@(loop for bit from 0
                     for thing in keywords
                     collect (let* ((thing (if (consp thing) thing (list thing)))
                                    (signed (when (eql (first thing) :signed) (pop thing)))
                                    (condition (first thing))
                                    (value (or (second thing) condition)))
                               `(,mac ,condition ,bit ,value ,signed))))))
       (map-keywords (head fun)
         (let ((mac (gensym "MAC")) (stuff (gensym "STUFF")))
           `(macrolet ((,mac (&rest ,stuff) (apply ,fun ,stuff)))
              (do-keywords ,head ,mac)))))
    (let ((buffer (make-array (+ 32 (map-keywords + (lambda (condition &rest ign)
                                                      (declare (ignore ign))
                                                      `(if ,condition 4 0))))
                              :element-type '(unsigned-byte 8))))
      (sb-sys:with-pinned-objects (buffer)
        (let ((buffer-sap (sb-sys:vector-sap buffer)))
          (setf (sb-sys:sap-ref-8 buffer-sap 1) (if (eql depth :copy-from-parent) 0 depth)
                (sb-sys:sap-ref-32 buffer-sap 4) wid
                (sb-sys:sap-ref-32 buffer-sap 8) parent
                (sb-sys:signed-sap-ref-16 buffer-sap 12) x
                (sb-sys:signed-sap-ref-16 buffer-sap 14) y
                (sb-sys:sap-ref-16 buffer-sap 16) width
                (sb-sys:sap-ref-16 buffer-sap 18) height
                (sb-sys:sap-ref-16 buffer-sap 20) border-width
                (sb-sys:sap-ref-16 buffer-sap 22) (position class #(:copy-from-parent :input-output :input-only))
                (sb-sys:sap-ref-32 buffer-sap 24) visual)
          (let ((offset 32))
            (map-keywords progn (lambda (condition bit value signed)
                                  `(when ,condition
                                     (setf (ldb (byte ,bit 0) (sb-sys:sap-ref-32 buffer-sap 28)) 1
                                           (,(if signed 'sb-sys:signed-sap-ref-32 'sb-sys:sap-ref-32) buffer-sap
                                            (prog1 offset (incf offset 4)))
                                           ,value)))))
          (com.andrewsoutar.just-enough-x11/codegen::xcb-send connection 1 nil buffer-sap (length buffer) nil 0))))))

(defun main ()
  (with-cleanups ((conn (xcb-connect (null-pointer) (null-pointer)) #'xcb-disconnect))
    (let* ((setup (xcb-get-setup conn))
           (setup-len (* 4 (sb-sys:sap-ref-16 setup 6)))
           (vends-len-pad (* (ceiling (sb-sys:sap-ref-16 setup 24) 4) 4))
           (formats-len (* 8 (sb-sys:sap-ref-8 setup 29)))
           (setup-screen-offs (+ 40 vends-len-pad formats-len))
           (root (when (<= (+ setup-screen-offs 36) setup-len)
                   (sb-sys:sap-ref-32 setup (+ setup-screen-offs 0))))
           (visual (when (<= (+ setup-screen-offs 36) setup-len)
                     (sb-sys:sap-ref-32 setup (+ setup-screen-offs 32))))
           (win (xcb-generate-id conn)))
      (assert root)
      (xcb-create-window conn :copy-from-parent win root 0 0 600 400 10 :input-output visual)
      (map-window conn win)
      (unless (= (xcb-flush conn) 1)
        (error "xcb_flush"))
      (sleep 100))))
