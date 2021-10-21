(uiop:define-package #:com.andrewsoutar.figment/x11
  (:use #:cl #:cffi #:com.andrewsoutar.just-enough-x11)
  (:use #:com.andrewsoutar.figment/utils))
(cl:in-package #:com.andrewsoutar.figment/x11)

(define-from-xml "xproto"
  create-window map-window)

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
      (create-window conn :copy-from-parent win root 0 0 600 400 10 :input-output visual)
      (map-window conn win)
      (unless (= (xcb-flush conn) 1)
        (error "xcb_flush"))
      (sleep 100))))
