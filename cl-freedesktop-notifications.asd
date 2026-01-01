(asdf:defsystem "cl-freedesktop-notifications"
  :author "luminous99"
  :description "A wrapper of the Freedesktop notifications specification."
  :license "MIT"
  :serial t
  :depends-on (:dbus)
  :components ((:file "notifications")))
