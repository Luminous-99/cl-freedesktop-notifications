(defpackage freedesktop-notifications
  (:use :cl :dbus)
  (:export
   #:notify
   #:close-notification
   #:get-server-information
   #:get-capabilities))

(in-package :freedesktop-notifications)

(macrolet ((method (&rest args)
             `(with-open-bus (bus (session-server-addresses))
                (with-introspected-object (notifications bus "/org/freedesktop/Notifications" "org.freedesktop.Notifications")
                  (notifications ,@args)))))
  (defun notify (summary body &key (app-name "") (app-icon "") (replaces-id 0) actions hints (expire-timeout -1))
    (method "org.freedesktop.Notifications" "Notify" app-name replaces-id app-icon summary body actions hints expire-timeout))

  (defun close-notification (id)
    (method "org.freedesktop.Notifications" "CloseNotification" id))

  (defun get-server-information ()
    "Returns the name of the server, the vendor name, the server's version number and the specification version"
    (method "org.freedesktop.Notifications" "GetServerInformation"))

  (defun get-capabilities ()
    (method "org.freedesktop.Notifications" "GetCapabilities")))
