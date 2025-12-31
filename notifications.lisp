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
  (flet ((normalize (x)
           (let* ((value (cadr x))
                  (type (cond
                          ((or (eq value t) (eq value nil)) #\b)
                          ((subtypep (type-of value) 'integer) #\i)
                          ((subtypep (type-of value) 'string) #\s))))
             `(,(car x) ((,type) ,value)))))
    (defun notify (summary body &key (app-name "") (app-icon "") (replaces-id 0) actions hints (expire-timeout -1))
      "hints are a list of pairs of form (name value)"
      (let ((hints (mapcar #'normalize hints))
            (body (if (stringp body) body (prin1-to-string body)))
            (summary (if (stringp summary) summary (prin1-to-string summary))))
        (method "org.freedesktop.Notifications" "Notify" app-name replaces-id app-icon summary body actions hints expire-timeout))))

  (defun close-notification (id)
    (method "org.freedesktop.Notifications" "CloseNotification" id))

  (defun get-server-information ()
    "Returns the name of the server, the vendor name, the server's version number and the specification version"
    (method "org.freedesktop.Notifications" "GetServerInformation"))

  (defun get-capabilities ()
    (method "org.freedesktop.Notifications" "GetCapabilities")))
