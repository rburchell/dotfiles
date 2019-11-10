;;; notify.el --- notification front-end

;; Copyright (C) 2008  Mark A. Hershberger

;; Original Author: Mark A. Hershberger <mhersberger@intrahealth.org>
;; Modified by Andrey Kotlarski <m00naticus@gmail.com>
;; Modified by Andrew Gwozdziewycz <git@apgwoz.com>
;; Modified by Aidan Gauland <aidalgol@no8wireless.co.nz> October 2011
;; Modified by Olivier Sirven <the.slaa@gmail.com> November 2013
;; Keywords: extensions, convenience, lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use, just put (autoload 'notify "notify" "Notify TITLE, BODY.")
;; in your init file.
;;; Code:

(defvar rb/notify-defaults
  (list
   :id nil
   :app "Emacs"
   :icon "emacs"
   :timeout 0
   :urgency "low"
   :category "emacs.message")
  "Notification settings' defaults.
May be overridden with key-value additional arguments to `notify'.")

(defvar rb/notify-id 0 "Current D-Bus notification id.")
(defun rb/notify-allocate-id ()
  (setq rb/notify-id (+ rb/notify-id 1)))

(defun rb/notify-via-dbus (title body)
  "Send notification with TITLE, BODY over `D-Bus'."
  (let ((message-id
         ;; If there isn't a provided id to replace, then allocate one.
         (if (not (get 'rb/notify-defaults :id))
             (rb/notify-allocate-id)
           (get 'rb/notify-defaults :id))))
    (message (format "Sending notification ID %s with title %s body %s" message-id title body))
    (dbus-call-method :session "org.freedesktop.Notifications"
                      "/org/freedesktop/Notifications"
                      "org.freedesktop.Notifications"
                      "Notify"
                      (get 'rb/notify-defaults :app) ;; app_name
                      ':uint32 message-id ;; replaces_id
                      (get 'rb/notify-defaults :icon) ;; app_icon
                      title ;; summary
                      body ;; body
                      '(:array) ;; actions
                      '(:array :signature "{sv}") ;; hints
                      ':int32 (get 'rb/notify-defaults :timeout) ;; expire_timeout
                      )))

(defun rb/notify-dismiss (id)
  "Dismiss a notification with a given ID."
  (message (format "Dismissing notification ID %s" id))
  (dbus-call-method :session "org.freedesktop.Notifications"
                    "/org/freedesktop/Notifications"
                    "org.freedesktop.Notifications"
                    "CloseNotification"
                    ':uint32 id))

(defun rb/notify-keywords-to-properties (symbol args &optional defaults)
  "Add to SYMBOL's property list key-values from ARGS and DEFAULTS."
  (when (consp defaults)
    (rb/notify-keywords-to-properties symbol defaults))
  (while args
    (put symbol (car args) (cadr args))
    (setq args (cddr args))))

;;;###autoload
(defun rb/notify (title body &rest args)
  "Notify TITLE, BODY.'.
ARGS may be amongst :timeout, :icon, :urgency, :app and :category.
The return value is a unique identifier which may be used to alter the notification by using :id in ARGS."
  (rb/notify-keywords-to-properties 'rb/notify-defaults args rb/notify-defaults)
  (rb/notify-via-dbus title body))

(provide 'notify)
