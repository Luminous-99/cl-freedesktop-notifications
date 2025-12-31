# Examples

Send 10 notifications containing "Hello!".
```lisp
(use-package :freedesktop-notifications)
(dotimes (i 10)
 (notify i "Hello!"))
```

If the notification server supports it, send a notification with a sound effect and app icon.
```lisp
(notify "New Mail!" "..." :hints '(("sound-name" "message-new-email")) :app-icon "/path/to/icon")
```

