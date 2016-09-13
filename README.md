A fairly simple wrapper around the MPD protocol


```
(WITH-MPD-CONNECTION (socket-symbol stream-symbol &optional (host "127.0.0.1") (port 6600)) &body body)
```

This macro wraps up a socket connection to the mpd server, binding the resulting socket and stream to the
appropriate symbols.

```
(SEND-COMMAND stream command &rest args &key)
```

This generic function wraps individual commands to be sent to the mpd server, using RENDER-COMMAND to
determine the textual representation of a command. Command is a keyword that determines the command to be set:
specialize these two methods with an eql-specializer on COMMAND to override the default handling.
In particular, commands that return song info use an eql-specializer on an `:AROUND` method to transform
the results from mpd into objects of the type MPD-REMOTE.SONG::SONG
