This is a simple presentation mode for Emacs. It works best in
Emacs >= 24, which has a nice font rendering engine.

To use, invoke `epresent-run' in an `org-mode' buffer. This will
make a full-screen frame. Use n/p to navigate, or q to quit. Read
below for more key bindings. Each top-level headline becomes a
frame in the presentation (configure `EPRESENT_FRAME_LEVEL' to
change this default). Org-mode markup is used to nicely display the
buffer's contents.
