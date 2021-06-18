This Falderal appliance tests the compiled-with-RPython version of the ZOWIE
reference interpreter.

This requires `fa-under-pty`, which is a part of Falderal since version 0.10.

It requires `fa-under-pty` because, for whatever reason, executables produced
by RPython from PyPy version 7.3.5 do not handle having their stdout redirected
very well.  Specifically, they dump core.  It is for this very reason that
`fa-under-pty` was written, in fact.

    -> Functionality "Interpret ZOWIE Program" is implemented by
    -> shell command
    -> "fa-under-pty ./bin/zowie-c %(test-body-file)"

