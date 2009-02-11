(begin
  (define p0 (open-input-file "/etc/passwd"))
  (define p1 (open-input-file "./wisp"))
  (open-input-file "file.h")
  (open-input-file "file.c")
  (open-input-file "wisp.h")
  (define p4 (open-input-file "main.c")))

(gc)
