(require "record")

(define-record file-stat
  (%mode link-count owner group last-access last-modification last-info-change size))

(define file-stat
  (let ((%file-stat file-stat))
    (lambda (path)
      (apply make-file-stat (vector->list (%file-stat path))))))


(define (file-stat-type fs)
  (let ((t (bitwise-and (file-stat-%mode fs) file-stat:ifmt)))
    (cond
      ((= t file-stat:ifreg) 'regular)
      ((= t file-stat:ifdir) 'directory)
      ((= t file-stat:ifchr) 'character-device)
      ((= t file-stat:ifblk) 'block-device)
      ((= t file-stat:ififo) 'fifo)
      ((= t file-stat:iflnk) 'symbolic-link)
      ((= t file-stat:ifsock) 'socket)
      (else 'other))))

(define (file-stat-mode fs)
  (bitwise-and (file-stat-%mode fs) file-stat:isugo))

(define-macro (file-mode . perm-names)
  (let loop ((perm-names perm-names))
    (if (null? perm-names) 0
      (let
        ((perm
           (case (car perm-names)
             ((set-uid)     file-stat:isuid)
             ((set-gid)     file-stat:isgid)
             ((sticky)      file-stat:isvtx)
             ((owner-read)  file-stat:irusr)
             ((owner-write) file-stat:iwusr)
             ((owner-exec)  file-stat:ixusr)
             ((group-read)  file-stat:irgrp)
             ((group-write) file-stat:iwgrp)
             ((group-exec)  file-stat:ixgrp)
             ((other-read)  file-stat:iroth)
             ((other-write) file-stat:iwoth)
             ((other-exec)  file-stat:ixoth)
             ((owner)       file-stat:irwxu)
             ((group)       file-stat:irwxg)
             ((other)       file-stat:irwxo)
             ((read)        file-stat:irall)
             ((write)       file-stat:iwall)
             ((exec)        file-stat:ixall)
             ((all)         file-stat:irwxa)
             (else (error "bad permission name")))))
        (bitwise-ior perm (loop (cdr perm-names)))))))

(define file-stat:ifmt   #o170000)  ; type of file
(define file-stat:ififo  #o010000)  ; named pipe (fifo)
(define file-stat:ifchr  #o020000)  ; character special
(define file-stat:ifdir  #o040000)  ; directory
(define file-stat:ifblk  #o060000)  ; block special
(define file-stat:ifreg  #o100000)  ; regular
(define file-stat:iflnk  #o120000)  ; symbolic link
(define file-stat:ifsock #o140000)  ; socket
(define file-stat:ifwht  #o160000)  ; whiteout

(define file-stat:isugo  #o007777)

(define file-stat:isuid  #o004000)  ; set user id on execution
(define file-stat:isgid  #o002000)  ; set group id on execution
(define file-stat:isvtx  #o001000)  ; save swapped text even after use

(define file-stat:irwxa  #o000777)  ; RWX mask for owner+group+other

(define file-stat:irwxu  #o000700)  ; RWX mask for owner
(define file-stat:irusr  #o000400)  ; R for owner
(define file-stat:iwusr  #o000200)  ; W for owner
(define file-stat:ixusr  #o000100)  ; X for owner

(define file-stat:irwxg  #o000070)  ; RWX mask for group
(define file-stat:irgrp  #o000040)  ; R for group
(define file-stat:iwgrp  #o000020)  ; W for group
(define file-stat:ixgrp  #o000010)  ; X for group

(define file-stat:irwxo  #o000007)  ; RWX mask for other
(define file-stat:iroth  #o000004)  ; R for other
(define file-stat:iwoth  #o000002)  ; W for other
(define file-stat:ixoth  #o000001)  ; X for other

(define file-stat:irall  #o000444)
(define file-stat:iwall  #o000222)
(define file-stat:ixall  #o000111)

