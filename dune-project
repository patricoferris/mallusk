(lang dune 3.14)
(generate_opam_files true)


(package
  (name cshell)
  (depends 
    eio_posix
    ISO8601
    ezjsonm
    search))
