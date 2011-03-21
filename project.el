(defun pad-ocaml-project-xxx ()
  (interactive)

  (setq 
   pad-ocaml-project-path "/home/pad/c-xxx"
   pad-ocaml-project-subdirs 
   (split-string 
    "commons globals extra 
     h_version-control h_statistics
     gui
    ")
   pad-ocaml-project-toplevel "xxx.top"
   )

  ; --------------------------------------------------------------------------
  ; xxx
  ; --------------------------------------------------------------------------
  (setq
   pad-ocaml-project-prog     "xxx"
   pad-ocaml-project-args 
   (join-string 
    (list 
     "-debugger"
     (case 0

       (0 "foo")

       )
     )
    )
   )

  ; --------------------------------------------------------------------------
  ; xxx2
  ; --------------------------------------------------------------------------

  (setq
   pad-ocaml-project-prog     "xxx2"
   ;pad-ocaml-project-prog "gui/test"
   pad-ocaml-project-args 
   (join-string 
    (list 
     "-debugger"
     (case 0
       (0 "")
       )
     ))
   )

  ; --------------------------------------------------------------------------
  ; for the help system, for C-c C-h to find where to look for
  (mapcar (lambda (p) 
            (ocaml-add-path (concat pad-ocaml-project-path "/" p))
            (ocaml-add-path "/usr/lib/ocaml/3.09.2/lablgtk2")
            (ocaml-add-path "/usr/lib/ocaml/3.09.2/lablgtksourceview")
            (ocaml-add-path "/home/pad/comments/ocamlgtk/src")
            (ocaml-add-path "/home/pad/packages/lib/ocaml/std-lib")
            (ocaml-add-path "/home/pad/packages/lib/ocaml/pkg-lib/lablgtk2")
            (ocaml-add-path "/home/pad/packages/lib/ocaml/pkg-lib/lablgtksourceview")
            )
          pad-ocaml-project-subdirs
          )
  )

  ;(setq ocaml-lib-path nil)
  ;(setq ocaml-module-alist 'lazy)
