open Common 

type rgb = (float * float * float)

let rainbow_array = [|
    0.0, 0.0, 0.0;

    1.0, 0.0, 0.0;
    0.0, 1.0, 0.0;
    0.0, 0.0, 1.0;

    1.0, 1.0, 0.0;
    1.0, 0.0, 1.0;
    0.0, 1.0, 1.0;


    0.5, 0.0, 0.0;
    0.0, 0.5, 0.0;
    0.0, 0.0, 0.5;

    0.5, 0.5, 0.0;
    0.5, 0.0, 0.5;
    0.0, 0.5, 0.5;


    0.2, 0.0, 0.0;
    0.0, 0.2, 0.0;
    0.0, 0.0, 0.2;

    0.7, 0.0, 0.0;
    0.0, 0.7, 0.0;
    0.0, 0.0, 0.7;

    0.2, 0.2, 0.0;
    0.2, 0.0, 0.2;
    0.0, 0.2, 0.2;

    0.7, 0.7, 0.0;
    0.7, 0.0, 0.7;
    0.0, 0.7, 0.7;


  |]

let rainbow_color i = 
  rainbow_array.(i)


(*
 emacs:
 
 (with-output-to-temp-buffer "res"
   (mapcar 
    '(lambda (c)
       (let* ((rgb (color-values c))
              (rgb2 (mapcar '(lambda (code) (/ code 65535.0)) rgb))
              )
         (insert 
          (format "\"%s\", %S" c rgb2))
         (format "%s: %S" c rgb2)
         )
       )
    (defined-colors))
   )
*)

let rgb_of_string s = 
  raise Todo

