(MAKE-PACKAGE "qi")
(LOAD "qi.lisp")
(COMPILE-FILE "qi.lisp")
(DEFUN load-qi-object-code ()
   #+CLISP (LOAD "qi.fas")
   #+(OR ALLEGRO SBCL) (LOAD "qi.fasl")
   #+CMU   (LOAD "qi.x96f"))
(load-qi-object-code)
(USE-PACKAGE "qi")
(qi::save)
(FORMAT T "~%Installation finished; hit RETURN.")
(READ-CHAR)
(qi::quit)

