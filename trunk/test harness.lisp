(DEFMACRO test (&REST Tests) 
    (LIST 'test-help (LIST 'QUOTE Tests)))

(DEFUN test-help (Tests)
    (COND ( (NULL Tests)   'completed)
                   (T (test-case (CAR Tests)) (test-help (CDR Tests)))))
          
(DEFUN test-case (Test)
   (READ-CHAR)
   (PRINT Test)
   (HANDLER-CASE 
      (qi::toplevel (LIST Test))
             (ERROR (condition) (PRINC condition))))  
