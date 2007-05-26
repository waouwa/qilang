"**********************************************************
 *       (C) Copyright Mark Tarver, 2000-2007, Lambda Associates		*
 *								*
 *		       	Qi 7.2 Lisp Source Code 			*
 *								*
 * Use and copying of this software and preparation of derivative works	*
 * based upon this software is permitted, as long as this copyright 	*
 * notice is left intact.						*
 * 								*
 * This software is made available under the GPL Licence AS IS, and the  	*
 * author makes no warranty about the software, its performance or its   	*
 * conformity to any specification.					*			
 * 								*
 * You may modify this program, but the copyright notice must remain.      *
 *                                                                                             *  
 * Further details about this program can be found in 			*
 * www.lambdassociates.org.					*
 **********************************************************	" 

"For lots of comments and wisdom, don't look here, look in the file Qi 7.2 in Qi.txt" 
"This is 95% machine generated code from Qi, Qi-Prolog and Qi-YACC"

(IN-PACKAGE "qi")

(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)

(SET-SYNTAX-FROM-CHAR #\; #\v)

(DEFUN waffle-off ()
   #+CLISP (PROGN (PROCLAIM '(OPTIMIZE (SPEED 3) (SAFETY 0)))
                                       (SETQ SYSTEM::*COMPILE-WARNINGS* NIL) 
                                       (SETQ *COMPILE-VERBOSE* NIL))
   #+CMU (PROGN (SETQ *COMPILE-PRINT* NIL) 
                                     (PROCLAIM '(OPTIMIZE (SPEED 3) (SAFETY 0) (EXTENSIONS::INHIBIT-WARNINGS 3)))
                                     (SETQ EXTENSIONS::*GC-VERBOSE* NIL))
   #-(OR CLISP CMU)  (ERROR "Unknown platform to Qi: ~A" (LISP-IMPLEMENTATION-TYPE)))

(waffle-off)

(DEFUN fix-closures ()
      #+CMU   (SETQ *closures* '(FUNCTION COMPILED-FUNCTION EVAL::INTERPRETED-FUNCTION))
      #+CLISP  (SETQ *closures* '(FUNCTION COMPILED-FUNCTION))
     #-(OR CLISP CMU) (ERROR "Qi does not recognise this platform ~A" (LISP-IMPLEMENTATION-TYPE)))

(fix-closures)

(DEFVAR *sysfuncs*
 '(<e> abstype and append apply assoc atp-credits atp-prompt back
   boolean? cd character? collect compile complex? concat congruent? cons? cons
   datatype debug define defcc defprolog delete-file destroy difference display-mode do dump
   dump-proof element? empty? error eval explode fail-if fix float? from-goals
   fst fst-ass fst-conc system fst-goal gensym get-array get-lambda
   get-prop get-rule head if if-with-checking if-without-checking input input+ integer? inferences 
   length let lineread list load make-array make-string map maxinferences  mlet  
   newfuntype new-assoc-type not notes-in nth number?
   occurrences occurs-check or output prf print profile profile-results prooftool provable?
   put-array put-prop quit random read-char read-file-as-charlist read-file
   read-chars-as-stringlist rational? real? refine remove reverse rotate round save set snd
   solved? specialise spy sqrt step string? strong-warning structure subst swap symbol?
   synonyms tail tc theory theory-size thin thm-intro time time-proof to-goals
   track tuple? undebug unprf union unprofile untrack value version
   unspecialise variable? warn write-to-file y-or-n? qi_> qi_< qi_>= qi_<= qi_= + *
   / /. - qi_= == @c @p preclude include preclude-all-but include-all-but
   when* eval* eval!* when!* ask query-prolog =* is* is!* return* bind* fail* not*
   ==* =!* typecheck* call* bagof* answer*))

(DEFMACRO define (&REST X) 
  `(COMPILE (EVAL (compile '<qi_compile> (remove-escape (QUOTE ,X))))))

(DEFUN remove-escape (Code) (SUBST '*qi-failure-object* #\Escape Code))

(DEFVAR *qi-failure-object* #\Escape)

(DEFUN <qi_compile> (Stream)
 (cases
  (LET ((<name> (<name> Stream)))
   (IF (NOT (failure? <name>))
    (LET ((<signature> (<signature> <name>)))
     (IF (NOT (failure? <signature>))
      (LET ((<rules> (<rules> <signature>)))
       (IF (NOT (failure? <rules>))
        (LIST (FIRST <rules>)
         (do (check_variable_occurrences (SECOND <rules>))
          (if *tc*
           (LET ((Type (curry_type (SECOND <signature>))))
            (LET
             ((Typecheck
               (procedure_T* (SECOND <name>) (SECOND <rules>)
                (specialise_type Type Type) 1)))
             (if (== Typecheck 'false)
              (error "type error in ~A" (SECOND <name>))
              (record_type (SECOND <name>) Type))))
           'ok)
          (compile_to_machine_code (SECOND <name>) (SECOND <rules>))))
        (LIST NIL #\Escape)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<name> (<name> Stream)))
   (IF (NOT (failure? <name>))
    (LET ((<rules> (<rules> <name>)))
     (IF (NOT (failure? <rules>))
      (LIST (FIRST <rules>)
       (do (check_variable_occurrences (SECOND <rules>))
        (if *tc*
         (error "~A has not been given a type.~%" (SECOND <name>))
         (compile_to_machine_code (SECOND <name>) (SECOND <rules>)))))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<name> (<name> Stream)))
   (IF (NOT (failure? <name>))
    (LIST (FIRST <name>) (error "Syntax error in ~A.~%" (SECOND <name>)))
    (LIST NIL #\Escape)))))

(DEFUN procedure_T* (V5371 V5372 V5373 V5374)
 (COND ((NULL V5372) 'true)
  ((CONSP V5372)
   (THE SYMBOL
    (and (type_rule? V5371 (CAR V5372) V5373 V5374)
     (procedure_T* V5371 (CDR V5372) V5373 (THE NUMBER (+ 1 V5374))))))
  (T (implementation_error 'procedure_T*))))

(DEFUN specialise_type (V5379 V5380)
 (COND ((NULL V5379) V5380)
  ((AND (CONSP V5379) (wrapper (variable? (CAR V5379))))
   (specialise_type (CDR V5379)
    (subst (THE SYMBOL (gensym "type")) (CAR V5379) V5380)))
  ((AND (CONSP V5379) (CONSP (CAR V5379)))
   (specialise_type (APPEND (CAR V5379) (CDR V5379)) V5380))
  ((CONSP V5379) (specialise_type (CDR V5379) V5380))
  (T (implementation_error 'specialise_type))))

(DEFUN type_rule? (V5381 V5382 V5383 V5384)
 (COND
  ((AND (CONSP V5382) (CONSP (CDR V5382)) (CONSP (CDR (CDR V5382)))
    (NULL (CDR (CDR (CDR V5382)))))
   (LET* ((V5385 (CAR V5382)) (V5386 (CDR V5382)))
    (THE SYMBOL
     (and (integrity? V5381 V5385 V5383 1 V5384)
      (correct? V5381 V5385 (CAR V5386) (CAR (CDR V5386)) V5383 V5384)))))
  (T (implementation_error 'type_rule?))))

(DEFUN integrity? (V5401 V5402 V5403 V5404 V5405)
 (COND ((NULL V5402) 'true)
  ((AND (CONSP V5402) (CONSP V5403) (CONSP (CDR V5403))
    (EQ '--> (CAR (CDR V5403))) (CONSP (CDR (CDR V5403)))
    (NULL (CDR (CDR (CDR V5403)))) (wrapper (variable? (CAR V5402))))
   (integrity? V5401 (CDR V5402) (CAR (CDR (CDR V5403)))
    (THE NUMBER (+ V5404 1)) V5405))
  ((AND (CONSP V5402) (CONSP V5403) (CONSP (CDR V5403))
    (EQ '--> (CAR (CDR V5403))) (CONSP (CDR (CDR V5403)))
    (NULL (CDR (CDR (CDR V5403)))))
   (LET* ((V5406 (CAR V5402)))
    (IF
     (EQ 'true
      (qi_=
       (typechecks? (build_patt_env (extract_vars V5406)) (stvars V5406)
        (CAR V5403))
       'false))
     (raise_type_failure V5401 V5404 V5405)
     (integrity? V5401 (CDR V5402) (CAR (CDR (CDR V5403)))
      (THE NUMBER (+ V5404 1)) V5405))))
  (T (raise_type_failure V5401 'arity V5405))))

(DEFUN build_patt_env (V5407)
 (COND ((NULL V5407) NIL)
  ((CONSP V5407)
   (CONS
    (CONS (THE SYMBOL (concat '&& (CAR V5407)))
     (CONS '$$ (CONS (THE SYMBOL (gensym "A")) NIL)))
    (build_patt_env (CDR V5407))))
  (T (implementation_error 'build_patt_env))))

(DEFUN raise_type_failure (V5408 V5409 V5410)
 (COND
  ((wrapper (number? V5409))
   (error "Integrity Check Failure; pattern ~A of rule ~A of ~A~%" V5409 V5410
    V5408))
  ((EQ 'arity V5409)
   (error
    "Integrity Check Failure; patterns and types in rule ~A of ~A~% do not correspond.~%"
    V5410 V5408))
  ((EQ 'guard V5409)
   (error "Match/guard failure in rule ~A of ~A.~%" V5410 V5408))
  ((EQ 'result V5409)
   (ERROR "Correctness Check Failure; rule ~A of ~A~%" V5410 V5408))
  (T (implementation_error 'raise_type_failure))))

(DEFUN stvars (V5411)
 (COND ((wrapper (variable? V5411)) (THE SYMBOL (concat '&& V5411)))
  ((CONSP V5411) (MAPCAR 'stvars V5411)) (T V5411)))

(DEFUN correct? (V5412 V5413 V5414 V5415 V5416 V5417)
 (LET ((Env (build_action_env (stvars V5413) V5416 NIL)))
  (LET
   ((Assumptions
     (stvars
      (IF (EQ 'true (occurs? V5412 V5415))
       (CONS (CONS V5412 (CONS '$$ (CONS V5416 NIL))) (fst Env)) (fst Env)))))
   (LET ((Atype (snd Env)))
    (execute_correctness_proof V5412 Assumptions
     (stvars_action V5413 (pback V5414 V5415)) Atype V5417)))))

(DEFUN stvars_action (V5422 V5423)
 (COND
  ((wrapper (and (variable? V5423) (occurs? V5423 V5422)))
   (THE SYMBOL (concat '&& V5423)))
  ((AND (CONSP V5423) (EQ 'let (CAR V5423)) (CONSP (CDR V5423))
    (CONSP (CDR (CDR V5423))) (CONSP (CDR (CDR (CDR V5423))))
    (NULL (CDR (CDR (CDR (CDR V5423))))))
   (LET* ((V5424 (CDR V5423)) (V5425 (CAR V5424)) (V5426 (CDR V5424)))
    (CONS 'let
     (CONS V5425
      (CONS (stvars_action V5422 (CAR V5426))
       (CONS
        (subst V5425 (THE SYMBOL (concat '&& V5425))
         (stvars_action V5422 (CAR (CDR V5426))))
        NIL))))))
  ((AND (CONSP V5423) (EQ '/. (CAR V5423)) (CONSP (CDR V5423))
    (CONSP (CDR (CDR V5423))) (NULL (CDR (CDR (CDR V5423)))))
   (LET* ((V5427 (CDR V5423)) (V5428 (CAR V5427)))
    (CONS '/.
     (CONS V5428
      (CONS
       (subst V5428 (THE SYMBOL (concat '&& V5428))
        (stvars_action V5422 (CAR (CDR V5427))))
       NIL)))))
  ((CONSP V5423) (MAPCAR #'(LAMBDA (Z) (stvars_action V5422 Z)) V5423))
  (T V5423)))

(DEFUN pback (V5429 V5430)
 (COND ((EQ V5429 '->) V5430)
  ((AND (CONSP V5430) (EQ 'where (CAR V5430)) (CONSP (CDR V5430))
    (CONSP (CDR (CDR V5430))) (CONSP (CAR (CDR (CDR V5430))))
    (EQ 'fail-if (CAR (CAR (CDR (CDR V5430)))))
    (CONSP (CDR (CAR (CDR (CDR V5430)))))
    (CONSP (CDR (CDR (CAR (CDR (CDR V5430))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CDR V5430)))))))
    (NULL (CDR (CDR (CDR V5430)))))
   (LET*
    ((V5431 (CDR V5430)) (V5432 (CDR V5431)) (V5433 (CAR V5432))
     (V5434 (CDR V5433)))
    (CONS 'where
     (CONS
      (CONS 'and (CONS (CAR V5431) (CONS (CONS 'not (CONS V5434 NIL)) NIL)))
      (CDR V5434)))))
  ((AND (CONSP V5430) (EQ 'where (CAR V5430)) (CONSP (CDR V5430))
    (CONSP (CDR (CDR V5430))) (NULL (CDR (CDR (CDR V5430)))))
   (LET* ((V5435 (CDR V5430)) (V5436 (CDR V5435)))
    (CONS 'where
     (CONS
      (CONS 'and
       (CONS (CAR V5435)
        (CONS
         (CONS 'not
          (CONS (CONS 'qi_= (CONS (CAR V5436) (CONS #\Escape NIL))) NIL))
         NIL)))
      V5436))))
  ((AND (CONSP V5430) (EQ 'fail-if (CAR V5430)) (CONSP (CDR V5430))
    (CONSP (CDR (CDR V5430))) (NULL (CDR (CDR (CDR V5430)))))
   (LET* ((V5437 (CDR V5430)))
    (CONS 'where (CONS (CONS 'not (CONS V5437 NIL)) (CDR V5437)))))
  (T
   (CONS 'where
    (CONS (CONS 'not (CONS (CONS 'qi_= (CONS #\Escape (CONS V5430 NIL))) NIL))
     (CONS V5430 NIL))))))

(DEFUN execute_correctness_proof (V5438 V5439 V5440 V5441 V5442)
 (COND
  ((AND (CONSP V5440) (EQ 'where (CAR V5440)) (CONSP (CDR V5440))
    (CONSP (CDR (CDR V5440))) (NULL (CDR (CDR (CDR V5440)))))
   (LET* ((V5443 (CDR V5440)) (V5444 (CAR V5443)))
    (IF (EQ 'true (qi_= (typechecks? V5439 V5444 'boolean) 'false))
     (raise_type_failure V5438 'guard V5442)
     (IF
      (EQ 'true
       (qi_=
        (typechecks? (APPEND (verification_conditions V5444) V5439)
         (CAR (CDR V5443)) V5441)
        'false))
      (raise_type_failure V5438 'result V5442) 'true))))
  (T
   (IF (EQ 'true (qi_= (typechecks? V5439 V5440 V5441) 'false))
    (raise_type_failure V5438 'result V5442) 'true))))

(DEFUN verification_conditions (V5445)
 (COND
  ((AND (CONSP V5445) (EQ 'and (CAR V5445)) (CONSP (CDR V5445))
    (CONSP (CDR (CDR V5445))) (NULL (CDR (CDR (CDR V5445)))))
   (LET* ((V5446 (CDR V5445)))
    (APPEND (verification_conditions (CAR V5446))
     (verification_conditions (CAR (CDR V5446))))))
  (T (CONS (CONS (cons->@c (curry V5445)) (CONS '$$ (CONS 'verified NIL))) NIL))))

(DEFUN build_action_env (V21647 V21648 V21649)
 (COND ((NULL V21647) (@p V21649 V21648))
  ((AND (CONSP V21647) (CONSP V21648) (CONSP (CDR V21648))
    (EQ '--> (CAR (CDR V21648))) (CONSP (CDR (CDR V21648)))
    (NULL (CDR (CDR (CDR V21648)))))
   (build_action_env (CDR V21647) (CAR (CDR (CDR V21648)))
    (CONS (CONS (cons->@c (CAR V21647)) (CONS '$$ (CONS (CAR V21648) NIL)))
     V21649)))
  (T (implementation_error 'build_action_env))))

(DEFUN curry_type (V5450)
 (COND
  ((AND (CONSP V5450) (CONSP (CDR V5450)) (EQ '--> (CAR (CDR V5450)))
    (CONSP (CDR (CDR V5450))) (CONSP (CDR (CDR (CDR V5450))))
    (EQ '--> (CAR (CDR (CDR (CDR V5450))))))
   (curry_type (CONS (CAR V5450) (CONS '--> (CONS (CDR (CDR V5450)) NIL)))))
  ((AND (CONSP V5450) (EQ 'cons (CAR V5450)) (CONSP (CDR V5450))
    (CONSP (CDR (CDR V5450))) (NULL (CDR (CDR (CDR V5450)))))
   (LET* ((V5451 (CDR V5450)))
    (CONS 'list
     (CONS (curry_type (CAR V5451)) (curry_type (CAR (CDR V5451)))))))
  ((CONSP V5450) (MAPCAR 'curry_type V5450)) (T V5450)))

(DEFUN curry_type (V8)
 (COND
  ((AND (CONSP V8) (CONSP (CDR V8)) (EQ '* (CAR (CDR V8)))
    (CONSP (CDR (CDR V8))) (CONSP (CDR (CDR (CDR V8))))
    (EQ '* (CAR (CDR (CDR (CDR V8))))))
   (curry_type (CONS (CAR V8) (CONS '* (CONS (CDR (CDR V8)) NIL)))))
  ((AND (CONSP V8) (CONSP (CDR V8)) (EQ '--> (CAR (CDR V8)))
    (CONSP (CDR (CDR V8))) (CONSP (CDR (CDR (CDR V8))))
    (EQ '--> (CAR (CDR (CDR (CDR V8))))))
   (curry_type (CONS (CAR V8) (CONS '--> (CONS (CDR (CDR V8)) NIL)))))
  ((AND (CONSP V8) (EQ 'cons (CAR V8)) (CONSP (CDR V8)) (CONSP (CDR (CDR V8)))
    (NULL (CDR (CDR (CDR V8)))))
   (LET* ((V9 (CDR V8)))
    (CONS 'list (CONS (curry_type (CAR V9)) (curry_type (CAR (CDR V9)))))))
  ((CONSP V8) (MAPCAR 'curry_type V8)) (T V8)))

(DEFUN <name> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
    (if
     (and (symbol? (CAAR Stream))
      (and (not (sysfunc? (CAAR Stream))) (not (variable? (CAAR Stream)))))
     (set '*currfunc* (CAAR Stream))
     (error '"~A is not a legimate functor." (CAAR Stream))))
   (LIST NIL #\Escape))))

(DEFUN sysfunc? (V5452)
 (IF (EQ 'true (THE SYMBOL (element? V5452 *sysfuncs*)))
  (error "~A is a system function and may not be redefined." V5452) 'false))

(DEFUN <signature> (Stream)
 (cases
  (IF (AND (CONSP (FIRST Stream)) (EQ (FIRST (FIRST Stream)) '{))
   (LET
    ((<signature-help>
      (<signature-help> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
    (IF (NOT (failure? <signature-help>))
     (IF
      (AND (CONSP (FIRST <signature-help>))
       (EQ (FIRST (FIRST <signature-help>)) '}))
      (LIST
       (FIRST (LIST (REST (FIRST <signature-help>)) (SECOND <signature-help>)))
       (SECOND <signature-help>))
      (LIST NIL #\Escape))
     (LIST NIL #\Escape)))
   (LIST NIL #\Escape))))

(DEFUN <signature-help> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LET
    ((<signature-help>
      (<signature-help> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
    (IF (NOT (failure? <signature-help>))
     (LIST (FIRST <signature-help>)
      (if (element? (CAAR Stream) (cons '{ (cons '} NIL))) #\Escape
       (cons (CAAR Stream) (SECOND <signature-help>))))
     (LIST NIL #\Escape)))
   (LIST NIL #\Escape))
  (LET ((<e> (<e> Stream)))
   (IF (NOT (failure? <e>)) (LIST (FIRST <e>) NIL) (LIST NIL #\Escape)))))

(DEFUN <rules> (Stream)
 (cases
  (LET ((<rule> (<rule> Stream)))
   (IF (NOT (failure? <rule>))
    (LET ((<rules> (<rules> <rule>)))
     (IF (NOT (failure? <rules>))
      (LIST (FIRST <rules>)
       (CONS (SECOND <rule>) (SECOND <rules>)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<rule> (<rule> Stream)))
   (IF (NOT (failure? <rule>))
    (LIST (FIRST <rule>) (CONS (SECOND <rule>) NIL))
    (LIST NIL #\Escape)))))

(DEFUN <rule> (Stream)
 (cases
  (LET ((<patterns> (<patterns> Stream)))
   (IF (NOT (failure? <patterns>))
    (LET ((<arrow> (<arrow> <patterns>)))
     (IF (NOT (failure? <arrow>))
      (LET ((<action> (<action> <arrow>)))
       (IF (NOT (failure? <action>))
        (IF (AND (CONSP (FIRST <action>)) (EQ (FIRST (FIRST <action>)) 'where))
         (LET
          ((<guard>
            (<guard> (LIST (REST (FIRST <action>)) (SECOND <action>)))))
          (IF (NOT (failure? <guard>))
           (LIST (FIRST <guard>)
            (left_linearise
             (CONS (SECOND <patterns>)
              (CONS (SECOND <arrow>)
               (CONS
                (CONS 'where
                 (CONS (SECOND <guard>) (CONS (SECOND <action>) NIL)))
                NIL)))))
           (LIST NIL #\Escape)))
         (LIST NIL #\Escape))
        (LIST NIL #\Escape)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<patterns> (<patterns> Stream)))
   (IF (NOT (failure? <patterns>))
    (LET ((<arrow> (<arrow> <patterns>)))
     (IF (NOT (failure? <arrow>))
      (LET ((<action> (<action> <arrow>)))
       (IF (NOT (failure? <action>))
        (LIST (FIRST <action>)
         (left_linearise
          (CONS (SECOND <patterns>)
           (CONS (SECOND <arrow>) (CONS (SECOND <action>) NIL)))))
        (LIST NIL #\Escape)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<arrow> (<arrow> Stream)))
   (IF (NOT (failure? <arrow>))
    (LET ((<action> (<action> <arrow>)))
     (IF (NOT (failure? <action>))
      (IF (AND (CONSP (FIRST <action>)) (EQ (FIRST (FIRST <action>)) 'where))
       (LET
        ((<guard> (<guard> (LIST (REST (FIRST <action>)) (SECOND <action>)))))
        (IF (NOT (failure? <guard>))
         (LIST (FIRST <guard>)
          (CONS NIL
           (CONS (SECOND <arrow>)
            (CONS
             (CONS 'where (CONS (SECOND <guard>) (CONS (SECOND <action>) NIL)))
             NIL))))
         (LIST NIL #\Escape)))
       (LIST NIL #\Escape))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<arrow> (<arrow> Stream)))
   (IF (NOT (failure? <arrow>))
    (LET ((<action> (<action> <arrow>)))
     (IF (NOT (failure? <action>))
      (LIST (FIRST <action>)
       (CONS NIL (CONS (SECOND <arrow>) (CONS (SECOND <action>) NIL))))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))))

(DEFUN <arrow> (Stream)
 (cases
  (IF (AND (CONSP (FIRST Stream)) (EQ (FIRST (FIRST Stream)) '->))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) '->)
   (LIST NIL #\Escape))
  (IF (AND (CONSP (FIRST Stream)) (EQ (FIRST (FIRST Stream)) '<-))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) '<-)
   (LIST NIL #\Escape))))

(DEFUN <patterns> (Stream)
 (cases
  (LET ((<pattern> (<pattern> Stream)))
   (IF (NOT (failure? <pattern>))
    (LET ((<patterns> (<patterns> <pattern>)))
     (IF (NOT (failure? <patterns>))
      (LIST (FIRST <patterns>) (CONS (SECOND <pattern>) (SECOND <patterns>)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<e> (<e> Stream)))
   (IF (NOT (failure? <e>)) (LIST (FIRST <e>) NIL) (LIST NIL #\Escape)))))

(DEFUN <pattern> (Stream)
 (cases
  (IF (AND (CONSP (FIRST Stream)) (CONSP (CAAR Stream)))
   (LET
    ((HeadStream167 (LIST (CAAR Stream) (CADR Stream)))
     (TailStream168 (LIST (CDAR Stream) (CADR Stream))))
    (IF
     (AND (CONSP (FIRST HeadStream167))
      (EQUAL (FIRST (FIRST HeadStream167)) '@p))
     (LET
      ((<pattern1>
        (<pattern1>
         (LIST (REST (FIRST HeadStream167)) (SECOND HeadStream167)))))
      (IF (NOT (failure? <pattern1>))
       (LET ((<pattern2> (<pattern2> <pattern1>)))
        (IF (NOT (failure? <pattern2>))
         (IF (NULL (FIRST <pattern2>))
          (LIST (FIRST TailStream168)
           (LIST '@p (SECOND <pattern1>) (SECOND <pattern2>)))
          (LIST NIL #\Escape))
         (LIST NIL #\Escape)))
       (LIST NIL #\Escape)))
     (LIST NIL #\Escape)))
   (LIST NIL #\Escape))
  (IF (AND (CONSP (FIRST Stream)) (CONSP (CAAR Stream)))
   (LET
    ((HeadStream169 (LIST (CAAR Stream) (CADR Stream)))
     (TailStream170 (LIST (CDAR Stream) (CADR Stream))))
    (IF
     (AND (CONSP (FIRST HeadStream169))
      (EQUAL (FIRST (FIRST HeadStream169)) 'cons))
     (LET
      ((<pattern1>
        (<pattern1>
         (LIST (REST (FIRST HeadStream169)) (SECOND HeadStream169)))))
      (IF (NOT (failure? <pattern1>))
       (LET ((<pattern2> (<pattern2> <pattern1>)))
        (IF (NOT (failure? <pattern2>))
         (IF (NULL (FIRST <pattern2>))
          (LIST (FIRST TailStream170)
           (LIST 'cons (SECOND <pattern1>) (SECOND <pattern2>)))
          (LIST NIL #\Escape))
         (LIST NIL #\Escape)))
       (LIST NIL #\Escape)))
     (LIST NIL #\Escape)))
   (LIST NIL #\Escape))
  (IF (AND (CONSP (FIRST Stream)) (CONSP (CAAR Stream)))
   (LET
    ((HeadStream171 (LIST (CAAR Stream) (CADR Stream)))
     (TailStream172 (LIST (CDAR Stream) (CADR Stream))))
    (IF
     (AND (CONSP (FIRST HeadStream171))
      (EQUAL (FIRST (FIRST HeadStream171)) '@c))
     (LET
      ((<pattern1>
        (<pattern1>
         (LIST (REST (FIRST HeadStream171)) (SECOND HeadStream171)))))
      (IF (NOT (failure? <pattern1>))
       (LET ((<pattern2> (<pattern2> <pattern1>)))
        (IF (NOT (failure? <pattern2>))
         (IF (NULL (FIRST <pattern2>))
          (LIST (FIRST TailStream172)
           (LIST 'cons (SECOND <pattern1>) (SECOND <pattern2>)))
          (LIST NIL #\Escape))
         (LIST NIL #\Escape)))
       (LIST NIL #\Escape)))
     (LIST NIL #\Escape)))
   (LIST NIL #\Escape))
  (IF (AND (CONSP (FIRST Stream)) (CONSP (CAAR Stream)))
   (LET
    ((HeadStream173 (LIST (CAAR Stream) (CADR Stream)))
     (TailStream174 (LIST (CDAR Stream) (CADR Stream))))
    (IF
     (AND (CONSP (FIRST HeadStream173))
      (EQUAL (FIRST (FIRST HeadStream173)) 'list))
     (LET
      ((<patterns>
        (<patterns>
         (LIST (REST (FIRST HeadStream173)) (SECOND HeadStream173)))))
      (IF (NOT (failure? <patterns>))
       (IF (NULL (FIRST <patterns>))
        (LIST (FIRST TailStream174)
         (cons_form (cons 'listit (SECOND <patterns>))))
        (LIST NIL #\Escape))
       (LIST NIL #\Escape)))
     (LIST NIL #\Escape)))
   (LIST NIL #\Escape))
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
    (if (cons? (CAAR Stream))
     (ERROR "~A is not a legitimate constructor~%" (CAAR Stream)) #\Escape))
   (LIST NIL #\Escape))
  (LET ((<simple_pattern> (<simple_pattern> Stream)))
   (IF (NOT (failure? <simple_pattern>))
    (LIST (FIRST <simple_pattern>) (SECOND <simple_pattern>))
    (LIST NIL #\Escape)))))

(DEFUN <simple_pattern> (Stream)
 (cases
  (IF (AND (CONSP (FIRST Stream)) (EQ (FIRST (FIRST Stream)) '_))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (gensym '"X"))
   (LIST NIL #\Escape))
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
    (if (element? (CAAR Stream) (CONS '-> (CONS '<- NIL))) '#\Escape
     (CAAR Stream)))
   (LIST NIL #\Escape))))

(DEFUN <pattern1> (Stream)
 (cases
  (LET ((<pattern> (<pattern> Stream)))
   (IF (NOT (failure? <pattern>))
    (LIST (FIRST <pattern>) (SECOND <pattern>)) (LIST NIL #\Escape)))))

(DEFUN <pattern2> (Stream)
 (cases
  (LET ((<pattern> (<pattern> Stream)))
   (IF (NOT (failure? <pattern>))
    (LIST (FIRST <pattern>) (SECOND <pattern>)) (LIST NIL #\Escape)))))

(DEFUN <action> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (CAAR Stream))
   (LIST NIL #\Escape))))

(DEFUN <guard> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (CAAR Stream))
   (LIST NIL #\Escape))))

(DEFUN escape? (V5464) (COND ((EQUAL #\Escape V5464) 'true) (T 'false)))

(DEFUN left_linearise (V5465) (fix 'left_linearise* V5465))

(DEFUN fix (V5467 V5468)
 (fix* V5467 V5468 (apply V5467 V5468)))

(DEFUN fix* (V5476 V5477 V5478)
 (COND ((wrapper (qi_= V5477 V5478)) V5478)
  (T (fix* V5476 V5478 (apply V5476 V5478)))))

(DEFUN left_linearise* (V5479)
 (COND
  ((AND (CONSP V5479) (CONSP (CDR V5479)) (CONSP (CDR (CDR V5479)))
    (CONSP (CAR (CDR (CDR V5479)))) (EQ 'where (CAR (CAR (CDR (CDR V5479)))))
    (CONSP (CDR (CAR (CDR (CDR V5479)))))
    (CONSP (CDR (CDR (CAR (CDR (CDR V5479))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CDR V5479)))))))
    (NULL (CDR (CDR (CDR V5479)))))
   (LET*
    ((V5480 (CAR V5479)) (V5481 (CDR V5479)) (V5482 (CDR V5481))
     (V5483 (CAR V5482)) (V5484 (CDR V5483)))
    (LET ((V (rpted_v V5480)))
     (IF (EQ 'true (qi_= V 'false)) V5479
      (LET ((X (gensym "X")))
       (CONS (left_linearise_v V X V5480)
        (CONS (CAR V5481)
         (CONS
          (CONS 'where
           (CONS
            (CONS 'and
             (CONS (CONS 'qi_= (CONS X (CONS V NIL))) (CONS (CAR V5484) NIL)))
            (CDR V5484)))
          NIL))))))))
  ((AND (CONSP V5479) (CONSP (CDR V5479)) (CONSP (CDR (CDR V5479)))
    (NULL (CDR (CDR (CDR V5479)))))
   (LET* ((V5485 (CAR V5479)) (V5486 (CDR V5479)))
    (LET ((V (rpted_v V5485)))
     (IF (EQ 'true (qi_= V 'false)) V5479
      (LET ((X (gensym "X")))
       (CONS (left_linearise_v V X V5485)
        (CONS (CAR V5486)
         (CONS
          (CONS 'where (CONS (CONS 'qi_= (CONS X (CONS V NIL))) (CDR V5486)))
          NIL))))))))
  (T (implementation_error 'left_linearise*))))

(DEFUN rpted_v (V5487) (rpted_v* (flatten V5487)))

(DEFUN rpted_v* (V5493)
 (COND ((NULL V5493) 'false)
  ((AND (CONSP V5493)
    (wrapper
     (and (variable? (CAR V5493)) (element? (CAR V5493) (CDR V5493)))))
   (CAR V5493))
  ((CONSP V5493) (rpted_v* (CDR V5493))) (T (implementation_error 'rpted_v*))))

(DEFUN left_linearise_v (V5503 V5504 V5505)
 (COND ((wrapper (qi_= V5503 V5505)) V5504)
  ((CONSP V5505)
   (LET* ((V5506 (CAR V5505)) (V5507 (CDR V5505)))
    (LET ((LL (left_linearise_v V5503 V5504 V5506)))
     (IF (EQ 'true (qi_= LL V5506))
      (CONS LL (left_linearise_v V5503 V5504 V5507)) (CONS LL V5507)))))
  (T V5505)))

(SETQ *userdefs* NIL)

(DEFUN compile_to_machine_code (V5508 V5509)
 (PUSHNEW V5508 *userdefs*)
  (compile_to_lisp (compile_to_lambda (compile_to_lambda+ V5508 V5509))))

(DEFUN check_variable_occurrences (V5510)
 (COND ((NULL V5510) NIL)
  ((CONSP V5510)
   (check_variable_occurrence (CAR V5510))
    (check_variable_occurrences (CDR V5510)))
  (T (implementation_error 'check_variable_occurrences))))

(DEFUN check_variable_occurrence (V5515)
 (COND
  ((AND (CONSP V5515) (CONSP (CDR V5515)) (CONSP (CDR (CDR V5515)))
    (NULL (CDR (CDR (CDR V5515)))))
   (LET
    ((Free
      (difference (extract_free_vars (CAR (CDR (CDR V5515))))
       (extract_free_vars (CAR V5515)))))
    (IF (EQ 'true (THE SYMBOL (empty? Free))) 'ok
     (warn (FORMAT NIL "the following variables are free in ~A:~{ ~A;~}~%"
      *currfunc* Free)))))
  (T (implementation_error 'check_variable_occurrence))))

(DEFUN extract_free_vars (V5517) (extract_free_vars* NIL V5517 NIL))

(DEFUN extract_free_vars* (V5526 V5527 V5528)
 (COND
  ((wrapper (and (variable? V5527) (not (element? V5527 V5526))))
   (THE LIST (union (CONS V5527 NIL) V5528)))
  ((AND (CONSP V5527) (EQ 'let (CAR V5527)) (CONSP (CDR V5527))
    (CONSP (CDR (CDR V5527))) (CONSP (CDR (CDR (CDR V5527))))
    (NULL (CDR (CDR (CDR (CDR V5527))))))
   (LET* ((V5529 (CDR V5527)) (V5530 (CDR V5529)))
    (THE LIST
     (union (extract_free_vars* V5526 (CAR V5530) V5528)
      (extract_free_vars* (CONS (CAR V5529) V5526) (CAR (CDR V5530)) V5528)))))
  ((AND (CONSP V5527) (EQ '/. (CAR V5527)) (CONSP (CDR V5527))
    (CONSP (CDR (CDR V5527))) (NULL (CDR (CDR (CDR V5527)))))
   (LET* ((V5531 (CDR V5527)))
    (extract_free_vars* (CONS (CAR V5531) V5526) (CAR (CDR V5531)) V5528)))
  ((CONSP V5527)
   (THE LIST
    (union (extract_free_vars* V5526 (CAR V5527) V5528)
     (extract_free_vars* V5526 (CDR V5527) V5528))))
  (T V5528)))

(DEFUN union (x y) (UNION x y :TEST 'ABSEQUAL))

(DEFUN ABSEQUAL (X Y)
 (COND ((EQUAL X Y) 'true)
  ((AND (TUPLE-P X) (TUPLE-P Y))
   (AND (ABSEQUAL (fst X) (fst Y)) (ABSEQUAL (snd X) (snd Y))))
  (T NIL)))

(DEFSTRUCT
 (TUPLE
  (:PRINT-FUNCTION
   (LAMBDA (Struct Stream Depth) (DECLARE (IGNORE Depth))
    (print_tuple Struct Stream)))
  (:CONC-NAME NIL) (:CONSTRUCTOR @p (fst snd)))
 fst snd)

(DEFUN print_tuple (Tuple Stream)
 (FORMAT Stream "(@p ~S ~S)" (fst Tuple) (snd Tuple)))

(DEFUN tuple? (X) (IF (TUPLE-P X) 'true 'false))

(DEFUN compile_to_lambda+ (V5535 V5536)
 (LET ((NewRules (MAPCAR 'check_choicepoint V5536)))
  (LET ((Vs (REVERSE (mkVs (arity V5535 NewRules)))))
   (LET ((Body (CONS 'cases (compile_to_lambda+_rules Vs NewRules))))
    (put-prop V5535 'lambda+ 
     (CONS 'y-combinator
     (CONS (CONS '/. (CONS V5535 (CONS (bld_abs Vs Body) NIL))) NIL)))))))

(DEFUN get-lambda (Name) (get-prop Name 'lambda+ NIL))

(DEFUN check_choicepoint (V5537)
 (COND
  ((AND (CONSP V5537) (CONSP (CDR V5537)) (CONSP (CDR (CDR V5537)))
    (NULL (CDR (CDR (CDR V5537)))) (wrapper (qi_= (CAR (CDR V5537)) '->)))
   (CONS (CAR V5537) (CDR (CDR V5537))))
  ((AND (CONSP V5537) (CONSP (CDR V5537)) (CONSP (CDR (CDR V5537)))
    (CONSP (CAR (CDR (CDR V5537)))) (EQ 'where (CAR (CAR (CDR (CDR V5537)))))
    (CONSP (CDR (CAR (CDR (CDR V5537)))))
    (CONSP (CDR (CDR (CAR (CDR (CDR V5537))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CDR V5537)))))))
    (NULL (CDR (CDR (CDR V5537)))))
   (LET*
    ((V5538 (CDR V5537)) (V5539 (CDR V5538)) (V5540 (CAR V5539))
     (V5541 (CDR V5540)))
    (LET ((V (gensym "choicepoint")))
     (CONS (CAR V5537)
      (CONS
       (CONS 'where
        (CONS
         (CONS 'and
          (CONS (CAR V5541)
           (CONS
            (CONS 'not
             (CONS (CONS 'escape? (CONS (CONS 'set (CONS V (CDR V5541))) NIL))
              NIL))
            NIL)))
         (CONS (CONS 'value (CONS V NIL)) NIL)))
       NIL)))))
  ((AND (CONSP V5537) (CONSP (CDR V5537)) (CONSP (CDR (CDR V5537)))
    (NULL (CDR (CDR (CDR V5537)))))
   (LET ((V (gensym "choicepoint")))
    (CONS (CAR V5537)
     (CONS
      (CONS 'where
       (CONS
        (CONS 'not
         (CONS
          (CONS 'escape? (CONS (CONS 'set (CONS V (CDR (CDR V5537)))) NIL))
          NIL))
        (CONS (CONS 'value (CONS V NIL)) NIL)))
      NIL))))
  (T (implementation_error 'check_choicepoint))))

(DEFUN bld_abs (V5542 V5543)
 (COND ((NULL V5542) V5543)
  ((CONSP V5542)
   (bld_abs (CDR V5542) (CONS '/. (CONS (CAR V5542) (CONS V5543 NIL)))))
  (T (implementation_error 'bld_abs))))

(DEFUN mkVs (V5544)
 (COND ((EQ 0 V5544) NIL)
  (T (CONS (THE SYMBOL (gensym "V")) (mkVs (THE NUMBER (- V5544 1)))))))

(DEFUN arity (V5547 V5548)
 (COND ((AND (CONSP V5548) (NULL (CDR V5548))) (arity_rule (CAR V5548)))
  ((AND (CONSP V5548) (CONSP (CDR V5548)))
   (LET* ((V5549 (CDR V5548)))
    (LET ((Arity1 (arity_rule (CAR V5548))))
     (LET ((Arity2 (arity_rule (CAR V5549))))
      (IF (EQ 'true (qi_= Arity1 Arity2)) (arity V5547 V5549)
       (error "arity error in ~A~%" V5547))))))
  (T (implementation_error 'arity))))

(DEFUN arity_rule (V5550)
 (COND
  ((AND (CONSP V5550) (CONSP (CDR V5550)) (NULL (CDR (CDR V5550))))
   (THE NUMBER (length (CAR V5550))))
  (T (implementation_error 'arity_rule))))

(DEFUN compile_to_lambda+_rules (V5554 V5555)
 (COND ((NULL V5555) NIL)
  ((AND (CONSP V5555) (CONSP (CAR V5555)) (CONSP (CDR (CAR V5555)))
    (NULL (CDR (CDR (CAR V5555)))))
   (LET* ((V5556 (CAR V5555)))
    (CONS
     (compile_to_lambda+_rule (REVERSE V5554) (REVERSE (CAR V5556))
      (CAR (CDR V5556)))
     (compile_to_lambda+_rules V5554 (CDR V5555)))))
  (T (implementation_error 'compile_to_lambda+_rules))))

(DEFUN compile_to_lambda+_rule (V5557 V5558 V5559)
 (COND ((NULL V5558) (bld_app V5557 V5559))
  ((CONSP V5558)
   (compile_to_lambda+_rule V5557 (CDR V5558)
    (CONS '/. (CONS (CAR V5558) (CONS V5559 NIL)))))
  (T (implementation_error 'compile_to_lambda+_rule))))

(DEFUN bld_app (V5560 V5561)
 (COND ((NULL V5560) V5561)
  ((CONSP V5560) (bld_app (CDR V5560) (CONS V5561 (CONS (CAR V5560) NIL))))
  (T (implementation_error 'bld_app))))

(DEFUN compile_to_lambda (V5562)
 (COND
  ((AND (CONSP V5562) (EQ 'y-combinator (CAR V5562)) (CONSP (CDR V5562))
    (NULL (CDR (CDR V5562))))
   (CONS 'y-combinator (CONS (compile_combinator_body (CAR (CDR V5562))) NIL)))
  (T (implementation_error 'compile_to_lambda))))

(DEFUN compile_combinator_body (V5563)
 (COND
  ((AND (CONSP V5563) (EQ '/. (CAR V5563)) (CONSP (CDR V5563))
    (CONSP (CDR (CDR V5563))) (NULL (CDR (CDR (CDR V5563)))))
   (LET* ((V5564 (CDR V5563)))
    (CONS '/.
     (CONS (CAR V5564)
      (CONS (compile_combinator_body (CAR (CDR V5564))) NIL)))))
  ((AND (CONSP V5563) (EQ 'cases (CAR V5563)))
   (CONS 'cases (beta_reduce (CDR V5563))))
  (T (implementation_error 'compile_combinator_body))))

(DEFUN beta_reduce (V5565)
 (COND ((NULL V5565) NIL)
  ((CONSP V5565)
   (CONS (beta_reduce* NIL (CAR V5565)) (beta_reduce (CDR V5565))))
  (T (implementation_error 'beta_reduce))))

(DEFUN beta_reduce* (V5566 V5567)
 (COND
  ((AND (CONSP V5567) (EQ 'where (CAR V5567)) (CONSP (CDR V5567))
    (CONSP (CDR (CDR V5567))) (NULL (CDR (CDR (CDR V5567)))))
   (LET* ((V5568 (CDR V5567)))
    (beta_reduce* (APPEND V5566 (CONS (CAR V5568) NIL)) (CAR (CDR V5568)))))
  ((AND (CONSP V5567) (CONSP (CAR V5567)) (EQ '/. (CAR (CAR V5567)))
    (CONSP (CDR (CAR V5567))) (CONSP (CAR (CDR (CAR V5567))))
    (EQ 'cons (CAR (CAR (CDR (CAR V5567)))))
    (CONSP (CDR (CAR (CDR (CAR V5567)))))
    (CONSP (CDR (CDR (CAR (CDR (CAR V5567))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR V5567)))))))
    (CONSP (CDR (CDR (CAR V5567)))) (NULL (CDR (CDR (CDR (CAR V5567)))))
    (CONSP (CDR V5567)) (NULL (CDR (CDR V5567))))
   (LET*
    ((V5569 (CDR V5567)) (V5570 (CAR V5567)) (V5571 (CDR V5570))
     (V5572 (CAR V5571)) (V5573 (CDR V5572)))
    (LET ((NewZ (substf (CAR V5569) V5572 (CAR (CDR V5571)))))
     (LET ((NewTest (APPEND V5566 (CONS (CONS 'cons? V5569) NIL))))
      (beta_reduce* NewTest
       (CONS
        (CONS
         (CONS '/.
          (CONS (CAR V5573)
           (CONS (CONS '/. (CONS (CAR (CDR V5573)) (CONS NewZ NIL))) NIL)))
         (CONS (CONS 'CAR V5569) NIL))
        (CONS (CONS 'CDR V5569) NIL)))))))
  ((AND (CONSP V5567) (CONSP (CAR V5567)) (EQ '/. (CAR (CAR V5567)))
    (CONSP (CDR (CAR V5567))) (CONSP (CAR (CDR (CAR V5567))))
    (EQ '@p (CAR (CAR (CDR (CAR V5567)))))
    (CONSP (CDR (CAR (CDR (CAR V5567)))))
    (CONSP (CDR (CDR (CAR (CDR (CAR V5567))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR V5567)))))))
    (CONSP (CDR (CDR (CAR V5567)))) (NULL (CDR (CDR (CDR (CAR V5567)))))
    (CONSP (CDR V5567)) (NULL (CDR (CDR V5567))))
   (LET*
    ((V5574 (CDR V5567)) (V5575 (CAR V5567)) (V5576 (CDR V5575))
     (V5577 (CAR V5576)) (V5578 (CDR V5577)))
    (LET ((NewZ (substf (CAR V5574) V5577 (CAR (CDR V5576)))))
     (LET ((NewTest (APPEND V5566 (CONS (CONS 'tuple? V5574) NIL))))
      (beta_reduce* NewTest
       (CONS
        (CONS
         (CONS '/.
          (CONS (CAR V5578)
           (CONS (CONS '/. (CONS (CAR (CDR V5578)) (CONS NewZ NIL))) NIL)))
         (CONS (CONS 'fst V5574) NIL))
        (CONS (CONS 'snd V5574) NIL)))))))
  ((AND (CONSP V5567) (CONSP (CAR V5567)) (EQ '/. (CAR (CAR V5567)))
    (CONSP (CDR (CAR V5567))) (CONSP (CDR (CDR (CAR V5567))))
    (NULL (CDR (CDR (CDR (CAR V5567))))) (CONSP (CDR V5567))
    (NULL (CDR (CDR V5567))) (wrapper (variable? (CAR (CDR (CAR V5567))))))
   (LET* ((V5579 (CAR V5567)) (V5580 (CDR V5579)))
    (beta_reduce* V5566
     (substf (CAR (CDR V5567)) (CAR V5580) (CAR (CDR V5580))))))
  ((AND (CONSP V5567) (CONSP (CAR V5567)) (EQ '/. (CAR (CAR V5567)))
    (CONSP (CDR (CAR V5567))) (CONSP (CDR (CDR (CAR V5567))))
    (NULL (CDR (CDR (CDR (CAR V5567))))) (CONSP (CDR V5567))
    (NULL (CDR (CDR V5567)))
    (wrapper
     (or (atom? (CAR (CDR (CAR V5567)))) (empty? (CAR (CDR (CAR V5567)))))))
   (LET* ((V5581 (CAR V5567)) (V5582 (CDR V5581)))
    (beta_reduce*
     (APPEND V5566 (CONS (CONS 'qi_= (CONS (CAR V5582) (CDR V5567))) NIL))
     (CAR (CDR V5582)))))
  ((AND (CONSP V5567) (CONSP (CDR V5567)) (NULL (CDR (CDR V5567))))
   (LET* ((V5583 (CAR V5567)))
    (LET ((Pair (beta_reduce* V5566 V5583)))
     (LET ((X* (head (tail Pair))))
      (LET ((Test* (head Pair)))
       (IF (EQ 'true (qi_= X* V5583)) (CONS V5566 (CONS V5567 NIL))
        (beta_reduce* Test* (CONS X* (CDR V5567)))))))))
  (T (CONS V5566 (CONS V5567 NIL)))))

(DEFUN substf (V5594 V5595 V5596)
 (COND ((wrapper (qi_= V5595 V5596)) V5594)
  ((AND (CONSP V5596) (EQ '/. (CAR V5596)) (CONSP (CDR V5596))
    (CONSP (CDR (CDR V5596))) (NULL (CDR (CDR (CDR V5596))))
    (wrapper (occurs? V5595 (CAR (CDR V5596)))))
   V5596)
  ((AND (CONSP V5596) (EQ 'let (CAR V5596)) (CONSP (CDR V5596))
    (CONSP (CDR (CDR V5596))) (CONSP (CDR (CDR (CDR V5596))))
    (NULL (CDR (CDR (CDR (CDR V5596)))))
    (wrapper (qi_= V5595 (CAR (CDR V5596)))))
   (LET* ((V5597 (CDR V5596)) (V5598 (CAR V5597)) (V5599 (CDR V5597)))
    (CONS 'let
     (CONS V5598 (CONS (substf V5594 V5598 (CAR V5599)) (CDR V5599))))))
  ((CONSP V5596) (THE LIST (map #'(LAMBDA (Z) (substf V5594 V5595 Z)) V5596)))
  (T V5596)))

(DEFUN atom? (V5602)
 (THE SYMBOL
  (or
   (THE SYMBOL (or (THE SYMBOL (string? V5602)) (THE SYMBOL (symbol? V5602))))
   (THE SYMBOL
    (or (THE SYMBOL (number? V5602))
     (THE SYMBOL
      (or (THE SYMBOL (boolean? V5602)) (THE SYMBOL (character? V5602)))))))))

(DEFUN compile_to_lisp (V5608)
 (COND
  ((AND (CONSP V5608) (EQ 'y-combinator (CAR V5608)) (CONSP (CDR V5608))
    (CONSP (CAR (CDR V5608))) (EQ '/. (CAR (CAR (CDR V5608))))
    (CONSP (CDR (CAR (CDR V5608)))) (CONSP (CDR (CDR (CAR (CDR V5608)))))
    (NULL (CDR (CDR (CDR (CAR (CDR V5608)))))) (NULL (CDR (CDR V5608))))
   (LET*
    ((V5609 (CDR V5608)) (V5610 (CAR V5609)) (V5611 (CDR V5610))
     (V5612 (CDR V5611)) (V5613 (CAR V5611)) (V5614 (CAR V5612)))
    (LET ((Params (params V5614)))
     (record_source
      (CONS 'DEFUN
       (CONS V5613
        (CONS Params
         (CONS
          (insert_type_declarations
           (find_and_bind_choicepoints
            (optimise_lisp
             (CONS 'COND
              (insert_errmess V5613 (lisp_body Params (body V5614)))))))
          NIL))))))))
  (T (implementation_error 'compile_to_lisp))))

(DEFUN find_and_bind_choicepoints (V5615)
 (bind_choicepoints (find_choicepoints V5615) V5615))

(DEFUN find_choicepoints (V5620)
 (COND
  ((AND (CONSP V5620) (CONSP (CDR V5620)) (CONSP (CDR (CDR V5620)))
    (NULL (CDR (CDR (CDR V5620))))
    (wrapper
     (and (qi_= (CAR V5620) 'SETQ) (choicepoint? (CAR (CDR V5620))))))
   (CONS (CAR (CDR V5620)) NIL))
  ((CONSP V5620)
   (THE LIST
    (union (find_choicepoints (CAR V5620)) (find_choicepoints (CDR V5620)))))
  (T NIL)))

(DEFUN bind_choicepoints (V5622 V5623)
 (COND ((NULL V5622) V5623)
  (T (CONS 'PROG (CONS V5622 (CONS (CONS 'RETURN (CONS V5623 NIL)) NIL))))))

(DEFUN choicepoint? (V5632)
 (COND
  ((wrapper (symbol? V5632))
   (LET ((Chars (coerce V5632))) (check_choicepoint? Chars)))
  (T 'false)))

(DEFUN coerce (X) (COERCE (FORMAT NIL "~S" X) 'LIST))

(DEFUN check_choicepoint? (V5639)
 (COND
  ((AND (CONSP V5639) (EQUAL #\c (CAR V5639)) (CONSP (CDR V5639))
    (EQUAL #\h (CAR (CDR V5639))) (CONSP (CDR (CDR V5639)))
    (EQUAL #\o (CAR (CDR (CDR V5639)))) (CONSP (CDR (CDR (CDR V5639))))
    (EQUAL #\i (CAR (CDR (CDR (CDR V5639)))))
    (CONSP (CDR (CDR (CDR (CDR V5639)))))
    (EQUAL #\c (CAR (CDR (CDR (CDR (CDR V5639))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR V5639))))))
    (EQUAL #\e (CAR (CDR (CDR (CDR (CDR (CDR V5639)))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR V5639)))))))
    (EQUAL #\p (CAR (CDR (CDR (CDR (CDR (CDR (CDR V5639))))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5639))))))))
    (EQUAL #\o (CAR (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5639)))))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5639)))))))))
    (EQUAL #\i (CAR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5639))))))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5639))))))))))
    (EQUAL #\n
     (CAR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5639)))))))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5639)))))))))))
    (EQUAL #\t
     (CAR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5639)))))))))))))
   'true)
  (T 'false)))

(DEFUN record_source (V5640)
 (COND
  ((AND (CONSP V5640) (CONSP (CDR V5640)) (CONSP (CDR (CDR V5640))))
   (LET* ((V5645 (CDR V5640)) (V5647 (CAR V5645)))
    (do (store_arity V5647 (THE NUMBER (length (CAR (CDR V5645)))))
     (IF (EQ 'true (dumped?)) (write-to-dump-file V5640) 'ok)
     (put-prop V5647 'source_code V5640))))
  (T (implementation_error 'record_source))))

(DEFUN dumped? NIL (qi_= T (BOUNDP '*dump-file*) ))

(DEFUN write-to-dump-file (V5662)
 (THE STRING (write-to-file *dump-file* V5662)))

(DEFUN insert_type_declarations (V5668)
 (COND
  ((CONSP V5668)
   (LET* ((V5671 (CAR V5668)) (V5672 (CDR V5668)))
    (LET ((Type (get_result_type V5671)))
     (IF (EQ 'true (THE SYMBOL (empty? Type)))
      (CONS V5671 (MAPCAR 'insert_type_declarations V5672))
      (CONS 'THE
       (CONS Type
        (CONS (CONS V5671 (MAPCAR 'insert_type_declarations V5672)) NIL)))))))
  (T V5668)))

(DEFUN get_result_type (V5679)
 (COND
  ((wrapper (symbol? V5679))
   (LET ((Type (get_type_of_func V5679)))
    (IF (EQ 'true (THE SYMBOL (empty? Type))) Type (result_type Type))))
  (T NIL)))

(DEFUN get_type_of_func (F)
 (LET ((Code (GETHASH F *sfht* NIL)))
  (IF (NULL Code) NIL (normalise_type (FUNCALL Code)))))

(DEFUN result_type (V5686)
 (COND
  ((AND (CONSP V5686) (CONSP (CDR V5686)) (EQ '--> (CAR (CDR V5686)))
    (CONSP (CDR (CDR V5686))) (NULL (CDR (CDR (CDR V5686))))
    (wrapper (symbol? (CAR (CDR (CDR V5686))))))
   (assoctype (CAR (CDR (CDR V5686)))))
  ((AND (CONSP V5686) (CONSP (CDR V5686)) (EQ '--> (CAR (CDR V5686)))
    (CONSP (CDR (CDR V5686))) (CONSP (CAR (CDR (CDR V5686))))
    (EQ 'list (CAR (CAR (CDR (CDR V5686)))))
    (CONSP (CDR (CAR (CDR (CDR V5686)))))
    (NULL (CDR (CDR (CAR (CDR (CDR V5686)))))) (NULL (CDR (CDR (CDR V5686))))
    (wrapper (symbol? (CAR (CDR (CAR (CDR (CDR V5686))))))))
   'LIST)
  ((AND (CONSP V5686) (CONSP (CDR V5686)) (EQ '--> (CAR (CDR V5686)))
    (CONSP (CDR (CDR V5686))) (NULL (CDR (CDR (CDR V5686)))))
   (result_type (CAR (CDR (CDR V5686)))))
  (T NIL)))

(DEFUN assoctype (V5687)
 (LET ((Lisptype (ASSOC V5687 *assoctypes*)))
  (IF (EQ 'true (THE SYMBOL (empty? Lisptype))) Lisptype
   (head (THE LIST (tail Lisptype))))))

(DEFVAR *assoctypes*
 '((string STRING) (character CHARACTER) (symbol SYMBOL) (boolean SYMBOL)
   (number NUMBER) (goals LIST)))

(DEFUN optimise_lisp (V5360)
 (COND
  ((AND (CONSP V5360) (CONSP (CDR V5360)) (CONSP (CAR (CDR V5360)))
    (CONSP (CDR (CAR (CDR V5360)))) (NULL (CDR (CDR (CAR (CDR V5360)))))
    (NULL (CDR (CDR V5360)))
    (wrapper
     (and (qi_= 'COND (CAR V5360)) (qi_= (CAR (CAR (CDR V5360))) 'T))))
   (optimise_lisp (CAR (CDR (CAR (CDR V5360))))))
  ((AND (CONSP V5360) (EQ (CAR V5360) 'COND))
   (CONS (CAR V5360)
    (MAPCAR 'optimise_lisp (MAPCAR 'optimise_car/cdr (CDR V5360)))))
  ((AND (CONSP V5360) (EQ 'set (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CAR (CDR V5360))) (CONSP (CDR (CAR (CDR V5360))))
    (NULL (CDR (CDR (CAR (CDR V5360))))) (CONSP (CDR (CDR V5360)))
    (NULL (CDR (CDR (CDR V5360))))
    (EQ (CAR (CAR (CDR V5360))) 'QUOTE))
   (LET* ((V5361 (CDR V5360)))
    (CONS 'SETQ
     (CONS (CAR (CDR (CAR V5361)))
      (CONS (optimise_lisp (CAR (CDR V5361))) NIL)))))  
 ((AND (CONSP V5360) (EQ 'map (CAR V5360)) (CONSP (CDR V5360))
   (CONSP (CDR (CDR V5360))) (NULL (CDR (CDR (CDR V5360))))
   (wrapper (simple_map? (CAR (CDR V5360)))))
  (LET* ((V5361 (CDR V5360)))
   (CONS 'MAPCAR
    (CONS (CAR V5361) (CONS (optimise_lisp (CAR (CDR V5361))) NIL)))))
  ((AND (CONSP V5360) (CONSP (CDR V5360)) (NULL (CAR (CDR V5360)))
    (CONSP (CDR (CDR V5360))) (NULL (CDR (CDR (CDR V5360))))
    (EQ (CAR V5360) 'LET*))
   (optimise_lisp (CAR (CDR (CDR V5360)))))
  ((AND (CONSP V5360) (EQ 'value (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CAR (CDR V5360))) (CONSP (CDR (CAR (CDR V5360))))
    (NULL (CDR (CDR (CAR (CDR V5360))))) (NULL (CDR (CDR V5360)))
    (wrapper
     (and (qi_= (CAR (CAR (CDR V5360))) 'QUOTE)
      (symbol? (CAR (CDR (CAR (CDR V5360))))))))
   (CAR (CDR (CAR (CDR V5360)))))
  ((AND (CONSP V5360) (EQ 'wrapper (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CAR (CDR V5360))) (EQ 'qi_= (CAR (CAR (CDR V5360))))
    (CONSP (CDR (CAR (CDR V5360)))) (NULL (CAR (CDR (CAR (CDR V5360)))))
    (CONSP (CDR (CDR (CAR (CDR V5360)))))
    (NULL (CDR (CDR (CDR (CAR (CDR V5360)))))) (NULL (CDR (CDR V5360))))
   (CONS 'NULL (CONS (optimise_lisp (CAR (CDR (CDR (CAR (CDR V5360)))))) NIL)))
  ((AND (CONSP V5360) (EQ 'wrapper (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CAR (CDR V5360))) (EQ 'qi_= (CAR (CAR (CDR V5360))))
    (CONSP (CDR (CAR (CDR V5360)))) (CONSP (CAR (CDR (CAR (CDR V5360)))))
    (CONSP (CDR (CAR (CDR (CAR (CDR V5360))))))
    (NULL (CDR (CDR (CAR (CDR (CAR (CDR V5360)))))))
    (CONSP (CDR (CDR (CAR (CDR V5360)))))
    (NULL (CDR (CDR (CDR (CAR (CDR V5360)))))) (NULL (CDR (CDR V5360)))
    (wrapper
     (and (qi_= (CAR (CAR (CDR (CAR (CDR V5360))))) 'QUOTE)
      (or (symbol? (CAR (CDR (CAR (CDR (CAR (CDR V5360)))))))
       (boolean? (CAR (CDR (CAR (CDR (CAR (CDR V5360)))))))))))
   (LET* ((V5362 (CDR V5360)) (V5363 (CAR V5362)) (V5364 (CDR V5363)))
    (CONS 'EQ
     (CONS (CAR V5364) (CONS (optimise_lisp (CAR (CDR V5364))) NIL)))))
  ((AND (CONSP V5360) (EQ 'wrapper (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CAR (CDR V5360))) (EQ 'qi_= (CAR (CAR (CDR V5360))))
    (CONSP (CDR (CAR (CDR V5360)))) (CONSP (CDR (CDR (CAR (CDR V5360)))))
    (NULL (CDR (CDR (CDR (CAR (CDR V5360)))))) (NULL (CDR (CDR V5360)))
    (wrapper
     (or (number? (CAR (CDR (CAR (CDR V5360)))))
      (string? (CAR (CDR (CAR (CDR V5360))))))))
   (LET* ((V5365 (CDR V5360)) (V5366 (CAR V5365)) (V5367 (CDR V5366)))
    (CONS 'EQUAL
     (CONS (CAR V5367) (CONS (optimise_lisp (CAR (CDR V5367))) NIL)))))
  ((AND (CONSP V5360) (EQ 'wrapper (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CAR (CDR V5360))) (EQ 'qi_= (CAR (CAR (CDR V5360))))
    (CONSP (CDR (CAR (CDR V5360)))) (CONSP (CDR (CDR (CAR (CDR V5360)))))
    (NULL (CDR (CDR (CDR (CAR (CDR V5360)))))) (NULL (CDR (CDR V5360)))
    (wrapper (character? (CAR (CDR (CAR (CDR V5360)))))))
   (LET* ((V5368 (CDR V5360)) (V5369 (CAR V5368)) (V5370 (CDR V5369)))
    (CONS 'EQ
     (CONS (CAR V5370) (CONS (optimise_lisp (CAR (CDR V5370))) NIL)))))
  ((AND (CONSP V5360) (EQ 'input+ (CAR V5360)) (CONSP (CDR V5360))
    (EQ '$$ (CAR (CDR V5360))) (CONSP (CDR (CDR V5360)))
    (NULL (CDR (CDR (CDR V5360)))))
   V5360)
  ((AND (CONSP V5360) (EQ 'list (CAR V5360)))
   (CONS 'LIST (MAPCAR 'optimise_lisp (CDR V5360))))
  ((AND (CONSP V5360) (EQ 'cons (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CDR (CDR V5360))) (NULL (CDR (CDR (CDR V5360)))))
   (LET* ((V5371 (CDR V5360)))
    (CONS 'CONS
     (CONS (optimise_lisp (CAR V5371))
      (CONS (optimise_lisp (CAR (CDR V5371))) NIL)))))
  ((AND (CONSP V5360) (EQ 'reverse (CAR V5360)) (CONSP (CDR V5360))
    (NULL (CDR (CDR V5360))))
   (CONS 'REVERSE (CONS (optimise_lisp (CAR (CDR V5360))) NIL)))
  ((AND (CONSP V5360) (EQ 'append (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CDR (CDR V5360))) (NULL (CDR (CDR (CDR V5360)))))
   (LET* ((V5372 (CDR V5360)))
    (CONS 'APPEND
     (CONS (optimise_lisp (CAR V5372))
      (CONS (optimise_lisp (CAR (CDR V5372))) NIL)))))
  ((AND (CONSP V5360) (EQ 'wrapper (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CAR (CDR V5360))) (EQ 'not (CAR (CAR (CDR V5360))))
    (CONSP (CDR (CAR (CDR V5360)))) (CONSP (CAR (CDR (CAR (CDR V5360)))))
    (EQ 'escape? (CAR (CAR (CDR (CAR (CDR V5360))))))
    (CONSP (CDR (CAR (CDR (CAR (CDR V5360))))))
    (CONSP (CAR (CDR (CAR (CDR (CAR (CDR V5360)))))))
    (CONSP (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360))))))))
    (CONSP (CDR (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360)))))))))
    (CONSP (CAR (CDR (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360))))))))))
    (EQ 'fail-if
     (CAR (CAR (CDR (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360)))))))))))
    (CONSP (CDR (CAR (CDR (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360)))))))))))
    (CONSP
     (CAR (CDR (CAR (CDR (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360))))))))))))
    (CONSP
     (CDR
      (CAR (CDR (CAR (CDR (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360)))))))))))))
    (NULL
     (CDR
      (CDR
       (CAR
        (CDR (CAR (CDR (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360))))))))))))))
    (CONSP
     (CDR (CDR (CAR (CDR (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360))))))))))))
    (NULL
     (CDR
      (CDR (CDR (CAR (CDR (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360)))))))))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360))))))))))
    (NULL (CDR (CDR (CAR (CDR (CAR (CDR V5360)))))))
    (NULL (CDR (CDR (CAR (CDR V5360))))) (NULL (CDR (CDR V5360)))
    (wrapper
     (and
      (qi_=
       (CAR
        (CAR
         (CDR (CAR (CDR (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360))))))))))))
       'QUOTE)
      (symbol?
       (CAR
        (CDR
         (CAR
          (CDR
           (CAR (CDR (CDR (CAR (CDR (CAR (CDR (CAR (CDR V5360)))))))))))))))))
   (LET*
    ((V5373 (CDR V5360)) (V5374 (CAR V5373)) (V5375 (CDR V5374))
     (V5376 (CAR V5375)) (V5377 (CDR V5376)) (V5378 (CAR V5377))
     (V5379 (CDR V5378)) (V5380 (CDR V5379)) (V5381 (CAR V5380))
     (V5382 (CDR V5381)))
    (CONS 'NOT
     (CONS
      (CONS 'EQ
       (CONS (CONS 'QUOTE (CONS 'true NIL))
        (CONS
         (optimise_lisp
          (CONS (CAR (CDR (CAR V5382)))
           (CONS (CONS (CAR V5378) (CONS (CAR V5379) (CDR V5382))) NIL)))
         NIL)))
      NIL))))
  ((AND (CONSP V5360) (EQ 'if (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CDR (CDR V5360))) (CONSP (CDR (CDR (CDR V5360))))
    (NULL (CDR (CDR (CDR (CDR V5360))))))
   (LET* ((V5383 (CDR V5360)) (V5384 (CDR V5383)))
    (CONS 'IF
     (CONS
      (CONS 'EQ
       (CONS (CONS 'QUOTE (CONS 'true NIL))
        (CONS (optimise_lisp (CAR V5383)) NIL)))
      (CONS (optimise_lisp (CAR V5384))
       (CONS (optimise_lisp (CAR (CDR V5384))) NIL))))))
  ((CONSP V5360) (MAPCAR 'optimise_lisp V5360)) 
  (T V5360)))

(DEFUN simple_map? (V5364)
 (COND
  ((AND (CONSP V5364) (CONSP (CDR V5364)) (NULL (CDR (CDR V5364)))
    (EQ (CAR V5364) 'QUOTE) (EQ (arityf (CAR (CDR V5364))) 1))
   'true)
  (T 'false)))

 (DEFUN optimise_car/cdr (V5770)
 (COND
  ((AND (CONSP V5770) (CONSP (CDR V5770)) (NULL (CDR (CDR V5770))))
   (CONS (CAR V5770) (CONS (occ* (CAR (CDR V5770))) NIL)))
  (T (implementation_error 'optimise_car/cdr))))

(DEFUN isocar/cdrs (V5775)
 (COND
  ((AND (CONSP V5775) (CONSP (CDR V5775)) (NULL (CDR (CDR V5775)))
    (wrapper (and (qi_= (CAR V5775) 'CAR) (symbol? (CAR (CDR V5775))))))
   (CONS V5775 NIL))
  ((AND (CONSP V5775) (CONSP (CDR V5775)) (NULL (CDR (CDR V5775)))
    (wrapper (and (qi_= (CAR V5775) 'CDR) (symbol? (CAR (CDR V5775))))))
   (CONS V5775 NIL))
  ((CONSP V5775) (APPEND (isocar/cdrs (CAR V5775)) (isocar/cdrs (CDR V5775))))
  (T NIL)))

(DEFUN optimise_car/cdr (V5776)
 (COND
  ((AND (CONSP V5776) (CONSP (CDR V5776)) (NULL (CDR (CDR V5776))))
   (CONS (CAR V5776) (CONS (occ* (CAR (CDR V5776))) NIL)))
  (T (implementation_error 'optimise_car/cdr))))

(DEFUN occ* (V5777) (occ** NIL (isocar/cdrs V5777) V5777 'unoptimised))

(DEFUN occ** (V5783 V5784 V5785 V5786)
 (COND ((AND (NULL V5783) (NULL V5785) (EQ 'unoptimised V5786)) V5784)
  ((AND (NULL V5784) (EQ 'unoptimised V5786))
   (CONS 'LET* (CONS (REVERSE V5783) (CONS V5785 NIL))))
  ((AND (NULL V5784) (EQ 'optimised V5786))
   (occ** V5783 (isocar/cdrs V5785) V5785 'unoptimised))
  ((AND (CONSP V5784) (wrapper (qi_> (occurrences (CAR V5784) V5785) 1)))
   (LET* ((V5787 (CAR V5784)))
    (LET ((Var (gensym "V")))
     (occ** (CONS (CONS Var (CONS V5787 NIL)) V5783) (CDR V5784)
      (subst Var V5787 V5785) 'optimised))))
  ((CONSP V5784) (occ** V5783 (CDR V5784) V5785 V5786))
  (T (implementation_error 'occ**))))

(DEFUN occurrences (V5797 V5798)
 (COND ((wrapper (qi_= V5797 V5798)) 1)
  ((CONSP V5798)
   (THE NUMBER
    (+ (THE NUMBER (occurrences V5797 (CAR V5798)))
     (THE NUMBER (occurrences V5797 (CDR V5798))))))
  (T 0)))

(DEFUN smallnum? (V5803)
 (THE SYMBOL (and (THE SYMBOL (number? V5803)) (THE SYMBOL (qi_> 100 V5803)))))

(DEFUN insert_errmess (V5363 V5364)
 (COND
  ((AND (CONSP V5364) (CONSP (CAR V5364)) (CONSP (CDR (CAR V5364)))
    (NULL (CDR (CDR (CAR V5364)))) (NULL (CDR V5364)))
   (LET* ((V5365 (CAR V5364)))
    (IF (EQ 'true (qi_= (CAR V5365) T)) V5364
     (CONS V5365
      (CONS
       (CONS T
        (CONS (CONS 'f_error (CONS (CONS 'QUOTE (CONS V5363 NIL)) NIL)) NIL))
       NIL)))))
  ((CONSP V5364) (CONS (CAR V5364) (insert_errmess V5363 (CDR V5364))))
  (T (implementation_error 'insert_errmess))))

(DEFUN f_error (X) (FORMAT T "error: partial function ~A;" X)
 (IF (AND (COMPILED-FUNCTION-P (SYMBOL-FUNCTION X)) (Y-OR-N-P "track ~A ? " X))
  (track_function (source_code X)))
 (ERROR ""))

(DEFUN params (V5812)
 (COND
  ((AND (CONSP V5812) (EQ '/. (CAR V5812)) (CONSP (CDR V5812))
    (CONSP (CDR (CDR V5812))) (NULL (CDR (CDR (CDR V5812)))))
   (LET* ((V5813 (CDR V5812))) (CONS (CAR V5813) (params (CAR (CDR V5813))))))
  (T NIL)))

(DEFUN body (V5814)
 (COND
  ((AND (CONSP V5814) (EQ '/. (CAR V5814)) (CONSP (CDR V5814))
    (CONSP (CDR (CDR V5814))) (NULL (CDR (CDR (CDR V5814)))))
   (body (CAR (CDR (CDR V5814)))))
  ((AND (CONSP V5814) (EQ 'cases (CAR V5814))) (CDR V5814))
  (T (implementation_error 'body))))

(DEFUN lisp_body (V5817 V5818)
 (COND ((NULL V5818) NIL)
  ((CONSP V5818)
   (CONS (lisp_case V5817 (CAR V5818)) (lisp_body V5817 (CDR V5818))))
  (T (implementation_error 'lisp_body))))

(DEFUN lisp_case (V5819 V5820)
 (COND
  ((AND (CONSP V5820) (CONSP (CDR V5820)) (NULL (CDR (CDR V5820))))
   (CONS (lisp_code_tests V5819 (CAR V5820))
    (CONS (lisp_code V5819 (CAR (CDR V5820))) NIL)))
  (T (implementation_error 'lisp_case))))

(DEFUN lisp_code_tests (V5821 V5822)
 (COND ((NULL V5822) )
  ((AND (CONSP V5822) (NULL (CDR V5822))) (lisp_code_test V5821 (CAR V5822)))
  (T
   (CONS 'AND (MAPCAR #'(LAMBDA (Test) (lisp_code_test V5821 Test)) V5822)))))

(DEFUN lisp_code_test (V5823 V5824)
 (COND
  ((AND (CONSP V5824) (EQ 'cons? (CAR V5824)) (CONSP (CDR V5824))
    (NULL (CDR (CDR V5824))))
   (CONS 'CONSP (CONS (lisp_code V5823 (CAR (CDR V5824))) NIL)))
  ((AND (CONSP V5824) (CONSP (CDR V5824)) (CONSP (CDR (CDR V5824)))
    (NULL (CDR (CDR (CDR V5824)))) (wrapper (arith? (CAR V5824))))
   (LET* ((V5826 (CDR V5824)))
    (CONS (map_arith (CAR V5824))
     (CONS (lisp_code V5823 (CAR V5826))
      (CONS (lisp_code V5823 (CAR (CDR V5826))) NIL)))))
  (T (CONS 'wrapper (CONS (lisp_code V5823 V5824) NIL)))))

(DEFUN arith? (V5828) (THE SYMBOL (element? V5828 *arith*)))

(DEFVAR *arith* '(qi_> qi_< qi_>= qi_<=))

(DEFUN map_arith (x)
  (COND ((EQ x 'qi_>) '>)
           ((EQ x 'qi_<) '<)
           ((EQ x 'qi_>=) '>=)
           ((EQ x 'qi_<=) '<=)))  

(DEFUN wrapper (Test)
 (COND ((EQ Test 'true) T) ((EQ Test 'false) NIL)
  (T (ERROR "~A is not a boolean.~%" Test))))

(DEFUN lisp_code (V72 V73)
 (COND
  ((AND (CONSP V73) (CONSP (CDR V73)) (NULL (CDR (CDR V73)))
    (EQ 'QUOTE (CAR V73)))
   V73)
  ((AND (CONSP V73) (EQ 'let (CAR V73)) (CONSP (CDR V73))
    (CONSP (CDR (CDR V73))) (CONSP (CDR (CDR (CDR V73))))
    (NULL (CDR (CDR (CDR (CDR V73))))))
   (LET* ((V74 (CDR V73)) (V75 (CAR V74)) (V76 (CDR V74)))
    (CONS 'LET
     (CONS (CONS (CONS V75 (CONS (lisp_code V72 (CAR V76)) NIL)) NIL)
      (CONS (lisp_code (CONS V75 V72) (CAR (CDR V76))) NIL)))))
  ((AND (CONSP V73) (EQ '/. (CAR V73)) (CONSP (CDR V73))
    (CONSP (CDR (CDR V73))) (NULL (CDR (CDR (CDR V73)))))
   (LET* ((V77 (CDR V73)) (V78 (CAR V77)))
    (CONS 'FUNCTION
     (CONS
      (CONS 'LAMBDA
       (CONS (CONS V78 NIL)
        (CONS (lisp_code (CONS V78 V72) (CAR (CDR V77))) NIL)))
      NIL))))
  ((AND (CONSP V73) (EQ 'input+ (CAR V73)) (CONSP (CDR V73))
    (EQ '$$ (CAR (CDR V73))) (CONSP (CDR (CDR V73)))
    (NULL (CDR (CDR (CDR V73)))))
   V73)
  ((AND (CONSP V73) (CONSP (CDR V73)) (NULL (CDR (CDR V73)))
    (wrapper (partial_application? (CAR V73) (CDR V73))))
   (CONS 'apply
    (CONS (lisp_code V72 (CAR V73))
     (CONS (lisp_code V72 (CAR (CDR V73))) NIL))))
  ((AND (CONSP V73) (CONSP (CDR V73))
    (wrapper (partial_application? (CAR V73) (CDR V73))))
   (LET* ((V79 (CDR V73)))
    (lisp_code V72
     (CONS (CONS 'apply (CONS (CAR V73) (CONS (CAR V79) NIL))) (CDR V79)))))
  ((AND (CONSP V73) (CONSP (CAR V73)) (CONSP (CDR V73)) (NULL (CDR (CDR V73))))
   (lisp_code V72
    (CONS 'apply
     (CONS (lisp_code V72 (CAR V73))
      (CONS (lisp_code V72 (CAR (CDR V73))) NIL)))))
  ((AND (CONSP V73) (CONSP (CAR V73)) (CONSP (CDR V73)))
   (LET* ((V80 (CDR V73)))
    (lisp_code V72
     (CONS (CONS 'apply (CONS (CAR V73) (CONS (CAR V80) NIL))) (CDR V80)))))
  ((AND (CONSP V73) (CONSP (CDR V73)) (NULL (CDR (CDR V73)))
    (wrapper (element? (CAR V73) V72)))
   (LET* ((V81 (CAR V73)))
    (CONS 'apply
     (CONS V81 (CONS (lisp_code (CONS V81 V72) (CAR (CDR V73))) NIL)))))
  ((AND (CONSP V73) (CONSP (CDR V73)) (wrapper (element? (CAR V73) V72)))
   (LET* ((V82 (CDR V73)))
    (lisp_code V72
     (CONS (CONS 'apply (CONS (CAR V73) (CONS (CAR V82) NIL))) (CDR V82)))))
  ((CONSP V73)
   (CONS (CAR V73)
    (THE LIST (map #'(LAMBDA (Z) (lisp_code V72 Z)) (CDR V73)))))
  ((NULL V73) NIL) ((wrapper (number? V73)) V73)
  ((wrapper (string? V73)) V73) ((wrapper (character? V73)) V73)
  ((wrapper (element? V73 V72)) V73)
  ((EQ '*qi-failure-object* V73) '*qi-failure-object*)
  (T (CONS 'QUOTE (CONS V73 NIL)))))

(DEFUN partial_application? (V5853 V5854)
 (THE SYMBOL
  (and (THE SYMBOL (symbol? V5853))
   (LET ((ArityF (arityf V5853)))
    (IF (EQ 'true (THE SYMBOL (number? ArityF)))
     (check_arityf V5853 ArityF (THE NUMBER (length V5854))) 'false)))))

(DEFUN check_arityf (V5861 V5862 V5863)
 (COND ((wrapper (qi_= V5862 V5863)) 'false)
  ((wrapper (qi_> V5863 V5862))
   (warn (FORMAT NIL "~A may not like ~A arguments.~%" V5861 V5863))
   'false)
  (T 'true)))

(DEFUN apply (V5864 V5865)
 (COND
  ((wrapper
    (or (full_application? V5864 (CONS V5865 NIL)) (closure? V5864)))
   (FUNCALL V5864 V5865))
  ((wrapper (partial_application? V5864 (CONS V5865 NIL)))
   (FUNCALL (make-closure V5864) V5865))
  (T (error "Odd application; ~A to ~A~%" V5864 V5865))))

(DEFUN closure? ( X) (element? (TYPE-OF X) *closures*))

(DEFUN make-closure (V5866) (EVAL (make_closure* V5866 (arityf V5866) NIL)))

(DEFUN make_closure* (V22597 V22598 V22599)
 (COND
  ((EQ 1 V22598)
   (LET ((X (gensym "X"))) (/@ X (CONS V22597 (REVERSE (CONS X V22599))))))
  (T
   (LET ((X (gensym "X")))
    (/@ X (make_closure* V22597 (THE NUMBER (- V22598 1)) (CONS X V22599)))))))

(DEFMACRO /. (X Body) `(FUNCTION (LAMBDA (,X) ,Body)))

(DEFUN /@ (X Body) `(FUNCTION (LAMBDA (,X) ,Body)))

(DEFUN full_application? (V5872 V5873)
 (THE SYMBOL
  (and (THE SYMBOL (symbol? V5872))
   (LET ((Arity (arityf V5872)))
    (THE SYMBOL
     (or (qi_= Arity (THE NUMBER (length V5873)))
      (THE SYMBOL (empty? Arity))))))))

(DEFUN map (V5879 V5880)
 (COND ((NULL V5880) NIL)
  ((CONSP V5880)
   (CONS (apply V5879 (CAR V5880))
    (THE LIST (map V5879 (CDR V5880)))))
  (T (error "map requires a list; not ~S" V5880))))

(DEFUN arityf (FUNC) (GETHASH FUNC *arity* NIL))

(DEFVAR *arity*
 (MAKE-HASH-TABLE :SIZE 300 :REHASH-SIZE 2 :REHASH-THRESHOLD 0.8))

(DEFUN initialise_arity_table ()
 (store_arities
  '(and 2 append 2 apply 2 atp-credits 1 atp-prompt 1 back 2 boolean? 1 cd 1
    character? 1 compile 2 complex? 1 concat 2 congruent? 2 cons? 1 debug 1
    destroy 1 delete-file 1 difference 2 display-mode 1 dump-proof 1 element? 2 empty? 1 eval
    1 explode 1 fail-if 2 fix 2 float? 1 from-goals 1 fst 1 fst-ass 1 fst-conc
    1 fst-goal 1 gensym 1 get-array 3 get-lambda 1 get-prop 3 get-rule 2 qi_> 2 qi_>= 2 qi_=
    2 head 1 if-with-checking 1 if-without-checking 1 integer? 1 inferences 1 length 1 loadurl 1 qi_< 2 qi_<= 2
    macroexpand 1 make-array 1 map 2 maxinferences 1 new-assoc-type 2 not 1 notes-in 1 nth 2
    number? 1 occurs-check 1 occurrences 2 or 2 prf 1 print 1 profile 1 profile-results 1 prooftool 1
    provable? 2 put-array 3 put-prop 3 random 1 read-char 1 read-file-as-charlist 1 
    read-file 1 read-chars-as-stringlist 2 rational? 1 real? 1 refine 4 remove 2
    reverse 1 rotate 3 round 1 snd 1 solved? 1 specialise 1 spy 1 sqrt 1 step 1
    string? 1 strong-warning 1 subst 3 swap 3 symbol? 1 system 1 tail 1 tc 1 theory-size 1 thin
    2 thm-intro 1 time 1 time-proof 3 to-goals 2 track 1 tuple 2 unprofile 1
    undebug 1 union 2 unprf 1 untrack 1 unspecialise 1 value 1 variable? 1 version 1 warn 1 write-to-file 2 y-or-n? 1 
    + 2 * 2 / 2 - 2 == 2 @p 2 preclude 1 include 1 preclude-all-but 1 include-all-but 1)))

(DEFUN new-assoc-type (X Y) (PUSHNEW  (LIST X Y) *assoctypes*) 'associated)

(DEFUN store_arities (V5883)
 (COND ((NULL V5883) NIL)
  ((AND (CONSP V5883) (CONSP (CDR V5883)))
   (LET* ((V5884 (CDR V5883)))
    (do (store_arity (CAR V5883) (CAR V5884)) (store_arities (CDR V5884)))))
  (T (implementation_error 'store_arities))))

(DEFUN store_arity (FUNC N)
 (LET ((Arity (arityf FUNC)))
  (IF (AND (NUMBERP Arity) (NOT (= Arity N)))
   (warn (FORMAT NIL "Changing the arity of ~A may cause errors~%" FUNC)))
  (SETF (GETHASH FUNC *arity*) N)))

(DEFMACRO do (&REST X) (CONS 'PROGN X))

(initialise_arity_table)

(DEFVAR *sfht*
 (MAKE-HASH-TABLE :SIZE 300 :REHASH-SIZE 2 :REHASH-THRESHOLD 0.8))

(DEFUN setup-base-types ()
 (setup-sfht
  '(and (boolean --> (boolean --> boolean)) append
    ((list A) --> ((list A) --> (list A))) apply ((A --> B) --> (A --> B))
    assoc (A --> ((list (list A)) --> (list A))) atp-credits
    (string --> string) atp-prompt (string --> string) boolean? (A --> boolean)
    cd (string --> string) character? (A --> boolean) collect
    (symbol --> (number --> ((list parameter) --> (goals --> (list goals)))))
    complex? (A --> boolean) concat (symbol --> (symbol --> symbol)) congruent?
    (A --> (A --> boolean)) cons? (A --> boolean) debug (A --> string)
    delete-file (string --> string) difference ((list A) --> ((list A) --> (list A))) 
    display-mode (symbol --> symbol) dump-proof (string --> string) element?
    (A --> ((list A) --> boolean)) empty? (A --> boolean) explode
    (A --> (list character)) fail-if
    ((character --> boolean) --> (character --> character)) fix
    ((A --> A) --> (A --> A)) float? (A --> boolean) from-goals
    (goals --> ((list ((list wff) * wff)) * (list note))) fst ((A * B) --> A)
    fst-goal (goals --> ((list wff) * wff)) fst-conc (goals --> wff) fst-ass
    (goals --> (list wff)) gensym (string --> symbol) get-array
    ((array A) --> ((list number) --> (A --> A))) 
    get-lambda ((A --> B) --> unit)
    get-rule (symbol --> (number --> (goals --> goals))) head ((list A) --> A)
    if-with-checking (string --> symbol)
    if-without-checking (string --> symbol)
    inferences (A --> number) integer? (A --> boolean) length
    ((list A) --> number) load (string --> symbol) make-array ((list number) --> (array A))
    maxinferences (number --> number) map
    ((A --> B) --> ((list A) --> (list B))) not (boolean --> boolean) 
    new-assoc-type (symbol --> (symbol --> symbol))
    notes-in  (goals --> (list note)) nth (number --> ((list A) --> A)) number?
    (A --> boolean) occurs-check (symbol --> boolean) occurrences (A --> (B --> number)) or
    (boolean --> (boolean --> boolean)) prf ((A --> B) --> (A --> B)) 
    print (A --> A) profile ((A --> B) --> (A --> B)) profile-results (A --> symbol) 
    prooftool (symbol --> symbol) provable?
    ((goals --> goals) --> ((list ((list wff) * wff)) --> boolean)) put-array
    ((array A) --> ((list number) --> (A --> A))) random (number --> number)
    rational? (A --> boolean) read-char (A --> character) 
    read-file (string --> (list unit))
    read-file-as-charlist  (string --> (list character)) read-chars-as-stringlist
    ((list character) --> ((character --> boolean) --> (list string))) 
    rational? (A --> boolean) real? (A --> boolean)
    refine (symbol --> (number --> ((list parameter) --> (goals --> goals))))
    remove
    (A --> ((list A) --> (list A))) 
    reverse ((list A) --> (list A)) rotate
    (number --> (number --> (goals --> goals))) round (number --> number) snd
    ((A * B) --> B) solved? (goals --> boolean) specialise (symbol --> symbol)
   sqrt (number --> number) spy
    (symbol --> boolean) step (symbol --> boolean) string? (A --> boolean) swap
    (number --> (number --> (goals --> goals))) strong-warning (symbol --> boolean)
    symbol? (A --> boolean) tail
    ((list A) --> (list A)) tc (symbol --> boolean) system (string --> string)
    theory-size (symbol --> number) thin (number --> (goals --> goals))
    thm-intro (symbol --> symbol) time (A --> A) time-proof
    ((goals --> goals) --> ((list wff) --> (wff --> boolean))) to-goals
    ((list ((list wff) * wff)) --> ((list note) --> goals)) track
    ((A --> B) --> (A --> B)) tuple? (A --> boolean) undebug (A --> string)
    unprofile ((A --> B) --> (A --> B)) untrack ((A --> B) --> (A --> B)) unprf
    ((A --> B) --> (A --> B)) union
    ((list A) --> ((list A) --> (list A))) 
    unspecialise (symbol --> symbol)
    variable? (A --> boolean)
    version (string --> string) warn (string --> string)
    write-to-file (string --> (A --> string)) y-or-n? (string --> boolean) qi_>
    (number --> (number --> boolean)) qi_< (number --> (number --> boolean))
    qi_>= (number --> (number --> boolean)) qi_<=
    (number --> (number --> boolean)) + (number --> (number --> number)) *
    (number --> (number --> number)) - (number --> (number --> number)) /
    (number --> (number --> number)) == (A --> (B --> boolean))

    preclude ((list symbol) --> (list symbol))
    include ((list symbol) --> (list symbol))
    preclude-all-but ((list symbol) --> (list symbol))
    include-all-but ((list symbol) --> (list symbol))

    )))

(DEFUN setup-sfht (L)
 (IF (NULL L) NIL
  (PROGN (add-to-type-discipline (CAR L) (CADR L)) (setup-sfht (CDDR L)))))

(DEFMACRO newfuntype (F Type) 
  `(newfuntype* (QUOTE ,F) (QUOTE ,Type)))

(DEFUN newfuntype* (F Type) (add-to-type-discipline F (curry_type Type)) F)

(DEFUN record_type (FUNC TYPE)
 (LET ((OLDTYPE (get_type_of_func FUNC)))
  (IF
   (OR (NULL OLDTYPE)
    (variant? (normalise_type TYPE) (normalise_type OLDTYPE)))
   (add-to-type-discipline FUNC TYPE)
   (warn (FORMAT NIL "~A already has a type.~%" FUNC)))))

(DEFUN warn (String)
  (COND ((EQ *strong-warning* 'true) (error String))
           (T (FORMAT T "======> ") (output "Warning: ~A~%" String))))

(SETQ *strong-warning* 'false)

(DEFUN strong-warning (X)
  (COND ((EQ X '+) (SETQ *strong-warning* 'true))
           ((EQ X '-) (SETQ *strong-warning* 'false)) 
           (T (ERROR "strong-warning expects a + or a -.~%"))))

(DEFUN variant? (NEW OLD)
 (COND ((EQUAL NEW OLD) T)
  ((EQUAL (CAR OLD) (CAR NEW)) (variant? (CDR OLD) (CDR NEW)))
  ((AND (EQ 'true (variable? (CAR OLD))) (EQ 'true (variable? (CAR NEW))))
   (variant? (SUBST 'a (CAR OLD) (CDR OLD)) (SUBST 'a (CAR NEW) (CDR NEW))))
  ((AND (CONSP (CAR OLD)) (CONSP (CAR NEW)))
   (variant? (APPEND (CAR OLD) (CDR OLD)) (APPEND (CAR NEW) (CDR NEW))))
  (T NIL)))

(DEFUN variable? (X)
 (IF (AND (SYMBOLP X) (NOT (NULL X)) (UPPER-CASE-P (CHAR (SYMBOL-NAME X) 0)))
  'true 'false))

(DEFUN add-to-type-discipline (FUNC TYPE)
 (SETF (GETHASH FUNC *sfht*)
  (COMPILE NIL (LIST 'LAMBDA () (st_code TYPE)))))

(DEFUN st_code (Type)
 (LET ((Vs (extract_vars Type))) (st_code* Vs (bld_st_code Vs Type))))

(DEFUN extract_vars (V5885) (extract_vars* V5885 NIL))

(DEFUN extract_vars* (V5890 V5891)
 (COND
  ((wrapper (variable? V5890)) (THE LIST (union (CONS V5890 NIL) V5891)))
  ((CONSP V5890)
   (THE LIST
    (union (extract_vars* (CAR V5890) V5891)
     (extract_vars* (CDR V5890) V5891))))
  (T V5891)))

(DEFUN bld_st_code (Vs Type)
 (COND ((NULL Type) NIL)
          ((CONSP Type)  
           (LIST 'CONS (bld_st_code Vs (CAR Type)) (bld_st_code Vs (CDR Type))))
         ((MEMBER Type Vs) Type) 
         (T (LIST 'QUOTE Type))))

(DEFUN st_code* (Variables Type)
 (IF (NULL Variables) Type
  (LIST 'LET (MAPCAR #'(LAMBDA (X) (LIST X (LIST 'gensym "A"))) Variables)
   Type)))

(setup-base-types)

(DEFUN destroy (Func) (REMHASH Func *sfht*) (REMHASH Func *arity*)
 (SETQ *userdefs* (REMOVE Func *userdefs*)) (FMAKUNBOUND Func))

(DEFVAR *Failure* '(NIL #\Escape))

(DEFUN compile (F X)
 (LET ((O (FUNCALL F (LIST X NIL))))
  (IF (OR (failure? O) (NOT (NULL (CAR O)))) #\Escape (CADR O))))

(DEFMACRO defcc (Symbol &REST CC_stuff)
 (LIST 'compile_cc (LIST 'QUOTE Symbol) (LIST 'QUOTE CC_stuff)))

(DEFUN compile_cc (Symbol CC_Stuff)
 (COMPILE
  (EVAL
   (record_source
    (LIST 'DEFUN Symbol '(Stream)
     (CONS 'cases (MAPCAR 'cc_body (split_cc_rules CC_Stuff NIL))))))))

(DEFUN split_cc_rules (CCstuff RevRule)
 (COND
  ((NULL CCstuff)
   (IF (NULL RevRule) NIL (LIST (split_cc_rule (REVERSE RevRule) NIL))))
  ((EQ (FIRST CCstuff) ';)
   (CONS (split_cc_rule (REVERSE RevRule) NIL)
    (split_cc_rules (REST CCstuff) NIL)))
  (T (split_cc_rules (REST CCstuff) (CONS (FIRST CCstuff) RevRule)))))

(DEFUN split_cc_rule (Rule RevSyntax)
  (COND ((EQ (FIRST Rule) ':=)
         (IF (= (LENGTH (REST Rule)) 1)
             (LIST (REVERSE RevSyntax) (SECOND Rule))
             (LIST (REVERSE RevSyntax) (CONS 'LIST (REST Rule)))))
        ((NULL Rule)
         (warn (FORMAT NIL "~{ ~S~} has no semantics.~%" (REVERSE RevSyntax)))
         (split_cc_rule (LIST ':= (default_semantics (REVERSE RevSyntax))) RevSyntax))
        (T (split_cc_rule (REST Rule) (CONS (FIRST Rule) RevSyntax)))))

(DEFUN default_semantics (Syntax)
 (COND ((NULL Syntax) Syntax)
  ((grammar_symbol? (CAR Syntax))
   (LIST 'APPEND (CAR Syntax) (default_semantics (CDR Syntax))))
  (T (LIST 'CONS (CAR Syntax) (default_semantics (CDR Syntax))))))

(DEFUN cc_body (Rule) (syntax (FIRST Rule) 'Stream (SECOND Rule)))

(DEFUN syntax (Syntax Stream Semantics)
  (COND ((NULL Syntax)
         `(LIST (FIRST ,Stream) ,(semantics Semantics)))
        ((close_proc? (FIRST Syntax)) Semantics)         
        ((grammar_symbol? (FIRST Syntax))
         (recursive_descent Syntax Stream Semantics))
        ((terminal? (FIRST Syntax))
         (check_stream Syntax Stream Semantics))
        ((eos? (FIRST Syntax))
         (check_eos Syntax Stream Semantics))
        ((jump_stream? (FIRST Syntax))
         (jump_stream Syntax Stream Semantics))
        ((list_call? (FIRST Syntax))
         (list_call (CONS (decons (FIRST Syntax))
                          (REST Syntax))
                    Stream Semantics))
        (T (ERROR "~S is not legal syntax.~%" (FIRST Syntax)))))

(DEFUN close_proc? (Fragment) (EQ Fragment '-end-of-list-))

(DEFUN grammar_symbol? (Fragment)
 (AND (SYMBOLP Fragment)
  (LET*
   ((CHARS (COERCE (FORMAT NIL "~A" Fragment) 'LIST)) (FCHAR (FIRST CHARS))
    (LCHAR (FIRST (LAST CHARS))))
   (AND (CHAR-EQUAL #\< FCHAR) (CHAR-EQUAL #\> LCHAR)))))

(DEFUN recursive_descent (Syntax Stream Semantics)
  (LET ((Test (LIST (FIRST Syntax) Stream))
        (Action (syntax (REST Syntax) (FIRST Syntax) Semantics))
        (Else (CONS 'LIST *Failure*)))
       `(LET ((,(FIRST Syntax) ,Test))
             (IF (NOT (failure? ,(FIRST Syntax)))
                 ,Action
                 ,Else))))

(DEFUN list_call? (Fragment) (AND (CONSP Fragment) (cons_structure? Fragment)))

(DEFUN cons_structure? (Fragment)
 (COND
  ((CONSP Fragment)
   (IF (AND (EQ (FIRST Fragment) 'cons) (qi_= (LENGTH Fragment) 3))
    (AND (cons_structure? (SECOND Fragment))
     (cons_structure? (THIRD Fragment)))
    NIL))
  (T)))

(DEFUN decons (Fragment)
 (COND
  ((AND (CONSP Fragment) (EQ (FIRST Fragment) 'cons))
   (CONS (decons (SECOND Fragment)) (decons (THIRD Fragment))))
  (T Fragment)))

(DEFUN list_call (Syntax Stream Semantics)
  (LET* ((LocalHead (GENTEMP "HeadStream"))
         (LocalTail (GENTEMP "TailStream"))
         (Test `(AND (CONSP (FIRST ,Stream))
                     (CONSP (CAAR ,Stream))))
         (HeadStream `(LIST (CAAR ,Stream) (CADR ,Stream)))
         (TailStream `(LIST (CDAR ,Stream) (CADR ,Stream)))
         (NewSyntax (APPEND (FIRST Syntax) '(! -end-of-list-)))
         (ParseTail (syntax (REST Syntax) LocalTail Semantics))
         (ParseHeadthenTail (syntax NewSyntax LocalHead ParseTail))
         (Else (CONS 'LIST *Failure*)))
        `(IF ,Test
             (LET ((,LocalHead ,HeadStream) 
                   (,LocalTail ,TailStream))
                   ,ParseHeadthenTail)
             ,Else)))       

(DEFUN terminal? (Fragment)
 (AND (NOT (CONSP Fragment)) (NOT (grammar_symbol? Fragment))
  (NOT (jump_stream? Fragment)) (NOT (rest_stream? Fragment))
  (NOT (eos? Fragment)) (NOT (close_proc? Fragment))))

(DEFUN check_stream (Syntax Stream Semantics)
  (LET ((Test `(AND (CONSP (FIRST ,Stream))
                   (EQUAL (FIRST (FIRST ,Stream))
                       (QUOTE ,(FIRST Syntax)))))
        (Action (syntax (REST Syntax)
                        `(LIST (REST (FIRST ,Stream)) (SECOND ,Stream))
                        Semantics))
        (Else (CONS 'LIST *Failure*))) 
         (LIST 'IF Test Action Else)))

(DEFUN eos? (Fragment) (EQ Fragment '!))

(DEFUN jump_stream? (Fragment) (EQ '-*- Fragment))

(DEFUN check_eos (Syntax Stream Semantics)
  (LET ((Test `(NULL (FIRST ,Stream)))
        (Action (syntax (REST Syntax)
                        `(LIST (REST (FIRST ,Stream)) (SECOND ,Stream))
                        Semantics))
        (Else (CONS 'LIST *Failure*))) 
         (LIST 'IF Test Action Else)))  

(DEFUN jump_stream (Syntax Stream Semantics)
  (LET ((Test `(CONSP (FIRST ,Stream)))
        (Action (syntax (REST Syntax)
                        `(LIST (REST (FIRST ,Stream)) (SECOND ,Stream))
                        Semantics))
        (Else (CONS 'LIST *Failure*))) 
         (LIST 'IF Test Action Else)))

(DEFUN semantics (Semantics)
 (COND ((NULL Semantics) NIL)
  ((AND (LISTP Semantics) (EQ (CAR Semantics) 'let)
    (qi_= (LIST-LENGTH Semantics) 4))
   (LIST 'LET (LIST (LIST (CADR Semantics) (semantics (CADDR Semantics))))
    (SUBST (CADR Semantics) (LIST 'QUOTE (CADR Semantics))
     (semantics (CADDDR Semantics)) :TEST 'EQUAL)))
  ((EQ 'Stream Semantics) Semantics)
  ((terminal? Semantics)
   (IF
    (OR (STRINGP Semantics) (NUMBERP Semantics) (CHARACTERP Semantics)
     (NULL Semantics))
    Semantics (LIST 'QUOTE Semantics)))
  ((grammar_symbol? Semantics) (LIST 'SECOND Semantics))
  ((jump_stream? Semantics) '(CAAR Stream))
  ((rest_stream? Semantics) '(CDAR Stream))
  ((CONSP Semantics)
   (CONS (FIRST Semantics) (MAPCAR 'semantics (REST Semantics))))))

(DEFUN rest_stream? (Semantics) (EQ Semantics '-s-))

(DEFUN <e> (Stream) Stream)

(DEFUN failure? (X) (EQ (CADR X) #\Escape))

(DEFMACRO cases (&REST X)
  (IF (NULL X)
      '*Failure*
      `(LET ((Y ,(CAR X)))
            (IF (failure? Y)
                ,(CONS 'cases (CDR X))
                Y))))

(DEFUN dump (X) (PROGV '(*dump-file*) (LIST (FORMAT NIL "~A.lsp" X)) (load X)))

(DEFUN load (File) 
 (TIME (evaluate_file_contents (MAPCAR 'macroexpand (read-file File))))
 (TERPRI) 
 'loaded)

(DEFUN macroexpand (X) X)

(DEFUN evaluate_file_contents (Contents)
 (IF (EQ *tc* 'true) (MAPCAR 'scan_for_signatures Contents))
 (MAPCAR #'(LAMBDA (X) (PROG2 (toplevel_evaluate (LIST X)) (TERPRI))) Contents)
 (IF (EQ *tc* 'true)
  (FORMAT T "typechecked in ~A inferences.~%" *logical-inferences*))
 (SETQ *tempsigs* NIL))

(SETQ *tempsigs* NIL)

(DEFUN scan_for_signatures (Expr)
 (IF (definition? Expr)
  (LET ((Type (curry_type (extract_signature (CADR Expr) (CDDR Expr)))))
   (newfuntype* (CADR Expr) Type) (PUSH (CADR Expr) *tempsigs*))))

(DEFUN definition? (Expr) (AND (CONSP Expr) (EQ (CAR Expr) 'define)))

(DEFUN extract_signature (Name Body)
 (IF (EQ (CAR Body) '{) (extract_signature* Name (CDR Body))
  (ERROR "~A lacks a type.~%" Name)))

(DEFUN extract_signature* (Name Body)
 (COND ((EQ (CAR Body) '}) NIL)
  ((CONSP Body) (CONS (CAR Body) (extract_signature* Name (CDR Body))))
  (T (ERROR "Unmatched { in ~A.~%" Name))))

(DEFUN write-to-file (Filename Output)
 (LET ((AbsFilename (FORMAT NIL "~A~A" *qi_home_directory* Filename)))
  (WITH-OPEN-FILE (OUTSTREAM AbsFilename
                               :DIRECTION :OUTPUT 
                               :IF-EXISTS :APPEND 
                               :IF-DOES-NOT-EXIST :CREATE)
    (FORMAT OUTSTREAM "~%")
    (COND ((STRINGP Output) (WRITE-STRING Output OUTSTREAM)) 
          (T (PPRINT Output OUTSTREAM)))  )
  AbsFilename))

(DEFUN delete-file (Filename) 
 (LET ((AbsFilename (FORMAT NIL "~A~A" *qi_home_directory* Filename))) 
   (IF (PROBE-FILE AbsFilename) 
        (DELETE-FILE AbsFilename)
        (warn (FORMAT NIL "~A does not exist.~%" AbsFilename))) 
   AbsFilename))

(DEFUN input () (eval (CAR (lineread))))

(DEFUN eval (V5360)
 (COND
  ((AND (CONSP V5360) (wrapper (lisp-macro? (CAR V5360))))
   (EVAL V5360))
  ((AND (CONSP V5360) (wrapper (macro? (CAR V5360))))
   (EVAL (lisp_code NIL V5360)))
  ((AND (CONSP V5360)
    (wrapper
     (and (symbol? (CAR V5360)) (empty? (arityf (CAR V5360))))))
   (APPLY (eval (CAR V5360)) (THE LIST (MAPCAR 'eval (CDR V5360)))))
  ((AND (CONSP V5360) (CONSP (CDR V5360)) (NULL (CDR (CDR V5360))))
   (apply (eval (CAR V5360)) (eval (CAR (CDR V5360)))))
  ((AND (CONSP V5360) (CONSP (CDR V5360)))
   (LET* ((V5361 (CDR V5360)))
    (eval (CONS (CONS (CAR V5360) (CONS (CAR V5361) NIL)) (CDR V5361)))))
  ((AND (CONSP V5360) (NULL (CDR V5360))) (FUNCALL (eval (CAR V5360))))
  (T V5360)))

(DEFUN lisp-macro? (V5366)
 (COND ((MEMBER V5366 '(define defcc datatype abstype structure 
                                  newfuntype synonyms theory)) 'true)
          (T (and (macro? V5366) (not (element? V5366 *sysfuncs*))))))

(DEFUN macro? (V5896)
 (THE SYMBOL
  (and (THE SYMBOL (symbol? V5896))
   (THE SYMBOL (not (THE SYMBOL (empty? (MACRO-FUNCTION V5896))))))))

(DEFMACRO input+ (Colon Type) (DECLARE (IGNORE Colon))
 (LIST 'input+help (LIST 'QUOTE Type)))

(DEFUN input+help (V3461)
 (LET ((I (CAR (lineread))))
  (IF (EQ (typechecks? NIL I (curry_type V3461)) 'false)
   (PROGN (output "~A is not of type" I)
             (FORMAT T " ~A~%Reinput: " (curry_type V3461))
              (input+help V3461))
   (eval I))))

(DEFUN read-file-as-charlist (File)
 (LET ((AbsFile (FORMAT NIL "~A~A" *qi_home_directory* File)))
  (IF (NOT (PROBE-FILE AbsFile)) (ERROR "~%~A does not exist~%" AbsFile))
  (WITH-OPEN-FILE (In AbsFile :DIRECTION :INPUT)
   (DO ((Letter T) (Letters NIL)) ((NULL Letter) (NREVERSE (CDR Letters)))
    (SETQ Letter (READ-CHAR In NIL NIL)) (PUSH Letter Letters)))))

(DEFVAR *qi_home_directory* "")

(DEFUN cd (String)
 (IF (EQUAL String "") (SETQ *qi_home_directory* String)
  (SETQ *qi_home_directory* (FORMAT NIL "~A/" String))))

(DEFUN read-file (Filename)
 (LET ((AbsFileName (FORMAT NIL "~A~A" *qi_home_directory* Filename)))
  (IF (NOT (PROBE-FILE AbsFileName)) (error "~%~A does not exist~%" AbsFileName))
  (WITH-OPEN-FILE (Stream AbsFileName :DIRECTION :INPUT)
   (read-user-input Stream (READ-CHAR Stream NIL NIL) NIL 1 (@p 0 1) (@p 0 1)
    'false 'false 'eof?))))

(DEFUN lineread ()
 (LET ((Input (read-user-input *STANDARD-INPUT* 
                     (read-user-input-stream *STANDARD-INPUT*) 
                     '(#\Space) 
                     1 
                     (@p 0 1) 
                     (@p 0 1)
  	         'false 
                     'false 
                     'endinput?)))
   (COND ((NULL Input) (lineread))
            (T (MAPCAR 'macroexpand Input)))))

(DEFUN eof? (V5494 V5495 V5496 V5497 V5498 V5499 V5500 V5501)
 (COND ((NULL V5495) 'true) 
          (T 'false)))

(DEFUN endinput? (V5556 V5557 V5558 V5559 V5560 V5561 V5562 V5563)
 (COND ((EQUAL #\^ V5557) (error "input aborted~%"))
  ((AND (EQUAL #\Newline V5557) 
          (CONSP V5558) 
          (wrapper (tuple? V5560))
          (EQ 0 (fst V5560))
          (wrapper (tuple? V5561)) 
          (EQ 0 (fst V5561))
          (EQ 'false V5562) 
          (EQ 'false V5563))
   'true)
  (T 'false)))

(DEFUN read-user-input (V5360 V5361 V5362 V5363 V5364 V5365 V5366 V5367 V5368)
 (COND
  ((wrapper
    (FUNCALL V5368 V5360 V5361 V5362 V5363 V5364 V5365 V5366 V5367))
   (expand-mlet (assemble-chars V5362 V5364 V5365 V5366 V5367)))
  ((EQUAL #\Newline V5361)
   (read-user-input V5360 (read-user-input-stream V5360) (CONS #\Newline V5362)
    (THE NUMBER (+ 1 V5363)) (incl V5363 V5364) (incl V5363 V5365) V5366 V5367 V5368))
  ((AND (EQUAL #\\ V5361) (CONSP V5362) (EQUAL #\# (CAR V5362)))
   (read-user-input V5360 (read-user-input-stream V5360) (CONS #\\ V5362) V5363
    V5364 V5365 V5366 V5367 V5368))
  ((AND (CONSP V5362) (EQUAL #\\ (CAR V5362)) (CONSP (CDR V5362))
    (EQUAL #\# (CAR (CDR V5362))))
   (read-user-input V5360 (read-user-input-stream V5360) (CONS V5361 V5362)
    V5363 V5364 V5365 V5366 V5367 V5368))
  ((EQUAL #\\ V5361)
   (read-user-input V5360 (read-user-input-stream V5360) V5362 V5363 V5364
    V5365 V5366 (THE SYMBOL (not V5367)) V5368))
  ((wrapper V5367)
   (read-user-input V5360 (read-user-input-stream V5360) V5362 V5363 V5364
    V5365 V5366 V5367 V5368))
  ((EQUAL #\" V5361)
   (read-user-input V5360 (read-user-input-stream V5360) (CONS #\" V5362) V5363
    V5364 V5365 (THE SYMBOL (not V5366)) V5367 V5368))
  ((wrapper V5366)
   (read-user-input V5360 (read-user-input-stream V5360) (CONS V5361 V5362)
    V5363 V5364 V5365 V5366 V5367 V5368))
  ((EQUAL #\[ V5361)
   (read-user-input V5360 (read-user-input-stream V5360)
    (APPEND
     (REVERSE
      (CONS #\Space
       (CONS #\(
        (CONS #\l
         (CONS #\i
          (CONS #\s (CONS #\t (CONS #\i (CONS #\t (CONS #\Space NIL))))))))))
     V5362)
    V5363 (inc V5364) V5365 V5366 V5367 V5368))
  ((EQUAL #\] V5361)
   (read-user-input V5360 (read-user-input-stream V5360) (CONS #\) V5362) V5363
    (dec V5364) V5365 V5366 V5367 V5368))
  ((EQUAL #\| V5361)
   (read-user-input V5360 (read-user-input-stream V5360)
    (CONS #\Space (CONS #\@ (CONS #\Space V5362))) V5363 V5364 V5365 V5366
    V5367 V5368))
  ((EQUAL #\( V5361)
   (read-user-input V5360 (read-user-input-stream V5360) (CONS #\( V5362) V5363
    V5364 (inc V5365) V5366 V5367 V5368))
  ((EQUAL #\) V5361)
   (read-user-input V5360 (read-user-input-stream V5360) (CONS #\) V5362) V5363
    V5364 (dec V5365) V5366 V5367 V5368))
  ((AND (EQUAL #\= V5361) (CONSP V5362)
    (wrapper (and (seperator? (CAR V5362)) (seperator? (peek V5360)))))
   (read-user-input V5360 (read-user-input-stream V5360)
    (CONS #\= (CONS #\_ (CONS #\i (CONS #\q V5362)))) V5363 V5364 V5365 V5366
    V5367 V5368))
  ((AND (EQUAL #\= V5361) (CONSP V5362) (EQUAL #\> (CAR V5362))
    (CONSP (CDR V5362))
    (wrapper
     (and (seperator? (CAR (CDR V5362))) (seperator? (peek V5360)))))
   (read-user-input V5360 (read-user-input-stream V5360)
    (CONS #\= (CONS #\> (CONS #\_ (CONS #\i (CONS #\q (CDR V5362)))))) V5363
    V5364 V5365 V5366 V5367 V5368))
  ((AND (EQUAL #\= V5361) (CONSP V5362) (EQUAL #\< (CAR V5362))
    (CONSP (CDR V5362))
    (wrapper
     (and (seperator? (CAR (CDR V5362))) (seperator? (peek V5360)))))
   (read-user-input V5360 (read-user-input-stream V5360)
    (CONS #\= (CONS #\< (CONS #\_ (CONS #\i (CONS #\q (CDR V5362)))))) V5363
    V5364 V5365 V5366 V5367 V5368))
  ((AND (EQUAL #\< V5361) (CONSP V5362)
    (wrapper (and (seperator? (CAR V5362)) (seperator? (peek V5360)))))
   (read-user-input V5360 (read-user-input-stream V5360)
    (CONS #\< (CONS #\_ (CONS #\i (CONS #\q V5362)))) V5363 V5364 V5365 V5366
    V5367 V5368))
  ((AND (EQUAL #\> V5361) (CONSP V5362)
    (wrapper (and (seperator? (CAR V5362)) (seperator? (peek V5360)))))
   (read-user-input V5360 (read-user-input-stream V5360)
    (CONS #\> (CONS #\_ (CONS #\i (CONS #\q V5362)))) V5363 V5364 V5365 V5366
    V5367 V5368))
  ((AND (EQUAL #\: V5361) (wrapper (seperator? (peek V5360))))
   (read-user-input V5360 (read-user-input-stream V5360)
    (CONS #\Space (CONS #\$ (CONS #\$ (CONS #\Space V5362)))) V5363 V5364 V5365
    V5366 V5367 V5368))
  ((EQUAL #\; V5361)
   (read-user-input V5360 (read-user-input-stream V5360)
    (CONS #\Space (CONS #\; (CONS #\Space V5362))) V5363 V5364 V5365 V5366
    V5367 V5368))
  ((EQUAL #\{ V5361)
   (read-user-input V5360 (read-user-input-stream V5360)
    (CONS #\Space (CONS #\{ (CONS #\Space V5362))) V5363 V5364 V5365 V5366
    V5367 V5368))
  ((EQUAL #\} V5361)
   (read-user-input V5360 (read-user-input-stream V5360)
    (CONS #\Space (CONS #\} (CONS #\Space V5362))) V5363 V5364 V5365 V5366
    V5367 V5368))
  ((EQUAL #\, V5361)
   (read-user-input V5360 (read-user-input-stream V5360) 
      (APPEND '(#\Space #\+ #\+ #\Space) V5362)  V5363 V5364 V5365 V5366 V5367 V5368))
  (T
   (read-user-input V5360 (read-user-input-stream V5360) (CONS V5361 V5362)
    V5363 V5364 V5365 V5366 V5367 V5368))))

(DEFUN expand-mlet (V33)
 (COND
  ((AND (CONSP V33) (EQ '@p (CAR V33)) (CONSP (CDR V33))
    (CONSP (CDR (CDR V33))) (CONSP (CDR (CDR (CDR V33)))))
   (LET* ((V34 (CDR V33)))
    (expand-mlet (CONS '@p (CONS (CAR V34) (CONS (CONS '@p (CDR V34)) NIL))))))
  ((AND (CONSP V33) (EQ 'mlet (CAR V33)) (CONSP (CDR V33))
    (CONSP (CDR (CDR V33))) (CONSP (CDR (CDR (CDR V33))))
    (CONSP (CDR (CDR (CDR (CDR V33))))))
   (LET* ((V35 (CDR V33)) (V36 (CDR V35)))
    (expand-mlet
     (CONS 'let
      (CONS (CAR V35) (CONS (CAR V36) (CONS (CONS 'mlet (CDR V36)) NIL)))))))
  ((AND (CONSP V33) (EQ 'mlet (CAR V33)) (CONSP (CDR V33))
    (CONSP (CDR (CDR V33))) (CONSP (CDR (CDR (CDR V33))))
    (NULL (CDR (CDR (CDR (CDR V33))))))
   (expand-mlet (CONS 'let (CDR V33))))
  ((CONSP V33) (map-dotted-pair 'expand-mlet V33)) 
  (T V33)))

(DEFUN map-dotted-pair (V96 V97)
 (COND ((NULL V97) NIL)
  ((CONSP V97) (CONS (apply V96 (CAR V97)) (map-dotted-pair V96 (CDR V97))))
  (T (apply V96 V97))))

(DEFUN read-user-input-stream (Stream)
  (LET ((CHAR (READ-CHAR Stream NIL NIL)))
       (PUSH CHAR *read-user-input-characters*)
       CHAR))

(DEFUN peek (Stream) (PEEK-CHAR NIL Stream NIL))

(DEFUN seperator? (V5940)
 (THE SYMBOL
  (element? V5940
   (CONS #\Space
    (CONS #\Newline
     (CONS #\( (CONS #\) (CONS #\] (CONS #\[ (CONS #\, (CONS NIL NIL)))))))))))

(DEFUN inc (V5942)
 (COND
  ((wrapper (tuple? V5942))
   (@p (THE NUMBER (+ 1 (fst V5942))) (snd V5942)))
  (T (implementation_error 'inc))))

(DEFUN dec (V5947)
 (COND
  ((wrapper (tuple? V5947))
   (@p (THE NUMBER (- (fst V5947) 1)) (snd V5947)))
  (T (implementation_error 'dec))))

(DEFUN incl (V5372 V5373)
 (COND
  ((AND (wrapper (tuple? V5373)) (EQ 0 (fst V5373)))
   (@p 0 (+ V5372 1)))
  (T V5373)))

(DEFUN assemble-chars (V4110 V4111 V4112 V4113 V4114)
 (COND
  ((AND (wrapper (tuple? V4111)) (EQ 0 (fst V4111))
    (wrapper (tuple? V4112)) (EQ 0 (fst V4112)) (EQ 'false V4113)
    (EQ 'false V4114))
   (cons_form
    (READ-FROM-STRING (COERCE (CONS #\( (REVERSE (CONS #\) V4110))) 'STRING))))
  ((EQ 'true V4113) (ERROR (FORMAT NIL "Unmatched ~C" #\")))
  ((EQ 'true V4114) (ERROR (FORMAT NIL "Unmatched ~C" #\\)))
  ((AND (wrapper (tuple? V4111)) (wrapper (not (qi_= (fst V4111) 0))))
   (IF (EQ 'true (THE SYMBOL (qi_> (fst V4111) 0)))
    (ERROR "~%Too many [s or too few ]s; last balanced at line ~A" (snd V4111))
    (ERROR "~%Too many ]s or too few [s; last balanced at line ~A" (snd V4111))))
  ((AND (wrapper (tuple? V4112)) (wrapper (not (qi_= (fst V4112) 0))))
   (IF (EQ 'true (THE SYMBOL (qi_> (fst V4112) 0)))
    (ERROR "~%Too many (s or too few )s; last balanced at line ~A" (snd V4112))
    (ERROR "~%Too many )s or too few (s; last balanced at line ~A" (snd V4112))))
  (T (implementation_error 'assemble-chars))))

(DEFUN cons_form (V6003)
 (COND ((AND (CONSP V6003) (EQ 'listit (CAR V6003)) (NULL (CDR V6003))) NIL)
  ((wrapper (atsign? V6003)) (error "Improper use of |~%"))
  ((AND (CONSP V6003) (EQ 'listit (CAR V6003)) (CONSP (CDR V6003))
    (CONSP (CDR (CDR V6003))) (NULL (CDR (CDR (CDR V6003))))
    (wrapper (atsign? (CAR (CDR V6003)))))
   (cons_form (CAR (CDR (CDR V6003)))))
  ((AND (CONSP V6003) (EQ 'listit (CAR V6003)) (CONSP (CDR V6003)))
   (LET* ((V6004 (CDR V6003)))
    (CONS 'cons
     (CONS (cons_form (CAR V6004))
      (CONS (cons_form (CONS 'listit (CDR V6004))) NIL)))))
  ((CONSP V6003) (MAPCAR 'cons_form V6003)) (T V6003)))

(DEFUN atsign? (X)
  (IF (EQ X '@) 
       'true 
       'false))  

(DEFUN error (&REST ARGS) 
   (FORMAT T "error: ")
   (FORMAT T (sanitise-string (APPLY 'FORMAT (CONS NIL ARGS))))
   (ERROR ""))

(DEFUN make-string (&REST ARGS) 
  (sanitise-string (APPLY 'FORMAT (CONS NIL ARGS))))

(DEFUN output (&REST ARGS) 
  (FORMAT T (sanitise-string (APPLY 'FORMAT (CONS NIL ARGS))))
  "done")

(DEFUN sanitise-string (String)
  (sanitise-string-help (COERCE String 'LIST) NIL 'false))

(DEFUN sanitise-string-help (V5362 V5363 V5364)
 (COND ((NULL V5362) (COERCE (REVERSE V5363) 'STRING))
  ((AND (CONSP V5362) (EQUAL #\" (CAR V5362)))
   (sanitise-string-help (CDR V5362) (CONS #\" V5363)
    (THE SYMBOL (not V5364))))
  ((AND (CONSP V5362) (EQ 'true V5364))
   (sanitise-string-help (CDR V5362) (CONS (CAR V5362) V5363) 'true))
  ((AND (CONSP V5362) (EQUAL #\' (CAR V5362)) (CONSP (CDR V5362))
    (EQUAL #\) (CAR (CDR V5362))))
   (sanitise-string-help (CDR (CDR V5362)) (CONS #\) V5363) V5364))
  ((AND (CONSP V5362) (EQUAL #\( (CAR V5362)) (CONSP (CDR V5362))
    (EQUAL #\@ (CAR (CDR V5362))) (CONSP (CDR (CDR V5362)))
    (EQUAL #\p (CAR (CDR (CDR V5362)))) (CONSP (CDR (CDR (CDR V5362))))
    (EQUAL #\Space (CAR (CDR (CDR (CDR V5362))))))
   (sanitise-string-help
    (find_matching_paren (CDR (CDR (CDR (CDR V5362)))) 1 NIL V5364)
    (CONS #\Space (CONS #\p (CONS #\@ (CONS #\( V5363)))) V5364))
  ((AND (CONSP V5362) (EQUAL #\# (CAR V5362)) (CONSP (CDR V5362))
    (EQUAL #\\ (CAR (CDR V5362))) (CONSP (CDR (CDR V5362))))
   (LET* ((V5365 (CDR V5362)) (V5366 (CDR V5365)))
    (sanitise-string-help (CDR V5366)
     (CONS (CAR V5366) (CONS #\\ (CONS #\# V5363))) V5364)))
  ((AND (CONSP V5362) (EQUAL #\( (CAR V5362)))
   (sanitise-string-help (CDR V5362) (CONS #\[ V5363) V5364))
  ((AND (CONSP V5362) (EQUAL #\) (CAR V5362)))
   (sanitise-string-help (CDR V5362) (CONS #\] V5363) V5364))
  ((AND (CONSP V5362) (EQUAL #\q (CAR V5362)) (CONSP (CDR V5362))
    (EQUAL #\i (CAR (CDR V5362))) (CONSP (CDR (CDR V5362)))
    (EQUAL #\_ (CAR (CDR (CDR V5362)))) (CONSP (CDR (CDR (CDR V5362))))
    (EQUAL #\> (CAR (CDR (CDR (CDR V5362))))))
   (sanitise-string-help (CDR (CDR (CDR (CDR V5362)))) (CONS #\> V5363) V5364))
  ((AND (CONSP V5362) (EQUAL #\q (CAR V5362)) (CONSP (CDR V5362))
    (EQUAL #\i (CAR (CDR V5362))) (CONSP (CDR (CDR V5362)))
    (EQUAL #\_ (CAR (CDR (CDR V5362)))) (CONSP (CDR (CDR (CDR V5362))))
    (EQUAL #\< (CAR (CDR (CDR (CDR V5362))))))
   (sanitise-string-help (CDR (CDR (CDR (CDR V5362)))) (CONS #\< V5363) V5364))
  ((AND (CONSP V5362) (EQUAL #\q (CAR V5362)) (CONSP (CDR V5362))
    (EQUAL #\i (CAR (CDR V5362))) (CONSP (CDR (CDR V5362)))
    (EQUAL #\_ (CAR (CDR (CDR V5362)))) (CONSP (CDR (CDR (CDR V5362))))
    (EQUAL #\> (CAR (CDR (CDR (CDR V5362)))))
    (CONSP (CDR (CDR (CDR (CDR V5362)))))
    (EQUAL #\= (CAR (CDR (CDR (CDR (CDR V5362)))))))
   (sanitise-string-help (CDR (CDR (CDR (CDR (CDR V5362)))))
    (CONS #\= (CONS #\> V5363)) V5364))
  ((AND (CONSP V5362) (EQUAL #\q (CAR V5362)) (CONSP (CDR V5362))
    (EQUAL #\i (CAR (CDR V5362))) (CONSP (CDR (CDR V5362)))
    (EQUAL #\_ (CAR (CDR (CDR V5362)))) (CONSP (CDR (CDR (CDR V5362))))
    (EQUAL #\< (CAR (CDR (CDR (CDR V5362)))))
    (CONSP (CDR (CDR (CDR (CDR V5362)))))
    (EQUAL #\= (CAR (CDR (CDR (CDR (CDR V5362)))))))
   (sanitise-string-help (CDR (CDR (CDR (CDR (CDR V5362)))))
    (CONS #\= (CONS #\< V5363)) V5364))
  ((AND (CONSP V5362) (EQUAL #\q (CAR V5362)) (CONSP (CDR V5362))
    (EQUAL #\i (CAR (CDR V5362))) (CONSP (CDR (CDR V5362)))
    (EQUAL #\_ (CAR (CDR (CDR V5362)))) (CONSP (CDR (CDR (CDR V5362))))
    (EQUAL #\= (CAR (CDR (CDR (CDR V5362))))))
   (sanitise-string-help (CDR (CDR (CDR (CDR V5362)))) (CONS #\= V5363) V5364))
  ((AND (CONSP V5362) (EQUAL #\N (CAR V5362)) (CONSP (CDR V5362))
    (EQUAL #\I (CAR (CDR V5362))) (CONSP (CDR (CDR V5362)))
    (EQUAL #\L (CAR (CDR (CDR V5362))))
    (wrapper (isolated_nil? (CDR (CDR (CDR V5362))) V5363)))
   (sanitise-string-help (CDR (CDR (CDR V5362))) (CONS #\] (CONS #\[ V5363))
    V5364))
  ((AND (CONSP V5362) (EQUAL #\~ (CAR V5362)))
   (sanitise-string-help (CDR V5362) (CONS #\~ (CONS #\~ V5363)) V5364))
  ((CONSP V5362)
   (sanitise-string-help (CDR V5362) (CONS (CAR V5362) V5363) V5364))
  (T (implementation_error 'sanitise-string-help))))

(DEFUN find_matching_paren (V5389 V5390 V5391 V5392)
 (COND
  ((AND (CONSP V5389) (EQUAL #\" (CAR V5389)))
   (find_matching_paren (CDR V5389) V5390 (CONS #\" V5391)
    (THE SYMBOL (not V5392))))
  ((AND (CONSP V5389) (EQ 'true V5392))
   (find_matching_paren (CDR V5389) V5390 (CONS (CAR V5389) V5391) 'true))
  ((AND (CONSP V5389) (EQUAL #\) (CAR V5389)) (EQ 1 V5390))
   (APPEND (REVERSE (CONS #\) (CONS #\' V5391))) (CDR V5389)))
  ((AND (CONSP V5389) (EQUAL #\) (CAR V5389)))
   (find_matching_paren (CDR V5389) (THE NUMBER (- V5390 1)) (CONS #\) V5391)
    V5392))
  ((AND (CONSP V5389) (EQUAL #\( (CAR V5389)))
   (find_matching_paren (CDR V5389) (THE NUMBER (+ V5390 1)) (CONS #\( V5391)
    V5392))
  ((CONSP V5389)
   (find_matching_paren (CDR V5389) V5390 (CONS (CAR V5389) V5391) V5392))
  (T (implementation_error 'find_matching_paren))))

(DEFUN isolated_nil? (V5374 V5375)
 (COND ((AND (NULL V5374) (NULL V5375)) 'true)
  ((AND (NULL V5374) (CONSP V5375))
   (element? (CAR V5375) (LIST #\Space #\Newline #\( #\) #\] #\[)))
  ((AND (CONSP V5374) (NULL V5375))
    (element? (CAR V5374) (LIST #\Space #\Newline #\( #\) #\] #\[)))
  ((AND (CONSP V5374) (CONSP V5375))
   (and
      (element? (CAR V5374)  (LIST #\Space #\Newline #\( #\) #\] #\[))
      (element? (CAR V5375)  (LIST #\Space #\Newline #\( #\) #\] #\[))))
  (T (implementation_error 'isolated_nil?))))

(DEFUN print (V6008) (output "~S" V6008) V6008)

(DEFUN outputrb (&REST ARGS) 
(FORMAT T 
  (SUBSTITUTE #\( #\[ (SUBSTITUTE #\) #\] (sanitise-string (APPLY 'FORMAT (CONS NIL ARGS))))))
  "done")

(DEFUN number? (X) (IF (NUMBERP X) 'true 'false))

(DEFUN string? (X) (IF (STRINGP X) 'true 'false))

(DEFUN character? (X) (IF (CHARACTERP X) 'true 'false))

(DEFUN boolean? (X) (IF (MEMBER X '(true false)) 'true 'false))

(DEFUN integer? (X) (IF (INTEGERP X) 'true 'false))

(DEFUN complex? (X) (IF (COMPLEXP X) 'true 'false))

(DEFUN float? (X) (IF (FLOATP X) 'true 'false))

(DEFUN real? (X) (IF (REALP X) 'true 'false))

(DEFUN rational? (X) (IF (RATIONALP X) 'true 'false))

(DEFUN sqrt (X) (SQRT X))

(DEFUN random (X) (RANDOM X))

(DEFUN round (X) (ROUND X))

(DEFUN congruent? (X Y) (IF (EQUALP X Y) 'true 'false))

(DEFUN qi_= (X Y) (IF (ABSEQUAL X Y) 'true 'false))

(DEFUN == (X Y) (qi_= X Y))

(DEFUN qi_> (X Y) (IF (> X Y) 'true 'false))

(DEFUN qi_< (X Y) (IF (< X Y) 'true 'false))

(DEFUN qi_>= (X Y) (IF (>= X Y) 'true 'false))

(DEFUN qi_<= (X Y) (IF (<= X Y) 'true 'false))

(DEFUN if-without-checking (String) 
  (IF (EQ *tc* 'false)
       (ERROR String)
       'ok))

(DEFUN if-with-checking (String) 
  (IF (EQ *tc* 'true)
       (ERROR String)
       'ok))

(DEFMACRO if (X Y Z)
 `(LET ((*C* ,X))
   (COND ((EQ *C* 'true) ,Y) ((EQ *C* 'false) ,Z)
    (T (ERROR "~S is not a boolean~%" *C*)))))

(DEFMACRO and (X Y) `(if ,X (if ,Y 'true 'false) 'false))

(DEFMACRO or (X Y) `(if ,X 'true (if ,Y 'true 'false)))

(DEFUN not (X)
 (COND ((EQ X 'true) 'false) ((EQ X 'false) 'true)
  (T (ERROR "~S is not a boolean~%" X))))

(DEFUN element? (x y) (IF (MEMBER x y :TEST 'ABSEQUAL) 'true 'false))

(DEFUN subst (X Y Z) (SUBST X Y Z :TEST 'ABSEQUAL))

(DEFUN remove (x y) (REMOVE x y :TEST 'ABSEQUAL))

(DEFUN difference (x y) (SET-DIFFERENCE x y :TEST 'ABSEQUAL))

(DEFUN assoc (x y) (ASSOC x y :TEST 'ABSEQUAL))

(DEFMACRO let (VAR VAL EXPR) (LIST 'LET (LIST (LIST VAR VAL)) EXPR))

(DEFMACRO list (&REST X) (CONS 'LIST X))

(DEFUN y-or-n? (X) (IF (Y-OR-N-P X) 'true 'false))

(DEFUN empty? (x) (IF (NULL x) 'true 'false))

(DEFUN value (X) (SYMBOL-VALUE X))

(DEFUN length (X) (LIST-LENGTH X))

(DEFUN nth (N L) 
  (IF (= N 1) (IF (NULL L) (error "nth expects a longer list.~%") (CAR L))
      (nth (1- N) (CDR L))))

(DEFUN concat (X Y) (READ-FROM-STRING (FORMAT NIL "~A~A" X Y)))

(DEFUN append (X Y) (APPEND X Y))

(DEFUN reverse (X) (REVERSE X))

(DEFUN set (X Y) (SET X Y))

(DEFUN cons (X Y) (CONS X Y))

(DEFUN @c (X Y) (CONS X Y))

(DEFUN cons? (X) (IF (CONSP X) 'true 'false))

(DEFMACRO time (X) (LIST 'TIME X))

(DEFUN gensym (X) (GENTEMP X))

(DEFUN implementation_error (Func)
 (ERROR "Qi implementation error in ~A: report to dr.mtarver@ukonline.co.uk~%" Func))

(DEFUN explode (X) (COERCE (FORMAT NIL "~S" X) 'LIST))

(DEFUN head (X)
 (IF (CONSP X) (CAR X) (ERROR "head expects a non-empty list.~% ")))

(DEFUN tail (X)
 (IF (CONSP X) (CDR X) (ERROR "tail expects a non-empty list.~% ")))

(DEFUN save ()
  (SETQ *history* NIL)
  #+CLISP
  (EXT:SAVEINITMEM)
  #+CMU
  (EXT:SAVE-LISP "Qi.core" :INIT-FUNCTION 'qi::qi :PRINT-HERALD NIL)
  #-(OR CLISP CMU)
  (ERROR "Unknown platform to Qi: ~A" (LISP-IMPLEMENTATION-TYPE)))

(DEFUN quit ()
  #+CLISP
  (EXT:EXIT)
  #+CMU
  (EXT:QUIT)
  #-(OR CLISP CMU)
  (ERROR "Unknown platform to Qi: ~A" (LISP-IMPLEMENTATION-TYPE)))

(DEFUN read-char (X)
  (DECLARE (IGNORE X))
   (READ-CHAR))

(DEFUN read-chars-as-stringlist (V164 V165)
 (read-chars-as-stringlist* V164 NIL NIL V165))

(DEFUN read-chars-as-stringlist* (V192 V193 V194 V195)
 (COND ((NULL V192) (REVERSE (CONS (COERCE V193 'STRING) V194)))
  ((CONSP V192)
   (LET* ((V196 (CAR V192)) (V197 (CDR V192)))
    (IF (EQ 'true (apply V195 V196))
     (IF (EQ 'true (THE SYMBOL (empty? V193)))
      (read-chars-as-stringlist* V197 NIL V194 V195)
      (read-chars-as-stringlist* V197 NIL
       (CONS (COERCE (REVERSE V193) 'STRING) V194) V195))
     (read-chars-as-stringlist* V197 (CONS V196 V193) V194 V195))))
  (T (implementation_error 'read-chars-as-stringlist*))))

(DEFUN make-array (dims) (MAKE-ARRAY dims :INITIAL-ELEMENT #\Escape))

(DEFUN get-array (array dims default)
 (LET ((array_element (APPLY #'AREF (CONS array dims))))
  (IF (EQ array_element #\Escape) default array_element)))

(DEFUN put-array (array dims value)
 (SETF (APPLY #'AREF (CONS array dims)) value))

(DEFUN put-prop (Ob Pointer Value) (SETF (GET Ob Pointer) Value))

(DEFUN get-prop (Ob Pointer Default) (GET Ob Pointer Default))

(DEFUN typechecks? (V1 V2 V3)
 (COND ((AND (CONSP V2) (wrapper (exempted? (CAR V2)))) 'unit)
 (T (LET ((Y (cons->@c (curry (list->cons V2)))))
  	(LET ((A (normalise_type V3)))
  	 (LET ((Context (normalise_type_env V1)))
   	 (LET
    	 ((ValidType
     	  (typecheck* (CONS Y (CONS '$$ (CONS A NIL))) Context
       	 #'(LAMBDA NIL (return* A T)))))
     	(IF (NULL ValidType) 'false ValidType))))))))

(DEFUN exempted? (V)
  (element? V *exempted*))

(SETQ *exempted* '(defcc datatype theory structure synonyms quit abstype defprolog))

(DEFUN normalise_type_env (V6041)
 (COND ((NULL V6041) NIL)
  ((AND (CONSP V6041) (CONSP (CDR V6041)) (EQ '$$ (CAR (CDR V6041)))
    (CONSP (CDR (CDR V6041))) (NULL (CDR (CDR (CDR V6041)))))
   (CONS (CAR V6041)
    (CONS '$$ (CONS (normalise_type (CAR (CDR (CDR V6041)))) NIL))))
  ((CONSP V6041) (MAPCAR 'normalise_type_env V6041)) (T V6041)))

(DEFUN list->cons (V6043)
 (COND ((AND (CONSP V6043) (EQ 'list (CAR V6043)) (NULL (CDR V6043))) NIL)
  ((AND (CONSP V6043) (EQ 'list (CAR V6043)) (CONSP (CDR V6043)))
   (LET* ((V6044 (CDR V6043)))
    (CONS 'cons
     (CONS (list->cons (CAR V6044))
      (CONS (list->cons (CONS 'list (CDR V6044))) NIL)))))
  ((CONSP V6043) (MAPCAR 'list->cons V6043)) 
  (T V6043)))

(DEFUN to-goals (V3377 V3378) (CONS V3377 (CONS V3378 (CONS NIL NIL))))

(DEFUN from-goals (V4484)
 (COND
  ((AND (CONSP V4484) (CONSP (CDR V4484))) (@p (CAR V4484) (CAR (CDR V4484))))
  (T (implementation_error 'from-goals))))

(DEFUN notes-in (V6055)
 (COND ((AND (CONSP V6055) (CONSP (CDR V6055))) (CAR (CDR V6055)))
  (T (implementation_error 'notes-in))))

(DEFUN normalise_type (V6056)
 (LET ((NewType (normalise_type* V6056)))
  (IF (EQ 'true (qi_= NewType V6056)) V6056 (normalise_type NewType))))

(DEFUN normalise_type* (V169)
 (COND
  ((AND (CONSP V169) (EQ 'cons (CAR V169)) (CONSP (CDR V169))
    (CONSP (CDR (CDR V169))) (NULL (CDR (CDR (CDR V169)))))
   (LET* ((V170 (CDR V169)))
    (CONS 'list
     (CONS (normalise_type* (CAR V170)) (normalise_type* (CAR (CDR V170)))))))
  ((CONSP V169) (THE LIST (map 'normalise_type* V169)))
  (T
   (LET ((Synonym (assoc V169 *synonyms*)))
    (IF (EQ 'true (THE SYMBOL (empty? Synonym))) V169 (CADR Synonym))))))

(SETQ *synonyms* NIL)
(SETQ *allsynonyms* NIL)

(DEFMACRO synonyms (&REST X) (LIST 'synonym-help (LIST 'QUOTE X)))

(DEFUN synonym-help (V5368)
 (COND ((NULL V5368) 'compiled)
  ((AND (CONSP V5368) (CONSP (CDR V5368))
    (wrapper (even? (CDR (CDR V5368)))))
   (LET* ((V5369 (CDR V5368)))
     (compile-synonym (CAR V5368) (CAR V5369) *synonyms* *allsynonyms*)
     (synonym-help (CDR V5369))))
  (T (error "Odd number of elements in synonyms declaration.~%"))))

(DEFUN even? (V5378)
 (COND ((NULL V5378) 'true)
  ((AND (CONSP V5378) (CONSP (CDR V5378))) (even? (CDR (CDR V5378))))
  (T 'false)))

(DEFUN compile-synonym (V5379 V5380 V5381 V5382)
 (IF (EQ 'true (test-valid-synonym? V5379))
      (PROGN (SETQ *synonyms* (CONS (CONS V5379 (CONS V5380 NIL)) V5381))
                (SETQ *allsynonyms* (CONS V5379 V5382)))
      (FORMAT 'T "Skipping ~A with ~A; invalid synonym.~%" V5379 V5380)))

(DEFUN test-valid-synonym? (V5383)
 (COND ((wrapper (not (symbol? V5383))) 'false)
  ((wrapper (variable? V5383)) 'false)
  ((wrapper (system_type? V5383)) 'false)
  (T
   (IF (EQ 'true (predefined_type? V5383))
    (do
     (warn
      (FORMAT NIL
       "~A is already given a synonym; overwriting old definition.~%" V5383))
     'true)
    'true))))

(DEFUN predefined_type? (V5384) (THE SYMBOL (element? V5384 *allsynonyms*)))

(DEFUN system_type? (V5386)
 (THE SYMBOL
  (element? V5386
   (CONS 'list
    (CONS 'array
     (CONS '*
      (CONS '-->
       (CONS 'goals
        (CONS 'symbol
         (CONS 'unit
          (CONS 'character
           (CONS 'string (CONS 'number (CONS 'boolean NIL))))))))))))))

(DEFUN theory-size (V6066)
 (THE NUMBER (/ (LIST-LENGTH (SYMBOL-PLIST V6066)) 2)))

(DEFUN maxinferences (V6067) (SETQ *maxinferences* V6067))

(SETQ *maxinferences* 1000000)

(DEFUN get-rule (V6075 V6076) (get-prop V6075 V6076 'identity))

(DEFUN identity (V6077) V6077)

(DEFUN curry (V6078)
 (COND
  ((AND (CONSP V6078) (EQ 'input+ (CAR V6078)) (CONSP (CDR V6078))
    (EQ '$$ (CAR (CDR V6078))) (CONSP (CDR (CDR V6078)))
    (NULL (CDR (CDR (CDR V6078)))))
   V6078)
  ((AND (CONSP V6078) (wrapper (special_form? (CAR V6078))))
   (CONS (CAR V6078) (MAPCAR 'curry (CDR V6078))))
  ((AND (CONSP V6078) (CONSP (CDR V6078)) (NULL (CDR (CDR V6078))))
   (CONS (curry (CAR V6078)) (CONS (curry (CAR (CDR V6078))) NIL)))
  ((AND (CONSP V6078) (CONSP (CDR V6078)))
   (LET* ((V6079 (CDR V6078)))
    (curry (CONS (CONS (CAR V6078) (CONS (CAR V6079) NIL)) (CDR V6079)))))
  (T V6078)))

(DEFUN map_base_type (V6084)
 (COND ((wrapper (symbol? V6084)) 'symbol)
  ((wrapper (string? V6084)) 'string)
  ((wrapper (number? V6084)) 'number)
  ((wrapper (character? V6084)) 'character)
  ((wrapper (boolean? V6084)) 'boolean)
  (T NIL)))

(DEFUN symbol? (X)
  (IF (AND (SYMBOLP X) 
	  (NOT (MEMBER X '(true false NIL)))
              (NOT (place_holder? X)))
      'true
      'false))

(DEFUN place_holder? (X)
  (LET ((NAMESTRING (SYMBOL-NAME X))) 
         (AND (> (LENGTH (THE STRING NAMESTRING)) 2)
	    (CHAR-EQUAL #\& (CHAR NAMESTRING 0))
                (CHAR-EQUAL #\& (CHAR NAMESTRING 1)))))  

(DEFUN special_form? (V6085) (THE SYMBOL (element? V6085 *special_forms*)))

(DEFUN specialise (V6087) (do (PUSH V6087 *special_forms*) V6087))

(DEFUN unspecialise (V6088)
 (do (SETQ *special_forms* (REMOVE V6088 *special_forms*)) V6088))

(DEFVAR *special_forms*
 '(cons let /. output error qi_= newfuntype if set do where cases @p list make-string @c))

(DEFUN occurs? (V6298 V6299)
 (COND ((wrapper (qi_= V6298 V6299)) 'true)
  ((CONSP V6299)
   (THE SYMBOL (or (occurs? V6298 (CAR V6299)) (occurs? V6298 (CDR V6299)))))
  (T 'false)))

(DEFUN qi ()
 (PROG ()
    (licence)
    (credit) 
    LOOP    (initialise_environment)
                    (prompt) 
                    (FORCE-OUTPUT)
                    (read-evaluate-print) 
    (GO LOOP)))

(DEFUN read-evaluate-print ()
   (HANDLER-CASE 
      (LET ((Input (lineread)))            
            (record_input_on_history)
             (toplevel Input))
             (ERROR (condition) (PRINC condition))))  

(DEFUN licence ()
  (COND ((NOT *licence*)
   (FORMAT T "~%Qi is distributed without warranty under the terms of the GPL licence.~%")
   (FORMAT T "You may not change or remove any copyright notices attached to this software.~%~%")
   (FORMAT T "1. I agree to the terms of the GPL licence.~%")
   (FORMAT T "2. I do not agree to the terms of the GPL licence.~%")
   (FORMAT T "3. I want to read the GPL licence and come back to this.~%~%")
   (FORMAT T "Choose: ")
   (LET ((Answer (READ)))
       (COND ((EQ Answer 1) (agree))
                ((EQ Answer 2) (disagree))
                ((EQ Answer 3) (lookat-GPL))
                (T (FORMAT T "~%~%This is not a valid answer.~%~%")  (licence))) )  )))

(SETQ *licence* NIL)

(DEFUN agree ()
   (FORMAT T "~%Thankyou.  Qi will be initialised to accept your agreement.~%")
   (FORMAT T "You will not be queried again.~%")
   (SETQ *licence* T)
   (save))
 
(DEFUN disagree ()
   (quit))

(DEFUN  lookat-GPL ()
   (ED "GPL.txt")
   (licence)) 
(DEFUN record_input_on_history ()
   (PUSH (NREVERSE *read-user-input-characters*) *history*))    

(DEFUN initialise_environment ()
       (SETQ *call* 0)
       (SETQ *logical-inferences* 0)
       (MAPC 'destroy *tempsigs*)
       (SETQ *tempsigs* NIL)
       (SETQ *read-user-input-characters* NIL))

(DEFUN version (String) (SETQ *version* String))

(SETQ *version* "version 7.2")

(DEFUN credit ()
  (FORMAT  T "~%Qi 2007, Copyright (C) 2001-2007 Mark Tarver~%")
  (FORMAT T  "www.lambdassociates.org~%")
  (FORMAT  T "~A~%" *version*))

(DEFVAR *tc* 'false)

(DEFVAR *history* NIL)

(DEFUN tc (V6583)
 (COND ((EQ '+ V6583) (SETQ *tc* 'true)) 
          ((EQ '- V6583) (SETQ *tc* 'false))
          (T (error "tc expects a + or -"))))

(DEFUN prompt NIL
 (IF (EQ 'true *tc*) (FORMAT  T "~%~%(~A+) " (THE NUMBER (length *history*)))
  (FORMAT  T "~%~%(~A-) " (THE NUMBER (length *history*)))))

(DEFUN toplevel (V5423)
 (COND ((wrapper (history-call? V5423)) (toplevel (call_history V5423)))
  (T (toplevel_evaluate V5423))))

(DEFUN history-call? (V5428)
 (COND
  ((AND (CONSP V5428) (NULL (CDR V5428)) (wrapper (symbol? (CAR V5428))))
   (LET ((Chars (explode (CAR V5428))))
    (THE SYMBOL (element? (head Chars) (CONS #\! (CONS #\% NIL))))))
  (T 'false)))

(DEFUN call_history (V5440)
 (COND
  ((AND (CONSP V5440) (NULL (CDR V5440)))
   (POP *history*)
   (FUNCALL (findcallfunc (THE LIST (explode (CAR V5440)))) *history*))
  (T (implementation_error 'call_history))))

(DEFUN findcallfunc (V5360)
 (COND
  ((AND (CONSP V5360) (EQUAL #\! (CAR V5360)) (CONSP (CDR V5360))
    (EQUAL #\! (CAR (CDR V5360))) (NULL (CDR (CDR V5360))))
   #'(LAMBDA (History)
      (call_by_number (1- (THE NUMBER (length History))) 0 (REVERSE History))))
  ((AND (CONSP V5360) (EQUAL #\% (CAR V5360)) (NULL (CDR V5360)))
   #'(LAMBDA (History) (print_all (REVERSE History) 0)))
  ((AND (CONSP V5360) (EQUAL #\! (CAR V5360)))
   (LET* ((V5361 (CDR V5360)))
    (LET ((Index (READ-FROM-STRING (COERCE V5361 'STRING))))
     (IF (EQ 'true (THE SYMBOL (integer? Index)))
      #'(LAMBDA (History) (call_by_number Index 0 (REVERSE History)))
      #'(LAMBDA (History)
         (call_by_name (1- (THE NUMBER (length History))) V5361 History))))))
  ((AND (CONSP V5360) (EQUAL #\% (CAR V5360)))
   (LET* ((V5362 (CDR V5360)))
    (LET ((Index (READ-FROM-STRING (COERCE V5362 'STRING))))
     (IF (EQ 'true (THE SYMBOL (integer? Index)))
      #'(LAMBDA (History) (print_by_number Index 0 (REVERSE History)))
      #'(LAMBDA (History) (print_by_name 0 V5362 (REVERSE History)))))))
  (T (implementation_error 'findcallfunc))))

(DEFUN print_by_name (V5381 V5382 V5383)
 (COND
  ((AND (CONSP V5383) (wrapper (prefix? V5382 (tail (CAR V5383)))))
   (do (print_command V5381 (CAR V5383))
    (print_by_name (THE NUMBER (+ V5381 1)) V5382 (CDR V5383))))
  ((CONSP V5383) (print_by_name (THE NUMBER (+ V5381 1)) V5382 (CDR V5383)))
  (T (ERROR ""))))

(DEFUN print_all (V5450 V5451)
 (COND ((NULL V5450) (ERROR ""))
  ((CONSP V5450)
    (print_command V5451 (CAR V5450))
    (print_all (CDR V5450) (THE NUMBER (+ V5451 1))))
  (T (implementation_error 'print_all))))

(DEFUN print_command (V5452 V5453)
 (FORMAT T "~%~A. ~A" V5452 (COERCE (BUTLAST V5453) 'STRING)))

(DEFUN print_by_number (V5471 V5472 V5473)
 (COND
  ((AND (CONSP V5473) (wrapper (qi_= V5471 V5472)))
   (do (print_command V5472 (CAR V5473)) (ERROR "")))
  ((CONSP V5473) (print_by_number V5471 (THE NUMBER (+ V5472 1)) (CDR V5473)))
  (T (error "number out of range~%"))))

(DEFUN call_by_number (V5504 V5505 V5506)
 (COND
  ((AND (CONSP V5506) (wrapper (qi_= V5504 V5505)))
   (print_command V5504 (CAR V5506))
   (TERPRI)
   (return_past_command (CAR V5506)))
  ((CONSP V5506) (call_by_number V5504 (THE NUMBER (+ V5505 1)) (CDR V5506)))
  (T (error "number out of range~%"))))

(DEFUN call_by_name (N V5518 V5519)
 (COND
  ((AND (CONSP V5519) (wrapper (prefix? V5518 (tail (CAR V5519)))))
   (print_command N (CAR V5519))
   (TERPRI)
   (return_past_command (CAR V5519)))
  ((CONSP V5519) (call_by_name (1- N) V5518 (CDR V5519)))
  (T (error "input not recorded~%"))))

(DEFUN return_past_command (Chars)
   (PUSH Chars *history*)
   (LET ((Command
            (MAPCAR 'macroexpand
               (LET ((Stream (MAKE-STRING-INPUT-STREAM (COERCE Chars 'STRING))))
                  (read-user-input Stream 
                                        (READ-CHAR Stream NIL NIL) 
                                        '(#\Space) 
                                        1 
                                        (@p 0 1) 
                                        (@p 0 1) 
                                        'false 
                                        'false 
                                        'eof?)))))
    Command))

(DEFUN toplevel_evaluate (V5366)
 (COND
  ((AND (CONSP V5366) (CONSP (CAR V5366)) (EQ 'define (CAR (CAR V5366)))
    (CONSP (CDR (CAR V5366))))
   (LET* ((V5367 (CAR V5366)))
    (IF (EQ 'true *tc*)
     (print-with-type (eval V5367) (typechecks? NIL (CAR (CDR V5367)) 'Type))
     (print (eval V5367)))))
  ((AND (CONSP V5366) (CONSP (CDR V5366)) (EQ '$$ (CAR (CDR V5366)))
    (CONSP (CDR (CDR V5366))) (NULL (CDR (CDR (CDR V5366)))))
   (LET* ((V5368 (CAR V5366)))
    (IF (EQ 'true *tc*)
     (LET ((Typecheck (typechecks? NIL V5368 (curry_type (CAR (CDR (CDR V5366)))))))
      (IF (EQ 'true (qi_= Typecheck 'false)) (error "type error~%")
       (print-with-type (eval V5368) Typecheck)))
     (print (eval V5368)))))
  ((CONSP V5366)
   (LET* ((V5369 (CAR V5366)))
    (IF (EQ 'true *tc*)
     (LET ((Typecheck (typechecks? NIL V5369 'A)))
      (IF (EQ 'true (qi_= Typecheck 'false)) (error "type error~%")
       (print-with-type (eval V5369) Typecheck)))
     (print (eval V5369)))))
  (T (implementation_error 'toplevel_evaluate))))

(DEFUN print-with-type (V5595 V5596)
  (print V5595) 
  (FORMAT T " : ~A" (pretty_type V5596)))

(DEFUN prefix? (V125 V126)
 (COND ((NULL V125) 'true)
  ((AND (CONSP V126) (EQ #\Space (CAR V126))) (prefix? V125 (CDR V126)))
  ((AND (CONSP V126) (EQ #\Newline (CAR V126))) (prefix? V125 (CDR V126)))
  ((AND (CONSP V126) (EQ #\( (CAR V126))) (prefix? V125 (CDR V126)))
  ((AND (CONSP V125) (CONSP V126) (EQ (CAR V125) (CAR V126)))
   (prefix? (CDR V125) (CDR V126)))
  (T 'false)))

(DEFUN store_in_history (X) (PUSH X *history*) X)

(DEFVAR *alphabet* '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

(DEFUN pretty_type (V6688) (mult_subst *alphabet* (extract_vars V6688) V6688))

(DEFUN mult_subst (V6693 V6694 V6695)
 (COND ((NULL V6693) V6695) ((NULL V6694) V6695)
  ((AND (CONSP V6693) (CONSP V6694))
   (mult_subst (CDR V6693) (CDR V6694) (subst (CAR V6693) (CAR V6694) V6695)))
  (T (implementation_error 'mult_subst))))

(DEFUN track (V6696) (track_function (source_code V6696)))

(DEFUN source_code (V6697)
 (LET ((Code (get-prop V6697 'source_code NIL)))
  (IF (EQ 'true (THE SYMBOL (empty? Code)))
   (error "you have not defined ~A in Qi~%" V6697) Code)))

(DEFUN track_function (V6699)
 (COND
  ((AND (CONSP V6699) (CONSP (CDR V6699)) (CONSP (CDR (CDR V6699)))
    (CONSP (CDR (CDR (CDR V6699)))) (NULL (CDR (CDR (CDR (CDR V6699))))))
   (LET*
    ((V6700 (CDR V6699)) (V6701 (CAR V6700)) (V6702 (CDR V6700))
     (V6703 (CAR V6702)))
    (EVAL
     (CONS (CAR V6699)
      (CONS V6701
       (CONS V6703
        (CONS (insert_tracking_code V6701 V6703 (CAR (CDR V6702))) NIL)))))))
  (T (implementation_error 'track_function))))

(DEFUN insert_tracking_code (V6704 V6705 V6706)
 (CONS 'PROGN
  (CONS (CONS 'INCF (CONS '*call* NIL))
   (CONS
    (CONS 'input_track
     (CONS '*call*
      (CONS (CONS 'QUOTE (CONS V6704 NIL)) (CONS (CONS 'LIST V6705) NIL))))
    (CONS (CONS 'terpri_or_read_char NIL)
     (CONS
      (CONS 'LET
       (CONS (CONS (CONS 'RESULT (CONS V6706 NIL)) NIL)
        (CONS
         (CONS 'output_track
          (CONS '*call*
           (CONS (CONS 'QUOTE (CONS V6704 NIL)) (CONS 'RESULT NIL))))
         (CONS (CONS 'DECF (CONS '*call* NIL))
          (CONS (CONS 'terpri_or_read_char NIL) (CONS 'RESULT NIL))))))
      NIL))))))

(DEFVAR *step* 'false)

(DEFUN step (X)
 (COND ((EQ X '+) (SETQ *step* 'true)) ((EQ X '-) (SETQ *step* 'false))
  (T (ERROR "step expects a + or a -.~%"))))

(DEFUN terpri_or_read_char NIL
 (IF (EQ 'true *step*) (check_char (READ-CHAR)) (TERPRI)))

(DEFUN check_char (V6711) (COND ((EQUAL #\^ V6711) (error "")) (T 'true)))

(DEFUN input_track (V6712 V6713 V6714)
 (output "~%~A<~A> Inputs to ~A ~%~A ~{~S, ~} ==>" (spaces V6712) V6712 V6713
  (spaces V6712) V6714))

(DEFUN spaces (V6715)
 (COND ((EQ 0 V6715) "")
  (T (FORMAT NIL "  ~A" (spaces (THE NUMBER (- V6715 1)))))))

(DEFUN output_track (V6716 V6717 V6718)
 (output "~%~A<~A> Output from ~A ~%~A==> ~S" (spaces V6716) V6716 V6717
  (spaces V6716) V6718))

(DEFUN untrack (V6719) (COMPILE (EVAL (source_code V6719))))

(DEFUN debug (X)
  (DECLARE (IGNORE X)) 
  (IF (PROBE-FILE "debug.txt") (DELETE-FILE "debug.txt")) 
  (DRIBBLE (FORMAT NIL "~A~A" *qi_home_directory* "debug.txt"))
  "done")

(DEFUN undebug (X) (DECLARE (IGNORE X)) (DRIBBLE) "done")

(DEFUN profile (V6720) (profile-help (source_code V6720)))

(DEFUN profile-help (V6725)
 (COND
  ((AND (CONSP V6725) (CONSP (CDR V6725)) (CONSP (CDR (CDR V6725))))
   (LET*
    ((V6726 (CAR V6725)) (V6727 (CDR V6725)) (V6728 (CAR V6727))
     (V6729 (CDR V6727)) (V6730 (CAR V6729)))
    (LET ((PrfFunc (gensym "Profile")))
     (do
      (EVAL
       (CONS V6726
        (CONS V6728
         (CONS V6730
          (CONS
           (CONS 'profile_func (CONS V6728 (CONS (CONS PrfFunc V6730) NIL)))
           NIL)))))
      (EVAL
       (CONS V6726
        (CONS PrfFunc
         (CONS V6730 (SUBST PrfFunc V6728 (CDR V6729) ':TEST 'EQUAL)))))
      (COMPILE V6728) (COMPILE PrfFunc) V6728))))
  (T (error "Cannot profile.~%"))))

(DEFUN unprofile (V6731) (untrack_function V6731))

(DEFMACRO profile_func (F EXPR)
   `(PROGN 
     (LET* ((START (GET-INTERNAL-RUN-TIME))
            (RESULT ,EXPR)
            (FINISH (- (GET-INTERNAL-RUN-TIME) START)))
           (put-prop 'profile-stats (QUOTE ,F) (+ (get-prop 'profile-stats (QUOTE ,F) 0) FINISH))
           RESULT)))

(DEFUN profile-results (X)
 (FORMAT T "~{~A, ~A secs~%~}~%" (calibrate-profile (SYMBOL-PLIST 'profile-stats)))
 (SETF (SYMBOL-PLIST 'profile-stats) NIL) 
 'profiled)

(DEFUN calibrate-profile (V6732)
 (COND ((NULL V6732) NIL)
  ((AND (CONSP V6732) (CONSP (CDR V6732)))
   (LET* ((V6733 (CDR V6732)))
    (CONS (CAR V6732)
     (CONS (calibrate (CAR V6733)) (calibrate-profile (CDR V6733))))))
  (T (implementation_error 'calibrate-profile))))

(DEFUN calibrate (Time) (* 1.0 (/ Time INTERNAL-TIME-UNITS-PER-SECOND)))

(DEFVAR *atp-credits* "Qi Proof Tool")

(DEFUN inferences (V6755) *inferences*)

(DEFVAR *spy* 'false)

(DEFUN spy (X) (SETQ *start-time* (GET-INTERNAL-RUN-TIME))
 (COND ((EQ X '+) (SETQ *spy* 'true)) ((EQ X '-) (SETQ *spy* 'false))
  (T (ERROR "spy expects a + or a -.~%"))))

(DEFVAR *inferences* 0)

(DEFUN display-mode (Flag)
 (COND ((EQ Flag '+) (SETQ *display-rb* 'true))
  ((EQ Flag '-) (SETQ *display-rb* 'false))
  (T (ERROR "display-mode expects a + or a -"))))

(DEFVAR *display-rb* 'false)

(DEFUN fst-ass (V6814)
 (COND
  ((AND (CONSP V6814) (CONSP (CAR V6814))
    (wrapper (tuple? (CAR (CAR V6814)))))
   (fst (CAR (CAR V6814))))
  (T (error "fst-ass cannot process this input; ~A" V6814))))

(DEFUN fst-conc (V6823)
 (COND
  ((AND (CONSP V6823) (CONSP (CAR V6823))
    (wrapper (tuple? (CAR (CAR V6823)))))
   (snd (CAR (CAR V6823))))
  (T (error "fst-ass cannot process this input; ~A" V6823))))

(DEFUN thm-intro (V6826)
 (do
  (THE STRING
   (write-to-file V6826 (thm_intro1 V6826 (generalise (retrieve_theorem)))))
  (load V6826)))

(DEFUN retrieve_theorem NIL
 (IF (EQ 'true (THE SYMBOL (empty? (BOUNDP '*thm*))))
  (error "no theorem has been proved~%")
  *thm*))

(DEFUN generalise (V625)
 (COND
  ((wrapper (tuple? V625))
   (generalise* (flatten (CONS (snd V625) (fst V625)))
    (CONS (fst V625) (CONS (snd V625) NIL))))
  (T (f_error 'generalise))))

(DEFUN generalise* (V6845 V6846)
 (COND ((NULL V6845) V6846)
  ((AND (CONSP V6845) (wrapper (test_for_constant? (CAR V6845))))
   (generalise* (CDR V6845) V6846))
  ((CONSP V6845)
   (generalise* (CDR V6845)
    (subst (THE SYMBOL (gensym "P")) (CAR V6845) V6846)))
  (T (implementation_error 'generalise*))))

(DEFUN test_for_constant? (X)
  (IF (FBOUNDP 'constant?)
       (constant? X)
       'true))

(DEFUN remove-if (V6854 V6855)
 (COND ((NULL V6855) NIL)
  ((CONSP V6855)
   (LET* ((V6856 (CAR V6855)) (V6857 (CDR V6855)))
    (IF (EQ 'true (apply V6854 V6856)) (remove-if V6854 V6857)
     (CONS V6856 (remove-if V6854 V6857)))))
  (T (implementation_error 'remove-if))))

(DEFUN thm_intro1 (V946 V947)
 (COND
  ((AND (CONSP V947) (CONSP (CDR V947))
    (NULL (CDR (CDR V947))))
   (LET* ((V948 (CAR V947)))
    (LET ((Ps (BUTLAST V948)))
     (LET ((P (LAST V948)))
      (FORMAT NIL "(theory ~A ~A)" (GENTEMP "theory_")
       (make-string
        "
                              name ~A
                              _______________________
                              ~{~S, ~} ~{~S ~}>> ~S; "
        V946 Ps P (CAR (CDR V947))))))))
  (T (implementation_error 'thm_intro1))))

(DEFUN exchange (V6871 V6872 V6873)
 (COND
  ((wrapper
    (or (or (qi_> 0 V6871) (qi_> 0 V6872))
     (or (qi_> V6871 (length V6873)) (qi_> V6872 (length V6873)))))
   V6873)
  (T (exchange1 V6871 V6872 V6873))))

(DEFUN exchange1 (V6882 V6883 V6884)
 (COND
  ((AND (EQ 1 V6882) (CONSP V6884))
   (insert_nth (CAR V6884) V6883 (CONS (nth V6883 V6884) (CDR V6884))))
  ((CONSP V6884)
   (CONS (CAR V6884) (exchange1 (1- V6882) (1- V6883) (CDR V6884))))
  (T V6884)))

(DEFUN insert_nth (V6888 V6889 V6890)
 (COND ((AND (EQ 1 V6889) (CONSP V6890)) (CONS V6888 (CDR V6890)))
  ((CONSP V6890) (CONS (CAR V6890) (insert_nth V6888 (1- V6889) (CDR V6890))))
  (T (implementation_error 'insert_nth))))

(DEFUN unsolved_goals (V6915)
 (COND ((CONSP V6915) (THE NUMBER (length (CAR V6915))))
  (T (implementation_error 'unsolved_goals))))

(DEFUN atp-credits (V6917) (SETQ *atp-credits* V6917))

(DEFUN atp-prompt (V6918) (SETQ *atp-prompt* V6918))

(DEFUN sequents-in (V6923)
 (COND ((CONSP V6923) (CAR V6923)) (T (implementation_error 'sequents-in))))

(DEFUN prf (V6930) (prf1 (source_code V6930)))

(DEFUN unprf (V6931) (COMPILE (EVAL (source_code V6931))))

(DEFUN prf1 (V951)
 (COND
  ((AND (CONSP V951) (CONSP (CDR V951)) (CONSP (CDR (CDR V951)))
    (CONSP (CAR (CDR (CDR V951))))
    (NULL (CDR (CAR (CDR (CDR V951)))))
    (CONSP (CDR (CDR (CDR V951))))
    (NULL (CDR (CDR (CDR (CDR V951)))))
    (wrapper (tactic? (CAR (CDR V951)))))
   (LET*
    ((V952 (CDR V951)) (V953 (CAR V952)) (V954 (CDR V952)) (V955 (CAR V954)))
    (EVAL
     (CONS (CAR V951)
      (CONS V953
       (CONS V955
        (CONS
         (CONS 'update-proof-if-needed
          (CONS (CAR V955) (CONS (CONS 'QUOTE (CONS V953 NIL)) (CDR V954))))
         NIL)))))))
  ((AND (CONSP V951) (CONSP (CDR V951)))
   (error "~A is not a tactic~%" (CAR (CDR V951))))
  (T (implementation_error 'prf1))))

(DEFUN update-proof-if-needed (V1892 V1893 V1894)
 (COND ((wrapper (qi_= V1892 V1894)) V1894)
  (T (update-proof (LIST V1893) V1894))))

(DEFMACRO structure (&REST X) (LIST 'struct1 (LIST 'QUOTE X)))

(DEFUN struct1 (V6970)
 (COND
  ((CONSP V6970)
   (LET* ((V6971 (CAR V6970)) (V6972 (CDR V6970)))
    (do (struct-syntax-check V6971 V6972)
     (add-to-type-discipline-selectors V6971 V6972)
     (add-constructor-type-discipline V6971 V6972)
     (add-recognisor-type-discipline V6971) (add-recognisor-function V6971)
     (make-structure V6971 V6972))))
  (T (implementation_error 'struct1))))

(DEFUN struct-syntax-check (V6973 V6974)
 (IF
  (EQ 'true
   (THE SYMBOL
    (and (THE SYMBOL (symbol? V6973))
     (THE SYMBOL (not (THE SYMBOL (variable? V6973)))))))
  (struct-syntax-check1 V6974)
  (error "~A: structure expects a non-variable symbol here" V6973)))

(DEFUN struct-syntax-check1 (V6983)
 (COND ((NULL V6983) NIL)
  ((AND (CONSP V6983) (NULL (CDR V6983)))
   (error "odd number of elements in structure"))
  ((AND (CONSP V6983) (CONSP (CDR V6983))
    (wrapper (and (symbol? (CAR V6983)) (not (variable? (CAR V6983))))))
   (struct-syntax-check1 (CDR (CDR V6983))))
  ((CONSP V6983)
   (error "~A: structure expects a non-variable symbol here" (CAR V6983)))
  (T (implementation_error 'struct-syntax-check1))))

(DEFUN add-recognisor-type-discipline (V6984)
 (add-to-type-discipline (THE SYMBOL (concat V6984 '?))
  (CONS (THE SYMBOL (gensym "Type_")) (CONS '--> (CONS 'boolean NIL)))))

(DEFUN add-constructor-type-discipline (V6985 V6986)
 (LET ((Constructor (concat 'make- V6985)))
  (add-to-type-discipline Constructor (structure_types V6986 V6985))))

(DEFUN structure_types (V6993 V6994)
 (COND
  ((AND (CONSP V6993) (CONSP (CDR V6993)) (NULL (CDR (CDR V6993))))
   (CONS (curry_type (CAR (CDR V6993))) (CONS '--> (CONS V6994 NIL))))
  ((AND (CONSP V6993) (CONSP (CDR V6993)))
   (LET* ((V6995 (CDR V6993)))
    (CONS (curry_type (CAR V6995))
     (CONS '--> (CONS (structure_types (CDR V6995) V6994) NIL)))))
  (T (implementation_error 'structure_types))))

(DEFUN add-to-type-discipline-selectors (V6996 V6997)
 (COND ((NULL V6997) NIL)
  ((AND (CONSP V6997) (CONSP (CDR V6997)))
   (LET* ((V6998 (CDR V6997)))
    (LET ((SelectorName (concat (concat V6996 '-) (CAR V6997))))
     (do
      (add-to-type-discipline SelectorName
       (curry_type (CONS V6996 (CONS '--> (CONS (CAR V6998) NIL)))))
      (store_arity SelectorName 1)
      (add-to-type-discipline-selectors V6996 (CDR V6998))))))
  (T (implementation_error 'add-to-type-discipline-selectors))))

(DEFUN add-recognisor-function (V6999)
 (LET ((RecognisorName (concat V6999 '?)))
  (do (store_arity RecognisorName 1)
   (EVAL
    (CONS 'DEFUN
     (CONS RecognisorName
      (CONS (CONS 'x NIL)
       (CONS
        (CONS 'IF
         (CONS (CONS (THE SYMBOL (concat V6999 '-P)) (CONS 'x NIL))
          (CONS ''true (CONS ''false NIL))))
        NIL))))))))

(DEFUN make-structure (V7000 V7001)
 (LET ((ConstructorName (concat 'make- V7000)))
  (do
   (store_arity ConstructorName (THE NUMBER (/ (THE NUMBER (length V7001)) 2)))
   (LET ((Structure (remove_types_from_struct V7001)))
    (EVAL
     (CONS 'DEFSTRUCT
      (APPEND
       (CONS
        (CONS V7000
         (CONS (CONS ':CONSTRUCTOR (CONS ConstructorName (CONS Structure NIL)))
          NIL))
        NIL)
       Structure)))))))

(DEFUN remove_types_from_struct (V7007)
 (COND ((NULL V7007) NIL)
  ((AND (CONSP V7007) (CONSP (CDR V7007)))
   (CONS (CAR V7007) (remove_types_from_struct (CDR (CDR V7007)))))
  (T (implementation_error 'remove_types_from_struct))))

(DEFUN defprolog (V5584)
 (compile-prolog
  (read-prolog-syntax (process-prolog-chars (COERCE V5584 'LIST) NIL))))

(DEFUN read-prolog-syntax (V5585)
 (LET ((Stream (MAKE-STRING-INPUT-STREAM (COERCE V5585 'STRING))))
  (LET
   ((RawProlog
     (read-user-input Stream (READ-CHAR Stream NIL NIL) NIL 1 (@p 0 1)
      (@p 0 1) 'false 'false 'eof?)))
   (divide-clauses RawProlog NIL))))

(DEFUN process-prolog-chars (V1 V2)
 (COND ((NULL V1) (REVERSE V2))
  ((AND (CONSP V1) (EQ #\) (CAR V1)) (CONSP (CDR V1)) (EQ #\. (CAR (CDR V1))))
   (process-prolog-chars (CDR (CDR V1))
    (CONS #\Space (CONS #\& (CONS #\& (CONS #\Space (CONS #\) V2)))))))
  ((AND (CONSP V1) (CONSP (CDR V1)) (EQ #\! (CAR (CDR V1)))
    (CONSP (CDR (CDR V1))) (EQ #\. (CAR (CDR (CDR V1))))
    (wrapper
     (element? (CAR V1)
      (CONS #\, (CONS #\Space (CONS #\Newline (CONS #\Tab NIL)))))))
   (process-prolog-chars (CDR (CDR (CDR V1)))
    (CONS #\Space
     (CONS #\& (CONS #\& (CONS #\Space (CONS #\! (CONS (CAR V1) V2))))))))
  ((AND (CONSP V1) (EQ #\, (CAR V1)))
   (process-prolog-chars (CDR V1) (CONS #\Space V2)))
  ((CONSP V1) (process-prolog-chars (CDR V1) (CONS (CAR V1) V2)))
  (T (f_error 'process-prolog-chars))))

(DEFUN divide-clauses (V10930 V10931)
 (COND
  ((AND (CONSP V10930) (EQ '&& (CAR V10930)) (NULL (CDR V10930)))
   (CONS (process_clause V10931) NIL))
  ((AND (CONSP V10930) (EQ '&& (CAR V10930)))
   (CONS (process_clause V10931) (divide-clauses (CDR V10930) NIL)))
  ((AND (CONSP V10930) (EQ '! (CAR V10930)))
   (divide-clauses (CONS 'cut (CONS NIL (CDR V10930))) V10931))
  ((CONSP V10930)
   (divide-clauses (CDR V10930) (APPEND V10931 (CONS (CAR V10930) NIL))))
  (T (error "~%misplaced . in Prolog program?~%"))))

(DEFUN process_clause (V5360)
 (COND
  ((AND (CONSP V5360) (CONSP (CDR V5360)) (CONSP (CDR (CDR V5360)))
    (EQ ':- (CAR (CDR (CDR V5360))))
    (wrapper (and (predicate? (CAR V5360)) (list? (CAR (CDR V5360))))))
   (LET* ((V5361 (CDR V5360)))
    (CONS (process_literal (CONS (CAR V5360) (convert_terms (CAR V5361))))
     (CONS ':- (CONS (process_body (CDR (CDR V5361))) NIL)))))
  ((AND (CONSP V5360) (CONSP (CDR V5360)) (NULL (CDR (CDR V5360)))
    (wrapper (and (predicate? (CAR V5360)) (list? (CAR (CDR V5360))))))
   (CONS (process_literal (CONS (CAR V5360) (convert_terms (CAR (CDR V5360)))))
    (CONS ':- (CONS NIL NIL))))
  (T (error "~A: is not a legal clause.~%" V5360))))

(DEFUN convert_terms (V5362)
 (remove_redundant_modes
  (MAPCAR #'(LAMBDA (X) (convert_term X '+)) V5362)))

(DEFUN remove_redundant_modes (V5368)
 (COND
  ((AND (CONSP V5368) (EQ 'mode (CAR V5368)) (CONSP (CDR V5368))
    (CONSP (CAR (CDR V5368))) (EQ 'mode (CAR (CAR (CDR V5368))))
    (CONSP (CDR (CAR (CDR V5368)))) (CONSP (CDR (CDR (CAR (CDR V5368)))))
    (NULL (CDR (CDR (CDR (CAR (CDR V5368)))))) (CONSP (CDR (CDR V5368)))
    (NULL (CDR (CDR (CDR V5368)))))
   (LET* ((V5369 (CDR V5368)) (V5370 (CAR V5369)) (V5371 (CDR V5370)))
    (CONS 'mode (CONS (remove_redundant_modes (CAR V5371)) (CDR V5371)))))
  ((AND (CONSP V5368) (EQ 'mode (CAR V5368)) (CONSP (CDR V5368))
    (CONSP (CAR (CDR V5368))) (EQ 'cons (CAR (CAR (CDR V5368))))
    (CONSP (CDR (CAR (CDR V5368)))) (CONSP (CDR (CDR (CAR (CDR V5368)))))
    (CONSP (CAR (CDR (CDR (CAR (CDR V5368))))))
    (EQ 'mode (CAR (CAR (CDR (CDR (CAR (CDR V5368)))))))
    (CONSP (CDR (CAR (CDR (CDR (CAR (CDR V5368)))))))
    (CONSP (CDR (CDR (CAR (CDR (CDR (CAR (CDR V5368))))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CDR (CAR (CDR V5368)))))))))
    (NULL (CDR (CDR (CDR (CAR (CDR V5368)))))) (CONSP (CDR (CDR V5368)))
    (NULL (CDR (CDR (CDR V5368))))
    (wrapper
     (qi_= (CAR (CDR (CDR (CAR (CDR (CDR (CAR (CDR V5368))))))))
      (CAR (CDR (CDR V5368))))))
   (LET* ((V5372 (CDR V5368)) (V5373 (CAR V5372)) (V5374 (CDR V5373)))
    (remove_redundant_modes
     (CONS 'mode
      (CONS
       (CONS 'cons (CONS (CAR V5374) (CONS (CAR (CDR (CAR (CDR V5374)))) NIL)))
       (CDR V5372))))))
  ((CONSP V5368) (THE LIST (MAPCAR 'remove_redundant_modes V5368))) (T V5368)))

(DEFUN convert_term (V5379 V5380)
 (COND
  ((AND (CONSP V5379) (EQ 'mode (CAR V5379)) (CONSP (CDR V5379))
    (CONSP (CDR (CDR V5379))) (NULL (CDR (CDR (CDR V5379)))))
   (LET* ((V5381 (CDR V5379)) (V5382 (CDR V5381)))
    (CONS 'mode (CONS (convert_term (CAR V5381) (CAR V5382)) V5382))))
  ((AND (CONSP V5379) (EQ 'cons (CAR V5379)) (CONSP (CDR V5379))
    (CONSP (CDR (CDR V5379))) (NULL (CDR (CDR (CDR V5379)))))
   (LET* ((V5383 (CDR V5379)))
    (CONS 'cons
     (CONS (convert_term (CAR V5383) V5380)
      (CONS (convert_term (CAR (CDR V5383)) V5380) NIL)))))
  ((AND (CONSP V5379) (NULL (CDR V5379)))
   (CONS 'cons
    (CONS (CONS 'mode (CONS (convert_term (CAR V5379) V5380) (CONS V5380 NIL)))
     (CONS (CONS 'mode (CONS NIL (CONS '- NIL))) NIL))))
  ((CONSP V5379)
   (CONS 'cons
    (CONS (CONS 'mode (CONS (convert_term (CAR V5379) V5380) (CONS V5380 NIL)))
     (CONS (CONS 'mode (CONS (convert_term (CDR V5379) V5380) (CONS '- NIL)))
      NIL))))
  (T (CONS 'mode (CONS V5379 (CONS V5380 NIL))))))

(DEFUN predicate? (V315)
 (COND ((wrapper (and (symbol? V315) (not (variable? V315)))) 'true)
  (T (error "~A is not a predicate.~%" V315))))

(DEFUN process_literal (V5360)
 (COND
  ((CONSP V5360) (CONS (rename_pred (CAR V5360)) (CDR V5360)))
  (T (implementation_error 'process_literal))))

(DEFUN rename_pred (V319)
 (COND ((EQ 'qi_= V319) '=*) 
          (T (THE SYMBOL (concat V319 '*)))))

(DEFUN process_body (V5362)
 (COND
  ((AND (CONSP V5362) (CONSP (CDR V5362)) (NULL (CDR (CDR V5362)))
    (wrapper (and (predicate? (CAR V5362)) (list? (CAR (CDR V5362))))))
   (CONS (process_literal (CONS (CAR V5362) (CAR (CDR V5362)))) NIL))
  ((AND (CONSP V5362) (CONSP (CDR V5362))
    (wrapper (and (predicate? (CAR V5362)) (list? (CAR (CDR V5362))))))
   (LET* ((V5363 (CDR V5362)))
    (CONS (process_literal (CONS (CAR V5362) (CAR V5363)))
     (process_body (CDR V5363)))))
  (T (error "~%syntax error in ~A~%" V5362))))

(DEFUN compile-prolog (V5617) (MAPCAR 'compile_clauses (group_clauses V5617)))

(DEFUN compile-prolog (V5360)
 (MAPCAR 'compile_clauses (group_clauses (MAPCAR 'test-for-naive-abs V5360))))

(DEFUN test-for-naive-abs (V5361)
 (COND
  ((AND (CONSP V5361) (CONSP (CAR V5361)) (CONSP (CDR V5361))
    (EQ ':- (CAR (CDR V5361))) (CONSP (CDR (CDR V5361)))
    (NULL (CDR (CDR (CDR V5361))))
    (< (complexity (CDR (CAR V5361))) *complexity-bound*))
   V5361)
  (T
     (FORMAT T "note: space optimisation; performing naive abstraction on ~S~%"
     (remove_modes V5361))
    (output "mode declarations will be ignored for this clause.~%" V5361)
    (naive-abs V5361))))

(SETQ *complexity-bound* 129)

(DEFUN complexity (V5362) 
  (IF (NULL V5362)
       1
       (* (complexity* (CAR V5362) '+) (complexity (CDR V5362)))))

(DEFUN complexity* (V5382 V5383)
 (COND
  ((AND (CONSP V5382) (EQ 'mode (CAR V5382)) (CONSP (CDR V5382))
    (CONSP (CDR (CDR V5382))) (NULL (CDR (CDR (CDR V5382)))))
   (LET* ((V5384 (CDR V5382))) (complexity* (CAR V5384) (CAR (CDR V5384)))))
  ((wrapper (or (qi_= V5382 '_) (variable? V5382))) 1)
  ((AND (CONSP V5382) (EQ 'cons (CAR V5382)) (CONSP (CDR V5382))
    (CONSP (CDR (CDR V5382))) (NULL (CDR (CDR (CDR V5382)))))
   (LET* ((V5385 (CDR V5382)))
    (THE NUMBER
     (* (mode_complexity V5383)
      (THE NUMBER
       (* (complexity* (CAR V5385) V5383)
        (complexity* (CAR (CDR V5385)) V5383)))))))
  (T (mode_complexity V5383))))

(DEFUN mode_complexity (V5386)
 (COND ((EQ '+ V5386) 2) ((EQ '- V5386) 1)
  (T (error "unknown mode ~S.~%" V5386))))

(DEFUN naive-abs (V5387)
 (COND
  ((AND (CONSP V5387) (CONSP (CAR V5387)) (CONSP (CDR V5387))
    (EQ ':- (CAR (CDR V5387))) (CONSP (CDR (CDR V5387)))
    (NULL (CDR (CDR (CDR V5387)))))
   (LET* ((V5388 (CAR V5387)) (V5389 (CDR V5388)))
    (LET ((AssocTerms (map 'assocterm V5389)))
     (LET ((Vars (MAPCAR 'fst AssocTerms)))
      (CONS (CONS (CAR V5388) Vars)
       (CONS ':-
        (CONS
         (CONS (CONS '=* (CONS Vars (CONS (remove_modes V5389) NIL)))
          (CAR (CDR (CDR V5387))))
         NIL)))))))
  (T (implementation_error 'naive-abs))))

(DEFUN assocterm (V5390) (@p (gensym "V") V5390))

(DEFUN group_clauses (V5618) (group_clauses_help V5618 NIL))

(DEFUN group_clauses_help (V5619 V5620)
 (COND ((NULL V5619) (THE LIST (map 'reverse V5620)))
  ((CONSP V5619)
   (group_clauses_help (CDR V5619) (place_in_group (CAR V5619) V5620)))
  (T (implementation_error 'group_clauses_help))))

(DEFUN place_in_group (V5623 V5624)
 (COND ((NULL V5624) (CONS (CONS V5623 NIL) NIL))
  ((AND (CONSP V5624) (wrapper (belongs? V5623 (CAR V5624))))
   (CONS (CONS V5623 (CAR V5624)) (CDR V5624)))
  ((CONSP V5624) (CONS (CAR V5624) (place_in_group V5623 (CDR V5624))))
  (T (implementation_error 'place_in_group))))

(DEFUN belongs? (V5637 V5638)
 (COND
  ((AND (CONSP V5637) (CONSP (CAR V5637)) (CONSP (CDR V5637))
    (EQ ':- (CAR (CDR V5637))) (CONSP V5638) (CONSP (CAR V5638))
    (CONSP (CAR (CAR V5638))) (CONSP (CDR (CAR V5638)))
    (EQ ':- (CAR (CDR (CAR V5638))))
    (wrapper (qi_= (CAR (CAR V5637)) (CAR (CAR (CAR V5638))))))
   'true)
  (T 'false)))

(DEFUN compile_clauses (V10932)
 (COND
  ((AND (CONSP V10932) (CONSP (CAR V10932)) (CONSP (CAR (CAR V10932)))
    (CONSP (CDR (CAR V10932))) (EQ ':- (CAR (CDR (CAR V10932))))
    (CONSP (CDR (CDR (CAR V10932)))) (NULL (CDR (CDR (CDR (CAR V10932))))))
   (LET* ((V10933 (CAR V10932)) (V10934 (CAR V10933)))
    (LET ((Fparams (make_fparams (CDR V10934))))
     (COMPILE
      (EVAL
       (record_source
        (CONS 'DEFUN
         (CONS (CAR V10934)
          (CONS (APPEND Fparams (CONS 'Continuation NIL))
           (CONS
            (insert_catch
             (CONS (CAR (CDR (CDR V10933))) (CONS (CDR V10932) NIL))
             (CONS 'OR
              (THE LIST
               (map #'(LAMBDA (Y) (compile_clause Y Fparams)) V10932))))
            NIL))))))))))
  (T (implementation_error 'compile_clauses))))

(DEFUN insert_catch (V10941 V10942)
 (COND
  ((wrapper (uses_cut? V10941))
   (CONS 'PROG (CONS NIL (CONS (CONS 'RETURN (CONS V10942 NIL)) NIL))))
  (T V10942)))

(DEFUN uses_cut? (V10943) (occurs? (CONS 'cut* NIL) V10943))

(DEFUN make_fparams (V5662) (REVERSE (fparams (THE NUMBER (length V5662)))))

(DEFUN fparams (V5668)
 (COND ((EQ 0 V5668) NIL)
  (T
   (CONS (THE SYMBOL (concat 'FP V5668)) (fparams (THE NUMBER (- V5668 1)))))))

(DEFUN compile_clause (V5671 V5672)
 (CONS 'PROG2
  (CONS (CONS 'INCF (CONS '*logical-inferences* NIL))
   (CONS (interpret_aum (aum (linearise_clause V5671) V5672)) NIL))))

(DEFUN interpret_aum (V5360)
 (COND
  ((AND (CONSP V5360) (EQ 'if (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CDR (CDR V5360))) (EQ 'then (CAR (CDR (CDR V5360))))
    (CONSP (CDR (CDR (CDR V5360)))) (CONSP (CDR (CDR (CDR (CDR V5360)))))
    (EQ 'else (CAR (CDR (CDR (CDR (CDR V5360))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR V5360))))))
    (NULL (CDR (CDR (CDR (CDR (CDR (CDR V5360)))))))
    (wrapper (qi_= (CAR (CDR (CDR (CDR (CDR (CDR V5360)))))) 'FAIL)))
   (LET* ((V5361 (CDR V5360)))
    (CONS 'AND
     (CONS (interpret_aum (CAR V5361))
      (CONS (interpret_aum (CAR (CDR (CDR V5361)))) NIL)))))
  ((AND (CONSP V5360) (EQ 'let (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CDR (CDR V5360))) (EQ 'be (CAR (CDR (CDR V5360))))
    (CONSP (CDR (CDR (CDR V5360)))) (CONSP (CDR (CDR (CDR (CDR V5360)))))
    (EQ 'in (CAR (CDR (CDR (CDR (CDR V5360))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR V5360))))))
    (NULL (CDR (CDR (CDR (CDR (CDR (CDR V5360))))))))
   (LET* ((V5362 (CDR V5360)) (V5363 (CDR V5362)) (V5364 (CDR V5363)))
    (CONS 'LET
     (CONS (CONS (CONS (CAR V5362) (CONS (interpret_aum (CAR V5364)) NIL)) NIL)
      (CONS (interpret_aum (CAR (CDR (CDR V5364)))) NIL)))))
  ((AND (CONSP V5360) (EQ 'the (CAR V5360)) (CONSP (CDR V5360))
    (EQ 'result (CAR (CDR V5360))) (CONSP (CDR (CDR V5360)))
    (EQ 'of (CAR (CDR (CDR V5360)))) (CONSP (CDR (CDR (CDR V5360))))
    (EQ 'dereferencing (CAR (CDR (CDR (CDR V5360)))))
    (CONSP (CDR (CDR (CDR (CDR V5360)))))
    (NULL (CDR (CDR (CDR (CDR (CDR V5360)))))))
   (CONS 'lazyderef
    (CONS (interpret_aum (CAR (CDR (CDR (CDR (CDR V5360)))))) NIL)))
  ((AND (CONSP V5360) (EQ 'if (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CDR (CDR V5360))) (EQ 'then (CAR (CDR (CDR V5360))))
    (CONSP (CDR (CDR (CDR V5360)))) (CONSP (CDR (CDR (CDR (CDR V5360)))))
    (EQ 'else (CAR (CDR (CDR (CDR (CDR V5360))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR V5360))))))
    (NULL (CDR (CDR (CDR (CDR (CDR (CDR V5360))))))))
   (LET* ((V5365 (CDR V5360)) (V5366 (CDR V5365)) (V5367 (CDR V5366)))
    (CONS 'IF
     (CONS (interpret_aum (CAR V5365))
      (CONS (interpret_aum (CAR V5367))
       (CONS (interpret_aum (CAR (CDR (CDR V5367)))) NIL))))))
  ((AND (CONSP V5360) (CONSP (CDR V5360)) (EQ 'is (CAR (CDR V5360)))
    (CONSP (CDR (CDR V5360))) (EQ 'a (CAR (CDR (CDR V5360))))
    (CONSP (CDR (CDR (CDR V5360))))
    (EQ 'non-empty (CAR (CDR (CDR (CDR V5360)))))
    (CONSP (CDR (CDR (CDR (CDR V5360)))))
    (EQ 'list (CAR (CDR (CDR (CDR (CDR V5360))))))
    (NULL (CDR (CDR (CDR (CDR (CDR V5360)))))))
   (CONS 'CONSP (CONS (CAR V5360) NIL)))
  ((AND (CONSP V5360) (CONSP (CDR V5360)) (EQ 'is (CAR (CDR V5360)))
    (CONSP (CDR (CDR V5360))) (EQ 'a (CAR (CDR (CDR V5360))))
    (CONSP (CDR (CDR (CDR V5360))))
    (EQ 'variable (CAR (CDR (CDR (CDR V5360)))))
    (NULL (CDR (CDR (CDR (CDR V5360))))))
   (CONS 'var? (CONS (interpret_aum (CAR V5360)) NIL)))
  ((AND (CONSP V5360) (EQ 'the (CAR V5360)) (CONSP (CDR V5360))
    (EQ 'head (CAR (CDR V5360))) (CONSP (CDR (CDR V5360)))
    (EQ 'of (CAR (CDR (CDR V5360)))) (CONSP (CDR (CDR (CDR V5360))))
    (NULL (CDR (CDR (CDR (CDR V5360))))))
   (CONS 'CAR (CONS (interpret_aum (CAR (CDR (CDR (CDR V5360))))) NIL)))
  ((AND (CONSP V5360) (EQ 'the (CAR V5360)) (CONSP (CDR V5360))
    (EQ 'tail (CAR (CDR V5360))) (CONSP (CDR (CDR V5360)))
    (EQ 'of (CAR (CDR (CDR V5360)))) (CONSP (CDR (CDR (CDR V5360))))
    (NULL (CDR (CDR (CDR (CDR V5360))))))
   (CONS 'CDR (CONS (interpret_aum (CAR (CDR (CDR (CDR V5360))))) NIL)))
  ((AND (CONSP V5360) (EQ 'rename (CAR V5360)) (CONSP (CDR V5360))
    (EQ 'the (CAR (CDR V5360))) (CONSP (CDR (CDR V5360)))
    (EQ 'variables (CAR (CDR (CDR V5360)))) (CONSP (CDR (CDR (CDR V5360))))
    (EQ 'in (CAR (CDR (CDR (CDR V5360)))))
    (CONSP (CDR (CDR (CDR (CDR V5360)))))
    (NULL (CAR (CDR (CDR (CDR (CDR V5360))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR V5360))))))
    (EQ 'and (CAR (CDR (CDR (CDR (CDR (CDR V5360)))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR V5360)))))))
    (EQ 'then (CAR (CDR (CDR (CDR (CDR (CDR (CDR V5360))))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5360))))))))
    (NULL (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5360))))))))))
   (interpret_aum (CAR (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5360))))))))))
  ((AND (CONSP V5360) (EQ 'rename (CAR V5360)) (CONSP (CDR V5360))
    (EQ 'the (CAR (CDR V5360))) (CONSP (CDR (CDR V5360)))
    (EQ 'variables (CAR (CDR (CDR V5360)))) (CONSP (CDR (CDR (CDR V5360))))
    (EQ 'in (CAR (CDR (CDR (CDR V5360)))))
    (CONSP (CDR (CDR (CDR (CDR V5360)))))
    (CONSP (CDR (CDR (CDR (CDR (CDR V5360))))))
    (EQ 'and (CAR (CDR (CDR (CDR (CDR (CDR V5360)))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR V5360)))))))
    (EQ 'then (CAR (CDR (CDR (CDR (CDR (CDR (CDR V5360))))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5360))))))))
    (NULL (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR V5360))))))))))
   (LET*
    ((V5368 (CDR V5360)) (V5369 (CDR V5368)) (V5370 (CDR V5369))
     (V5371 (CDR V5370)))
    (CONS 'LET
     (CONS (rename_vars (CAR V5371))
      (CONS (interpret_aum (CAR (CDR (CDR (CDR V5371))))) NIL)))))
  ((AND (CONSP V5360) (EQ 'bind (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CDR (CDR V5360))) (EQ 'to (CAR (CDR (CDR V5360))))
    (CONSP (CDR (CDR (CDR V5360)))) (CONSP (CDR (CDR (CDR (CDR V5360)))))
    (EQ 'in (CAR (CDR (CDR (CDR (CDR V5360))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR V5360))))))
    (NULL (CDR (CDR (CDR (CDR (CDR (CDR V5360))))))))
   (LET* ((V5372 (CDR V5360)) (V5373 (CDR V5372)) (V5374 (CDR V5373)))
    (CONS 'PROGV
     (CONS (CONS 'LIST (CONS (CAR V5374) NIL))
      (CONS (CONS 'LIST (CONS (quote (CAR V5372)) NIL))
       (CONS (interpret_aum (CAR (CDR (CDR V5374)))) NIL))))))
  ((AND (CONSP V5360) (CONSP (CDR V5360)) (EQ 'is (CAR (CDR V5360)))
    (CONSP (CDR (CDR V5360))) (EQ 'identical (CAR (CDR (CDR V5360))))
    (CONSP (CDR (CDR (CDR V5360)))) (EQ 'to (CAR (CDR (CDR (CDR V5360)))))
    (CONSP (CDR (CDR (CDR (CDR V5360)))))
    (NULL (CDR (CDR (CDR (CDR (CDR V5360)))))))
   (LET*
    ((V5375 (CDR V5360)) (V5376 (CAR V5360)) (V5377 (CDR V5375))
     (V5378 (CDR V5377)) (V5379 (CDR V5378)) (V5380 (CAR V5379)))
    (IF
     (EQ 'true
      (THE SYMBOL
       (or (THE SYMBOL (empty? V5380)) (THE SYMBOL (symbol? V5380)))))
     (CONS 'EQ (CONS V5376 (CONS (quote V5380) NIL)))
     (CONS 'EQUAL (CONS V5376 (CONS (quote V5380) NIL))))))
  ((AND (CONSP V5360) (EQ 'pop (CAR V5360)) (CONSP (CDR V5360))
    (EQ 'the (CAR (CDR V5360))) (CONSP (CDR (CDR V5360)))
    (EQ 'stack (CAR (CDR (CDR V5360)))) (NULL (CDR (CDR (CDR V5360)))))
   (CONS 'popstack (CONS 'Continuation NIL)))
  ((AND (CONSP V5360) (EQ 'call (CAR V5360)) (CONSP (CDR V5360))
    (EQ 'the (CAR (CDR V5360))) (CONSP (CDR (CDR V5360)))
    (EQ 'continuation (CAR (CDR (CDR V5360)))) (CONSP (CDR (CDR (CDR V5360))))
    (NULL (CDR (CDR (CDR (CDR V5360))))))
   (process_continuation (CAR (CDR (CDR (CDR V5360))))))
  ((wrapper (qi_= V5360 'FAIL)) NIL) 
  (T V5360)))

(DEFUN ask (V53)
 (SETQ *logical-inferences* 0)
  (LET ((Answer (time (run-query V53 (extract_variables V53)))))
   (output "~%~A logical inference~P~%" *logical-inferences* *logical-inferences*)
    (IF (EQ 'true Answer) 'yes 'no)))

(DEFUN query-prolog (V56)
 (LET ((Result (popstack (prolog-calls V56))))
  (IF (NULL Result) 'false 
      (IF (EQ T Result) 'true Result))))

(DEFUN prolog-calls (V14)
 (EVAL (tinker (pc-help (MAPCAR 'process_literal V14)))))

(DEFUN run-query (V54 V55)
 (COND
  ((CONSP V54)
   (query-prolog (CONS V54 (CONS (CONS 'answer (CONS V55 NIL)) NIL))))
  (T (ERROR "~S is not a valid query.~%" V54))))

(DEFUN tinker (V7)
 (COND
  ((EQ 'Continuation V7)
   (CONS 'FUNCTION (CONS (CONS 'LAMBDA (CONS NIL (CONS 'T NIL))) NIL)))
  ((wrapper (variable? V7)) (CONS 'QUOTE (CONS V7 NIL)))
  ((CONSP V7) (CONS (CAR V7) (MAPCAR 'tinker (CDR V7)))) 
  (T V7)))

(DEFUN process_continuation (V10944)
 (COND
  ((AND (CONSP V10944) (CONSP (CAR V10944))
    (wrapper (evaluable_predicate? (CAR (CAR V10944)))))
   (LET* ((V10945 (CAR V10944)) (V10946 (CAR V10945)))
    (CONS (mapF V10946)
     (APPEND
      (THE LIST
       (MAPCAR
        #'(LAMBDA (Term) (process_evaluable_terms (mode_deref V10946) Term))
        (CDR V10945)))
      (CONS (pc-help (CDR V10944)) NIL)))))
  ((AND (CONSP V10944) (CONSP (CAR V10944)) (EQ 'cut* (CAR (CAR V10944)))
    (NULL (CDR (CAR V10944))) (NULL (CDR V10944)))
   (CONS 'OR
    (CONS (CONS 'popstack (CONS 'Continuation NIL))
     (CONS (CONS 'RETURN (CONS NIL NIL)) NIL))))
  ((AND (CONSP V10944) (CONSP (CAR V10944)) (EQ 'cut* (CAR (CAR V10944)))
    (NULL (CDR (CAR V10944))))
   (CONS 'OR
    (CONS (process_continuation (CDR V10944))
     (CONS (CONS 'RETURN (CONS NIL NIL)) NIL))))
  ((AND (CONSP V10944) (CONSP (CAR V10944)))
   (LET* ((V10947 (CAR V10944)))
    (CONS (CAR V10947)
     (APPEND (THE LIST (map 'quote (CDR V10947)))
      (CONS (pc-help (CDR V10944)) NIL)))))
  (T (implementation_error 'process_continuation))))

(DEFUN mode_deref (V1030)
 (COND ((EQ 'is* V1030) 'lazyderef) ((EQ 'when* V1030) 'lazyderef)
  ((EQ 'eval* V1030) 'lazyderef) (T 'deref)))

(DEFUN mapF (V1031)
 (COND ((EQ 'is!* V1031) 'is*) ((EQ 'when!* V1031) 'when*)
  ((EQ 'eval!* V1031) 'eval*) 
  (T V1031)))

(DEFUN when* (V1039 V1040)
(INCF *logical-inferences*)
 (COND ((EQ 'true V1039) (popstack V1040)) ((EQ 'false V1039) NIL)
  (T (error "when expects a boolean, not ~S.~%" V1039))))

(DEFUN eval* (V1045 V1050) (INCF *logical-inferences*) (popstack V1050))

(DEFUN is* (V1051 V1052 V1053)
(INCF *logical-inferences*)
 (PROGV (CONS V1051 NIL) (CONS V1052 NIL) (popstack V1053)))

(DEFUN evaluable_predicate? (V1054)
 (THE SYMBOL
  (element? V1054
   (CONS 'is*
    (CONS 'when*
     (CONS 'eval* (CONS 'is!* (CONS 'when!* (CONS 'eval!* NIL)))))))))

(DEFUN process_evaluable_terms (V1062 V1063)
 (COND
  ((CONSP V1063)
   (CONS (CAR V1063)
    (THE LIST
     (MAPCAR #'(LAMBDA (Term) (process_evaluable_terms V1062 Term))
      (CDR V1063)))))
  ((wrapper (variable? V1063)) (CONS V1062 (CONS V1063 NIL)))
  ((wrapper (or (number? V1063) (or (character? V1063) (string? V1063))))
   V1063)
  ((AND (EQ '- V1062) (NULL V1063)) NIL)
  ((EQ '_ V1063) (CONS 'GENSYM (CONS "V" NIL)))
  (T (CONS 'QUOTE (CONS V1063 NIL)))))

(DEFUN pc-help (V10952)
 (COND ((NULL V10952) 'Continuation)
  ((AND (CONSP V10952) (CONSP (CAR V10952))
    (wrapper (evaluable_predicate? (CAR (CAR V10952)))))
   (LET* ((V10953 (CAR V10952)) (V10954 (CAR V10953)))
    (CONS 'FUNCTION
     (CONS
      (CONS 'LAMBDA
       (CONS NIL
        (CONS
         (APPEND
          (CONS (mapF V10954)
           (THE LIST
            (MAPCAR
             #'(LAMBDA (Term)
                (process_evaluable_terms (mode_deref V10954) Term))
             (CDR V10953))))
          (CONS (pc-help (CDR V10952)) NIL))
         NIL)))
      NIL))))
  ((AND (CONSP V10952) (CONSP (CAR V10952)) (EQ 'cut* (CAR (CAR V10952)))
    (NULL (CDR (CAR V10952))))
   (CONS 'FUNCTION
    (CONS
     (CONS 'LAMBDA
      (CONS NIL
       (CONS
        (CONS 'OR
         (CONS (CONS 'popstack (CONS (pc-help (CDR V10952)) NIL))
          (CONS (CONS 'RETURN (CONS NIL NIL)) NIL)))
        NIL)))
     NIL)))
  ((AND (CONSP V10952) (CONSP (CAR V10952)))
   (LET* ((V10955 (CAR V10952)))
    (CONS 'FUNCTION
     (CONS
      (CONS 'LAMBDA
       (CONS NIL
        (CONS
         (APPEND (CONS (CAR V10955) (THE LIST (map 'quote (CDR V10955))))
          (CONS (pc-help (CDR V10952)) NIL))
         NIL)))
      NIL))))
  (T (ERROR "~S is not a valid continuation.~%" V10952))))

(DEFUN rename_vars (V5777)
 (COND ((NULL V5777) NIL)
  ((CONSP V5777)
   (CONS (CONS (CAR V5777) (CONS (CONS 'GENSYM (CONS "V" NIL)) NIL))
    (rename_vars (CDR V5777))))
  (T (implementation_error 'rename_vars))))

(DEFUN quote (V5778)
 (COND
  ((AND (CONSP V5778) (EQ 'cons (CAR V5778)) (CONSP (CDR V5778))
    (CONSP (CDR (CDR V5778))) (NULL (CDR (CDR (CDR V5778)))))
   (LET* ((V5779 (CDR V5778)))
    (CONS 'CONS
     (CONS (quote (CAR V5779)) (CONS (quote (CAR (CDR V5779))) NIL)))))
  ((AND (CONSP V5778) (wrapper (list? V5778)))
   (CONS 'LIST (THE LIST (map 'quote V5778))))
  ((CONSP V5778)
   (CONS 'CONS (CONS (quote (CAR V5778)) (CONS (quote (CDR V5778)) NIL))))
  ((wrapper (variable? V5778)) V5778)
  ((wrapper (or (number? V5778) (or (string? V5778) (character? V5778))))
   V5778)
  ((NULL V5778) NIL)
  ((wrapper (qi_= '_ V5778)) (CONS 'GENSYM (CONS "V" NIL)))
  (T (CONS 'QUOTE (CONS V5778 NIL)))))

(DEFUN list? (V5788)
 (COND ((NULL V5788) 'true) ((CONSP V5788) (list? (CDR V5788))) (T 'false)))

(DEFUN linearise_clause (V5789)
 (COND
  ((AND (CONSP V5789) (CONSP (CDR V5789)) (EQ ':- (CAR (CDR V5789)))
    (CONSP (CDR (CDR V5789))) (NULL (CDR (CDR (CDR V5789)))))
   (LET ((V (first_rpted_var (CAR V5789))))
    (IF (EQ 'true (qi_= V 'left_linear)) V5789
     (linearise_clause (left_linearise_clause V V5789)))))
  (T (implementation_error 'linearise_clause))))

(DEFUN first_rpted_var (V5790) (fpr-help (flatten V5790)))

(DEFUN fpr-help (V5793)
 (COND
  ((AND (CONSP V5793)
    (wrapper
     (and (variable? (CAR V5793)) (element? (CAR V5793) (CDR V5793)))))
   (CAR V5793))
  ((CONSP V5793) (fpr-help (CDR V5793))) (T 'left_linear)))

(DEFUN flatten (V5794)
 (COND
  ((AND (CONSP V5794) (CONSP (CAR V5794)))
   (APPEND (flatten (CAR V5794)) (flatten (CDR V5794))))
  ((CONSP V5794) (CONS (CAR V5794) (flatten (CDR V5794)))) 
   (T V5794)))

(DEFUN left_linearise_clause (V5795 V5796)
 (COND
  ((AND (CONSP V5796) (CONSP (CDR V5796)) (EQ ':- (CAR (CDR V5796)))
    (CONSP (CDR (CDR V5796))) (NULL (CDR (CDR (CDR V5796)))))
   (LET ((V* (gensym "V")))
    (CONS (rename_V V5795 V* (CAR V5796))
     (CONS ':-
      (CONS
       (CONS (CONS (unifpred) (CONS V5795 (CONS V* NIL))) (CAR (CDR (CDR V5796))))
       NIL)))))
  (T (implementation_error 'left_linearise_clause))))

(DEFUN unifpred ()
    (COND ((wrapper *occurs*) '=!*) 
               (T '=*)))

(DEFUN occurs-check (V632)
 (COND ((EQ '+ V632) (SETQ *occurs* 'true))
  ((EQ '- V632) (SETQ *occurs* 'false))
  (T (error "occurs-check expects a + or a -"))))

(SETQ *occurs* 'false)

(DEFUN rename_V (V5806 V5807 V5808)
 (COND ((wrapper (qi_= V5806 V5808)) V5807)
  ((CONSP V5808)
   (LET* ((V5809 (CAR V5808)) (V5810 (CDR V5808)))
    (LET ((Renamed (rename_V V5806 V5807 V5809)))
     (IF (EQ 'true (qi_= Renamed V5809))
      (CONS V5809 (rename_V V5806 V5807 V5810)) (CONS Renamed V5810)))))
  (T V5808)))

(DEFUN aum (V5811 V5812)
 (COND
  ((AND (CONSP V5811) (CONSP (CAR V5811)) (CONSP (CDR V5811))
    (EQ ':- (CAR (CDR V5811))) (CONSP (CDR (CDR V5811)))
    (NULL (CDR (CDR (CDR V5811)))))
   (LET* ((V5813 (CAR V5811)) (V5814 (CDR V5813)))
    (mu_reduction
     (make_mu_application
      (CONS 'mu
       (CONS V5814
        (CONS (continuation_call V5814 (CAR (CDR (CDR V5811)))) NIL)))
      V5812))))
  (T (implementation_error 'aum))))

(DEFUN continuation_call (V5815 V5816)
 (cc-help (free_variables_in_body V5815 V5816) V5816))

(DEFUN free_variables_in_body (V5817 V5818)
 (fv_decl_help (flatten V5817) (flatten V5818)))

(DEFUN fv_decl_help (V5821 V5822)
 (COND ((NULL V5822) NIL)
  ((AND (CONSP V5822) (wrapper (free_variable_in? (CAR V5822) V5821)))
   (LET* ((V5823 (CAR V5822)))
    (adjoin V5823 (fv_decl_help V5821 (THE LIST (remove V5823 (CDR V5822)))))))
  ((CONSP V5822) (fv_decl_help V5821 (CDR V5822)))
  (T (implementation_error 'fv_decl_help))))

(DEFUN adjoin (V5825 V5826)
 (IF (EQ 'true (THE SYMBOL (element? V5825 V5826))) V5826 (CONS V5825 V5826)))

(DEFUN free_variable_in? (V5828 V5829)
 (THE SYMBOL
  (and (THE SYMBOL (variable? V5828))
   (THE SYMBOL (not (THE SYMBOL (element? V5828 V5829)))))))

(DEFUN cc-help (V5832 V5833)
 (COND
  ((AND (NULL V5832) (NULL V5833)) (CONS 'pop (CONS 'the (CONS 'stack NIL))))
  ((NULL V5833)
   (CONS 'rename
    (CONS 'the
     (CONS 'variables
      (CONS 'in
       (CONS V5832
        (CONS 'and
         (CONS 'then
          (CONS (CONS 'pop (CONS 'the (CONS 'stack NIL))) NIL)))))))))
  ((NULL V5832) (CONS 'call (CONS 'the (CONS 'continuation (CONS V5833 NIL)))))
  (T
   (CONS 'rename
    (CONS 'the
     (CONS 'variables
      (CONS 'in
       (CONS V5832
        (CONS 'and
         (CONS 'then
          (CONS (CONS 'call (CONS 'the (CONS 'continuation (CONS V5833 NIL))))
           NIL)))))))))))

(DEFUN make_mu_application (V5834 V5835)
 (COND
  ((AND (CONSP V5834) (EQ 'mu (CAR V5834)) (CONSP (CDR V5834))
    (NULL (CAR (CDR V5834))) (CONSP (CDR (CDR V5834)))
    (NULL (CDR (CDR (CDR V5834)))) (NULL V5835))
   (CAR (CDR (CDR V5834))))
  ((AND (CONSP V5834) (EQ 'mu (CAR V5834)) (CONSP (CDR V5834))
    (CONSP (CAR (CDR V5834))) (CONSP (CDR (CDR V5834)))
    (NULL (CDR (CDR (CDR V5834)))) (CONSP V5835))
   (LET* ((V5836 (CDR V5834)) (V5837 (CAR V5836)))
    (CONS
     (CONS 'mu
      (CONS (CAR V5837)
       (CONS
        (make_mu_application (CONS 'mu (CONS (CDR V5837) (CDR V5836)))
         (CDR V5835))
        NIL)))
     (CONS (CAR V5835) NIL))))
  (T (implementation_error 'make_mu_application))))

(DEFUN mu_reduction (V5384)
 (COND
  ((AND (CONSP V5384) (CONSP (CAR V5384)) (EQ 'mu (CAR (CAR V5384)))
    (CONSP (CDR (CAR V5384))) (CONSP (CAR (CDR (CAR V5384))))
    (EQ 'mode (CAR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CDR (CAR (CDR (CAR V5384))))))
    (EQ '+ (CAR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CDR (CAR V5384)))) (NULL (CDR (CDR (CDR (CAR V5384)))))
    (CONSP (CDR V5384)) (NULL (CDR (CDR V5384))))
   (LET* ((V5385 (CAR V5384)) (V5386 (CDR V5385)))
    (mu_reduction
     (CONS (CONS 'mu (CONS (CAR (CDR (CAR V5386))) (CDR V5386)))
      (CDR V5384)))))
  ((AND (CONSP V5384) (CONSP (CAR V5384)) (EQ 'mu (CAR (CAR V5384)))
    (CONSP (CDR (CAR V5384))) (CONSP (CAR (CDR (CAR V5384))))
    (EQ 'mode (CAR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CAR (CDR (CAR V5384)))))
    (CONSP (CAR (CDR (CAR (CDR (CAR V5384))))))
    (EQ 'mode (CAR (CAR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CAR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CDR (CAR (CDR (CAR (CDR (CAR V5384))))))))
    (EQ '+ (CAR (CDR (CDR (CAR (CDR (CAR (CDR (CAR V5384)))))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR (CDR (CAR V5384)))))))))
    (CONSP (CDR (CDR (CAR (CDR (CAR V5384))))))
    (EQ '- (CAR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CDR (CAR V5384)))) (NULL (CDR (CDR (CDR (CAR V5384)))))
    (CONSP (CDR V5384)) (NULL (CDR (CDR V5384))))
   (LET* ((V5387 (CAR V5384)) (V5388 (CDR V5387)))
    (mu_reduction
     (CONS (CONS 'mu (CONS (CAR (CDR (CAR (CDR (CAR V5388))))) (CDR V5388)))
      (CDR V5384)))))
  ((AND (CONSP V5384) (CONSP (CAR V5384)) (EQ 'mu (CAR (CAR V5384)))
    (CONSP (CDR (CAR V5384))) (CONSP (CAR (CDR (CAR V5384))))
    (EQ 'mode (CAR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CAR (CDR (CAR V5384)))))
    (CONSP (CAR (CDR (CAR (CDR (CAR V5384))))))
    (EQ 'mode (CAR (CAR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CAR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CDR (CAR (CDR (CAR (CDR (CAR V5384))))))))
    (EQ '- (CAR (CDR (CDR (CAR (CDR (CAR (CDR (CAR V5384)))))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR (CDR (CAR V5384)))))))))
    (CONSP (CDR (CDR (CAR (CDR (CAR V5384))))))
    (EQ '- (CAR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CDR (CAR V5384)))) (NULL (CDR (CDR (CDR (CAR V5384)))))
    (CONSP (CDR V5384)) (NULL (CDR (CDR V5384))))
   (LET* ((V5389 (CAR V5384)) (V5390 (CDR V5389)))
    (mu_reduction
     (CONS (CONS 'mu (CONS (CAR (CDR (CAR V5390))) (CDR V5390)))
      (CDR V5384)))))
  ((AND (CONSP V5384) (CONSP (CAR V5384)) (EQ 'mu (CAR (CAR V5384)))
    (CONSP (CDR (CAR V5384))) (CONSP (CDR (CDR (CAR V5384))))
    (NULL (CDR (CDR (CDR (CAR V5384))))) (CONSP (CDR V5384))
    (NULL (CDR (CDR V5384))) (wrapper (qi_= '_ (CAR (CDR (CAR V5384))))))
   (mu_reduction (CAR (CDR (CDR (CAR V5384))))))
  ((AND (CONSP V5384) (CONSP (CAR V5384)) (EQ 'mu (CAR (CAR V5384)))
    (CONSP (CDR (CAR V5384))) (CONSP (CAR (CDR (CAR V5384))))
    (EQ 'mode (CAR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CDR (CAR (CDR (CAR V5384))))))
    (EQ '- (CAR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CDR (CAR V5384)))) (NULL (CDR (CDR (CDR (CAR V5384)))))
    (CONSP (CDR V5384)) (NULL (CDR (CDR V5384)))
    (EQ '_ (CAR (CDR (CAR (CDR (CAR V5384)))))))
   (mu_reduction (CAR (CDR (CDR (CAR V5384))))))
  ((AND (CONSP V5384) (CONSP (CAR V5384)) (EQ 'mu (CAR (CAR V5384)))
    (CONSP (CDR (CAR V5384))) (CONSP (CDR (CDR (CAR V5384))))
    (NULL (CDR (CDR (CDR (CAR V5384))))) (CONSP (CDR V5384))
    (NULL (CDR (CDR V5384)))
    (wrapper
     (ephemeral_variable? (CAR (CDR (CAR V5384))) (CAR (CDR V5384)))))
   (LET* ((V5391 (CAR V5384)) (V5392 (CDR V5391)))
    (subst (CAR (CDR V5384)) (CAR V5392) (mu_reduction (CAR (CDR V5392))))))
  ((AND (CONSP V5384) (CONSP (CAR V5384)) (EQ 'mu (CAR (CAR V5384)))
    (CONSP (CDR (CAR V5384))) (CONSP (CDR (CDR (CAR V5384))))
    (NULL (CDR (CDR (CDR (CAR V5384))))) (CONSP (CDR V5384))
    (NULL (CDR (CDR V5384))) (wrapper (variable? (CAR (CDR (CAR V5384))))))
   (LET* ((V5393 (CAR V5384)) (V5394 (CDR V5393)))
    (CONS 'let
     (CONS (CAR V5394)
      (CONS 'be
       (CONS (CAR (CDR V5384))
        (CONS 'in (CONS (mu_reduction (CAR (CDR V5394))) NIL))))))))
  ((AND (CONSP V5384) (CONSP (CAR V5384)) (EQ 'mu (CAR (CAR V5384)))
    (CONSP (CDR (CAR V5384))) (CONSP (CAR (CDR (CAR V5384))))
    (EQ 'mode (CAR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CDR (CAR (CDR (CAR V5384))))))
    (EQ '- (CAR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CDR (CAR V5384)))) (NULL (CDR (CDR (CDR (CAR V5384)))))
    (CONSP (CDR V5384)) (NULL (CDR (CDR V5384)))
    (wrapper (variable? (CAR (CDR (CAR (CDR (CAR V5384))))))))
   (LET* ((V5395 (CAR V5384)) (V5396 (CDR V5395)))
    (mu_reduction
     (CONS (CONS 'mu (CONS (CAR (CDR (CAR V5396))) (CDR V5396)))
      (CDR V5384)))))
  ((AND (CONSP V5384) (CONSP (CAR V5384)) (EQ 'mu (CAR (CAR V5384)))
    (CONSP (CDR (CAR V5384))) (CONSP (CAR (CDR (CAR V5384))))
    (EQ 'mode (CAR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CDR (CAR (CDR (CAR V5384))))))
    (EQ '- (CAR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CDR (CAR V5384)))) (NULL (CDR (CDR (CDR (CAR V5384)))))
    (CONSP (CDR V5384)) (NULL (CDR (CDR V5384)))
    (wrapper (prolog_constant? (CAR (CDR (CAR (CDR (CAR V5384))))))))
   (LET* ((V5397 (CAR V5384)) (V5398 (CDR V5397)))
    (LET ((Z (gensym "X")))
     (CONS 'let
      (CONS Z
       (CONS 'be
        (CONS
         (CONS 'the
          (CONS 'result (CONS 'of (CONS 'dereferencing (CDR V5384)))))
         (CONS 'in
          (CONS
           (CONS 'if
            (CONS
             (CONS Z
              (CONS 'is
               (CONS 'identical
                (CONS 'to (CONS (CAR (CDR (CAR V5398))) NIL)))))
             (CONS 'then
              (CONS (mu_reduction (CAR (CDR V5398)))
               (CONS 'else (CONS 'FAIL NIL))))))
           NIL)))))))))
  ((AND (CONSP V5384) (CONSP (CAR V5384)) (EQ 'mu (CAR (CAR V5384)))
    (CONSP (CDR (CAR V5384))) (CONSP (CDR (CDR (CAR V5384))))
    (NULL (CDR (CDR (CDR (CAR V5384))))) (CONSP (CDR V5384))
    (NULL (CDR (CDR V5384)))
    (wrapper (prolog_constant? (CAR (CDR (CAR V5384))))))
   (LET*
    ((V5399 (CAR V5384)) (V5400 (CDR V5399)) (V5401 (CAR V5400))
     (V5402 (CDR V5400)) (V5403 (CAR V5402)))
    (LET ((Z (gensym "X")))
     (CONS 'let
      (CONS Z
       (CONS 'be
        (CONS
         (CONS 'the
          (CONS 'result (CONS 'of (CONS 'dereferencing (CDR V5384)))))
         (CONS 'in
          (CONS
           (CONS 'if
            (CONS
             (CONS Z (CONS 'is (CONS 'identical (CONS 'to (CONS V5401 NIL)))))
             (CONS 'then
              (CONS (mu_reduction V5403)
               (CONS 'else
                (CONS
                 (CONS 'if
                  (CONS (CONS Z (CONS 'is (CONS 'a (CONS 'variable NIL))))
                   (CONS 'then
                    (CONS
                     (CONS 'bind
                      (CONS V5401
                       (CONS 'to
                        (CONS Z (CONS 'in (CONS (mu_reduction V5403) NIL))))))
                     (CONS 'else (CONS 'FAIL NIL))))))
                 NIL))))))
           NIL)))))))))
  ((AND (CONSP V5384) (CONSP (CAR V5384)) (EQ 'mu (CAR (CAR V5384)))
    (CONSP (CDR (CAR V5384))) (CONSP (CAR (CDR (CAR V5384))))
    (EQ 'mode (CAR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CAR (CDR (CAR V5384)))))
    (CONSP (CAR (CDR (CAR (CDR (CAR V5384))))))
    (EQ 'cons (CAR (CAR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CAR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CDR (CAR (CDR (CAR (CDR (CAR V5384))))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR (CDR (CAR V5384)))))))))
    (CONSP (CDR (CDR (CAR (CDR (CAR V5384))))))
    (EQ '- (CAR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CDR (CAR V5384)))) (NULL (CDR (CDR (CDR (CAR V5384)))))
    (CONSP (CDR V5384)) (NULL (CDR (CDR V5384))))
   (LET*
    ((V5404 (CAR V5384)) (V5405 (CDR V5404)) (V5406 (CAR V5405))
     (V5407 (CDR V5406)) (V5408 (CAR V5407)) (V5409 (CDR V5407))
     (V5410 (CDR V5408)))
    (LET ((Z (gensym "X")))
     (CONS 'let
      (CONS Z
       (CONS 'be
        (CONS
         (CONS 'the
          (CONS 'result (CONS 'of (CONS 'dereferencing (CDR V5384)))))
         (CONS 'in
          (CONS
           (CONS 'if
            (CONS
             (CONS Z (CONS 'is (CONS 'a (CONS 'non-empty (CONS 'list NIL)))))
             (CONS 'then
              (CONS
               (mu_reduction
                (CONS
                 (CONS 'mu
                  (CONS (CONS 'mode (CONS (CAR V5410) V5409))
                   (CONS
                    (CONS
                     (CONS 'mu
                      (CONS (CONS 'mode (CONS (CAR (CDR V5410)) V5409))
                       (CDR V5405)))
                     (CONS (CONS 'the (CONS 'tail (CONS 'of (CONS Z NIL))))
                      NIL))
                    NIL)))
                 (CONS (CONS 'the (CONS 'head (CONS 'of (CONS Z NIL)))) NIL)))
               (CONS 'else (CONS 'FAIL NIL))))))
           NIL)))))))))
  ((AND (CONSP V5384) (CONSP (CAR V5384)) (EQ 'mu (CAR (CAR V5384)))
    (CONSP (CDR (CAR V5384))) (CONSP (CAR (CDR (CAR V5384))))
    (EQ 'cons (CAR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CAR (CDR (CAR V5384)))))
    (CONSP (CDR (CDR (CAR (CDR (CAR V5384))))))
    (NULL (CDR (CDR (CDR (CAR (CDR (CAR V5384)))))))
    (CONSP (CDR (CDR (CAR V5384)))) (NULL (CDR (CDR (CDR (CAR V5384)))))
    (CONSP (CDR V5384)) (NULL (CDR (CDR V5384))))
   (LET*
    ((V5411 (CAR V5384)) (V5412 (CDR V5411)) (V5413 (CAR V5412))
     (V5414 (CDR V5412)) (V5415 (CDR V5413)))
    (LET ((Z (gensym "X")))
     (CONS 'let
      (CONS Z
       (CONS 'be
        (CONS
         (CONS 'the
          (CONS 'result (CONS 'of (CONS 'dereferencing (CDR V5384)))))
         (CONS 'in
          (CONS
           (CONS 'if
            (CONS
             (CONS Z (CONS 'is (CONS 'a (CONS 'non-empty (CONS 'list NIL)))))
             (CONS 'then
              (CONS
               (mu_reduction
                (CONS
                 (CONS 'mu
                  (CONS (CAR V5415)
                   (CONS
                    (CONS (CONS 'mu (CONS (CAR (CDR V5415)) V5414))
                     (CONS (CONS 'the (CONS 'tail (CONS 'of (CONS Z NIL))))
                      NIL))
                    NIL)))
                 (CONS (CONS 'the (CONS 'head (CONS 'of (CONS Z NIL)))) NIL)))
               (CONS 'else
                (CONS
                 (CONS 'if
                  (CONS (CONS Z (CONS 'is (CONS 'a (CONS 'variable NIL))))
                   (CONS 'then
                    (CONS
                     (CONS 'rename
                      (CONS 'the
                       (CONS 'variables
                        (CONS 'in
                         (CONS (extract_variables V5415)
                          (CONS 'and
                           (CONS 'then
                            (CONS
                             (CONS 'bind
                              (CONS (remove_modes V5413)
                               (CONS 'to
                                (CONS Z
                                 (CONS 'in
                                  (CONS (mu_reduction (CAR V5414)) NIL))))))
                             NIL))))))))
                     (CONS 'else (CONS 'FAIL NIL))))))
                 NIL))))))
           NIL)))))))))
  (T V5384)))

(DEFUN remove_modes (V5410)
 (COND
  ((AND (CONSP V5410) (EQ 'mode (CAR V5410)) (CONSP (CDR V5410))
    (CONSP (CDR (CDR V5410))) (EQ '+ (CAR (CDR (CDR V5410))))
    (NULL (CDR (CDR (CDR V5410)))))
   (remove_modes (CAR (CDR V5410))))
  ((AND (CONSP V5410) (EQ 'mode (CAR V5410)) (CONSP (CDR V5410))
    (CONSP (CDR (CDR V5410))) (EQ '- (CAR (CDR (CDR V5410))))
    (NULL (CDR (CDR (CDR V5410)))))
   (remove_modes (CAR (CDR V5410))))
  ((CONSP V5410) (MAPCAR 'remove_modes V5410)) 
  (T V5410)))

(DEFUN ephemeral_variable? (V5886 V5887)
 (THE SYMBOL
  (and (THE SYMBOL (variable? V5886)) (THE SYMBOL (variable? V5887)))))

(DEFUN extract_variables (V861)
 (COND ((wrapper (variable? V861)) (CONS V861 NIL))
  ((CONSP V861)
   (THE LIST
    (union (extract_variables (CAR V861)) (extract_variables (CDR V861)))))
  (T NIL)))

(DEFUN prolog_constant? (V5902)
 (COND ((NULL V5902) 'true) ((wrapper (symbol? V5902)) 'true)
  ((wrapper (number? V5902)) 'true) ((wrapper (boolean? V5902)) 'true)
  ((wrapper (string? V5902)) 'true)
  ((wrapper (character? V5902)) 'true) (T 'false)))

(DEFUN lazyderef (X)
  (IF (AND (SYMBOLP X) 
              (BOUNDP X) 
              (NOT (NULL X))
              (NOT (EQ X T))
              (UPPER-CASE-P (CHAR (SYMBOL-NAME X) 0)))
        (lazyderef (SYMBOL-VALUE X))
        X))               

(DEFUN var? (X)
  (AND (SYMBOLP X) 
       (NOT (NULL X))
       (NOT (EQ X T))
       (UPPER-CASE-P (CHAR (SYMBOL-NAME X) 0))))

(DEFUN =* (X Y Continuation)
  (lzy=* (lazyderef X) (lazyderef Y) Continuation))

(DEFUN lzy=* (X Y Continuation)
   (COND ((EQUAL X Y) (popstack Continuation))
         ((var? X) 
          (PROGV (LIST X) (LIST Y) (popstack Continuation)))
         ((var? Y) 
          (PROGV (LIST Y) (LIST X) (popstack Continuation)))
         ((AND (CONSP X) (CONSP Y))
          (lzy=* (lazyderef (CAR X)) (lazyderef (CAR Y))
              (FUNCTION (LAMBDA () 
			(lzy=* (lazyderef (CDR X)) 
				(lazyderef (CDR Y)) 
  				 Continuation)))))
         (T NIL)))

(DEFUN popstack (Continuation)  (FUNCALL Continuation))

(DEFUN answer* (Vars Continuation)
  (DECLARE (IGNORE Continuation))
  (COND ((NULL Vars) T)
         (T (MAPC (FUNCTION 
         (LAMBDA (V) (output "~%~A = ~S~%" V (deref V)))) Vars)
            (NOT (Y-OR-N-P "~%More?")))))

(DEFUN deref (x)
  (COND ((CONSP x) (CONS (deref (CAR x)) (deref (CDR x))))
        ((boundvar? x) (deref (SYMBOL-VALUE x)))
        (T x)))

(DEFUN boundvar? (X)
  (AND (var? X) (BOUNDP X)))

(DEFUN return* (Val Continuation)
  (DECLARE (IGNORE Continuation))
  (INCF *logical-inferences*)
  (deref Val))

(DEFUN bind* (Var Val Continuation)
  (INCF *logical-inferences*)
  (PROGV (LIST Var) (LIST Val) (popstack Continuation)))

(DEFUN fail* (Continuation)
  (DECLARE (IGNORE Continuation))
  (INCF *logical-inferences*)
  NIL)

(DEFUN ==* (X Y Continuation)
  (lzy==* (lazyderef X) (lazyderef Y) Continuation))

(DEFUN lzy==* (X Y Continuation)
   (COND ((EQUAL X Y) (popstack Continuation))
         ((AND (CONSP X) (CONSP Y))
          (lzy==* (lazyderef (CAR X)) (lazyderef (CAR Y))
              (FUNCTION (LAMBDA () 
			(lzy==* (lazyderef (CDR X)) 
				(lazyderef (CDR Y)) 
  				 Continuation)))))
         (T NIL)))

(DEFUN =!* (X Y Continuation)
  (lzy=!* (lazyderef X) (lazyderef Y) Continuation))

(DEFUN lzy=!* (X Y Continuation)
   (COND ((EQUAL X Y) (popstack Continuation))
         ((AND (var? X) (EQ (occurs? X (deref Y)) 'false))
          (PROGV (LIST X) (LIST Y) (popstack Continuation)))
         ((AND (var? Y) (EQ (occurs? Y (deref X)) 'false))
          (PROGV (LIST Y) (LIST X) (popstack Continuation)))
         ((AND (CONSP X) (CONSP Y))
          (lzy=!* (lazyderef (CAR X)) (lazyderef (CAR Y))
              (FUNCTION (LAMBDA () 
			(lzy=!* (lazyderef (CDR X)) 
				(lazyderef (CDR Y)) 
  				 Continuation)))))
         (T NIL)))

(DEFUN bagof* (Var Pred Terms Out Continuation)
   (INCF *logical-inferences*)
   (LET ((Store (GENSYM "Store")))
          (SET Store NIL)
          (APPLY (concat Pred '*)
                    (APPEND Terms 
                               (LIST (FUNCTION (LAMBDA () (store* Var Store))))))
          (LET ((BAG (NREVERSE (SYMBOL-VALUE Store))))
                 (MAKUNBOUND Store)
                 (PROGV (LIST Out) 
                           (LIST BAG)
                           (popstack Continuation)))))               

(DEFUN store* (Var Store)
  (INCF *logical-inferences*)
  (SET Store (CONS (deref Var) (SYMBOL-VALUE Store)))
  NIL)

(DEFUN not* (Pred Terms Continuation)
  (IF (APPLY (concat Pred '*)  (APPEND Terms (LIST (FUNCTION (LAMBDA () T)))))
       NIL
       (popstack Continuation)))

(DEFUN call* (F Terms Continuation)
  (INCF *logical-inferences*)
  (APPLY (concat F '*) (APPEND Terms (LIST Continuation))))

(DEFUN typecheck* (FP1 FP2 Continuation)
 (OR
  (PROG2 (INCF *logical-inferences*)
   (when* (maximum_inferences_exceeded?)
    #'(LAMBDA NIL (eval* (error_max_inferences) Continuation))))
  (PROG2 (INCF *logical-inferences*)
   (when* (spy?)
    #'(LAMBDA NIL
       (when* (not (typing? (lazyderef FP1)))
        #'(LAMBDA NIL
           (show_checking* FP1 FP2 #'(LAMBDA NIL (fail* Continuation))))))))
  (PROG2 (INCF *logical-inferences*)
   (LET ((X10960 (lazyderef FP1)))
    (AND (CONSP X10960)
     (LET ((X (CAR X10960)))
      (LET ((X10961 (lazyderef (CDR X10960))))
       (AND (CONSP X10961)
        (LET ((X10962 (lazyderef (CAR X10961))))
         (AND (EQ X10962 '$$)
          (LET ((X10963 (lazyderef (CDR X10961))))
           (AND (CONSP X10963)
            (LET ((A (CAR X10963)))
             (LET ((X10964 (lazyderef (CDR X10963))))
              (AND (EQ X10964 NIL) (tt* X A FP2 Continuation))))))))))))))
  (PROG2 (INCF *logical-inferences*)
   (LET ((UserTypes (GENSYM "V")))
    (when* (not (typing? (lazyderef FP1)))
     #'(LAMBDA NIL
        (is* (lazyderef UserTypes) (usertypes)
         #'(LAMBDA NIL (try_usetypes* UserTypes FP1 FP2 Continuation)))))))))

(DEFUN tt* (FP1 FP2 FP3 Continuation)
 (PROG NIL
  (RETURN
   (OR
    (PROG2 (INCF *logical-inferences*)
     (when* (spy?)
      #'(LAMBDA NIL
         (show_checking* (LIST FP1 '$$ FP2) FP3
          #'(LAMBDA NIL (fail* Continuation))))))
    (PROG2 (INCF *logical-inferences*)
     (by_hypothesis* FP1 FP2 FP3 Continuation))
    (PROG2 (INCF *logical-inferences*)
     (LET ((B (GENSYM "V")))
      (when* (symbol? (lazyderef FP1))
       #'(LAMBDA NIL
          (is* (lazyderef B) (get_type_of_func (lazyderef FP1))
           #'(LAMBDA NIL
              (when* (cons? (lazyderef B))
               #'(LAMBDA NIL (=!* FP2 B Continuation)))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((B (GENSYM "V")))
      (when* (atom? (lazyderef FP1))
       #'(LAMBDA NIL
          (is* (lazyderef B) (map_base_type (lazyderef FP1))
           #'(LAMBDA NIL (=* FP2 B Continuation)))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X898 (lazyderef FP1)))
      (AND (EQ X898 NIL)
       (LET ((X899 (lazyderef FP2)))
        (IF (CONSP X899)
         (LET ((X900 (lazyderef (CAR X899))))
          (AND (EQ X900 'list)
           (LET ((X901 (lazyderef (CDR X899))))
            (AND (CONSP X901)
             (LET ((X902 (lazyderef (CDR X901))))
              (AND (EQ X902 NIL)
               (OR (popstack Continuation) (RETURN NIL))))))))
         (AND (var? X899)
          (PROGV (LIST X899) (LIST (CONS 'list (CONS (GENSYM "V") NIL)))
           (OR (popstack Continuation) (RETURN NIL)))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X903 (lazyderef FP1)))
      (AND (CONSP X903)
       (LET ((F (CAR X903)))
        (LET ((X904 (lazyderef (CDR X903))))
         (AND (CONSP X904)
          (LET ((X (CAR X904)))
           (LET ((X905 (lazyderef (CDR X904))))
            (AND (EQ X905 NIL)
             (LET ((A (GENSYM "V")))
              (tt* F (LIST A '--> FP2) FP3
               #'(LAMBDA NIL (tt* X A FP3 Continuation)))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X906 (lazyderef FP1)))
      (AND (CONSP X906)
       (LET ((X907 (lazyderef (CAR X906))))
        (AND (EQ X907 '@c)
         (LET ((X908 (lazyderef (CDR X906))))
          (AND (CONSP X908)
           (LET ((X (CAR X908)))
            (LET ((X909 (lazyderef (CDR X908))))
             (AND (CONSP X909)
              (LET ((Y (CAR X909)))
               (LET ((X910 (lazyderef (CDR X909))))
                (AND (EQ X910 NIL)
                 (LET ((X911 (lazyderef FP2)))
                  (IF (CONSP X911)
                   (LET ((X912 (lazyderef (CAR X911))))
                    (AND (EQ X912 'list)
                     (LET ((X913 (lazyderef (CDR X911))))
                      (AND (CONSP X913)
                       (LET ((A (CAR X913)))
                        (LET ((X914 (lazyderef (CDR X913))))
                         (AND (EQ X914 NIL)
                          (tt* X A FP3
                           #'(LAMBDA NIL
                              (tt* Y (LIST 'list A) FP3 Continuation))))))))))
                   (AND (var? X911)
                    (LET ((A (GENSYM "V")))
                     (PROGV (LIST X911) (LIST (CONS 'list (CONS A NIL)))
                      (tt* X A FP3
                       #'(LAMBDA NIL
                          (tt* Y (LIST 'list A) FP3
                           Continuation)))))))))))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X915 (lazyderef FP1)))
      (AND (CONSP X915)
       (LET ((X916 (lazyderef (CAR X915))))
        (AND (EQ X916 '@p)
         (LET ((X917 (lazyderef (CDR X915))))
          (AND (CONSP X917)
           (LET ((X (CAR X917)))
            (LET ((X918 (lazyderef (CDR X917))))
             (AND (CONSP X918)
              (LET ((Y (CAR X918)))
               (LET ((X919 (lazyderef (CDR X918))))
                (AND (EQ X919 NIL)
                 (LET ((X920 (lazyderef FP2)))
                  (IF (CONSP X920)
                   (LET ((A (CAR X920)))
                    (LET ((X921 (lazyderef (CDR X920))))
                     (AND (CONSP X921)
                      (LET ((X922 (lazyderef (CAR X921))))
                       (AND (EQ X922 '*)
                        (LET ((X923 (lazyderef (CDR X921))))
                         (AND (CONSP X923)
                          (LET ((B (CAR X923)))
                           (LET ((X924 (lazyderef (CDR X923))))
                            (AND (EQ X924 NIL)
                             (tt* X A FP3
                              #'(LAMBDA NIL
                                 (tt* Y B FP3 Continuation)))))))))))))
                   (AND (var? X920)
                    (LET ((A (GENSYM "V")) (B (GENSYM "V")))
                     (PROGV (LIST X920) (LIST (CONS A (CONS '* (CONS B NIL))))
                      (tt* X A FP3
                       #'(LAMBDA NIL
                          (tt* Y B FP3 Continuation)))))))))))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X925 (lazyderef FP1)))
      (AND (CONSP X925)
       (LET ((X926 (lazyderef (CAR X925))))
        (AND (EQ X926 'qi_=)
         (LET ((X927 (lazyderef (CDR X925))))
          (AND (CONSP X927)
           (LET ((X (CAR X927)))
            (LET ((X928 (lazyderef (CDR X927))))
             (AND (CONSP X928)
              (LET ((Y (CAR X928)))
               (LET ((X929 (lazyderef (CDR X928))))
                (AND (EQ X929 NIL)
                 (LET ((X930 (lazyderef FP2)))
                  (IF (EQ X930 'boolean)
                   (LET ((A (GENSYM "V")))
                    (OR (tt* X A FP3 #'(LAMBDA NIL (tt* Y A FP3 Continuation)))
                     (RETURN NIL)))
                   (AND (var? X930)
                    (PROGV (LIST X930) (LIST 'boolean)
                     (LET ((A (GENSYM "V")))
                      (OR
                       (tt* X A FP3 #'(LAMBDA NIL (tt* Y A FP3 Continuation)))
                       (RETURN NIL))))))))))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X931 (lazyderef FP1)))
      (AND (CONSP X931)
       (LET ((X932 (lazyderef (CAR X931))))
        (AND (EQ X932 'if)
         (LET ((X933 (lazyderef (CDR X931))))
          (AND (CONSP X933)
           (LET ((X (CAR X933)))
            (LET ((X934 (lazyderef (CDR X933))))
             (AND (CONSP X934)
              (LET ((Y (CAR X934)))
               (LET ((X935 (lazyderef (CDR X934))))
                (AND (CONSP X935)
                 (LET ((Z (CAR X935)))
                  (LET ((X936 (lazyderef (CDR X935))))
                   (AND (EQ X936 NIL)
                    (OR
                     (tt* X 'boolean FP3
                      #'(LAMBDA NIL
                         (OR
                          (popstack
                           #'(LAMBDA NIL
                              (tt* Y FP2 FP3
                               #'(LAMBDA NIL (tt* Z FP2 FP3 Continuation)))))
                          (RETURN NIL))))
                     (RETURN NIL))))))))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X937 (lazyderef FP1)))
      (AND (CONSP X937)
       (LET ((X938 (lazyderef (CAR X937))))
        (AND (EQ X938 'let)
         (LET ((X939 (lazyderef (CDR X937))))
          (AND (CONSP X939)
           (LET ((X (CAR X939)))
            (LET ((X940 (lazyderef (CDR X939))))
             (AND (CONSP X940)
              (LET ((Y (CAR X940)))
               (LET ((X941 (lazyderef (CDR X940))))
                (AND (CONSP X941)
                 (LET ((Z (CAR X941)))
                  (LET ((X942 (lazyderef (CDR X941))))
                   (AND (EQ X942 NIL)
                    (LET ((B (GENSYM "V")) (X* (GENSYM "V")) (Z* (GENSYM "V")))
                     (OR
                      (when* (variable? (lazyderef X))
                       #'(LAMBDA NIL
                          (tt* Y B FP3
                           #'(LAMBDA NIL
                              (is* (lazyderef X*) (arbterm)
                               #'(LAMBDA NIL
                                  (is* (lazyderef Z*)
                                   (substfree (lazyderef X*) (lazyderef X)
                                    (lazyderef Z))
                                   #'(LAMBDA NIL
                                      (tt* Z* FP2 (CONS (LIST X* '$$ B) FP3)
                                       Continuation)))))))))
                      (RETURN NIL)))))))))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X943 (lazyderef FP1)))
      (AND (CONSP X943)
       (LET ((X944 (lazyderef (CAR X943))))
        (AND (EQ X944 'set)
         (LET ((X945 (lazyderef (CDR X943))))
          (AND (CONSP X945)
           (LET ((X (CAR X945)))
            (LET ((X946 (lazyderef (CDR X945))))
             (AND (CONSP X946)
              (LET ((Y (CAR X946)))
               (LET ((X947 (lazyderef (CDR X946))))
                (AND (EQ X947 NIL)
                 (OR
                  (when* (symbol? (lazyderef X))
                   #'(LAMBDA NIL
                      (tt* (LIST 'value X) FP2 FP3
                       #'(LAMBDA NIL (tt* Y FP2 FP3 Continuation)))))
                  (RETURN NIL)))))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X948 (lazyderef FP1)))
      (AND (CONSP X948)
       (LET ((X949 (lazyderef (CAR X948))))
        (AND (EQ X949 '/.)
         (LET ((X950 (lazyderef (CDR X948))))
          (AND (CONSP X950)
           (LET ((X (CAR X950)))
            (LET ((X951 (lazyderef (CDR X950))))
             (AND (CONSP X951)
              (LET ((Y (CAR X951)))
               (LET ((X952 (lazyderef (CDR X951))))
                (AND (EQ X952 NIL)
                 (LET ((X953 (lazyderef FP2)))
                  (IF (CONSP X953)
                   (LET ((A (CAR X953)))
                    (LET ((X954 (lazyderef (CDR X953))))
                     (AND (CONSP X954)
                      (LET ((X955 (lazyderef (CAR X954))))
                       (AND (EQ X955 '-->)
                        (LET ((X956 (lazyderef (CDR X954))))
                         (AND (CONSP X956)
                          (LET ((B (CAR X956)))
                           (LET ((X957 (lazyderef (CDR X956))))
                            (AND (EQ X957 NIL)
                             (LET ((X* (GENSYM "V")) (Y* (GENSYM "V")))
                              (OR
                               (when* (variable? (lazyderef X))
                                #'(LAMBDA NIL
                                   (is* (lazyderef X*) (arbterm)
                                    #'(LAMBDA NIL
                                       (is* (lazyderef Y*)
                                        (substfree (lazyderef X*) (lazyderef X)
                                         (lazyderef Y))
                                        #'(LAMBDA NIL
                                           (tt* Y* B (CONS (LIST X* '$$ A) FP3)
                                            Continuation)))))))
                               (RETURN NIL)))))))))))))
                   (AND (var? X953)
                    (LET ((A (GENSYM "V")) (B (GENSYM "V")))
                     (PROGV (LIST X953)
                      (LIST (CONS A (CONS '--> (CONS B NIL))))
                      (LET ((X* (GENSYM "V")) (Y* (GENSYM "V")))
                       (OR
                        (when* (variable? (lazyderef X))
                         #'(LAMBDA NIL
                            (is* (lazyderef X*) (arbterm)
                             #'(LAMBDA NIL
                                (is* (lazyderef Y*)
                                 (substfree (lazyderef X*) (lazyderef X)
                                  (lazyderef Y))
                                 #'(LAMBDA NIL
                                    (tt* Y* B (CONS (LIST X* '$$ A) FP3)
                                     Continuation)))))))
                        (RETURN NIL)))))))))))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X958 (lazyderef FP1)))
      (AND (CONSP X958)
       (LET ((F (CAR X958)))
        (LET ((X959 (lazyderef (CDR X958))))
         (AND (CONSP X959)
          (LET ((String (CAR X959)))
           (LET ((Y (CDR X959)))
            (LET ((X960 (lazyderef FP2)))
             (IF (EQ X960 'string)
              (member* F (CONS 'output (CONS 'make-string NIL))
               #'(LAMBDA NIL
                  (OR
                   (popstack
                    #'(LAMBDA NIL
                       (tt* String 'string FP3
                        #'(LAMBDA NIL (all_typed?* Y FP3 Continuation)))))
                   (RETURN NIL))))
              (AND (var? X960)
               (PROGV (LIST X960) (LIST 'string)
                (member* F (CONS 'output (CONS 'make-string NIL))
                 #'(LAMBDA NIL
                    (OR
                     (popstack
                      #'(LAMBDA NIL
                         (tt* String 'string FP3
                          #'(LAMBDA NIL (all_typed?* Y FP3 Continuation)))))
                     (RETURN NIL))))))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X961 (lazyderef FP1)))
      (AND (CONSP X961)
       (LET ((X962 (lazyderef (CAR X961))))
        (AND (EQ X962 'error)
         (LET ((X963 (lazyderef (CDR X961))))
          (AND (CONSP X963)
           (LET ((String (CAR X963)))
            (LET ((Y (CDR X963)))
             (tt* String 'string FP3
              #'(LAMBDA NIL
                 (OR (popstack #'(LAMBDA NIL (all_typed?* Y FP3 Continuation)))
                  (RETURN NIL)))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X964 (lazyderef FP1)))
      (AND (CONSP X964)
       (LET ((X965 (lazyderef (CAR X964))))
        (AND (EQ X965 'input+)
         (LET ((X966 (lazyderef (CDR X964))))
          (AND (CONSP X966)
           (LET ((X967 (lazyderef (CAR X966))))
            (AND (EQ X967 '$$)
             (LET ((X968 (lazyderef (CDR X966))))
              (AND (CONSP X968)
               (LET ((A (CAR X968)))
                (LET ((X969 (lazyderef (CDR X968))))
                 (AND (EQ X969 NIL)
                  (OR (monomorphic?* A #'(LAMBDA NIL (=* A FP2 Continuation)))
                   (RETURN NIL))))))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X970 (lazyderef FP1)))
      (AND (CONSP X970)
       (LET ((X971 (lazyderef (CAR X970))))
        (AND (EQ X971 'do)
         (LET ((X972 (lazyderef (CDR X970))))
          (AND (CONSP X972)
           (LET ((X (CAR X972)))
            (LET ((X973 (lazyderef (CDR X972))))
             (AND (EQ X973 NIL)
              (OR (tt* X FP2 FP3 Continuation) (RETURN NIL))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X974 (lazyderef FP1)))
      (AND (CONSP X974)
       (LET ((X975 (lazyderef (CAR X974))))
        (AND (EQ X975 'do)
         (LET ((X976 (lazyderef (CDR X974))))
          (AND (CONSP X976)
           (LET ((X (CAR X976)))
            (LET ((X977 (lazyderef (CDR X976))))
             (AND (CONSP X977)
              (LET ((Y (CAR X977)))
               (LET ((Z (CDR X977)))
                (LET ((A (GENSYM "V")))
                 (OR
                  (tt* X A FP3
                   #'(LAMBDA NIL
                      (tt* (CONS 'do (CONS Y Z)) FP2 FP3 Continuation)))
                  (RETURN NIL)))))))))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((NewContext (GENSYM "V")))
      (split_cons* FP3 NewContext
       #'(LAMBDA NIL
          (OR (popstack #'(LAMBDA NIL (tt* FP1 FP2 NewContext Continuation)))
           (RETURN NIL))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((NewContext (GENSYM "V")))
      (split_pair* FP3 NewContext
       #'(LAMBDA NIL
          (OR (popstack #'(LAMBDA NIL (tt* FP1 FP2 NewContext Continuation)))
           (RETURN NIL))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((UserTypes (GENSYM "V")))
      (is* (lazyderef UserTypes) (usertypes)
       #'(LAMBDA NIL
          (try_usetypes* UserTypes (CONS FP1 (CONS '$$ (CONS FP2 NIL))) FP3
           Continuation)))))))))

(DEFUN all_typed?* (FP1 FP2 Continuation)
 (OR
  (PROG2 (INCF *logical-inferences*)
   (LET ((X11045 (lazyderef FP1)))
    (AND (EQ X11045 NIL) (popstack Continuation))))
  (PROG2 (INCF *logical-inferences*)
   (LET ((X11046 (lazyderef FP1)))
    (AND (CONSP X11046)
     (LET ((X (CAR X11046)))
      (LET ((Y (CDR X11046)))
       (LET ((A (GENSYM "V")))
        (all_typed?* Y FP2 #'(LAMBDA NIL (tt* X A FP2 Continuation)))))))))))

(DEFUN split_cons* (FP1 FP2 Continuation)
 (OR
  (PROG2 (INCF *logical-inferences*)
   (LET ((X11047 (lazyderef FP1)))
    (AND (CONSP X11047)
     (LET ((X11048 (lazyderef (CAR X11047))))
      (AND (CONSP X11048)
       (LET ((X11049 (lazyderef (CAR X11048))))
        (AND (CONSP X11049)
         (LET ((X11050 (lazyderef (CAR X11049))))
          (AND (EQ X11050 '@c)
           (LET ((X11051 (lazyderef (CDR X11049))))
            (AND (CONSP X11051)
             (LET ((X (CAR X11051)))
              (LET ((X11052 (lazyderef (CDR X11051))))
               (AND (CONSP X11052)
                (LET ((Y (CAR X11052)))
                 (LET ((X11053 (lazyderef (CDR X11052))))
                  (AND (EQ X11053 NIL)
                   (LET ((X11054 (lazyderef (CDR X11048))))
                    (AND (CONSP X11054)
                     (LET ((X11055 (lazyderef (CAR X11054))))
                      (AND (EQ X11055 '$$)
                       (LET ((X11056 (lazyderef (CDR X11054))))
                        (AND (CONSP X11056)
                         (LET ((X11057 (lazyderef (CAR X11056))))
                          (AND (CONSP X11057)
                           (LET ((X11058 (lazyderef (CAR X11057))))
                            (AND (EQ X11058 'list)
                             (LET ((X11059 (lazyderef (CDR X11057))))
                              (AND (CONSP X11059)
                               (LET ((A (CAR X11059)))
                                (LET ((X11060 (lazyderef (CDR X11059))))
                                 (AND (EQ X11060 NIL)
                                  (LET ((X11061 (lazyderef (CDR X11056))))
                                   (AND (EQ X11061 NIL)
                                    (LET ((Context (CDR X11047)))
                                     (bind* FP2
                                      (CONS (LIST X '$$ A)
                                       (CONS (LIST Y '$$ (LIST 'list A))
                                        Context))
                                      Continuation))))))))))))))))))))))))))))))))))))
  (PROG2 (INCF *logical-inferences*)
   (LET ((X11063 (lazyderef FP1)))
    (AND (CONSP X11063)
     (LET ((V11062 (CAR X11063)))
      (LET ((Context (CDR X11063)))
       (LET ((X11064 (lazyderef FP2)))
        (IF (CONSP X11064)
         (LET ((P (CAR X11064)))
          (LET ((NewContext (CDR X11064)))
           (=* P V11062
            #'(LAMBDA NIL (split_cons* Context NewContext Continuation)))))
         (AND (var? X11064)
          (LET ((P (GENSYM "V")) (NewContext (GENSYM "V")))
           (PROGV (LIST X11064) (LIST (CONS P NewContext))
            (=* P V11062
             #'(LAMBDA NIL
                (split_cons* Context NewContext Continuation)))))))))))))))

(DEFUN split_pair* (FP1 FP2 Continuation)
 (OR
  (PROG2 (INCF *logical-inferences*)
   (LET ((X11065 (lazyderef FP1)))
    (AND (CONSP X11065)
     (LET ((X11066 (lazyderef (CAR X11065))))
      (AND (CONSP X11066)
       (LET ((X11067 (lazyderef (CAR X11066))))
        (AND (CONSP X11067)
         (LET ((X11068 (lazyderef (CAR X11067))))
          (AND (EQ X11068 '@p)
           (LET ((X11069 (lazyderef (CDR X11067))))
            (AND (CONSP X11069)
             (LET ((X (CAR X11069)))
              (LET ((X11070 (lazyderef (CDR X11069))))
               (AND (CONSP X11070)
                (LET ((Y (CAR X11070)))
                 (LET ((X11071 (lazyderef (CDR X11070))))
                  (AND (EQ X11071 NIL)
                   (LET ((X11072 (lazyderef (CDR X11066))))
                    (AND (CONSP X11072)
                     (LET ((X11073 (lazyderef (CAR X11072))))
                      (AND (EQ X11073 '$$)
                       (LET ((X11074 (lazyderef (CDR X11072))))
                        (AND (CONSP X11074)
                         (LET ((X11075 (lazyderef (CAR X11074))))
                          (AND (CONSP X11075)
                           (LET ((A (CAR X11075)))
                            (LET ((X11076 (lazyderef (CDR X11075))))
                             (AND (CONSP X11076)
                              (LET ((X11077 (lazyderef (CAR X11076))))
                               (AND (EQ X11077 '*)
                                (LET ((X11078 (lazyderef (CDR X11076))))
                                 (AND (CONSP X11078)
                                  (LET ((B (CAR X11078)))
                                   (LET ((X11079 (lazyderef (CDR X11078))))
                                    (AND (EQ X11079 NIL)
                                     (LET ((X11080 (lazyderef (CDR X11074))))
                                      (AND (EQ X11080 NIL)
                                       (LET ((Context (CDR X11065)))
                                        (bind* FP2
                                         (CONS (LIST X '$$ A)
                                          (CONS (LIST Y '$$ B) Context))
                                         Continuation)))))))))))))))))))))))))))))))))))))))
  (PROG2 (INCF *logical-inferences*)
   (LET ((X11082 (lazyderef FP1)))
    (AND (CONSP X11082)
     (LET ((V11081 (CAR X11082)))
      (LET ((Context (CDR X11082)))
       (LET ((X11083 (lazyderef FP2)))
        (IF (CONSP X11083)
         (LET ((P (CAR X11083)))
          (LET ((NewContext (CDR X11083)))
           (=* P V11081
            #'(LAMBDA NIL (split_pair* Context NewContext Continuation)))))
         (AND (var? X11083)
          (LET ((P (GENSYM "V")) (NewContext (GENSYM "V")))
           (PROGV (LIST X11083) (LIST (CONS P NewContext))
            (=* P V11081
             #'(LAMBDA NIL
                (split_pair* Context NewContext Continuation)))))))))))))))

(DEFUN by_hypothesis* (FP1 FP2 FP3 Continuation)
 (OR
  (PROG2 (INCF *logical-inferences*)
   (LET ((X322 (lazyderef FP3)))
    (AND (CONSP X322)
     (LET ((X323 (lazyderef (CAR X322))))
      (AND (CONSP X323)
       (LET ((Y (CAR X323)))
        (LET ((X324 (lazyderef (CDR X323))))
         (AND (CONSP X324)
          (LET ((X325 (lazyderef (CAR X324))))
           (AND (EQ X325 '$$)
            (LET ((X326 (lazyderef (CDR X324))))
             (AND (CONSP X326)
              (LET ((B (CAR X326)))
               (LET ((X327 (lazyderef (CDR X326))))
                (AND (EQ X327 NIL)
                 (==* FP1 Y
                  #'(LAMBDA NIL (=!* FP2 B Continuation))))))))))))))))))
  (PROG2 (INCF *logical-inferences*)
   (LET ((X328 (lazyderef FP3)))
    (AND (CONSP X328)
     (LET ((Context (CDR X328)))
      (by_hypothesis* FP1 FP2 Context Continuation)))))))

(DEFUN member* (FP1 FP2 Continuation)
 (OR
  (PROG2 (INCF *logical-inferences*)
   (LET ((X11092 (lazyderef FP2)))
    (AND (CONSP X11092) (LET ((X (CAR X11092))) (=* X FP1 Continuation)))))
  (PROG2 (INCF *logical-inferences*)
   (LET ((X11093 (lazyderef FP2)))
    (AND (CONSP X11093)
     (LET ((Y (CDR X11093))) (member* FP1 Y Continuation)))))))

(DEFUN monomorphic?* (FP1 Continuation)
 (PROG NIL
  (RETURN
   (OR
    (PROG2 (INCF *logical-inferences*)
     (LET ((X11094 (lazyderef FP1)))
      (AND (CONSP X11094)
       (LET ((X (CAR X11094)))
        (LET ((Y (CDR X11094)))
         (monomorphic?* X
          #'(LAMBDA NIL
             (OR (popstack #'(LAMBDA NIL (monomorphic?* Y Continuation)))
              (RETURN NIL)))))))))
    (PROG2 (INCF *logical-inferences*)
     (LET ((X11095 (lazyderef FP1)))
      (AND (EQ X11095 NIL) (popstack Continuation))))
    (PROG2 (INCF *logical-inferences*)
     (when* (atom? (lazyderef FP1))
      #'(LAMBDA NIL
         (when* (not (variable? (lazyderef FP1))) Continuation))))))))

(DEFUN try_usetypes* (FP1 FP2 FP3 Continuation)
 (OR
  (PROG2 (INCF *logical-inferences*)
   (LET ((X11096 (lazyderef FP1)))
    (AND (CONSP X11096)
     (LET ((UserType (CAR X11096)))
      (call* UserType (LIST FP2 FP3) Continuation)))))
  (PROG2 (INCF *logical-inferences*)
   (LET ((X11097 (lazyderef FP1)))
    (AND (CONSP X11097)
     (LET ((UserTypes (CDR X11097)))
      (try_usetypes* UserTypes FP2 FP3 Continuation)))))))

(DEFUN show_checking* (FP1 FP2 Continuation)
 (OR
  (PROG2 (INCF *logical-inferences*)
   (eval* (print_line)
    #'(LAMBDA NIL
       (eval* (display_formula (deref FP1))
        #'(LAMBDA NIL
           (eval* (carriage_return)
            #'(LAMBDA NIL
               (eval* (map 'display_formula (deref FP2))
                #'(LAMBDA NIL (eval* (wait_for_user) Continuation))))))))))))

(DEFUN typing? (V477)
 (COND
  ((AND (CONSP V477) (CONSP (CDR V477))
    (EQ '$$ (CAR (CDR V477))) (CONSP (CDR (CDR V477)))
    (NULL (CDR (CDR (CDR V477)))))
   'true)
  (T 'false)))

(DEFUN usertypes () *usertypes*)

(SETQ *usertypes* NIL)
(SETQ *all-usertypes* NIL)

(DEFUN substfree (V1023 V1024 V1025)
 (COND ((wrapper (qi_= V1024 V1025)) V1023)
  ((AND (CONSP V1025) (EQ '/. (CAR V1025)) (CONSP (CDR V1025))
    (CONSP (CDR (CDR V1025))) (NULL (CDR (CDR (CDR V1025))))
    (wrapper (qi_= V1024 (CAR (CDR V1025)))))
   V1025)
  ((AND (CONSP V1025) (EQ 'let (CAR V1025)) (CONSP (CDR V1025))
    (CONSP (CDR (CDR V1025))) (CONSP (CDR (CDR (CDR V1025))))
    (NULL (CDR (CDR (CDR (CDR V1025)))))
    (wrapper (qi_= V1024 (CAR (CDR V1025)))))
   V1025)
  ((CONSP V1025)
   (CONS (substfree V1023 V1024 (CAR V1025))
    (substfree V1023 V1024 (CDR V1025))))
  (T V1025)))

(DEFUN arbterm () (THE SYMBOL (gensym "&&x")))

(DEFUN spy? NIL *spy*)

(DEFUN display_formula (V1026)
 (COND
  ((AND (CONSP V1026) (CONSP (CDR V1026))
    (EQ '$$ (CAR (CDR V1026)))
    (CONSP (CDR (CDR V1026)))
    (NULL (CDR (CDR (CDR V1026)))))
    (pretty (CAR V1026) "") 
    (FORMAT T " : ~A~%" (CAR (CDR (CDR V1026)))))
  (T (pretty V1026 "") (TERPRI))))

(DEFUN pretty (V889 V890)
 (COND
  ((AND (CONSP V889) (EQ '@c (CAR V889)) (CONSP (CDR V889))
    (CONSP (CDR (CDR V889))) (NULL (CDR (CDR (CDR V889)))))
   (LET* ((V891 (CDR V889)))
     (output "~A[" V890) (pretty (CAR V891) "")
     (mapit #'(LAMBDA (Z) (pretty Z " ")) (elim_@c (CAR (CDR V891))))
     (output "]")))
  ((AND (CONSP V889) (CONSP (CDR V889)) (EQ '$$ (CAR (CDR V889)))
    (CONSP (CDR (CDR V889))) (NULL (CDR (CDR (CDR V889)))))
    (pretty (CAR V889) V890) (FORMAT T " : ")
    (pretty (CAR (CDR (CDR V889))) ""))
  ((CONSP V889)
   (FORMAT T "~A(" V890) 
   (pretty (CAR V889) "")
   (MAPCAR #'(LAMBDA (Z) (pretty Z " ")) (CDR V889)) 
   (FORMAT T ")"))
  ((wrapper (string? V889)) (output "~A" V890) (WRITE V889))
  (T (output "~A~S" V890 (strip_&& V889)))))

(DEFUN mapit (V1032 V1033)
 (COND ((CONSP V1033) (THE LIST (map V1032 V1033)))
  (T (FORMAT T " |") (apply V1032 V1033))))

(DEFUN strip_&& (V1038) (strip_&&_help (THE LIST (explode V1038)) V1038))

(DEFUN strip_&&_help (V1046 V1047)
 (COND
  ((AND (CONSP V1046) (EQUAL #\& (CAR V1046)) (CONSP (CDR V1046))
    (EQUAL #\& (CAR (CDR V1046))))
   (READ-FROM-STRING (COERCE (CDR (CDR V1046)) 'STRING)))
  (T V1047)))

(DEFUN elim_@c (V1048)
 (COND
  ((AND (CONSP V1048) (EQ '@c (CAR V1048)) (CONSP (CDR V1048))
    (CONSP (CDR (CDR V1048))) (NULL (CDR (CDR (CDR V1048)))))
   (LET* ((V1049 (CDR V1048))) (CONS (CAR V1049) (elim_@c (CAR (CDR V1049))))))
  (T V1048)))

(DEFUN carriage_return () (TERPRI))

(DEFUN wait_for_user ()
 (FORMAT T "~%> ")
  (LET ((Char (READ-CHAR)))
   (IF (EQ Char #\^) (error "aborted~%") 'ok)))

(DEFUN print_line ()
 (FORMAT T
  "______________________________________________ ~A inference~P~%?- "
  *logical-inferences* *logical-inferences*))

(DEFUN maximum_inferences_exceeded? ()
 (THE SYMBOL (qi_> *logical-inferences* *maxinferences*)))

(DEFUN error_max_inferences () (error "maximum inferences exceeded~%"))

(DEFMACRO abstype (Type Rules Defs) 
 `(PROGV '(*tc*) '(false) (abstype* (QUOTE ,Type) (QUOTE ,Rules) (QUOTE ,Defs))))

(DEFUN abstype* (V941 V942 V943)
 (COND
  ((AND (CONSP V942) 
          (EQ ':types (CAR V942)) 
          (CONSP V943)
         (EQ ':defs (CAR V943)))
   (process_type_declarations (CDR V942)) 
   (MAPC 'EVAL (CDR V943))
    V941)
  (T (implementation_error 'abstype*))))

(DEFUN process_type_declarations (V5374)
 (COND ((NULL V5374) NIL)
  ((AND (CONSP V5374) (CONSP (CDR V5374)) (EQ '$$ (CAR (CDR V5374)))
    (CONSP (CDR (CDR V5374))))
   (LET* ((V5375 (CDR V5374)) (V5376 (CDR V5375)))
     (newfuntype* (CAR V5374) (CAR V5376))
     (process_type_declarations (CDR V5376))))
  (T (ERROR "Odd number of type declarations in abstract datatype.~%"))))

(DEFUN preclude (V5377)
 (SETQ *usertypes* (difference *usertypes* (validate-usertypes V5377) )))

(DEFUN include (V5378)
 (SETQ *usertypes* (THE LIST (union (validate-usertypes V5378) *usertypes*))))

(DEFUN preclude-all-but (V5380) (SETQ *usertypes* (validate-usertypes V5380)))

(DEFUN include-all-but (V5381)
 (SETQ *usertypes*
  (THE LIST (difference *all-usertypes* (validate-usertypes V5381)))))

(DEFUN validate-usertypes (V5387)
 (COND ((NULL V5387) NIL)
  ((AND (CONSP V5387) (wrapper (valid-usertype? (CAR V5387))))
   (CONS (CAR V5387) (validate-usertypes (CDR V5387))))
  ((CONSP V5387) (error "~A is not a user type~%" (CAR V5387)))
  (T (implementation_error 'validate-usertypes))))

(DEFUN valid-usertype? (V5388) (THE SYMBOL (element? V5388 *all-usertypes*)))

(DEFMACRO datatype (&REST X) (LIST 'datatype_help (LIST 'QUOTE X)))

(DEFUN datatype_help (DataType)
  (LET ((DataTypeName (CAR DataType)))
         (compile '<datatype_definition> DataType)
         (warn-if-defined DataTypeName)
         (PUSHNEW DataTypeName *usertypes*)
         (PUSHNEW DataTypeName *all-usertypes*)
         DataTypeName))

(DEFUN warn-if-defined (DataTypeName)
 (IF (MEMBER DataTypeName *all-usertypes*)
     (warn (FORMAT NIL "redefining ~A can cause type errors." DataTypeName))))

(DEFMACRO theory (&REST X) (LIST 'theory_help (LIST 'QUOTE X)))

(DEFUN theory_help (Theory)
  (PROGV '(*tc*) '(false)
  (compile '<theory> Theory)
  (CAR Theory)))

(DEFUN <datatype_definition> (Stream)
 (cases
  (LET ((<lowercase> (<lowercase> Stream)))
   (IF (NOT (failure? <lowercase>))
    (LET ((<A-rules> (<A-rules> <lowercase>)))
     (IF (NOT (failure? <A-rules>))
      (LET ((<end> (<end> <A-rules>)))
       (IF (NOT (failure? <end>))
        (LIST (FIRST <end>)
         (compile-datatype (SECOND <lowercase>) (SECOND <A-rules>)))
        (LIST NIL #\Escape)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<lowercase> (<lowercase> Stream)))
   (IF (NOT (failure? <lowercase>))
    (LET ((<A-rules> (<A-rules> <lowercase>)))
     (IF (NOT (failure? <A-rules>))
      (LET ((<datatype_error> (<datatype_error> <A-rules>)))
       (IF (NOT (failure? <datatype_error>))
        (LIST (FIRST <datatype_error>)
         (APPEND (SECOND <lowercase>)
          (APPEND (SECOND <A-rules>) (APPEND (SECOND <datatype_error>) NIL))))
        (LIST NIL #\Escape)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<lowercase> (<lowercase> Stream)))
   (IF (NOT (failure? <lowercase>))
    (LET ((<datatype_error> (<datatype_error> <lowercase>)))
     (IF (NOT (failure? <datatype_error>))
      (LIST (FIRST <datatype_error>)
       (APPEND (SECOND <lowercase>) (APPEND (SECOND <datatype_error>) NIL)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<datatype_error> (<datatype_error> Stream)))
   (IF (NOT (failure? <datatype_error>))
    (LIST (FIRST <datatype_error>) (APPEND (SECOND <datatype_error>) NIL))
    (LIST NIL #\Escape)))))

(DEFUN <theory> (Stream)
 (cases
  (LET ((<lowercase> (<lowercase> Stream)))
   (IF (NOT (failure? <lowercase>))
    (LET ((<B-rules> (<B-rules> <lowercase>)))
     (IF (NOT (failure? <B-rules>))
      (LET ((<end> (<end> <B-rules>)))
       (IF (NOT (failure? <end>))
        (LIST (FIRST <end>)
         (compile-theory (SECOND <lowercase>) (SECOND <B-rules>)))
        (LIST NIL #\Escape)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<lowercase> (<lowercase> Stream)))
   (IF (NOT (failure? <lowercase>))
    (LET ((<B-rules> (<B-rules> <lowercase>)))
     (IF (NOT (failure? <B-rules>))
      (LET ((<theory_error> (<theory_error> <B-rules>)))
       (IF (NOT (failure? <theory_error>))
        (LIST (FIRST <theory_error>)
         (APPEND (SECOND <lowercase>)
          (APPEND (SECOND <B-rules>) (APPEND (SECOND <theory_error>) NIL))))
        (LIST NIL #\Escape)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<lowercase> (<lowercase> Stream)))
   (IF (NOT (failure? <lowercase>))
    (LET ((<theory_error> (<theory_error> <lowercase>)))
     (IF (NOT (failure? <theory_error>))
      (LIST (FIRST <theory_error>)
       (APPEND (SECOND <lowercase>) (APPEND (SECOND <theory_error>) NIL)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<theory_error> (<theory_error> Stream)))
   (IF (NOT (failure? <theory_error>))
    (LIST (FIRST <theory_error>) (APPEND (SECOND <theory_error>) NIL))
    (LIST NIL #\Escape)))))

(DEFUN <end> (V532)
 (COND
  ((AND (CONSP V532) (wrapper (qi_= NIL (CAR V532))) (CONSP (CDR V532))
    (wrapper (qi_= NIL (CDR (CDR V532)))))
   V532)
  ((AND (CONSP V532) (CONSP (CDR V532)) (wrapper (qi_= NIL (CDR (CDR V532)))))
   (CONS (CAR V532) (CONS *qi-failure-object* NIL)))
  (T (implementation_error '<end>))))

(DEFUN <datatype_error> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
    (error "Syntax error in datatype here;~%~% ~A"
     (cons (CAAR Stream) (CDAR Stream))))
   (LIST NIL #\Escape))))

(DEFUN <theory_error> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
    (error "Syntax error in theory here;~%~% ~A"
     (cons (CAAR Stream) (CDAR Stream))))
   (LIST NIL #\Escape))))

(DEFUN <A-rules> (Stream)
 (cases
  (LET ((<A-rule> (<A-rule> Stream)))
   (IF (NOT (failure? <A-rule>))
    (LET ((<A-rules> (<A-rules> <A-rule>)))
     (IF (NOT (failure? <A-rules>))
      (LIST (FIRST <A-rules>) (cons (SECOND <A-rule>) (SECOND <A-rules>)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<A-rule> (<A-rule> Stream)))
   (IF (NOT (failure? <A-rule>))
    (LIST (FIRST <A-rule>) (cons (SECOND <A-rule>) NIL))
    (LIST NIL #\Escape)))))

(DEFUN raise-datatype-syntax-error (V533)
 (error "Syntax error in datatype definition beginning here ~%~% ~A" V533))

(DEFUN <B-rules> (Stream)
 (cases
  (LET ((<B-rule> (<B-rule> Stream)))
   (IF (NOT (failure? <B-rule>))
    (LET ((<B-rules> (<B-rules> <B-rule>)))
     (IF (NOT (failure? <B-rules>))
      (LIST (FIRST <B-rules>) (cons (SECOND <B-rule>) (SECOND <B-rules>)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<B-rule> (<B-rule> Stream)))
   (IF (NOT (failure? <B-rule>))
    (LIST (FIRST <B-rule>) (cons (SECOND <B-rule>) NIL))
    (LIST NIL #\Escape)))))

(DEFUN raise-theory-syntax-error (V534)
 (error "Syntax error in datatype definition beginning here ~%~% ~A" V534))

(DEFUN <A-rule> (Stream)
 (cases
  (LET ((<A-preamble> (<A-preamble> Stream)))
   (IF (NOT (failure? <A-preamble>))
    (LET ((<sequents> (<sequents> <A-preamble>)))
     (IF (NOT (failure? <sequents>))
      (LET ((<underline> (<underline> <sequents>)))
       (IF (NOT (failure? <underline>))
        (LET ((<sequent> (<sequent> <underline>)))
         (IF (NOT (failure? <sequent>))
          (LET ((<semi-colon> (<semi-colon> <sequent>)))
           (IF (NOT (failure? <semi-colon>))
            (LIST (FIRST <semi-colon>)
             (cons (SECOND <A-preamble>)
              (cons (SECOND <sequents>)
               (cons 'underline (cons (SECOND <sequent>) NIL)))))
            (LIST NIL #\Escape)))
          (LIST NIL #\Escape)))
        (LIST NIL #\Escape)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<A-preamble> (<A-preamble> Stream)))
   (IF (NOT (failure? <A-preamble>))
    (LET ((<simple_sequents> (<simple_sequents> <A-preamble>)))
     (IF (NOT (failure? <simple_sequents>))
      (LET ((<double_underline> (<double_underline> <simple_sequents>)))
       (IF (NOT (failure? <double_underline>))
        (LET ((<simple_sequent> (<simple_sequent> <double_underline>)))
         (IF (NOT (failure? <simple_sequent>))
          (LIST (FIRST <simple_sequent>)
           (cons (SECOND <A-preamble>)
            (cons (SECOND <simple_sequents>)
             (cons 'doubleunderline (cons (SECOND <simple_sequent>) NIL)))))
          (LIST NIL #\Escape)))
        (LIST NIL #\Escape)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))))

(DEFUN <B-rule> (Stream)
 (cases
  (LET ((<B-preamble> (<B-preamble> Stream)))
   (IF (NOT (failure? <B-preamble>))
    (LET ((<sequents> (<sequents> <B-preamble>)))
     (IF (NOT (failure? <sequents>))
      (LET ((<underline> (<underline> <sequents>)))
       (IF (NOT (failure? <underline>))
        (LET ((<sequent> (<sequent> <underline>)))
         (IF (NOT (failure? <sequent>))
          (LET ((<semi-colon> (<semi-colon> <sequent>)))
           (IF (NOT (failure? <semi-colon>))
            (LIST (FIRST <semi-colon>)
             (cons (SECOND <B-preamble>)
              (cons (SECOND <sequents>)
               (cons 'underline (cons (SECOND <sequent>) NIL)))))
            (LIST NIL #\Escape)))
          (LIST NIL #\Escape)))
        (LIST NIL #\Escape)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))))

(DEFUN <A-preamble> (Stream)
 (cases
  (IF (AND (CONSP (FIRST Stream)) (EQUAL (FIRST (FIRST Stream)) 'commit!))
   (LET
    ((<side-conditions>
      (<side-conditions> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
    (IF (NOT (failure? <side-conditions>))
     (LIST (FIRST <side-conditions>)
      (append (SECOND <side-conditions>) (cons 'commit! NIL)))
     (LIST NIL #\Escape)))
   (LIST NIL #\Escape))
  (LET ((<side-conditions> (<side-conditions> Stream)))
   (IF (NOT (failure? <side-conditions>))
    (LIST (FIRST <side-conditions>) (SECOND <side-conditions>))
    (LIST NIL #\Escape)))))

(DEFUN <B-preamble> (Stream)
 (cases
  (LET ((<title> (<title> Stream)))
   (IF (NOT (failure? <title>))
    (LET ((<side-conditions> (<side-conditions> <title>)))
     (IF (NOT (failure? <side-conditions>))
      (LIST (FIRST <side-conditions>)
       (cons (SECOND <title>) (SECOND <side-conditions>)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<side-conditions> (<side-conditions> Stream)))
   (IF (NOT (failure? <side-conditions>))
    (LIST (FIRST <side-conditions>) (SECOND <side-conditions>))
    (LIST NIL #\Escape)))))

(DEFUN <title> (Stream)
 (cases
  (IF (AND (CONSP (FIRST Stream)) (EQUAL (FIRST (FIRST Stream)) 'name))
   (LET
    ((<lowercase> (<lowercase> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
    (IF (NOT (failure? <lowercase>))
     (LIST (FIRST <lowercase>) (cons 'name (cons (SECOND <lowercase>) NIL)))
     (LIST NIL #\Escape)))
   (LIST NIL #\Escape))
  (IF (AND (CONSP (FIRST Stream)) (EQUAL (FIRST (FIRST Stream)) 'name))
   (IF
    (AND (CONSP (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
     (CONSP (CAAR (LIST (REST (FIRST Stream)) (SECOND Stream)))))
    (LET
     ((HeadStream535
       (LIST (CAAR (LIST (REST (FIRST Stream)) (SECOND Stream)))
        (CADR (LIST (REST (FIRST Stream)) (SECOND Stream)))))
      (TailStream536
       (LIST (CDAR (LIST (REST (FIRST Stream)) (SECOND Stream)))
        (CADR (LIST (REST (FIRST Stream)) (SECOND Stream))))))
     (LET ((<lowercase> (<lowercase> HeadStream535)))
      (IF (NOT (failure? <lowercase>))
       (LET ((<natnum> (<natnum> <lowercase>)))
        (IF (NOT (failure? <natnum>))
         (IF (NULL (FIRST <natnum>))
          (LIST (FIRST TailStream536)
           (cons 'name
            (cons (cons (SECOND <lowercase>) (cons (SECOND <natnum>) NIL))
             NIL)))
          (LIST NIL #\Escape))
         (LIST NIL #\Escape)))
       (LIST NIL #\Escape))))
    (LIST NIL #\Escape))
   (LIST NIL #\Escape))))

(DEFUN <natnum> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
    (if (and (integer? (CAAR Stream)) (qi_> (CAAR Stream) 0)) (CAAR Stream)
     #\Escape))
   (LIST NIL #\Escape))))

(DEFUN <lowercase> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
    (if (and (symbol? (CAAR Stream)) (not (variable? (CAAR Stream))))
     (CAAR Stream) #\Escape))
   (LIST NIL #\Escape))))

(DEFUN <side-conditions> (Stream)
 (cases
  (LET ((<side-condition> (<side-condition> Stream)))
   (IF (NOT (failure? <side-condition>))
    (LET ((<side-conditions> (<side-conditions> <side-condition>)))
     (IF (NOT (failure? <side-conditions>))
      (LIST (FIRST <side-conditions>)
       (cons (SECOND <side-condition>) (SECOND <side-conditions>)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<side-condition> (<side-condition> Stream)))
   (IF (NOT (failure? <side-condition>))
    (LIST (FIRST <side-condition>) (cons (SECOND <side-condition>) NIL))
    (LIST NIL #\Escape)))
  (LET ((<e> (<e> Stream)))
   (IF (NOT (failure? <e>)) (LIST (FIRST <e>) NIL) (LIST NIL #\Escape)))))

(DEFUN <side-condition> (Stream)
 (cases
  (IF (AND (CONSP (FIRST Stream)) (EQUAL (FIRST (FIRST Stream)) 'if))
   (LET ((<item> (<item> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
    (IF (NOT (failure? <item>))
     (LIST (FIRST <item>) (cons 'if (cons (SECOND <item>) NIL)))
     (LIST NIL #\Escape)))
   (LIST NIL #\Escape))
  (IF (AND (CONSP (FIRST Stream)) (EQUAL (FIRST (FIRST Stream)) 'let))
   (LET ((<item1> (<item1> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
    (IF (NOT (failure? <item1>))
     (LET ((<item2> (<item2> <item1>)))
      (IF (NOT (failure? <item2>))
       (LIST (FIRST <item2>)
        (cons 'let (cons (SECOND <item1>) (cons (SECOND <item2>) NIL))))
       (LIST NIL #\Escape)))
     (LIST NIL #\Escape)))
   (LIST NIL #\Escape))))

(DEFUN <item1> (Stream)
 (cases
  (LET ((<item> (<item> Stream)))
   (IF (NOT (failure? <item>)) (LIST (FIRST <item>) (SECOND <item>))
    (LIST NIL #\Escape)))))

(DEFUN <item2> (Stream)
 (cases
  (LET ((<item> (<item> Stream)))
   (IF (NOT (failure? <item>)) (LIST (FIRST <item>) (SECOND <item>))
    (LIST NIL #\Escape)))))

(DEFUN <item> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (CAAR Stream))
   (LIST NIL #\Escape))))

(DEFUN <sequents> (Stream)
 (cases
  (LET ((<sequent> (<sequent> Stream)))
   (IF (NOT (failure? <sequent>))
    (LET ((<semi-colon> (<semi-colon> <sequent>)))
     (IF (NOT (failure? <semi-colon>))
      (LET ((<sequents> (<sequents> <semi-colon>)))
       (IF (NOT (failure? <sequents>))
        (LIST (FIRST <sequents>) (cons (SECOND <sequent>) (SECOND <sequents>)))
        (LIST NIL #\Escape)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<sequent> (<sequent> Stream)))
   (IF (NOT (failure? <sequent>))
    (LET ((<semi-colon> (<semi-colon> <sequent>)))
     (IF (NOT (failure? <semi-colon>))
      (LIST (FIRST <semi-colon>) (cons (SECOND <sequent>) NIL))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<e> (<e> Stream)))
   (IF (NOT (failure? <e>)) (LIST (FIRST <e>) NIL) (LIST NIL #\Escape)))))

(DEFUN <simple_sequents> (Stream)
 (cases
  (LET ((<simple_sequent> (<simple_sequent> Stream)))
   (IF (NOT (failure? <simple_sequent>))
    (LET ((<simple_sequents> (<simple_sequents> <simple_sequent>)))
     (IF (NOT (failure? <simple_sequents>))
      (LIST (FIRST <simple_sequents>)
       (cons (SECOND <simple_sequent>) (SECOND <simple_sequents>)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<simple_sequent> (<simple_sequent> Stream)))
   (IF (NOT (failure? <simple_sequent>))
    (LIST (FIRST <simple_sequent>) (cons (SECOND <simple_sequent>) NIL))
    (LIST NIL #\Escape)))))

(DEFUN <simple_sequent> (Stream)
 (cases
  (LET ((<formula> (<formula> Stream)))
   (IF (NOT (failure? <formula>))
    (LET ((<semi-colon> (<semi-colon> <formula>)))
     (IF (NOT (failure? <semi-colon>))
      (LIST (FIRST <semi-colon>) (cons NIL (cons (SECOND <formula>) NIL)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))))

(DEFUN <semi-colon> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
    (if (qi_= (CAAR Stream) ';) (CAAR Stream) #\Escape))
   (LIST NIL #\Escape))))

(DEFUN <sequent> (Stream)
 (cases
  (LET ((<formulae> (<formulae> Stream)))
   (IF (NOT (failure? <formulae>))
    (IF (AND (CONSP (FIRST <formulae>)) (EQUAL (FIRST (FIRST <formulae>)) '>>))
     (LET
      ((<formula>
        (<formula> (LIST (REST (FIRST <formulae>)) (SECOND <formulae>)))))
      (IF (NOT (failure? <formula>))
       (LIST (FIRST <formula>)
        (cons (SECOND <formulae>) (cons (SECOND <formula>) NIL)))
       (LIST NIL #\Escape)))
     (LIST NIL #\Escape))
    (LIST NIL #\Escape)))
  (LET ((<formula> (<formula> Stream)))
   (IF (NOT (failure? <formula>))
    (LIST (FIRST <formula>) (cons NIL (cons (SECOND <formula>) NIL)))
    (LIST NIL #\Escape)))))

(DEFUN <formulae> (Stream)
 (cases
  (LET ((<formula> (<formula> Stream)))
   (IF (NOT (failure? <formula>))
    (LET ((<comma> (<comma> <formula>)))
     (IF (NOT (failure? <comma>))
      (LET ((<formulae> (<formulae> <comma>)))
       (IF (NOT (failure? <formulae>))
        (LIST (FIRST <formulae>) (cons (SECOND <formula>) (SECOND <formulae>)))
        (LIST NIL #\Escape)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<formula> (<formula> Stream)))
   (IF (NOT (failure? <formula>))
    (LIST (FIRST <formula>) (cons (SECOND <formula>) NIL))
    (LIST NIL #\Escape)))
  (LET ((<e> (<e> Stream)))
   (IF (NOT (failure? <e>)) (LIST (FIRST <e>) NIL) (LIST NIL #\Escape)))))

(DEFUN <comma> (Stream)
 (cases
  (IF (AND (CONSP (FIRST Stream)) (EQUAL (FIRST (FIRST Stream)) '++))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (CONS '++ NIL))
   (LIST NIL #\Escape))))

(DEFUN <formula> (Stream)
 (cases
  (LET ((<ob> (<ob> Stream)))
   (IF (NOT (failure? <ob>))
    (IF (AND (CONSP (FIRST <ob>)) (EQUAL (FIRST (FIRST <ob>)) '$$))
     (LET ((<type> (<type> (LIST (REST (FIRST <ob>)) (SECOND <ob>)))))
      (IF (NOT (failure? <type>))
       (LIST (FIRST <type>)
        (cons (cons->@c (curry (SECOND <ob>)))
         (cons '$$ (cons (normalise_type (SECOND <type>)) NIL))))
       (LIST NIL #\Escape)))
     (LIST NIL #\Escape))
    (LIST NIL #\Escape)))
  (LET ((<ob> (<ob> Stream)))
   (IF (NOT (failure? <ob>)) (LIST (FIRST <ob>) (cons->@c (SECOND <ob>)))
    (LIST NIL #\Escape)))))

(DEFUN <type> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
    (if
     (or (underline? (CAAR Stream))
      (or (double_underline? (CAAR Stream))
       (element? (CAAR Stream) (cons '; (cons '>> (cons '$$ NIL))))))
     #\Escape (curry_type (CAAR Stream))))
   (LIST NIL #\Escape))))

(DEFUN <obs> (Stream)
 (cases
  (LET ((<formula> (<formula> Stream)))
   (IF (NOT (failure? <formula>))
    (LET ((<obs> (<obs> <formula>)))
     (IF (NOT (failure? <obs>))
      (LIST (FIRST <obs>) (cons (SECOND <formula>) (SECOND <obs>)))
      (LIST NIL #\Escape)))
    (LIST NIL #\Escape)))
  (LET ((<e> (<e> Stream)))
   (IF (NOT (failure? <e>)) (LIST (FIRST <e>) NIL) (LIST NIL #\Escape)))))

(DEFUN <ob> (Stream)
 (cases
  (IF (AND (CONSP (FIRST Stream)) (CONSP (CAAR Stream)))
   (LET
    ((HeadStream13 (LIST (CAAR Stream) (CADR Stream)))
     (TailStream14 (LIST (CDAR Stream) (CADR Stream))))
    (LET ((<obs> (<obs> HeadStream13)))
     (IF (NOT (failure? <obs>))
      (IF (NULL (FIRST <obs>)) (LIST (FIRST TailStream14) (SECOND <obs>))
       (LIST NIL #\Escape))
      (LIST NIL #\Escape))))
   (LIST NIL #\Escape))
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
    (if
     (or (underline? (CAAR Stream))
      (or (double_underline? (CAAR Stream))
       (element? (CAAR Stream) (cons '; (cons '>> (cons '$$ NIL))))))
     #\Escape (CAAR Stream)))
   (LIST NIL #\Escape))))
 
(DEFUN cons->@c (V537) (subst '@c 'cons V537))

(DEFUN <underline> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
    (if (underline? (CAAR Stream)) (CAAR Stream) #\Escape))
   (LIST NIL #\Escape))))

(DEFUN <double_underline> (Stream)
 (cases
  (IF (CONSP (FIRST Stream))
   (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
    (if (double_underline? (CAAR Stream)) (CAAR Stream) #\Escape))
   (LIST NIL #\Escape))))

(DEFUN double_underline? (V540)
 (THE SYMBOL
  (and (THE SYMBOL (symbol? V540))
   (double_underlines? (THE LIST (explode V540))))))

(DEFUN underlines? (V547)
 (COND ((NULL V547) 'true)
  ((AND (CONSP V547) (EQ #\_ (CAR V547))) (underlines? (CDR V547)))
  (T 'false)))

(DEFUN underline? (V548)
 (THE SYMBOL
  (and (THE SYMBOL (symbol? V548)) (underlines? (THE LIST (explode V548))))))

(DEFUN double_underlines? (V555)
 (COND ((NULL V555) 'true)
  ((AND (CONSP V555) (EQ #\= (CAR V555)))
   (double_underlines? (CDR V555)))
  (T 'false)))

(DEFUN compile-theory (V556 V557) (compile-theory-help V556 V557 1))

(DEFUN compile-theory-help (V562 V563 V564)
 (COND ((NULL V563) NIL)
  ((CONSP V563)
   (do (compile-theory-rule V562 (CAR V563) V564)
    (compile-theory-help V562 (CDR V563) (THE NUMBER (+ V564 1)))))
  (T (implementation_error 'compile-theory-help))))

(DEFUN compile-theory-rule (V565 V566 V567)
 (do (typecheck-refinement-rule V565 V566 V567)
  (put-prop V565 V567
   (process_asm_to_functions (THE SYMBOL (gensym "f")) (asm V566 'functional)))
  (generate_refinement_function V565 V566 V567)))

(DEFUN typecheck-refinement-rule (V570 V571 V572)
 (COND
  ((AND (CONSP V571) (CONSP (CAR V571)) (CONSP (CAR (CAR V571)))
    (EQ 'name (CAR (CAR (CAR V571))))
    (CONSP (CDR (CAR (CAR V571))))
    (NULL (CDR (CDR (CAR (CAR V571))))) (CONSP (CDR V571))
    (CONSP (CDR (CDR V571))) (EQ 'underline (CAR (CDR (CDR V571))))
    (CONSP (CDR (CDR (CDR V571)))) (CONSP (CAR (CDR (CDR (CDR V571)))))
    (CONSP (CDR (CAR (CDR (CDR (CDR V571))))))
    (NULL (CDR (CDR (CAR (CDR (CDR (CDR V571)))))))
    (NULL (CDR (CDR (CDR (CDR V571))))))
   (LET*
    ((V573 (CDR V571)) (V574 (CAR V571)) (V575 (CDR V573)) (V576 (CAR V574))
     (V577 (CDR V575)) (V578 (CDR V576)) (V579 (CAR V577)) (V580 (CAR V578))
     (V581 (CAR V579)) (V582 (CDR V579)) (V583 (CAR V582)))
    (THE SYMBOL
     (and (refinement-integrity-check V570 V581 V583 V580)
      (refinement-correctness-check V570 V581 V583 (CAR V573) (CDR V574)
       V580)))))
  ((AND (CONSP V571) (CONSP (CDR V571)) (CONSP (CDR (CDR V571)))
    (EQ 'underline (CAR (CDR (CDR V571))))
    (CONSP (CDR (CDR (CDR V571)))) (CONSP (CAR (CDR (CDR (CDR V571)))))
    (CONSP (CDR (CAR (CDR (CDR (CDR V571))))))
    (NULL (CDR (CDR (CAR (CDR (CDR (CDR V571)))))))
    (NULL (CDR (CDR (CDR (CDR V571))))))
   (LET*
    ((V584 (CDR V571)) (V585 (CDR V584)) (V586 (CDR V585)) (V587 (CAR V586))
     (V588 (CAR V587)) (V589 (CDR V587)) (V590 (CAR V589)))
    (THE SYMBOL
     (and (refinement-integrity-check V570 V588 V590 V572)
      (refinement-correctness-check V570 V588 V590 (CAR V584) (CAR V571)
       V572)))))
  (T (implementation_error 'typecheck-refinement-rule))))

(DEFUN refinement-integrity-check (V2330 V2331 V2332 V2333)
 (LET
  ((Context
    (build_patt_env (extract_variables (CONS V2331 (CONS V2332 NIL))))))
  (LET
   ((Conclusion
     (CONS '@p
      (CONS (stvars-but-bound (@c-wffs V2331)) (CONS (stvars V2332) NIL)))))
   (LET
    ((Type
      (normalise_type
       (CONS (CONS 'list (CONS 'wff NIL)) (CONS '* (CONS 'wff NIL))))))
    (IF (EQ 'true (qi_= (typechecks? Context Conclusion Type) 'false))
     (raise-result-error V2333 V2330) 'true)))))

(DEFUN refinement-correctness-check (V2071 V2072 V2073 V2074 V2075 V2076)
 (LET
  ((Context
    (normalise_type_env
     (stvars
      (CONS
       (CONS (@c-wffs V2072)
        (CONS '$$ (CONS (CONS 'list (CONS 'wff NIL)) NIL)))
       (CONS (CONS (cons->@c V2073) (CONS '$$ (CONS 'wff NIL)))
        (CONS
         (CONS 'Assumptions (CONS '$$ (CONS (CONS 'list (CONS 'wff NIL)) NIL)))
         (CONS
          (CONS 'Notes (CONS '$$ (CONS (CONS 'list (CONS 'note NIL)) NIL)))
          (CONS
           (CONS 'Parameters
            (CONS '$$ (CONS (CONS 'list (CONS 'parameter NIL)) NIL)))
           (CONS
            (CONS 'Sequents
             (CONS '$$
              (CONS
               (CONS 'list
                (CONS
                 (CONS (CONS 'list (CONS 'wff NIL)) (CONS '* (CONS 'wff NIL)))
                 NIL))
               NIL)))
            NIL))))))))))
  (LET
   ((Conclusion
     (stvars-but-bound (construct_conclusion V2075 (@c-sequents V2074)))))
   (LET
    ((Type
      (normalise_type
       (CONS 'list
        (CONS (CONS (CONS 'list (CONS 'wff NIL)) (CONS '* (CONS 'wff NIL)))
         NIL)))))
    (IF (EQ 'true (qi_= (typechecks? Context Conclusion Type) 'false))
     (raise-result-error V2076 V2071) 'true)))))

(DEFUN stvars-but-bound (V595)
 (COND ((wrapper (variable? V595)) (THE SYMBOL (concat '&& V595)))
  ((AND (CONSP V595) (EQ 'let (CAR V595)) (CONSP (CDR V595))
    (CONSP (CDR (CDR V595))) (CONSP (CDR (CDR (CDR V595))))
    (NULL (CDR (CDR (CDR (CDR V595))))))
   (LET* ((V596 (CDR V595)) (V597 (CAR V596)) (V598 (CDR V596)))
    (CONS 'let
     (CONS V597
      (CONS (stvars-but-bound (CAR V598))
       (CONS
        (subst V597 (THE SYMBOL (concat '&& V597))
         (stvars-but-bound (CAR (CDR V598))))
        NIL))))))
  ((AND (CONSP V595) (EQ '/. (CAR V595)) (CONSP (CDR V595))
    (CONSP (CDR (CDR V595))) (NULL (CDR (CDR (CDR V595)))))
   (LET* ((V599 (CDR V595)) (V600 (CAR V599)))
    (CONS '/.
     (CONS V600
      (CONS
       (subst V600 (THE SYMBOL (concat '&& V600))
        (stvars-but-bound (CAR (CDR V599))))
       NIL)))))
  ((CONSP V595) (MAPCAR 'stvars-but-bound V595)) 
  (T V595)))

(DEFUN raise-result-error (V601 V602)
 (error "type failure in conclusion of rule ~A of theory ~A~%" V601 V602))

(DEFUN refine (Theory N Params Goalstack)
   (INCF *inferences*)
   (LET ((F (get-prop Theory N 'IDENTITY)))
          (FUNCALL F Goalstack Params)))

(DEFUN collect_or_return (Goals)
   (COND ((EQUAL Goals #\Escape) Goals)
            ((BOUNDP '*collect*) (PUSH Goals *collect*) #\Escape)
            (T Goals)))

(DEFUN fail-if (F X)
  (IF (EQ (FUNCALL F X) 'true) 
       #\Escape
       X))

(DEFUN collect (Theory N Parameters Goals)
  (PROGV '(*collect*) '(())
            (refine Theory N Parameters Goals)
            *collect*))

(DEFUN @c-sequents (V609)
 (COND ((NULL V609) NIL)
  ((AND (CONSP V609) (CONSP (CAR V609)) (CONSP (CDR (CAR V609)))
    (NULL (CDR (CDR (CAR V609)))))
   (LET* ((V610 (CAR V609)))
    (CONS '@c
     (CONS
      (CONS '@p
       (CONS (@c-wffs (CAR V610)) (CONS (cons->@c (CAR (CDR V610))) NIL)))
      (CONS (@c-sequents (CDR V609)) NIL)))))
  (T (implementation_error '@c-sequents))))

(DEFUN @c-wffs (V611)
 (COND ((NULL V611) NIL)
  ((CONSP V611)
   (CONS '@c (CONS (cons->@c (CAR V611)) (CONS (@c-wffs (CDR V611)) NIL))))
  (T (implementation_error '@c-wffs))))

(DEFUN construct_conclusion (V612 V613)
 (COND ((NULL V612) V613)
  ((AND (CONSP V612) (CONSP (CAR V612)) (EQ 'if (CAR (CAR V612)))
    (CONSP (CDR (CAR V612))) (NULL (CDR (CDR (CAR V612)))))
   (CONS 'if
    (CONS (CAR (CDR (CAR V612)))
     (CONS (construct_conclusion (CDR V612) V613) (CONS NIL NIL)))))
  ((AND (CONSP V612) (CONSP (CAR V612)) (EQ 'let (CAR (CAR V612)))
    (CONSP (CDR (CAR V612))) (CONSP (CDR (CDR (CAR V612))))
    (NULL (CDR (CDR (CDR (CAR V612))))))
   (LET* ((V614 (CAR V612)) (V615 (CDR V614)))
    (CONS 'let
     (CONS (CAR V615)
      (CONS (CAR (CDR V615))
       (CONS (construct_conclusion (CDR V612) V613) NIL))))))
  (T (implementation_error 'construct_conclusion))))

(DEFUN raise-premisses-error (V616 V617)
 (error "type failure in premisses of rule ~A of theory ~A~%" V616 V617))

(DEFUN generate_refinement_function (V618 V619 V620)
 (COND
  ((AND (CONSP V619) (CONSP (CDR V619)) (CONSP (CDR (CDR V619)))
    (EQ 'underline (CAR (CDR (CDR V619))))
    (CONSP (CDR (CDR (CDR V619)))) (CONSP (CAR (CDR (CDR (CDR V619)))))
    (CONSP (CDR (CAR (CDR (CDR (CDR V619))))))
    (NULL (CDR (CDR (CAR (CDR (CDR (CDR V619)))))))
    (NULL (CDR (CDR (CDR (CDR V619))))))
   (grf* V618 (CAR V619) V620))
  (T (implementation_error 'generate_refinement_function))))

(DEFUN grf* (V631 V632 V633)
 (COND ((NULL V632) NIL)
  ((AND (CONSP V632) (CONSP (CAR V632)) (EQ 'name (CAR (CAR V632)))
    (CONSP (CDR (CAR V632))) (CONSP (CAR (CDR (CAR V632))))
    (CONSP (CDR (CAR (CDR (CAR V632)))))
    (NULL (CDR (CDR (CAR (CDR (CAR V632))))))
    (NULL (CDR (CDR (CAR V632)))))
   (LET*
    ((V634 (CAR V632)) (V635 (CDR V634)) (V636 (CAR V635)) (V637 (CDR V636))
     (V638 (CAR V636)) (V639 (CAR V637)))
    (do
     (compile-string
      (LET ((Parameters (rpt_times V639)))
       (FORMAT NIL
        "(define ~A
                                         ~{~A ~} Goals -> (refine ~A ~A (list ~{~A ~}) Goals))"
        V638 Parameters V631 V633 Parameters)))
     (newfuntype* V638 (constr_tactic_type V639))
     (FORMAT 'T "~A : ~A~%" V638 (constr_tactic_type V639)))))
  ((AND (CONSP V632) (CONSP (CAR V632)) (EQ 'name (CAR (CAR V632)))
    (CONSP (CDR (CAR V632))) (NULL (CDR (CDR (CAR V632)))))
   (LET* ((V640 (CAR V632)) (V641 (CDR V640)) (V642 (CAR V641)))
    (do
     (compile-string
      (FORMAT NIL
       "(define ~A
                                          Goals -> (refine ~A ~A NIL Goals))"
       V642 V631 V633))
     (newfuntype* V642 (CONS 'goals (CONS '--> (CONS 'goals NIL))))
     (FORMAT 'T "~A : (goals --> goals)~%" V642))))
  (T 'ok)))

(DEFUN rpt_times (V643)
 (COND ((EQ 0 V643) NIL)
  (T
   (CONS (THE SYMBOL (gensym "Param")) (rpt_times (THE NUMBER (- V643 1)))))))

(DEFUN constr_tactic_type (V644)
 (COND ((EQ 0 V644) (CONS 'goals (CONS '--> (CONS 'goals NIL))))
  (T
   (CONS 'parameter
    (CONS '--> (CONS (constr_tactic_type (THE NUMBER (- V644 1))) NIL))))))

(DEFUN process_asm_to_functions (V645 V646)
 (COND
  ((AND (CONSP V646) (EQ 'match (CAR V646)) (CONSP (CDR V646))
    (CONSP (CDR (CDR V646))) (EQ 'and (CAR (CDR (CDR V646))))
    (CONSP (CDR (CDR (CDR V646))))
    (EQ 'then (CAR (CDR (CDR (CDR V646)))))
    (CONSP (CDR (CDR (CDR (CDR V646)))))
    (NULL (CDR (CDR (CDR (CDR (CDR V646)))))))
   (LET* ((V647 (CDR V646)))
    (compile-string
     (FORMAT NIL
      "(define ~A
           (list (cons (@p Context ~S) Sequents) Notes Proof) Parameters <- (let Accum NIL ~S)
           Goals _ -> Goals)"
      V645 (CAR V647)
      (process_constraint_functions (CAR (CDR (CDR (CDR V647)))))))))
  (T (implementation_error 'process_asm_to_functions))))

(DEFUN compile-string (V650) (EVAL (READ-FROM-STRING V650)))

(DEFUN process_constraint_functions (V655)
 (COND
  ((AND (CONSP V655) (CONSP (CAR V655)) (EQ 'find (CAR (CAR V655)))
    (CONSP (CDR (CAR V655))) (CONSP (CDR (CDR (CAR V655))))
    (EQ 'with (CAR (CDR (CDR (CAR V655)))))
    (CONSP (CDR (CDR (CDR (CAR V655)))))
    (EQ 'constraints (CAR (CDR (CDR (CDR (CAR V655))))))
    (CONSP (CDR (CDR (CDR (CDR (CAR V655)))))))
   (LET*
    ((V656 (CAR V655)) (V657 (CDR V656)) (V658 (CDR V657)) (V659 (CDR V658))
     (V660 (CDR V659)) (V661 (CAR V660)))
    (LET ((G (gensym "g")))
     (do (generate_search_function G (CAR V657) V661 (CDR V655))
      (CONS G
       (APPEND V661
        (CONS (CONS 'append (CONS 'Accum (CONS 'Context NIL)))
         (CONS NIL
          (CONS 'Sequents
           (CONS 'Notes (CONS 'Proof (CONS 'Parameters NIL))))))))))))
  ((AND (CONSP V655) (EQ 'in (CAR V655)) (CONSP (CDR V655))
    (CONSP (CAR (CDR V655))) (EQ 'prove (CAR (CAR (CDR V655))))
    (CONSP (CDR (CAR (CDR V655))))
    (EQ 'sequents (CAR (CDR (CAR (CDR V655)))))
    (CONSP (CDR (CDR (CAR (CDR V655)))))
    (NULL (CAR (CDR (CDR (CAR (CDR V655))))))
    (NULL (CDR (CDR V655))))
   (CONS 'collect_or_return
    (CONS (CONS 'list (CONS 'Sequents (CONS 'Notes (CONS 'Proof NIL)))) NIL)))
  ((AND (CONSP V655) (EQ 'in (CAR V655)) (CONSP (CDR V655))
    (NULL (CDR (CDR V655))))
   (CONS 'collect_or_return
    (CONS
     (CONS 'let
      (CONS 'Assumptions
       (CONS (CONS 'append (CONS 'Accum (CONS 'Context NIL)))
        (CONS (process_out_call (CAR (CDR V655))) NIL))))
     NIL)))
  (T (implementation_error 'process_constraint_functions))))

(DEFUN process_out_call (V666)
 (COND
  ((AND (CONSP V666) (EQ 'if (CAR V666)) (CONSP (CDR V666))
    (CONSP (CDR (CDR V666))) (EQ 'then (CAR (CDR (CDR V666))))
    (CONSP (CDR (CDR (CDR V666))))
    (NULL (CDR (CDR (CDR (CDR V666))))))
   (LET* ((V667 (CDR V666)))
    (CONS 'if
     (CONS (CAR V667)
      (CONS (process_out_call (CAR (CDR (CDR V667))))
       (CONS *qi-failure-object* NIL))))))
  ((AND (CONSP V666) (EQ 'let (CAR V666)) (CONSP (CDR V666))
    (CONSP (CDR (CDR V666))) (EQ 'be (CAR (CDR (CDR V666))))
    (CONSP (CDR (CDR (CDR V666)))) (CONSP (CDR (CDR (CDR (CDR V666)))))
    (EQ 'in (CAR (CDR (CDR (CDR (CDR V666))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR V666))))))
    (NULL (CDR (CDR (CDR (CDR (CDR (CDR V666))))))))
   (LET* ((V668 (CDR V666)) (V669 (CDR V668)) (V670 (CDR V669)))
    (CONS 'let
     (CONS (CAR V668)
      (CONS (CAR V670)
       (CONS (process_out_call (CAR (CDR (CDR V670)))) NIL))))))
  ((AND (CONSP V666) (EQ 'prove (CAR V666)) (CONSP (CDR V666))
    (EQ 'sequents (CAR (CDR V666))) (CONSP (CDR (CDR V666))))
   (CONS 'list
    (CONS (compose_sequents (CAR (CDR (CDR V666))))
     (CONS 'Notes (CONS 'Proof NIL)))))
  (T (implementation_error 'process_out_call))))

(DEFUN compose_sequents (V671)
 (COND ((NULL V671) 'Sequents)
  ((AND (CONSP V671) (CONSP (CAR V671)) (NULL (CAR (CAR V671)))
    (CONSP (CDR (CAR V671))) (NULL (CDR (CDR (CAR V671)))))
   (CONS 'cons
    (CONS (@p 'Assumptions (CAR (CDR (CAR V671))))
     (CONS (compose_sequents (CDR V671)) NIL))))
  ((AND (CONSP V671) (CONSP (CAR V671)) (CONSP (CDR (CAR V671)))
    (NULL (CDR (CDR (CAR V671)))))
   (LET* ((V672 (CAR V671)))
    (CONS 'cons
     (CONS
      (@p (CONS 'append (CONS (CONS 'list (CAR V672)) (CONS 'Assumptions NIL)))
       (CAR (CDR V672)))
      (CONS (compose_sequents (CDR V671)) NIL)))))
  (T (implementation_error 'compose_sequents))))

(DEFUN generate_search_function (V673 V674 V675 V676)
 (compile-string
  (FORMAT NIL
   "(define ~A
                   ~{~S ~}(cons ~S Context) Accum Sequents Notes Proof Parameters <- ~S
                   ~{~S ~}(cons C1234 Context) Accum Sequents Notes Proof Parameters
                    -> (~A ~{~S ~}Context (cons C1234 Accum) Sequents Notes Proof Parameters)
                   ~{~S ~}_ _ _ _ _ _ -> #\\Escape)"
   V673 V675 V674 (process_constraint_functions V676) V675 V673 V675 V675)))

(DEFUN compile-datatype (V677 V678)
 (compile_logic_string (compile-datatype-help V677 V678)))

(DEFUN compile-datatype-help (V679 V680)
 (COND ((NULL V680) NIL)
  ((AND (CONSP V680) (wrapper (single_underline? (CAR V680))))
   (CONS (compile-datatype-rule V679 (CAR V680))
    (compile-datatype-help V679 (CDR V680))))
  ((CONSP V680)
   (compile-datatype-help V679
    (APPEND (unfold_double_underline (CAR V680)) (CDR V680))))
  (T (implementation_error 'compile-datatype-help))))

(DEFUN single_underline? (V685)
 (COND
  ((AND (CONSP V685) (CONSP (CDR V685)) (CONSP (CDR (CDR V685)))
    (EQ 'underline (CAR (CDR (CDR V685))))
    (CONSP (CDR (CDR (CDR V685))))
    (NULL (CDR (CDR (CDR (CDR V685))))))
   'true)
  (T 'false)))

(DEFUN unfold_double_underline (V686)
 (COND
  ((AND (CONSP V686) (CONSP (CDR V686)) (CONSP (CDR (CDR V686)))
    (CONSP (CDR (CDR (CDR V686)))) (CONSP (CAR (CDR (CDR (CDR V686)))))
    (NULL (CAR (CAR (CDR (CDR (CDR V686))))))
    (CONSP (CDR (CAR (CDR (CDR (CDR V686))))))
    (NULL (CDR (CDR (CAR (CDR (CDR (CDR V686)))))))
    (NULL (CDR (CDR (CDR (CDR V686))))))
   (LET*
    ((V687 (CAR V686)) (V688 (CDR V686)) (V689 (CAR V688)) (V690 (CDR V688))
     (V691 (CDR V690)))
    (LET ((P* (gensym "P")))
     (CONS (CONS V687 (CONS V689 (CONS 'underline V691)))
      (CONS
       (CONS V687
        (CONS (CONS (CONS (insert_as_assumptions V689) (CONS P* NIL)) NIL)
         (CONS 'underline (CONS (CONS (CDR (CAR V691)) (CONS P* NIL)) NIL))))
       NIL)))))
  (T (implementation_error 'unfold_double_underline))))

(DEFUN insert_as_assumptions (V692)
 (COND ((NULL V692) NIL)
  ((AND (CONSP V692) (CONSP (CAR V692)) (NULL (CAR (CAR V692)))
    (CONSP (CDR (CAR V692))) (NULL (CDR (CDR (CAR V692)))))
   (CONS (CAR (CDR (CAR V692))) (insert_as_assumptions (CDR V692))))
  (T (implementation_error 'insert_as_assumptions))))

(DEFUN compile_logic_string (V693)
 (defprolog (FORMAT NIL "~{~{ ~S~}.~%~%~}" V693)))

(DEFUN compile-datatype-rule (V694 V695) (interp_asm V694 (asm V695 'logic)))

(DEFUN interp_asm (V5377 V5378)
 (COND
  ((AND (CONSP V5378) (EQ 'match (CAR V5378)) (CONSP (CDR V5378))
    (CONSP (CDR (CDR V5378))) (EQ 'and (CAR (CDR (CDR V5378))))
    (CONSP (CDR (CDR (CDR V5378)))) (EQ 'then (CAR (CDR (CDR (CDR V5378)))))
    (CONSP (CDR (CDR (CDR (CDR V5378)))))
    (NULL (CDR (CDR (CDR (CDR (CDR V5378)))))))
   (LET* ((V5379 (CDR V5378)) (V5380 (CAR V5379)))
    (LET ((Tail (ia* (CAR (CDR (CDR (CDR V5379)))))))
     (IF (EQ 'true (THE SYMBOL (empty? Tail)))
      (CONS V5377
       (CONS (CONS (insert_type_mode_decl V5380) (CONS 'Context NIL)) NIL))
      (CONS V5377
       (CONS (CONS (insert_type_mode_decl V5380) (CONS 'Context NIL))
        (CONS ':- Tail)))))))
  (T (implementation_error 'interp_asm))))

(DEFUN insert_type_mode_decl (V5365)
 (COND
  ((AND (CONSP V5365) (CONSP (CDR V5365)) (EQ '$$ (CAR (CDR V5365)))
    (CONSP (CDR (CDR V5365))) (NULL (CDR (CDR (CDR V5365)))))
   (CONS 'mode
    (CONS
     (CONS (insert_type_mode_decl (CAR V5365))
      (CONS '$$
       (CONS (CONS 'mode (CONS (CAR (CDR (CDR V5365))) (CONS '+ NIL))) NIL)))
     (CONS '- NIL))))
  ((CONSP V5365) 
  (MAPCAR 'insert_type_mode_decl V5365)) 
(T V5365)))

(DEFUN ia* (V701)
 (COND
  ((AND (CONSP V701) (CONSP (CAR V701)) (EQ 'find (CAR (CAR V701)))
    (CONSP (CDR (CAR V701))) (CONSP (CDR (CDR (CAR V701))))
    (EQ 'with (CAR (CDR (CDR (CAR V701)))))
    (CONSP (CDR (CDR (CDR (CAR V701)))))
    (EQ 'constraints (CAR (CDR (CDR (CDR (CAR V701))))))
    (CONSP (CDR (CDR (CDR (CDR (CAR V701))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CAR V701)))))))
    (EQ 'in (CAR (CDR (CDR (CDR (CDR (CDR (CAR V701))))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR (CAR V701))))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CAR V701)))))))))
    (EQ 'and (CAR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CAR V701))))))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CAR V701))))))))))
    (EQ 'generate
      (CAR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CAR V701)))))))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CAR V701)))))))))))
    (EQ 'new
      (CAR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CAR V701))))))))))))
    (CONSP
     (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CAR V701))))))))))))
    (EQ 'context
      (CAR
       (CDR
        (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CAR V701)))))))))))))
    (CONSP
     (CDR
      (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CAR V701)))))))))))))
    (NULL
      (CDR
       (CDR
        (CDR
         (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CDR (CAR V701)))))))))))))))
   (LET*
    ((V702 (CAR V701)) (V703 (CDR V702)) (V704 (CDR V703)) (V705 (CDR V704))
     (V706 (CDR V705)) (V707 (CAR V706)) (V708 (CDR V706)) (V709 (CDR V708)))
    (LET ((Find (gensym "find")))
     (do (generate_search_clause Find (CAR V703) V707)
      (CONS Find
       (CONS
        (generate_search_literal V707 (CAR V709)
         (CAR (CDR (CDR (CDR (CDR (CDR V709)))))))
        (ia* (CDR V701))))))))
  ((AND (CONSP V701) (EQ 'in (CAR V701)) (CONSP (CDR V701))
    (NULL (CDR (CDR V701))))
   (ia-output (CAR (CDR V701))))
  (T (implementation_error 'ia*))))

(DEFUN generate_search_literal (V710 V711 V712)
 (APPEND V710 (CONS V711 (CONS V712 NIL))))

(DEFUN generate_search_clause (V713 V714 V715)
 (defprolog
  (FORMAT NIL
   "~%~%~A(~{~S ~}(mode [~S | Context59739] -) Context59739).
                         ~%~A(~{~S ~}(mode [P59739 | Context59739] -) [P59739 | NewContext59739])
                          :- ~A(~{~S ~}Context59739 NewContext59739)."
   V713 V715 (insert_type_mode_decl V714) V713 V715 V713 V715)))

(DEFUN ia-output (V15)
 (COND
  ((AND (CONSP V15) (EQ 'if (CAR V15)) (CONSP (CDR V15))
    (CONSP (CDR (CDR V15))) (EQ 'then (CAR (CDR (CDR V15))))
    (CONSP (CDR (CDR (CDR V15)))) (NULL (CDR (CDR (CDR (CDR V15))))))
   (LET* ((V16 (CDR V15)))
    (CONS 'when!
     (CONS (CONS (CAR V16) NIL) (ia-output (CAR (CDR (CDR V16))))))))
  ((AND (CONSP V15) (EQ 'let (CAR V15)) (CONSP (CDR V15))
    (CONSP (CDR (CDR V15))) (EQ 'be (CAR (CDR (CDR V15))))
    (CONSP (CDR (CDR (CDR V15)))) (CONSP (CDR (CDR (CDR (CDR V15)))))
    (EQ 'in (CAR (CDR (CDR (CDR (CDR V15))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR V15))))))
    (NULL (CDR (CDR (CDR (CDR (CDR (CDR V15))))))))
   (LET* ((V17 (CDR V15)) (V18 (CDR V17)) (V19 (CDR V18)))
    (CONS 'is!
     (CONS (CONS (CAR V17) (CONS (CAR V19) NIL))
      (ia-output (CAR (CDR (CDR V19))))))))
  ((AND (CONSP V15) (EQ 'commit (CAR V15)) (CONSP (CDR V15))
    (EQ 'to (CAR (CDR V15))) (CONSP (CDR (CDR V15)))
    (EQ 'proving (CAR (CDR (CDR V15)))) (CONSP (CDR (CDR (CDR V15))))
    (EQ 'sequents (CAR (CDR (CDR (CDR V15)))))
    (CONSP (CDR (CDR (CDR (CDR V15)))))
    (CONSP (CDR (CDR (CDR (CDR (CDR V15))))))
    (EQ 'in (CAR (CDR (CDR (CDR (CDR (CDR V15)))))))
    (CONSP (CDR (CDR (CDR (CDR (CDR (CDR V15)))))))
    (NULL (CDR (CDR (CDR (CDR (CDR (CDR (CDR V15)))))))))
   (LET* ((V20 (CDR V15)) (V21 (CDR V20)) (V22 (CDR V21)) (V23 (CDR V22)))
    (CONS '! (ia-sequents (CAR V23) (CAR (CDR (CDR V23)))))))
  ((AND (CONSP V15) (EQ 'prove (CAR V15)) (CONSP (CDR V15))
    (EQ 'sequents (CAR (CDR V15))) (CONSP (CDR (CDR V15)))
    (CONSP (CDR (CDR (CDR V15)))) (EQ 'in (CAR (CDR (CDR (CDR V15)))))
    (CONSP (CDR (CDR (CDR (CDR V15)))))
    (NULL (CDR (CDR (CDR (CDR (CDR V15)))))))
   (LET* ((V24 (CDR V15)) (V25 (CDR V24)))
    (ia-sequents (CAR V25) (CAR (CDR (CDR V25))))))
  (T (implementation_error 'ia-output))))

(DEFUN ia-sequents (V724 V725)
 (COND ((NULL V724) NIL)
  ((AND (CONSP V724) (CONSP (CAR V724)) (CONSP (CDR (CAR V724)))
    (NULL (CDR (CDR (CAR V724)))))
   (LET* ((V726 (CAR V724)))
    (CONS 'typecheck
     (CONS (CONS (CAR (CDR V726)) (CONS (cons_each (CAR V726) V725) NIL))
      (ia-sequents (CDR V724) V725)))))
  (T (implementation_error 'ia-sequents))))

(DEFUN cons_each (V727 V728)
 (COND ((NULL V727) V728)
  ((CONSP V727)
   (CONS 'cons (CONS (CAR V727) (CONS (cons_each (CDR V727) V728) NIL))))
  (T (implementation_error 'cons_each))))

(DEFUN create_clause_head (V729 V730)
 (CONS V729
  (CONS (insert_type_mode_decl (CONS 'mode (CONS V730 (CONS '- NIL))))
   (CONS 'Context NIL))))

(DEFUN asm (V731 V732)
 (COND
  ((AND (CONSP V731) (CONSP (CDR V731)) (CONSP (CDR (CDR V731)))
    (EQ 'underline (CAR (CDR (CDR V731))))
    (CONSP (CDR (CDR (CDR V731)))) (CONSP (CAR (CDR (CDR (CDR V731)))))
    (CONSP (CDR (CAR (CDR (CDR (CDR V731))))))
    (NULL (CDR (CDR (CAR (CDR (CDR (CDR V731)))))))
    (NULL (CDR (CDR (CDR (CDR V731))))))
   (LET*
    ((V733 (CDR V731)) (V734 (CDR V733)) (V735 (CDR V734)) (V736 (CAR V735))
     (V737 (CDR V736)) (V738 (CAR V737)))
    (CONS 'match
     (CONS V738
      (CONS 'and
       (CONS 'then
        (CONS
         (tail-sequent (CAR V736) (CAR V731) (CAR V733)
          (extract_variables V738) 'Context V732)
         NIL)))))))
  (T (implementation_error 'asm))))

(DEFUN tail-sequent (V741 V742 V743 V744 V745 V746)
 (COND
  ((NULL V741)
   (CONS 'in (CONS (sides-sequent V742 V743 V745) NIL)))
  ((CONSP V741)
   (LET* ((V747 (CAR V741)) (V748 (CDR V741)))
    (LET ((QVs (extract_variables V747)))
     (LET
      ((Forward (extract_variables (CONS V748 (CONS V742 (CONS V743 NIL))))))
      (LET ((NewContext (gensym "NewContext")))
       (CONS
        (CONS 'find
         (CONS V747
          (CONS 'with
           (CONS 'constraints
            (CONS (calculate_constraints V746 QVs V744 Forward)
             (CONS 'in
              (CONS V745
               (CONS 'and
                (CONS 'generate
                 (CONS 'new (CONS 'context (CONS NewContext NIL))))))))))))
        (tail-sequent V748 V742 V743 (THE LIST (union QVs V744)) NewContext
         V746)))))))
  (T (implementation_error 'tail-sequent))))

(DEFUN calculate_constraints (V758 V759 V760 V761)
 (COND
  ((EQ 'logic V758)
   (intersection V759 (THE LIST (union V760 V761))))
  ((EQ 'functional V758) V760)
  (T (implementation_error 'calculate_constraints))))

(DEFUN intersection (V769 V770)
 (COND ((NULL V769) NIL)
  ((AND (CONSP V769) (wrapper (element? (CAR V769) V770)))
   (CONS (CAR V769) (intersection (CDR V769) V770)))
  ((CONSP V769) (intersection (CDR V769) V770)) 
  (T (implementation_error 'intersection))))

(DEFUN sides-sequent (V180 V181 V182)
 (COND ((NULL V180) (out-sequents V181 V182))
  ((AND (CONSP V180) (CONSP (CAR V180)) (EQ 'if (CAR (CAR V180)))
    (CONSP (CDR (CAR V180))) (NULL (CDR (CDR (CAR V180)))))
   (CONS 'if
    (CONS (CAR (CDR (CAR V180)))
     (CONS 'then (CONS (sides-sequent (CDR V180) V181 V182) NIL)))))
  ((AND (CONSP V180) (CONSP (CAR V180)) (EQ 'name (CAR (CAR V180)))
    (CONSP (CDR (CAR V180))) (NULL (CDR (CDR (CAR V180)))))
   (sides-sequent (CDR V180) V181 V182))
  ((AND (CONSP V180) (EQ 'commit! (CAR V180)) (NULL (CDR V180)))
   (CONS 'commit
    (CONS 'to
     (CONS 'proving (CONS 'sequents (CONS V181 (CONS 'in (CONS V182 NIL))))))))
  ((AND (CONSP V180) (CONSP (CAR V180)) (EQ 'let (CAR (CAR V180)))
    (CONSP (CDR (CAR V180))) (CONSP (CDR (CDR (CAR V180))))
    (NULL (CDR (CDR (CDR (CAR V180))))))
   (LET* ((V183 (CAR V180)) (V184 (CDR V183)))
    (CONS 'let
     (CONS (CAR V184)
      (CONS 'be
       (CONS (CAR (CDR V184))
        (CONS 'in (CONS (sides-sequent (CDR V180) V181 V182) NIL))))))))
  (T (implementation_error 'sides-sequent))))

(DEFUN out-sequents (V776 V777)
 (CONS 'prove (CONS 'sequents (CONS V776 (CONS 'in (CONS V777 NIL))))))

(DEFUN prooftool (V5689)
 (COND
  ((EQ '- V5689)
    (FORMAT 'T "~%~%~A~%~%" *atp-credits*) (SETQ *inferences* 0)
    (SETQ *problem*
     (CONS (CONS (@p (enter_assumptions 1) (enter_conclusion)) NIL) (CONS NIL (CONS NIL NIL))))
    (SETQ *start* (GET-INTERNAL-RUN-TIME)) 
    (prooftool_loop *problem*))
  ((AND (EQ '+ V5689) (BOUNDP '*problem*))
    (FORMAT T "~%~%~A~%~%" *atp-credits*) 
    (SETQ *inferences* 0)
    (SETQ *start* (GET-INTERNAL-RUN-TIME)) 
    (prooftool_loop *problem*))
  ((EQ '+ V5689) (error "prooftool expects a problem.~%"))
  (T (error "prooftool expects a + or a -.~%"))))

(DEFUN enter_assumptions (V783)
  (output "~A. " V783)
  (LET ((Assumption (enter_format (CAR (lineread)))))
   (IF (EQ Assumption 'ok) NIL
    (IF (EQ 'true (well_formed? Assumption))
     (CONS (eval Assumption) (enter_assumptions (THE NUMBER (+ 1 V783))))
     (PROGN (output "this is not a wff~%") (enter_assumptions V783))))))

(DEFUN well_formed? (V784) (qi_= (typechecks? NIL V784 'wff) 'wff))

(DEFUN enter_format (X) (IF (EQ (pr_rb?) 'true)  (insert_conses X) X))

(DEFUN insert_conses (X)
   (IF (CONSP X)
        (LIST 'cons (insert_conses (CAR X)) (insert_conses (CDR X)))
        X))        

(DEFUN enter_conclusion NIL
  (output "~%~%?- ")
  (LET ((Conclusion (enter_format (CAR (lineread)))))
   (IF (EQ 'true (well_formed? Conclusion)) (eval Conclusion)
    (do (output "this is not a wff~%") (enter_conclusion)))))

(DEFUN prooftool_loop (V5699)
 (COND
  ((AND (CONSP V5699) (NULL (CAR V5699)) (CONSP (CDR V5699))
    (CONSP (CDR (CDR V5699))) (NULL (CDR (CDR (CDR V5699)))))
   (SETQ *thm* (CAAR *problem*))
   (SETQ *proof* (CAR (CDR (CDR V5699)))) 
   (stats))
  (T (print-first-sequent V5699)
    (LET ((Tactic (read-tactic)))
     (prooftool_loop (apply Tactic (update-proof *tactic* V5699)))))))

(DEFUN stats ()
 (LET ((Time (calculate-run-time)))
  (LET ((Inferences *inferences*))
   (output "~A seconds		~A refinement~P		~A RPS~%" Time Inferences Inferences
     (IF (ZEROP Time) 'infinite
      (ROUND (THE NUMBER (/ Inferences Time)))))
    'proved)))

(SETQ *inferences* 0)

(DEFUN time-proof (V5378 V5379 V5380)
 (SETQ *start* (GET-INTERNAL-RUN-TIME)) 
 (SETQ *inferences* 0)
  (LET
   ((Result
     (solved?
      (apply V5378
       (CONS (CONS (@p V5379 V5380) NIL) (CONS NIL (CONS NIL NIL)))))))
       (print-sequent (@p V5379 V5380) NIL 1 1 *inferences*) 
       (stats) 
       (TERPRI)
       Result))

(DEFUN solved? (V5711)
 (COND ((AND (CONSP V5711) (NULL (CAR V5711))) 'true)
  (T 'false)))

(DEFUN calculate-run-time ()
 (THE NUMBER
  (* 1.0
   (THE NUMBER
    (/ (THE NUMBER (- (GET-INTERNAL-RUN-TIME) *start*))
     INTERNAL-TIME-UNITS-PER-SECOND)))))

(DEFUN print-first-sequent (V5360)
 (COND
  ((AND (CONSP V5360) (CONSP (CAR V5360)) (CONSP (CDR V5360))
    (CONSP (CDR (CDR V5360))) (NULL (CDR (CDR (CDR V5360)))))
   (LET* ((V5361 (CAR V5360)) (V5362 (CDR V5360)))
    (print-sequent (CAR V5361) (CAR V5362) (THE NUMBER (length V5361))
     (1+ (THE NUMBER (length (CAR (CDR V5362))))) *inferences*)))
  (T (implementation_error 'print-first-sequent))))

(DEFUN update-proof (V5716 V5717)
 (COND
  ((AND (CONSP V5717) (CONSP (CDR V5717)) (CONSP (CDR (CDR V5717)))
    (NULL (CDR (CDR (CDR V5717)))))
   (LET* ((V5718 (CAR V5717)) (V5719 (CDR V5717)) (V5720 (CAR V5719)))
    (CONS V5718
     (CONS V5720
      (CONS
       (CONS (CONS V5718 (CONS V5720 (CONS V5716 (CONS *inferences* NIL))))
        (CAR (CDR V5719)))
       NIL)))))
  (T (implementation_error 'update-proof))))

(DEFUN print-sequent (V5370 V5371 V5372 V5373 V5374)
 (COND
  ((wrapper (tuple? V5370))
    (output "________________________________________________~%")
    (output "Step ~A. [~A]			~A refinement~P~%~%" V5373 V5372 V5374 V5374)
    (print-notes V5371) 
    (output "?- ") 
    (display-wff (snd V5370))
    (output "~%~%") 
    (display-wffs 1 (fst V5370)))
  (T (implementation_error 'print-sequent))))

(DEFUN print-notes (V5379)
 (COND ((NULL V5379) NIL) 
          (T (output "Notes: ~{~A ~}~%~%" V5379))))

(DEFUN display-wff (V5360)
 (COND
  ((AND (CONSP V5360) (CONSP (CDR V5360)) (EQ '$$ (CAR (CDR V5360)))
    (CONSP (CDR (CDR V5360))) (NULL (CDR (CDR (CDR V5360))))
    (wrapper (pr_rb?)))
    (outputrb "~S" (CAR V5360)) 
    (FORMAT T " : ")
    (outputrb "~S" (CAR (CDR (CDR V5360)))))
  ((wrapper (pr_rb?)) (outputrb "~S" V5360))
  ((AND (CONSP V5360) (CONSP (CDR V5360)) (EQ '$$ (CAR (CDR V5360)))
    (CONSP (CDR (CDR V5360))) (NULL (CDR (CDR (CDR V5360)))))
    (output "~S" (CAR V5360)) 
    (FORMAT T " : ")
    (output "~S" (CAR (CDR (CDR V5360)))))
  (T (output "~S" V5360))))

(DEFUN pr_rb? () *display-rb*)

(DEFUN display-wffs (V822 V823)
 (COND ((NULL V823) NIL)
  ((CONSP V823)
    (output "~A. " V822) 
    (display-wff (CAR V823)) (output "~%")
    (display-wffs (THE NUMBER (+ V822 1)) (CDR V823)))
  (T (implementation_error 'display-wffs))))

(DEFUN read-tactic ()
  (output "~%~A " *atp-prompt*)
  (LET ((Tactic (extract_tactic (SETQ *tactic* (prooftool_lineread)))))
   (IF (EQ 'true (tactic? Tactic)) 
        (eval Tactic)
        (PROGN (output "error: this is not a tactic.~%") 
                   (read-tactic)))))

(DEFUN prooftool_lineread ()
 (HANDLER-CASE (MAPCAR 'enter_format (lineread)) (ERROR (condition) (query_exit))))

(DEFUN query_exit ()
 (READ-CHAR)
 (IF (Y-OR-N-P "Exit proof? ") (ERROR "Proof aborted.~%"))
 (output "~%~A " *atp-prompt*)
 (prooftool_lineread))

(SETQ *atp-prompt* ">")

(DEFUN tactic? (V824)
 (COND
  ((AND (CONSP V824) (EQ 'back (CAR V824)) (CONSP (CDR V824))
    (NULL (CDR (CDR V824))))
   'true)
  (T
   (qi_= (typechecks? NIL V824 (CONS 'goals (CONS '--> (CONS 'goals NIL))))
    (CONS 'goals (CONS '--> (CONS 'goals NIL)))))))

(DEFUN extract_tactic (V825)
 (COND ((AND (CONSP V825) (NULL (CDR V825))) (CAR V825))
          ((CONSP V825) V825) 
          (T (implementation_error 'extract_tactic))))

(DEFUN back (V1241 V1242)
 (COND ((EQUAL -1 V1241) V1242)
  ((AND (CONSP V1242) (CONSP (CDR V1242)) (CONSP (CDR (CDR V1242)))
    (CONSP (CAR (CDR (CDR V1242)))) (CONSP (CAR (CAR (CDR (CDR V1242)))))
    (CONSP (CDR (CAR (CAR (CDR (CDR V1242))))))
    (CONSP (CDR (CDR (CAR (CAR (CDR (CDR V1242)))))))
    (CONSP (CDR (CDR (CDR (CAR (CAR (CDR (CDR V1242))))))))
    (NULL (CDR (CDR (CDR (CDR (CAR (CAR (CDR (CDR V1242)))))))))
    (NULL (CDR (CDR (CDR V1242))))
    (wrapper (and (integer? V1241) (qi_>= V1241 0))))
   (LET*
    ((V1243 (CDR V1242)) (V1244 (CDR V1243)) (V1245 (CAR V1244))
     (V1246 (CAR V1245)))
    (back (1- V1241)
     (CONS (CAR V1246) (CONS (CAR (CDR V1246)) (CONS (CDR V1245) NIL))))))
  (T (output "Cannot process this command.~%") V1242)))

(DEFUN thin (V838 V839)
 (COND
  ((AND (CONSP V839) (CONSP (CAR V839)) (wrapper (tuple? (CAR (CAR V839))))
    (CONSP (CDR V839)) (CONSP (CDR (CDR V839)))
    (NULL (CDR (CDR (CDR V839)))))
   (LET* ((V840 (CAR V839)) (V841 (CAR V840)))
    (CONS (CONS (@p (thin* V838 (fst V841)) (snd V841)) (CDR V840))
     (CDR V839))))
  (T (implementation_error 'thin))))

(DEFUN thin* (V852 V853)
 (COND ((AND (EQ 1 V852) (CONSP V853)) (CDR V853))
  ((CONSP V853) (CONS (CAR V853) (thin* (THE NUMBER (- V852 1)) (CDR V853))))
  (T V853)))

(DEFUN swap (V854 V855 V856)
 (COND
  ((AND (CONSP V856) (CONSP (CAR V856)) (wrapper (tuple? (CAR (CAR V856))))
    (CONSP (CDR V856)) (CONSP (CDR (CDR V856)))
    (NULL (CDR (CDR (CDR V856)))))
   (LET* ((V857 (CAR V856)) (V858 (CAR V857)))
    (CONS (CONS (@p (exchange V854 V855 (fst V858)) (snd V858)) (CDR V857))
     (CDR V856))))
  (T (implementation_error 'swap))))

(DEFUN rotate (V863 V864 V865)
 (COND
  ((AND (CONSP V865) (CONSP (CDR V865)) (CONSP (CDR (CDR V865)))
    (NULL (CDR (CDR (CDR V865)))))
   (CONS (exchange V863 V864 (CAR V865)) (CDR V865)))
  (T (implementation_error 'rotate))))

(DEFUN dump-proof (Filename)
(LET ((AbsFileName (FORMAT NIL "~A~A" *qi_home_directory* Filename)))
 (DRIBBLE AbsFileName) 
 (dump-proof-help (REVERSE *proof*) 1) 
 (DRIBBLE) 
 Filename))

(DEFUN dump-proof-help (V5371 V5372)
 (COND ((NULL V5371) (output "proved in ~A refinements~%~%" *inferences*))
  ((AND (CONSP V5371) (CONSP (CAR V5371)) (CONSP (CAR (CAR V5371)))
    (CONSP (CDR (CAR V5371))) (CONSP (CDR (CDR (CAR V5371))))
    (CONSP (CDR (CDR (CDR (CAR V5371)))))
    (NULL (CDR (CDR (CDR (CDR (CAR V5371)))))))
   (LET*
    ((V5373 (CAR V5371)) (V5374 (CAR V5373)) (V5375 (CDR V5373))
     (V5376 (CDR V5375)))
     (print-sequent (CAR V5374) (CAR V5375) (THE NUMBER (length V5374)) V5372
      (CAR (CDR V5376)))
     (output "~%~A ~{~S ~}~%" *atp-prompt* (CAR V5376))
     (dump-proof-help (CDR V5371) (THE NUMBER (+ V5372 1)))))
  (T (implementation_error 'dump-proof-help))))

(MAPC (FUNCTION (LAMBDA (F) (IMPORT F :COMMON-LISP-USER)))
         *sysfuncs*)

(MAPC (FUNCTION (LAMBDA (F) (IMPORT F :COMMON-LISP-USER)))
     '(listit true false @ { } -> <- --> _ $$ where loaded associated
      boolean symbol string character list number array constant?
      ; && mode name >> yes no Context typecheck wff goals
     Accum Sequents Notes Proof Parameters Assumptions ok proved
     compiled verified note call suppose suppose! when when! eval! =!
     profile-stats profiled -*- -s- is is! macroexpand cases
     y-combinator commit! cut* parameter -*- -s- -o- -end-of-list-))

(SHADOWING-IMPORT '! :COMMON-LISP-USER)
"(SHADOWING-IMPORT ':= :COMMON-LISP-USER)"



