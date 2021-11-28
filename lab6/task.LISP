#|
Ќапишите макрос do-spec-nums, который позвол€ет перечислить все специальные числа из заданного диапазона. Ѕудем называть специальными целые положительные числа (в дес€тичной системе счислени€), которые состо€т из двух и более четных цифр, например: 22, 68, 204 и т.д.
ћакрос должен иметь следующий вид:

(defmacro do-spec-nums ((var startv endv) &body body)
;; код макроса
)


јргумент var Ц им€ переменной, котора€ на каждой итерации принимает значение очередного специального числа; аргумент startv Ц значение, с которого начинаетс€ проверка (начало диапазона); endv Ц значение, на котором заканчиваетс€ проверка (конец диапазона).

ѕример: ¬ызов (do-spec-nums (n 5 25) (fresh-line) (prinс n)) должен вывести на экран следующие числа:
20
22
24


¬нимание! ¬ам необходимо написать только макрос (и, возможно, какие-то вспомогательные макросы и функции).  од дл€ проверки, который использует написанный вами макрос, уже добавлен в окно редактора. Ќе удал€йте и не мен€йте его!
|#


(defmacro with-gensyms- (names &body body)
  `(let ,(loop for n in names collect `(,n (gensym))) ,@body))

(defmacro with-gensyms-init (names init-vals &body body)
  `(with-gensyms- (,@names)
     `(let (,,@(loop for v in init-vals for n in names collect ``(,,n ,,v)))
        ,,@body)))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(with-gensyms-init (,@gensyms) (,@names)
       (let (,@(loop for n in names for g in gensyms
                     collect `(,n ,g)))
         ,@body))))


(defun specp (n)
  (setf n (abs n))
  (let ((cnt 0))
    (loop while (and (< cnt 2) (> n 0))
          do (progn
               (if (evenp (mod n 10))
                   (setf cnt (+ cnt 1)))
               (setf n (floor (/ n 10)))))
    (if (eq cnt 2)
        T
      NIL)))
                  
(defmacro do-spec-nums ((var startv endv) &body body)
  (once-only (startv endv)
  `(loop for ,var from ,startv to ,endv when (specp ,var) do (progn ,@body))))

;;;; !!! не удал€йте и не измен€йте этот код, он необходим дл€ проверки !!! ;;;;
(defparameter s (read))
(print (with-output-to-string (*standard-output*) (do-spec-nums (i (elt s 0) (elt s 1)) (princ i)) (do-spec-nums (i (elt s 2) (elt s 3)) (princ i))))
;;;; !!! не удал€йте и не измен€йте этот код, он необходим дл€ проверки !!! ;;;;

            

