#|
Напишите макрос do-spec-nums, который позволяет перечислить все специальные числа из заданного диапазона. Будем называть специальными целые положительные числа (в 
десятичной системе счисления), которые состоят из двух и более четных цифр, например: 22, 68, 204 и т.д.
Макрос должен иметь следующий вид:

(defmacro do-spec-nums ((var startv endv) &body body)
;; код макроса
)


Аргумент var – имя переменной, которая на каждой итерации принимает значение очередного специального числа; аргумент startv – значение, с которого начинается проверка 
(начало диапазона); endv – значение, на котором заканчивается проверка (конец диапазона).

Пример: Вызов (do-spec-nums (n 5 25) (fresh-line) (prinс n)) должен вывести на экран следующие числа:
20
22
24


Внимание! Вам необходимо написать только макрос (и, возможно, какие-то вспомогательные макросы и функции). Код для проверки, который использует написанный вами макрос, 
уже добавлен в окно редактора. Не удаляйте и не меняйте его!
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

;;;; !!! не удаляйте и не изменяйте этот код, он необходим для проверки !!! ;;;;
(defparameter s (read))
(print (with-output-to-string (*standard-output*) (do-spec-nums (i (elt s 0) (elt s 1)) (princ i)) (do-spec-nums (i (elt s 2) (elt s 3)) (princ i))))
;;;; !!! не удаляйте и не изменяйте этот код, он необходим для проверки !!! ;;;;
