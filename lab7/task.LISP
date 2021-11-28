#|
Напишите функцию format+, которая поддерживает все возможности функции format, но вдобавок позволяет обрабатывать форматную директиву ~z, предназначенную для 
форматирования телефонного номера.

Номер должен форматироваться следующим образом: +CC(ABC_DEF)DIGITS, где CC – код страны, ABC_DEF – код зоны нумерации (код населенного пункта или географически 
неопределяемого узла связи), DIGITS – цифры номера. Если в DIGITS семь цифр, то они должны разбиваться на группу из трех цифр, дефис, группу из двух цифр, дефис и группу из
двух цифр. Если суммарное количество цифр во всех частях номера больше 15, то номер считать неправильным и вместо него выводить строку [bad phone].

Аргументом директивы ~z должен выступать список из трех элементов (CC ABC_DEF DIGITS), каждый из которых может быть либо целым положительным числом, либо строкой. Если 
какой-либо из элементов списка равен nil, номер считается неправильным.

Примеры форматирования с помощью директивы ~z:
(format+ t "Ordеrs ~d fоr numbеr ~z" 42 '(7 972 "9231018")) ==> Ordеrs 42 fоr numbеr +7(972)923-10-18
(format+ nil "~z is calling..." '(51 3882 99342)) ==> "+51(3882)99342 is calling..."
(format+ nil "This is ~z!!!" '(111 222 333444555666)) ==> "This is [bad phone]!!!"

Замечание:
Для упрощения задания можно считать, что директива ~z не поддерживается внутри условной директивы ~[, внутри циклов ~{ и в сочетании с директивами выбора аргументов типа
~:*

Подсказка:
Разумеется, реализовывать самостоятельно все директивы функции format не нужно. Нужно обработать директиву ~z и затем передать новую форматную строку и измененный список 
аргументов в фукнцию format.
|#

(defun check-and-transforming (n)
  (if (not (listp n))
      "[bad phone]"
  (let ((lst (mapcan #'(lambda (x) (if (string= "" (remove-if #'digit-char-p x)) (list x) NIL)) (mapcan #'(lambda (x) (if (not (stringp x)) (list (write-to-string x)) (list x))) n))))
    (if (or (not (eq (length lst) 3)) (> (reduce #'(lambda(s x) (+ s (length x))) lst :initial-value 0) 15))
       "[bad phone]"
      (let ((thd (third lst)))
        (if (= (length thd) 7)
            (setf thd (concatenate 'string (subseq thd 0 3) "-" (subseq thd 3 5) "-" (subseq thd 5))))
        (concatenate 'string "+" (first lst) "(" (second lst) ")" thd))))))

(defun count-directives-without-arguments (ms &rest ss)
  (let ((counter 0))
    (loop for s in ss do
          (loop for x from 0 to (- (length ms) (length s)) do
                (if (string= s (subseq ms x (+ x (length s))))
                    (setf counter (+ counter 1)))))
    counter))

(defun check-symbol (symb &rest asymbs)
  (loop for asymb in asymbs do
        (if (char-equal symb asymb)
            (return-from check-symbol T)))
  (return-from check-symbol NIL))

(defun count-directives-with-optional-numbers (s)
  (let* ((counter 0) (tilda NIL))
    (loop for x from 0 below (length s) do
          (progn
            (let ((sign (aref s x)))
              (if (eq tilda T)
                  (progn
                    (if (and (check-symbol sign #\< #\t #\[ #\{ #\% #\&) (not (null (digit-char-p (aref s (- x 1))))))
                        (progn
                          (setf counter (+ counter 1))
                          (setf tilda NIL)))
                    (if (char-equal sign #\Space)
                        (setf tilda NIL))
                    (let ((previous-sign (aref s (- x 1))))
                      (if (and (check-symbol sign #\c #\d #\R #\b #\o #\x #\f #\e #\g #\$ #\a #\r #\s #\w #\*) (or (check-symbol previous-sign sign #\: #\@) (not (null (digit-char-p previous-sign)))))
                          (setf tilda NIL)))))
              (if (char-equal #\~ sign)
                  (setf tilda T)))))
    counter))

(defun change-arguments (numargs args)
  (let ((newargs))
    (loop for x from 0 below (length args)
          for arg in args do
          (progn
            (if (and (not (null numargs)) (= (car numargs) x))
                (progn
                  (setf arg (check-and-transforming arg))
                  (setf numargs (remove (car numargs) numargs))))
            (if (null newargs)
                (setf newargs (list arg))
              (setf newargs (append newargs (list arg))))))
    newargs))                         

(defun format+ (*out-stream* str &rest args)
  (let* ((before) (number) (numargs) (e))
    (loop while (search "~z" str) do
          (progn
            (setf e (search "~z" str))
            (setf before (subseq str 0 e))
            (setf number (- (count #\~ before) (count-directives-without-arguments before "~%" "~&" "~|" "~_" "~:_" "~@_" "~:@_" "~<" "~>" "~i" "~:i" "~t" "~:t" "~@t" 
                      "~:@t" "~?" "~@?" "~*" "~:*" "~@*" "~[" "~:[" "~@[" "~;" "~:;" "~{" "~}" "~(" "~:(" "~@(" "~:@(" "~)") (count-directives-with-optional-numbers before)))
            (if (null numargs)
                (setf numargs (list number))
              (setf numargs (append numargs (list number))))
            (setf (aref str (+ e 1)) #\a)))
    (apply #'format *out-stream* str (change-arguments numargs args))))
