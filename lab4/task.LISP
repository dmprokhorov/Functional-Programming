#|
Прочитать из стандартного потока ввода список, содержащий целые числа, значения nil и другие списки. Рекурсивно заменить в прочитанном списке и вложенных в него
списках все списки, содержащие только нечетные числа, суммой этих чисел. Вывести в стандартный поток вывода полученный результат.

Пример 1:
Дан список (1 3 5 (7 9 1) 11). Список (7 9 1) содержит только нечетные числа, а значит, должен быть заменен на их сумму. Получаем список (1 3 5 17 11), который также 
содержит только нечетные числа, поэтому он заменяется на 37. Этот ответ и выводится в стандартный поток вывода.

Пример 2:
Дан список (0 1 nil (1 3 (1 3) 5) 6). Список (1 3) содержит только нечетные числа, поэтому он должен быть заменен на их сумму. Получаем список (0 1 nil (1 3 4 5) 6). 
Здесь больше нечего заменять, этот результат и выводится в стандартный поток вывода.
|#

(defun sum (lst)
  (cond ((null lst) 0)                    
        (T (+ (car lst) (sum (cdr lst))))))

(defun modify (lst &aux (ce NIL) (res '()) (r '()))
  (if lst
      (progn
        (dolist (element lst)
          (if (atom element)
              (progn
                (setf res (append res (list element)))
                (if (not (and (eq (integerp element) T) (eq (oddp element) T)))
                    (setf ce T)))
            (progn
              (setf r (modify element))
              (setf res (append res (list r)))
              (if (not (and (eq (integerp r) T) (eq (oddp r) T)))
                  (setf ce T)))))
        (if (eq ce NIL)
            (return-from modify (sum res))
          (return-from modify res))))
  lst)
 
(defparameter lst (read))
(print (modify lst))
(makunbound 'lst)

