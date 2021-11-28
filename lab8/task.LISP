#|
Необходимо расширить синтаксис Лиспа так, чтобы многомерные массивы можно было создавать с помощью последовательности #Md1xd2x...xdn_I, где d1, d2, ..., dn – размерности 
массива (целые положительные числа), I – значение, которым следует инициализировать массив (целое или вещественное число). Размерности отделяются друг от друга буквами 'x'
или 'X', а значение инициализации – символом подчеркивания. Должна быть указана как минимум одна размерность. Значение инициализации (вместе с символом подчеркивания) может
отсутствовать, по умолчанию элементы массива должны инициализироваться нулями. В случае ошибок должно возвращаться значение nil.

Пример:
В результате вычисления формы (defparameter arr1 #M3x3_1.5) переменная arr1 должна содержать массив #2A((1.5 1.5 1.5) (1.5 1.5 1.5) (1.5 1.5 1.5)).
В результате вычисления формы (defparameter arr2 #M2x2x4) переменная arr2 должна содержать массив #3A(((0 0 0 0) (0 0 0 0)) ((0 0 0 0) (0 0 0 0))).
В результате вычисления формы (defparameter arr3 #M_2) переменная arr3 должна быть равна nil.
|#

(defun reading (s)
  (with-input-from-string (stream s)
    (let* ((lst NIL) (number 0) (value 0))
      (if (or (eq (peek-char NIL stream NIL) NIL) (char-equal (peek-char NIL stream NIL) #\_))
          (values NIL NIL)
        (progn
          (loop while (and  (not (eq (peek-char NIL stream NIL) NIL)) (not (char-equal (peek-char NIL stream NIL) #\_))) do
                (let ((ch (read-char stream)))
                  (cond
                   ((not (null (digit-char-p ch))) (setf number (+ (* number 10) (digit-char-p ch))))
                   ((char-equal ch (or #\x #\X))
                    (progn
                      (if (not lst)
                          (setf lst (list number))
                        (setf lst (append lst (list number))))
                      (setf number 0)))
                   (T (return-from reading (values NIL NIL))))))
          (if (not (zerop number))
              (if (not lst)
                  (setf lst (list number))
                (setf lst (append lst (list number)))))
          (let ((fraction NIL))
            (if (not (eq (peek-char NIL stream NIL) NIL))
                (progn
                  (read-char stream)
                  (loop while (not (eq (peek-char NIL stream NIL) NIL)) do
                          (let ((ch (read-char stream)))
                            (cond
                             ((and (not (null (digit-char-p ch))) (null fraction)) (setf value (+ (* value 10) (digit-char-p ch))))
                             ((char-equal ch #\.)
                              (if (not (null fraction))
                                  (return-from reading (values NIL NIL))
                                  (setf fraction (/ 1 10.0))))
                            ((and (not (null (digit-char-p ch))) (not (null fraction)))
                             (progn
                               (setf value (+ value (* (digit-char-p ch) fraction)))
                               (setf fraction (/ fraction 10.0))))
                            (T (return-from reading (values NIL NIL)))))))))
              (values lst value))))))

(set-dispatch-macro-character #\# #\M
                              #'(lambda (stream c1 c2)
                                  (declare (ignore c1 c2))
                                  (multiple-value-bind (lst value) (reading (string (read stream NIL (values) T)))
                                    (if (null lst)
                                        NIL
                                      `(quote
                                        ,(make-array lst :initial-element value))))))
