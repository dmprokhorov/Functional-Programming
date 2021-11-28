#|
Прочитать беззнаковое целое число из стандартного потока ввода и в каждом байте числа инвертировать значения двух старших битов каждой тетрады, если младший бит тетрады единичный. Результат вывести в стандартныйпоток вывода.

Примеры преобразования:
65535 (11111111 11111111) ==> 13107 (00110011 00110011)
333444 (00000101 00010110 10000100) ==> 644740 (00001001 11010110 10000100)
|#

(defun reverse-high-bits (b)
    (dotimes (j 1 b)
        (when
            (eq
                (logbitp 0 b) T)
            (dotimes (i 2 b)
                (setf b (dpb (logxor (ldb (byte 1 (- 3 i)) b) 1) (byte 1 (- 3 i)) b))))))
            
(defvar x 0)
(setf x (read))
(dotimes (i (ceiling (integer-length x) 4))
    (setf x
        (dpb
            (reverse-high-bits (ldb (byte 4 (* i 4)) x))
            (byte 4 (* i 4)) x)))
(print x)
(makunbound 'x)

;ИЛИ

#|
(defun reverse-high-bits (b)
        (if (not (eq (logbitp 0 b) T)) b 
            (dotimes (i 2 b)
                (setf b (dpb (logxor (ldb (byte 1 (- 3 i)) b) 1) (byte 1 (- 3 i)) b)))))
            
(defvar x 0)
(setf x (read))
(dotimes (i (ceiling (integer-length x) 4))
    (setf x
        (dpb
            (reverse-high-bits (ldb (byte 4 (* i 4)) x))
            (byte 4 (* i 4)) x)))
(print x)
|#