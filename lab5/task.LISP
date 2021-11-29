#|
Дана информация о телефонных разговорах сотрудников некоторой компании в виде списка CDR-записей (Call Detail Record). Каждая запись представляет собой строку, содержащую 
сведения об одном звонке. Формат записи следующий: "Номер-А;Номер-Б;Длительность-разговора". "Номер А" – это номер вызывающего абонента (то есть номер того, кто звонил),
"Номер Б" – номер вызываемого абонента (то есть кому звонили), "Длительность-разговора" - длительность разговорного состояния в секундах. Разделитель полей – точка с 
запятой ';', между полей могут присутствовать пробелы (но не внутри полей). Записи другого формата считать некорректными и игнорировать.

Внутренние номера сотрудников компании – четырехзначные и начинаются с единицы, например, "1101", "1299" и т.д. Внешние номера записаны в соответствии с общим 
международным телекоммуникационным планом нумерации E.164, то есть начинаются с символа '+', за которым следует не более 15 цифр: код страны и телефонный номер, например
"+78129991316" (7 - Россия, 812 - Санкт-Петербург, 999-13-16 – номер) или "+3314562024112" (33 - Франция, 1 - Париж, 45 62 02 41 12 - номер).

Необходимо прочитать список CDR-записей из стандартного потока ввода и определить, какова общая длительность исходящих звонков сотрудника компании, которому чаще всего 
звонили из Норвегии (код страны 47). В стандартный поток вывода записать общую длительность исходящих звонков этого сотрудника.

Пример: Дан список CDR-записей ("1101;+79119989911;122" " +47231114563;1102;347" "+47023334521;1101;134" "1102;+49023334521;811" "1102;1101;42" "ERR01:1234;;;0;0" 
" +390145211212; 1102; 93" "+47023414522;1102;753" "1102;+79119989911;771"). Из Норвегии чаще всего звонили абоненту 1102. Этот абонент совершил исходящие звонки на 
номера 1101, +49023334521 и +79119989911 общей длительностью 1624 секнуды. В стандартный поток вывода необходимо записать число 1624.

Примечание: Настоящие CDR-записи, которые можно получить от настоящих телефонных станций, несут гораздо больше информации и не содержат приведенных к E.164 номеров, а 
содержат либо точную последовательность цифр, набранных на аппарате абонента, либо АОН, пришедший из ТФОП.
|#

(defun remove-spaces (str)
  (remove-if #'(lambda(x) (eq (char-equal x #\Space) T)) str))

(defun pluses (str)
  (let ((p1 (position #\+ str)) (p2 (position #\+ str :from-end T)))
    (if (eq p1 NIL)
        T
      (if (eq p1 p2)
          (or (eq p1 0) (eq p1 (+ (position #\; str) 1)))
        (and (eq p1 0) (eq p2 (+ (position #\; str) 1)))))))

(defun filter (bstr &aux str)
  (setf str (remove-spaces bstr))
  (if (not (and (= (count #\; str) 2) (not (eq (+ (position #\; str :from-end T) 1) (length str))) (< (count #\+ str) 3)))
      NIL
    (if (not (string-equal "" (remove-if #'(lambda(x) (or (not (eq (digit-char-p x) NIL)) (eq (char-equal x #\+) T) (eq (char-equal x #\;) T))) str)))
        NIL
      (if (null (pluses str))
          NIL
        (let* ((p1 (position #\; str)) (p2 (position #\; str :from-end T)))
          (if
              (char-equal #\+ (elt str 0))
              (if (or (< p1 4) (> p1 16))
                  NIL
                (if (char-equal #\+ (elt str (+ p1 1)))
                    (not (or (< (- p2 p1) 5) (> (- p2 p1) 17)))   
                  (if (char-equal #\1 (elt str (+ p1 1)))
                      (eq (- p2 p1) 5)
                    NIL)))
            (if (char-equal #\1 (elt str 0))
                (if (eq p1 4)
                    (if (char-equal #\+ (elt str (+ p1 1)))
                        (not (or (< (- p2 p1) 5) (> (- p2 p1) 17)))
                      (if (char-equal #\1 (elt str (+ p1 1)))
                          (eq (- p2 p1) 5)
                        NIL))
                  NIL)
              NIL)))))))

(defun check-norwegian-number (d)
  (and (char-equal #\+ (elt d 0)) (char-equal #\4 (elt d 1)) (char-equal #\7 (elt d 2)) (= (- (position #\; d :from-end T) (position #\; d)) 5)))

(defun cars (lst)
  (nreverse
   (reduce #' (lambda (acc x)
           (push (car x) acc))
           lst :initial-value NIL)))

(defun hash-table-key-value-pairs (h)
  (let (pairs)
    (maphash #'(lambda (k v)
                 (push (list k v) pairs)) h)
    (nreverse pairs)))

(defun get-first (s)
  (subseq s 0 (position #\; s)))

(defun get-second (s)
  (subseq s (+ (position #\; s) 1) (position #\; s :from-end T)))

(defun get-third (s)
  (subseq s (+ (position #\; s :from-end T) 1))) 

(defun get-prior (data)
  (setf data (mapcan #'(lambda (e) (if (filter e) (list (remove-spaces e)) NIL)) data)) 
  (let* ((ht (make-hash-table :test #'equal)) (emp (mapcan #'(lambda(d) (if (check-norwegian-number d) (list (get-second d)) NIL)) data)))
    (reduce #'(lambda (h e) (if (and (check-norwegian-number e) (member (get-second e) emp :test #'equal)) (let ((val (gethash (get-second e) h))) 
                                                                             (cond ((null val) (setf (gethash (get-second e) h) 1)) (T (setf (gethash (get-second e) h) (+ val 1)))))) h) data :initial-value ht)
    (let* ((lst (sort (hash-table-key-value-pairs ht) #'> :key #'second)) (mlst (cars (remove-if-not #'(lambda (d) (= (cadr d) (cadar lst))) lst))))
      (reduce #'(lambda (h e) (if (member (get-first e) mlst :test #'equal) (let ((val (gethash (get-first e) h))) (setf (gethash (get-first e) h) (+ val (parse-integer (get-third e)))))) h) data
              :initial-value (reduce #'(lambda (h e) (if (member e mlst :test #'equal) (setf (gethash e h) 0) (remhash e h)) h) (cars lst) :initial-value ht))
      (print (cadar (sort (hash-table-key-value-pairs ht) #'> :key #'second))))))

(defparameter data (read))
(get-prior data) 
(makunbound 'data)

