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