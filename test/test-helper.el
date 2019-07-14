;;; test-helper.el --- Helpers for json-rpc-server-test.el


(defun json-rpc-server--structs-equal (instance1 instance2)
  "Compare two structs. Are they equal?

Equality here means all of the following are true:

  - Both `INSTANCE1' and `INSTANCE2' are the same type of struct

  - Every slot in `INSTANCE1' is equal to the same slot in
    `INSTANCE2', according to cl-equal.

An error will be raised if a type other than `cl-struct' is
provided.

This method gets around a quirk in normal struct comparison where
two equivalent structs will not count as equal."
  (unless (cl-struct-p instance1)
    (error "`instance1' not a cl-struct"))
  (unless (cl-struct-p instance2)
    (error "`instance2' not a cl-struct"))
  (and
   ;; Ensure they're both the same type of object
   (eq (type-of instance1)
       (type-of instance2))
   ;; Ensure every property is the same.
   (let ((struct-type
          ;; Remember - they have the same type.
          (type-of instance1)))
     (cl-every
      (lambda (slot-pair)
        ;; Ensure this slot is the same in both instances
        (let ((slot-name (car slot-pair)))
          (cl-equalp (cl-struct-slot-value struct-type slot-name instance1)
                     (cl-struct-slot-value struct-type slot-name instance2))))
      ;; Map over all slot names
      (cdr (cl-struct-slot-info struct-type))))))

;;; test-helper.el ends here
