
(defun myLength (lst)
  (if (null lst)
    0
    (+ 1 (myLength (cdr lst))))
  
  )


(defun myMerge (l1 l2)
   (cond 
     ((null l1) l2)
     ((null l2) l1)
     ((<= (car l1) (car l2))
      (cons (car l1) (myMerge (cdr l1) l2)))
     (t
      (cons (car l2) (myMerge l1 (cdr l2))))))



(defun allButLast (lst)
  (if (> (length (cdr lst)) 0)
    (cons (car lst) (allButLast(cdr lst))) 
    nil
    )
)


(defun myLast (lst)
    
  (cond 
    
    ((null lst) nil)
    ((= (length lst) 1) (car lst))
    (t (myLast (cdr lst)))
))


(defun rcons (val lst)
  (append lst (list val))
  )


(defun myReverse (lst)
  (if (null lst)
    nil
    (append (list (myLast lst)) (myReverse (allButLast lst)))
    )
  )


(defun palindromep (lst) 
  (cond
    ((null lst) t)
    ((= (length lst) 1) t)  
    ((equal (car lst) (myLast lst))
     (palindromep (allButLast (cdr lst)))
     )
  )
  
)


(defun mySearch (lst val)
  (cond
    ((null lst) nil)
    ((equal (car lst) val) t)
    (t
      (mysearch  (cdr lst) val )) 
    ) 
  )  



(print "***myLength***")
(print (mylength '(1 2 3 4)))
(print (mylength '()))

(print "***myMerge***")
(print (myMerge '(1 3 5) '(2 4 6)))
(print (myMerge '() '(2 4 6)))

(print "***allButLast***")
(print (allButLast '(a b c)))
(print (allButLast '()))

(print "***myLast***")
(print (myLast '(a b c)))
(print (myLast '()))
(print (myLast '(a)))

(print "***rcons***")
(print (rcons 'A '()))
(print (rcons 'C '(A B)))

(print "***myReverse***")
(print (myReverse '(a b c)))
(print (myReverse '(a)))
(print (myReverse '()))

(print "***palindrome***")
(print (palindromep '()))
(print (palindromep '(a)))
(print (palindromep '(a b a)))
(print (palindromep '(a b c)))

(print "***mySearch***")
(print (mySearch '(a b c) 'd))
(print (mySearch '() 'd))
(print (mySearch '(a) 'a))
(print (mySearch '(a b c) 'b))
