#lang racket

(define sync
  (lambda (a b)
    (if (not (= (length a) (length b)))
        #f
        (if (= (length a) 0)
            #t
            (if (equal? (car a) (car b))
                (sync (cdr a) (cdr b))
                #f)))))
(define three_eq
  (lambda (a b c)
           (and (equal? a b) (equal? a c))))

(define replicator
  (lambda (a b c)
    (if (or (not (= (length a) (length b))) (not (= (length a) (length c))))
        #f
        (if (= (length a) 0)
            #t
            (if (three_eq (car a) (car b) (car c))
                (replicator (cdr a) (cdr b) (cdr c))
                #f)))))

(define not_null?
  (lambda (a)
    (not (equal? 'null a))))

(define qnull?
  (lambda (a)
    (equal? 'null a)))

(define fifo_and_mem
  (lambda (a b m)
    (if (= (length a) 0)
        #t
        (if (and (not_null? (car a)) (not_null? m))
             #f
            (if (and (not_null? (car a)) (qnull? (car b)))
                (fifo_and_mem (cdr a) (cdr b) (car a))
                (if (and (qnull? m) (not_null? (car b)))
                     #f
                    (if (not_null? m)
                        (if (equal? m (car b))
                            (fifo_and_mem (cdr a) (cdr b) 'null)
                            (fifo_and_mem (cdr a) (cdr b) m))
                        #t)))))))

(define fifo1
  (lambda (a b)
    (if (not (= (length a) (length b)))
        #f
        (fifo_and_mem a b 'null))))


(define syncdrain
  (lambda (a b)
                    (if (not (= (length a) (length b))) #f
                        (if (null? a) #t
                            (if (or (and (qnull? (car a)) (not (qnull? (car b)))) (and (qnull? (car b)) (not (qnull? (car a))))) #f
                                (if (syncdrain (cdr a) (cdr b)) #t #f))))
                    ))

