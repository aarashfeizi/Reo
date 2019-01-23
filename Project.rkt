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

;(define fifo_and_mem
;  (lambda (a b m)
;    (if (= (length a) 0)
;        #t
;        (if (qnull? m)
;            (if (and (not (qnull? (car a))) (qnull? (car b))) (fifo_and_mem (cdr a) (cdr b) (car a)) #f)
;            (if (qnull? (car b))
;                (fifo_and_mem (cdr a) (cdr b) m)
;                (if (not (qnull? (car a))) #f
;                    (if (equal? (car b) m) (fifo_and_mem (cdr a) (cdr b) 'null) #f)
;                    )
;                )
;         ))))

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


(define join
  (lambda (a b c)
    (if (or (not (equal? (length a) (length b))) (not (equal? (length a) (length c)))) #f
        (if (null? a) #t
            (if (and (qnull? (car a)) (qnull? (car b))) (if (not (qnull? (car c))) #f (if (join (cdr a) (cdr b) (cdr c)) #t #f))
                (if (or (not (equal? (car a) (car (car c)))) (not (equal? (car b) (list-ref (car c) 1)))) #f
                    (if (join (cdr a) (cdr b) (cdr c)) #t #f))
             )
         )
     )
   ))

(define create_replicator_output
  (lambda (a)
    (list a a)))

(define create_sync_output
  (lambda (a)
    a))

(define connector
  (lambda (a b c)
     (if (or (not (equal? (length a) (length b))) (not (equal? (length a) (length c))))
         #f
         (let ([a_output (create_replicator_output a)])
           (let ([r_b (list-ref a_output 0)]
                 [r_c (list-ref a_output 1)])
             (and (sync r_c c) (syncdrain r_b b)))))))


(define merger
  (lambda (a b c)
    (if (or (not (equal? (length a) (length b))) (not (equal? (length a) (length c)))) #f
        (if (null? a) #t
            (if (or (and (qnull? (car a)) (equal? (car b) (car c))) (and (qnull? (car b)) (equal? (car a) (car c))))
                (merger (cdr a) (cdr b) (cdr c)) #f)
            )
        )
    )
  )

(define create_other_merger_input
  (lambda (a b c) ; b is null at the beginning
    (if (= (length a) 0)
        b
        (if (or (qnull? (car c)) (equal? (car c) (car a)))
             (create_other_merger_input (cdr a) (append b (list 'null)) (cdr c))
             (if (and (not_null? (car c)) (qnull? (car a)))
                  (create_other_merger_input (cdr a) (append b (list (car c))) (cdr c))
                  #f)))))

(define alternator
  (lambda (a b console)
    (if (not (three_eq (length a) (length b) (length console)))
        #f
        (let ([fifo1_output (create_other_merger_input a '() console)])
          (and (connector a b a) (connector b a b) (fifo1 b fifo1_output))))))
