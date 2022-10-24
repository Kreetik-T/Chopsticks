;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chopsticks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (@htdd Chopsticks)
(define-struct chop (p1h1 p1h2 p2h1 p2h2))
;; Chopsticks is (make-chop Number Number Number Number)
(define CH1 (make-chop 1 1 1 1))

;(define (fn-for-chop ch)
;  (... (p1h1 ch)
;       (p1h2 ch)
;       (p2h1 ch)
;       (p2h2 ch)))

;; (@htdf chopsticks-solve)
;; (@signature Chopsticks -> (listof Chopsticks) or false)
;; Returns valid chopstick end-game solution or false

(check-expect (chopsticks-solve CH1)
              (list
               (make-chop 1 1 1 1)
               (make-chop 1 2 1 1)
               (make-chop 2 2 1 1)
               (make-chop 2 4 1 1)
               (make-chop 3 4 1 1)
               (make-chop 3 4 4 1)
               (make-chop 3 5 4 1)
               (make-chop 1 2 4 1)
               (make-chop 5 2 4 1)
               (make-chop 1 1 4 1)
               (make-chop 5 1 4 1)
               (make-chop 5 1 5 1)
               (make-chop 5 2 5 1)
               (make-chop 1 1 5 1)
               (make-chop 2 1 5 1)
               (make-chop 2 3 5 1)
               (make-chop 3 3 5 1)
               (make-chop 3 3 5 4)
               (make-chop 3 3 2 2)
               (make-chop 3 3 5 2)
               (make-chop 3 3 1 1)
               (make-chop 3 3 4 1)
               (make-chop 4 3 4 1)
               (make-chop 4 3 4 5)
               (make-chop 4 3 2 2)
               (make-chop 4 3 5 2)
               (make-chop 4 3 1 1)
               (make-chop 4 3 5 1)
               (make-chop 5 3 5 1)
               (make-chop 1 2 5 1)
               (make-chop 2 2 5 1)
               (make-chop 2 4 5 1)
               (make-chop 3 4 5 1)
               (make-chop 3 4 5 5)))

(define (chopsticks-solve ch0)
  (local [(define (fn-for-chop ch path p1? ch-wl path-wl p1?-wl)
            (cond [(solved? ch) (append path (list ch))]
                  [(member? ch path) (fn-for-loc
                                      ch-wl
                                      path-wl
                                      p1?-wl)]
                  [else
                   (fn-for-loc
                    (append (next-valid-ch ch p1?) ch-wl)
                    (append (map (λ (e) (append path (list ch)))
                                 (next-valid-ch ch p1?))
                            path-wl)
                    (append (map (λ (e) (not p1?))
                                 (next-valid-ch ch p1?))
                            p1?-wl))]))
          
          (define (fn-for-loc ch-wl path-wl p1?-wl)
            (cond [(empty? ch-wl) false]
                  [else
                   (fn-for-chop (first ch-wl) (first path-wl) (first p1?-wl)
                                (rest ch-wl)  (rest path-wl)  (rest p1?-wl))]))
          
          (define (solved? ch1)
            (local [(define p1h1 (chop-p1h1 ch1))
                    (define p1h2 (chop-p1h2 ch1))
                    (define p2h1 (chop-p2h1 ch1))
                    (define p2h2 (chop-p2h2 ch1))]
              (or (= 5 p1h1 p1h2)
                  (= 5 p2h1 p2h2))))

          (define (next-valid-ch ch p1?)
            (local [(define p1h1 (chop-p1h1 ch))
                    (define p1h2 (chop-p1h2 ch))
                    (define p2h1 (chop-p2h1 ch))
                    (define p2h2 (chop-p2h2 ch))

                    (define valid-p1-moves (append
                                            (cond [(or (and (= p1h1 2)
                                                            (= p1h2 5))
                                                       (and (= p1h1 5)
                                                            (= p1h2 2)))
                                                   (list
                                                    (make-chop 1
                                                               1
                                                               p2h1
                                                               p2h2))]
                                                  [(or (and (= p1h1 3)
                                                            (= p1h2 5))
                                                       (and (= p1h1 5)
                                                            (= p1h2 3)))
                                                   (list
                                                    (make-chop 1
                                                               2
                                                               p2h1
                                                               p2h2))]
                                                  [(or (and (= p1h1 4)
                                                            (= p1h2 5))
                                                       (and (= p1h1 5)
                                                            (= p1h2 4)))
                                                   (list
                                                    (make-chop 2
                                                               2
                                                               p2h1
                                                               p2h2))]
                                                  [else empty])
                                            (list (make-chop p1h1
                                                             (+ p1h1 p1h2)
                                                             p2h1
                                                             p2h2)
                                                  (make-chop p1h1
                                                             p1h2
                                                             (+ p1h1 p2h1)
                                                             p2h2)
                                                  (make-chop p1h1
                                                             p1h2
                                                             p2h1
                                                             (+ p1h1 p2h2))
                                                  (make-chop (+ p1h2 p1h1)
                                                             p1h2
                                                             p2h1
                                                             p2h2)
                                                  (make-chop p1h1
                                                             p1h2
                                                             (+ p1h2 p2h1)
                                                             p2h2)
                                                  (make-chop p1h1
                                                             p1h2
                                                             p2h1
                                                             (+ p1h2 p2h2)))))
                    (define valid-p2-moves (append
                                            (cond [(or (and (= p2h1 2)
                                                            (= p2h2 5))
                                                       (and (= p2h1 5)
                                                            (= p2h2 2)))
                                                   (list
                                                    (make-chop p1h1
                                                               p1h2
                                                               1
                                                               1))]
                                                  [(or (and (= p2h1 3)
                                                            (= p2h2 5))
                                                       (and (= p2h1 5)
                                                            (= p2h2 3)))
                                                   (list
                                                    (make-chop p1h1
                                                               p1h2
                                                               1
                                                               2))]
                                                  [(or (and (= p2h1 4)
                                                            (= p2h2 5))
                                                       (and (= p2h1 5)
                                                            (= p2h2 4)))
                                                   (list
                                                    (make-chop p1h1
                                                               p1h2
                                                               2
                                                               2))]
                                                  [else empty])
                                            (list (make-chop (+ p2h1 p1h1)
                                                             p1h2
                                                             p2h1
                                                             p2h2)
                                                  (make-chop p1h1
                                                             (+ p2h1 p1h2)
                                                             p2h1
                                                             p2h2)
                                                  (make-chop p1h1
                                                             p1h2
                                                             p2h1
                                                             (+ p2h1 p2h2))
                                                  (make-chop (+ p2h2 p1h1)
                                                             p1h2
                                                             p2h1
                                                             p2h2)
                                                  (make-chop p1h1
                                                             (+ p2h2 p1h2)
                                                             p2h1
                                                             p2h2)
                                                  (make-chop p1h1
                                                             p1h2
                                                             (+ p2h2 p2h1)
                                                             p2h2))))

                    (define (filter-moves ch0)
                      (not (or (> (chop-p1h1 ch0) 5)
                               (> (chop-p1h2 ch0) 5)
                               (> (chop-p2h1 ch0) 5)
                               (> (chop-p2h2 ch0) 5))))]
              
              (if p1?
                  (filter filter-moves valid-p1-moves)
                  (filter filter-moves valid-p2-moves))))]
    
    (fn-for-chop ch0 empty true empty empty empty)))