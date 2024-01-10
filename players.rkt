#lang racket

;; Provided functions for A10.Q03

(provide goldfish cautious reckless follow)
         
(define (card? c)
  (cond [(integer? c) (and (<= 2 c) (>= 10 c))]
        [(symbol? c) (or (symbol=? c 'Jack) (symbol=? c 'Queen)
                         (symbol=? c 'King) (symbol=? c 'Ace)
                         (symbol=? c 'Black) (symbol=? c 'Red))]
        [else false]))

(define (card-value card)
  (cond [(and (integer? card) (= card 2)) 100]
        [(integer? card) card]
        [(symbol=? card 'Jack) 11]
        [(symbol=? card 'Queen) 12]
        [(symbol=? card 'King) 13]
        [(symbol=? card 'Ace) 14]
        [(symbol=? card 'Black) 200]
        [else 300]))

(define (card=? card1 card2) (= (card-value card1) (card-value card2)))

(define (card<? card1 card2) (< (card-value card1) (card-value card2)))

(define (card>? card1 card2) (> (card-value card1) (card-value card2)))

(define (card<=? card1 card2) (<= (card-value card1) (card-value card2)))

(define (card>=? card1 card2) (>= (card-value card1) (card-value card2)))

(define (insert-card card hand)
  (cond [(empty? hand) (cons card empty)]
        [(card<? card (first hand)) (cons card hand)]
        [else (cons (first hand) (insert-card card (rest hand)))]))

(define (sort-cards loc)
  (cond [(empty? loc) empty]
        [else (insert-card (first loc) (sort-cards (rest loc)))]))


(define (remove-one-of-each hand)
  (cond [(or (empty? hand) (empty? (rest hand))) empty]
        [(not (card=? (first hand) (second hand)))
         (remove-one-of-each (rest hand))]
        [else (cons (first hand) (remove-one-of-each (rest hand)))]))

(define (remove-n-of-each n hand)
  (cond [(zero? n) hand]
        [else (remove-one-of-each (remove-n-of-each (sub1 n) hand))]))

(define (dedup-hand hand)
  (cond [(or (empty? hand) (empty? (rest hand))) hand]
        [(card=? (first hand) (second hand))
         (dedup-hand (rest hand))]
        [else (cons (first hand) (dedup-hand (rest hand)))]))

(define (find-kind n hand)
  (dedup-hand (remove-n-of-each (sub1 n) hand)))


(define (make-solos hand)
  (cond [(empty? hand) empty]
        [else (cons (list (first hand)) (make-solos (rest hand)))]))

(define (solos hand)
  (make-solos (find-kind 1 hand)))

(define (make-pairs lst)
  (cond [(empty? lst) empty]
        [else (cons (list (first lst) (first lst)) (make-pairs (rest lst)))]))

(define (pairs hand)
  (make-pairs (find-kind 2 hand)))


(define (make-trios lst)
  (cond [(empty? lst) empty]
        [else (cons (list (first lst) (first lst) (first lst))
                    (make-trios (rest lst)))]))

(define (trios hand)
  (make-trios (find-kind 3 hand)))

(define (rocket? hand)
  (and (= (length hand) 2)
       (card=? (first hand) 'Black)
       (card=? (second hand) 'Red)))

(define (bomb? hand)
  (and (= (length hand) 4)
       (card=? (first hand) (second hand))
       (card=? (first hand) (third hand))
       (card=? (first hand) (fourth hand))))

(define (hand-elementwise<? a b)
  (cond [(empty? a) (not (empty? b))]
        [(empty? b) false]
        [(card<? (first a) (first b)) true]
        [(card>? (first a) (first b)) false]
        [else (hand-elementwise<? (rest a) (rest b))]))

(define (hand<? a b)
  (cond [(rocket? a) false]
        [(rocket? b) true]
        [(bomb? a) (and (bomb? b)
                        (card<? (first a) (first b)))]
        [(bomb? b) true]
        [(= (length a) (length b)) (hand-elementwise<? a b)]
        [else (< (length a) (length b))]))

(define (insert-hands hand hands)
  (cond [(empty? hands) (cons hand empty)]
        [(hand<? hand (first hands)) (cons hand hands)]
        [(hand<? (first hands) hand)
         (cons (first hands) (insert-hands hand (rest hands)))]
        [else hands]))

(define (sort-hands unsorted-hands)
  (cond [(empty? unsorted-hands) empty]
        [else (insert-hands (first unsorted-hands)
                            (sort-hands (rest unsorted-hands)))]))

(define (card-follows? card1 card2)
  (= (add1 (card-value card1)) (card-value card2)))

(define (straight? hand)
  (cond [(empty? hand) true]
        [(> (card-value (first hand)) (card-value 'Ace)) false]
        [(empty? (rest hand)) true]
        [(card-follows? (first hand) (second hand)) (straight? (rest hand))]
        [else false]))

(define (filter-straights n hands)
  (cond [(empty? hands) empty]
        [(and (<= n (length (first hands))) (straight? (first hands)))
         (cons (first hands) (filter-straights n (rest hands)))]
        [else (filter-straights n (rest hands))]))

(define (first-n n lst)
  (cond [(zero? n) empty]
        [(empty? lst) empty]
        [else (cons (first lst) (first-n (sub1 n) (rest lst)))]))

(define (prefixes-helper n lst)
  (cond [(zero? n) empty]
        [(empty? lst) empty]
        [else (cons (first-n n lst) (prefixes-helper (sub1 n) lst))]))

(define (prefixes lst)
  (prefixes-helper (length lst) lst))

(define (subsequences lst)
  (cond [(empty? lst) empty]
        [else (append (prefixes lst) (subsequences (rest lst)))]))

(define (straights-of-length n hand)
  (filter-straights n (sort-hands (subsequences (dedup-hand hand)))))

(define (straights hand) (straights-of-length 5 hand))



(define (double-hand hand)
  (cond [(empty? hand) empty]
        [else (cons (first hand)
                    (cons (first hand)
                          (double-hand (rest hand))))]))
(define (double-hands hands)
  (cond [(empty? hands) empty]
        [else (cons (double-hand (first hands))
                    (double-hands (rest hands)))]))

(define (straight-pairs hand)
  (double-hands (straights-of-length 3 (find-kind 2 hand))))


(define (triple-hand hand)
  (cond [(empty? hand) empty]
        [else (cons (first hand)
                    (cons (first hand)
                          (cons (first hand) 
                                (triple-hand (rest hand)))))]))

(define (triple-hands hands)
  (cond [(empty? hands) empty]
        [else (cons (triple-hand (first hands)) (triple-hands (rest hands)))]))

(define (airplanes hand)
  (triple-hands (straights-of-length 2 (find-kind 3 hand))))

(define (bombs hand)
  (cond [(or (empty? hand) (empty? (rest hand))
             (empty? (rest (rest hand))) (empty? (rest (rest (rest hand)))))
         empty]
        [(and (eq? (first hand) (first (rest hand)))
              (eq? (first hand) (first (rest (rest hand))))
              (eq? (first hand) (first (rest (rest (rest hand))))))
         (cons (list (first hand) (first hand) (first hand) (first hand))
               (bombs (rest (rest (rest hand)))))]
        [else (bombs (rest hand))]))

(define (rockets hand)
  (cond [(or (empty? hand) (empty? (rest hand))) empty]
        [(and (eq? 'Black (first hand)) (eq? 'Red (second hand)))
         '((Black Red))]
        [else (rockets (rest hand))]))

(define (beats-r? hand1 hand2) 
  (cond [(or (empty? hand1) (empty? hand2)) false]
        [(and (empty? (rest hand1)) (empty? (rest hand2)))
         (card<? (first hand1) (first hand2))]
        [(or (empty? (rest hand1)) (empty? (rest hand2))) false]
        [else (and (card<? (first hand1) (first hand2))
                   (= (- (card-value (second hand1)) (card-value (first hand1)))
                      (- (card-value (second hand2)) (card-value (first hand2))))
                   (beats-r? (rest hand1) (rest hand2)))]))

(define (beats? hand1 hand2)
  (cond [(empty? hand1) (not (empty? hand2))]
        [(empty? hand2) false]
        [(rocket? hand1) false]
        [(rocket? hand2) true]
        [(and (bomb? hand2) (not (bomb? hand1))) true]
        [(and (bomb? hand1) (not (bomb? hand2))) false]
        [else (beats-r? hand1 hand2)]))

(define (all-hands holding)
  (sort-hands (append
               (solos holding)
               (append
                (straights holding)
                (append
                 (pairs holding)
                 (append
                  (straight-pairs holding)
                  (append
                   (trios holding)
                   (append
                    (airplanes holding)
                    (append
                     (bombs holding)
                     (rockets holding))))))))))

(define (filter-hands previous hands)
  (cond [(empty? hands) empty]
        [(beats? previous (first hands))
         (cons (first hands) (filter-hands previous (rest hands)))]
        [else (filter-hands previous (rest hands))]))

(define (follow previous holding)
  (filter-hands previous (all-hands holding)))

(define (both-passed played)
  (and (not (empty? played)) (empty? (first played))
       (not (empty? (rest played))) (empty? (second played))))

(define (goldfish hand role played)   
  (cond [(both-passed played) (list (first hand))]
        [else empty]))

(define (hand-to-beat played)
  (cond [(empty? played) empty]
        [(or (not (empty? (first played))) (empty? (rest played)))
         (first played)]
        [else (second played)]))

(define (cautious hand role played)
  (local [(define could-play (follow (hand-to-beat played) hand))]
    (cond [(empty? could-play) empty]
          [else (first could-play)])))

(define (last lst)
  (cond [(empty? lst) empty]
        [(empty? (rest lst)) (first lst)]
        [else (last (rest lst))]))

(define (reckless hand role played)
  (local [(define could-play (follow (hand-to-beat played) hand))]
    (cond [(empty? could-play) empty]
          [else (last could-play)])))