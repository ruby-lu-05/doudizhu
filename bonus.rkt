;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; *****************************************************************************************
;; Ruby Lu (21063222)
;; CS 135 Fall 2023
;; Assignment 10, Question 4 (Bonus)
;; *****************************************************************************************
;;

(require "players.rkt")

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
        [(and (card=? (first hand) (first (rest hand)))
              (card=? (first hand) (first (rest (rest hand))))
              (card=? (first hand) (first (rest (rest (rest hand))))))
         (cons (list (first hand) (first hand) (first hand) (first hand))
               (bombs (rest (rest (rest hand)))))]
        [else (bombs (rest hand))]))

(define (rockets hand)
  (cond [(or (empty? hand) (empty? (rest hand))) empty]
        [(and (card=? 'Black (first hand)) (card=? 'Red (second hand)))
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

(define (both-passed played)
  (and (not (empty? played)) (empty? (first played))
       (not (empty? (rest played))) (empty? (second played))))

(define (hand-to-beat played)
  (cond [(empty? played) empty]
        [(or (not (empty? (first played))) (empty? (rest played)))
         (first played)]
        [else (second played)]))

(define (last lst)
  (cond [(empty? lst) empty]
        [(empty? (rest lst)) (first lst)]
        [else (last (rest lst))]))

(define (remove-cards hand lst)
  (cond [(empty? hand) lst]
        [(empty? lst) empty]
        [(card=? (first hand) (first lst)) (remove-cards (rest hand) (rest lst))]
        [else (cons (first lst) (remove-cards hand (rest lst)))]))

(define (get-airplanes holding)
  (cond [(empty? (airplanes holding)) empty]
        [else (append (get-airplanes (remove-cards (first (reverse (airplanes holding)))
                                                   holding))
                      (list (first (reverse (airplanes holding)))))]))

(define (get-straight-pairs holding)
  (cond [(empty? (straight-pairs holding)) empty]
        [else (append (get-straight-pairs (remove-cards (first (reverse (straight-pairs holding)))
                                                        holding))
                      (list (first (reverse (straight-pairs holding)))))]))

(define (get-straights holding)
  (cond [(empty? (straights holding)) empty]
        [else (append (get-straights (remove-cards (first (reverse (straights holding)))
                                                   holding))
                      (list (first (reverse (straights holding)))))]))

(define (group-hand holding)
  (local [(define (new-hand removed holding)
            (remove-cards (foldr append empty removed) holding))]
    (cond [(empty? holding) empty]
          [(not (empty? (bombs holding)))
           (append (group-hand (new-hand (bombs holding) holding)) (bombs holding))]
          [(not (empty? (rockets holding)))
           (append (group-hand (new-hand (rockets holding) holding)) (list (list 'Black))
                   (list (list 'Red)) (rockets holding))]
          [(not (empty? (get-airplanes holding)))
           (append (group-hand (new-hand (get-airplanes holding) holding))
                   (get-airplanes holding))]
          [(not (empty? (get-straight-pairs holding)))
           (append (group-hand (new-hand (get-straight-pairs holding) holding))
                   (get-straight-pairs holding))]
          [(not (empty? (trios holding)))
           (append (group-hand (new-hand (trios holding) holding))
                   (solos (filter (lambda (x) (card=? 2 x)) holding))
                   (pairs (filter (lambda (x) (card=? 2 x)) holding))
                   (trios holding))]
          [(not (empty? (get-straights holding)))
           (append (group-hand (new-hand (get-straights holding) holding))
                   (get-straights holding))]
          [(not (empty? (pairs holding)))
           (append (group-hand (new-hand (pairs holding) holding))
                   (solos (filter (lambda (x) (card=? 2 x)) holding)) (pairs holding))]
          [else (append (group-hand (new-hand (solos holding) holding)) (solos holding))])))

(define (lowest-valid hand options)
  (cond [(empty? hand) (first options)]
        [(empty? options) empty]
        [(beats? hand (first options)) (first options)]
        [else (lowest-valid hand (rest options))]))

(define (get-lowest-valid hand holding)
  (lowest-valid hand (group-hand holding)))

(define (solos-amount holding)
  (cond [(empty? holding) 0]
        [(= 1 (length (first holding))) (+ 1 (solos-amount (rest holding)))]
        [else (solos-amount (rest holding))]))

(define (pairs-amount holding)
  (cond [(empty? holding) 0]
        [(= 2 (length (first holding))) (+ 2 (pairs-amount (rest holding)))]
        [else (pairs-amount (rest holding))]))

(define (remaining-cards holding played)
  (local [(define full-deck (list 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9
                                  10 10 10 10 'Jack 'Jack 'Jack 'Jack 'Queen 'Queen 'Queen
                                  'Queen 'King 'King 'King 'King 'Ace 'Ace 'Ace 'Ace
                                  2 2 2 2 'Black 'Red))
          (define (remove-each deck lst)
            (cond [(empty? lst) deck]
                  [else (remove-each (remove-cards (first lst) deck) (rest lst))]))]
    (remove-each (remove-each full-deck played) (list holding))))

(define (student holding role played)
  (local [(define possible-play (get-lowest-valid (hand-to-beat played) holding))
          (define remaining (remaining-cards holding played))
          (define no-bombs (remove-cards (foldr append empty
                                                (filter (lambda (x) (bomb? x))
                                                        (group-hand holding))) holding))]
    (cond [(and (empty? (hand-to-beat played)) (not (symbol=? role 'Right)))
           (cond [(not (empty? (get-airplanes no-bombs)))
                  (first (get-airplanes no-bombs))]
                 [(not (empty? (get-straight-pairs no-bombs)))
                  (first (get-straight-pairs no-bombs))]
                 [(not (empty? (get-straights no-bombs))) (first (get-straights no-bombs))]
                 [(or (and (not (empty? (trios no-bombs)))
                           (not (empty? (rest (trios no-bombs)))))
                      (and (not (empty? (trios no-bombs)))
                           (<= (length no-bombs) 8))) (first (trios no-bombs))]
                 [(and (not (zero? (pairs-amount (group-hand holding))))
                       (not (zero? (solos-amount (group-hand holding))))
                       (>= (pairs-amount (group-hand holding))
                           (solos-amount (group-hand holding)))
                       (>= (card-value (first (get-lowest-valid (list 0) holding)))
                           (card-value (first (get-lowest-valid (list 0 0) holding)))))
                  (get-lowest-valid (list 0 0) holding)]
                 [else possible-play])]
          [(and (empty? possible-play) (symbol=? role 'Left)
                (not (empty? (follow (hand-to-beat played) no-bombs))))
           (first (follow (hand-to-beat played) no-bombs))]
          [(and (= 1 (length possible-play)) (card=? 2 (first possible-play)))
           (cond [(and (or (member? 'Black remaining) (member? 'Red remaining))
                       (> (length (group-hand holding)) 8)) empty]
                 [else possible-play])]
          [(and (= 1 (length possible-play)) (card=? 'Black (first possible-play)))
           (cond [(and (member? 'Red remaining) (> (length (group-hand holding)) 8)) empty]
                 [else possible-play])]
          [(and (= 1 (length possible-play)) (card=? 'Red (first possible-play)))
           (cond [(> (length (group-hand holding)) 8) empty]
                 [else possible-play])]
          [else possible-play])))