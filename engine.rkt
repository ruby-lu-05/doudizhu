;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname engine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; *****************************************************************************************
;; Ruby Lu (21063222)
;; CS 135 Fall 2023
;; Assignment 10, Question 3
;; *****************************************************************************************
;;

(require "players.rkt")

;; sample doudizhu hands
(define hand0 '(3 3 3 3 4 5 6 7 7 7 9 9 Jack Jack Queen King 2 2 Black Red))
(define hand1 '(4 4 4 5 5 6 6 7 8 9 10 Jack Queen King Ace 2 2))
(define hand2 '(5 6 8 8 8 9 10 10 10 Jack Queen Queen King King Ace Ace Ace))
(define hand3 (list 3 3 4 5 5 6 7 8 9 10 'Jack 'Jack 'Queen 'King 'King 'Ace 'Ace 2 2 'Black))
(define hand4 (list 4 6 6 7 7 8 9 9 10 'Jack 'Queen 'Queen 'Ace 'Ace 2 2 'Red))
(define hand5 (list 3 3 4 4 5 5 6 7 8 8 9 10 10 'Jack 'Queen 'King 'King))


;; (doudizhu players hands) simulates a game of doudizhu and produces the winning player

;; Examples:
(check-expect (doudizhu (list goldfish goldfish goldfish) (list hand0 hand1 hand2)) 'Left)
(check-expect (doudizhu (list reckless goldfish goldfish) (list hand0 hand1 hand2)) 'Landlord)
(check-expect (doudizhu (list cautious reckless goldfish) (list hand0 hand1 hand2)) 'Landlord)

;; doudizhu: (listof Player) (listof Hand) -> Sym
(define (doudizhu players hands)
  (local [;; (card=? card1 card2) determines whether card1 and card2 are the same
          ;; card=?: Card Card -> Bool
          (define (card=? card1 card2)
            (or (and (symbol? card1) (symbol? card2) (symbol=? card1 card2))
                (and (number? card1) (number? card2) (= card1 card2))))

          ;; (play player hand role played) outputs one player's action
          ;; play: Player Hand Role -> Hand
          (define (play player hand role played)
            (local [(define (remove-hand hand lst)
                      (cond [(empty? hand) lst]
                            [(card=? (first hand) (first lst))
                             (remove-hand (rest hand) (rest lst))]
                            [else (cons (first lst) (remove-hand hand (rest lst)))]))]
              (list (remove-hand (player hand role played) hand)
                    (cons (player hand role played) played))))

          ;; (landlord-turn players hands played) determines the landlord's next move
          ;; (when it's their turn)
          ;; landlord-turn: (listof Player) (listof Hand) (listof Hand) -> Hand
          (define (landlord-turn players hands played)
            (cond [(empty? (first hands)) (doudizhu players hands)]
                  [else (right-turn players
                                    (list (first (play (first players) (first hands)
                                                       'Landlord played))
                                          (second hands) (third hands))
                                    (second (play (first players) (first hands)
                                                  'Landlord played)))]))

          ;; (right-turn players) determines the right peasant's next move (when it's their turn)
          ;; right-turn: (listof Player) (listof Hand) (listof Hand) -> Hand
          (define (right-turn players hands played)
            (cond [(empty? (second hands)) (doudizhu players hands)]
                  [else (left-turn players
                                   (list (first hands)
                                         (first (play (second players) (second hands)
                                                      'Right played))
                                         (third hands))
                                   (second (play (second players) (second hands)
                                                 'Right played)))]))

          ;; (left-turn players) determines the left peasant's next move (when it's their turn)
          ;; left-turn: (listof Player) (listof Hand) (listof Hand) -> Hand
          (define (left-turn players hands played)
            (cond [(empty? (third hands)) (doudizhu players hands)]
                  [else (landlord-turn players
                                       (list (first hands) (second hands)
                                             (first (play (third players)
                                                          (third hands) 'Left played)))
                                       (second (play (third players) (third hands)
                                                     'Left played)))]))]
    
    (cond [(empty? (first hands)) 'Landlord]
          [(empty? (second hands)) 'Right]
          [(empty? (third hands)) 'Left]
          [else (landlord-turn players hands empty)])))

;; Tests:
(check-expect (doudizhu (list reckless cautious cautious) (list hand3 hand4 hand5)) 'Right)
(check-expect (doudizhu (list cautious reckless reckless) (list hand2 hand4 hand5)) 'Landlord)
(check-expect (doudizhu (list cautious reckless cautious) (list hand2 hand4 hand5)) 'Landlord)