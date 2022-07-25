;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |HW #7 3:10|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Homework 7

;;; Exercise 1

(define-struct user [username friends rating])
(define-struct proctor [username friends community rank])
; A SiteMember is one of:
; - (make-user String [List-of String] NatNum)
; - (make-proctor String [List-of String] String NatNum)
; and represents either:
; - a regular user's login username, friends' usernames, and community rating
; - a site supervisor's login user name, friends' usernames, community they manage,
;   and their supervisory rank
(define SM-USER-1
  (make-user "fundies1"
             (list "jo6n")
             2))
(define SM-USER-2
  (make-user "jo6n"
             (list "fundies1" "adam12")
             4))
(define SM-PROCTOR-1
  (make-proctor "agent86"
                (list "adam12" "agent99")
                "Control Agents"
                99))
(define SM-PROCTOR-2
  (make-proctor "adam12"
                (list "agent86" "fundies1")
                "CHIPs"
                12))
; sitemember-temp : SiteMember -> ?
#;(define (sitemember-temp sm)
    (... (cond [(user? sm)    (... (user-username sm)
                                   (los-temp (user-friends sm))
                                   (user-rating sm) ...)]
               [(proctor? sm) (... (proctor-username sm)
                                   (los-temp (proctor-friends sm))
                                   (proctor-community sm)
                                   (proctor-rank sm) ...)])))

;; [A List-of SiteMember] is one of:
;; - '()
;; - (cons X [List-of SiteMember])
;; represents a list of site members
#;(define (losm-temp losm)
    (cond [(empty? losm) ...]
          [(cons? losm) ... (sitemember-temp (first losm)) ...
                        ... (losm-temp (rest losm)) ...]))
(define LoSM1 (list SM-USER-1 SM-USER-2 SM-PROCTOR-1 SM-PROCTOR-2))
(define LoSM2 (list SM-USER-1 SM-PROCTOR-2))

;; only-proctors : [List-of SiteMember] -> [List-of SiteMember]
;; takes a list of site members and produces a list of only the proctors
(check-expect (only-proctors LoSM1)
              (list SM-PROCTOR-1 SM-PROCTOR-2))
(check-expect (only-proctors LoSM2)
              (list SM-PROCTOR-2))
(check-expect (only-proctors '()) '())
(define (only-proctors losm)
  (filter proctor? losm))

;;; Exercise 2

;; all-friends : [List-of SiteMember] -> [List-of String]
;; returns a single list of all of the friends mentioned by anyone in the given list
(check-expect
 (all-friends LoSM1)
 (list "jo6n" "fundies1" "adam12" "adam12" "agent99" "agent86" "fundies1"))
(check-expect
 (all-friends LoSM2)
 (list "jo6n" "agent86" "fundies1"))
(check-expect (all-friends '()) '())
(define (all-friends losm)
  (foldr append '() (map list-friends losm)))

;; list-friends : SiteMember -> [List-of String]
;; takes a site member and produces all of their friends
(check-expect (list-friends SM-USER-1) (list "jo6n"))
(check-expect (list-friends '()) '())
(check-expect (list-friends SM-PROCTOR-2) (list "agent86" "fundies1"))
(define (list-friends sm)
  (cond
    [(user? sm) (user-friends sm)]
    [(proctor? sm) (proctor-friends sm)]
    [(empty? sm) '()]))

;;; Exercise 4

;; popular? : [List-of SiteMember] -> Boolean
;; determines if at least one person in a site member list has more than one friend
(check-expect (popular? LoSM1) #t)
(check-expect (popular? (list SM-USER-1)) #f)
(check-expect (popular? '()) #f)
(define (popular? losm)
  (ormap over-one-friend? losm))

;; over-one-friend? : SiteMember -> Boolean
;; determines if a site member has more than one friend
(check-expect (over-one-friend? SM-USER-1) #f)
(check-expect (over-one-friend? SM-USER-2) #t)
(check-expect (over-one-friend? SM-PROCTOR-2) #t)
(check-expect (over-one-friend? '()) #f)
(define (over-one-friend? sm)
  (cond [(user? sm) (> (length (user-friends sm)) 1)]
        [(proctor? sm) (> (length (proctor-friends sm)) 1)]
        [(empty? sm) #f]))


;;; Exercise 5

;; most-popular : [List-of SiteMember] -> PosInt
;; computes for a list of site members the number of friends 
;; the member with the most friends has
(check-expect (most-popular LoSM1) 2)
(check-expect (most-popular LoSM2) 2)
(check-expect (most-popular (list SM-USER-1)) 1)
(check-expect (most-popular '()) 0)
(define (most-popular losm)
  (foldr max 0 (map length (list-all-friends losm))))

;; list-all-friends: [List-of SiteMember] -> [List-of String]
;; for a list of site members, produces a list lists of all of their friends
(check-expect (list-all-friends '()) '())
(check-expect
 (list-all-friends LoSM1)
 (list (list "jo6n") (list "fundies1" "adam12") (list "adam12" "agent99") (list "agent86" "fundies1")))
(check-expect (list-all-friends LoSM2) (list (list "jo6n") (list "agent86" "fundies1")))
(define (list-all-friends losm) 
  (cond [(empty? losm) '()]
        [(cons? losm) (cons (list-friends (first losm)) 
                            (list-all-friends (rest losm)))]))


;;; Exercise 6

;; highest-rating : [List-of SiteMember] -> Number
;; when given a list of site members, returns the rating of the user who has the highest rating
;; or 0 if there are no users in the given list
(check-expect (highest-rating (list SM-USER-1)) 2)
(check-expect (highest-rating (list SM-USER-1 SM-USER-2)) 4)
(check-expect (highest-rating LoSM1) 4)
(check-expect (highest-rating '()) 0)
(define (highest-rating losm)
  (foldr max 0 (list-all-ratings losm)))

;; list-all-ratings : [List-of SiteMember] -> [List-of Number]
;; for a list of site members, produces a list containing all of their user ratings,
;; or '() if they're a proctor
(check-expect (list-all-ratings LoSM1) (list 2 4 0 0))
(check-expect (list-all-ratings '()) '())
(check-expect (list-all-ratings (list SM-USER-1)) (list 2))
(define (list-all-ratings losm) 
  (cond [(empty? losm) '()]
        [(cons? losm) (cons (give-rating (first losm)) 
                            (list-all-ratings (rest losm)))]))

;; give-rating : SiteMember -> Number
;; takes a site member and produces their rating, or '() if they're a proctor
(check-expect (give-rating SM-USER-1) 2)
(check-expect (give-rating '()) '())
(check-expect (give-rating SM-PROCTOR-2) 0)
(define (give-rating sm)
  (cond
    [(user? sm) (user-rating sm)]
    [(proctor? sm) 0]
    [(empty? sm) '()]))












































;;; Exercise 7

;; Constants:
(define FROG-WIDTH 15)
(define FROG (ellipse FROG-WIDTH 10 "solid" "green"))
(define HEIGHT 280)
(define ROW-HEIGHT (/ 280 7))
(define WIDTH 300)
(define BUS-WIDTH 20)
(define BUS (rectangle BUS-WIDTH 15 "solid" "red"))
(define BACKGROUND (place-image (rectangle WIDTH ROW-HEIGHT "solid" "purple") 150 (/ ROW-HEIGHT 2)
                                (place-image (rectangle WIDTH ROW-HEIGHT "solid" "purple") 150 260
                                             (rectangle WIDTH HEIGHT "solid" "black"))))

(define WIN-SCREEN (text "YOU WON!" 50 "green"))            
(define LOSE-SCREEN (text "YOU LOST" 50 "white"))
(define MOVE-SPEED 1)


;;; Data Definitions:

;; A Frog is a (make-posn RealNum RealNum)
;; A posn with the x and y coordinates of the frog
(define F1 (make-posn (/ WIDTH 2) (- HEIGHT (/ ROW-HEIGHT 2))))
(define F2 (make-posn (/ WIDTH 2) (- HEIGHT (- 300 ROW-HEIGHT))))
(define F3 (make-posn 40 220))
#;(define (frog-temp f)
    (... (posn-x f)... (posn-y f)))

(define-struct bus [x y facingleft?])
;; A Bus is a (make-bus RealNum RealNum Boolean)
;; A Bus has an x coordinate, y coordinate, and direction, represented by
;; a real number, a real number, and a boolean
; there are 5 traffic rows with 4 busses each, these examples are sorted
; from left to right, bottom to top

(define B1 (make-bus (- WIDTH 260) (- HEIGHT (* 1.5 ROW-HEIGHT)) #true))
(define B2 (make-bus (- WIDTH 185) (- HEIGHT (* 1.5 ROW-HEIGHT)) #true))
(define B3 (make-bus (- WIDTH 110) (- HEIGHT (* 1.5 ROW-HEIGHT)) #true))
(define B4 (make-bus (- WIDTH 35) (- HEIGHT (* 1.5 ROW-HEIGHT)) #true))

(define B5 (make-bus (- WIDTH 260) (- HEIGHT (* 2.5 ROW-HEIGHT)) #false))
(define B6 (make-bus (- WIDTH 185) (- HEIGHT (* 2.5 ROW-HEIGHT)) #false))
(define B7 (make-bus (- WIDTH 110) (- HEIGHT (* 2.5 ROW-HEIGHT)) #false))
(define B8 (make-bus (- WIDTH 35) (- HEIGHT (* 2.5 ROW-HEIGHT)) #false))

(define B9 (make-bus (- WIDTH 260) (- HEIGHT (* 3.5 ROW-HEIGHT)) #true))
(define B10 (make-bus (- WIDTH 185) (- HEIGHT (* 3.5 ROW-HEIGHT)) #true))
(define B11 (make-bus (- WIDTH 110) (- HEIGHT (* 3.5 ROW-HEIGHT)) #true))
(define B12 (make-bus (- WIDTH 35) (- HEIGHT (* 3.5 ROW-HEIGHT)) #true))

(define B13 (make-bus (- WIDTH 260) (- HEIGHT (* 4.5 ROW-HEIGHT)) #false))
(define B14 (make-bus (- WIDTH 185) (- HEIGHT (* 4.5 ROW-HEIGHT)) #false))
(define B15 (make-bus (- WIDTH 110) (- HEIGHT (* 4.5 ROW-HEIGHT)) #false))
(define B16 (make-bus (- WIDTH 35) (- HEIGHT (* 4.5 ROW-HEIGHT)) #false))

(define B17 (make-bus (- WIDTH 260) (- HEIGHT (* 5.5 ROW-HEIGHT)) #true))
(define B18 (make-bus (- WIDTH 185) (- HEIGHT (* 5.5 ROW-HEIGHT)) #true))
(define B19 (make-bus (- WIDTH 110) (- HEIGHT (* 5.5 ROW-HEIGHT)) #true))
(define B20 (make-bus (- WIDTH 35) (- HEIGHT (* 5.5 ROW-HEIGHT)) #true))

#;(define (bus-temp b)
    (... (bus-x b) ... (bus-y b) ... (facingleft? b)))


;; A NELOB (non-empty list of buses) is one of:
;; - (list bus)
;; - (list bus NELOB)
;; represents a non-empty list of buses, it must contain at least one bus
(define NEL-ALL
  (list B20 B19 B18 B17 B16 B15 B14 B13 B12 B11 B10 B9 B8 B7 B6 B5 B4 B3 B2 B1))
(define NEL-TEST (list B5 B4 B3 B2 B1)) 
#;(define (nelob-temp list)
    (cond [(empty? (rest list)) ... (bus-temp (first list)) ...]
          [(cons? (rest list)) ... (bus-temp (first list)) ...
                               ... (nelob-temp (rest list)) ...]))


(define-struct gamestate [frog buses])
;; A GameState is a (make-struct Frog NELOC) 
;; represents the positions of the frog and all the buses
(define GS1 (make-gamestate F1 NEL-ALL))
(define GS2 (make-gamestate F2 NEL-ALL))
(define GS3 (make-gamestate F1 NEL-TEST))
(define GS4 (make-gamestate F2 NEL-TEST))
(define GS5 (make-gamestate F3 NEL-ALL))
#;(define (gamestate-temp gs)
    (... (frog-temp (gamestate-frog gs)) ... (nelob-temp (gamestate-buses gs)...)))


;;; Functions

;; frogger: GameState -> GameState
;; launches the frogger game
(define (frogger gs)
  (big-bang gs
    [to-draw draw-world]
    [on-tick move-buses]
    [on-key move-frog]
    [stop-when game-end? final-screen]
    ))


;; draw-world : GameState -> Image
;; places the frog and buses at the starting row
(check-expect (draw-world GS3)
              (place-image FROG (posn-x F1) (posn-y F1)
                           (place-image
                            BUS (bus-x B5) (bus-y B5)
                            (place-image
                             BUS (bus-x B4) (bus-y B4)
                             (place-image
                              BUS (bus-x B3) (bus-y B3)
                              (place-image
                               BUS (bus-x B2) (bus-y B2)
                               (place-image
                                BUS (bus-x B1) (bus-y B1) BACKGROUND)))))))
(check-expect (draw-world GS4)
              (place-image FROG (posn-x F2) (posn-y F2)
                           (place-image
                            BUS (bus-x B5) (bus-y B5)
                            (place-image
                             BUS (bus-x B4) (bus-y B4)
                             (place-image
                              BUS (bus-x B3) (bus-y B3)
                              (place-image
                               BUS (bus-x B2) (bus-y B2)
                               (place-image
                                BUS (bus-x B1) (bus-y B1) BACKGROUND)))))))              

(define (draw-world gs)
  (place-image FROG (posn-x (gamestate-frog gs)) (posn-y (gamestate-frog gs))
               (draw-buses (gamestate-buses gs))))

; draw-buses : Buses -> Image
; draws the buses onto the image of the frog and background
(check-expect (draw-buses NEL-TEST)
              (place-image
               BUS (bus-x B5) (bus-y B5)
               (place-image
                BUS (bus-x B4) (bus-y B4)
                (place-image
                 BUS (bus-x B3) (bus-y B3)
                 (place-image
                  BUS (bus-x B2) (bus-y B2)
                  (place-image
                   BUS (bus-x B1) (bus-y B1)
                   BACKGROUND))))))
(check-expect (draw-buses (list B1)) (place-image BUS (bus-x B1) (bus-y B1) BACKGROUND))
(check-expect (draw-buses (list B1 B2 B3))
              (place-image BUS (bus-x B1) (bus-y B1)
                           (place-image BUS (bus-x B2) (bus-y B2)
                                        (place-image BUS (bus-x B3) (bus-y B3) BACKGROUND))))
(define (draw-buses alob)
  (cond [(empty? (rest alob)) (draw-bus (first alob) BACKGROUND)]
        [(cons? (rest alob)) (draw-bus (first alob)
                                       (draw-buses (rest alob)))]))

; draw-bus : Bus Image -> Image
; draws one bus onto the background scene 
(check-expect (draw-bus B1 BACKGROUND) (place-image BUS (bus-x B1) (bus-y B1) BACKGROUND))
(check-expect (draw-bus B2 BACKGROUND) (place-image BUS (bus-x B2) (bus-y B2) BACKGROUND))
(check-expect (draw-bus B3 BACKGROUND) (place-image BUS (bus-x B3) (bus-y B3) BACKGROUND))
(define (draw-bus b back)
  (place-image BUS (bus-x b) (bus-y b) back))

; move-buses : GameState -> GameState
; moves all of the buses at once, based on which direction they are facing
(check-expect (move-buses GS3)
              (make-gamestate
               (make-posn 150 260)
               (list
                (make-bus 41 180 #false)
                (make-bus 264 220 #true)
                (make-bus 189 220 #true)
                (make-bus 114 220 #true)
                (make-bus 39 220 #true))))
(check-expect (move-buses GS4)
              (make-gamestate
               (make-posn 150 20)
               (list (make-bus 41 180 #false)
                     (make-bus 264 220 #true)
                     (make-bus 189 220 #true)
                     (make-bus 114 220 #true)
                     (make-bus 39 220 #true))))
(define (move-buses gs)
  (make-gamestate (gamestate-frog gs) (map move-bus (gamestate-buses gs))))



; use local for this? to cut out repetition
; move-bus : Bus -> Bus
; moves one bus, based on which direction it faces
(check-expect (move-bus B1) (make-bus (- (bus-x B1) MOVE-SPEED) 220 #t))
(check-expect (move-bus B13) (make-bus (+ (bus-x B13) MOVE-SPEED) 100 #f))
(check-expect (move-bus (make-bus 0 100 #true)) (make-bus -1 100 #true))
(check-expect (move-bus (make-bus -1 100 #true)) (make-bus 299 100 #true))
(check-expect (move-bus (make-bus 301 100 #false)) (make-bus 1 100 #false))
(define (move-bus b)
  (cond
    [(and (bus-facingleft? b) (<= 0 (bus-x b) WIDTH))
     (make-bus (- (bus-x b) MOVE-SPEED) (bus-y b) (bus-facingleft? b))]
    [(and (bus-facingleft? b) (<= (bus-x b) 0))
     (make-bus (modulo -1 WIDTH) (bus-y b) (bus-facingleft? b))]
    [(and (not (bus-facingleft? b)) (<= 0 (bus-x b) WIDTH))
     (make-bus (+ (bus-x b) MOVE-SPEED) (bus-y b) (bus-facingleft? b))]
    [(and (not (bus-facingleft? b)) (>= (bus-x b) WIDTH))
     (make-bus (modulo (bus-x b) WIDTH) (bus-y b) (bus-facingleft? b))]))


; game-end? : GameState -> Boolean
; stops the GameState when the frog reaches the y-position of the purple border
(check-expect (game-end? GS1) #false)
(check-expect (game-end? GS2) #true)
(check-expect (game-end? GS5) #true)
(define (game-end? gs)
  (or (< (posn-y (gamestate-frog gs)) 40) (collided? (gamestate-buses gs) (gamestate-frog gs))))

; collided? : [List-of Bus] Posn -> Boolean
; determines if the frog has collided with a bus
(check-expect (collided? NEL-ALL (make-posn 40 220)) #true)
(check-expect (collided? NEL-TEST F1) #false)
(define (collided? lob frog)
  (local [;; touching-frog : Bus -> Boolean
          ;; determines if a bus is touching the frog
          ; given B1, frog = (make-posn 40 220), expects #t
          ; given B1, frog = (make-posn 100 220), expects #f
          (define (touching-frog bus)
            (<= (sqrt (+ (sqr (- (bus-x bus) (posn-x frog)))
                         (sqr (- (bus-y bus) (posn-y frog))))) (+ (/ FROG-WIDTH 2) (/ BUS-WIDTH 2))))]
    (ormap touching-frog lob)))


; won? : GameState -> Boolean
; did the frog reach the top of the screen?
(check-expect (won? GS2) #t)
(check-expect (won? GS1) #f)
(define (won? gs)
  (< (posn-y (gamestate-frog gs)) 40))

; final-screen : GameState -> Image
; displays the final screen of the frogger game
(check-expect (final-screen GS5) (overlay LOSE-SCREEN (draw-world GS5)))
(check-expect (final-screen GS3) (place-image FROG (posn-x F1) (posn-y F1)
                                              (place-image
                                               BUS (bus-x B5) (bus-y B5)
                                               (place-image
                                                BUS (bus-x B4) (bus-y B4)
                                                (place-image
                                                 BUS (bus-x B3) (bus-y B3)
                                                 (place-image
                                                  BUS (bus-x B2) (bus-y B2)
                                                  (place-image
                                                   BUS (bus-x B1) (bus-y B1) BACKGROUND)))))))
(check-expect (final-screen GS2) (overlay WIN-SCREEN (draw-world GS2)))

(define (final-screen gs)
  (cond
    [(collided? (gamestate-buses gs) (gamestate-frog gs)) (overlay LOSE-SCREEN (draw-world gs))]
    [(won? gs) (overlay WIN-SCREEN (draw-world gs))]
    [else (draw-world gs)]))

; frogger-keys : GameState KeyEvent-> GameState
; moves the frog up, down, left, or right, based on an arrow key press
(check-expect (move-frog GS1 "up") (make-gamestate (make-posn 150 220) (gamestate-buses GS1)))
(check-expect (move-frog GS1 "down") (make-gamestate (make-posn 150 300) (gamestate-buses GS1)))
(check-expect (move-frog GS1 "left") (make-gamestate (make-posn 130 260) (gamestate-buses GS1)))
(check-expect (move-frog GS1 "right") (make-gamestate (make-posn 170 260) (gamestate-buses GS1)))
(check-expect (move-frog GS1 "q") GS1)
(define (move-frog gs ke)
  (cond
    [(key=? ke "up")
     (make-gamestate (make-posn (posn-x (gamestate-frog gs))
                                (- (posn-y (gamestate-frog gs)) ROW-HEIGHT))
                     (gamestate-buses gs))]
    [(key=? ke "down")
     (make-gamestate (make-posn (posn-x (gamestate-frog gs))
                                (+ (posn-y (gamestate-frog gs)) ROW-HEIGHT))
                     (gamestate-buses gs))]
    [(key=? ke "right")
     (make-gamestate (make-posn (+ (posn-x (gamestate-frog gs)) (/ ROW-HEIGHT 2))
                                (posn-y (gamestate-frog gs)))
                     (gamestate-buses gs))]
    [(key=? ke "left")
     (make-gamestate (make-posn (- (posn-x (gamestate-frog gs)) (/ ROW-HEIGHT 2))
                                (posn-y (gamestate-frog gs)))
                     (gamestate-buses gs))]
    [else gs]))
