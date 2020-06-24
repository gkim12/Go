#lang typed/racket

(require typed/test-engine/racket-tests)
(require (only-in typed/racket/gui/base put-file get-file))

(require "./include/cs151-core.rkt")
(require "./include/cs151-image.rkt")
(require "./include/cs151-universe.rkt")
(require typed/lang/posn)

; Structure definitions

(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-type Stone
  (U 'black 'white))

(define-struct LogicalLoc
  ([col : Integer]
   [row : Integer]))

(define-type Board
  (Vectorof (Vectorof (Optional Stone))))

(define-struct Go
  ([board : Board]
   [next-to-play : Stone]
   [history : (Listof Board)]
   [last-turn-place : (Optional LogicalLoc)]
   [last-turn-opp-captures : (Listof LogicalLoc)]
   [last-turn-self-captures : (Listof LogicalLoc)]
   [consecutive-passes : Integer]))

(define-struct PhysicalLoc
  ([x-offset-from-left : Integer]
   [y-offset-from-top  : Integer]))

(define-struct BoardSpec
  ([background-color : Image-Color]
   [cell-size-pixels : Integer]
   [margin-pixels : Integer]
   [stone-radius-pixels : Integer]))

(define-struct World
  ([spec : BoardSpec]
   [game : Go]
   [status-message : String]
   [black-tenths : Integer]
   [white-tenths : Integer]
   [hover : (Optional LogicalLoc)]))

(define-struct Outcome
  ([black  : Integer]
   [white  : Integer]
   [winner : (U Stone 'draw)]))
;-------------------------------------------------------------------------------
;; distance: returns the distance between two PhysicalLocs
(: distance : PhysicalLoc PhysicalLoc -> Integer)
(define (distance p1 p2)
  (match* (p1 p2)
    [((PhysicalLoc x1 y1) (PhysicalLoc x2 y2))
     (integer-sqrt (+ (sqr (- x1 x2))
                      (sqr (- y1 y2))))]))

;; logical->physical: converts a LogicalLoc to its corresponding PhysicalLoc.
(: logical->physical : LogicalLoc Integer BoardSpec -> PhysicalLoc)
(define (logical->physical loc i board)
  (match* (loc board)
    [((LogicalLoc col row) (BoardSpec _ cell-pixels margin-pixels _))
     (PhysicalLoc (+ (* col cell-pixels) margin-pixels)
                  (+ (* (- i row 1) cell-pixels) margin-pixels))]))

;; all-loglocs: returns all LogicalLoc for given row and column values
(: all-loglocs : Integer Integer -> (Listof LogicalLoc))
(define (all-loglocs r c)
  (if (zero? r) '()
      (local
        {(define gen-col : (Integer Integer -> (Listof LogicalLoc))
           (lambda ([x : Integer] [y : Integer])
             (if (zero? y) '()
                 (cons (LogicalLoc (sub1 x) (sub1 y))
                       (gen-col x (sub1 y))))))}
        (append (gen-col r c) (all-loglocs (sub1 r) c)))))

;; logicalloc=?
(: logicalloc=? : LogicalLoc LogicalLoc -> Boolean)
(define (logicalloc=? l1 l2)
  (match* (l1 l2)
    [((LogicalLoc c1 r1) (LogicalLoc c2 r2))
     (and (= c1 c2) (= r1 r2))]))

;; physical->logical: returns either 'None if there are no LogicalLocs within a
;; stone's radius of a PhysicalLoc or the closest LogicalLoc to a PhysicalLoc.
(: physical->logical : PhysicalLoc Integer BoardSpec -> (Optional LogicalLoc))
(define (physical->logical p i board)
  (local
    {(: in-range : PhysicalLoc Integer BoardSpec -> (Listof LogicalLoc))
     (define (in-range p i board)
       (match board
         [(BoardSpec _ _ _ srad)
          (filter (lambda ([l : LogicalLoc])
                    (<= (distance (logical->physical l i board) p) srad))
                  (all-loglocs i i))]))
     (define ls : (Listof LogicalLoc) (in-range p i board))}
    (match ls
      ['() 'None]
      [_ (Some (first ls))])))

;; alphabet: the English alphabet without 'I'
(define alphabet (list #\A #\B #\C #\D #\E #\F #\G #\H #\J #\K #\L #\M #\N
                            #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

;; logical->string: convert logical locations to strings.
(: logical->string : LogicalLoc -> String)
(define (logical->string loc)
  (match loc
    [(LogicalLoc col row)
     (local
       {(define lenstr : Integer (add1 (quotient col 25)))
        (define letter : Char (list-ref alphabet (remainder col 25)))}
       (string-append (make-string lenstr letter)
                      (number->string (add1 row))))]))

;; board-ref: return the stone at the specified location on the board, or
;; indicate it is unoccupied.
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref go loc)
  (match loc
    [(LogicalLoc col row) (vector-ref (vector-ref (Go-board go) col) row)]))

;; bd-ref: same thing as board-ref except taking in a board
(: bd-ref : Board LogicalLoc -> (Optional Stone))
(define (bd-ref b loc)
  (match loc
    [(LogicalLoc col row) (vector-ref (vector-ref b col) row)]))

;; board-set!: Find corresponding spot on Go board to given LogicalLoc
;; and set that spot with given Optional Stone
(: board-set! : Go LogicalLoc (Optional Stone) -> Void)
(define (board-set! go loc s)
  (match* (go loc)
    [((Go board _ _ _ _ _ _) (LogicalLoc col row))
     (vector-set! (vector-ref board col) row s)]))

;; bd-set!: same thing as board-set! except taking in a board
(: bd-set! : Board LogicalLoc (Optional Stone) -> Void)
(define (bd-set! b loc s)
  (match loc
    [(LogicalLoc col row)
     (vector-set! (vector-ref b col) row s)]))

;; bd-set!-tester: method to test bd-set!
(: bd-set!-tester : Board LogicalLoc (Optional Stone) -> Board)
(define (bd-set!-tester board loc s)
  (begin (bd-set! board loc s) board))

;; optional-stone=?: tests whether two Optional Stone are equal
(: optional-stone=? : (Optional Stone) (Optional Stone) -> Boolean)
(define (optional-stone=? o1 o2)
  (match* (o1 o2)
    [('None 'None) #t]
    [('None _) #f]
    [(_ 'None) #f]
    [((Some s1) (Some s2)) (symbol=? s1 s2)]))

;; board=?: tests whether two boards are equal
(: board=? : Board Board -> Boolean)
(define (board=? b1 b2)
  (if (= (vector-length b1) (vector-length b2))
      (local
        {;; locs: all LogicalLoc possible for given boards
         (define locs : (Listof LogicalLoc)
           (all-loglocs (vector-length b1) (vector-length b1)))

         ;; helper: helper method for board=?
         (: helper : (Listof LogicalLoc) -> Boolean)
         (define (helper ls)
           (match ls
             ['() #t]
             [(cons hd tl) (and (optional-stone=? (bd-ref b1 hd) (bd-ref b2 hd))
                                (helper tl))]))}
        (helper locs))
      #f))

;; board-copy: copies given board into a new board
(: board-copy : Board -> Board)
(define (board-copy b)
  (local
    {;; copy: the new board that will contain the copy
     (define copy : Board (make-board (vector-length b)))

     ;; locs: all possible LogicalLocs for given board
     (define locs : (Listof LogicalLoc)
       (all-loglocs (vector-length b) (vector-length b)))

     ;; helper: helper method for board-copy
     (: helper : (Listof LogicalLoc) -> Void)
     (define (helper ls)
       (match ls
         ['() (void)]
         [(cons (LogicalLoc col row) tl)
          (begin (vector-set! (vector-ref copy col) row
                              (vector-ref (vector-ref b col) row))
                 (helper tl))]))}
    (begin (helper locs) copy)))

;; make-board: makes an empty board of dimension given
(: make-board : Integer -> Board)
(define (make-board dim)
  (build-vector dim
                (lambda ([i : Integer])
                  (make-vector dim (cast 'None (Optional Stone))))))

;; swap: swaps color of stone
(: swap : Stone -> Stone)
(define (swap s)
  (match s
    ['black 'white]
    ['white 'black]))

;; valid-board-spec? : tests whether given BoardSpec is valid.
(: valid-board-spec? : BoardSpec -> Boolean)
(define (valid-board-spec? bs)
  (match bs
    [(BoardSpec _ csize msize srad)
     (and (positive? csize) (positive? msize) (positive? srad)
          (< srad (/ csize 2)))]))

;; legal-move?: determines whether given move is legal
(: legal-move? : Go LogicalLoc -> Boolean)
(define (legal-move? go loc)
  (match* (go loc)
    [((Go board plyr hist _ _ _ _) (LogicalLoc col row))
     (if (two-passes? go)
         #f
         (if (and (>= col 0)
                  (>= row 0)
                  (< col (vector-length board))
                  (< row (vector-length board)))
             (local
               {;; copy
                (define copy : Board (board-copy board))
                
                ;; next: what the board WOULD look like if the move was made
                (define next : Board
                  (begin (self-capture!-test
                          (capture!-test
                           (bd-set!-tester copy loc (Some plyr)) loc) loc)
                         copy))

                ;; not-in-history?
                (: not-in-history? : Board (Listof Board) -> Boolean)
                (define (not-in-history? bd bs)
                  (foldr (lambda ([b : Board] [res : Boolean])
                           (and (not (board=? bd b)) res))
                         #t bs))}
               (and (liberty? board loc) (not-in-history? next hist)))
             #f))]))

;; neighbors: returns adjacent LogicalLocs within bounds
(: neighbors : Board LogicalLoc -> (Listof LogicalLoc))
(define (neighbors b loc)
  (local
    {;; valid?: checks whether given LogicalLoc is in bounds of given board
     (: valid? : Board LogicalLoc -> Boolean)
     (define (valid? b l)
       (match l
         [(LogicalLoc c r)
          (and (>= c 0)
               (>= r 0)
               (< c (vector-length b))
               (< r (vector-length b)))]))}
  (match loc
    [(LogicalLoc col row)
     (filter (lambda ([l : LogicalLoc]) (valid? b l))
             (list (LogicalLoc (add1 col) row)
                   (LogicalLoc (sub1 col) row)
                   (LogicalLoc col (add1 row))
                   (LogicalLoc col (sub1 row))))])))

;; liberty?: checks whether intersection is empty at given LogicalLoc
(: liberty? : Board LogicalLoc -> Boolean)
(define (liberty? board l)
  (optional-stone=? 'None (bd-ref board l)))

;; libs: returns liberties from a given list of LogicalLoc
(: libs : Board (Listof LogicalLoc) -> (Listof LogicalLoc))
(define (libs b ls)
  (filter (lambda ([l : LogicalLoc]) (liberty? b l)) ls))

;; identify-chain: identifies a chain of stones that need to be captured
(: identify-chain : Board (Listof LogicalLoc) (Listof LogicalLoc) ->
   (Optional (Listof LogicalLoc)))
(define (identify-chain board to-explore marked)
  (match to-explore
    ['() (Some marked)]
    [(cons hd tl)
     (local
       {;; e: a LogicalLoc to be explored
        (define e : LogicalLoc hd)

        ;; liberties: adjacent liberties to e
        (define liberties : (Listof LogicalLoc)
          (libs board (neighbors board e)))}
       (match liberties
         ['()
          (local
            {;; same-neighbors: returns adjacent LogicalLocs which have the
             ;; same state as given LogicalLoc
             (: same-neighbors : Board LogicalLoc -> (Listof LogicalLoc))
             (define (same-neighbors b loc)
               (local
                 {;; same-color?: returns stones in list that have the same
                  ;; color as given stone
                  (: same-color? : Board (Listof LogicalLoc) (Optional Stone) ->
                     (Listof LogicalLoc))
                  (define (same-color? b to-test color)
                    (match color
                      ['None '()]
                      [(Some c)
                       (filter (lambda ([n : LogicalLoc])
                                 (optional-stone=? color (bd-ref b n)))
                               to-test)]))}
                 (same-color? b (neighbors b loc) (bd-ref b loc))))

             ;; unmarked: returns unmarked LogicalLocs from a given list
             (: unmarked : (Listof LogicalLoc) (Listof LogicalLoc) ->
                (Listof LogicalLoc))
             (define (unmarked to-test marked)
               (local
                 {;; marked?: checks whether given LogicalLoc is marked
                  (: marked? : LogicalLoc (Listof LogicalLoc) -> Boolean)
                  (define (marked? l marked)
                    (local
                      {(: helper : LogicalLoc (Listof LogicalLoc) -> Boolean)
                       (define (helper l ls)
                         (match ls
                           ['() #f]
                           [(cons hd tl)
                            (or (logicalloc=? l hd) (helper l tl))]))}
                      (helper l marked)))}
                 (filter (lambda ([n : LogicalLoc])
                           (not (marked? n marked))) to-test)))
             
             ;; m: unmarked neighbors of the same color
             (define m : (Listof LogicalLoc)
               (unmarked (same-neighbors board e) marked))}
            (identify-chain board (append m tl) (append m marked)))]
         [(cons hd tl) 'None]))]))

;; test-board: board used for testing
(define test-board : Board (vector (vector (Some 'black) (Some 'black))
                                   (vector (Some 'black) (Some 'white))))

;; test-board2: self-capture! board test
(define test-board2 : Board (vector (vector (Some 'black) (Some 'black))
                                    (vector (Some 'black) 'None)))

;; remove-stones!: removes stones if given (Some Listof LogicalLoc) or does
;; nothing if given 'None
(: remove-stones! : Board (Optional (Listof LogicalLoc)) -> Void)
(define (remove-stones! board to-remove)
  (local
    {;; remove-stone: remove stone at given LogicalLoc
     (: remove-stone : Board LogicalLoc -> Void)
     (define (remove-stone board loc)
       (bd-set! board loc 'None))}
    (match to-remove
      ['None (void)]
      [(Some '()) (void)]
      [(Some (cons hd tl))
       (begin (remove-stone board hd) (remove-stones! board (Some tl)))])))

;; remove-stones!-tester: tester for remove-stones
(: remove-stones!-tester : Board (Optional (Listof LogicalLoc)) -> Board)
(define (remove-stones!-tester board to-remove)
  (begin (remove-stones! board to-remove) board))

;; capture!: the capture mechanic
(: capture! : Board LogicalLoc -> Void)
(define (capture! board loc)
  (local
    {;; opp-neighbors: returns adjacent LogicalLocs which have the opposite
     ;; color from given LogicalLoc
     (: opp-neighbors : Board LogicalLoc -> (Listof LogicalLoc))
     (define (opp-neighbors b loc)
       (match (bd-ref b loc)
         ['None '()]
         [(Some c)
          (filter (lambda ([n : LogicalLoc])
                    (match (bd-ref b n)
                      ['None #f]
                      [(Some col) (not (symbol=? col c))]))
                  (neighbors b loc))]))

     ;; potential-captures: adjacent different colored LogicalLocs
     (define potential-captures : (Listof LogicalLoc)
       (opp-neighbors board loc))

     ;; to-capture: chains that need to be captured/removed
     (define to-capture : (Listof (Optional (Listof LogicalLoc)))
       (map (lambda ([l : LogicalLoc])
              (identify-chain board (list l) (list l))) potential-captures))

     ;; helper: removes given chains
     (: helper : (Listof (Optional (Listof LogicalLoc))) -> Void)
     (define (helper ls)
       (match ls
         ['() (void)]
         [(cons hd tl) (begin (remove-stones! board hd) (helper tl))]))}
    (helper to-capture)))

;; captured: what is captured
(: captured : Board LogicalLoc -> (Listof LogicalLoc))
(define (captured board loc)
  (local
    {;; opp-neighbors: returns adjacent LogicalLocs which have the opposite
     ;; color from given LogicalLoc
     (: opp-neighbors : Board LogicalLoc -> (Listof LogicalLoc))
     (define (opp-neighbors b loc)
       (match (bd-ref b loc)
         ['None '()]
         [(Some c)
          (filter (lambda ([n : LogicalLoc])
                    (match (bd-ref b n)
                      ['None #f]
                      [(Some col) (not (symbol=? col c))]))
                  (neighbors b loc))]))
     
     ;; potential-captures: adjacent different colored LogicalLocs
     (define potential-captures : (Listof LogicalLoc)
       (opp-neighbors board loc))

     ;; to-capture: chains that need to be captured/removed
     (define to-capture : (Listof (Optional (Listof LogicalLoc)))
       (map (lambda ([l : LogicalLoc])
              (identify-chain board (list l) (list l))) potential-captures))

     ;; helper
     (: helper : (Listof (Optional (Listof LogicalLoc))) (Listof LogicalLoc)
        -> (Listof LogicalLoc))
     (define (helper opts ls)
       (match opts
         ['() ls]
         [(cons 'None tl) (helper tl ls)]
         [(cons (Some l) tl) (helper tl (append l ls))]))}
    (helper to-capture '())))

;; capture!-test
(: capture!-test : Board LogicalLoc -> Board)
(define (capture!-test board loc)
  (begin (capture! board loc) board))

;; self-capture!: the self-capture mechanic
(: self-capture! : Board LogicalLoc -> Void)
(define (self-capture! board loc)
  (local
    {(define to-capture : (Optional (Listof LogicalLoc))
       (identify-chain board (list loc) (list loc)))}
    (remove-stones! board to-capture)))

;; self-captured: what is self-captured
(: self-captured : Board LogicalLoc -> (Listof LogicalLoc))
(define (self-captured board loc)
  (local
    {(define to-capture : (Optional (Listof LogicalLoc))
       (identify-chain board (list loc) (list loc)))}
    (match to-capture
      ['None '()]
      [(Some l) l])))

;; self-capture!-test
(: self-capture!-test : Board LogicalLoc -> Board)
(define (self-capture!-test board loc)
  (begin (self-capture! board loc) board))

;; apply-move: applies the given move then captures then self-captures
(: apply-move : Go LogicalLoc -> Go)
(define (apply-move go loc)
  (match go
    [(Go board plyr hist lt-place lt-opp lt-self consec)
     (local
       {;; copy: a copy of the board given
        (define copy : Board (board-copy board))

        ;; c: what pieces are captured with the given move
        (define c : (Listof LogicalLoc)
          (captured (bd-set!-tester (board-copy board) loc (Some plyr)) loc))

        ;; sc: what pieces are self-captured with the given move
        (define sc : (Listof LogicalLoc)
          (self-captured
           (capture!-test
            (bd-set!-tester (board-copy board) loc (Some plyr)) loc) loc))}
       (Go (begin (bd-set! board loc (Some plyr))
                  (capture! board loc)
                  (self-capture! board loc) board)
           (swap plyr) (cons copy hist) (Some loc)
           c sc 0))]))

;; two-passes?: returns whether two passes in a row have been made
(: two-passes? : Go -> Boolean)
(define (two-passes? go)
  (= (Go-consecutive-passes go) 2))

;; test-board3
(define test-board3 : Board (vector (vector (Some 'black) (Some 'white))
                                    (vector (Some 'black) 'None)))

;; identify-territory: identifies a territory of stones
(: identify-territory : Board (Listof LogicalLoc) (Listof LogicalLoc)
   (Listof LogicalLoc) -> (Optional (Listof LogicalLoc)))
(define (identify-territory board to-explore marked stones-touching)
  (match to-explore
    ['() (if (empty? stones-touching)
             (Some marked)
             (local
               {;; fstone: the first stone in stones-touching
                (define fstone : (Optional Stone)
                  (bd-ref board (first stones-touching)))

                ;; same-color?: returns whether stones touching the liberties
                ;; are all of the same color
                (define same-color? : Boolean
                  (foldr (lambda ([l : LogicalLoc] [b : Boolean])
                           (and (optional-stone=? (bd-ref board l) fstone) b))
                         #t stones-touching))}
               (if same-color?
                   (Some marked)
                   'None)))]
    [(cons hd tl)
     (local
       {;; e: a LogicalLoc to be explored
        (define e : LogicalLoc hd)

        ;; stones: adjacent stones to e
        (define stones : (Listof LogicalLoc)
          (filter (lambda ([l : LogicalLoc])
                    (not (optional-stone=? (bd-ref board l) 'None)))
                  (neighbors board e)))

        ;; liberties: adjacent liberties to e
        (define liberties : (Listof LogicalLoc)
          (libs board (neighbors board e)))
        
        ;; unmarked: returns unmarked LogicalLocs from a given list
        (: unmarked : (Listof LogicalLoc) (Listof LogicalLoc) ->
           (Listof LogicalLoc))
        (define (unmarked to-test marked)
          (local
            {;; marked?: checks whether given LogicalLoc is marked
             (: marked? : LogicalLoc (Listof LogicalLoc) -> Boolean)
             (define (marked? l marked)
               (local
                 {(: helper : LogicalLoc (Listof LogicalLoc) -> Boolean)
                  (define (helper l ls)
                    (match ls
                      ['() #f]
                      [(cons hd tl)
                       (or (logicalloc=? l hd) (helper l tl))]))}
                 (helper l marked)))}
            (filter (lambda ([n : LogicalLoc])
                      (not (marked? n marked))) to-test)))
             
        ;; m: unmarked neighbors which are liberties
        (define m : (Listof LogicalLoc)
          (unmarked liberties marked))}
       (identify-territory board (append m tl) (append m marked)
                           (append stones stones-touching)))]))

;; territory: returns the territory of a given player, no duplicates
(: territory : Board Stone -> (Listof LogicalLoc))
(define (territory board stn)
  (local
    {;; contains? returns whether given LogicalLoc is in given list
     (: contains? : LogicalLoc (Listof LogicalLoc) -> Boolean)
     (define (contains? x ls)
       (foldr (lambda ([l : LogicalLoc] [b : Boolean])
                (or (logicalloc=? x l) b))
              #f ls))

     ;; no-dupes: creates new list with no duplicates
     (: no-dupes : (Listof LogicalLoc) (Listof LogicalLoc)
        -> (Listof LogicalLoc))
     (define (no-dupes ls res)
       (match ls
         ['() res]
         [(cons hd tl) (if (contains? hd res)
                           (no-dupes tl res)
                           (no-dupes tl (cons hd res)))]))

     ;; stns: all stones of the given color
     (define stns : (Listof LogicalLoc)
       (filter (lambda ([l : LogicalLoc])
                 (optional-stone=? (bd-ref board l) (Some stn)))
               (all-loglocs (vector-length board) (vector-length board))))

     ;; liberties: all liberties touching any of the player's stones
     (define liberties : (Listof LogicalLoc)
       (no-dupes
        (foldr
         (lambda ([ls : (Listof LogicalLoc)] [res : (Listof LogicalLoc)])
           (append ls res))
         '()
         (map (lambda ([l : LogicalLoc])
                (libs board (neighbors board l))) stns))
        '()))

     ;; ts: the territories of the player, no dupes
     (define ts : (Listof LogicalLoc)
       (no-dupes
        (foldr
         (lambda ([ls : (Optional (Listof LogicalLoc))]
                  [res : (Listof LogicalLoc)])
           (match ls ['None res] [(Some x) (append x res)]))
         '()
         (map (lambda ([l : LogicalLoc])
                (identify-territory board (list l) (list l) '())) liberties))
        '()))}
    ts))

;; outcome: producing an Outcome from a given Go/Game
(: outcome : Go -> Outcome)
(define (outcome go)
  (match go
    [(Go board _ _ _ _ _ _)
     (local
       {;; outcome: all LogicalLoc in a game
        (define ls : (Listof LogicalLoc)
          (all-loglocs (vector-length board) (vector-length board)))

        ;; bstones: the number of black stones on the board
        (define bstones : Integer
          (foldr (lambda ([l : LogicalLoc] [i : Integer])
                   (if (optional-stone=? (bd-ref board l) (Some 'black))
                       (add1 i)
                       i))
                 0 ls))

        ;; wstones: the number of white stones on the board
        (define wstones : Integer
          (foldr (lambda ([l : LogicalLoc] [i : Integer])
                   (if (optional-stone=? (bd-ref board l) (Some 'white))
                       (add1 i)
                       i))
                 0 ls))

        ;; bscore
        (define bscore : Integer
          (+ bstones (length (territory board 'black))))

        ; wscore
        (define wscore : Integer
          (+ wstones (length (territory board 'white))))}
       (Outcome bscore
                wscore
                (cond
                  [(> bscore wscore) 'black]
                  [(< bscore wscore) 'white]
                  [else 'draw])))]))

;; img-stone: returns an image of a stone
(: img-stone : Integer Stone -> Image)
(define (img-stone r stone)
  (match stone
    ['black (circle r "solid" "black")]
    ['white (circle r "solid" "white")]))
 
;; place-stones: overlays black and white stones upon an image.
(: place-stones : Board BoardSpec Image -> Image)
(define (place-stones board bspec img)
  (local
    {;; locs: all LogicalLocs possible for a given board
     (define locs : (Listof LogicalLoc)
       (all-loglocs (vector-length board) (vector-length board)))

     ;; loglocs->imgs: calculates corresponding image for given loglocs
     (: loglocs->imgs : (Listof LogicalLoc) -> (Listof Image))
     (define (loglocs->imgs ls)
       (match ls
         ['() '()]
         [(cons hd tl)
          (local
            {;; stone: state of given position
             (define stone : (Optional Stone)
               (bd-ref board hd))}
            (match stone
              ['None (cons empty-image (loglocs->imgs tl))]
              [(Some 'black)
               (cons (img-stone (BoardSpec-stone-radius-pixels bspec) 'black)
                     (loglocs->imgs tl))]
              [(Some 'white)
               (cons (img-stone (BoardSpec-stone-radius-pixels bspec) 'white)
                     (loglocs->imgs tl))]))]))
     
     ;; imgs: all loglocs converted to images
     (define imgs : (Listof Image) (loglocs->imgs locs))

     ;; loglocs->posns: calculates corresponding posn for given loglocs
     (: loglocs->posns : (Listof LogicalLoc) -> (Listof Posn))
     (define (loglocs->posns ls)
       (local
         {;; phylocs: LogicalLocs converted to PhysicalLocs
          (define phylocs : (Listof PhysicalLoc)
            (map (lambda ([l : LogicalLoc])
                   (logical->physical l (vector-length board) bspec)) ls))}
         (match phylocs
           ['() '()]
           [(cons (PhysicalLoc x y) tl)
            (cons (make-posn x y) (loglocs->posns (rest ls)))])))

     ;; posns: LogicalLocs converted to Posns
     (define posns : (Listof Posn) (loglocs->posns locs))}
    (place-images imgs posns img)))

;; draw: draw an image of the world.
(: draw : World -> Image)
(define (draw w)
  ;; Start helper methods
  (local
    {;; img-cell: draw an image of a cell.
     (: img-cell : BoardSpec -> Image)
     (define (img-cell board)
       (match board
         [(BoardSpec bg-color csize _ _)
          (square csize "outline" "black")]))
     
     ;; img-grid: draw an image of the grid/board.
     (: img-grid : Integer Image -> Image)
     (define (img-grid n img)
       (local
         {(: img-col : Integer Image -> Image)
          (define (img-col n img)
            (if (zero? n)
                empty-image
                (beside img (img-col (sub1 n) img))))
          (: img-row : Integer Image -> Image)
          (define (img-row n img)
            (if (zero? n)
                empty-image
                (above img (img-row (sub1 n) img))))}
         (img-row n (img-col n img))))
     
     ;; label: takes in a string and returns a text image
     (: label : String -> Image)
     (define (label str)
       (text str 12 "black"))

     ;; column->string: convert the column index to a string label
     ;; 0 => "A", ..., 24 => "Z", 25 => "AA", ...
     ;; source: project1 reference implementation
     (: column->string : Integer -> String)
     (define (column->string n)
       (make-string (add1 (quotient n 25))
                    (list-ref alphabet (remainder n 25))))
     
     ;; draw column labels to go along the bottom of the board
     ;; source: project1 reference implementation
     (: col-labels : World -> Image)
     (define (col-labels w)
       (match w
         [(World (BoardSpec _ cell margin _) (Go board _ _ _ _ _ _) _ _ _ _)
          (foldr (lambda ([column-label : String] [img : Image])
                   (beside (overlay (text column-label 10 'black)
                                    (square cell 'outline 'white))
                           img))
                 empty-image
                 (build-list (vector-length board) column->string))]))

     ;; draw the row labels that go on the right edge of the board
     ;; 1 goes at the bottom, and numbers increase going up
     ;; source: project1 reference implementation
     (: row-labels : World -> Image)
     (define (row-labels w)
       (match w
         [(World (BoardSpec _ cell margin _) (Go board _ _ _ _ _ _) _ _ _ _)
          (above (square (max 0 (- margin (quotient cell 2))) 'solid 'white)
                 (foldr (lambda ([row-label : String] [img : Image])
                          (above (overlay (text row-label 11 'black)
                                          (square cell 'outline 'white))
                                 img))
                        empty-image
                        (build-list
                         (vector-length board)
                         (lambda ([i : Integer])
                           (number->string (- (vector-length board) i))))))]))

     ;; img-timer: image of a timer
     (: img-timer : Integer String -> Image)
     (define (img-timer i c)
       (local
         {;; tenths->string: converts time given in tenths of a second to a
          ;; string in minutes, seconds, tenths format (i.e. 1:02.3)
          (: tenths->string : Integer -> String)
          (define (tenths->string i)
            (local
              {;; minutes: number of minutes to display
               (: minutes : Integer -> String)
               (define (minutes i)
                 (number->string (quotient i 600)))

               ;; seconds: number of seconds to display
               (: seconds : Integer -> String)
               (define (seconds i)
                 (local
                   {(define secs : Integer (remainder (quotient i 10) 60))}
                   (if (< secs 10)
                       (string-append "0" (number->string secs))
                       (number->string secs))))

               ;; tenths: number of tenths to display
               (: tenths : Integer -> String)
               (define (tenths i)
                 (number->string (remainder i 10)))}
              (string-append (minutes i) ":" (seconds i) "." (tenths i))))

          ;; time: result of calling tenths->string with given time
          (define time : String (tenths->string i))}
         
         (overlay (text time 10 "black")
                  (rectangle 40 20 "solid" "white")
                  (match c
                    ["white" (circle 30 "outline" "black")]
                    ["black" (circle 30 "solid" "black")]
                    [c (circle 30 "solid" c)]))))
     
     ;; hover-stone: image of the ghost stone
     (: hover-stone : World Image -> Image)
     (define (hover-stone w img)
       (match w
         [(World (BoardSpec _ _ _ srad) (Go board plyr _ _ _ _ _) _ _ _ h)
          (local
            {;; phyloc: PhysicalLoc of the ghost stone
             (define phyloc : (Optional PhysicalLoc)
               (match h
                 ['None 'None]
                 [(Some l)
                  (Some (logical->physical l (vector-length board)
                                           (World-spec w)))]))}
            (match phyloc
              ['None img]
              [(Some (PhysicalLoc x y))
               (place-image (circle srad 128 plyr) x y img)]))]))
     
     ;; lstone: last stone captured marked with a red "X"
     (: lstone : World Image -> Image)
     (define (lstone w img)
       (match w
         [(World bspec (Go board _ _ last-place _ _ _) _ _ _ _)
          (local
            {;; phyloc: PhysicalLoc of last stone captured
             (define phyloc : (Optional PhysicalLoc)
               (match last-place
                 ['None 'None]
                 [(Some l)
                  (Some (logical->physical l (vector-length board) bspec))]))}
            (match phyloc
              ['None img]
              [(Some (PhysicalLoc x y))
               (place-image (text "X" 10 "red") x y img)]))]))

     ;; lcap: last stones captured marked with "O" of corresponding color
     (: lcap : World Image -> Image)
     (define (lcap w img)
       (match w
         [(World bspec (Go board plyr _ _ last-opp _ _) _ _ _ _)
          (local
            {;; phylocs: Listof PhysicalLoc of last stones captured
             (define phylocs : (Listof PhysicalLoc)
               (map (lambda ([l : LogicalLoc])
                      (logical->physical l (vector-length board) bspec))
                    last-opp))

             ;; posns: phylocs turned into Listof Posn
             (define posns : (Listof Posn)
               (map (lambda ([l : PhysicalLoc])
                      (match l
                        [(PhysicalLoc x y) (make-posn x y)]))
                    phylocs))

             ;; imgs: "O" of corresponding color
             (define imgs : (Listof Image)
               (make-list (length posns)
                          (circle (BoardSpec-stone-radius-pixels bspec)
                                  "outline" plyr)))}
            (place-images imgs posns img))]))
     
     ;; lscap: last stones self-captured marked with "O" of corresponding color
     (: lscap : World Image -> Image)
     (define (lscap w img)
       (match w
         [(World bspec (Go board plyr _ _ _ last-self _) _ _ _ _)
          (local
            {;; phylocs: Listof PhysicalLoc of last stones self-captured
             (define phylocs : (Listof PhysicalLoc)
               (map (lambda ([l : LogicalLoc])
                      (logical->physical l (vector-length board) bspec))
                    last-self))

             ;; posns: phylocs turned into Listof Posn
             (define posns : (Listof Posn)
               (map (lambda ([l : PhysicalLoc])
                      (match l
                        [(PhysicalLoc x y) (make-posn x y)]))
                    phylocs))

             ;; imgs: "O" of corresponding color
             (define imgs : (Listof Image)
               (make-list (length posns)
                          (circle (BoardSpec-stone-radius-pixels bspec)
                                  "outline" (swap plyr))))}
            (place-images imgs posns img))]))}
         
    ;; End helper methods and start draw method
    (match w
      [(World (BoardSpec bg-color csize msize srad)
              (Go board plyr _ _ _ _ _)
              msg bt wt h)
       (local
         {(define base : Image
            (underlay
             (square (+ (* msize 2) (* csize (sub1 (vector-length board))))
                     "solid" bg-color)
             (img-grid (sub1 (vector-length board)) (img-cell (World-spec w)))))
          (define gameboard : Image
            (place-stones board (World-spec w) base))}
         (beside
          (beside
           (above (hover-stone w (lscap w (lcap w (lstone w gameboard))))
                  (col-labels w)
                  (overlay
                   (if (two-passes? (World-game w))
                       (match (outcome (World-game w))
                         [(Outcome b w win)
                          (above
                           (match win
                             ['draw (text "Draw!" 10 "black")]
                             ['black (text "Black Wins!" 10 "black")]
                             ['white (text "White Wins!" 10 "black")])
                           (text (string-append "Black: "
                                                (number->string b)
                                                " | White: "
                                                (number->string w))
                                 10 "black"))])
                       (above
                        (text (match plyr
                                ['black "Black's Turn"]
                                ['white "White's Turn"])
                              10 "black")
                        (text msg 10 "black")))
                   (rectangle (image-width base) 40 "solid" bg-color)))
           (above (row-labels w)
                  (square
                   (+ (image-height (col-labels w)) 25)
                   "solid" "white")))
          (above
           (img-timer bt "black")
           (img-timer (+ bt wt) "burlywood")
           (img-timer wt "white"))))])))
  
;; react-to-keyboard: pass turn when "p" is hit
(: react-to-keyboard : World String -> World)
(define (react-to-keyboard w key)
  (match key
    ["p"
     (match w
       [(World bspec (Go board plyr hist _ _ _ consec) msg bt wt h)
        (if (two-passes? (World-game w))
            (World bspec (World-game w) msg bt wt h)
            (local
              {;; poss-go: what the game would look like if player passes
               (define poss-go : Go
                 (Go board (swap plyr) (cons board hist)
                     'None '() '() (add1 consec)))}
              (match plyr
                ['white
                 (World bspec (Go board 'black (cons board hist) 'None '() '()
                                  (add1 consec)) "white passed" bt wt
                                                 (match h
                                                   ['None 'None]
                                                   [(Some hv)
                                                    (if (legal-move? poss-go hv)
                                                        h
                                                        'None)]))]
                ['black
                 (World bspec (Go board 'white (cons board hist) 'None '() '()
                                  (add1 consec)) "black passed" bt wt
                                                 (match h
                                                   ['None 'None]
                                                   [(Some hv)
                                                    (if (legal-move? poss-go hv)
                                                        h
                                                        'None)]))])))])]
    ["s" (begin (save-game! w) w)]
    ["l" (load-game (World-spec w))]
    [_ w]))

;; react-to-click: place stone at spot where board is clicked 
(: react-to-click : World Integer Integer Mouse-Event -> World)
(define (react-to-click w x y e)
  (match e
    ["button-down"
     (match w
       [(World bspec (Go board plyr hist _ _ _ _) msg bt wt h)
        (local
          {;; new-stone: (Optional LogicalLoc) of the given move
           (define new-stone : (Optional LogicalLoc)
             (physical->logical (PhysicalLoc x y) (vector-length board) bspec))
           
           ;; play-message
           (: play-message : Stone LogicalLoc -> String)
           (define (play-message s loc)
             (match s
               ['black (string-append "black played " (logical->string loc))]
               ['white (string-append "white played " (logical->string loc))]))}
          (match new-stone
            ['None w]
            [(Some s)
             (if (legal-move? (World-game w) s)
                 (World bspec (apply-move (World-game w) s)
                        (play-message plyr s) bt wt 'None)
                 (World bspec (World-game w) "illegal move" bt wt 'None))]))])]
    ["move"
     (match w
       [(World bspec (Go board plyr hist _ _ _ _) msg bt wt h)
        (local
          {;; log: (Optional LogicalLoc) of the proposed move
           (define log : (Optional LogicalLoc)
             (physical->logical (PhysicalLoc x y) (vector-length board) bspec))}
          (match log
            ['None (World bspec (World-game w) msg bt wt 'None)]
            [(Some l) (if (legal-move? (World-game w) l)
                          (World bspec (World-game w) msg bt wt log)
                          (World bspec (World-game w) msg bt wt 'None))]))])]
    [_ w]))

;; react-to-tick: updates the time/timers
(: react-to-tick (World -> World))
(define (react-to-tick w)
  (match w
    [(World bspec go msg bt wt h)
     (if (two-passes? go)
         (World bspec go "Game over!" bt wt h)
         (match (Go-next-to-play go)
           ['black (World bspec go msg (add1 bt) wt h)]
           ['white (World bspec go msg bt (add1 wt) h)]))]
    [_ w]))
       
;; play: initiate big bang and the game of Go
(: play : Integer BoardSpec -> World)
(define (play dim bspec)
  (cond
    [(< dim 2) (error "play: dim less than 2")]
    [(not (valid-board-spec? bspec)) (error "play: invalid BoardSpec")]
    [else
     (big-bang (World bspec
                      (Go (make-board dim) 'black '() 'None '() '() 0)
                      "Welcome to Go!" 0 0 'None) : World
       [to-draw draw]
       [on-mouse react-to-click]
       [on-key react-to-keyboard]
       [on-tick react-to-tick 1/10])]))

;; board->string: converts given Board into String
(: board->string : Board -> String)
(define (board->string board)
  (local
    {;; column->string: converts given column into String
     (: column->string : Integer Integer String -> String)
     (define (column->string c r s)
       (if (= r (vector-length board))
           s
           (match (bd-ref board (LogicalLoc c r))
             ['None
              (string-append "_" (column->string c (add1 r) s))]
             [(Some 'black)
              (string-append "*" (column->string c (add1 r) s))]
             [(Some 'white)
              (string-append "o" (column->string c (add1 r) s))])))

     (: helper : Integer String -> String)
     (define (helper c s)
       (if (= c (vector-length board))
           s
           (string-append (column->string c 0 "")
                          "|"
                          (helper (add1 c) s))))
     
     ;; str: the String produced with an extra "|" at the end
     (define str : String (helper 0 ""))}
    (substring str 0 (sub1 (string-length str)))))

;; string->board: converts given String into Board
(: string->board : String -> Board)
(define (string->board str)
  (local
    {;; dim: dimensions of the Board
     (: dim : String Integer Integer -> Integer)
     (define (dim str i res)
       (if (= i (string-length str))
           (add1 res)
           (if (char=? (string-ref str i) #\|)
               (dim str (add1 i) (add1 res))
               (dim str (add1 i) res))))

     ;; board: the empty board with given dimension
     (define board : Board (make-board (dim str 0 0)))

     ;; info: given String broken down into columns
     (define info : (Listof String) (string-split str "|"))

     (: helper : Integer Integer -> Void)
     (define (helper c r)
       (if (= r (vector-length board))
           (helper (add1 c) 0)
           (if (= c (vector-length board))
               (void)
               (local
                 {(define pt : Char (string-ref (list-ref info c) r))}
                 (match pt
                   [#\_
                    (begin (bd-set! board (LogicalLoc c r) 'None)
                           (helper c (add1 r)))]
                   [#\*
                    (begin (bd-set! board (LogicalLoc c r) (Some 'black))
                           (helper c (add1 r)))]
                   [#\o
                    (begin (bd-set! board (LogicalLoc c r) (Some 'white))
                           (helper c (add1 r)))])))))}
    (begin (helper 0 0) board)))
                
;; history->string: converts given history into String
(: history->string : (Listof Board) -> String)
(define (history->string bs)
  (if (empty? bs) ""
      (local
        {;; str: the String produced with an extra "!" at the end
         (define str : String
           (foldr (lambda ([b : Board] [s : String])
                    (string-append (board->string b) "!" s))
                  ""
                  bs))}
        (substring str 0 (sub1 (string-length str))))))

;; string->history: converts given String into history
(: string->history : String -> (Listof Board))
(define (string->history str)
  (if (string=? "" str) '()
      (local
        {;; info: given String broken down into Boards
         (define info : (Listof String) (string-split str "!"))
         
         (: helper : (Listof String) -> (Listof Board))
         (define (helper ss)
           (match ss
             ['() '()]
         [(cons hd tl) (cons (string->board hd) (helper tl))]))}
        (helper info))))

;; go->string: converts given Go into String
(: go->string : Go -> String)
(define (go->string go)
  (match go
    [(Go board plyr hist _ _ _ consec)
     (string-append (match plyr ['black "*"] ['white "o"])
                    "~"
                    (board->string board)
                    "~"
                    (history->string hist)
                    "~"
                    (number->string consec))]))

;; string->go: converts given String into Go
(: string->go : String -> Go)
(define (string->go str)
  (local
    {(define info : (Listof String) (string-split str "~"))}
    (Go (string->board (list-ref info 1))
        (match (list-ref info 0) ["*" 'black] ["o" 'white])
        (string->history (list-ref info 2))
        'None '() '() (string->integer (list-ref info 3)))))

;; world->string: converts given World into String
(: world->string : World -> String)
(define (world->string w)
  (match w
    [(World _ go _ bt wt _)
     (string-append (number->string bt)
                    "@"
                    (number->string wt)
                    "@"
                    (go->string go))]))

;; string->world: converts given BoardSpec and String into World
(: string->world : BoardSpec String -> World)
(define (string->world bspec str)
  (local
   {;; info: given String decoded into info
    (define info : (Listof String) (string-split str "@"))}
    (World bspec (string->go (list-ref info 2)) ""
           (string->integer (list-ref info 0))
           (string->integer (list-ref info 1)) 'None)))

;; string-split: splits given String at given delimiter
(: string-split : String String -> (Listof String))
(define (string-split str del)
  (local
    {(: helper : String Integer (Listof String) -> (Listof String))
     (define (helper s i ss)
       (if (= i (string-length s))
           (cons s ss)
           (if (char=? (string-ref s i) (string-ref del 0))
               (cons (substring s 0 i)
                     (helper (substring s (add1 i) (string-length s)) 0 ss))
               (helper s (add1 i) ss))))}
    (helper str 0 '())))

;; convert a text representation of an Integer to an Integer
;; raise an error if the string is not a number
;; return the integer part of the resulting number only
;; (this is intended only to be used with integers)
(: string->integer : String -> Integer)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) 
        (exact-round (real-part conv))
        (error "string->integer: invalid integer"))))

;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(: save-game! : World -> Void)
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
        (local
          {(define op : Output-Port (open-output-file path))}
          (begin (write-string (world->string w) op)
                 (close-output-port op)))
        (void))))

;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided BoardSpec to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : BoardSpec -> World)
(define (load-game bs)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
        (local
          {(define ip : Input-Port (open-input-file path))
           (define w : World
             (string->world bs (port->string (open-input-file path))))}
          (begin (close-input-port ip) w))
        (error "load-game: user cancelled"))))

(test)