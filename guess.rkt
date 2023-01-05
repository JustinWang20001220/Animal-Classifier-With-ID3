;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)

(require "animals.rkt") 
(define seen
  (list
   (list 'squirrel 'small 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'crow 'medium 'flies 'angry)))

;;
;; (a)
;;

;; (collect-attributes examples) consumes a list of examples and produces
;; a list of attributes contained in the examples with no duplicates
;; Examples:
(check-expect (collect-attributes seen)
              (list 'medium 'flies 'swims 'large 'angry 'small))

;; collect-attributes: (Listof Example) -> (List of Sym)
(define (collect-attributes examples)
  (local [(define (attributes-flatner examples)
            (cond [(empty? examples) empty]
                  [else (append (rest (first examples))
                                (attributes-flatner (rest examples)))]))
          (define (duplicate-remover attrs accu-list)
              (cond [(empty? attrs) accu-list]
                    [(member? (first attrs) accu-list)
                     (duplicate-remover (rest attrs) accu-list)]
                    [else
                     (duplicate-remover
                      (rest attrs)
                      (cons (first attrs) accu-list))]))]
    (duplicate-remover (attributes-flatner examples) empty)))

;; Tests:
(check-expect (collect-attributes empty) empty)

(define test-examples1
  (list
   (list 'captain 'hook 'angry)
   (list 'sailor 'pegleg 'eyepatch)
   (list 'captain 'hat 'hook 'parrot)))
(check-expect (collect-attributes test-examples1)
              (list 'parrot 'hat 'eyepatch 'pegleg 'angry 'hook))

(define test-examples2
  (list
   (list 'captain 'what 'angry)
   (list 'sailor 'pegleg 'eyepatch)
   (list 'captain 'hat 'hook 'parrot)))
(check-expect (collect-attributes test-examples2)
              (list 'parrot 'hook 'hat 'eyepatch 'pegleg 'angry 'what))

(define test-examples3
  (list
   (list 'captain 'what)
   (list 'sailor 'what)
   (list 'captain 'what)))
(check-expect (collect-attributes test-examples3)
              (list 'what))



;;
;; (b)
;;

;; (split-examples examples symbol): that splits the list of examples on the
;; given symbol
;; Examples:
(check-expect (split-examples seen 'goose)
              (list
               (list
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry))
               (list
                (list 'crow 'medium 'flies 'angry)
                (list 'squirrel 'small 'angry))))

(check-expect (split-examples seen 'small)
              (list
               (list
                (list 'squirrel 'small 'angry))
               (list
                (list 'crow 'medium 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry))))

;; split-examples: (Listof Example) Sym -> (Listof (Listof Example))
(define (split-examples examples sym)
  (local [(define (splitter examples lst-with lst-without)
            (cond [(empty? examples) (list lst-with lst-without)]
                  [(sym-checker (first examples))
                   (splitter (rest examples)
                             (cons (first examples) lst-with) lst-without)]
                  [else (splitter (rest examples)
                             lst-with (cons (first examples) lst-without))]))
          (define (sym-checker example)
            (cond [(empty? example) false]
                  [(symbol=? sym (first example)) true]
                  [else (sym-checker (rest example))]))]
    (splitter examples empty empty)))

;; Tests:
(check-expect (split-examples empty 'nothing) (list '() '()))
(check-expect (split-examples test-examples1 'captain)
              (list
               (list
                (list 'captain 'hat 'hook 'parrot)
                (list 'captain 'hook 'angry))
               (list
                (list 'sailor 'pegleg 'eyepatch))))

(check-expect (split-examples test-examples2 'nothing)
              (list
               '()
               (list
                (list 'captain 'hat 'hook 'parrot)
                (list 'sailor 'pegleg 'eyepatch)
                (list 'captain 'what 'angry))))

(check-expect (split-examples test-examples3 'sailor)
              (list
               (list (list 'sailor 'what))
               (list
                (list 'captain 'what)
                (list 'captain 'what))))



;;
;; (c)
;;

;; (historgram seen) consumes a example and produce a histogram that shows
;; the frequecy of attrbutes appearing
;; Examples:
(check-expect (histogram seen)
              (list
               (list 'small 1) (list 'angry 4) (list 'large 2)
               (list 'swims 2) (list 'flies 3) (list 'medium 1)))

;; histogram: (Listof Example) -> (listof (List Sym Num))
(define (histogram examples)
  (local [(define (attributes-flatner examples)
            (cond [(empty? examples) empty]
                  [else (append (rest (first examples))
                                (attributes-flatner (rest examples)))]))
          (define (his-creator attrs his)
            (cond [(empty? attrs) his]
                  [else (his-creator
                         (rest attrs)
                         (his-changer (first attrs) his))]))
          (define (his-changer new-sym his)
            (cond [(empty? his) (list (list new-sym 1))]
                  [(symbol=? (first (first his)) new-sym)
                   (cons (list (first (first his))
                               (add1 (first (rest (first his))))) (rest his))]
                  [else (cons (first his) (his-changer new-sym (rest his)))]))]
    (his-creator (attributes-flatner examples) empty)))

;; Tests:
(check-expect (histogram test-examples1)
              (list
               (list 'hook 2)
               (list 'angry 1)
               (list 'pegleg 1)
               (list 'eyepatch 1)
               (list 'hat 1)
               (list 'parrot 1)))
(check-expect (histogram test-examples2)
              (list
               (list 'what 1)
               (list 'angry 1)
               (list 'pegleg 1)
               (list 'eyepatch 1)
               (list 'hat 1)
               (list 'hook 1)
               (list 'parrot 1)))
(check-expect (histogram test-examples3)
              (list (list 'what 3)))
          


;;
;; (d)
;;

;; (augment-histogram histogram attributes total) consumes a list of all
;; attributes and a total for the number of examples, produces a augmented
;; histogram, which is a list of (List Sym Nat Nat) triples
;; Examples:
(check-expect
 (augment-histogram
  (list (list 'a 100) (list 'c 50))
  (list 'a 'b 'c)
  200)
 (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))
(check-expect
 (augment-histogram empty (list 'x 'y) 10)
 (list (list 'x 0 10) (list 'y 0 10)))

;; augment-histogram: Histogram (Listof Sym) Nat -> AH
;; Require: the total >= all the count in the histogram
(define (augment-histogram histogram attributes total)
  (local [(define (count-returer sym his)
            (cond [(empty? his) -1]
                  [(symbol=? (first (first his)) sym)
                   (first (rest (first his)))]
                  [else (count-returer sym (rest his))]))
          (define (aug-builder attris)
            (local [(define count
                      (cond [(empty? attris) -1]
                            [else (count-returer (first attris) histogram)]))]
              (cond [(empty? attris) empty]
                    [(>= count 0)
                     (cons (list (first attris) count (- total count))
                           (aug-builder (rest attris)))]
                    [else (cons (list (first attris) 0 total)
                           (aug-builder (rest attris)))])))]
    (aug-builder attributes)))

;; Tests:
(check-expect
 (augment-histogram
  (list (list 'a 100) (list 'b 10) (list 'c 50))
  (list 'a 'b 'c)
  200)
 (list (list 'a 100 100) (list 'b 10 190) (list 'c 50 150)))

(check-expect (augment-histogram empty empty 10) empty)
                           


;;
;; (e)
;;

;; (entropy positive-counts negative-counts) consumes two element from
;; augmented histogram and prduce their entropy
;; Examples:
(check-within (entropy (list 'large 126 59) (list 'large 146 669))
              #i0.5663948489858 0.001)
(check-within (entropy (list 'small 17 168) (list 'small 454 361))
              #i0.5825593868115 0.001)
(check-within (entropy (list 'a 0 100) (list 'b 100 0))
              0.0 0.001)

;; entropy: (List Sym Num Num) (List Sym Num Num) -> Num
(define (entropy positive-counts negative-counts)
  (local [(define a (first (rest positive-counts)))
          (define c (first (rest (rest positive-counts))))
          (define b (first (rest negative-counts)))
          (define d (first (rest (rest negative-counts))))
          (define (p n m)
            (cond [(= 0 (+ n m)) 0.5]
                  [else (/ n (+ n m))]))
          (define (e p)
            (cond [(= 0 p) 0]
                  [else (* -1 p (log p 2))]))
          (define result
            (+ (* (p (+ a b) (+ c d))
                  (+ (e (p a b)) (e (p b a))))
               (* (p (+ c d) (+ a b))
                  (+ (e (p c d)) (e (p d c))))))]
    result))

;; Tests:
(check-within (entropy (list 'large 50 50) (list 'large 50 50)) #i1.0 0.001)
(check-within (entropy (list 'large 50 100) (list 'large 100 50))
              #i0.9182958340544896 0.001)
(check-within (entropy (list 'large 0 0) (list 'large 0 0)) #i1.0 0.001)



;;
;; (f)
;;

;; (entropy-attributes positive negative) consumes two augmented histogram
;; and computes the entropy of each attribute, producing a list of attribute
;; /entropy that will appear in the same order
;; Examples:
(check-within
 (entropy-attributes
  (list
   (list 'large 126 59) (list 'angry 161 24)
   (list 'small 17 168) (list 'flies 170 15)
   (list 'swims 162 23) (list 'medium 42 143))
  (list
   (list 'large 146 669) (list 'angry 469 346)
   (list 'small 454 361) (list 'flies 615 200)
   (list 'swims 365 450) (list 'medium 215 600)))
 (list
  (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
  (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
  (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)) 0.001)

;; entropy-attributes: (Listof (List Sym Num Num)) (Listof (List Sym Num Num))
;;                     -> (Listof (List Sym Num))

(define (entropy-attributes positive negative)
  (cond [(empty? positive) empty]
        [else (cons (list (first (first positive))
                          (entropy (first positive) (first negative)))
                    (entropy-attributes (rest positive) (rest negative)))]))

;; Tests:
(check-within (entropy-attributes empty empty) empty 0.001)
(check-within
 (entropy-attributes
  (list
   (list 'large 126 59) (list 'angry 161 24))
  (list
   (list 'large 146 669) (list 'angry 469 346)))
 (list
  (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)) 0.001)




;;
;; (g)
;;

;; (best-attribute entropies) consumes a non-empty list of attribute/entropy
;; pairs, produces the attribute with the minimum entropy
;; Examples:
(check-expect
 (best-attribute
  (list
   (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
   (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
   (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)))
 'large)

;; best-attribute: (Listof (List Sym Num)) -> Sym
(define (best-attribute entropies)
  (local [(define (comparitor entropies current-minimum current-attribute)
            (cond [(empty? entropies) current-attribute]
                  [(< (first (rest (first entropies))) current-minimum)
                   (comparitor (rest entropies)
                               (first (rest (first entropies)))
                               (first (first entropies)))]
                  [else (comparitor
                         (rest entropies) current-minimum current-attribute)]))]
    (comparitor entropies 1000 'nothing)))

;; Tests:
(check-expect
 (best-attribute
  (list
   (list 'angry #i0.6447688190492)
   (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
   (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)))
 'small)

(check-expect
 (best-attribute
  (list
   (list 'angry #i0.6447688190492) (list 'flies #i0.6702490498564)
   (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)))
 'swims)
                        


;;
;; (h)
;;

;; (build-dt examples lable) consumes a list of examples and a symbol and
;; generate its corresponding decision tree
;; Examples:

;; build-dt: (Listof example) Sym -> DT
(define (build-dt examples label)
  (local [(define (remover sym examples)
            (local [(define (remover-helper example)
                      (cond [(empty? example) empty]
                            [(symbol=? (first example) sym)
                             (remover-helper (rest example))]
                            [else (cons (first example)
                                        (remover-helper (rest example)))]))
                    (define (remover-wrapper examples)
                      (cond [(empty? examples) empty]
                            [else (cons (remover-helper (first examples))
                                        (remover-wrapper (rest examples)))]))]
              (remover-wrapper examples)))
          (define attributes (collect-attributes examples))
          (define splitted-examples-label (split-examples examples label))
          (define positive-examples-label
            (first splitted-examples-label))
          (define negative-examples-label
            (first (rest splitted-examples-label)))]
    (cond [(empty? positive-examples-label) false]
          [(empty? negative-examples-label) true]
          [(empty? attributes)
           (cond [(> (length positive-examples-label)
                     (length negative-examples-label))
                  true]
                 [else false])]
          [else
           (local [(define positive-his (histogram positive-examples-label))
                   (define negative-his (histogram negative-examples-label))
                   (define total (length examples))
                   (define positive-aug-his
                     (augment-histogram positive-his
                                        attributes
                                        total))
                   (define negative-aug-his
                     (augment-histogram negative-his
                                        attributes
                                        total))
                   (define entropy-list (entropy-attributes
                                         positive-aug-his
                                         negative-aug-his))
                   (define root-attribute (best-attribute entropy-list))
                   
                   (define splitted-examples-attris
                     (split-examples examples root-attribute))
                   (define positive-examples-attris
                     (first splitted-examples-attris))
                   (define negative-examples-attris
                     (first (rest splitted-examples-attris)))
                   (define positive-examples-attris-removed
                     (remover root-attribute positive-examples-attris))
                   (define left-subtree
                     (build-dt positive-examples-attris-removed label))
                   (define right-subtree
                     (build-dt negative-examples-attris label))]
             (cond [(equal? left-subtree right-subtree) left-subtree]
                   [else (list root-attribute left-subtree right-subtree)]))])))
                  


;;
;; (i)
;;

;; (train-classifier examples label) consumes a list of example and produce
;; a function which consumes a list of attributes and a label and produce true
;; if the attributes satisfy the label object, false otherwise

;; train-classifier: (Listof Example) Sym -> ((Listof Sym)-> Bool)
(define (train-classifier examples label)
  (local [(define (classifier attributes)
            (classifier-helper (build-dt examples label) attributes))
          (define (classifier-helper dt attributes)
            (cond [(boolean? dt) dt]
                  [(member? (first dt) attributes)
                   (classifier-helper (first (rest dt)) attributes)]
                  [else (classifier-helper (first (rest (rest dt)))
                                           attributes)]))]
    classifier))

;; Examples/Tests:

(define goose? (train-classifier (random-animals 1000) 'goose))
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)
(define squirrel? (train-classifier (random-animals 1000) 'squirrel))
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small 'angry)) true)
(define crow? (train-classifier (random-animals 1000) 'crow))
(check-expect (crow? (list 'angry 'flies 'medium)) true)

(define emu? (train-classifier (random-animals 1000) 'emu))
(check-expect (emu? (list 'large 'angry 'flies 'swims)) false)
                                              
                  
;;
;; Bonus
;;
;; (performance classifier? examples label) that takes a classifier, examples,
;; and a label and computes the sensitivity and specificity for the classifier
;; on the examples.

;; performance: ((Listof Sym)-> Bool) (Listof Example) Sym -> (List Sym Num Num)
(define (performance classifier examples label)
  (local [(define (actual-value-generator examples)
            (cond [(empty? examples) empty]
                  [(symbol=? (first (first examples)) label)
                   (cons true (actual-value-generator (rest examples)))]
                  [else (cons false
                              (actual-value-generator (rest examples)))]))
          (define actual-values (actual-value-generator examples))
          (define (test-attribute-generator examples)
            (cond [(empty? examples) empty]
                  [else (cons (rest (first examples))
                              (test-attribute-generator (rest examples)))]))
          (define test-attributes-list (test-attribute-generator examples))
          (define test-classifier (train-classifier examples label))
          (define (result-value-generator test-attributes-list)
            (cond [(empty? test-attributes-list) empty]
                  [else
                   (cons (test-classifier
                          (first test-attributes-list))
                         (result-value-generator
                          (rest test-attributes-list)))]))
          (define result-values (result-value-generator test-attributes-list))
          (define pos-count
            (length (filter (lambda (x) x) actual-values)))
          (define neg-count
            (length (filter (lambda (x) (not x)) actual-values)))
          (define (main actual-values result-values sen-count spe-count)
            (cond [(empty? actual-values)
                   (list label (round (* 100 (/ sen-count pos-count)))
                         (round (* 100 (/ spe-count neg-count))))]
                  [(and (first actual-values) (first result-values))
                   (main (rest actual-values)
                         (rest result-values)
                         (add1 sen-count) spe-count)]
                  [(and (not (first actual-values)) (not (first result-values)))
                   (main (rest actual-values)
                         (rest result-values)
                         sen-count (add1 spe-count))]
                  [else (main (rest actual-values)
                         (rest result-values)
                         sen-count spe-count)]))
          (define performance-meter
            (main actual-values result-values 0 0))]
    performance-meter))
                  
                  
                  
    
            
                    

                    
                    
            
            
          


                   
                   



          
            














                   