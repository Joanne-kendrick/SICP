#lang racket
;;;;;Nalikka Joan Deborah

(require csv-reading math math/matrix racket/hash)
(require data-science-master)
(require plot)
(require date-master)


;;;;1. Read the twitter csv file.
(define twitter-data (read-csv "Joanne.csv" #:header? #t))
(define created-at-column ($ twitter-data 'created_at))
(define text-column ($ twitter-data 'text))

;;;;;clean the csv data.
(define (clean-tweet text)
  (string-normalize-spaces
   (remove-punctuation
    (remove-urls                                  
     (string-downcase text)))))                

(define cleaned-tweets (map clean-tweet text-column));;cleaned tweets using the clean text rows.

(define all-clean(string-join cleaned-tweets " "));; join the cleaned data.


;;;define words and sentiments:
(define words (document->tokens all-clean #:sort? #t))
(define sentiment (list->sentiment words #:lexicon 'nrc))

;;;show the first 10 lines of sentiments.
(take sentiment 10)

;;;;Visualisation of the total/aggregated sentiments in our csv file.
(displayln (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))) ;;;print on screen
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Emotions"
	  #:y-label "Frequency")))


;;;;Extract months from data and group by month:
(define (extract-month datetime)
    (let* ([date-part(first (string-split datetime " "))]
           [month (string->number (first (string-split date-part "/")))]) ; Extract month as number
      month))


(define months (map extract-month created-at-column))
;(define sentiment-by-month-pairs (map list months (map second sentiment)))

;;;code to remove duplicates from the extracted months.
(define (unique-months datetime-list)
  (define seen-months (make-hash)) ; 
  (define (is-unique month)
    (if (hash-has-key? seen-months month)
        #f
        (begin
          (hash-set! seen-months month #t)
          #t)))
   ;(define extracted-months (map extract-month datetime-list))
  (filter is-unique months))

(displayln months)
(displayln (unique-months months))

;;;sentiment by month.
(define tweet-sentiments
  (map (lambda (tweet)
         (list->sentiment (document->tokens tweet #:sort? #t) #:lexicon 'nrc)) ; Compute sentiment
       cleaned-tweets))

(define sentiment-by-month-pairs (map list months tweet-sentiments))
(displayln sentiment-by-month-pairs)

(define grouped-sentiments (group-with first sentiment-by-month-pairs))

(define summed-sentiments
  (for/hash ([month (hash-keys grouped-sentiments)])
    (values month (apply + (map second (hash-ref grouped-sentiments month))))))

(for-each
 (lambda (month)
   (printf "Month: ~a | Total Sentiment: ~a\n"
           month
           (hash-ref summed-sentiments month)))
 (hash-keys summed-sentiments))

#|;;;;; sentiments for a given month
(define (sentiments-for-month month-number)
  (let ([sentiments (hash-ref grouped-sentiments month-number '())])
    (if (null? sentiments)
        (printf "No sentiments found for month ~a\n" month-number)
        (printf "Sentiments for month ~a: ~a\n" month-number (map second sentiments)))))|#