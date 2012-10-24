#lang scribble/manual

@(require scribble/eval)

@(require (for-label racket/base))

@(define my-eval (make-base-eval))
@(my-eval '(require (for-syntax racket/base)))


@title{The 3n+1 problem}
@author["Danny Yoo"]
@section{Introduction}

I'm starting to go through
@link["http://www.amazon.com/exec/obidos/ASIN/0387001638/thealgorithmrepo"]{Programming
Challenges: The Programming Contest Training Manual}, by Steven S. Skiena and
Miguel Revilla.  I thought it would be fun to show how to approach the problems
using the @link["http://racket-lang.org"]{Racket} programming language.  Rather
than use a small, toy educational subset of the language, I'll take off the kid
gloves, and use whatever's available in
@link["http://docs.racket-lang.org/guide/index.html"]{@litchar{#lang racket}}.



@section{The problem}

The @link["http://acm.uva.es/p/v1/100.html"]{3n+1} problem is as follows:
consider a positive number n.  The @emph{cycle length} of n is the number of
times we repeat the following, until we reach n=1:
@itemlist[
@item{If n is odd, then n ⇐ 3n+1}
@item{If n is even, then n ⇐ n/2}
]

For example, given n=22, we see the following sequence: 22 11 34 17 52 26 13 40
20 10 5 16 8 4 2 1.  The cycle length of 22 is, therefore, 16, since it took 16
repetitions to get to 1.


Given a definition of cycle length of a number, here's the rest of the problem:
given any two numbers i and j, compute the maximum cycle length over all
numbers between i and j, inclusive.

@subsection{The plan}
Before we do any real coding, let's figure out a plan of attack and how to test
that plan.

@itemlist[@item{We need a way of computing cycle-length.}
          @item{We need to run cycle-length over a range of values and pick out the biggest result.}]


It sounds like we may want a function called @racket[cycle-length] that will
compute how long it takes for us to get n to 1.  If we have
@racket[cycle-length] as a helper function, then it becomes a fairly direct
loop through the range between i and j to pick out which one produces the
largest cycle length.

Let's first write up a stub function that computes some nonsense.  We'll
correct it in a moment, of course!

@interaction[#:eval my-eval
@code:comment{cycle-length: positive-integer -> positive-integer}
@code:comment{Computes the cycle length of n, according to}
@code:comment{the 3n+1 rules.}
(define (cycle-length n)
  42)]

This is certainly not right, but it's a start.  And it's something we can test!


@section{Test cases}

We want @racketblock[(cycle-length 1) ==> 1]

Let's express this more formally with the @racketmodname[rackunit] unit testing
library in Racket.

@interaction[#:eval my-eval
@code:comment{Load up rackunit:}
(require rackunit)

@code:comment{Let's express that test:}
(check-equal? (cycle-length 1) 1)

@code:comment{A few more tests, according to the problem statement above:}
(check-equal? (cycle-length 2) 2)
(check-equal? (cycle-length 4) 3)
(check-equal? (cycle-length 5) 6)
(check-equal? (cycle-length 22) 16)
]

All of our test cases fail.  Hurrah!


@section{A solution}

Ok, now that we coded up the tests, let's write a solution.  We can write out a
definition for @racket[cycle-length] almost straight out of the problem
statement:

@interaction[#:eval my-eval
(define (cycle-length n)
  (cond
    [(= n 1)
     1]
    [(odd? n) 
     (add1 (cycle-length (add1 (* 3 n))))]
    [(even? n)
     (add1 (cycle-length (/ n 2)))]))

@code:comment{Let us try it out on a few values:}
(cycle-length 1)
(cycle-length 2)
(cycle-length 22)
]


If we run this through our test suite, we should be fairly confident
that @racket[cycle-length] is probably doing the right thing.
(... modulo crazy inputs into the function such as @racket[0].  If we
want to guard against such inputs, we can use the features in
@racketmodname[racket/contract].)




@section{Optimizing @racket[cycle-length]}

How fast is the performance for @racket[cycle-length]?  Let's try timing it for
a few values, using the @racket[time] utility.  We'll run cycle-length for a
range of numbers, and see how long it takes.

@interaction[#:eval my-eval
(time (for ([i (in-range 1 100000)])
        (cycle-length i)))]


@subsection{Introducing an accumulator}
There are a few things we might do to improve the performance of this.  Having
the @racket[(add1 ...)] in the definition, waiting until the recursion finishes
up, seems ok, but I'm curious to see whether writing the definition with an
explicit accumulator will help us.

@interaction[#:eval my-eval
(define (cycle-length n)
  (cycle-length/acc n 1))

@code:comment{Helper function:}
(define (cycle-length/acc n acc)
  (cond
    [(= n 1)
     acc]
    [(odd? n) 
     (cycle-length/acc (add1 (* 3 n)) (add1 acc))]
    [(even? n)
     (cycle-length/acc (/ n 2) (add1 acc))]))
]

With this reformulation, how does this do now?
@interaction[#:eval my-eval
(time (for ([i (in-range 1 100000)])
        (cycle-length i)))]


This does help.  Although we do get an improvement, let's drop this
version for now and go back to our previous definition since it's
simpler---and because the next potential optimization will work better
on it!


@subsection{Adding memoization}

Another thing that comes to mind is this: our first good version of
@racket[cycle-length] works recursively.  More to the point: repeated use of
@racket[cycle-length] can reuse prior results that we computed earlier.  Maybe
@emph{memoization} will help.  Let's try it out: we'll keep a small table of
results, and consult that to see if we've already encountered the solution
before.


@interaction[#:eval my-eval
@code:comment{We'll maintain a table of known results.}
(define table (make-hash))

(define (cycle-length n)
  (cond
    @code:comment{Consult the table:}
    [(hash-has-key? table n)
     (hash-ref table n)]
    [else
      @code:comment{If we can't find it, compute it...}
      (define answer 
        (cond
         [(= n 1)
          1]
         [(odd? n) 
          (add1 (cycle-length (add1 (* 3 n))))]
         [(even? n)
          (add1 (cycle-length (/ n 2)))]))
      @code:comment{... and then put it into the table.}
      (hash-set! table n answer)
      @code:comment{Don't forget to return the value back!}
      answer]))]


Does the overhead of setting up this table pay for itself?  Let's see:

@interaction[#:eval my-eval
(time (for ([i (in-range 1 100000)])
        (cycle-length i)))]

Hey, not bad at all!  That's significantly better.

We should make sure, of course, that all our test cases are running on this ok.

@interaction[#:eval my-eval
(check-equal? (cycle-length 1) 1)
(check-equal? (cycle-length 2) 2)
(check-equal? (cycle-length 4) 3)
(check-equal? (cycle-length 5) 6)
(check-equal? (cycle-length 22) 16)]

All's quiet on the @racket[cycle-length] front.  The tests are all passing.


@subsection{Advanced: abstracting memoization to a helper macro}

It turns out that the kind of memoization we've done here can be lifted out, so
that we can easily perform it at will.  That is, what we're doing is something
like the following:

@nested[#:style 'inset]{
Given a definition that we'd like to memoize:

@itemlist[
   @item{create a table for exclusive use by the definition, and}

   @item{slightly tweak the definition's body so it consults the table
   before going through computation.}
]
}


In terms of Racket, we can say that like this:
@interaction[#:eval my-eval
@code:comment{A little helper to centralize the memoization logic}
@code:comment{into a single rewrite rule:}
(define-syntax-rule (define/memo (name id) body ...)
  (begin 
    (define table (make-hash))
    (define (name id)
      (cond
        [(hash-has-key? table id)
         (hash-ref table id)]
        [else
         (define answer (begin body ...))
         (hash-set! table id answer)
         answer]))))]


This defines a small rewrite rule that expresses the idea of memoizing simple,
1-argument function definitions.  Once we have this @racket[define/memo], we
can rewrite @racket[cycle-length] to use it:

@interaction[#:eval my-eval
(define/memo (cycle-length n)
  (cond
    [(= n 1)
     1]
    [(odd? n) 
     (add1 (cycle-length (add1 (* 3 n))))]
    [(even? n)
     (add1 (cycle-length (/ n 2)))]))]

which is nice because it's easy to read.




@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@; I don't think I want this section, so it's commented out for now.
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@void{
@subsection{Advanced: unsafe operations with @tt{racket/unsafe/ops}}

If we want to improve the runtime a little more, we can also change the numeric
operations in @racket[cycle-length]'s definition to use the unsafe variations
in @racketmodname[racket/unsafe/ops].

Let's see what that might look like:

@interaction[#:eval my-eval
(require racket/unsafe/ops)
(define/memo (cycle-length n)
  (cond
    [(unsafe-fx= n 1)
     1]
    [(= (unsafe-fxmodulo n 2) 1)
     (unsafe-fx+ 1 (cycle-length (unsafe-fx+ 1 (unsafe-fx* 3 n))))]
    [else
     (unsafe-fx+ 1 (cycle-length (unsafe-fxquotient n 2)))]))


(time (for ([i (in-range 1 100000)])
        (cycle-length i)))]
}



@section{Cycling back to a loop}

Now that we have a fairly robust @racket[cycle-length] function, we can do the
rest of the problem.  Given a range of numbers, we want to go through them,
compute the cycle lengths, and pick out the biggest one.

We can try to write this directly with a @racket[for/list] to create a list of
all the cycle-lengths, and @racket[apply] the @racket[max] across that list.
Let's write this in code:

@interaction[#:eval my-eval
(define (max-cycle-length-range i j)
  (apply max
         (for/list ([n (in-range i (add1 j))])
                       @code:comment{(add1 j) for inclusion ...}
           (cycle-length i))))]

Let's write a few test cases to make sure that this is computing the right
thing:
@interaction[#:eval my-eval
@code:comment{From the "Sample Output" section of}
@code:comment{http://acm.uva.es/p/v1/100.html}
(check-equal? (max-cycle-length-range 1 10) 20)
]

What?!  Oh, whoops, I wasn't using the @racket[n] in the loop.  Silly me.  Let's fix that.


@interaction[#:eval my-eval
(define (max-cycle-length-range i j)
  (apply max
         (for/list ([n (in-range i (add1 j))])
           (cycle-length n))))]

Thank goodness for test cases.


Ok, let's try that again.

@interaction[#:eval my-eval
(check-equal? (max-cycle-length-range 1 10) 20)
(check-equal? (max-cycle-length-range 100 200) 125)
(check-equal? (max-cycle-length-range 201 210) 89)
(check-equal? (max-cycle-length-range 900 1000) 174)
]

All passing?  Much better!


@subsection{Advanced: maximizing a loop}

It would be nice if we could directly express taking the maximum
across a @racket[for] loop.  We're performing the maximum computation
by first constructing a list of all the cycle lengths, then applying
@racket[max] over the whole list.  Can we avoid that auxiliary list
construction, and just compute the max as we're running through the
numbers?

In fact, there are several variations of @racket[for] loops in Racket,
so maybe one of those variations will work for us.  For example, we
could use @racket[for/fold], which gives us enough expressive power to
take the maximum during iteration.

@interaction[#:eval my-eval
(for/fold ([current-max -inf.0])
          ([n '(3 1 4 1 5 9 2 6)])
  (if (> n current-max) n current-max))]

There are other versions of @racket[for] loops, such as the one for
taking sums (@racket[for/sum]).  But as of this writing, there doesn't
seem to be be a @racket[for/max] form that lets us take the maximum
directly.


The question arises: how difficult is it to build @racket[for/max]?


It turns out that it's not too bad, though it requires a little more macrology:
we'll use @racket[for/fold/derived] to express our own @racket[for/max] loop in terms of folding:

@interaction[#:eval my-eval
(define-syntax (for/max stx)
  (syntax-case stx ()
   [(_ clauses . defs+exprs)
    (with-syntax ([original stx])
      #'(for/fold/derived original 
                          ([current-max -inf.0])
                          clauses
          (define maybe-new-max 
            (let () . defs+exprs))
          (if (> maybe-new-max current-max)
              maybe-new-max
              current-max)))]))
]

Essentially, as we're looping through numbers, we maintain a
@racket[current-max], and update that max accordingly as we walk
across the iteration.  The rest of the code in @racket[for/max]
delegates the rest of the gruntwork to
@racket[for/fold] (technically, @racket[for/fold/derived]).


We must test this, of course:

@interaction[#:eval my-eval
@code:comment{Edge case: if we take the maximum of no numbers,}
@code:comment{let's see -inf.0.}
(check-equal? (for/max ([i '()])
                i)
              -inf.0)

(check-equal?
 (for/max ([i '(3 1 4 1 5 9 2 6)])
   i)
 9)           
         
(check-equal?
 (for/max [(i (in-range 1 23))]
   i)
 22)

(check-equal? 
 (for/max ([n '(3.14159 2.71828 1.61803)]
           [s '(-1      1       1)])
   (* n s))
 2.71828)

@code:comment{... and of course...}
(check-equal? 
 (for/max [(i (in-range 900 (add1 1000)))]
   (cycle-length i))
 174)
]

Looks good.  With this, let's express @racket[max-cycle-length-range]
in terms of @racket[for/max] now:

@interaction[#:eval my-eval
(define (max-cycle-length-range i j)
  (for/max ([n (in-range i (add1 j))])
    (cycle-length n)))
]




@section{Making a module}



Now that we have most of the solution worked out, let's make a module
that encapsulates what we've done.  Let's lift up the definitions that
we used to make the solution nice and pretty, and place them into
@filepath{helpers.rkt}:

@filebox["helpers.rkt"]{
@codeblock|{
#lang racket

(provide for/max define/memo)

(define-syntax (for/max stx)
  (syntax-case stx ()
   [(_ clauses . defs+exprs)
    (with-syntax ([original stx])
      #'(for/fold/derived original 
                          ([current-max -inf.0])
                          clauses
          (define maybe-new-max
            (let () . defs+exprs))
          (if (> maybe-new-max current-max)
              maybe-new-max
              current-max)))]))


(define-syntax-rule (define/memo (name id) body ...)
  (begin 
    (define table (make-hash))
    (define (name id)
      (cond
        [(hash-has-key? table id)
         (hash-ref table id)]
        [else
         (define answer (begin body ...))
         (hash-set! table id answer)
         answer]))))

(module+ test
  (require rackunit)
  (check-equal? (for/max ([i '()])
                  i)
                -inf.0)
  (check-equal? (for/max ([i '(3 1 4 1 5 9 2 6)])
                  i)
                9)                    
  (check-equal? (for/max [(i (in-range 1 23))]
                  i)
                22)
  
  (check-equal? 
   (for/max ([n '(3.14159 2.71828 1.61803)]
             [s '(-1      1       1)])
     (* n s))
   2.71828))
}|
}
Who knows?  We might reuse @filepath{helpers.rkt} sometime.

(You may note that the bottom of @filepath{helpers.rkt} contains a
@link["http://docs.racket-lang.org/guide/Module_Syntax.html#(part._main-and-test)"]{@racket[test]
submodule} which collects the unit tests that we've written.  We can
run a module's test suite by using
@link["http://docs.racket-lang.org/raco/test.html"]{@tt{raco test}}.)


With our @filepath{helpers.rkt} in in hand, let's put our solution in
@filepath{three-n-plus-one.rkt}:

@filebox["three-n-plus-one.rkt"]{
@codeblock|{
#lang racket

(require "helpers.rkt")

(define/memo (cycle-length n)
  (cond
    [(= n 1)
     1]
    [(odd? n) 
     (add1 (cycle-length (add1 (* 3 n))))]
    [(even? n)
     (add1 (cycle-length (/ n 2)))]))

(define (max-cycle-length-range i j)
  (for/max ([n (in-range i (add1 j))])
    (cycle-length n)))


(module+ test
  (require rackunit)

  (check-equal? (cycle-length 1) 1)
  (check-equal? (cycle-length 2) 2)
  (check-equal? (cycle-length 4) 3)
  (check-equal? (cycle-length 5) 6)
  (check-equal? (cycle-length 22) 16)

  (check-equal? 
   (max-cycle-length-range 1 10) 20)
  (check-equal?
   (max-cycle-length-range 100 200) 125)
  (check-equal? 
   (max-cycle-length-range 201 210) 89)
  (check-equal?
   (max-cycle-length-range 900 1000) 174)
  (check-equal?
   (for/max [(i (in-range 900 (add1 1000)))]
     (cycle-length i))
   174))
}|
}



@section{Integrating with I/O and a @tt{main}}

Finally, all this unit testing is fine and dandy, but we don't
actually read input from standard input.  Let's fix that, and modify
@filepath{three-n-plus-one.rkt} so it can be run as the @emph{main}
entry point.

We can read individual lines as strings by iterating across
@racket[current-input-port] with @racket[in-lines]:
@racketblock[(for ([line (in-lines (current-input-port))]) ...)]

Once we have a line in hand, we can parse out the individual chunks
with @racket[read].  @racket[read] doesn't normally read from strings
directly, so we first translate each string into a port-like value
using @racket[open-input-string].

Last of all, let's add the following to the bottom of
@filepath{three-n-plus-one.rkt}:

@racketblock[
(module+ main
  (for ([line (in-lines (current-input-port))])
    (define line-port (open-input-string line))
    (define i (read line-port))
    (define j (read line-port))
    (when (and (number? i)  (number? j))
      (printf "~a ~a ~a\n"
              i j 
              (max-cycle-length-range i j)))))
]

This defines a @tt{main} submodule.  When we run
@filepath{three-n-plus-one.rkt} directly from the command line, it will
run @tt{main}:

@nested[#:style 'code-inset]{@verbatim|{
$ cat sample-data.txt
1 10
100 200
201 210
900 1000

$ racket three-n-plus-one.rkt < sample-data.txt
1 10 20
100 200 125
201 210 89
900 1000 174
}|}


@section{The files}
@itemlist[
@item{@link["http://hashcollision.org/three-n-plus-one/helpers.rkt"]{helpers.rkt}}
@item{@link["http://hashcollision.org/three-n-plus-one/three-n-plus-one.rkt"]{three-n-plus-one.rkt}}
]