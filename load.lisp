;; Copyright (c) 2011 Phil Hargett

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(in-package #:hh-load)

(defun generate-load (generator-function
		      &key 
		      ((:concurrency concurrency) 1)
		      ((:iterations iterations) 1))
  (let ((completed-iterations 0)
	(max-iteration-duration most-negative-fixnum)
	(min-iteration-duration most-positive-fixnum)
	(total-iterations-duration 0)
	(total-errors 0)
	(all-errors nil)
	(overall-start-time (get-internal-real-time)))
    (advise-thread-pool-size concurrency)
    (iterate-futures #'(lambda (f all)
			 (multiple-value-bind (duration error-p an-error) (yield f)
			   (incf completed-iterations)
			   (when error-p (incf total-errors))
			   (incf total-iterations-duration duration)
			   (when (> duration max-iteration-duration)
			     (setf max-iteration-duration duration))
			   (when (< duration min-iteration-duration)
			     (setf min-iteration-duration duration))
			   (when an-error
			     (push an-error all-errors))))
		     (loop for i from 1 to iterations
			collect (pcall #'(lambda ()
					   (let ((start-time (get-internal-real-time))
						 (error-p nil)
						 (an-error nil))
					     (handler-case 
						 (funcall generator-function)
					       (error (e)
						 (setf error-p t)
						 (setf an-error e)))
					     (values (/ (- (get-internal-real-time) start-time)
							internal-time-units-per-second)
						     error-p
						     an-error))))))
    (let* ((average-duration (/ total-iterations-duration completed-iterations))
	  (error-rate (/ total-errors completed-iterations))
	  (elapsed-time (/ (- (get-internal-real-time) overall-start-time)
			   internal-time-units-per-second))
	  (iterations-per-second (/ completed-iterations elapsed-time)))
      (format *error-output* "Elapsed time: ~,3f~%Completed: ~s iterations~%Iterations per second: ~,3f/second~%Errors: ~s (~,3f%)~%Longest: ~,3f seconds~%Average: ~,3f seconds~%Fastest: ~,3f seconds~%"
	      elapsed-time completed-iterations iterations-per-second total-errors (* 100 error-rate) max-iteration-duration average-duration min-iteration-duration)
      (when all-errors
	;; try to count errors of the same kind
	(let ((error-counts (make-hash-table :test #'equal)))
	  (loop for e in all-errors
	     do (let* ((error-string (let ((*print-escape* nil))
					(format nil "~a" e)))
		      (count (gethash error-string error-counts 0)))
		  (setf (gethash error-string error-counts) (1+ count))))
	  (loop for e being the hash-keys of error-counts
	       do (format *error-output* "~s times '~a'~%" (gethash e error-counts) e))))
      (values elapsed-time completed-iterations iterations-per-second total-errors error-rate max-iteration-duration average-duration min-iteration-duration))))

(defun generate-http-load (url
			   &key 
			   ((:concurrency concurrency) 1)
			   ((:iterations iterations) 1))
  (generate-load #'(lambda ()
		     (let ((completed-p nil))
		       (loop do (handler-case (progn 
						(http-request url)
						(setf completed-p t))
				  ;; we swallow these because they happen a lot on SBCL, likely due to GC
				  (sb-bsd-sockets:interrupted-error ()
				    t))
			    until completed-p)))
		 :concurrency concurrency
		 :iterations iterations))