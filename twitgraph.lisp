;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TWITGRAPH -*-
;;; Copyright (c) 2012 Kenan Bölükbaşı. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defpackage #:twitgraph
  (:use #:common-lisp)
  (:export
   #:search-and-render-diagram))

(in-package #:twitgraph)

;;; *MINIMUM-FONT-SIZE*
;; The nodes are automatically scaled
;; to include to text properly.
;; So font size also effects node scale.
(defvar *minimum-font-size* 15)

;;; *FONT-SIZE-FACTOR*
;; This parameter is defined to easily
;; control shapes scales in case query
;; causes nonproportional diagrams.
(defvar *font-size-factor* 50)

;; It is possible to solve probably most of
;; the problems with diagram proportions
;; using the combination of *minimum-font-size*
;; and *font-size-factor* parameters.

;;; +RESULTS-PER-PAGE+
;;; Maximum number of results you can get
;; per-page is limited to 100 in Twitter API.
(defconstant +results-per-page+ 100)


;;; SEARCH-AND-EXTRACT-TWEETS
;;; Search Twitter with given parameters,
;; extract SEARCH-REF data for every page,
;; append the pages of refs into a single list.
(defun search-and-extract-tweets (keyword &key (lang "en") (page 5)) 
  (reduce #'append (loop for x from 1 to page collecting 
			(twit:search-result-results (twit:search-twitter 
						     keyword 
						     :rpp +results-per-page+ 
						     :lang lang 
						     :page x)))))

;;; TWEET-TAG-P
;;; Query if STRING is a proper hashtag/mention.
;; TAG parameter gets a char as an argument.
;; tests for MENTION if TAG is #\@
;; tests for HASHTAG if TAG is #\#
;; TAG parameter gets its argument from
;; the first CHAR of SEARCH QUERY string. 
(defun tweet-tag-p (string tag)
  (if (string-equal string "") 
      nil 
      (eql tag (char string 0))))

;;; GET-TAGS
;;; Extract the text from the SEARCH-REF list.
;; Remove everything except proper TAGS.
;; Doesn't remove the nesting.
;; TAGS from same Tweet are grouped.
;; Removes all NON-ALPHANUMERIC chars from tags.
;; If TAG is EMPTY-STRING after NON-ALPHANUMERIC
;; removal, it is deleted.
(defun get-tags (search-ref tag)
  (remove "" 
	  (mapcar #'(lambda (tag) 
		      (string-upcase (remove-if-not 
				      #'alphanumericp tag))) 
		  (remove-if-not #'(lambda (tweet-content) 
				     (tweet-tag-p tweet-content tag))
				 (ppcre:split "\\s" 
					      (twit:search-ref-text 
					       search-ref))))
	  :test #'string-equal))

;;; COUNT-AND-LIMIT-TAGS
;;; The argument of LIST is the appended TAG list.
;; On first run, copies the argument of LIST
;; parameter to STACK, and removes the duplicates
;; from STACK, uses STACK as a base for counting
;; the occurrences of each TAG in the list,
;; removing the TAGS that occur less than MIN times
;; from LIST. For TAGS that occur at least MIN times,
;; pushes a CONS to LIST that contains name of 
;; the TAG in CAR, the number of occurrences in CDR, 
;; then removes the occurrences of that TAG from LIST.
;; Runs itself recursively with (CDR STACK) and the
;; modified argument to LIST parameter.
(defun count-and-limit-tags (list &key 
		    (test #'equal) 
		    (min 5)
		    (stack (remove-duplicates 
			    list 
			    :test test)))
  (let ((count (count (car stack) list :test test)))
    (if (car stack) 
	(count-and-limit-tags
	 (if (<= min count) 
	     (remove (car stack) 
		     (push (cons (car stack) 
				 count) 
			   list) :test test) 
	     (remove (car stack) list :test test))
	 :test test
	 :min min
	 :stack (cdr stack)) 
	list)))

;;; LIST-NODES
;;; Appends nested TAG lists, Calls COUNT-AND-LIMIT-TAGS
;; with the given argument to MIN paremeter. Result is 
;; a list of (TAG . COUNT) CONSES, COUNT representing
;; the number of occurrences of associated TAG.
(defun list-nodes (tag-data &key (min 5)) 
  (count-and-limit-tags (reduce #'append 
		       tag-data) 
	       :min min))

;;; DRAW-NODES
;;; Creates the list data structure for all NODES.
;; This list is used as an input to S-DOT library,
;; a GRAPHVIZ DOT LANGUAGE wrapper library for
;; COMMON LISP. Gets the number of tweets returned
;; as the argument to TWEET-NUMBER, uses that
;; number and NUMBER OF OCCURRENCES of the NODE
;; to calculate relative FONTSIZE of each NODE.
(defun draw-nodes (node-list shape tweet-number) 
  (mapcar #'(lambda (node) 
	      `(s-dot::node ((s-dot::id ,(car node)) 
			     (s-dot::label ,(car node)) 
			     (s-dot::shape ,shape) 
			     (s-dot::style "filled") 
			     (s-dot::color "#333333")
			     (s-dot::fontname "DejaVu,Sans")
			     (s-dot::fontsize ,(write-to-string 
						(+ *minimum-font-size* (floor 
						       (* (/ (cdr node) 
							     tweet-number) 
							  *font-size-factor*)))))
			     (s-dot::fillcolor "#333333") 
			     (s-dot::fontcolor "#DDDDDD")))) 
	  node-list))

;;; DRAW-EDGES
;;; Creates the list structure for edges of a TWEET.
;; This list is used as an input to S-DOT library,
;; a GRAPHVIZ DOT LANGUAGE wrapper library for
;; COMMON LISP. Recurses over argument of EDGE,
;; Uses TAG name at CAR as value of FROM NODE,
;; Uses TAG name at CADR as value of TO NODE.
;; CONDs on (CDR EDGE) because it need two elements
;; of EDGE on each iteration.
(defun draw-edges (edge)
  (if (cdr edge)
      (cons `(s-dot::edge ((s-dot::arrowhead "none") 
			   (s-dot::from ,(car edge)) 
			   (s-dot::to ,(cadr edge))
			   (s-dot::color "#333333")))
	    (draw-edges (cdr edge)))
      nil))

;;; MAP-DRAW-EDGES
;; MAPS a LAMBDA function to every nested TAG LIST of the
;; list containing all TAGS. The function first removes 
;; the duplicates of TAGS from the given nested TAG list, 
;; intersects the list with the NODE LIST to remove TAGS
;; that are eliminated after OCCURRENCES test. Finally, 
;; Appends all nested list of edges into a single list.
(defun map-draw-edges (edge-list node-list)
  (let ((test #'string-equal))
    (reduce #'append
	    (mapcar #'(lambda (edge)
			(draw-edges 
			 (intersection (remove-duplicates edge :test test) 
				       (mapcar #'car node-list)
				       :test test)))
		    edge-list))))

;;; DRAW-DOT-GRAPH
;;; Appends created NODE and EDGE data structures 
;; with a GRAPH structure that defines the
;; seperation of NODES and RANKS, the argument
;; to RANKDIR parameter can either be "TB" which
;; means Top->Bottom, or "LR" which means Left->Right.
;; It determines the direction diagram will be placed.
(defun draw-dot-graph (nodes edges rankdir)
  (append
   `(s-dot::graph ((s-dot::rankdir ,rankdir) 
		   (s-dot::nodesep "0.05") 
		   (s-dot::ranksep "0.05")))
   nodes 
   edges))

;;; SEARCH-AND-RENDER-DIAGRAM
;;; Queries the provided KEYWORD for PAGE*100
;; results. LANG determines the preferred language
;; of tweets search returns. FILE-FORMAT is the format
;; the resulting diagram will be saved as. Defaults to
;; SVG as a proper vector graphics format alternative.
;; The argument of MIN will be passed as argument to 
;; the MIN parameter of COUNT-AND-LIMIT-TAGS function.
;; SHAPE is the shape of NODES. The argument to RANKDIR
;; parameter will be passed as argument to RANKDIR parameter
;; of DRAW-DOT-GRAPH.
(defun search-and-render-diagram (keyword &key 
			  (page 5)
			  (min 5)
			  (lang "en")
			  (file-format "svg")
			  (shape "doublecircle") 
			  (rankdir "TB"))
  (let* ((file-name (concatenate 'string 
				 (remove-if-not #'alphanumericp keyword) 
				 "." file-format)) 
	 (tags (mapcar #'(lambda (search-ref) 
			   (get-tags search-ref 
				     (char keyword 0))) 
		       (search-and-extract-tweets keyword 
						  :lang lang 
						  :page page)))
	 (node-list (list-nodes tags :min min)))
    (s-dot:render-s-dot file-name 
			file-format 
			(draw-dot-graph 
			 (draw-nodes node-list shape (* page +results-per-page+))
			 (map-draw-edges tags node-list) rankdir))))

;;; EXAMPLE USAGE OF SEARCH-AND-RENDER-DIAGRAM FUNCTION
;;; Look for the "example.svg" file in the folder you run LISP from.
;; (twitgraph:search-and-render-diagram "#example" :page 10 :min 5 :lang "en" :rankdir "LR")
