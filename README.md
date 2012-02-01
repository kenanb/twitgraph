TWITGRAPH
---------

Queries a hashtag/mention in Twitter and renders a network graph of resulting hashtags/mentions via Graphviz.

Check [my blog](http://kenanb.wordpress.com) for [examples](http://kenanb.wordpress.com/2012/01/22/twitter-diagrams/) and [more info](http://kenanb.wordpress.com/2012/02/01/twitgraph-at-github/).


Usage:

(twitgraph:search-and-render-diagram "#example" 
				     :page 10 
			       	     :min 5 
				     :lang "en" 
				     :rankdir "LR")

Look for the "example.svg" file in the folder you run LISP from.

 - All parameters except query-string are optional. Check definition for other optional parameters.
 - Change the values of *minimum-font-size* and *font-size-factor* parameters if you encounter proportional problems with the diagram.