doc:
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --dest-name index.html 3n+1.scrbl


blog:
	raco scriblogify -p the-racket-blog 3n+1.scrbl
