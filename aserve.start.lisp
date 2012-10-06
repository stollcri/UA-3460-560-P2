(load '/Users/stollcri/Documents/College/2012-AI/portableaserve/INSTALL.lisp)
(net.aserve:publish-directory
	:prefix "/"
	:destination "/Users/stollcri/Documents/College/2012-AI/UA-3460-560-P2/www/")
(net.aserve:start :port 8080 :listeners 0)