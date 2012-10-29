I managed to get this server running on my Mac running Mountain Lion, if you are using a different OS then you might have slightly different issues. But, AllegroServe is a big piece of junk, it crashes A LOT.

/portableaserve/aserve/main.cl
Change line 217 to: (defun getpid () (sys::process-id))
/portableaserve/aserve/log.cl
Change line 129 to: ()

I run aserve using the command: clisp startas.lisp
And startas.lisp contains the following lines:
(load '/Users/USERNAME/Documents/College/2012-AI/portableaserve/INSTALL.lisp)
(net.aserve:publish-directory
	:prefix "/"
	:destination "/Users/USERNAME/Documents/College/2012-AI/UA-3460-560-P2/www/")
(net.aserve:start :port 8080 :listeners 0)