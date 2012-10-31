# Setup AllegroServe

### Install Allegro CL
Put Gtk.framework in /Library/Frameworks. I've just been running alisp from its directory.
	
### Start alisp
In the AllegroCl directory:

	./alisp
	
### Load AllegroServe

	:ld src/aserve/load.cl
	
### Start the server

	(net.aserve:start :port 8000)
	
### Check if it worked

[localhost:8000](localhost:8000)
	
### Publish a file
The :path is what will be in the URL. The :file is the full path of the file to be published.

	(net.aserve:publish-file :path "/test.html" :file "/Users/USERNAME/Sites/UA-3460-560-P2/Allegro/test.html")
	
### Check if it worked

[localhost:8000/test.html](localhost:8000/test.html)
	
### Publish a directory
The :prefix will be replaced by the :destination. If index.html is in the destination directory, just add /index.html to the URL to get to it.

	(net.aserve:publish-directory :prefix "/" :destination "/Users/USERNAME/Sites/UA-3460-560-P2/Allegro/testDir/")
	
### Check if it worked

[localhost:8000/index.html](localhost:8000/index.html)
	
### Remove a file/directory

	(net.aserve:publish-file :path "/test.html" :remove t)
	(net.aserve:publish-directory :prefix "/" :remove t)
	
### Stop the server

	(net.aserve:shutdown)