# Add user apache config (for current user)

	whoami # gives your USERNAME
	sudo touch /etc/apache2/users/USERNAME.conf
	sudo pico /etc/apache2/users/USERNAME.conf

# Add the following lines ot the file (user your USERNAME)

	<Directory "/Users/USERNAME/Sites/">
	     Options Indexes MultiViews
	     AllowOverride All
	     Order allow,deny
	     Allow from all
	</Directory>

# Fix apxs path issue (Mountain Lion)

	cd /Applications/Xcode.app/Contents/Developer/Toolchains
	sudo ln -s ./XcodeDefault.xctoolchain/ ./OSX10.8.xctoolchain

# Install mod_lisp2

	sudo apxs -i -c mod_lisp2.c

# Load Apache

	sudo apachectl start

# Tell Apache to start automatically

	sudo launchctl load -w /System/Library/LaunchDaemons/org.apache.httpd.plist

# Add a web project directory and add files here

	mkdir /Users/USERNAME/Sites/lisp

# View the files in a web browser

	http://localhost/~USERNAME/lisp/FILENAME