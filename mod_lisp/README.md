# Setup mod_lisp with Apache2 on OS X Mountain Lion
All of the commands should be typed in the terminal unless stated otherwise (You can open the terminal by pressing CMD-SPACE and then typing "terminal" into spotlight).

## Fix apxs path issue (OS X Mountain Lion)

	cd /Applications/Xcode.app/Contents/Developer/Toolchains
	sudo ln -s ./XcodeDefault.xctoolchain/ ./OSX10.8.xctoolchain

## Compile and Install mod_lisp2
I have UA-3460-560-P2 checked out to /Users/USERNAME/Sites/UAkron/UA-3460-560-P2 so the commands below are what I use, you will have to navigate to the directoy where mod_lisp2.c is located

	cd
	cd Sites/UAkron/UA-3460-560-P2/mod_lisp
	sudo apxs -i -c mod_lisp2.c

## Edit Apache config to activate mod_lisp

	sudo vi /etc/apache2/httpd.conf

## Add the following lines at end of LoadModule section in httpd.conf

	LoadModule lisp_module libexec/apache2/mod_lisp2.so
	LispServer 127.0.0.1 3000 "lisp"
	<Location /lisp>
	  SetHandler lisp-handler
	</Location>



## Optional steps to add per user sites
These steps are not really required, but if you are interested in enabling per-user sites (where you can place files in the directory /Users/USERNAME/Sites and they will show up under http://localhost/~USERNAME/), then you might as well do that while you are here.

### (Optional) Know your USERNAME

	whoami

### (Optional) Add user apache config (for current user)

	sudo touch /etc/apache2/users/USERNAME.conf
	sudo pico /etc/apache2/users/USERNAME.conf

### (Optional) Add the following lines to the file (use your USERNAME) from above

	<Directory "/Users/USERNAME/Sites/">
	     Options Indexes MultiViews
	     AllowOverride All
	     Order allow,deny
	     Allow from all
	</Directory>



## Check the Apache config for errors

	sudo apachectl -t

## Load Apache

	sudo apachectl start

## Tell Apache to start automatically
If you do not do this, then everytime you restart your computer you will have to run the command from the last step.

	sudo launchctl load -w /System/Library/LaunchDaemons/org.apache.httpd.plist



## CMU Common Lisp (cmucl)
This version of lisp seems to work much better than clisp, at least for this application. As a bonus, it is also much easier to install (although I had already installed clisp beforehand, so some dependencies could have been taken care of during that process, but I cannot say for sure).

### To Install CMUCL on a Mac (Mountain Lion)
First go to [http://www.cons.org/cmucl/download.html](http://www.cons.org/cmucl/download.html) and download the appropriate version for your system.

#### Install CMUCL

	cd /opt/local
	sudo tar xjf /Users/USERNAME/Downloads/cmucl-20c-x86-darwin.tar.bz2

### Start Lisp

	lisp

### Load the sample file

	(load "modlisp-cmucl.lisp")

### Check your handywork
[http://localhost/lisp](http://localhost/lisp)



## ANSI Common Lisp (clisp)
This version of lisp does not produce consistent results. Sometimes it will serve pages and other times it will not; it seems that it does not like to serve up the same page within a certain period of time, so maybe there should be some sort of caching. Regardless, clisp was not working well with mod_lisp.

### To Install CLISP on a Mac (Mountain Lion)

#### Install Xcode
* Install Xcode from the Mac App Store
* Install command line tool from Xcode preferences
* agree to command line tools license: `sudo xcodebuild -license`

#### Install MacPorts

	http://www.macports.org/install.php

#### Install CLISP

	sudo port install clisp

### Start Clisp

	clisp

### Load the sample file

	(load 'modlisp-clisp.lisp)
	(modlisp:modlisp-server)

### Check your handywork
[http://localhost/lisp](http://localhost/lisp)