# 26. Practical: Web Programming with AllegroServe
For project 2 we selected this topic to better familiarize ourselves with Lisp. We followed the directions on the [Gigamonkeys website](http://www.gigamonkeys.com/book/practical-web-programming-with-allegroserve.html) to get AllegroServe installed, but it did not run properly (at least not with CLISP on OS X Mountain Lion). The files /portableaserve/aserve/main.cl and /portableaserve/aserve/log.cl were changed so that AllegroServe would start without reporting errors, but it still would not serve up the example web page (this is documented under ./aserve/README.md). After struggling with this for some time we decided to swtich to mod_lisp for Apache. The combination of Apache, mod_lisp, and CLISP worked, but it did not run reliably on Chris's laptop. We installed the CMUCL version of lisp and it performed well for Chris. Oddly, Patrick had the opposite results; CLISP worked well with Apache and mod_lisp while CMUCL did not. It may be related to threading, CLISP seems to be designed for interactive use (running the mod_lisp script consumes the interactive session) while CMUCL seems to be designed to for use with an external editor (the mod_lisp script runs in the background and the backsace and arrow keys do not work properly in interactive mode).

Patrick was able to get AllegroServe to work, but it required installing the Allegro version of Common Lisp.

## Project Files
* [AllegroServe Notes](./aserve/README.md)
* [Apache + mod_lisp](./mod_lisp/README.md)
* [Lisp Web Data](./www/index.lisp)