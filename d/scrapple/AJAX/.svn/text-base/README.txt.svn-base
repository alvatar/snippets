**** DISTRIBUTED WITHOUT WARRANTY OF ANY KIND ****

Bottled AJAX

Written by Benjamin Shropshire



This template library generates a JavaScript object declaration that
proxies for a D object on the server and a D function that is used
on the server to carry out the proxy operations.

see echo.d for an example of it's use.

To use this program you will need "scrapple.cgi.cgi" It's imported as
"cgi.cgi" so you might need to tweak some paths or package names.

On my system I build and deploy like this

dmd echo.d bottledAJAX.d cgi/cgi.d
install -gapache -oapache -m644 u.html /var/www/html/
install -gapache -oapache -m644 request.js /var/www/html/scripts/
install -gapache -oapache -m755 echo /var/www/cgi-bin/

if any of the paths change or the name of the executable changes you
will need to modify u.html and/or the build command.

