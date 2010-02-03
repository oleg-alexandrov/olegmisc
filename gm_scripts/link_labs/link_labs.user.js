// ==UserScript==
// @name          x
// @version        1.1
// @date           2010-01-20
// @creator        
// @namespace      x
// @include        https://www.google.com/firefox*
// ==/UserScript==

(function() {

	var docsLink = document.createElement('A');	
	var linkName = document.createTextNode('Docs ');
	docsLink.appendChild(linkName);
	docsLink.setAttribute('href','http://docs.google.com');
      var gbar = document.getElementById('gbar');
      gbar.appendChild(docsLink); 

      var elems = gbar.getElementsByClassName('gb1');
      //alert('length is ' + elems.length)

      var elem = elems[2];
      elem.parentNode.insertBefore(docsLink, elem);

      //elem.gbar.insertBefore(docsLink, elems[2]); 
      
})()


