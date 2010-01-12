// ==UserScript==
// @name           Strip Digg Announcement
// @namespace      Digg
// @description    Strip Digg Announcement
// @include        http://digg.com/
// ==/UserScript==

	
(function() {

	document.getElementById('game').style["display"] = "none";

	document.getElementById('game').style["display"] = "none";

	document.getElementById('user-announcement').style["display"] = "none";

var adBar = document.getElementById('user-announcement');

if (adBar)
	adBar.parentNode.removeChild(adBar);

})()


