// ==UserScript==
// @name          Clean Slashdot
// @include       http://*digg.com/*
// @exclude       
// ==/UserScript==
	
(function() {

       var adIDAry = ['firehose-message-tray'],
               classNameAry = [],
               tempEle = null,
               tempEles = null;

       // remove known ad ids
       for (var i = 0; i < adIDAry.length; i++) {
               tempEle = document.getElementById(adIDAry[i]);
               if (tempEle) tempEle.parentNode.removeChild(tempEle);
       }
       // remove known ad class names
       for (var i = 0; i < classNameAry.length; i++) {
               tempEles = document.getElementsByClassName(classNameAry[i]);
               for (var j = 0; j < tempEles.length; j++) {
                       tempEles[j].parentNode.removeChild(tempEles[j]);
               }
       }

	//document.getElementById('contents').style["display"] = "none"; // for testing

})()

