// ==UserScript==
// @name          Digg3 Ad Remover
// @namespace     http://www.waxydesign.com
// @description   Removes the ads on Digg 3.0
// @include       http://*digg.com/*
// @exclude       
// ==/UserScript==
	
(function() {

       var adIDAry = ['user-announcement', 'ad hdr', 'advertisement','ad','sponsor-banner','div#topads','topads','block_ad_msft'],
               classNameAry = ['selector', 'advertisement','ad','sponsored','ad-list','promo'],
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

	document.getElementById('game').style["display"] = "none";
	document.getElementById('userNotify').style["display"] = "none";
	document.getElementById('ad hdr').style["display"] = "none";
	document.getElementById('game-clock').style["display"] = "none";
	document.getElementById('game-content').style["display"] = "none";
	document.getElementById('game-diggers').style["display"] = "none";
	document.getElementById('user-announcement').style["display"] = "none";

	document.getElementById('top_ad').style["display"] = "none";
	document.getElementById('item_ad').style["display"] = "none";
	document.getElementById('comments_ad').style["display"] = "none";
	document.getElementById('col-promo').style["display"] = "none";
      document.getElementById('div#topads').style.display="none";
      document.getElementById('col-promo').style.display="none";

var adBar = document.getElementById('col-promo');

if (adBar)
	adBar.parentNode.removeChild(adBar);

})()

