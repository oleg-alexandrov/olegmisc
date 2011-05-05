// ==UserScript==
// @name           EETimes
// @namespace      local
// @description    Wipe user comments embedded in the middle of article
// @include        http://*.eetimes.com*
// ==/UserScript==

	
(function() {

var adIDAry = ['NNA_Comment'],
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

//for (i=0; i<document.getElementById('toolsHome').childNodes.length; i++){
// alert("Child is " + document.getElementById('toolsHome').childNodes[i].nodeName);
//}

//tempEle = document.getElementById('toolsHome');
//var items = tempEle.getElementsByTagName("a");
//var item = items[0];
//item.parentNode.removeChild(item);

// document.getElementById('toolsHome').getElementsByTagName("a")[0].text.replace('o', 'e');
// document.getElementById('toolsHome').getElementsByTagName("a")[0].setAttribute('text', 'hi')
// document.getElementById('toolsHome').getElementsByTagName("a")[0].href = "xuxa";
 
// alert( document.getElementById('toolsHome').getElementsByTagName("a")[0].href )


//alert("The number of items is " + items.length + " item name is " + item.text);

// if(tempEle.value != "")
//	alert("You entered: " + tempEle.firstChild.nodeValue);

})()



