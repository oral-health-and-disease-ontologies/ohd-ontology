//https://gist.github.com/LaurensRietveld/7d8beca5dbeb6b909b6f
// var yasqe = YASQE(document.getElementById("yasqe"), {
// 	sparql: {
// 	    showQueryButton: true,
// 	    endpoint: "http://127.0.0.1:8080/openrdf-sesame/repositories/OHDRL20150416"
// 	}
// });
// var yasr = YASR(document.getElementById("yasr"), {
// 	//this way, the URLs in the results are prettified using the defined prefixes in the query
// 	getUsedPrefixes: yasqe.getPrefixesFromQuery
// });

//https://gist.github.com/LaurensRietveld/7d4ccee95a0d45e95865

YASR=YASGUI.YASR;
YASQE=YASGUI.YASQE;
YASR.plugins.table.defaults.fetchTitlesFromPreflabel=false;
YASR.plugins.table.defaults.mergeLabelsWithUris=true;
YASR.plugins.table.defaults.useGoogleCharts=false;

//document.getElementsByClassName("yasqe_buttons").item(0).appendChild(document.getElementsByClassName("select_rawResponse").item(0))
//document.getElementsByClassName("yasqe_buttons").item(0).appendChild(document.getElementsByClassName("select_table").item(0))
//document.getElementsByClassName("yasqe_buttons").item(0).appendChild(document.getElementsByClassName("yasr_downloadIcon").item(0))
//document.getElementsByClassName("yasr_downloadIcon").item(0).setWidth(40)


//YASQE.defaults.sparql.endpoint="http://127.0.0.1:8080/openrdf-sesame/repositories/OHDRL20150416"
YASQE.defaults.collapsePrefixesOnLoad = true

var sheet = (function() {
	// Create the <style> tag
	var style = document.createElement("style");

	// Add a media (and/or media query) here if you'd like!
	// style.setAttribute("media", "screen")
	// style.setAttribute("media", "only screen and (max-width : 1024px)")

	// WebKit hack :(
	style.appendChild(document.createTextNode(""));

	// Add the <style> element to the page
	document.head.appendChild(style);

	return style.sheet;
})();

sheet.insertRule(".cm-string-2 {color: #339933!important;}",0)
sheet.insertRule(".cm-keyword {color: #8F0000!important;}",0)
sheet.insertRule(".matchingVar {background-color:#FFAAAA!important; border-radius: 6px;transition: background 1s linear;}",0)
sheet.insertRule(".CodeMirror-hint {font-family:Arial!important;font-size:12px}",0)
sheet.insertRule(".yasgui sup {display:none;}",0);
sheet.insertRule(".yasgui .CodeMirror { position:left!important;font-size:11px!important;line-height:13px!important;font-family:Source Code Pro!important}", 0);
sheet.insertRule(".yasr table.dataTable tr {line-height:1px}",0)
sheet.insertRule(".yasr table.dataTable td { overflow:hidden; text-overflow: ellipsis;white-space: nowrap}",0)
sheet.insertRule(".yasr table.dataTable th sorting {width:auto!important}",0)
sheet.insertRule(".yasr table.dataTable thead tr th {width:auto!important;}",0)
sheet.insertRule(".yasr table.dataTable {width:auto!important;margin-left:0}",0)
sheet.insertRule(".yasgui .controlbar .endpointText {font-size:13px;width:500px!important}",0)
sheet.insertRule(".yasguiLogo {display:none;}",0)
sheet.insertRule(".yasr_header {display:none;}",0)
sheet.insertRule(".yasgui {font-size:11px;}",0)
sheet.insertRule(".yasgui .nav>li>a { padding: 2px }",0)
sheet.insertRule(".yasgui .nav-tabs li a[role=addTab] {margin-top:0px;border-top:0;line-height:15px}",0)


//;****************************************************************


// codemirror=document.getElementsByClassName("CodeMirror").item(0)
// codemirror.style.fontSize="12px"
// codemirror.style.fontFamily="Source Code Pro"
// codemirror.style.lineHeight="14px"
// only works once - new table each query
//table = document.getElementsByClassName("resultsTable").item(0)
//table.className=table.classname+" compact"

// http://davidwalsh.name/add-rules-stylesheets 

//document.getElementsByClassName("dataTables_wrapper").item(0).style.className

var yasgui = YASGUI(document.getElementById("yasgui"),{sparql: {
    showQueryButton: true,

    endpoint: "http://127.0.0.1:8080/openrdf-sesame/repositories/OHDRL20150416"
}})

YASR.defaults.useGoogleCharts = false


//http://www.w3schools.com/jsref/dom_obj_style.asp
//https://codemirror.net/doc/manual.html#styling
yasqe=yasgui.current().yasqe
//yasqe.options.sparql.endpoint="http://127.0.0.1:8080/openrdf-sesame/repositories/OHDRL20150416"
yasqe.setSize(null,"auto")
