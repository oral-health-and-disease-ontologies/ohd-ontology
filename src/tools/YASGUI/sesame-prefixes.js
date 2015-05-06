YASGUI.YASQE.defaults.sparql.callbacks.success =  function(data){console.log("success", data);};

/* Adapted from https://gist.github.com/LaurensRietveld/3549c02f5727346ae89c#file-init-js to get 
   prefixes from a sesame endpoint */

/**
 * We use most of the default settings for the property and class autocompletion types. This includes:
 * -  the pre/post processing of tokens
 * -  detecting whether we are in a valid autocompletion position
 * -  caching of the suggestion list. These are cached for a period of a month on the client side.
 */

var getAutocompletionsArrayFromSesameCsv = function(csvString) {
    var completionsArray = [];
    csvString.split("\n").splice(1).forEach(function(url) {//remove first line, as this one contains the projection variable
	url=url.replace("\r","");
	    headerprefix = url.split(",")
	    completestring= headerprefix[0] + ": <" + headerprefix[1] + ">";
	completionsArray.push(completestring);
	});
    sorted=completionsArray.sort();
    minusfirst=sorted.splice(1);
    return minusfirst;
}

var customPrefixCompleter = function(yasqe) {
    //we use several functions from the regular prefix autocompleter (this way, we don't have to re-define code such as determining whether we are in a valid autocompletion position)
    yasqe.on("change", function() {
	YASGUI.YASQE.Autocompleters.prefixes.appendPrefixIfNeeded(yasqe, 'customPrefixCompleter');
    });
    var returnObj = {
	isValidCompletionPosition: function(){var token = yasqe.getCompleteToken();
					      return(
						  token.string.length > 2 && (
					      YASGUI.YASQE.Autocompleters.prefixes.isValidCompletionPosition(yasqe) ||
							  //YASGUI.YASQE.Autocompleters.classes.isValidCompletionPosition(yasqe) ||
							  //YASGUI.YASQE.Autocompleters.properties.isValidCompletionPosition(yasqe) ||
							  YASGUI.YASQE.Autocompleters.variables(yasqe)))},

	//if (token.string.length < 3 ) 
					      // 	  return false;
					      // return true}, //

	
	preProcessToken: function(token) {return YASGUI.YASQE.Autocompleters.prefixes.preprocessPrefixTokenForCompletion(yasqe, token)},
	appendPrefixIfNeeded: function(yasqe, completerName) {return YASGUI.YASQE.Autocompleters.prefixes.appendPrefixIfNeeded(yasqe,completerName)} }
    
    //In this case we assume the properties will fit in memory. So, turn on bulk loading, which will make autocompleting a lot faster
    returnObj.bulk = true;
    returnObj.async = true;
    
    //and, as everything is in memory, enable autoShowing the completions
    returnObj.autoShow = true;
    
    returnObj.persistent = "customPrefixes";//this will store the sparql results in the client-cache for a month. 
    returnObj.get = function(token, callback) {
	$.ajax({
	    data: {},
	    url: "http://127.0.0.1:8080/openrdf-sesame/repositories/OHDRL20150416/namespaces",
	    headers: {Accept: "text/csv"},//ask for csv. Simple, and uses less bandwidth
	    success: function(data) {
		callback(getAutocompletionsArrayFromSesameCsv(data));
	    }
	});
    }
    return returnObj;
}
    
//now register our new autocompleter
YASGUI.YASQE.registerAutocompleter('customPrefixCompleter', customPrefixCompleter);
YASGUI.YASQE.defaults.autocompleters=['customPrefixCompleter']

