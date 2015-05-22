//https://github.com/YASGUI/YASQE/issues/67

var checkPlainPrefixes = function(yasqe) {
        $(yasqe.getWrapperElement()).find('.cm-string-2').each(function(i, el){
	    val=el.innerText;
            if (val.charAt(val.length-1) == ":") 
	    { $(el).addClass("plainPrefix");
            } else { $(el).removeClass("plainPrefix");
            }
        })
    }
    checkPlainPrefixes(yasqe);//run for first time
    yasqe.on('change', checkPlainPrefixes);
