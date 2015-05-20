library(pryr)

# The caller calls this function on a set of strings that act as a sparql template
# The function gets the arglist it was called with, which may have replacements for any sparql variable
# A replacement is specified as a named argument.
#  e.g. proci="?procedure" which would substitute ?procedure for ?proci in the template.

# In addition, blank nodes are expected to be unique to the
# pattern. So we replace those with ones with a counter to make sure
# they are.

# triple-block: Used to group together a series of triples in a query
# so they can be passed, e.g. to sparql_union

tb <- function(...) { paste(...,"\n",sep="\n")}

sparql_interpolate <- function(...) 
    {
        #cat("spi:",...,"\n");
        sbgp<-paste(...,"",sep="\n")
        call<-sys.call(sys.parent(1));
        argnames1 <- names(call);
        argnames <- argnames1[argnames1!=""]
        env <- sys.frame(sys.parent(2))
        values <- unlist(lapply(argnames,FUN=function(name) { eval(call[[name]],env)}))
        potential <- cbind(argnames,values)
        firstchar = substr(values,start=1,stop=1);
        whichones<- (firstchar=="?"|| firstchar=='"' || firstchar=="<" || substr(values,start=nchar(values),stop=nchar(values))==":");
        if (length(argnames)==0) { return(sbgp) };
        actual<- potential[whichones,]
        substitute_sparql_variable <- 
            function(old,replacement)
                { if (substr(replacement,start=1,stop=1)=="?")
                      { pattern <- paste0("(\\?",old,")\\b") }
                  else if (substring(replacement,1,1)=="<")
                      { pattern <- paste0("<",old,">") }
                  else if (substring(replacement,1,1)=='"')
                      { pattern <- paste0("'",old,"'") }
                  else { pattern <- paste0("(\\b",old,"[:])") };
                  sbgp <- gsub(pattern,replacement,sbgp,perl=TRUE)
                  sbgp <<- sbgp
              }
                if (length(argnames) == 1)
            { substitute_sparql_variable(argnames,values) }
        else
            { uu <<- actual;
              mapply(substitute_sparql_variable,actual[,"argnames"],actual[,"values"]) }
        make_blanks_unique(sbgp);
    }

make_blanks_unique <- function (bgp)
    { to_be_replaced<-unique(regmatches(bgp,gregexpr("(?s)_\\:([A-Za-z0-9_]*)",bgp,perl=TRUE))[[1]]);
      to_be_replaced<-rev(to_be_replaced[order(nchar(to_be_replaced), to_be_replaced)])
      count<-gensymcounter;
      gensymcounter <<- gensymcounter + 1
      replace_w_numbered <- 
          function(string)
              {replacement <- paste0(string,count);
               bgp<<- gsub(string,replacement,bgp,perl=TRUE)
           }
      lapply(to_be_replaced,replace_w_numbered)
      bgp}      

gensymcounter <- 1;
reset_var_counter <- function()
{
  gensymcounter <<- 1;
}

genvar <- function(base)
  { var <- paste("?",base,gensymcounter,sep="");
    gensymcounter <<- gensymcounter+1;
    var
  }

sparql_union <- function(...)
{   paste0("{{",paste(...,sep="} UNION {"),"}}")
}

