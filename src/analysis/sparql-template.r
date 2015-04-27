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
       # construct the pattern string - a basic graph pattern
       bgp=paste(...,"",sep="\n")
       # get the call to the pattern invoker
       call<-sys.call(sys.parent(1)); 
       # extract the arguments
       argnames <- names(call);
       argnames <- argnames[argnames!=""]
       # get its environment so we can evaluate the arguments
       env <- sys.frame(sys.parent(2))
       # and do so
       values <- lapply(argnames,FUN=function(name) { eval(call[[name]],env)})
       # all arguments are potential replacements until we remove the
       # ones that don't have values starting with "?"
       potential <- cbind(argnames,values)
       # which values start with "?"
       which<- (substr(values,start=1,stop=1)=="?")
       if (length(which)==0) { return(bgp) }
       # remove the others
       actual<- as.matrix(potential[which,])
       # Now substitute the values into the bgp
       #
       # I hate R. If there are two arguments the columns are
       # arguments. If there is one it is a rowname
       if (dim(actual)[2] == 1)
         { pattern <- paste0("(\\?",actual[1],")\\b")
           bgp<-gsub(pattern,actual[2],bgp,perl=TRUE)
           }
       else
           { substitute_sparql_variables <- 
               function(old,replacement)
                   {   pattern <- paste0("(\\?",old,")\\b")
                       bgp<<- gsub(pattern,replacement,bgp,perl=TRUE)
                   }
             mapply(substitute_sparql_variables,actual[,"argnames"],actual[,"values"])
             bgp}
       make_blanks_unique(bgp);
   }


make_blanks_unique <- function (bgp)
    { to_be_replaced<-unique(regmatches(bgp,gregexpr("(?s)_\\:([A-Za-z0-9_]*)",bgp,perl=TRUE))[[1]]);
      to_be_replaced<-rev(to_be_replaced[order(nchar(to_be_replaced), to_be_replaced)])
      count<-gensymcounter;
      gensymcounter <<- gensymcounter + 1
      { replace_w_numbered <- 
               function(string)
                   {replacement <- paste0(string,count)
                    #cat("before->",bgp,",",replacement,",",string,"\n")
                    bgp<<- gsub(string,replacement,bgp,perl=TRUE)
                    #cat("after->",bgp,",",replacement,"\n")
                }
        lapply(to_be_replaced,replace_w_numbered)
        bgp}      
    }

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

