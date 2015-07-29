get_sesame_repositories <-function(root="http://127.0.0.1:8080")
    { result = "No local repositories";
      result = tryCatch(
          { read.csv(textConnection(getURL(paste0(root,"/openrdf-sesame/repositories"), httpheader = c(Accept = "text/csv"))))}, 
          error = function(e) {}, 
          finally = {}
          )
      return(if(is.null(result)) {"No local repositories"} else {result} );
  }

set_sesame_repository <-function(index)
    { suppressWarnings(if (!(get_sesame_repositories()[1] == "No local repositories"))
          {current_endpoint <<- as.character(get_sesame_repositories()[index,"uri"]);
           cat(paste0("current_endpoint set to ",current_endpoint,"\n"))})
  }

setRepositoryPrefixes <- function()
  { DELETE(paste0(current_endpoint,"/namespaces"));
    lapply(ls(prefixes),function(p) {PUT(paste0(current_endpoint,"/namespaces/",gsub(":","",p)),body=gsub("<|>","",get(p,prefixes)))});
    NULL
  }
