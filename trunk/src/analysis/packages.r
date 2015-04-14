
install_if_necessary <- function (package,from=NULL)
{ if (!require(package,character.only = TRUE)) 
    { if (is.Null(from))
        { install.packages(package); require(package) }
    else
      { install.packages(c((paste(getwd(),"/",from,sep="")),repos=NULL),type="source") }
    }
}

install_if_necessary("rJava")
install_if_necessary("rrdflibs","rrdflibs_1.3.0.tar.gz")
install_if_necessary("rrdf","rrdf_2.0.2.tar.gz")
install_if_necessary("ggthemes")
install_if_necessary("Cairo")
install_if_necessary("gridExtra")

