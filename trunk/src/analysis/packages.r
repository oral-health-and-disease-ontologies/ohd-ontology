install_if_necessary <- function (package)
{ if (!require(package,character.only = TRUE)) { install.packages(package) } }

install_if_necessary("rJava")
install_if_necessary("ggthemes")
install_if_necessary("Cairo")
install_if_necessary("gridExtra")

