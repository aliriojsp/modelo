my_packages = c("ggplot2","dplyr","shinythemes","flexdashboard","fastDummies","lubridate","DT","htmlwidgets","sortable","magrittr")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
