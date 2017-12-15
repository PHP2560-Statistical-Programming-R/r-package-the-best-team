#Install the necessary packages this program needs.
#Not install if they are already installed.

install_packages = function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org")
    
    library(name, character.only=TRUE)
  }
}

install_packages(c("stringr","ggplot2","Cairo","ggmap","grid","scales","png","jpeg"))