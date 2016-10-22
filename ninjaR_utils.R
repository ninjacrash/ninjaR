packagedownloader <- function(requirementsfile) {
  # Function to download packages in r-requirements.txt
  requirements <- read.table(requirementsfile, header = T, sep = ",")
  packagelist <- c(names(requirements))
  new.packages <- packagelist[!(packagelist %in% installed.packages()[, "Package"])]
  if (length(new.packages)) {
    install.packages(new.packages, dependencies = TRUE, repos = "http://cran.us.r-project.org")
  }
  sapply(packagelist, require, character.only = TRUE)
}

# Store all scripts here
scripts <- c("ninjaR_driver.R",
             "generate_data.R")

repo_sourcer <- function(){
  # Sources all R scripts in repo
  for(s in scripts){
    source(s)
    print(paste("Sourced",s))
  }  
}