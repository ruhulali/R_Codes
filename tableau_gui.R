# From CRAN
install.packages("esquisse", dependencies = T)

# with remotes
remotes::install_github("dreamRs/esquisse")

# or with install-github.me service (based on remotes)
source("https://install-github.me/dreamRs/esquisse")

# or with devtools:
devtools::install_github("dreamRs/esquisse")



#loading tidyverse to read input
library(tidyverse)

# loading itunesr for retrieving itunes review data that we will use in this analysis
install.packages('itunesr', dependencies = T)
library(itunesr)

#loading the magical esquisse library
library(esquisse)
esquisse::esquisser(data = tips)

