# # Install packages

# # Install packages from CRAN
# install.packages(c(
#     "cowplot",
#     "ggrepel",
#     "here",
#     "janitor",
#     "kableExtra",
#     "knitr",
#     "logitr",
#     "remotes",
#     "rmarkdown",
#     "tidyverse",
#     "viridis",
# ))

# # Install development packages from GitHub
# remotes::install_github("jhelvy/logitr")
# remotes::install_github("jhelvy/conjointTools")

# Load functions and libraries
library(cowplot)
library(conjointTools)
library(ggrepel)
library(here)
library(janitor)
library(kableExtra)
library(knitr)
library(logitr)
library(remotes)
library(rmarkdown)
library(tidyverse)
library(viridis)

# Set option to preview all columns in a data.frame or tibble:
options(dplyr.width = Inf)
