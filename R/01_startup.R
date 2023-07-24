#### 01 STARTUP ################################################################

# There is usually no need to run this script directly; it is sourced from the
# other scripts which need it.


# Optionally install packages from GitHub ---------------------------------

# remotes::install_github("UPGo-McGill/upgo")
# remotes::install_github("UPGo-McGill/strr")
# remotes::install_github("UPGo-McGill/matchr")


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(upgo)
library(strr)
library(sf)
library(future)
library(progressr)
library(slider)
library(data.table)
library(gt)
library(patchwork)
library(qs)
library(tsibble)
# remotes::install_github("liamgilbey/ggwaffle")
library(ggwaffle)


# Set global variables ----------------------------------------------------

plan(multisession)
handlers(global = TRUE)
col_palette <-
  c("#14325C", "#5398D9", "#F4E3B1", "#D96B0C", "#A53A3B", "#A2E8AF", "#C7C9C9")

