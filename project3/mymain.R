################ Load Environment ##################
# clean workspace
rm(list = ls())

# load necessary packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "forecast"
)

