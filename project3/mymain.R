################ Load Environment ##################
# clean workspace
#rm(list = ls())

# load necessary packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "xgboost",
  "kernlab"
)

train.file = "train1.csv"
test.file = "test1.csv"
label.file = "label1.csv"

train.data = read.csv(train.file)
test.data = read.csv(test.file)
label.data = read.csv(label.file)


