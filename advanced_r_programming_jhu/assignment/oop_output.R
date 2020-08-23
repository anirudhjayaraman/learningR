# Saving code output / results to a text file =================================
sink("./advanced_r_programming_jhu/assignment/oop_output.txt", append = FALSE)

## Read in the data
library(readr, quietly = TRUE)
library(magrittr, quietly = TRUE)

source("./advanced_r_programming_jhu/assignment/oop_code.R")

## Load any other packages that you may need to execute your code
library(tidyverse, quietly = TRUE)

data <- read_csv(file = "./advanced_r_programming_jhu/assignment/data/MIE.csv")


x <- make_LD(data)
print(class(x))
cat("\n", "\n")

print(x)
cat("\n", "\n")
## Subject 10 doesn't exist

out <- subject(x, 10)
print(out)
cat("\n", "\n")

out <- subject(x, 14)
print(out)
cat("\n", "\n")

out <- subject(x, 54) %>% summary
print(out)
cat("\n", "\n")

out <- subject(x, 14) %>% summary
print(out)
cat("\n", "\n")

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)
cat("\n", "\n")

## Show a summary of the pollutant values
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)
cat("\n", "\n")

out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)
cat("\n", "\n")

cat("\n# That's all folks -------------------------------------------------\n")

sink()

###############################################################################
