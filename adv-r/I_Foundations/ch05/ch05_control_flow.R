# https://adv-r.hadley.nz/control-flow.html


## Intro -------------------------------------------------------------
x <- TRUE
y <- NA
class(y) # logical
z <- if (x) 3; z # 3
z <- if(!x) 3; z # NULL
# z <- if(y) 3; z # Error in if (y) 3 : missing value where TRUE/FALSE needed/

switch("x", x = , y = 2, z = 3)

## Choices ------------------------------------------------------------

greet <- function(name, birthday = FALSE) {
  paste0("Hi ", name,if (birthday) " and HAPPY BIRTHDAY")
}
greet("Maria") # "Hi Maria"
greet("Jaime", T) # "Hi Jaime and HAPPY BIRTHDAY"

# Invalid Inputs ------------------------------------------------------
if (c(TRUE, FALSE)) 1
# Warning message:
#   In if (c(TRUE, FALSE)) 1 :
#   the condition has length > 1 and only the first element will be used

Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
if (c(TRUE, FALSE)) 1
#> Error in if (c(TRUE, FALSE)) 1: the condition has length > 1


# Vectorized If -------------------------------------------------------
x <- 1:10
ifelse(x %% 5 == 0, "XXX", as.character(x))
#>  [1] "1"   "2"   "3"   "4"   "XXX" "6"   "7"   "8"   "9"   "XXX"

ifelse(x %% 2 == 0, "even", "odd")
#>  [1] "odd"  "even" "odd"  "even" "odd"  "even" "odd"  "even" "odd"  "even"

dplyr::case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  is.na(x) ~ "???",
  TRUE ~ as.character(x)
)
#>  [1] "1"    "2"    "3"    "4"    "fizz" "6"    "buzz" "8"    "9"    "fizz"

# switch() ------------------------------------------------------------
x_option <- function(x) {
  if (x == "a") {
    "option 1"
  } else if (x == "b") {
    "option 2" 
  } else if (x == "c") {
    "option 3"
  } else {
    stop("Invalid `x` value")
  }
}

x_option("b")

# with a switch statement:
x_option <- function(x){
  switch(x, 
         a )
}


