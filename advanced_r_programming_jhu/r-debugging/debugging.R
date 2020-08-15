check_n_value <- function(n) {
  if(n > 0) {
    stop("n should be <= 0")
  }
}

check_n_value_alt <- function(n){
  if(n > 0){
    browser()
    stop("n should be <= 0")
  }
}


error_if_n_is_greater_than_zero <- function(n){
  check_n_value(n)
  n
}

error_if_n_is_greater_than_zero_alt <- function(n){
  check_n_value_alt(n)
  return(n)
}


error_if_n_is_greater_than_zero_alt_alt <- function(n){
  return(tryCatch(check_n_value(n), 
                  error = function(e) NA))
}

error_if_n_is_greater_than_zero(5)
error_if_n_is_greater_than_zero_alt(2)
error_if_n_is_greater_than_zero_alt_alt(4)


# try() -------------------------------------------------------------------


for (i in 1:10) {
  1 <- 1
}

for (i in 1:10) {
  try(1 <- 1, silent = T)
}


# trace() -----------------------------------------------------------------

trace("check_n_value")

for (i in 1:5) {
  if (i %% 2 == 0) {
    error_if_n_is_greater_than_zero(1)
  } else {
    error_if_n_is_greater_than_zero(-2)
  }
}

# The trace() function has a side effect of modifying the function and 
# converting into a new object of class “functionWithTrace”

class(check_n_value)


untrace("check_n_value")

for (i in 1:5) {
  if (i %% 2 == 0) {
    error_if_n_is_greater_than_zero(1)
  } else {
    error_if_n_is_greater_than_zero(-2)
  }
}


# as.list(body(function_name)) --------------------------------------------

as.list(body(error_if_n_is_greater_than_zero_alt_alt))
as.list(body(check_n_value))
as.list(body(check_n_value)[[2]])
as.list(body(body))
as.list(body(body)[[2]])


# debug -------------------------------------------------------------------

datset <- datasets::airquality

debug(lm)

modl <- lm(formula = Ozone ~ `Solar.R` + Wind + Temp + Month, data = datset)

undebug(lm)

modl <- lm(formula = Ozone ~ `Solar.R` + Wind + Temp + Month, data = datset)
summary(modl)



# recover() ---------------------------------------------------------------

options(error = utils::recover)
error_if_n_is_greater_than_zero(6)
