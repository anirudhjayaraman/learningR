# illustrating polymorphism in R using summary
diamonds <- ggplot2::diamonds
summary(diamonds$carat)
summary(diamonds$color)

# sloop: think sailing the seas of OOP
library(sloop)
otype(1:10) # base
otype(diamonds) # S3
otype(ts(1:19)) # S3
mle_obj <- stats4::mle(function(x = 1) (x - 2) ^ 2)
otype(mle_obj) # S4

##---------------------- Chapter 12 - Base Types ---------------------

# a base object
is.object(1:10) # FALSE
sloop::otype(1:10) # base

# an OO object
is.object(diamonds) # TRUE
sloop::otype(diamonds) # S3

# the difference between base and OO objects (class attribute)
attr(1:10,"class")
attr(diamonds,"class")

# the class() function yields misleading results with base types
# OK to use it to S3 and S4 objects
class(1:10) # integer
sloop::otype(1:10) # base
sloop::s3_class(1:10) # integer numeric
class(diamonds) # tbl_df tbl data.frame

# more on the Numeric type
# In the S3 and S4 systems, numeric is used as a shorthand 
# for either integer or double type
sloop::s3_class(1L) # "integer" "numeric"
class(1L) # integer
sloop::s3_class(1) # "double"  "numeric"
class(1) # numeric

# is.numeric() checks for objects behaving like numbers
typeof(factor("1")) # integer
is.numeric(typeof(factor("1"))) # FALSE

##---------------------- Chapter 13 - S3 -----------------------------


