###############################################################################
### S3 OOP                                                                  ###
###############################################################################
# Class Assignments with structure() ------------------------------------------
special_1 <- structure(1, class = "special_number")
class(special_1)

print(special_1 + 3)
class(special_1 + 3)


# Or this way -----------------------------------------------------------------

special_2 <- 2
class(special_2) <- "special_number"

val <- special_1 + special_2
class(val)
print(val)


# create a constructor which returns an S3 object -----------------------------

shape_S3 <- function(x){
  structure(list(side_lengths = x), class = "shape_S3")
}

triangle_3 <- shape_S3(x = 3:1)
print(triangle_3)
class(triangle_3)

square_4 <- shape_S3(rep(x = 5,4))
class(square_4)
print(square_4)


# Generic Methods ------------------------------------------------------------

is_square <- function(x) UseMethod("is_square")

is_square.default <- function(x) NA

is_square.shape_S3 <- function(x){
  return(length(x$side_lengths) == 4 & 
           (max(x$side_lengths) == min(x$side_lengths)))
}

is_square(shape_S3(x = c(1:4)))
is_square(shape_S3(x = c(rep(10,4))))
is_square(triangle_3)
is_square(square_4)
is_square()


methods(print)
methods(is_square)


print.shape_S3 <- function(x){
  if (length(x$side_lengths)==1) {
    return(paste0("line of length ", x$side_lengths))
  }
  
  else if (length(x$side_lengths) == 3) {
    if (max(x$side_lengths) == min(x$side_lengths)) {
      return(paste("an equilateral triangle of side", x$side_lengths[1]))
    } else {
      return(paste("a triangle with sides", 
                    x$side_lengths[1],
                    x$side_lengths[2],
                    x$side_lengths[3]))
    }
  }
  
  else if (length(x$side_lengths) == 4) {
    if (max(x$side_lengths) == min(x$side_lengths)) {
      return(paste("a square of side", x$side_lengths[1]))
    } else {
      return(paste("a quadrilateral of sides",
                   x$side_lengths[1],
                   x$side_lengths[2],
                   x$side_lengths[3],
                   x$side_lengths[4]))
    }
  }
  
  else{
    return(paste("A polygon with", length(x$side_lengths), "sides"))
  }
  
}

print(triangle_3)
print(square_4)
print(shape_S3(x = c(1,5,6,7)))
print(shape_S3(x = rep(7,5)))
print(shape_S3(x = c(1,4,6,2,8,1)))

class(square_4)
class(square_4) <- c("shape_S3", "square")
print(square_4)
class(square_4)
inherits(square_4, "square")

# -----------------------------------------------------------------------------
# The S3 system does not have a formal way to define a class but typically, we 
# use a list to define the class and elements of the list serve as data 
# elements.
# -----------------------------------------------------------------------------

## Constructor function for polygon objects
## x a numeric vector of x coordinates
## y a numeric vector of y coordinates

make_poly <- function(x, y) {
  if(length(x) != length(y))
    stop("'x' and 'y' should be the same length")
  
  ## Create the "polygon" object 
  object <- list(xcoord = x, ycoord = y)
  
  ## Set the class name
  class(object) <- "polygon"
  object
}

p <- make_poly(x = 4:9, y = 3:8)
print(p)

## Print method for polygon objects
## x an object of class "polygon"

print.polygon <- function(x, ...){
  cat("A polygon with", length(x$xcoord), "\n", "vertices.")
  invisible(x)
}

print(p)

summary.polygon <- function(object, ...){
  object <- list(rng.x = range(object$xcoord),
                 rng.y = range(object$ycoord))
  class(object) <- "summary_polygon"
  return(object)
}

summary(p)
print(summary(p))

print.summary_polygon <- function(object, ...){
  cat(paste0("abcissa range: [", object$rng.x[1], ",", object$rng.x[2],"]\n",
      "ordinate range: [", object$rng.y[1], ",", object$rng.y[2], "]\n"))
  invisible(object)
}

print(summary(p))

print.summary_polygon <- function(x, ...) {
  cat("x:", x$rng.x[1], "-->", x$rng.x[2], "\n")
  cat("y:", x$rng.y[1], "-->", x$rng.y[2], "\n")
  invisible(x)
}

print(summary(p))

###############################################################################
