###############################################################################
### S4 OOP                                                                  ###
###############################################################################

# Class: this argument provides the name of the class as a string
# slots: a named list of attributes and the class of those attributes
# contains: optional super-Class 

setClass(Class = "bus_S4",
         slots = list(n_seats = "numeric", 
                      top_speed = "numeric",
                      current_speed = "numeric",
                      brand = "character"))

# party_bus_S4 inherits from bus_S4
setClass(Class = "party_bus_S4",
         slots = list(n_subwoofers = "numeric",
                      smoke_machine_on = "logical"),
         contains = "bus_S4")


# Create objects using the `new` function -------------------------------------

myBus <- new(Class = "bus_S4")
print(myBus)

# mutate the object using the @ operator to access class elements
myBus@n_seats = 50
myBus@brand = "Ashok Leyland"


hisBus <- new(Class = "bus_S4",
              n_seats = 60,
              top_speed = 50,
              current_speed = 7,
              brand = "Tata")
print(hisBus)
hisBus@n_seats
hisBus@brand

# Generic Functions with S4 ---------------------------------------------------

# Generic function `is_moving` to asess whether bus_S4 is moving
setGeneric(name = "is_moving", 
           def = function(x){
             standardGeneric("is_moving")
           })


setMethod(f = "is_moving", 
          signature = "bus_S4", 
          definition = function(x){
            return(x@current_speed > 0)
          })

is_moving(myBus)  # Returns logical(0) since the speed parameter of this object
                  # is zero
is_moving(hisBus)  # TRUE

is_moving

# nonstandardGenericFunction for "is_moving" defined from package ".GlobalEnv"
# 
# function (x) 
# {
#   standardGeneric("is_moving")
# }
# <environment: 0x000001f0271075d8>
#   Methods may be defined for arguments: x
# Use  showMethods("is_moving")  for currently available ones.

# Create a method for your new class from an existing generic -----------------

setGeneric("print")

setMethod(f = "print", 
          signature = "bus_S4", 
          definition = function(x){
            if(length(x@n_seats) == 0){
              return(cat("Bus with number of seats yet to be parametrized."))
            }
            return(cat("Bus with ", x@n_seats, " seats."))
          })

print(hisBus)
print(myBus)

###############################################################################