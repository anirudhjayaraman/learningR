###############################################################################
### RC OOP                                                                  ###
###############################################################################

# A Reference Class defining the characteristics of a student -----------------
student <- setRefClass(Class = "student", 
                       fields = list(name = "character",
                                     yob = "Date",
                                     height = "numeric",
                                     id = "character",
                                     courses = "list",
                                     credits = "numeric"), 
                       methods = list(
                         hello = function(){
                           cat("The student's name is ", name, sep = "")
                         },
                         age = function(){
                           return(unclass(Sys.Date() - yob) / 365)
                         },
                         add_credits = function(n){
                           credits <<- credits + n
                         },
                         get_email = function(univ){
                           cat(id, "@", univ, ".edu", sep = "")
                         }
                       ))

# Creating new class objects --------------------------------------------------


anirudh <- new(Class = "student", 
               name = "Anirudh",
               yob = as.Date("1991-01-01"),
               height = 179,
               id = "anirudhjay",
               courses = list(Major = "Econometrics", 
                              Minor = "Macroeconomics",
                              Additional = "Psychology"),
               credits = 20)

# checking / printing object attributes ---------------
anirudh$courses

# Applying class functions ----------------------------
anirudh$age()
anirudh$hello()
anirudh$get_email("JHU")

# Adding 4 credits ------------------------------------
anirudh$credits
anirudh$add_credits(4)
anirudh$credits


# Inheritance =================================================================

grad_student <- setRefClass(Class = "grad_student", 
                            fields = list(thesis = "character"), 
                            contains = "student")

Aditi <- new(Class = "grad_student",
             name = "Aditi",
             yob = as.Date("1992-01-01"),
             height = 170,
             id = "aditigaur",
             courses = list(Major = "Orthodontics", 
                            Minor = "Endodontics",
                            Additional = "Child Psychology"),
             credits = 40,
             thesis = "Correction of malocclusions")

Aditi$get_email("amu")
class(Aditi)

###############################################################################