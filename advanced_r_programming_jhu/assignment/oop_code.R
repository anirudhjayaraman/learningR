###############################################################################
###                                                                         ###
### Assignment for the course on Advanced R Programming by JHU (Coursera)   ###
### Author: Anirudh Jayaraman                                               ###
###                                                                         ###
###############################################################################

# =============================================================================
# The variables in the dataset are:
# 
# id:        the subject identification number
# visit:     the visit number which can be 0, 1, or 2
# room:      the room in which the monitor was placed
# value:     the level of pollution in micrograms per cubic meter
# timepoint: the time point of the monitor value for a given visit/room
# 
# You will need to design a class called "LongitudinalData" that characterizes 
# the structure of this longitudinal dataset. You will also need to design 
# classes to represent the concept of a "subject", a "visit", and a "room".
# 
# In addition you will need to implement the following functions:
# 
# make_LD: a function converting a data frame into a "LongitudinalData" object
# subject: a generic function for extracting subject-specific information
# visit:   a generic function for extracting visit-specific information
# room:    a generic function for extracting room-specific information
# 
# To complete this Part, you can use either the S3 system, the S4 system, 
# or the reference class system to implement the necessary functions. 
# It is probably not wise to mix any of the systems together, but you should be 
# able to compete the assignment using any of the three systems. The amount of 
# work required should be the same when using any of the systems.
# =============================================================================

# =============================================================================
# Load library dependencies 
# =============================================================================
require(tidyverse)

# =============================================================================
# Generic methods subject, visit and room (S3 implementation) 
# =============================================================================

subject <- function(x, ...) UseMethod("subject", x)
visit <- function(x, ...) UseMethod("visit", x)
room <- function(x, ...) UseMethod("room", x)

# Generic method to check whether an object is of class LongitudinalData ======

is_LD <- function(x, ...) UseMethod("is_LD", x)

# =============================================================================
# "LongitudinalData" Class 
# =============================================================================

# Function to create a LongitudinalData object out of a dataframe -------------

make_LD <- function(data){
  if ("id" %in% names(data)) {
    return(structure(data %>% group_by(id) %>% nest(), 
                     class = "LongitudinalData"))
  } else stop("no subject identification id to make LongitudinalData object")
}

# is_LD default function definition to check whether an object is of type -----
# LongitudinalData

is_LD.default <- function(ldata){
  "LongitudinalData" %in% class(ldata)
}

# A print method for objects of class LongitudinalData ------------------------
print.LongitudinalData <- function(ldata){
  cat("Longitudinal dataset with", length(ldata$id), "subjects")
  return(invisible(ldata))
}

# =============================================================================
# "subject" method returning an object of class "subject"
# =============================================================================

# Generic method "subject" applied to an object of class "LongitudinalData" 
# returning an object of class "subject". 

subject.LongitudinalData <- function(ldata, id){
  if(id %in% ldata$id) {
    idx <- which(ldata$id == id)
    sub <- structure(list(id = id, data = ldata$data[[idx]]),
                     class = "subject")
    return(sub)
  } else {
    return(NULL)
  }
}

# Generic method "print" to print an object of class "subject"

print.subject <- function(sub){
  cat("Subject ID:", sub$id)
  return(invisible(sub))
}

# Generic method "summary" applied to an object of class "subject" to return
# an object of class "summary_subject"

summary.subject <- function(sub){
  rooms <- sub$data$room %>% unique %>% sort
  df <- as.data.frame(matrix(data = 0, nrow = 3,  ncol = 1 + length(rooms)))
  names(df) <- c("visit", rooms)
  df$visit <- 0:2  # visits can be 0, 1 or 2
  for (i in 1:length(rooms)) {
    df[, i + 1] <- map(.x = df$visit, .f = function(x){
      sub$data %>% 
        filter(room == rooms[i] & visit == x) %>% 
        pull(value) %>% 
        mean(na.rm = TRUE) -> out
      if (is.nan(out)) return(NA) else return(out)
    }) %>% unlist
  }
  return(structure(list(id = sub$id, data = df), class = "summary_subject"))
}


# Generic method "print" applied to an object of class "subject_summary"

print.summary_subject <- function(sub_summary){
  cat("ID:", sub_summary$id, "\n")
  print(sub_summary$data)
  return(invisible(sub_summary))
}

# =============================================================================
# "visit" method used to specify the visit number, returning an object of 
# class "visit"
# =============================================================================

# Generic method "visit" applied to an object of class "subject" 
# returning an object of class "visit".

visit.subject <- function(sub, visit_number){
  if(!("subject" %in% class(sub))) stop("class of parameter sub incorrect")
  if(!(visit_number %in% 0:2)) stop("illegal visit_number")
  return(structure(list(id = sub$id,
                        visit_number = visit_number,
                        data = sub$data %>% filter(visit == visit_number)), 
                   class = "visit"))
}

# Generic method "print" applied to an object of class "visit"
print.visit <- function(vis){
  cat("ID:", vis$id, "\n", "Visit:", visit_number)
  return(invisible(vis))
}


# =============================================================================
# "room" method to specify the room being visited applied to an object of class
# "visit" to return an object of class "room"
# =============================================================================

# Generic method "room" applied to an object of class "visit" 
# returning an object of class "room".

room.visit <- function(vis, room_type){
  if(!("visit" %in% class(vis))) stop("class of parameter vis incorrect")
  if(!(room_type %in% unique(vis$data$room))) stop("illegal room_type")
  return(structure(list(id = vis$id, 
                        visit_number = vis$visit_number,
                        room_type = room_type,
                        data = vis$data %>% filter(room == room_type)),
                   class = "room"))
}

# Generic method "print" applied to an object of class "room"

print.room <- function(r){
  cat(paste0("ID: ",
             r$id %>% as.character,
             "\n", "Visit: ",
             r$visit_number %>% as.character,
             "\n",
             "Room: ",
             r$room_type))
  return(invisible(r))
}

# Generic method "summary" applied to an object of class "room" to return 
# an object of class "room_summary"

summary.room <- function(r){
  return(structure(list(id = r$id, 
                        summary = r$data$value %>% summary), 
                   class = c("list", "room_summary")))
}

# Generic method "print" applied to an object of class "room_summary"

print.room_summary <- function(r_summary){
  cat("ID: ", r_summary$id, "\n", sep = "")
  print(r_summary$summary)
  return(invisible(r_summary))
}

###############################################################################

