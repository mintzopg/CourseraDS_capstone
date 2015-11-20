library(doMC)
registerDoMC(cores = 2)
library(data.table);library(dplyr, tidyr); library(stringr);library(lubridate)

load("~/ds-capstone/read_data.RData") # load the data from previous step (data tranformed to R objs)
#---------------------------------------------------------------------------------------------------

#------------------ Functions used will be dded here -------------------------
list2vec <- function(listIn){
  # IN: a recursive list with lists elements of length=1; OUT: a Vector  
  n <- length(listIn)
  vec <- c()
  for(i in 1:n) vec[i] <- ifelse(!is.null(listIn[[i]]), vec[i] <- listIn[[i]], NA)
  return (vec)
}


## ---------------------------------------------------------------------------

# Convert to data.table format and Set keys
business <- data.table(business)
checkin <- data.table(checkin)
review <- data.table(review)
tip <- data.table(tip)
user <- data.table(user)

# the tables are:
tables()

#   Drop some unneeded columns
business[, type := NULL]
checkin[, type := NULL]
review[, type := NULL]
tip[, type := NULL]
user[, type := NULL]

#   Manipulations- Elementary

## TABLE ----> business
names(business) <- str_replace(names(business), "attributes.", "") # simplify columns names
names(business) <- str_replace_all(names(business), " ", "")
# 1st unlist all elements that are lists
business[, AcceptsCreditCards := sapply(AcceptsCreditCards, unlist)]
# 2nd apply the function list2vec
business[, AcceptsCreditCards := list2vec(AcceptsCreditCards)]

# reorder columns
setcolorder(business, 
    c(1, 4, 7, 2, 5, 9, 12, 10, 8, 11, 3, 6, 17, 16, 13, 19, 18, 21, 20, 15, 14, 25, 24, 23, 22, 26:104))

# are there any NULLs
sum(apply(business, 2, function(x){sum(is.null(x))}) >0) # no

length(unique(business$business_id)) == dim(business)[1]  # business_id is a unique identifier

## TABLE ----> checkin
names(checkin) <- str_replace_all(names(checkin), " ", "")
# any NULLs
sum(apply(checkin, 2, function(x){sum(is.null(x))}) >0) # no
length(unique(checkin$business_id))  == dim(checkin)[1]   # same as before

## TABLE ----> review
names(review) <- str_replace_all(names(review), " ", "")
# review[, c("date") := NULL]
# review[, review_id, by = user_id] # arrange them by user_id

# any NULLs
sum(apply(review, 2, function(x){sum(is.null(x))}) >0) #no

## TABLE ----> tip
names(tip) <- str_replace_all(names(tip), " ", "")
# any NULLs
sum(apply(tip, 2, function(x){sum(is.null(x))}) >0) # no
# tip[, date := NULL]

## TABLE ----> user
names(user) <- str_replace_all(names(user), " ", "")

# any NULLs
sum(apply(user, 2, function(x){sum(is.null(x))}) >0) # no
# DROP yelping_since
# user[, c("yelping_since") := NULL]

# First conclusions on the 5 tables
##       Where in the wolrd are the mentioned businesses? 
# load a list of the US postal codes from an external csv file
US.Postal.codes <- read.csv("~/ds-capstone/diafora/US Postal codes.csv", sep="")
US.Postal.codes <- unlist(US.Postal.codes)
US.Postal.codes <- as.vector(US.Postal.codes)
state <- unique(business$state)
nonUS <- state[! state %in% US.Postal.codes]
nonUScities <- unique(business$city[business$state %in% nonUS]) # not all cities are in US
rm(US.Postal.codes, state, nonUS)

#______________________________________________________________________________#

# At this point I will ignore the checkin data set as non essentially informative
rm(checkin)

####

categories <- unique(unlist(business$categories))
categories <- str_replace_all(categories, " ", "")
categories <- str_trim(categories, side = c("both", "left", "right"))
categories <- str_replace_all(categories, "[&,()]", "")






