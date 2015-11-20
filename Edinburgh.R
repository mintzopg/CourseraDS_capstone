library(doMC)
registerDoMC(cores = 2)

load("~/ds-capstone/data_wrangling.RData") # load the data from previous step

library(lubridate); library(data.table); library(dplyr)

# focus on city of Edinburgh and area around it (?)
setkey(business, city)

i <- grep("Edinburgh", nonUScities)
nonUScities[i]; rm(i)
Edinburgh_business <- business["Edinburgh"]
City_of_Edinburgh <- business["City of Edinburgh"]; dim(City_of_Edinburgh)
City_of_Edinburgh$business_id %in% Edinburgh_business$business_id
Edinburgh_City_of <- business["Edinburgh City of"]; dim(Edinburgh_City_of)
Edinburgh_City_of$business_id %in% Edinburgh_business$business_id
# include these two into Edinburgh dataset
Edinburgh_business <- rbind(Edinburgh_business, City_of_Edinburgh)
Edinburgh_business <- rbind(Edinburgh_business, Edinburgh_City_of)
rm(City_of_Edinburgh, Edinburgh_City_of)

dim(Edinburgh_business)

# which columns have no value at all
i <- which(apply(Edinburgh_business, 2, function(x){sum(is.na(x))}) == dim(Edinburgh_business)[1])
Edinburgh_business[, c(names(Edinburgh_business)[i]) := NULL]; rm(i)
dim(Edinburgh_business)
 # remove closed
Edinburgh_business <- Edinburgh_business[Edinburgh_business$open]
dim(Edinburgh_business)
Edinburgh_business[, open := NULL]

# in Edinburgh_business the state column has no meaning for me so drop it, along with full address, 
## city as it will be Edinburgh ofcourse.
Edinburgh_business[, c("full_address", "city", "state") := NULL]


# get reviews for Edinburgh
Edinburgh_reviews <- review %>% filter(business_id %in% Edinburgh_business$business_id)
Edinburgh_users <- user %>% filter(user$user_id %in% Edinburgh_reviews$user_id)
Edinburgh_tip <- tip %>% filter(tip$business_id %in% Edinburgh_business$business_id)
rm(business, review, tip, user)

# manipulate Edinburgh_reviews
Edinburgh_reviews[, "votes" := votes.funny + votes.useful + votes.cool] # add a column which sums all reviews
Edinburgh_reviews[, c("votes.funny", "votes.cool", "votes.useful") := NULL]
Edinburgh_reviews$date <- lubridate::ymd(Edinburgh_reviews$date)

# manipulate Edinburgh_users
# In columns friends, replace the friends user_ids with a count of the friends the user has
Edinburgh_users[, friends := sapply(friends, length)]
# consolidate vote types into a single column
Edinburgh_users[, "votes" := votes.funny + votes.useful + votes.cool]
# further drop columns not to be used
Edinburgh_users[, c("votes.funny", "votes.cool", "votes.useful", "name", "elite") := NULL]
## complete yelping day as 01 to use lubridate::ymd
Edinburgh_users[, yelping_since := paste(yelping_since, "-01", sep = "")]
Edinburgh_users$yelping_since <- lubridate::ymd(Edinburgh_users$yelping_since)
# consolidate all compliments in Edinburgh_users into one column. Replace NAs with 0
i <- grep("compliment", names(Edinburgh_users), ignore.case = T)
Edinburgh_users[, "compliments" := sum(i)]
Edinburgh_users[, c(names(Edinburgh_users)[i]) := NULL]; rm(i) # remove all compliment.type columns
sum(is.na(Edinburgh_users$compliments))
summary(Edinburgh_users) # this is a nice format


# manipulate Edinburgh_tip
Edinburgh_tip$date <- lubridate::ymd(Edinburgh_tip$date)

# interested in reviews for businesses in Edinburgh_business (after filtering out closed ones)
Edinburgh_reviews <- Edinburgh_reviews %>% filter(business_id %in% Edinburgh_business$business_id)
## no change there are all included

# re-order columns
setcolorder(Edinburgh_reviews, c("date", "review_id", "user_id", "business_id", "text", "stars", "votes"))
setcolorder(Edinburgh_tip, c("date", "user_id", "business_id", "text", "likes"))

# time ordering of data
setkey(Edinburgh_tip, date)
setkey(Edinburgh_reviews, date)
setkey(Edinburgh_users, yelping_since)


categories <- unique(unlist(Edinburgh_business$categories))
# transform categories columns to a numeric vector with the number of the categories a business falls in
Edinburgh_business[, categories := lapply(Edinburgh_business$categories, length)]

rm(nonUScities)
rm(categories)
rm(list2vec)
