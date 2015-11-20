load("~/ds-capstone/Edinburgh.RData") # load the data from previous step
library(data.table); library(dplyr); library(lubridate)

## get some info from users
##
setcolorder(Edinburgh_users, 
            c('user_id', 'yelping_since', 'review_count', 'friends', 'fans', 'average_stars', 'votes', 'compliments'))
Edinburgh_users <- Edinburgh_users %>% rename(user_votes = votes)

setkey(Edinburgh_users, yelping_since) # order from start yelping date
Edinburgh_users[, yelping_yrs := as.duration(yelping_since - yelping_since[1])]
Edinburgh_users[, yelping_yrs := as.numeric(yelping_yrs / (60 * 60 * 24 * 365))]
Edinburgh_users[, yelping_since := NULL] # remove yelping_sice
Edinburgh_users[, compliments := NULL] # compliments columns has zero variance, so removed


summary(Edinburgh_users)

#####     Construct features
## in reviews (dat1)
setkey(Edinburgh_reviews, business_id) #order by business_id
setcolorder(Edinburgh_reviews, c("business_id", "review_id", "user_id", "text", "stars", "votes", "date"))
Edinburgh_reviews <- Edinburgh_reviews %>% rename(review_votes = votes)

summary(Edinburgh_reviews)


length(unique(Edinburgh_reviews$business_id)) # not all businesses have reviews

## do a full join of Edinburgh_reviews with Edinburgh_users, by user_id
reviews <- dplyr::full_join(Edinburgh_reviews, Edinburgh_users, by = "user_id")
reviews <- as.data.table(reviews)

#---------------------  Ivestigate Text Language ---------------------------------
library(textcat)

nonEng <- which(textcat(reviews$text) != "english" & textcat(reviews$text) != "scots")
langs <- textcat(reviews$text[nonEng])
table(langs)

names(nonEng) <- langs
nonEngText <- reviews$text[nonEng]

# by eyebolling the following are in English (wrongly categorized)
en <- c(1, 5, 18, 26, 31, 33, 40, 44, 46, 48, 62, 77, 82, 86, 99, 113, 119, 125, 127, 133, 151, 154, 165,
        166, 172, 190, 199, 203, 204, 212, 218, 221, 226, 230, 249, 252, 260, 264, 288, 290, 293, 304)

nonEng <- nonEng[- en] # extract actual non English
reviews$text[nonEng] <- ""

#----------------------
reviews_data <- reviews[, .(number_of_reviews = length(review_id), 
                                 review_stars_avg = mean(stars),
                                 user_stars_avg = mean(average_stars),
                                 review_text = stringr::str_c(text, collapse = TRUE), # join all text entries into one
                                 friends = mean(friends),
                                 fans = mean(fans),
                                 yelping_yrs = mean(yelping_yrs),
                                 review_votes_avg = mean(review_votes),
                                 user_votes_avg = mean(user_votes),
                                 num_yrs_reviewed = length(unique(year(date)))), 
                             by = business_id]

no_review <- dplyr::setdiff(Edinburgh_business$business_id, Edinburgh_reviews$business_id) # business with no review data
no_review <- Edinburgh_business %>% filter(business_id %in% no_review) # get as subset

dat1 <- data.frame(no_review$business_id, matrix(nrow = 139, ncol = 10, rep(0, 10 * 139))) #???
names(dat1) <- names(reviews_data)
dat1 <- rbind(reviews_data, dat1)
rm(reviews_data, no_review)
dim(dat1)

## in tip (dat2)
length(unique(Edinburgh_tip$business_id))
tip_data <- Edinburgh_tip[, .(number_of_tips = length(text),
                              tip_text = stringr::str_c(text, collapse = TRUE),
                              likes_avg = mean(likes),
                              num_years_tiped = length(unique(year(date)))), 
                          by = business_id]
no_tip <- dplyr::setdiff(Edinburgh_business$business_id, Edinburgh_tip$business_id) # business with no tip data
no_tip <- Edinburgh_business %>% filter(business_id %in% no_tip) # get as subset
dat2 <- data.frame(no_tip$business_id, matrix(nrow = 1477, ncol = 4, rep(0, 4 * 1477)))
names(dat2) <- names(tip_data)
dat2 <- rbind(tip_data, dat2)
rm(tip_data, no_tip)
dim(dat2)

# garbage
rm(en, langs, nonEng, nonEngText, reviews, Edinburgh_business, Edinburgh_reviews, Edinburgh_tip, Edinburgh_users)


