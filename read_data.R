library(doMC)
registerDoMC()
library(jsonlite)

fileDir <- 'yelp_dataset_challenge_academic_dataset'
file1 <- 'yelp_academic_dataset_business.json'
file2 <- 'yelp_academic_dataset_checkin.json'
file3 <- 'yelp_academic_dataset_review.json'
file4 <- 'yelp_academic_dataset_tip.json'
file5 <- 'yelp_academic_dataset_user.json'

## Read business JSON data
con <- file(file.path(fileDir, file1), 'r')
business <- stream_in(con = con)
close(con = con)
business <- flatten(business)

## Read checkin JSON data
con <- file(file.path(fileDir, file2), 'r')
checkin <- stream_in(con = con)
close(con = con)
checkin <- flatten(checkin)

## Read review JSON data
con <- file(file.path(fileDir, file3), 'r')
review <- stream_in(con = con)
close(con = con)
review <- flatten(review)

## Read tip JSON data
con <- file(file.path(fileDir, file4), 'r')
tip <- stream_in(con = con)
close(con = con)
tip <- flatten(tip)

## Read user JSON data
con <- file(file.path(fileDir, file5), 'r')
user <- stream_in(con = con)
close(con = con)
user <- flatten(user)

rm(fileDir, file1, file2, file3, file4, file5, con)
