load("~/ds-capstone/features.RData") # load the data from previous step
library(data.table); library(dplyr)
# Extract features from Text ...
library(tm); library(SnowballC)

# work with dat1 ----> review_text
docs1 <- VCorpus(VectorSource(dat1$review_text))
length(docs1)
## inspect sample entry
inspect(docs1[1])
docs1[[1]]$content
docs1[[1]]$meta

f <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

docs1 <- tm_map(docs1, f, "\\n")
docs1 <- tm_map(docs1, removePunctuation, TRUE) # remove punc. keep inter-words dashes
docs1 <- tm_map(docs1, f, "-") # replace dashes with space
docs1 <- tm_map(docs1, removeNumbers)
docs1 <- tm_map(docs1, content_transformer(tolower))
docs1 <- tm_map(docs1, removeWords, stopwords("english"))

# stemming
docs1 <- tm_map(docs1, stemDocument)
docs1 <- tm_map(docs1, stripWhitespace)
docs1 <- tm_map(docs1, PlainTextDocument)

# edit metadata
for ( i in 1:dim(dat1)[2]){
  docs1[[i]]$meta$id <- i
  docs1[[i]]$meta$description <- "Reviews for Edinburgh places"
  docs1[[i]]$meta$language <- "English"
}


# Stage the data. Create a document term matrix

# dtm <- DocumentTermMatrix(docs1) # rows = Documents, columns = Terms
# dim(dtm)
# tdm <- TermDocumentMatrix(docs1) # rows = Terms, columns = Documents
# dim(tdm)

# play with removeSparseTerms to see what are the important words
# dtms <- removeSparseTerms(dtm, 0.3) # remove terms with at least 30% of sparse
# dtms
# dtms$dimnames$Terms # see Terms

# Can use for example a dictionary  to derive the docs in Corpus <docs1>,
## that have the words in "dictionary" present. 
### Then the created matrix is tabulated against the dictionary, 
#### i.e., only terms from the dictionary appear in the matrix.
# dtm <- DocumentTermMatrix(docs1, list(dictionary = c("good", "great", "like", "love", "best", "friend", "delicious")))
# dim(dtm)
# Can also use findAssocs() to find associations and findFreqTerms() to find frequent terms

# Now could do a sum of all terms found in a vector using
# score1 <- tm_term_score(dtm, c("good", "great", "like", "love", "best", "friend", "delicious"), slam::row_sums)
# length(score1)

#  Or can do >> 
##  Test for positive and negative sentiments
    # install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
library("tm.lexicon.GeneralInquirer")

score_pos <- sapply(docs1, tm_term_score, terms_in_General_Inquirer_categories("Positiv"))
score_neg <- sapply(docs1, tm_term_score, terms_in_General_Inquirer_categories("Negativ"))

sc1 <- score_pos - score_neg 

# replace text in dat1 with sentimel score
dat1[, review_text := sc1]  

###### work with tip text in dat2
docs2 <- VCorpus(VectorSource(dat2$tip_text))
length(docs2)

docs2 <- tm_map(docs2, f, "\\n")
docs2 <- tm_map(docs2, removePunctuation, TRUE) # remove punc. keep inter-words dashes
docs2 <- tm_map(docs2, f, "-") # replace dashes with space
docs2 <- tm_map(docs2, removeNumbers)

docs2 <- tm_map(docs2, f, "TRUE")

docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removeWords, stopwords("english"))

# stemming
# docs2 <- tm_map(docs2, stemDocument)
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, PlainTextDocument)

# edit metadata
for ( i in 1:dim(dat2)[2]){
  docs2[[i]]$meta$id <- i
  docs2[[i]]$meta$description <- "Tips given for Edinburgh places"
  docs2[[i]]$meta$language <- "English"
}

terms <- c("good", "great", "nice", "like", "love", "best", "epic", "delicious", "mmm", "wow", "awesome", "cool")

dtm_docs2 <- DocumentTermMatrix(docs2)
sc2 <- tm_term_score(dtm_docs2, terms, slam::row_sums)

# replace text in dat2 with sentimel score
dat2[, tip_text := sc2]  

### Combine dat1 and dat2 into my FULL DATASET named D
sum(dat2$business_id %in% dat1$business_id)
setkey(dat1, business_id)
setkey(dat2, business_id)
D <- dplyr::full_join(dat1, dat2, by = "business_id")
D <- as.data.table(D)
setcolorder(D, neworder = c(1, 3, 2, 4:15))
# drop business_id as it is not needed in predictions
D[, business_id := NULL]

# transform response variable (Stars) to a factor with levels 1, 2, 3, 4, 5 and rename it to stars
D[, Stars := as.factor(round(review_stars_avg, 0))]
D[, review_stars_avg := NULL]

# remove garbage
  # rm(i, terms, sc1, sc2, f, score_pos, score_neg, docs1, docs2, dtm_docs2, dat1, dat2)
##########
glimpse(D)






