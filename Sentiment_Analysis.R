# load in the libraries we'll need
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

# get a list of the files in the input directory
files <- list.files("../input")

files <- list.files("C:/Users/admin/Documents/Kaggle_R/Sentiment_Analysis")
# stick together the path to the file & 1st file name
fileName <- glue("../input/", files[1], sep = "")
# get rid of any sneaky trailing spaces
fileName <- trimws(fileName)

# read in the new file
fileText <- glue(read_file(fileName))
# remove any dollar signs (they're special characters in R)
fileText <- gsub("\\$", "", fileText) 

# tokenize
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)

# get the sentiment from the first text: 
tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds
# write a function that takes the name of a file and returns the # of postive
# sentiment words, negative sentiment words, the difference & the normalized difference
GetSentiment <- function(file){
  # get the file
  fileName <- glue("../input/", file, sep = "")
  # get rid of any sneaky trailing spaces
  fileName <- trimws(fileName)
  
  # read in the new file
  fileText <- glue(read_file(fileName))
  # remove any dollar signs (they're special characters in R)
  fileText <- gsub("\\$", "", fileText) 
  
  # tokenize
  tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
  
  # get the sentiment from the first text: 
  sentiment <- tokens %>%
    inner_join(get_sentiments("bing")) %>% # pull out only sentimen words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(sentiment = positive - negative) %>% # # of positive words - # of negative owrds
    mutate(file = file) %>% # add the name of our file
    mutate(year = as.numeric(str_match(file, "\\d{4}"))) %>% # add the year
    mutate(president = str_match(file, "(.*?)_")[2]) # add president
  # return our sentiment dataframe
  return(sentiment)
}

# test: should return
# negative	positive	sentiment	file	year	president
# 117	240	123	Bush_1989.txt	1989	Bush
GetSentiment(files[1])
# file to put our output in
sentiments <- data_frame()

# get the sentiments for each file in our datset
for(i in files){
  sentiments <- rbind(sentiments, GetSentiment(i))
}

# disambiguate Bush Sr. and George W. Bush 
# correct president in applicable rows
bushSr <- sentiments %>% 
  filter(president == "Bush") %>% # get rows where the president is named "Bush"...
  filter(year < 2000) %>% # ...and the year is before 200
  mutate(president = "Bush Sr.") # and change "Bush" to "Bush Sr."

# remove incorrect rows
sentiments <- anti_join(sentiments, sentiments[sentiments$president == "Bush" & sentiments$year < 2000, ])

# add corrected rows to data_frame 
sentiments <- full_join(sentiments, bushSr)

# summerize the sentiment measures
summary(sentiments)
# plot of sentiment over time & automatically choose a method to model the change
ggplot(sentiments, aes(x = as.numeric(year), y = sentiment)) + 
  geom_point(aes(color = president))+ # add points to our plot, color-coded by president
  geom_smooth(method = "auto") # pick a method & fit a model
# plot of sentiment by president
ggplot(sentiments, aes(x = president, y = sentiment, color = president)) + 
  geom_boxplot() # draw a boxplot for each president
# is the difference between parties significant?
# get democratic presidents & add party affiliation
democrats <- sentiments %>%
  filter(president == c("Clinton","Obama")) %>%
  mutate(party = "D")

# get democratic presidents & party add affiliation
republicans <- sentiments %>%
  filter(president != "Clinton" & president != "Obama") %>%
  mutate(party = "R")

# join both
byParty <- full_join(democrats, republicans)

# the difference between the parties is significant
t.test(democrats$sentiment, republicans$sentiment)

# plot sentiment by party
ggplot(byParty, aes(x = party, y = sentiment, color = party)) + geom_boxplot() + geom_point()

### Specifically for the tokens

# in this code block, we're getting a list of the 100 most frequent words in this
# corpus that 1) aren't stop words and 2) aren't already in the Bing lexicon

# function to get tokens from a file
fileToTokens <- function(file){
  # get the file
  fileName <- glue("../input/", file, sep = "")
  # get rid of any sneaky trailing spaces
  fileName <- trimws(fileName)
  
  # read in the new file
  fileText <- glue(read_file(fileName))
  # remove any dollar signs (they're special characters in R)
  fileText <- gsub("\\$", "", fileText) 
  
  # tokenize
  tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
}

# empty data_frame to save our data in
allTokens <- NULL
# get the tokens in each file
for(i in files){
  allTokens <- rbind(allTokens, fileToTokens(i))
}

# get words already in the Bing sentiment dictionary
bingWords <- get_sentiments("bing")[,1]

# get the top 100 most frequent words, excluding stop words
# and word already in the "bing" lexicon
top100Words <- allTokens %>% 
  anti_join(stop_words) %>% # remove stop words
  anti_join(bingWords) %>% # remove words in the bing lexicon
  count(word, sort = T) %>% # sort by frequency
  top_n(100) # get the top 100 terms

# Save out the file (it will show up under "output") so you can download it
# and annotate it in a different program (if you like)
write.csv(top100Words, "top100Words.csv")
