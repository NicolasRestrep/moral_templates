# Creating a csv file with all the interviews 
library(tidyverse)
library(readtext)
library(tm)
library(tidytext)
library(quanteda)
here::here()

# Read all the files 
first_set <- read_csv("Data/articles1.csv")
second_set <- read_csv("Data/articles2.csv")
third_set <- read_csv("Data/articles3.csv")
articles <- rbind(first_set, 
                  second_set, 
                  third_set)

# Function that get rids of html formating 
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# Empty vector for the clean strings
clean_strings <- rep(NA, nrow(articles))

# Clean the strings
for (i in 1:nrow(articles)) {
  mystring <- cleanFun(articles$content[i])
  # Replace escaped spaces
  clean_strings[i] <- gsub("\\Q\\n\\E", '', mystring)
}

# Check that they are the same dimension and that the articles correspond
length(clean_strings)
nrow(articles)
# Same length 

# Join 
articles_clean <- cbind(articles, clean_strings)

breibart <- articles_clean %>% 
  filter(publication == "Breitbart")

write_csv(breibart, 
          "Data/breitbart_articles.csv")
