# Load tidyverse
library(tidyverse)

# Get the Vox articles in 
# Dowloaded from https://data.world/elenadata/vox-articles
articles <- read_tsv('Data/dsjVoxArticles.tsv')

# Function that get rids of html formating 
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# Empty vector for the clean strings
clean_strings <- rep(NA, nrow(articles))

# Clean the strings
for (i in 1:nrow(articles)) {
  mystring <- cleanFun(articles$body[i])
  # Replace escaped spaces
  clean_strings[i] <- gsub("\\Q\\n\\E", '', mystring)
}

# Check that they are the same dimension and that the articles correspond
length(clean_strings)
nrow(articles)
# Same length 


articles[55,]$body
clean_strings[55]
# Same article 

# Join 
articles_clean <- cbind(articles, clean_strings)

# Create new clean file
write_csv(articles_clean, "Data/vox_articles.csv")
