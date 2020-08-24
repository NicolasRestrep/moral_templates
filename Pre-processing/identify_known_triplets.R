# Packages
library(tidyverse)
library(textstem)

# Import SVO objects from vox 
triplets <- read_csv("C:/Users/nro04/Documents/moral_templates/Data/vox_triplets_dataset.csv")
glimpse(triplets)

# Lower everything for easier manipulation
triplets <- triplets %>% 
  mutate_at(.vars = c('subject', 'verb', 'object'), tolower)

# Import the ACT dictionary
dictionary <- read_csv("C:/Users/nro04/Documents/moral_templates/Data/FullSurveyorUSMeans.csv")

# Lemmatize the verbs so they are all in the present tense
# Lemmatize objects so we avoid the plural (not in the ACT dictionaries)
triplets_clean <- triplets %>% 
  mutate(clean_verbs = lemmatize_strings(verb, language = 'en'), 
         clean_objs = lemmatize_strings(object, language = "en")) 

# What to do with conjugations? Let's split the strings 
# Create a function to split the strings and get the last word 
break_verb <- function(x) {
  words <- str_split(triplets_clean$clean_verbs[x], " ") 
  if(triplets_clean$clean_verbs[x] %>% str_count('\\S+') > 1) {
  return(words[[1]][length(words[[1]])])
    } else { 
  return(words[[1]][1])
  }
}

# Get only the verbs into a string
# Takes some time
lemm_verbs <- map_chr(c(1:nrow(triplets_clean)), break_verb)

# Add new clean verbs to the main df 
triplets_clean <- triplets_clean %>% 
  mutate(clean_verbs = lemm_verbs)

# Check that the verbs make sense 
triplets_clean %>% select(verb, clean_verbs) %>% head()
# Looks good

# Verbs and objects from vox articles
verbs_vox <- triplets_clean$clean_verbs
objects_vox <- triplets_clean$clean_objs

# Verbs and objects from ACT 
verbs_act <- dictionary %>% filter(Type == "Behaviors") %>%  .$term %>% tolower(.) %>% str_replace_all(., "_", " ")
objects_act <- dictionary %>% filter(Type == "Identities") %>%  .$term %>% tolower(.) %>% str_replace_all(., "_", " ")

# Find common objects 
common_verbs <- intersect(verbs_vox, verbs_act)
common_objects <- intersect(objects_act, objects_vox)

# Get triplets for which we already have the EPA values of objects & verbs
d_filtered <- triplets_clean %>% 
  filter(clean_objs %in% common_objects & clean_verbs %in% common_verbs) %>% 
  rename(index = X1)

# Check index works 
d_filtered[1,]
triplets_clean[triplets_clean$X1==34,]
# Looks good 

# Save the new dataset
write_csv(d_filtered, "C:/Users/nro04/Documents/moral_templates/Data/already_known.csv", col_names = TRUE)

  


