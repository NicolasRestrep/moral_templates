library(tidyverse)
library(knitr)
library(kableExtra)
library(ggrepel)
library(gridExtra)
library(haven)
library(lavaan)
library(lme4)
library(nlme)
library(stargazer)
library(broom)
library(psych)
library(brms)
library(plotly)
library(sjPlot)
library(quanteda)

# Get the datasets in 
triplets_sentiment <- read_csv("Data/triplets_mfd_bb.csv")
dictionary <- read_csv("Data/FullSurveyorUSMeans.csv")

# I need to clean the dictionary a bit before we do the matching 
dictionary <- dictionary %>% 
  mutate(term = term %>% tolower(.) %>% str_replace_all(., "_", " "))

# Empty matrix for the loop 
epa_matrix <- matrix(NA, nrow(triplets_sentiment), 6)

for (x in 1:nrow(triplets_sentiment)) {
  
  match_obj <- match(triplets_sentiment$clean_objs[x], dictionary$term)
  match_verb <- match(triplets_sentiment$clean_verbs[x], dictionary$term)
  epa_matrix[x,1:3] <- dictionary[match_obj, 2:4] %>% as.matrix()
  epa_matrix[x,4:6] <- dictionary[match_verb, 2:4] %>% as.matrix
  
  
}

# Rename resulting matrix 
colnames(epa_matrix) <- c("oe", "op", "oa", "be", "bp", "bo")


df <- cbind(triplets_sentiment, epa_matrix)

# Now calculate the Euclidean distance 
eucl_dist_prot <- function(x) { 
  op <-  df[x, 'op'] 
  be <-  df[x, 'be'] 
  bp <-  df[x, 'bp']
  
  # Formula   
  # The values of the prototype are always the ones the events are compared against
  distance <- sqrt(((-1.14) - op)^2 + ((-4.26) - be)^2 + (1.95 - bp)^2)
  return(as.double(distance))
}

distances_prot <- map_dbl(1:nrow(df), eucl_dist_prot)

# Add to the main dataframe 
df <- df %>% 
  mutate(distances_prot = distances_prot) %>% 
  mutate(dist_stand = as.vector(scale(distances_prot)), 
         comp_stand = as.vector(scale(compound)), 
         neg_stand = as.vector(scale(negative)), 
         len = excerpt_end-excerpt_start) %>% 
  mutate(negation = ifelse(str_detect(verb, "n't") == TRUE | 
                             str_detect(verb, "never") == TRUE | 
                             str_detect(verb, "not") == TRUE, 1, 0))
dfbin <- df %>% 
  mutate(bin_count = ifelse(moral_count == 0, 0, 1)) %>% 
  filter(negation != 1)
m <- rstanarm::stan_glmer(bin_count ~ dist_stand+len+comp_stand + (1 | Document), 
      data = dfbin, 
      family = 'binomial')

saveRDS(m, "~/Documents/moral_templates/fitted_models/poiss_model_bb.rds")
