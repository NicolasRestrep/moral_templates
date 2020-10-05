####### Fitting the Models ############

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
library(plot3D)
library(scatterplot3d)
library(quanteda)



# Load in the conservative data
c_data <- read_csv("Data/conservatives_full.csv")

# Get rid of the first 2 rows (they don't mean anything)
c_data <- c_data[-(1:2), ]

# Load pretest data 
p_data <- read_csv("data/pretest.csv")

# Same issue with the rows
p_data <- p_data[-(1:3), ]

# Load the liberal data 
l_data <- read_csv("data/liberals_complete.csv")

l_data <- l_data[-(1:2),]

# Now bind them together 
data <- rbind(c_data, p_data, l_data)

# Get attention checks for the data 
data <- data %>% 
  mutate(passed_1 = ifelse(ach_1_1 == "extremely harmful", 1, 0), 
         passed_2 = ifelse(ach_2_1 == "not at all immoral", 1, 0), 
         passed_3 = ifelse(ach_3_1 == "moderately unexpected", 1, 0), 
         passed_4 = ifelse(ach_4_1 == "not at all harmful", 1, 0), 
         passed_5 = ifelse(ach_5_1 == "extremely immoral", 1, 0), 
         passed_6 = ifelse(ach_6_1 == "extremely unexpected", 1, 0), 
         sum_pass = passed_1 + passed_2 + passed_3 + passed_4 + passed_5 + passed_6)

# Now drop cases which did not pass the attention checks 
data <- data %>% 
  filter(sum_pass == 6)

# Create variable that indicates if someone is conservative
data <- data %>% 
  mutate(conservative = ifelse(ideology_1 == 5 | ideology_1 == 6 | ideology_1 == 7, 1, 0)) 


# Mutate all main questions so that we get numbers 
data <- data %>% 
  mutate_at(vars(contains("_h_1")), funs(recode(., 
                                                "not at all harmful" = 1, 
                                                "slightly harmful" = 2, 
                                                "moderately harmful" = 3, 
                                                "very harmful" = 4, 
                                                "extremely harmful" = 5))) %>% 
  mutate_at(vars(contains("_i_1")), funs(recode(., 
                                                "not at all immoral" = 1, 
                                                "slightly immoral" = 2, 
                                                "moderately immoral" = 3, 
                                                "very immoral" = 4, 
                                                "extremely immoral" = 5))) %>% 
  mutate_at(vars(contains("_u_1")), funs(recode(., 
                                                "not at all unexpected" = 1, 
                                                "slightly unexpected" = 2, 
                                                "moderately unexpected" = 3, 
                                                "very unexpected" = 4, 
                                                "extremely unexpected" = 5)))


# Get of of the first set of uninformative columns 
d <- data %>% 
  select(-contains("ach")) %>% 
  select(pkp_h_1:msas_u_1, ideology_1, conservative)

# Now let's create a more amenable dataset 
d <- d %>% 
  mutate(id = 1:n()) %>% 
  select(id, everything()) 

# Rename all variables 
d <- d %>%  
  rename_all(
    funs(str_remove(., "_1"))
  ) %>% 
  mutate_if(is.character, as.numeric)


# Reshape harmful perceptions from wide to long 
d_harm_long <- d %>% 
  select(id, ideology, conservative, contains("_h")) %>% 
  gather(key = "scenario", value = "harm", 4:28) %>% 
  mutate(scenario = str_remove(scenario, "_h$"))

# Reshape immoral perceptions from wide to long 

d_imm_long <- d %>% 
  select(id, ideology, conservative, contains("_i")) %>% 
  gather(key = "scenario", value = "immoral", 4:28) %>% 
  mutate(scenario = str_remove(scenario, "_i$"))

# Reshape unexpected perceptions from wide to long 

d_un_long <- d %>% 
  select(id, ideology, conservative, contains("_u")) %>% 
  gather(key = "scenario", value = "unexpected", 4:28) %>% 
  mutate(scenario = str_remove(scenario, "_u$"))

# Now join them 
d_long <- d_harm_long %>% 
  mutate(immoral = d_imm_long$immoral, 
         unexpected = d_un_long$unexpected)

# Load dataset with the deflections 
selected_events <- read_csv("Data/full_events.csv")

# Create column for abreviations 
events <- c("pch", 
            "pkp", 
            "tmdp", 
            "adr", 
            "elb", 
            "acr", 
            "jbd", 
            "scc", 
            "ddf", 
            "git", 
            "idb", 
            "ayc", 
            "sip", 
            "ecc", 
            "ccr", 
            "mbr", 
            "msn", 
            "mbw", 
            "php", 
            "mmc", 
            "mmls", 
            "msas", 
            "mpsa", 
            "whh", 
            "thls")

# Create new column 
events_def <- selected_events %>% 
  mutate(scenario = events) %>% 
  select(scenario, def, type, 5:33)

# Add to long dataset 
d_long <- d_long %>% 
  left_join(events_def, by = "scenario")

# create a summary table for the values of each question 
gd <- d %>% 
  select(-c(id, ideology, conservative)) %>% 
  gather(key = "scenario", 
         value = "score") %>% 
  group_by(scenario) %>% 
  summarise_all(funs(med = median(.), avg = mean(.), maximum = max(.), minimum = min(.), st_dv = sd(.), fq = quantile(., 0.25), tq = quantile(., 0.75))) 

gd <- gd %>% 
  mutate(scenario = str_replace(scenario, "acr", "athlete_cheats_rival"), 
         scenario = str_replace(scenario, "adr", "athlete_deceives_referee"), 
         scenario = str_replace(scenario, "ayc", "athlete_yells_at_coach"), 
         scenario = str_replace(scenario, "ccr", "coach_cheers_rival"), 
         scenario = str_replace(scenario, "ddf", "daughter_disobeys_father"),
         scenario = str_replace(scenario, "ecc", "employee_conspires_with_comp."),
         scenario = str_replace(scenario, "elb", "employee_lies_to_boss"), 
         scenario = str_replace(scenario, "git", "girl_interrupts_teacher"),
         scenario = str_replace(scenario, "idb", "intern_disobeys_boss"), 
         scenario = str_replace(scenario, "jbd", "judge_befriends_defendant"), 
         scenario = str_replace(scenario, "mbr", "man_betrays_relative"), 
         scenario = str_replace(scenario, "mbw", "man_betrays_wife"), 
         scenario = str_replace(scenario, "mmc", "man_marries_cousin"),
         scenario = str_replace(scenario, "mmls", "man_makes_love_sister"), 
         scenario = str_replace(scenario, "mpsa", "married_has_sex_adulterer"), 
         scenario = str_replace(scenario, "msas", "mother_sexually_arouses_son"), 
         scenario = str_replace(scenario, "msn", "mayor_slanders_neighbor"), 
         scenario = str_replace(scenario, "pch", "person_hurts_child"), 
         scenario = str_replace(scenario, "php", "person_hires_prostitute"), 
         scenario = str_replace(scenario, "pkp", "person_kills_person"),
         scenario = str_replace(scenario, "scc", "student_cheats_classmate"), 
         scenario = str_replace(scenario, "sip", "student_insults_professor"), 
         scenario = str_replace(scenario, "thls", "teacher_hits_lazy_student"), 
         scenario = str_replace(scenario, "whh", "wife_hits_husband"), 
         scenario = str_replace(scenario, "tmdp", "teenager_mocks_disabled"))

# create mean measures from the long dataset

dsl <- d_long %>% 
  group_by(scenario, type, def, dop) %>% 
  summarise(mean_harm = mean(harm), 
            mean_imm = mean(immoral), 
            mean_unex = mean(unexpected)) 

# Scale all variables before the analysis
d_long_scaled <- 
  d_long %>% 
  mutate_at(c("id", "type", "scenario", "conservative"), ~as.factor(.)) %>% 
  mutate_if(is.numeric, scale)

# Fit the initial models 
b1 <- brm(immoral ~ 1 + ae + ap + aa + be + bp + ba + 
            oe + op + oa + (1 | id) + (1 | scenario),
          iter = 5000, warmup = 1000, chains = 4, cores = 6,  
          control = list(adapt_delta = 0.95), 
          prior = c(prior(normal(0, 5), class = Intercept),
                    prior(normal(0, 1), class = b),
                    prior(cauchy(0, 1), class = sd)),
          family = gaussian, 
          data = d_long_scaled)

b2 <- update(b1, 
             newdata = filter(d_long_scaled, type != "purity"))

saveRDS(b1, "~/Documents/moral_templates/fitted_models/ir_complete_cases.rds")
saveRDS(b2, "~/Documents/moral_templates/fitted_models/ir_no_purity.rds")


########### Reaction Time models ###########

# Load in conservative data
rt_conservative <- read_csv("Data/rt_conservative.csv")

# Load in liberal data 
rt_liberal <- read_csv("Data/rt_liberal.csv")

# Delete unnecessary rows 
rt_conservative <- rt_conservative[-(1:2),]
rt_liberal <- rt_liberal[-(1:2),]

# How many people passed the attention checks?
rt_conservative <- rt_conservative %>% 
  mutate(passed = if_else(att_1 == "Harmless (K)" & att_2 == "Immoral (J)" & att_3 == "Harmful (J)", 1, 0)) 

rt_conservative %>% 
  group_by(passed) %>% 
  summarise(n())

rt_liberal <- rt_liberal %>% 
  mutate(passed = if_else(att_1 == "Harmless (K)" & att_2 == "Immoral (J)" & att_3 == "Harmful (J)", 1, 0)) 

rt_liberal %>% 
  group_by(passed) %>% 
  summarise(n()) 

# Join both datasets 
data <- rbind(rt_liberal, rt_conservative)

# Filter people who did not pass
data <- data %>% 
  filter(passed == 1)

# Now let's turn this into a long dataframe 

# Create a manageable wide dataframe 

rtw <- data %>% 
  select(ends_with("Page Submit"), i_phc, i_pkp, i_tmdp, i_adr, i_elb, 
         i_acr, i_jbd, i_scc, i_ddf, i_git, i_idb, i_ayc, i_sip, i_ecc, 
         i_ccr, i_mbr, i_msn, i_mbw, i_php, i_mmc, i_mmls, i_msas, i_mpsa, i_whh, i_thls, h_phc, h_pkp, h_tmdp, h_adr, h_elb, 
         h_acr, h_jbd, h_scc, h_ddf, h_git, h_idb, h_ayc, h_sip, h_ecc, 
         h_ccr, h_mbr, h_msn, h_mbw, h_php, h_mmc, h_mmls, h_msas, h_mpsa, h_whh, h_thls) %>% 
  mutate(ids = 1:184) %>% 
  select(ids, everything())


# Create a long dataframe for harm and reaction time

d_long_harm_rt <- rtw %>% 
  select(ids, starts_with("h_")) %>% 
  pivot_longer(2:26, names_to = "scenario", values_to = "reaction_time_harm") %>% 
  select(ids, scenario, reaction_time_harm) %>% 
  mutate(scenario = str_remove(scenario, "_t_Page Submit$")) %>% 
  mutate(scenario = str_remove(scenario, "^h_"))

# Create a long dataframe for is_harm

d_long_harm_bin <- rtw %>% 
  select(ids, starts_with("h_")) %>% 
  pivot_longer(27:51, names_to = "scenario", values_to = "is_harmful") %>% 
  select(ids, scenario, is_harmful) %>% 
  mutate(scenario = str_remove(scenario, "^h_"))

d_long_harm <- left_join(d_long_harm_rt, d_long_harm_bin, by = c('ids', "scenario"))

# Create a long dataframe for immorality and reaction time 

d_long_imm_rt <- rtw %>% 
  select(ids, starts_with("i_")) %>% 
  pivot_longer(2:26, names_to = "scenario", values_to = "reaction_time_imm") %>% 
  select(ids, scenario, reaction_time_imm) %>% 
  mutate(scenario = str_remove(scenario, "_t_Page Submit$")) %>% 
  mutate(scenario = str_remove(scenario, "^i_"))

# Create a long dataframe for immorality and is_immoral

d_long_imm_bin <- rtw %>% 
  select(ids, starts_with("i_")) %>% 
  pivot_longer(27:51, names_to = "scenario", values_to = "is_immoral") %>% 
  select(ids, scenario, is_immoral) %>% 
  mutate(scenario = str_remove(scenario, "^i_"))

d_long_imm <- left_join(d_long_imm_rt, d_long_imm_bin, by = c('ids', 'scenario'))

# Now join them 

d_long <- left_join(d_long_harm, d_long_imm, by = c('ids', 'scenario')) %>% 
  mutate_at(c('reaction_time_harm', 'reaction_time_imm'), as.numeric)


# Let's get some information about the events in there 

# Load dataset with the deflections 

selected_events <- read_csv("data/full_events.csv")

# Create column for abreviations 

events <- c("phc", 
            "pkp", 
            "tmdp", 
            "adr", 
            "elb", 
            "acr", 
            "jbd", 
            "scc", 
            "ddf", 
            "git", 
            "idb", 
            "ayc", 
            "sip", 
            "ecc", 
            "ccr", 
            "mbr", 
            "msn", 
            "mbw", 
            "php", 
            "mmc", 
            "mmls", 
            "msas", 
            "mpsa", 
            "whh", 
            "thls")

# Create new column 

events_def <- selected_events %>% 
  mutate(scenario = events) %>% 
  select(scenario, def, type, 5:33)

# Add to long dataset 

d_long <- d_long %>% 
  left_join(events_def, by = "scenario")

# How many weird responses do we have? 

sum(d_long$reaction_time_harm < 1)
sum(d_long$reaction_time_imm < 1)
sum(d_long$reaction_time_harm > 10)
sum(d_long$reaction_time_imm > 10)

# There are 66 responses that don't seem valid. Let's delete them

d_long <- d_long %>% 
  filter(reaction_time_harm >= 1 & 
           reaction_time_imm >= 1 & 
           reaction_time_harm <= 10 & 
           reaction_time_imm <= 10)
# Recode variables to binary 

d_long <- d_long %>%  
  mutate(is_immoral = case_when(is_immoral == "Immoral (J)" ~ 1, 
                                is_immoral == "Not Immoral (K)" ~ 0), 
         is_harmful = case_when(is_harmful == "Harmful (J)" ~ 1, 
                                is_harmful == "Harmless (K)" ~ 0))

# Create a new variable for the variances 

d_long_var <- d_long %>% 
  group_by(scenario) %>% 
  summarise(p_harm = mean(is_harmful, na.rm = T), 
            p_imm = mean(is_immoral, na.rm = T), 
            v_harm = var(is_harmful, na.rm = T), 
            v_imm = var(is_immoral, na.rm = T))

# Join back to the long dataframe 

d_long <- left_join(d_long, d_long_var, by = "scenario")

dsl <- d_long %>% 
  group_by(scenario, type) %>% 
  summarise(mean_harm = mean(reaction_time_harm), 
            mean_imm = mean(reaction_time_imm)) %>% 
  ungroup()

# Name the scenarios with the full length 

dsl <- dsl %>% 
  mutate(full_string = case_when(scenario == "acr" ~ "An athlete cheats their rival", 
                                 scenario == "adr" ~ "An athlete deceives the referee", 
                                 scenario == "ayc" ~ "An athlete yells at their coach", 
                                 scenario == "ccr" ~ "A coach cheers for the rival", 
                                 scenario == "ddf" ~ "A daughter disobeys her father", 
                                 scenario == "ecc" ~ "An employee conspires with a competitor", 
                                 scenario == "elb" ~ "An employee lies to the boss", 
                                 scenario == "git" ~ "A girl interrupts her teacher", 
                                 scenario == "idb" ~ "An intern disobeys their boss", 
                                 scenario == "jbd" ~ "A judge befriends the defendant", 
                                 scenario == "mbr" ~ "A man betrays his relative", 
                                 scenario == "mbw" ~ "A man betrays his wife", 
                                 scenario == "mmc" ~ "A man marries his cousin", 
                                 scenario == "mmls" ~ "A man makes love to his sister", 
                                 scenario == "mpsa" ~ "A married person has sex with an adulterer", 
                                 scenario == "msas" ~ "A mother sexually arouses her son", 
                                 scenario == "msn" ~ "A mayor slanders a neighbor", 
                                 scenario == "phc" ~ "A person hurts a child", 
                                 scenario == "php" ~ "A person hires a prostitute", 
                                 scenario == "pkp" ~ "A person kills a person", 
                                 scenario == "scc" ~ "A student cheats their classmate", 
                                 scenario == "sip" ~ "A student insults the professor", 
                                 scenario == "thls" ~ "A teacher hits a lazy student", 
                                 scenario == "whh" ~ "A wife hits her forgetful husband", 
                                 scenario == "tmdp" ~ "A teenager mocks a disabled person"))

# Add the short versions again for the plot

dsl <- dsl %>% 
  mutate(length = str_count(full_string)) 

# Calculate readibility 

readibility_indices <- c(NA, rep = 25)

for (i in 1: 25) {
  
  y <- textstat_readability(dsl$full_string[i], measure = "Flesch.Kincaid")
  readibility_indices[i] <- y$Flesch.Kincaid
}

# Join with dsl 

dsl <- cbind(dsl, readibility_indices)

# Create function to calculate the euclidean distance in three-dimensional space 

events_values <- events_def %>% 
  select(scenario, op, be, bp)

eucl_dist_pkp <- function(x) { 
  op <-  events_values[x, 2] 
  be <-  events_values[x, 3] 
  bp <-  events_values[x, 4]
  
  # Formula   
  # The values of the prototype are always the ones the events are compared against
  distance <- sqrt((0.95 - op)^2 + ((-4.26) - be)^2 + (1.95 - bp)^2)
  return(as.double(distance))
}


distances_pkp <- map_dbl(1:25, eucl_dist_pkp)

events_values_pkp <- events_values %>% 
  cbind(distances_pkp) %>% 
  select(scenario, distances_pkp)

# Join values with the long dataset 

dsl <- dsl %>% 
  left_join(events_values_pkp, by = "scenario")

# Create new function with Person hurts child as the prototype 

eucl_dist_phc <- function(x) { 
  op <-  events_values[x, 2] 
  be <-  events_values[x, 3] 
  bp <-  events_values[x, 4]
  
  # Formula   
  # The values of the prototype are always the ones the events are compared against
  distance <- sqrt(((-1.14) - op)^2 + ((-3.17) - be)^2 + (1.06 - bp)^2)
  return(as.double(distance))
}


distances_phc <- map_dbl(1:25, eucl_dist_phc)

events_values_phc <- events_values %>% 
  cbind(distances_phc) %>% 
  select(scenario, distances_phc)

# Join values with the long dataset 

dsl <- dsl %>% 
  left_join(events_values_phc, by = "scenario")

# Create new function with Person hurts child as the prototype 

eucl_dist_prot <- function(x) { 
  op <-  events_values[x, 2] 
  be <-  events_values[x, 3] 
  bp <-  events_values[x, 4]
  
  # Formula   
  # The values of the prototype are always the ones the events are compared against
  distance <- sqrt(((-1.14) - op)^2 + ((-4.26) - be)^2 + (1.95 - bp)^2)
  return(as.double(distance))
}


distances_prot <- map_dbl(1:25, eucl_dist_prot)

events_values_prot <- events_values %>% 
  cbind(distances_prot) %>% 
  select(scenario, distances_prot)

# Join values with the long dataset 

dsl <- dsl %>% 
  left_join(events_values_prot, by = "scenario")

lengths <- dsl %>% 
  select(scenario, length, readibility_indices, distances_phc, distances_pkp, distances_prot)

d_long <- d_long %>% 
  select(1:16, 36:39) %>% 
  left_join(lengths, by = "scenario")

# Add one column for the average reaction time of the person 

# Create the averages 
avg_rt <- d_long %>% 
  select(ids, reaction_time_harm, reaction_time_imm) %>% 
  pivot_longer(2:3) %>% 
  group_by(ids) %>% 
  summarise(avg_rt = mean(value))

# Add averages to long data 

d_long <- left_join(d_long, avg_rt, by = "ids")

# Add ideological beliefs

data_pol <- data %>% 
  select(ideology_1) %>% 
  mutate(ids = 1:184)

d_long <- left_join(d_long, data_pol, by = "ids")

# Scale variables 

d_long_scaled <- d_long %>% 
  mutate_at(c("ids", "type", "scenario", "is_harmful", "is_immoral"), ~as.factor(.)) %>% 
  mutate_if(is.numeric, scale)

b3 <- brm(reaction_time_harm ~ 1 + distances_prot + I(distances_prot^2) + 
            length + readibility_indices + (1 | ids) + (1 | scenario),
          iter = 5000, warmup = 1000, chains = 4, cores = 6,  
          control = list(adapt_delta = 0.95), 
          prior = c(prior(normal(0, 10), class = Intercept),
                    prior(normal(0, 5), class = b),
                    prior(cauchy(0, 1), class = sd)),
          family = gaussian,
           data = d_long_scaled)

b4 <- brm(reaction_time_imm ~ 1 + distances_prot + I(distances_prot^2) + 
            length + readibility_indices + (1 | ids) + (1 | scenario),
          iter = 5000, warmup = 1000, chains = 4, cores = 6,  
          control = list(adapt_delta = 0.95), 
          prior = c(prior(normal(0, 10), class = Intercept),
                    prior(normal(0, 5), class = b),
                    prior(cauchy(0, 1), class = sd)),
          family = gaussian,
          data = d_long_scaled)

b5 <- brm(is_harmful ~ 1 + distances_prot + length + 
          readibility_indices + (1 | ids) + (1 | scenario),
          data = d_long_scaled, 
          family = bernoulli, 
          iter = 5000, warmup = 1000, chains = 4, cores = 6,  
          control = list(adapt_delta = 0.95), 
          prior = c(prior(normal(0, 1), class = Intercept),
                    prior(normal(0, 1), class = b),
                    prior(cauchy(0, 1), class = sd)))

saveRDS(b3, "~/Documents/moral_templates/fitted_models/rt_harm_model.rds")
saveRDS(b4, "~/Documents/moral_templates/fitted_models/rt_imm_model.rds")
saveRDS(b5, "~/Documents/moral_templates/fitted_models/log_model_harmful.rds")

#### Text Analysis Model #### 

# Get the datasets in 
triplets_sentiment <- read_csv("Data/triplets_mfd.csv")
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
         len = excerpt_end-excerpt_start)

b6 <- brm(moral_count ~ dist_stand+len+comp_stand, 
          data = df, 
          family = 'poisson', 
          iter = 5000, warmup = 1000, chains = 4, cores = 6,  
          control = list(adapt_delta = 0.95), 
          prior = c(prior(normal(0, 10), class = Intercept),
                    prior(normal(0, 1), class = b)))

saveRDS(b6, "~/Documents/fitted_models/poiss_model.rds")         
 