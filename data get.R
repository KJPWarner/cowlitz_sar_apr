#########SSBR data get using rRMIS
remotes::install_github("Ben-Cox/rRMIS")
devtools::install_github("KJPWarner/RMISr")
#devtools::install_github("KJPWARNER/rRMIS")##fork of ben_cox's package with temp workaround for file references

#list of packages used
p<- c("dplyr", "RCurl", "purrr", "tibble", "stringr", "DBI", "readr", "rvest", "RSQLite", "lubridate",
      "tidyverse", "jsonlite", "magrittr", "httr", "rmisr", "rRMIS", "doParallel", "knitr", "kableExtra",
      "nortest", "odbc"
)

#pass list of packages in object p. Then we pass object p using lapply to load required packages

lapply(p, require, character.only = TRUE)


#############################release data
rel <- get_release_data(first_by=1975, last_by=2024) %>% #set brood year range for releases
  filter(species_name == "Chinook")

###translation function and prep for tag rate calc
rel2 <- rel %>% 
  releases_for_tr()

##################tag rate calc for same data both Mark selective and non-mark selective rates. can be used to extract total
#release download
j <- juv_tr_lut(first_by=1975, last_by=2024, species_name == "Chinook")

#####release cleanup- taking the rel data and trim it to just Cowlitz Salmon

#RMIS lookup tables
download_luts

#tag codes of r
cowrel <- rel2 %>% 
  filter(brood_year %in% 2000:2020,
         species_name == "Chinook",
         run_name == "Fall") %>% 
  # filter(hatchery %in% c("COWLITZ SALMON HATCHERY",
  #                        "BIG CR HATCHERY",
  #                        "KALAMA FALLS HATCHERY",
  #                        "FALLERT CR HATCHERY",
  #                        "NORTH TOUTLE HATCHERY",
  #                        "WASHOUGAL HATCHERY"
  #                        )) %>% 
  # #filter(tag_code_or_release_id %in% c(636854,
  #                                      637005,
  #                                      637106,
  #                                      211178,
  #                                      211179,
  #                                      637051,
  #                                      637052,
  #                                      637195,
  #                                      637196,
  #                                      636855,
  #                                      637011,
  #                                      637091,
  #                                      211222,
  #                                      211369,
  #                                      636770,
  #                                      211223,
  #                                      211254,
  #                                      636771,
  #                                      637080,
  #                                      211253)) %>% 
  rename(TagCode = tag_code_or_release_id) %>% 
  left_join(j, by = "TagCode")#joins estimated tag rates based on the juv_tr_lut calculations. for some years this is clean, others needed to be calculated manually

cowrel2 <- cowrel %>% #estimate total releases from the tag rate calculation
  filter(!is.na(TR)) %>% 
  filter(TagRateType == "NonMS" | is.na(TagRateType)) %>%  
  group_by(TagCode) %>% 
  #mutate(total_release = round(Ad_CWT / TR)) %>% 
  ungroup() %>% 
  filter(run_name == "Fall")



#######create date aggregation variable to represent release groups in the barging study

# Function to group dates as a release group, change 'days =' value to increase or decrease interval
group_within_days <- function(dates, days = 14) {
  groups <- numeric(length(dates))
  current_group <- 1
  for (i in seq_along(dates)) {
    if (i == 1 || (dates[i] - dates[i-1]) > days(days)) {
      current_group <- current_group + 1
    }
    groups[i] <- current_group
  }
  groups
}



# Function to group dates based on Julian dates
group_within_julian_days <- function(julian_dates, julian_interval = 14) {
  groups <- numeric(length(julian_dates))
  current_group <- 1
  
  for (i in seq_along(julian_dates)) {
    if (i == 1 || (julian_dates[i] - julian_dates[i-1]) > julian_interval) {
      current_group <- current_group + 1
    }
    groups[i] <- current_group
  }
  groups
}

# Using the release grouping function
cowrel3 <- cowrel2 %>%
  mutate(LastRelDate = ymd(LastRelDate),
         first_release_date = ymd(first_release_date)) %>%
  mutate(last_rel_date_jd = yday(LastRelDate),
         first_rel_date_jd = yday(first_release_date)) %>% 
  group_by(brood_year) %>% 
  arrange(brood_year, last_rel_date_jd) %>%  # Ensure dates are sorted within each group
  mutate(release_group = group_within_julian_days(last_rel_date_jd, 14))


#group by release group to get summed tags released
cowrel4 <- cowrel3 %>%
  mutate(hatchery = case_when(
    TagCode %in% c(636270,
                   636793,
                   636921,
                   637153,
                   637341) ~ "MAYFIELD NET PEN",
    TRUE ~ hatchery
  )) %>%
  group_by(TagCode) %>%
  mutate(total_cwt_rel = sum(Ad_CWT, Unclipped_CWT)) %>%#create column of total cwts released
  ungroup() %>% 
  select(brood_year,
         TagCode,
         hatchery,
         species_name,
         run_name,
         stock,
         psc_region,
         avg_weight,
         avg_length,
         first_release_date,
         LastRelDate,
         Ad_CWT,
         Unclipped_CWT,
         total_cwt_rel,
         release_site,
         release_year,
         last_rel_date_jd,
         release_group) %>%
  filter(run_name == "Fall",
         psc_region == "LOCR") %>% 
  group_by(across(-c(TagCode, avg_weight, Ad_CWT, Unclipped_CWT, total_cwt_rel))) %>%
  mutate(tag_group = paste(TagCode, collapse = ", ")) %>% #create new row with tags belonging to a specific group
  ungroup() #%>%
# distinct() %>% 
# group_by(tag_group) %>% 
# mutate(Ad_CWT = sum(Ad_CWT),
#        Unclipped_CWT = sum(Unclipped_CWT),
#        avg_weight_rel = round(mean(avg_weight)),
#        avg_length_rel = round(mean(avg_length)),
#        total_cwt_rel = sum(total_cwt_rel)) %>%
# ungroup() %>% 
# select(!c(#TagCode, 
#   avg_weight,
#   Ad_CWT, 
#   Unclipped_CWT)) %>% 
# distinct() %>% 
# ungroup()

cowrel5 <- cowrel4 %>% 
  #select(!TagCode) %>% 
  distinct()

# Check if the "outputs" folder exists, and create it if it doesn't
if (!dir.exists("./outputs")) {
  dir.create("./outputs")
}

#write .csv of release data
write.csv(cowrel5, "./outputs/Cowlitz Salmon FaCk releases.csv")

####released tags in the South sound net pens barging study SSNPBS

cow_alltags <- data.frame(cowrel3$TagCode)

##################################################
######################recoveries via rRMIS

#this pulls all recoveries from brood years 1975-2018 regardless of any other filters.
#the files can be quite large, so setting bounds is recommended. by_brood references brood year rather than run year

download_recoveries(1975, 2024, by_brood = TRUE)

d_path <- "./RMIS/Recoveries"

#create file list of downloaded recovery files
rec_files <- list.files(path = d_path, pattern = "*.csv", full.names = TRUE)

#create single dataframe of all recoveries, and filter irrelevant data
r2 <- do.call(rbind, lapply(rec_files, function(x) read.csv(x, stringsAsFactors = FALSE)))

r3 <- r2 %>% #filter by tag codes that are part of this study
  filter(tag_code %in% cow_alltags$cowrel3.TagCode) %>% 
  filter(tag_status == 1) %>%# remove un-confirmed recoveries
  mutate_all(as.character)

remove(r2)

#tidying function translates several of RMIS's numerical codes into descriptive text
r4 <- tidy_recoveries(r3) %>%
  mutate(species_name = case_when(#species #2 is coho
    species == 1 ~ "Chinook",
    TRUE ~ species
  ))

r5 <- r4 %>%#join release data to the recovery data
  left_join(cowrel4, by = c("tag_code" = "TagCode"))

r6 <- r5 %>% #isolate the mayfield net pen tags
  mutate(hatchery = case_when(
    tag_code %in% c(636270,
                    636793,
                    636921,
                    637153,
                    637341) ~ "MAYFIELD NET PEN",
    TRUE ~ hatchery)) %>% 
  filter(!fishery %in% c(70:79))####this is juvenile sampling, and are excluded as adult recoveries

r7 <- r6 %>%
  mutate(age_at_rec = (as.numeric(run_year) - as.numeric(brood_year))) %>% 
  #trim to relevant fields
  select(recovery_id,
         brood_year,
         run_year,
         hatchery,
         species_name.x,
         release_site,
         release_group,
         first_release_date,
         LastRelDate,
         avg_weight,
         #avg_weight_rel,
         tag_code,
         length,
         age_at_rec,
         fishery,
         fishery_name,
         mgt_fishery,
         catch_sample_id,
         recovery_date,
         recovery_location_code,
         recovery_location,
         description,
         tag_group,
         number_cwt_estimated,
         total_cwt_rel,
         last_rel_date_jd
  ) %>% 
  rename(species_name = species_name.x) |> 
  mutate(number_cwt_estimated_trad = ifelse(is.na(number_cwt_estimated), 0, number_cwt_estimated)) %>%
  mutate(number_cwt_estimated = ifelse(number_cwt_estimated_trad == 0, 1, number_cwt_estimated_trad)) %>% 
  mutate(number_cwt_estimated = as.numeric(number_cwt_estimated),
         number_cwt_estimated_trad = as.numeric(number_cwt_estimated_trad)) %>%
  #group_by(brood_year) %>% 
  # mutate(release_group = (categorize_values(release_group))) %>% 
  ungroup()

# Variance calculations from catch_sample data

# Catch_sample data for variance calcs
cat_sam <- r7 %>% 
  select(catch_sample_id) %>% 
  na.omit() %>% #remove nas
  filter(!catch_sample_id == "") %>% #remove blanks
  distinct()


# API key
token <- Sys.getenv("my_rmis")

# loop for catch samples

# vectorize the tag codes for the release loop
cs <- cat_sam$catch_sample_id

#initialize empty data frame to store release results
cs_df <- data.frame()

csdf2 <- cs_df |> 
  filter(is.na(number_caught) | is.na(number_sampled))

write.csv(csdf2, "./catch sample records with missing fields.csv")
###for loop that iterates and compiles release data from all recovered tag codes
for(catch_sample_id in cs){
  # Using tryCatch to handle potential errors
  cs <- tryCatch({
    get_catchsample(token = token, catch_sample_id = catch_sample_id)
  }, error = function(e) {
    # If an error occurs, return an empty data frame
    return(data.frame())
  })
  
  
  if (nrow(cs) > 0) {
    
    
    # Bind the data frames
    cs_df <- rbind(cs_df, cs)
  }
}


# Catch sample data cleanup
# API went down so I had to pull raw csvs, 3 
cscsv1 <- read.csv("~/R/Cowlitz Fall Chinook/2024 fall chinook review/CSV9952.csv")
cscsv2 <- read.csv("~/R/Cowlitz Fall Chinook/2024 fall chinook review/CSV9988.csv")
cscsv3 <- read.csv("~/R/Cowlitz Fall Chinook/2024 fall chinook review/CSV10011.csv")

# Single data frame of catch sample data
cs_csvs <- rbind(cscsv1, cscsv2, cscsv3) |> 
  rename(run_year = catch_year) |> 
  filter(run_year %in% c(2000:2024)) |> 
  select(catch_sample_id,
         sampling_agency,
         run_year,
         catch_location_code,
         fishery,
         species,
         number_caught,
         number_sampled
  ) |> 
  distinct() |> 
  mutate(run_year = as.character(run_year),
         fishery = as.character(fishery)) |> 
  right_join(r7, by = c("catch_sample_id", "run_year", "fishery"))

###### MOVE TO MODEL PREP.R AFTER RUNNING LINE 320 ##########


#############################
cat_sam2 <- cs_df %>% 
  rename(run_year = catch_year) %>% 
  group_by(run_year, catch_sample_id) %>% 
  mutate(sample_frac = number_sampled / number_caught) %>% 
  filter(run_year %in% c(2000:2024)) %>% # trim to relevant years
  select(catch_sample_id,
         sampling_agency,
         run_year,
         catch_location_code,
         fishery,
         species,
         number_caught,
         number_sampled,
         sample_frac#,
         #number_recovered_decoded,
         #number_cwt_estimated,
         #number_recovered_no_cwts,
         #number_recovered_unreadable
  ) %>% 
  mutate(run_year = as.character(run_year)) #%>% 
#filter(sample_frac <= 1)

cat_sam3 <- cat_sam2 %>% 
  right_join(r7, by = c("catch_sample_id", "run_year", "fishery"))



######cowlitz variance testing
var_1 <- cat_sam3 %>% 
  #filter(tag_code == 635694) %>% 
  group_by(catch_sample_id,
           run_year,
           fishery)
#tag_code#,
#recovery_location,
#recovery_location_code
#          ) %>% # group by sampling strata, event, year
# mutate(total_catch = sum(number_caught),
#        total_sample = sum(number_sampled),
#        raw_tags_by_sample = n(),
#        total_estimated_tags = sum(number_cwt_estimated))

var_2 <- var_1 %>% 
  select(catch_sample_id,
         brood_year,
         run_year,
         total_cwt_rel,
         #recovery_location,
         number_caught,
         number_sampled,
         raw_tags_by_sample,
         total_estimated_tags) %>% 
  distinct() %>% 
  filter(!catch_sample_id == "") %>%
  na.omit() %>%
  ungroup()
#mutate(total_estimated_tags = round(total_estimated_tags))


var_3 <- var_2 %>%
  group_by(catch_sample_id) %>% 
  mutate(prop_rec = total_estimated_tags / total_cwt_rel) %>% 
  ungroup() %>% 
  mutate(mean_tags = total_estimated_tags / number_sampled) %>% 
  mutate(variance_by_event = (total_estimated_tags * (1 - mean_tags)) / number_sampled) %>% 
  mutate(total_means_by_tag = sum(mean_tags),
         total_var_by_tag = sum(variance_by_event)) %>% 
  group_by(catch_sample_id) %>% 
  mutate(sample_frac = number_sampled / number_caught) %>% 
  filter(number_caught > 0) %>% 
  filter(sample_frac <= 1)



# Calculate SAR and variance for each brood year
results <- var_3 %>%
  group_by(brood_year) %>%
  summarise(
    total_estimated_tags = sum(total_estimated_tags),
    total_cwt_rel = total_cwt_rel,
    # Use mean or other appropriate statistic for sample_frac if needed
    sample_frac = sample_frac,  
    
    SAR = calculate_SAR(total_estimated_tags, L, total_cwt_rel),
    variance_SAR = calculate_SAR_variance(total_estimated_tags, sample_frac, L, total_cwt_rel, SAR),
    lower_CI = calculate_confidence_interval(SAR, variance_SAR)[1],
    upper_CI = calculate_confidence_interval(SAR, variance_SAR)[2]
  )

# Plotting using ggplot2
ggplot(results, aes(x = brood_year, y = SAR)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +
  labs(title = "Smolt-to-Adult Ratio (SAR) by Brood Year",
       x = "Brood Year",
       y = "Smolt-to-Adult Ratio (SAR)") +
  theme_minimal() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

###########binomial proportion confidence interval- Wilson Interval

n <- var_3 %>% 
  select(total_cwt_rel) %>% 
  distinct()


n2 <- n$total_cwt_rel


x <- sum(var_3$total_estimated_tags)


p_hat <- (x / n) 

sar <- 100 * p_hat

z <- qnorm(0.975)

lower_bound <- round((p_hat + z^2 / (2 *n) - (z * sqrt((p_hat * (1 - p_hat) + z^2 / (4*n)) / n))) / (1 + (z^2 / n)), digits = 5)
upper_bound <- round((p_hat + z^2 / (2 *n) + (z * sqrt((p_hat * (1 - p_hat) + z^2 / (4*n)) / n))) / (1 + (z^2 / n)), digits = 5)

print(paste("95% Confidence Interval (Binomial):", lower_bound, "to", upper_bound))


# Total number of tags released
n <- sum(var_3$total_cwt_rel)

# Sum of total estimated tags recovered
x <- sum(var_3$total_estimated_tags)

# Proportion of tags recovered
p_hat <- x / n 

# Z-value for a 95% confidence interval
z <- qnorm(0.975)

# Calculate the Wilson score interval bounds
lower_bound <- ((p_hat + (z^2 / (2 * n)) - (z * sqrt((p_hat * (1 - p_hat) + (z^2 / (4 * n))) / n))) / (1 + (z^2 / n)))
upper_bound <- ((p_hat + (z^2 / (2 * n)) + (z * sqrt((p_hat * (1 - p_hat) + (z^2 / (4 * n))) / n))) / (1 + (z^2 / n)))

# Output the results
cat("Proportion of tags recovered (p_hat):", p_hat, "\n")
cat("Confidence Interval: [", lower_bound, ",", upper_bound, "]\n")

#############glm approach


##########################model input datasets##################################

####summary of total recoveries by release group/site
mi_total <- r7 %>%
  group_by(tag_group) %>% 
  mutate(raw_recovered = n()) %>% #counts rows based on groups, each line is 1 tag, row counts work to represent
  #raw tags
  mutate(exp_cwt_estd_sum_trad = round(sum(number_cwt_estimated_trad))) %>% #excludes un-expanded values
  mutate(exp_cwt_estd_sum = round(sum(number_cwt_estimated))) %>%
  mutate(total_unexpanded_tags = exp_cwt_estd_sum - exp_cwt_estd_sum_trad) %>% #includes raw value when expanded is not available
  ungroup() %>%
  select(brood_year,#selecting relevant fields
         hatchery,
         release_site,
         release_group,
         first_release_date,
         LastRelDate,
         last_rel_date_jd,
         avg_weight_rel,
         raw_recovered,
         exp_cwt_estd_sum,
         exp_cwt_estd_sum_trad,
         total_unexpanded_tags,
         total_cwt_rel,
         tag_group) %>%
  arrange(desc(brood_year)) %>% 
  distinct()

##########################value categorization function

categorize_values <- function(x) {
  if (length(x) == 1) {
    return("normal")
  } else if (length(x) == 2) {
    ordered_x <- x[order(x)]
    categories[ordered_x == min(ordered_x)] <- "normal"
    categories[ordered_x == max(ordered_x)] <- "late"
    return(categories)
  } else {
    ordered_x <- x[order(x)]
    categories <- rep("normal", length(x))
    categories[ordered_x == min(ordered_x)] <- "early"
    categories[ordered_x == max(ordered_x)] <- "late"
    return(categories)
  }
}

#############################first data set- all standard releases
mi_timing_sar <- mi_total %>%
  filter(brood_year %in% 2009:2018) %>% 
  filter(hatchery %in% c("COWLITZ SALMON HATCHERY",
                         "MAYFIELD NET PEN")) %>% 
  group_by(brood_year) %>%
  #mutate(release_cat = (categorize_values(release_group))) %>% 
  ungroup() %>%
  group_by(#tag_group,
    hatchery,
    brood_year) %>%
  mutate(sar = (exp_cwt_estd_sum / total_cwt_rel) * 100)


plot(mi_timing_sar$brood_year, mi_timing_sar$sar)

mod1 <- lm(sar ~ brood_year, data = mi_timing_sar)

abline(mod1)

summary(mod1)

#total recoveries aggregated by the above criterion
write.csv(mi_timing_sar, "./outputs/cowlitz main all recoveries.csv")

################################################################################


mi_timing_sar_mf <- mi_total %>%
  filter(hatchery == "MAYFIELD NET PEN") %>% 
  group_by(brood_year) %>%
  mutate(release_cat = (categorize_values(release_group))) %>% 
  ungroup() %>%
  group_by(tag_group) %>%
  mutate(sar = (exp_cwt_estd_sum / total_cwt_rel) * 100)

ggplot(mi_timing_sar, aes(x = brood_year, y = sar, color = hatchery, group = hatchery)) +
  geom_line() +
  geom_point() +
  # geom_ribbon(aes(ymin = sar - SE, ymax = sar + SE), alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +  #add lines of best fit
  labs(x = "Brood Year", y = "SAR") +
  ggtitle("Fall Chinook SAR by Brood Year and Hatchery- 2009 to 2018")


plot(mi_timing_sar_mf$brood_year, mi_timing_sar_mf$sar)

mod2 <- lm(sar ~ brood_year, data = mi_timing_sar_mf)
abline(mod2)

summary(mod2)

write.csv(mi_timing_sar, "./outputs/mayfield all recoveries.csv")


# data set 2, all recoveries from all comparable hatcheries for years 2009-2018

mi_total_sar <- mi_total %>%
  filter(brood_year %in% 2009:2018) %>% 
  group_by(brood_year) %>%
  # mutate(release_cat = (categorize_values(release_group))) %>% 
  ungroup() %>%
  group_by(hatchery, brood_year) %>%
  mutate(sar = (sum(exp_cwt_estd_sum) / sum(total_cwt_rel)) * 100) %>%
  ungroup() %>% 
  group_by(hatchery, brood_year) %>% 
  mutate(SE = sd(sar) / sqrt(n())) %>%
  ungroup()

