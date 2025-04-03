#########data retreival using two rmis api workarounds, run this script first, then model prep, then plots.r
remotes::install_github("Ben-Cox/rRMIS")
devtools::install_github("KJPWarner/RMISr")

#list of packages used
p<- c("dplyr", "RCurl", "purrr", "tibble", "stringr", "DBI", "readr", "rvest", "RSQLite", "lubridate",
      "tidyverse", "jsonlite", "magrittr", "httr", "rmisr", "rRMIS", "doParallel", "knitr", "kableExtra",
      "nortest", "odbc"
)

#pass list of packages in object p, several of these are not used in this analysis

lapply(p, require, character.only = TRUE)


#############################release data
rel <- get_release_data(first_by=1975, last_by=2024) |> #set brood year range for releases
  filter(species_name == "Chinook")

###translation function and prep for tag rate calc
rel2 <- rel |> 
  releases_for_tr()

##################tag rate calc for same data both Mark selective and non-mark selective rates. can be used to extract total
#release download
j <- juv_tr_lut(first_by=1975, last_by=2024, species_name == "Chinook")


#tag codes of releases, for retrieving recoveries
cowrel <- rel2 |> 
  filter(brood_year %in% 2000:2019,
         species_name == "Chinook",
         run_name == "Fall")  |>  
  rename(TagCode = tag_code_or_release_id) |> 
  left_join(j, by = "TagCode")#joins estimated tag rates based on the juv_tr_lut calculations. for some years this is clean, others needed to be calculated manually

#trim down first unneeded rows
cowrel2 <- cowrel |> #estimate total releases from the tag rate calculation
  filter(!is.na(TR)) |> 
  filter(TagRateType == "NonMS" | is.na(TagRateType))#filter down to non-mark selective counts


#group by release group to get summed tags released
cowrel3 <- cowrel2 |>
  mutate(hatchery = case_when(
    TagCode %in% c(636270,
                   636793,
                   636921,
                   637153,
                   637341) ~ "MAYFIELD NET PEN", #mayfield net pen cwts flag as cowlitz hatchery proper, this isolates those tags
    TRUE ~ hatchery
  )) |>
  mutate(last_release_date = ymd(LastRelDate),
         first_release_date = ymd(as.character(first_release_date))) |> 
  group_by(TagCode) |>
  mutate(total_cwt_rel = sum(Ad_CWT, Unclipped_CWT)) |>#create column of total cwts released
  ungroup() |> 
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
         release_year) |>
  filter(psc_region == "LOCR") |> 
  distinct() 

#check if the "outputs" folder exists, and create it if it doesn't
if (!dir.exists("./outputs")) {
  dir.create("./outputs")
}

#write .csv of release data
write.csv(cowrel3, "./outputs/fall chinook releases.csv")

####vector of released tags in the data frame

cow_alltags <- data.frame(cowrel3$TagCode)

##################################################
######################retrieving recoveries via rRMIS

#this pulls all recoveries from brood years selected regardless of any other filters.
#the files can be quite large, so setting bounds is recommended. by_brood references brood year rather than run year

download_recoveries(1975, 2019, by_brood = TRUE)

##designate dowloand path for making a single dataframe
d_path <- "./RMIS/Recoveries"

#create file list of downloaded recovery files
rec_files <- list.files(path = d_path, pattern = "*.csv", full.names = TRUE)

#create single dataframe of all recoveries, and filter irrelevant data
r2 <- do.call(rbind, lapply(rec_files, function(x) read.csv(x, stringsAsFactors = FALSE)))

r3 <- r2 |> #filter by tag codes that are part of this study
  filter(tag_code %in% cow_alltags$cowrel3.TagCode) |> 
  filter(tag_status == 1) |>#remove un-confirmed recoveries, this can include misreads, mismatch species id, etc
  mutate_all(as.character)

#remove(r2) can uncomment and run this to remove the unfiltered data object, it's memory intensive

#tidying function translates several of RMIS's numerical codes into descriptive text
r4 <- tidy_recoveries(r3) |>
  mutate(species_name = case_when(#species #2 is coho
    species == 1 ~ "Chinook",
    TRUE ~ species
  ))

r5 <- r4 |>#join release data to the recovery data
  left_join(cowrel3, by = c("tag_code" = "TagCode"))

r6 <- r5 |> #isolate the mayfield net pen tags
  mutate(hatchery = case_when(
    tag_code %in% c(636270,
                    636793,
                    636921,
                    637153,
                    637341) ~ "MAYFIELD NET PEN",
    TRUE ~ hatchery)) |> 
  filter(!fishery %in% c(70:79))####this is juvenile sampling, and are excluded as adult recoveries

r7 <- r6 |>
  mutate(age_at_rec = (as.numeric(run_year) - as.numeric(brood_year))) |> 
  #trim to relevant fields
  select(recovery_id,
         brood_year,
         run_year,
         hatchery,
         species_name.x,
         release_site,
         first_release_date,
         LastRelDate,
         avg_weight,
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
         number_cwt_estimated,
         total_cwt_rel
  ) |> 
  rename(species_name = species_name.x) |> 
  mutate(number_cwt_estimated_trad = ifelse(is.na(number_cwt_estimated), 0, number_cwt_estimated)) |> #replace nas with 0s to allow for summation- this is an ongoing data issue
  mutate(number_cwt_estimated = ifelse(number_cwt_estimated_trad == 0, 1, number_cwt_estimated_trad)) |> #same as above, but substitute 0 for 1, essentially raw count of tags in lieu of unexpanded
  mutate(number_cwt_estimated = as.numeric(number_cwt_estimated),
         number_cwt_estimated_trad = as.numeric(number_cwt_estimated_trad)) |>
  ungroup()


#catch_sample extract available catch sample records
cat_sam <- r7 |> 
  select(catch_sample_id) |> 
  na.omit() |> #remove nas
  filter(!catch_sample_id == "") |> #remove blanks
  distinct()


# API key is needed to access the rmis API. Contact admins to receive access
#token <- Sys.getenv("my_rmis")

# loop for catch samples- commented out, but functional

# # vectorize the tag codes for the release loop
# cs <- cat_sam$catch_sample_id
# 
# #initialize empty data frame to store release results
# cs_df <- data.frame()
# 
# csdf2 <- cs_df |> 
#   filter(is.na(number_caught) | is.na(number_sampled))
# 
# write.csv(csdf2, "./catch sample records with missing fields.csv")
# ###for loop that iterates and compiles release data from all recovered tag codes
# for(catch_sample_id in cs){
#   # Using tryCatch to handle potential errors
#   cs <- tryCatch({
#     get_catchsample(token = token, catch_sample_id = catch_sample_id)
#   }, error = function(e) {
#     # If an error occurs, return an empty data frame
#     return(data.frame())
#   })
#   
#   
#   if (nrow(cs) > 0) {
#     
#     
#     # Bind the data frames
#     cs_df <- rbind(cs_df, cs)
#   }
# }


# catch sample data cleanup
# API went down so I had to pull raw csvs from https://www.rmpc.org/
cscsv1 <- read.csv("./data/CSV9952.csv")
cscsv2 <- read.csv("./data/CSV9988.csv")
cscsv3 <- read.csv("./data/CSV10011.csv")

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

###### MOVE TO MODEL PREP.R #############
