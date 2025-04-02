#further tidying and summation of a consistent tags-recovered subset for calculating relative sar point estimates

clean_rec1 <- cs_csvs|> 
  mutate(run_year = as.character(run_year)) |> 
  select(recovery_id,
         catch_sample_id,
         sampling_agency,
         run_year,
         brood_year,
         fishery_name,
         mgt_fishery,
         species_name,
         hatchery,
         release_site,
         first_release_date,
         avg_weight,
         tag_code,
         length,
         age_at_rec,
         recovery_date,
         recovery_location_code,
         recovery_location,
         description,
         number_caught,
         number_sampled,
         number_cwt_estimated,
         number_cwt_estimated_trad,
         total_cwt_rel)

# bring in cleaned data from the CAM work that took place last year. This should shore up a ton of the unexpanded
# recoveries in the lower columbia

patched_locr_catsam <- read.csv("./data/cleaned escapement recoveries v1.csv") |> 
  filter(tag_code %in% clean_rec1$tag_code)

# Select the relevant columns from df2 before the join
patched_subset <- patched_locr_catsam |> 
  mutate(run_year = as.character(run_year)) |> 
  select(catch_sample_id,
         CAM_name,
         escapement_location_name,
         sampling_agency,
         run_year,
         recovery_id,
         fishery,
         X1.p,
         Number_Caught,
         Number_Sampled,
         one_over_p_trad,
         one_over_p)

# join based on identifying criterion
clean_rec2 <- clean_rec1 |> 
  left_join(patched_subset, by = c("catch_sample_id",
                                   "recovery_id",
                                   "run_year",
                                   "sampling_agency"))

clean_rec3 <- clean_rec2 |> 
  #filter(number_caught != Number_Caught, number_sampled != Number_Sampled) |> 
  filter(escapement_location_name == "lower cowlitz")

#export from data.wa.gov of lower cowlitz fall chinook estimates to provide more accurate terminal expansions than those provided
spi <- read.csv("./data/SPI_Escapement_20250326.csv")

#where available, prioritize the TOTAL SPAWNER ABUNDANCE EXCLUDING JACKS (TSAEJ) derived from the jolly seber model, then next best estimate
spi2 <- spi |>
  filter(Population.Name == "Lower Cowlitz Fall (Tule) Chinook",
         Data.Type %in% c("TSAEJ"#, use estimate excluding jacks
                          #"TSAIJ"
         )
  )|>
  group_by(Year) |>
  filter(
    any(Biologist.Methodology.Description == "2021-2023 New Method = Jolly Seber Mark Recapture.") |
      Biologist.Methodology.Description == "Bias-corrected peak redd count expansions using 2021-2023 mark-recapture estimates."
  ) |>
  slice(which.max(Biologist.Methodology.Description == "2021-2023 New Method = Jolly Seber Mark Recapture.")) |>
  ungroup() |>
  mutate(escapement_location_name = "lower cowlitz") |>
  rename(run_year = Year) |> 
  mutate(run_year = as.character(run_year))


########### clean up and tag recovery summations
model_inputs1 <- clean_rec2 |> 
  left_join(spi2, by = c("run_year", "escapement_location_name")) |> 
  janitor::clean_names() |> 
  filter(!is.na(hatchery),
         !hatchery == "NATURAL PRODUCTION TAGGIN") |> 
  mutate(number_caught_best = coalesce(abundance_quantity,
                                       number_caught_2,
                                       number_caught),
         number_sampled_best = coalesce(number_sampled_2,
                                        number_sampled)) |> 
  select(-c(#recovery_location_code,
    number_caught,
    number_sampled,
    number_caught_2,
    number_sampled_2, #trimming irrelevant fields
    population_name,
    species,
    abundance_quantity,
    #data_type,
    production_type,
    calculation_type,
    escapement_methodology,
    escapement_methodology_description,
    biologist_methodology_description,
    comments,
    report_types,
    last_updated)) |> 
  mutate(
    life_stage = if_else(
      mgt_fishery %in% c("freshwater rec", "freshwater sport", "escapement") & age_at_rec < 3,
      "jack",
      "adult"
    )
  ) |> 
  mutate(
    fishery_2 = if_else(
      is.na(mgt_fishery), fishery_name, mgt_fishery
    )
  ) |> 
  mutate(sample_fraction = number_sampled_best / number_caught_best,
         expanded_cwt_best = round((1 / sample_fraction), 2)) |>  #calculating sampling fractions based on best available data, for terminal areas this is a function of escapement estimates as the denominator
  mutate(num_diff = abs(number_cwt_estimated - expanded_cwt_best))


write.csv(model_inputs1,"./outputs/fall chinook locr sar data.csv")


#### plots.r contains diagnostic histograms of frequency of sampling errors. 
#### upon examination, fisheries from CDFO, and returns from Big creek hatchery have been excluded from the analysis

sample_errs <- model_inputs1 |> 
  filter(sample_fraction > 1 | is.na(sample_fraction))

######################## Final clean up and summation for modeling

model_inputs2 <- model_inputs1 |> 
  filter(!sampling_agency == "CDFO",# problematic factors excluded to not introduce noise into the sar comparison
         !hatchery == "BIG CR HATCHERY", # same as above
         !run_year == 2004) |> # again
  mutate(raw_tags_recovered = 1) |> 
  mutate(expanded_cwt_best = coalesce(expanded_cwt_best,
                                      raw_tags_recovered)) |> # use raw number count when sampling expansions are not available
  filter(!expanded_cwt_best == Inf) |> ###exclude sampling fractions that are dividing by 0
  group_by(tag_code,
           mgt_fishery#,
           #brood_year
  ) |> 
  reframe(estimated_tags_by_fishery = sum(expanded_cwt_best),# sum recoveries by fishery, rest is retaining necessary effect fields
          raw_tags_by_fishery = sum(raw_tags_recovered),
          hatchery = hatchery,
          brood_year = brood_year,
          total_cwt_rel = total_cwt_rel,
          avg_weight = avg_weight,
          release_site = release_site,
          first_release_date = first_release_date
  ) |>
  ungroup() |> 
  distinct() |> 
  group_by(tag_code) |> 
  mutate(total_cwt_estimated = sum(estimated_tags_by_fishery),
         total_raw_tags = sum(raw_tags_by_fishery),
         relative_sar = round(total_cwt_estimated / total_cwt_rel, 7)) |> #relative sars calculated as total recovered / total released
  select(-c(estimated_tags_by_fishery,
            mgt_fishery,
            raw_tags_by_fishery)# drop the fisheries field
  ) |>
  ungroup() |> 
  distinct() |> # remove repeated rows
  arrange(hatchery, brood_year) |> 
  filter(!hatchery %in% c("LYONS FERRY HATCHERY",
                          "N BONNEVILLE LADDER",
                          "SEA RESOURCES HATCH")) #remove the one-off hatcheries. each of these only had one or two years of LOCR releases//



write.csv(model_inputs2,"./outputs/fall chinook relative sar data.csv")

################## move to plots.R###################