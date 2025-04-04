library(viridis)# colorblind-friendly color palettes
#create the directory if it doesn't already exist
if (!dir.exists("./figures")) {
  dir.create("./figures", recursive = TRUE)
}

#pdf device to save all plots
pdf(file = "./figures/all_plots.pdf", width = 11, height = 8.5) # Standard landscape letter size

# plot 1- error histogram by sampling agency
ggplot(sample_errs, aes(x = sampling_agency)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Sampling Agency Errors",
    x = "Sampling Agency",
    y = "Number of Occurrences"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Histogram of hatchery counts
ggplot(sample_errs2, aes(x = hatchery)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Sampling Hatchery Errors",
    x = "Hatchery",
    y = "Number of Occurrences"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 3: Histogram of run year error counts
ggplot(sample_errs3, aes(x = run_year)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Run Years with Errors",
    x = "Year",
    y = "Number of Occurrences"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 4: Histogram of fishery error counts
ggplot(sample_errs3, aes(x = mgt_fishery)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Fisheries with Errors",
    x = "Fishery",
    y = "Number of Occurrences"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot 5: histogram of strata used in the relative sar calculation
ggplot(model_inputs1, aes(x = mgt_fishery, fill = mgt_fishery %in% c("Escapement", "Buoy 10 Sport", "Freshwater Net"))) +
  geom_bar(color = "black") +
  scale_fill_manual(
    values = c("TRUE" = "pink", "FALSE" = "skyblue"),
    labels = c("TRUE" = "Included in analysis", "FALSE" = "Excluded from analysis")
  ) +
  labs(
    title = "Number of recovery records by management fishery",
    x = "Fishery",
    y = "Number of Occurrences",
    fill = "Strata"  
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot 6: Plot of relative SARs (log-transformed)
graphic_inputs <- model_inputs2 |> 
  group_by(hatchery, brood_year) |> 
  mutate(mean_sar = mean(relative_sar))

ggplot(graphic_inputs, aes(x = brood_year, y = log(mean_sar), color = hatchery, group = hatchery)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  #scale_color_viridis_d(option = "B", end = 0.9) +
  labs(x = "Brood Year", y = "log-SAR") +
  ggtitle("Log-transformed Relative Fall Chinook SAR by Brood Year and Hatchery") +
  theme_minimal()

# Plot 7: Colorblind-friendly (log-transformed relative SAR)
custom_colors <- c(
  "NORTH TOUTLE HATCHERY" = "gray",
  "COWLITZ SALMON HATCHERY" = "black",
  "MAYFIELD NET PEN" = "darkgray"
)

remaining_colors <- setNames(
  viridis(length(setdiff(unique(graphic_inputs$hatchery), names(custom_colors))), option = "C"),
  setdiff(unique(graphic_inputs$hatchery), names(custom_colors))
)

combined_colors <- c(custom_colors, remaining_colors)

ggplot(graphic_inputs, aes(x = brood_year, y = log(mean_sar), color = hatchery, group = hatchery)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = combined_colors) +
  labs(x = "Brood Year", y = "log(SAR)") +
  ggtitle("Log Transformed Fall Chinook Relative SAR by Brood Year and Hatchery") +
  theme_minimal()

# Plot 7: Colorblind-friendly untransformed SAR
ggplot(graphic_inputs, aes(x = brood_year, y = mean_sar, color = hatchery, group = hatchery)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = combined_colors) +
  labs(x = "Brood Year", y = "SAR") +
  ggtitle("Fall Chinook SAR by Brood Year and Hatchery") +
  theme_minimal()

#grouped and tiled barchart by year for exploitation rates

fc2 <- fc |>
  mutate(fishery_name = case_when(fishery_name %in% c("Hake Trawl Fishery, At-Sea component (CA/OR/WA)",
                                       "Hake Trawl Fishery, Shoreside component (OR/WA)") ~ "Hake Trawl",
                                  fishery_name %in% c("Sport (charter)",
                                                      "Sport (private)") ~ "Offshore Sport",
                                  fishery_name %in% c("Ocean Troll - Day Boat",
                                                      "Ocean Troll (non-treaty)") ~"Ocean Troll",
                   TRUE ~ fishery_name))
  
ggplot(fc2, aes(x = brood_year, y = percent_rec)) +
  geom_bar(stat = "identity",
           aes(fill = fishery_name)
  ) +
  scale_fill_viridis_d(option = "C")+
  facet_wrap(~hatchery) +
  labs(title = "Proportional Fisheries Exploitation",
       x = "Brood Year", 
       y = "Recovery by fishery (% total tags recovered by brood year)") +
  theme(legend.title = element_blank())#removes title from the legend

#close PDF device
dev.off()
