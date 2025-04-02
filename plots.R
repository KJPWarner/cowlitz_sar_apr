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
ggplot(sample_errs, aes(x = hatchery)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Sampling Hatchery Errors",
    x = "Hatchery",
    y = "Number of Occurrences"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 3: Histogram of run year error counts
ggplot(sample_errs, aes(x = run_year)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Run Years with Errors",
    x = "Year",
    y = "Number of Occurrences"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 4: Histogram of fishery error counts
ggplot(sample_errs, aes(x = mgt_fishery)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Fisheries with Errors",
    x = "Fishery",
    y = "Number of Occurrences"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 5: Plot of relative SARs (log-transformed)
graphic_inputs <- model_inputs2 |> 
  group_by(hatchery, brood_year) |> 
  mutate(mean_sar = mean(relative_sar))

ggplot(graphic_inputs, aes(x = brood_year, y = log(mean_sar), color = hatchery, group = hatchery)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  #scale_color_viridis_d(option = "B", end = 0.9) +
  labs(x = "Brood Year", y = "log-SAR") +
  ggtitle("Fall Chinook SAR by Brood Year and Hatchery") +
  theme_minimal()

# Plot 6: Colorblind-friendly (log-transformed SAR)
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
  ggtitle("Log Transformed Fall Chinook SAR by Brood Year and Hatchery") +
  theme_minimal()

# Plot 7: Colorblind-friendly untransformed SAR
ggplot(graphic_inputs, aes(x = brood_year, y = mean_sar, color = hatchery, group = hatchery)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = combined_colors) +
  labs(x = "Brood Year", y = "SAR") +
  ggtitle("Fall Chinook SAR by Brood Year and Hatchery") +
  theme_minimal()

# close PDF device
dev.off()
