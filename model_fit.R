# load certain R libraries
pacman::p_load(tidyverse,
               mgcv,
               here,
               lubridate,
               tidymv)

# set plotting theme
theme_set(theme_minimal())

# read in data
dat <-
  read_csv(here("outputs",
                "fall chinook relative sar data.csv"),
           show_col_types = F) |>
  rename(record_id = `...1`,
         total_cwt_recovered = total_cwt_estimated) |> 
  mutate(across(c(hatchery,
                  tag_code),
                as.factor),
         across(total_cwt_recovered,
                ~ round(.)),
         jday = yday(first_release_date)) |>
  mutate(hatchery_group = case_when(hatchery %in%
                                      c("COWLITZ SALMON HATCHERY",
                                        "NORTH TOUTLE HATCHERY",
                                        "MAYFIELD NET PEN") ~ "COWLITZ BASIN",
                                    .default = "OTHER"),
         across(hatchery_group,
                ~ fct_relevel(.,
                              "COWLITZ BASIN",
                              after = Inf))) |>
  mutate(across(hatchery,
                ~ fct_relevel(.,
                              "COWLITZ SALMON HATCHERY",
                              "NORTH TOUTLE HATCHERY",
                              "MAYFIELD NET PEN",
                              after = Inf))) |>
  # filter 2020, not all returns back
  filter(brood_year < 2020)

# exploratory data plots

pdf(here("figures",
         "eda_plots.pdf"), 
    width=8,
    height=5)

# relative recovery across all years
dat |>
  ggplot(aes(x = hatchery_group,
             y = relative_sar,
             fill = hatchery_group)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log",
                     breaks = scales::breaks_pretty()) +
  labs(x = "Hatchery Group",
       y = "Observed Recovery Rate")

# by year and hatchery
dat |> 
  filter(hatchery_group != "COWLITZ BASIN") |> 
  ggplot(aes(x = brood_year,
             y = relative_sar,
             color = hatchery)) +
  geom_point() +
  geom_line() +
  geom_point(data = dat |>
               filter(hatchery_group == "COWLITZ BASIN"),
             size = 4) +
  geom_line(data = dat |>
              filter(hatchery_group == "COWLITZ BASIN"),
            linewidth = 2) +
  scale_colour_viridis_d(direction = -1) +
  labs(
    x = "Brood Year",
    y = "Observed Recovery Rates"
  ) +
  # scale_y_log10() +
  scale_y_continuous(trans = "log",
                     breaks = scales::breaks_pretty()) +
  theme(legend.position = "bottom")

# by year and hatchery group
dat |>
  unite("grp",
        hatchery_group, brood_year,
        remove = F) |>
  ggplot(aes(x = brood_year,
             y = relative_sar,
             # color = hatchery_group,
             fill = hatchery_group,
             group = grp)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Brood Year",
    y = "Observed Recovery Rates"
  ) +
  # scale_y_log10() +
  scale_y_continuous(trans = "log",
                     breaks = scales::breaks_pretty()) +
  theme(legend.position = "bottom")

dev.off()


dat |> 
  ggplot(aes(x = avg_weight,
             color = hatchery_group,
             fill = hatchery_group)) +
  geom_density(alpha = 0.2)


# dat %>%
#   group_by(hatchery, brood_year) %>%
#   summarize(count = n(), .groups = "drop")%>%
#   filter(count>1)

#----------------------------------------------------
# fit a model
model <-
  gam(
    cbind(total_cwt_recovered, total_cwt_rel - total_cwt_recovered) ~
      s(brood_year, bs="cs", k=length(min(dat$brood_year):max(dat$brood_year))) +
      hatchery_group +
      s(hatchery, bs="re") +
      s(avg_weight, bs = "tp", k = 3) +
      s(jday, bs = "tp") +
      s(hatchery, brood_year, bs="fs",k=-1),
    family = quasibinomial(link = "logit"),
    data = dat,
    method="REML"
  )

summary(model)
# plot(model)

# use tidymv to create some marginal plots
# plot_smooths(model,
#              series = "brood_year",
#              exclude_terms = "hatchery_group",
#              transform = boot::inv.logit) +
#   labs(x = "Brood Year",
#        y = "Predicted Recovery Probability")
# 
# plot_smooths(model,
#              series = "brood_year",
#              comparison = "hatchery_group",
#              transform = boot::inv.logit) +
#   labs(x = "Brood Year",
#        y = "Predicted Recovery Probability")
# 
# plot_smooths(model,
#              series = "avg_weight",
#              exclude_terms = "hatchery_group",
#              transform = boot::inv.logit) +
#   labs(x = "Weight at Release",
#        y = "Predicted Recovery Probability")
# 
# plot_smooths(model,
#              series = "avg_weight",
#              comparison = "hatchery_group",
#              transform = boot::inv.logit) +
#   labs(x = "Weight at Release",
#        y = "Predicted Recovery Probability")
# 
# plot_smooths(model,
#              series = "jday",
#              exclude_terms = "hatchery_group",
#              transform = boot::inv.logit) +
#   labs(x = "Julian Day of Release",
#        y = "Predicted Recovery Probability")
# 
# plot_smooths(model,
#              series = "jday",
#              comparison = "hatchery_group",
#              transform = boot::inv.logit) +
#   labs(x = "Julian Day of Release",
#        y = "Predicted Recovery Probability")
# 
# 
# get_gam_predictions(model,
#                     series = c("hatchery_group"),
#                     exclude_random = F,
#                     transform = boot::inv.logit) |>
#   as_tibble() |> 
#   rename(pred = `cbind(total_cwt_recovered, total_cwt_rel - total_cwt_recovered)`) |> 
#   ggplot(aes(x = hatchery_group,
#              y = pred,
#              color = hatchery_group)) + 
#   geom_errorbar(aes(ymin = CI_lower,
#                     ymax = CI_upper),
#                 width = 0.1) +
#   geom_point(size = 4) +
#   labs(x = "Hatchery",
#        y = "Predicted Recovery Proability")



# create a prediction dataset
newdat <-
  expand(dat,
         nesting(hatchery,
                 hatchery_group),
         brood_year) |>
  mutate(avg_weight = mean(dat$avg_weight, na.rm = T),
         jday = mean(dat$jday))


# Ensure factor variables match the model structure
identical(levels(dat$hatchery),
          levels(newdat$hatchery))

# Make predictions without including "tag_code"
preds <-
  predict(model,
          newdata = newdat,
          type = "response",
          se.fit = T) |>
  as_tibble() |>
  rename(prediction = fit,
         pred_se = se.fit) |>
  mutate(lci = qnorm(0.025, prediction, pred_se),
         uci = qnorm(0.975, prediction, pred_se),
         across(lci,
                ~ case_when(. < 0 ~ 0,
                            .default = .)))
# add predictions
newdat <-
  newdat |>
  bind_cols(preds)


# create some plots
pdf(here("figures",
         "prediction_plots.pdf"),
    width = 8,
    height = 5)

ggplot(newdat, aes(x = brood_year,
                   y = prediction,
                   color = hatchery_group,
                   fill = hatchery_group,
                   group = hatchery)) +
  # geom_ribbon(aes(ymin = lci,
  #                 ymax = uci),
  #             alpha = 0.05,
  #             color = NA) +
  geom_line() +  # Line plot to show trends over brood years
  geom_point() + # Add points for individual predictions
  theme_minimal() +
  labs(
    x = "Brood Year",
    y = "Predicted Recovery Rate"
  ) +
  scale_y_log10(breaks = scales::breaks_pretty()) +
  # scale_y_continuous(trans = "log",
  #                    breaks = scales::breaks_pretty()) +
  theme(legend.position = "right")

ggplot(newdat, 
       aes(x = brood_year, 
           y = prediction, 
           color = hatchery, 
           group = hatchery)) +
  geom_line() +  # Line plot to show trends over brood years
  geom_point() + # Add points for individual predictions
  theme_minimal() +
  labs(
    x = "Brood Year",
    y = "Predicted Recovery Rate"
  ) +
  scale_y_log10(breaks = scales::breaks_pretty())+
  theme(legend.position = "right")

dev.off()



# all_preds <-
#   dat |> 
#   bind_cols(predict(model,
#                     # newdata = newdat,
#                     type = "response",
#                     se.fit = T) |>
#               as_tibble() |>
#               rename(prediction = fit,
#                      pred_se = se.fit) |>
#               mutate(lci = qnorm(0.025, prediction, pred_se),
#                      uci = qnorm(0.975, prediction, pred_se),
#                      across(lci,
#                             ~ case_when(. < 0 ~ 0,
#                                         .default = .))))

# add mean observed values by hatchery / brood year
all_preds <-
  newdat |>
  select(hatchery,
         hatchery_group,
         brood_year) |>
  left_join(dat |>
              group_by(hatchery,
                       brood_year) |>
              summarize(across(c(avg_weight,
                                 jday,
                                 relative_sar),
                               ~ mean(., na.rm = T)),
                        .groups = "drop"),
            by = join_by(hatchery,
                         brood_year)) |> 
  filter(!is.na(relative_sar))

all_preds <-
  all_preds |> 
  bind_cols(predict(model,
                    newdata = all_preds,
                    type = "response",
                    se.fit = T) |>
              as_tibble() |>
              rename(prediction = fit,
                     pred_se = se.fit) |>
              mutate(lci = qnorm(0.025, prediction, pred_se),
                     uci = qnorm(0.975, prediction, pred_se),
                     across(lci,
                            ~ case_when(. < 0 ~ 0,
                                        .default = .))))


pdf(here("figures",
         "diagnostic_plots.pdf"),
    width = 8,
    height = 5)


ggplot(all_preds,
       aes(x = brood_year,
           y = prediction,
           color = hatchery_group,
           group = hatchery_group)) +
  geom_line() +  # Line plot to show trends over brood years
  geom_point(aes(y=relative_sar)) + # Add points for individual predictions
  facet_wrap(~hatchery,scales="free_y")+
  theme_minimal() +
  labs(
    x = "Brood Year",
    y = "Predicted Recovery Rate"
  ) +
  scale_y_log10(breaks = scales::breaks_pretty())+
  theme(legend.position = "none")

all_preds |> 
  ggplot(aes(x = relative_sar,
             y = prediction,
             color = hatchery_group)) + 
  geom_abline(linetype = 2) +
  geom_point(size = 3) +
  scale_x_log10(breaks = scales::breaks_pretty()) +
  scale_y_log10(breaks = scales::breaks_pretty()) +
  labs(x = "Observed Recovery Rate",
       y = "Predicted Recovery Rate")


dev.off()
