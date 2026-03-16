getwd()
setwd("C:/Users/molly/OneDrive/Documents/GitHub/StatsII_2026/Replication Project")
rmarkdown::render("index.Rmd")
#Orginal Model
lm_robust(
  outcome_diff ~ negative_diff + financialA_diff + financialB_diff + transaction_diff,
  data = df, se_type = "stata") %>%
  tidy

lm_robust(
  outcome ~ negative + financialA + financialB + transaction,
  data = df_stacked, se_type = "stata", fixed_effects = ~ID) %>%
  tidy

our_analysis <- function(dff)
  lm_robust(
    outcome_diff ~ negative*financialA*transaction + negative*financialB*transaction,
    data = dff %>%
      mutate(
        negative = negative_diff - mean(negative_diff),
        financialA = financialA_diff - mean(financialA_diff),
        financialB = financialB_diff - mean(financialB_diff),
        transaction = transaction_diff - mean(transaction_diff)
      ),
    se_type = "stata")

statuses <- unique(df$status)

model_list <- lapply(statuses, function(j) our_analysis(df %>% dplyr::filter(status == j)))

names(model_list) <- statuses

model_list$All <- our_analysis(df)

by_status <-
  lapply(model_list, function(model) tidy(model)) %>%
  bind_rows(.id = "status") %>%
  mutate(status = factor(status, c("All", statuses)))

policy_experiments <- list(
  experiment_1 =
    df_stacked %>%
    mutate(
      Z = negative * financialB * transaction,
      zip = (1-negative) * (1-financialA) * (1-financialB) * (1-transaction)
    ) %>%
    dplyr::filter((Z==1) | (zip==1)) %>%
    group_by(Z) %>%
    mutate(N = n()) %>% ungroup() %>%
    group_by(status, Z) %>%
    summarize(n = n()/mean(N), share_pop_vaccinated = n()*mean(outcome)/mean(N)),

  experiment_2 =
    df_stacked %>%
    dplyr::filter(financialA == 0 & negative == 1 & transaction == 1) %>%
    group_by(financialB) %>%
    mutate(N = n()) %>% ungroup() %>%
    group_by(status, financialB) %>%
    summarize(n = n()/mean(N), share_pop_vaccinated = n()*mean(outcome)/mean(N)) %>%
    arrange(financialB) %>% mutate(Z = financialB)

) %>% bind_rows(.id = "experiment") %>%
  mutate(
    Z = (experiment == "experiment_1") * Z +
      (experiment == "experiment_2") * (Z + .5),
    Z = factor(Z, c(0, .5, 1), c("No incentives", "Local doctors + Freedoms", "All incentives"))
  ) %>%
  dplyr::filter(!is.na(Z))

fig_2_ABC <- ggarrange(
  lm_plot + theme(plot.margin=unit(c(1,1,1.5,1.2)*.6,"cm")),
  ggarrange(
    age_plot + ylim(-.03, .13) + theme(plot.margin=unit(c(1,1,1.5,1.2)*.6,"cm")),
    distance_plot + ylab("") + ylim(-.03, .13) + theme(plot.margin=unit(c(1,1,1.5,1.2)*.6,"cm")),
    labels = c("B", "C"),
    ncol = 2, common.legend = TRUE, legend="bottom"
  ),
  nrow = 2, labels = "A"
)

lm_robust(
  outcome_diff ~ negative_diff + financialA_diff + financialB_diff + transaction_diff,
  data = df, se_type = "stata"
)
# Figure 1
figure_1A <- by_status %>%
  filter(term %in% c("negative", "financialA", "financialB", "transaction")) %>%
  filter(status != "Vaccinated") %>%
  mutate(Treatment = factor(term, treatment_levels, treatment_labels)) %>%
  ggplot(aes(Treatment, estimate, color = status, shape = status)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.3), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "longdash", size = 0.75, colour = "#B55555") +
  theme_bw(base_size = 14) +
  ylab("Estimated effect on prob vaccination (0-1)") +
  theme(legend.position = "bottom")
#Figure 2
figure_1B <- policy_experiments %>%
  ggplot(aes(fill = status, y = share_pop_vaccinated, x = Z)) +
  geom_bar(position = "stack", stat = "identity", width = 0.5, alpha = 0.7) +
  scale_fill_manual(values = group.colors) +
  ylab("Share of population vaccinated") +
  xlab("Incentives") +
  theme_bw(base_size = 14) +
  ylim(0:1) +
  theme(legend.position = "bottom")
# Figure 3
fig_2_ABC <- ggarrange(
  lm_plot + theme(plot.margin = unit(c(1,1,1.5,1.2) * 0.6, "cm")),
  ggarrange(
    age_plot + ylim(-0.03, 0.13) +
      theme(plot.margin = unit(c(1,1,1.5,1.2) * 0.6, "cm")),
    distance_plot + ylab("") + ylim(-0.03, 0.13) +
      theme(plot.margin = unit(c(1,1,1.5,1.2) * 0.6, "cm")),
    labels = c("B", "C"),
    ncol = 2,
    common.legend = TRUE,
    legend = "bottom"
  ),
  nrow = 2,
  labels = "A"
)

fig_2_ABC
# Modifications
model_interact <- lm_robust(
  outcome_diff ~
    negative_diff * age2 +
    financialA_diff * age2 +
    financialB_diff * age2 +
    transaction_diff * age2,
  data = df,
  se_type = "stata"
)

library(broom)

plot_data <- tidy(model_interact) %>%
  dplyr::filter(grepl("diff", term)) %>%
  mutate(
    Treatment = case_when(
      grepl("negative_diff", term) ~ "Freedoms",
      grepl("financialA_diff", term) ~ "25 Euro",
      grepl("financialB_diff", term) ~ "50 Euro",
      grepl("transaction_diff", term) ~ "Local Doctor",
      TRUE ~ term
    ),
    EffectType = ifelse(grepl(":age2", term), "Interaction with Age", "Main Effect")
  )

p <- ggplot(plot_data, aes(x = Treatment, y = estimate, fill = EffectType)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.9),
    width = 0.25
  ) +
  theme_bw(base_size = 14) +
  ylab("Estimated Effect on Vaccination Probability") +
  xlab("Treatment") +
  scale_fill_manual(values = c("Main Effect" = "#1f78b4", "Interaction with Age" = "#33a02c")) +
  ggtitle("Estimated Treatment Effects with Age Interactions")
