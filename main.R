## new_project

library(conflicted)
library(tidyverse)
conflicts_prefer(dplyr::filter, dplyr::lag)

sdf23 <- read_tsv(
  "data_raw/sdf23_1a.txt",
  col_select = c(
    LEAID, PID6, UNIT_TYPE, FIPST, CONUM, CSA, CBSA,
    NAME, STNAME, STABBR, SCHLEV, AGCHRT, YEAR, MEMBERSCH,
    CCDNF, CENFILE, GSLO, GSHI,
    TOTALREV, TFEDREV, TSTREV, TLOCREV,
    TOTALEXP, TCURELSC, TCURINST, TCURSSVC, TCAPOUT
  )
)

perinc_cnty <- read_csv(
  "data_raw/perinc_disagg_cnty_2022.csv",
  show_col_types = FALSE
) |>
  filter(Description %in% c(
    "Supplemental Nutrition Assistance Program (SNAP)",
    "Public assistance medical care benefits"
  )) |>
  mutate(desc_short = case_when(
    Description == "Supplemental Nutrition Assistance Program (SNAP)" ~ "snap",
    Description == "Public assistance medical care benefits" ~ "pub_asst_med"
  )) |>
  select(-inccat, -Description) |>
  pivot_wider(
    names_from = desc_short,
    values_from = c(val_pr, val_tra_lrt, val_tra_cm, pct_val_pr, pct_val_tra_lrt, pct_val_tra_cm),
    names_glue = "{desc_short}_{.value}"
  )

finance_vars <- c(
  "MEMBERSCH", "TOTALREV", "TFEDREV", "TSTREV", "TLOCREV",
  "TOTALEXP", "TCURELSC", "TCURINST", "TCURSSVC", "TCAPOUT"
)

sdf23_cnty <- sdf23 |>
  mutate(across(all_of(finance_vars), \(x) if_else(x < 0, NA_real_, x))) |>
  summarize(
    across(all_of(finance_vars), \(x) sum(x, na.rm = TRUE)),
    .by = c(FIPST, CONUM, STNAME, STABBR)
  )

cnty_data <- perinc_cnty |>
  inner_join(sdf23_cnty, by = join_by(GeoFIPS == CONUM))


ggplot(cnty_data, aes(x = MEMBERSCH)) +
  geom_histogram(bins = 50, fill = "#2c7bb6", color = "white", linewidth = 0.2) +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Student enrollment (log scale)",
    y = "Number of counties",
    title = "Distribution of student enrollment in counties"
  ) +
  theme_minimal()

ggsave("docs/enrollment_histogram.png", width = 8, height = 5, dpi = 150)

ggplot(cnty_data, aes(x = pub_asst_med_val_tra_lrt)) +
  geom_histogram(bins = 50, fill = "#2c7bb6", color = "white", linewidth = 0.2) +
  scale_x_log10(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Medicaid spending (log scale)",
    y = "Number of counties",
    title = "Distribution of county-level Medicaid spending"
  ) +
  theme_minimal()

ggsave("docs/medicaid_spending_histogram.png", width = 8, height = 5, dpi = 150)

ggplot(cnty_data, aes(x = MEMBERSCH, y = pub_asst_med_val_tra_lrt)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    x = "Student enrollment in counties (log scale)",
    y = "Public assistance medical benefits\n(dollars, log scale)",
    title = "Student enrollment in counties vs. public assistance medical benefit dollars"
  ) +
  theme_minimal()

ggsave("docs/enrollment_vs_pub_asst_med_val.png", width = 8, height = 5, dpi = 150)

ggplot(cnty_data, aes(x = MEMBERSCH, y = snap_val_tra_lrt)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    x = "Student enrollment in counties (log scale)",
    y = "SNAP benefits (dollars, log scale)",
    title = "Student enrollment in counties vs. SNAP benefit dollars"
  ) +
  theme_minimal()

ggsave("docs/enrollment_vs_snap_val.png", width = 8, height = 5, dpi = 150)

ggplot(cnty_data, aes(x = MEMBERSCH, y = pub_asst_med_pct_val_tra_lrt)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Student enrollment in counties (log scale)",
    y = "Medicaid as share of tradable income (%)",
    title = "Student enrollment vs. Medicaid share of a county's tradable income"
  ) +
  theme_minimal()

ggsave("docs/enrollment_vs_medicaid_pct_tra.png", width = 8, height = 5, dpi = 150)

ggplot(cnty_data, aes(x = MEMBERSCH, y = snap_pct_val_tra_lrt)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Student enrollment in counties (log scale)",
    y = "SNAP as share of tradable income (%)",
    title = "Student enrollment vs. SNAP share of a county's tradable income"
  ) +
  theme_minimal()

ggsave("docs/enrollment_vs_snap_pct_tra.png", width = 8, height = 5, dpi = 150)

