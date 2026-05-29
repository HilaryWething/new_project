## new_project

library(conflicted)
library(tidyverse)
library(readxl)
conflicts_prefer(dplyr::filter, dplyr::lag)

# Read the District Cost Database (v6.0, Albert Shanker Institute) and keep
# only the 2023 school year. Contains district-level per-pupil spending,
# adequate cost estimates, funding gaps, and student demographic shares.
dcd <- read_xlsx("data_raw/districtcostdatabase_2026.xlsx", sheet = "Data") |>
  filter(year == 2023)

# Read the NCES School District Finance Survey (2023). We only need the
# district identifier (LEAID), county identifier (CONUM), state fields,
# and membership (enrollment) to aggregate districts up to counties.
sdf23 <- read_tsv(
  "data_raw/sdf23_1a.txt",
  col_select = c(LEAID, FIPST, CONUM, STNAME, STABBR, MEMBERSCH)
)

# Read Manduca BEA personal income data at the county level and keep only SNAP and
# Medicaid (public assistance medical care) benefit variables. Pivot to wide
# format so each county has one row with separate columns for each program's
# dollar value and share of tradable income.
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
  select(GeoFIPS, GeoName, desc_short, val_tra_lrt, pct_val_tra_lrt) |>
  pivot_wider(
    names_from = desc_short,
    values_from = c(val_tra_lrt, pct_val_tra_lrt),
    names_glue = "{desc_short}_{.value}"
  )

# Aggregate district-level enrollment to counties. Negative values in
# MEMBERSCH indicate missing/suppressed data in the source file, so they
# are set to NA before summing.
sdf23_cnty <- sdf23 |>
  mutate(MEMBERSCH = if_else(MEMBERSCH < 0, NA_real_, MEMBERSCH)) |>
  summarize(
    MEMBERSCH = sum(MEMBERSCH, na.rm = TRUE),
    .by = c(FIPST, CONUM, STNAME, STABBR)
  )

# Join the cost database onto the finance survey at the district level to
# attach county identifiers (CONUM) to each DCD record.
dcd_dist <- sdf23 |>
  inner_join(dcd, by = join_by(LEAID == leaid))

# Collapse district-level DCD variables to counties using enrollment-weighted
# averages, so larger districts have proportionally more influence on the
# county estimate.
dcd_cnty <- dcd_dist |>
  summarize(
    across(
      c(pov, iep, ell, fundinggap, outcomegap),
      \(x) weighted.mean(x, w = enroll, na.rm = TRUE)
    ),
    .by = c(FIPST, CONUM, STNAME, STABBR)
  )

# Join county-level BEA safety net data with county-level enrollment totals
# from the finance survey, then bring in the DCD weighted averages for
# additional scatterplots. This is the main analysis dataset for the plots.
cnty_data <- perinc_cnty |>
  inner_join(sdf23_cnty, by = join_by(GeoFIPS == CONUM)) |>
  inner_join(dcd_cnty, by = join_by(GeoFIPS == CONUM))


# --- Figures -----------------------------------------------------------------

# Histogram: distribution of county-level student enrollment (log scale)
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

# Histogram: distribution of county-level Medicaid spending (log scale)
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

# Scatterplot: student enrollment vs. Medicaid benefit dollars
ggplot(cnty_data, aes(x = MEMBERSCH, y = pub_asst_med_val_tra_lrt)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    x = "Student enrollment in counties (log scale)",
    y = "Public assistance medical benefits\n(dollars, log scale)",
    title = "Student enrollment vs. Medicaid benefit dollars"
  ) +
  theme_minimal()

ggsave("docs/enrollment_vs_pub_asst_med_val.png", width = 8, height = 5, dpi = 150)

# Scatterplot: student enrollment vs. SNAP benefit dollars
ggplot(cnty_data, aes(x = MEMBERSCH, y = snap_val_tra_lrt)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    x = "Student enrollment in counties (log scale)",
    y = "SNAP benefits (dollars, log scale)",
    title = "Student enrollment vs. SNAP benefit dollars"
  ) +
  theme_minimal()

ggsave("docs/enrollment_vs_snap_val.png", width = 8, height = 5, dpi = 150)

# Scatterplot: student enrollment vs. Medicaid as share of tradable income
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

# Scatterplot: student enrollment vs. SNAP as share of tradable income
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

# Scatterplot: Medicaid share of tradable income vs. county poverty rate
ggplot(cnty_data, aes(x = pov, y = pub_asst_med_pct_val_tra_lrt)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "County poverty rate",
    y = "Medicaid as share of tradable income",
    title = "County poverty rate vs. Medicaid share of tradable income"
  ) +
  theme_minimal()

ggsave("docs/medicaid_pct_vs_pov.png", width = 8, height = 5, dpi = 150)

# Scatterplot: Medicaid share of tradable income vs. district funding gap
ggplot(cnty_data, aes(x = fundinggap, y = pub_asst_med_pct_val_tra_lrt)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Enrollment-weighted funding gap ($ per pupil)",
    y = "Medicaid as share of tradable income",
    title = "School funding gap vs. Medicaid share of tradable income"
  ) +
  theme_minimal()

ggsave("docs/medicaid_pct_vs_fundinggap.png", width = 8, height = 5, dpi = 150)

# Scatterplot: Medicaid dollars vs. county poverty rate
ggplot(cnty_data, aes(x = pov, y = pub_asst_med_val_tra_lrt)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(
    x = "County poverty rate",
    y = "Medicaid dollars (log scale)",
    title = "County poverty rate vs. Medicaid dollars"
  ) +
  theme_minimal()

ggsave("docs/medicaid_val_vs_pov.png", width = 8, height = 5, dpi = 150)

# Scatterplot: Medicaid dollars vs. district funding gap
ggplot(cnty_data, aes(x = fundinggap, y = pub_asst_med_val_tra_lrt)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_log10(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(
    x = "Enrollment-weighted funding gap ($ per pupil)",
    y = "Medicaid dollars (log scale)",
    title = "School funding gap vs. Medicaid dollars"
  ) +
  theme_minimal()

ggsave("docs/medicaid_val_vs_fundinggap.png", width = 8, height = 5, dpi = 150)
