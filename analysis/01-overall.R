# This script is used to calculate a three-level random effects
# meta-analytical model. It requires a data file in .xlsx format.

# Load packages -----------------------------------------------------------

library(here)
library(readxl)
library(janitor)
library(metafor)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(glue)
library(ggplot2)
library(clubSandwich)
library(writexl)

source(here("R/descriptives.R"))
source(here("R/pairwise_tests.R"))
source(here("R/get_coefs.R"))

# Set parameters ----------------------------------------------------------
# In this part, we set all the parameters that we might be interested in changing

# Data file name (needs quotes around name, and file ending)
data_file <- "Meta-analysis spreadsheet specific factors to AI 221202DBA.xlsx"

# Here you set the name for the tables and figures produced (without file ending)
files_out_name <- "01-overall"

# Choose what variable to group the data by (run separate model for)
# Example: grouping <- "outcome"
grouping <- c("outcome")

# Exclude outcome types from the analysis bi including them here (in quotation)
# Example: exclude <- c("AS", "RS")
exclude <- c()

# Include one or several moderators in the model here (in quotation)
# Example: mods <- c("subgroup", "demographics")
mods <- c()

# Confidence level
ci <- .95

# Load data ---------------------------------------------------------------

meta_data <- read_xlsx(path = here("data", data_file))

# Prepare data ------------------------------------------------------------

# Create R-friendly names (lowercase with underscore)
# Remove all data missing vital info
# Adds id for effect sizes
clean_data <- clean_names(meta_data) |> 
  drop_na(id, outcome, correlation, std_error) |> 
  mutate(
    es_id = row_number()
  ) |> 
  escalc(
    measure = "ZCOR", 
    ri = correlation, 
    ni = sample_size, 
    slab = study_name,
    data = _
  )

# Create a nested dataframe for each outcome
# Filter out outcome with only one effect size
# Filter out outcomes with only one level of moderator
nested_data <- clean_data |> 
  filter(!(.data[[grouping]] %in% exclude)) |> 
  nest_by(.data[[grouping]]) |> 
  filter(
    nrow(data) > 1
    # length(unique(data[[mods]])) > 1
  ) |> 
  mutate(
    V = list(vcalc(vi = vi, cluster = id, nearpd = TRUE, data = data))
  )

# Run analysis ------------------------------------------------------------

# Descriptives
nr_studies <- clean_data |> 
  group_by() |> 
  summarise(
    n_effect_sizes = n(),
    k_studies = n_distinct(id),
    .by = c({{grouping}}, {{mods}})
  )

# Set up moderator formula
mod_formula <- if(length(mods) > 0) {
  mod_string <- paste(mods, collapse = "+")
  as.formula(paste0("~ 0 +", mod_string))
} else {
  as.formula("~ NULL")
}

models <- nested_data |> 
  mutate(
    model = list(
        rma.mv(
          yi = yi,
          V = V,
          random = ~ 1|id/es_id,
          mods = mod_formula,
          slab = study_name,
          data = data,
          test= "t",
          dfs = "contain",
          level = ci,
          control=list(iter.max=1000, rel.tol=1e-8)
        )
    )
  )

# Check results -----------------------------------------------------------

descs <- models |> 
  summarise(descriptives(model))

coefs <- models |> 
  summarise(get_coefs(model, data))

pairs <- if (length(mods) > 0) models |> summarise(pairwise_tests(model))

# Eggers test for funnel plot asymetry -----------------------------------

regtest_models <- nested_data |> 
  mutate(
    model = list(
      rma.mv(
        yi = yi,
        V = V,
        random = ~ 1|id/es_id,
        mods = ~std_error,
        slab = study_name,
        data = data,
        test = "t",
        dfs = "contain",
        level = ci,
        control = list(iter.max = 1000, rel.tol = 1e-8)
      )
    )
  )

regtest_coefs <- regtest_models |> 
  reframe(get_coefs(model, data)) |> 
  filter(term == "std_error")

# Write excel file with all output ----------------------------------------

tabs <- if (length(mods) > 0) {
  list(
    descriptives = descs, 
    coefficients = coefs, 
    pairwise = pairs, 
    eggers_test = regtest_coefs,
    nr_studies = nr_studies
  )
} else {
  list(
    descriptives = descs, 
    coefficients = coefs, 
    eggers_test = regtest_coefs,
    nr_studies = nr_studies
  )
  }

write_xlsx(
  tabs,
  here("output", paste0(files_out_name, ".xlsx"))
)


# Create forest plots for each outcome -----------------------------------

walk2(models$model, models[[grouping]], ~funnel(.x, main = .y))



# Visualisation -----------------------------------------------------------

axis_labels_summary <- coefs |>
  mutate(
    across(
      c(estimate, conf.low, conf.high),
      ~formatC(round(., digits = 2), digits = 2, format = "f")
    ),
    m_ci = glue("{estimate} [{conf.low}, {conf.high}]")
  )

coefs |>
  ggplot(aes(x = estimate, y = outcome)) +
  geom_vline(xintercept = 0, color = "gray30", size = .3) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_text(
    aes(label = m_ci, x = Inf),
    data = axis_labels_summary,
    hjust = 0,
    size = 2.5
    ) +
  scale_y_discrete(limits = rev) +
  coord_cartesian(clip = 'off') +
  theme_void(base_size = 6) +
  theme(
    axis.line.x.bottom = element_line(.3, color = "gray30"),
    axis.text.y.left = element_text(hjust = 1, vjust = 1),
    axis.text.y.right = element_text(hjust = 1, vjust = 1),
    axis.text.x = element_text(),
    axis.ticks.length.x = unit(1, "pt"),
    axis.ticks.x = element_line(color = "gray30", size = .3),
    plot.margin = margin(10, 150, 10, 10)
  )
