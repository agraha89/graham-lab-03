## Setup
library(carData)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(performance)
library(gt)

attitude.df <- data("attitude")

workplace.rating.df <- attitude |> 
  select(rating, complaints, learning, raises)

#Exploring the data
head(workplace.rating.df)
str(workplace.rating.df)

descriptives <- workplace.rating.df |>
  summarise(across(
      c(rating, complaints, learning, raises),
      list(mean = mean, sd = sd, min = min, max = max))) |>
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_sep = "_")
  
descriptives |> 
  gt() |>
  fmt_number(columns = where(is.numeric), decimals = 2)

#Distributions
ggplot(workplace.rating.df, aes(x = rating)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 15,
    fill = "#669BBC",
    color = "white",
    alpha = 0.8) +
  geom_density(
    linewidth = 0.9,
    color = "#9A3038") +
  labs(x = "Overall Departmental Workplace Rating",
    y = "Density") +
  theme_minimal(base_size = 12)

ggplot(workplace.rating.df, aes(x = complaints)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 15,
    fill = "#669BBC",
    color = "white",
    alpha = 0.8) +
  geom_density(
    linewidth = 0.9,
    color = "#9A3038") +
  labs(x = "Departmental Handling of Employee Complaints",
       y = "Density") +
  theme_minimal(base_size = 12)

ggplot(workplace.rating.df, aes(x = learning)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 15,
    fill = "#669BBC",
    color = "white",
    alpha = 0.8) +
  geom_density(
    linewidth = 0.9,
    color = "#9A3038") +
  labs(x = "Departmental Opportunity to Learn",
       y = "Density") +
  theme_minimal(base_size = 12)

ggplot(workplace.rating.df, aes(x = raises)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 15,
    fill = "#669BBC",
    color = "white",
    alpha = 0.8) +
  geom_density(
    linewidth = 0.9,
    color = "#9A3038") +
  labs(x = "Departmental Pay Increases",
       y = "Density") +
  theme_minimal(base_size = 12)

#Relationships
rating_by_complaints <- cor.test(workplace.rating.df$rating, workplace.rating.df$complaints, method = "pearson")

rating_by_learning <- cor.test(workplace.rating.df$rating, workplace.rating.df$learning, method = "pearson")

rating_by_raises <- cor.test(workplace.rating.df$rating, workplace.rating.df$raises, method = "pearson")

ggplot(workplace.rating.df, aes(x = complaints, y = rating)) +
  geom_point(alpha = 0.75) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#27638A") +
  labs(x = "Departmental Handling of Employee Complaints",
    y = "Overall Departmental Workplace Rating") +
  theme_minimal(base_size = 12) +
  annotate("label",
           x = 80,
           y = 50,
           label = "r = .83")

ggplot(workplace.rating.df, aes(x = learning, y = rating)) +
  geom_point(alpha = 0.75) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#27638A") +
  labs(x = "Departmental Opportunity to Learn",
       y = "Overall Departmental Workplace Rating") +
  theme_minimal(base_size = 12) +
  annotate("label",
           x = 70,
           y = 50,
           label = "r = .62")

ggplot(workplace.rating.df, aes(x = raises, y = rating)) +
  geom_point(alpha = 0.75) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#27638A") +
  labs(x = "Departmental Pay Increases",
       y = "Overall Departmental Workplace Rating") +
  theme_minimal(base_size = 12) +
  annotate("label",
           x = 80,
           y = 50,
           label = "r = .59")

## Intercept-only Model
mod0 <- lm(rating ~ 1, data = workplace.rating.df)

# Complaints model
mod1 <- lm(rating ~ complaints, data = workplace.rating.df)

# Complaints + Learning model
mod2 <- lm(rating ~ complaints + learning, data = workplace.rating.df)

# Expanded model
mod3 <- lm(rating ~ complaints + learning + raises, data = workplace.rating.df)

# Inspecting the Models
summary(mod0)
summary(mod1)
summary(mod2)
summary(mod3)

confint(mod3)

# Nested-Model F-Tests
tests <- anova(mod0, mod1, mod2, mod3)

nested.f.tests <- tests |>
  select(Res.Df, Df, F, `Pr(>F)`) |>
  rename("Residual DF" = Res.Df,
    "Change in Df" = Df,
    "Nested F" = F,
    "Nested p" = `Pr(>F)`) |> 
  mutate(`Nested p` = ifelse(`Nested p` < 0.001,
      "< .001", sprintf("%.3f", `Nested p`))) |> 
  mutate(Model = c("Intercept-only", "M1: Complaints", "M2: + Learning", "M3: + Raises")) |> 
  relocate(Model, .before = 1)

# Extracting Overall Model Performance
glance0 <- glance(mod0)
glance1 <- glance(mod1)
glance2 <- glance(mod2)
glance3 <- glance(mod3)

glances <- rbind(glance0,
  glance1,
  glance2,
  glance3) |> 
  mutate(Model = c("Intercept-only", "M1: Complaints", "M2: + Learning", "M3: + Raises")) |> 
  relocate(Model, .before = 1)

nested.f.tests.table <- gt(nested.f.tests) |> 
  fmt_number(columns = c("Nested F"), decimals = 2) |> 
  cols_align(align = "left")

## Combining nested tests and model performance

comparison_table <- cbind(
  Model = glances,
  nested.f.tests)

comparison_table <- comparison_table |>
  mutate(Delta.R2 = Model.r.squared - lag(Model.r.squared),
    RMSE = c(sqrt(mean(residuals(mod0)^2)),
      sqrt(mean(residuals(mod1)^2)),
      sqrt(mean(residuals(mod2)^2)),
      sqrt(mean(residuals(mod3)^2)))) |>
  select(Model.Model,
    Model.r.squared,
    Delta.R2,
    Model.adj.r.squared,
    RMSE,
    "Residual DF",
    Model.df,
    "Nested F",
    "Nested p")

## Displaying combined model-comparison table
comparison_table |>
  gt() |>
  fmt_number(columns = c(
    Model.r.squared,
    Model.adj.r.squared,
    RMSE,
    Delta.R2,
    "Nested F"),
    decimals = 2) |>
  sub_missing(columns = c(Delta.R2, Model.df, "Nested F", "Nested p"),
              missing_text = "—") |> 
  cols_label(Model.Model = "Model",
    Model.r.squared = "R²",
    Model.adj.r.squared = "Adjusted R²",
    RMSE = "RMSE",
    Delta.R2 = "ΔR²",
    "Residual DF" = "Residual df",
    Model.df = "Nested df") |> 
  cols_move(columns = Delta.R2,
    after = "Residual DF")

## Additional Performance Comparison
performance_comparison <- compare_performance(
  mod0,
  mod1,
  mod2,
  mod3,
  rank = TRUE)

performance_comparison
plot(performance_comparison)

## Selecting a Provisional Model
selected_row <- comparison_table |>
  slice_max(order_by = Model.adj.r.squared,
    n = 1,
    with_ties = FALSE)

selected_name <- selected_row$Model.Model

selected_model <- switch(
  selected_name,
  "Intercept-only" = mod0,
  "M1: Complaints" = mod1,
  "M2: + Learning" = mod2,
  "M3: + Raises" = mod3)

## Evaluate Regression Diagnostics
op <- par(mfrow = c(2, 2))

plot(selected_model)

check_model(mod1)
check_model(mod2)
check_model(mod3)
check_collinearity(mod2)
check_collinearity(mod3)

## Reporting Regression Coefficients
coefficient_results <- tidy(mod1,
  conf.int = TRUE,
  conf.level = 0.95)

coefficient_results |>
  select(term,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value) |>
  gt() |>
  cols_label(term = "Predictor",
    estimate = "Estimate",
    std.error = "SE",
    conf.low = "95% CI lower",
    conf.high = "95% CI upper",
    p.value = "p") |>
  fmt_number(columns = c(
      estimate,
      std.error,
      conf.low,
      conf.high),
    decimals = 3) |>
  fmt(columns = p.value,
    fns = function(x) {
      ifelse(
        x < .001,
        "< .001",
        sprintf("%.3f", x))})

mod1_row <- comparison_table |> 
  slice(2) |>
  select(Model.Model,
    Model.r.squared,
    Model.adj.r.squared,
    RMSE) |> 
  gt() |>
  cols_label(
    Model.Model = "Model",
    Model.r.squared = "R²",
    Model.adj.r.squared = "Adjusted R²",
    RMSE = "RMSE") |>
  fmt_number(columns = where(is.numeric),
    decimals = 3)

