# Race/Ethnicity

# Manipulate Table

contestants_ethnicity <-
contestants |>
  mutate(
    ethnicity = case_when(
      african_american == 1 ~ "African American",
      asian_american == 1 ~ "Asian American",
      latin_american == 1 ~ "Latin American",
      poc == 0 ~ "Caucasian"
    ),
    ethnicity = as.factor(ethnicity)
  )

national_pct <- tibble(
  ethnicity = c("Latin American", "Asian American", "African American",
                "Caucasian"),
  national_pct = c(18.7, 6.1, 12.1, 57.8)) ## Sourced from US Census

# Univariate
contestants_ethnicity |>
  count(ethnicity) |>
  inner_join(national_pct, join_by(ethnicity)) |>
  mutate(
    pct = n / sum(n) * 100,
    difference = pct - national_pct) |>
  knitr::kable()

contestants_ethnicity |>
  ggplot(aes(fct_infreq(ethnicity))) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Survivor Contestant Race/Ethnicity",
       x = "Race/Ethnicity",
       y = "Count")

# Winners
contestants_ethnicity |>
  filter(finish == 1) |>
  ggplot(aes(fct_infreq(ethnicity))) +
  geom_bar(fill = "green") +
  labs(title = "Distribution of Survivor Winners' Race/Ethnicity",
       x = "Race/Ethnicity",
       y = "Count")

contestants_ethnicity |>
  filter(finish == 1) |>
  count(ethnicity) |>
  inner_join(national_pct, join_by(ethnicity)) |>
  mutate(
    pct = n / sum(n) * 100,
    difference = pct - national_pct) |>
  relocate(pct, .before = national_pct) |>
  knitr::kable()

# Bivariate

# Ethnicity vs. Gender
contestants_ethnicity |>
  ggplot(aes(ethnicity, fill = gender)) +
  geom_bar()

contestants_ethnicity |>
  summarize(
    .by = c(ethnicity, gender),
    n = n()
  ) |>
  pivot_wider(
    names_from = gender,
    values_from = n
  ) |>
  select(!N) |>
  knitr::kable()

# Ethnicity vs. Placement
contestants_ethnicity |>
  ggplot(aes(finish, color = ethnicity)) +
  geom_density() +
  labs(title = "Placement vs. Ethnicity",
       x = "Placement",
       y = "Density",
       color = "Ethnicity")

# Ethnicity vs. Season (over time)
contestants_ethnicity |>
  summarize(
    .by = num_season,
    n_contestants = n(),
    pct_poc = round((sum(poc) / n_contestants * 100), 2), 
    pct_white = round((100 - pct_poc), 2)) |>
  knitr::kable()

contestants_ethnicity |>
  summarize(
    .by = num_season,
    n_contestants = n(),
    pct_poc = sum(poc) / n_contestants * 100) |>
  ggplot(aes(num_season, pct_poc)) +
  geom_smooth(method = "loess") +
  labs(title = "Percentage of POC Contestants Over 43 Seasons",
       x = "Season",
       y = "% POC")

# Ethnicity vs. Jury Votes
contestants_ethnicity |>
  filter(finish <= 3) |>
  ggplot(aes(num_jury_votes, ethnicity)) +
  geom_boxplot() +
  labs(title = "Distribution of Jury Votes Earned by Ethnicity",
       x = "Number of Jury Votes",
       y = "Ethnicity")

  # Finalists
contestants_ethnicity |>
  filter(finish <= 3) |>
  summarize(
    .by = ethnicity,
    mean_votes = mean(num_jury_votes, na.rm = TRUE)
  ) |>
  knitr::kable()

  # Winners
contestants_ethnicity |>
  filter(finish == 1) |>
  summarize(
    .by = ethnicity,
    mean_votes = mean(num_jury_votes, na.rm = TRUE)
  ) |>
  knitr::kable()

# Multivariate

# Ethnicity vs. Placement vs. Gender
contestants_ethnicity |>
  filter(gender != "N") |>
  ggplot(aes(finish, color = ethnicity)) +
  geom_density() +
  facet_wrap(~gender) +
  labs(title = "Density of Final Placements According to Gender
                and Ethnicity",
       x = "Placement",
       y = "Density",
       color = "Ethnicity")

# Ethnicity vs. Time vs. Gender
contestants_ethnicity |>
  filter(gender != "N") |>
  summarize(
    .by = c(num_season, gender, ethnicity),
    n = n()
  ) |>
  ggplot(aes(num_season, n, color = ethnicity)) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~gender) +
  labs(title = "Ethnic Compositions of Each Season by Gender",
       x = "Season",
       y = "Number of Contestants",
       color = "Ethnicity")

  
  



