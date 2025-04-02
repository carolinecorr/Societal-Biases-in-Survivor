## Age

  # Univariate

contestants |>
ggplot(aes(age)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Contestant Age",
       x = "Age",
       y = "Count")

contestants |>
summarize(
  mean_age = mean(age),
  median_age = median(age),
  sd_age = sd(age),
  n = n()
) |>
  knitr::kable(title = "Summary Statistics for Contestant Age")

## Winners
contestants |>
  filter(finish == 1) |>
  ggplot(aes(age)) +
  geom_density() +
  labs(title = "Distribution of Age of Survivor Winners",
       x = "Age",
       y = "Density")

  # Bivariate

# Age vs. Placement
contestants |>
  ggplot(aes(finish, age)) +
  geom_point(alpha = 0.2, color = "blue") +
  labs(title = "Placement Versus Age",
       x = "Placement",
       y = "Age")

# Age vs. Season (over time)

  # median by season
contestants |>
  summarize(
    .by = num_season,
    median_age = median(age)
  ) |>
  ggplot(aes(num_season, median_age)) +
  geom_point()

  # finalists
contestants |>
  filter(finish <= 3) |>
  ggplot(aes(num_season, age)) +
  geom_point()

# Age vs. Jury Votes 
contestants |>
  filter(finish <= 3) |>
  ggplot(aes(age, num_jury_votes)) +
  geom_point()

# finalist jury votes by age cohort
contestants |>
  filter(finish <= 3) |>
  mutate(cohort = case_when(
          age < quantile(age, 0.25) ~ "Youngest",
          (age >= quantile(age, 0.25) & age < quantile(age, 0.5)) ~ "Young",
          (age >= quantile(age, 0.5) & age < quantile(age, 0.75)) ~ "Old",
          age > quantile(age, 0.75) ~ "Oldest"
  )) |>
  drop_na() |>
  summarize(
    .by = cohort,
    mean_jury_votes = mean(num_jury_votes, na.rm = TRUE)
  ) |>
  arrange(cohort) |>
  knitr::kable()

# Age vs. Gender
contestants |>
  ggplot(aes(gender, age)) +
  geom_boxplot() +
  labs(title = "Distribution of Contestant Age According to Gender",
       x = "Gender",
       y = "Age",
       caption = "Male contestants tend to be older, and the oldest
                  contestants have been men")

# Multivariate

# Age vs. Placement vs. Gender
contestants |>
  summarize(
    .by = c(finish, gender),
    mean_age = mean(age),
    median_age = median(age)
  ) |>
  ggplot(aes(finish, median_age, color = gender)) +
  geom_smooth(method = "loess") +
  geom_point() +
  labs(title = "Distribution of Placement According to Contestant Age and Gender",
       x = "Season Placement",
       y = "Median Age",
       caption = "Figure 1")


contestants |>
  mutate(finish = as.factor(finish)) |>
  summarize(
    .by = c(finish, gender),
    mean_age = mean(age),
    median_age = median(age)
  ) |>
  arrange(finish, gender) |>
  knitr::kable(caption = "Table 1")

# Age vs. Time vs. Gender
contestants |>
  summarize(
    .by = c(num_season, gender),
    mean_age = mean(age)
  ) |>
  ggplot(aes(num_season, mean_age, color = gender)) +
  geom_smooth(method = "loess") +
  geom_point() +
  labs(title = "Distribution of Mean Contestant Age over Time",,
       x = "Season",
       y = "Mean Age",
       caption = "Figure 2")

contestants |>
  summarize(
    .by = c(num_season, gender),
    mean_age = mean(age),
    median_age = median(age)
  ) |>
  arrange(num_season, gender) |>
  knitr::kable(caption = "Table 2")

