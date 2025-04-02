# Gender and Sexuality

# Univariate

## Gender Breakdown of Winners
contestants |>
  filter(finish == 1) |>
  ggplot(aes(gender)) +
  geom_bar(fill = "green") +
  labs(title = "Distribution of Survivor Winners According to Gender",
       x = "Gender",
       y = "Count",
       caption = "There are significantly more male winners.")

## LGBT contestants
ntl_pct <- tibble(
            sexuality = c("LGBT", "Non-LGBT"),
            national_pct = c(7.1, 92.9)
)

# Manipulate
contestants_lgbt <-
contestants |>
  mutate(sexuality = if_else(lgbt == 1, "LGBT", "Non-LGBT"))

contestants_lgbt |>
  count(sexuality) |>
  inner_join(ntl_pct) |>
  mutate(
    pct = n / sum(n) * 100,
    difference = pct - national_pct) |>
  relocate(pct, .before = national_pct) |>
  knitr::kable()
# LGBT contestants are actually overly represented!

# Winners
contestants_lgbt |>
  filter(finish == 1) |>
  count(sexuality) |>
  inner_join(ntl_pct) |>
  mutate(
    pct = round(n / sum(n) * 100, 2),
    difference = round(pct - national_pct, 2)) |>
  relocate(pct, .before = national_pct) |>
  knitr::kable()


## Bivariate

# Gender vs. Sexuality
contestants_lgbt |>
  filter(gender != "N") |>
  ggplot(aes(gender, fill = sexuality)) +
  geom_bar() +
  labs(title = "Breakdown of LGBT Contestants According to Gender",
       x = "Gender",
       y = "Count",
       fill = "Sexuality",
       caption = "There have been slightly more LGBT male contestants.")

# Gender vs. Placement
contestants |>
  filter(gender != "N") |>
  ggplot(aes(finish, color = gender)) +
  geom_density(linewidth = 1) +
  labs(title = "Distribution of Final Placement According to Gender",
       x = "Placement",
       y = "Density",
       color = "Gender")
# More women voted out pre merge, more men voted out at merge

# Sexuality vs. Placement
contestants_lgbt |>
  ggplot(aes(finish, color = sexuality)) +
  geom_density(linewidth = 1) +
  labs(title = "Distribution of Final Placement According to Sexuality",
       x = "Placement",
       y = "Density",
       color = "Sexuality")

# Sexuality vs. Season (over time)
seasons |>
  mutate(prop_lgbt = lgbt / num_contestants) |>
  ggplot(aes(num_season, prop_lgbt)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Proportion of LGBT Contestants over Time",
       x = "Season",
       y = "Proportion LGBT")

# Gender vs. Jury Votes
contestants |>
  filter(finish <= 3) |>
  ggplot(aes(num_jury_votes, gender)) +
  geom_boxplot() +
  labs(title = "Distribution of Jury Votes Received According to Gender",
       x = "Number of Jury Votes",
       y = "Gender")

contestants |>
  filter(finish <= 3) |>
  summarize(
    .by = gender,
    mean_jury_votes = mean(num_jury_votes, na.rm = TRUE)
  ) |>
  knitr::kable()

# Winners
contestants |>
  filter(finish == 1) |>
  summarize(
    .by = gender,
    mean_jury_votes = round(mean(num_jury_votes, na.rm = TRUE), 2)
  ) |>
  knitr::kable()

# Sexuality vs. Jury Votes
contestants_lgbt |>
  filter(finish <= 3) |>
  ggplot(aes(num_jury_votes, sexuality)) +
  geom_boxplot() +
  labs(title = "Distribution of Jury Votes Received According to Sexuality",
       x = "Number of Jury Votes",
       y = "Sexuality")

contestants_lgbt |>
  filter(finish <= 3) |>
  summarize(
    .by = sexuality,
    mean_jury_votes = round(mean(num_jury_votes, na.rm = TRUE), 2)
  ) |>
  knitr::kable()

# Winners
contestants_lgbt |>
  filter(finish == 1) |>
  summarize(
    .by = sexuality,
    mean_jury_votes = round(mean(num_jury_votes, na.rm = TRUE), 2)
  ) |>
  knitr::kable()

# Multivariate

# Sexuality vs. Placement vs. Gender
contestants_lgbt |>
  filter(gender != "N") |>
  ggplot(aes(finish, color = sexuality)) +
  geom_density(linewidth = 1) +
  facet_wrap(~gender) +
  labs(title = "Density of Final Placements According to Gender
                and Sexuality",
       x = "Placement",
       y = "Density",
       color = "Sexuality")

# Sexuality vs. Time vs. Gender
contestants_lgbt |>
  filter(gender != "N") |>
  summarize(
    .by = c(num_season, gender, sexuality),
    n = n()
  ) |>
  ggplot(aes(num_season, n, color = sexuality)) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~gender) +
  labs(title = "Sexuality Composition of Each Season by Gender",
       x = "Season",
       y = "Number of Contestants",
       color = "Sexuality")

