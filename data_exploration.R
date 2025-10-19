
# read data
netflix <- read.csv("netflix_movies_data.csv")
str(netflix)
summary(netflix)
hist(netflix$revenue, breaks = 50, main = "Distribution of Revenue", xlab = "Revenue (USD)")

# no nulls but lots of zero revenue
sum(is.na(netflix$revenue))
sum(netflix$revenue != 0)
sum(netflix$budget != 0)

#clean dataset
netflix_clean <- subset(netflix, revenue > 0)
netflix_clean <- subset(netflix_clean, budget > 0)
summary(netflix_clean)
hist(netflix_clean$revenue, breaks = 50, main = "Distribution of Revenue", xlab = "Revenue (USD)")
head(sort(netflix_clean$revenue), 15)
sapply(netflix_clean, function(x) sum(!is.na(x)))

# duration is null, type is all movie.
netflix_clean <- subset(netflix_clean, select = -duration)
netflix_clean <- subset(netflix_clean, select = -type)


#still 20ish movies with $2-$15 so remove them
netflix_clean <- subset(netflix_clean, revenue >= 1000)
summary(netflix_clean$revenue)
hist(netflix_clean$revenue, breaks = 50, main = "Distribution of Revenue", xlab = "Revenue (USD)")

#very skewed, so use log scale to make a lil more readable
hist(log(netflix_clean$revenue),
     breaks = 50,
     main = "Log-Transformed Distribution of Revenue",
     xlab = "log(Revenue in USD)")

# predictors used in the preliminary model
#install.packages("psych")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("kableExtra")

names(netflix_clean)
# currently have 16 features and one predictor
# but have to make some more usable like cast,
# director, (even though they do have influence,
# to make it more usable we will just look at count as a
# measure of production scale for lin reg.)

library(stringr)
library(dplyr)
library(knitr)
library(kableExtra)
library(psych)
library(forcats)

netflix_clean <- netflix_clean %>%
  mutate(
    cast_count = ifelse(is.na(cast), 0, str_count(cast, ",") + 1),
    director_count = ifelse(is.na(director), 0, str_count(director, ",") + 1),
  )

netflix_clean <- netflix_clean %>%
  mutate(
    language = as.factor(language),
    country = as.factor(country),
    genres = fct_lump_min(as.factor(genres), min = 40)
  )

#keeping top 10 languages and countries with the rest in other
netflix_clean <- netflix_clean %>%
  mutate(
    language = fct_lump(language, n = 10),
    country = fct_lump(country, n = 10)
  )


#generate summary table
# Select numeric predictors
numeric_vars <- netflix_clean %>%
  select(budget, vote_average, vote_count, popularity,
         release_year, cast_count, director_count)

# Compute updated summary
desc <- describe(numeric_vars)

# Format results cleanly
summary_table <- desc %>%
  select(vars, n, min, max, mean, sd, skew, kurtosis) %>%
  mutate(Variable = rownames(desc)) %>%
  select(Variable, everything(), -vars) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Display updated summary table
kable(summary_table, caption = "Table 1: Summary Statistics for Numeric Predictors (After Removing Zero Budgets)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


# summarize category
# function to summarize any categorical variable
summarize_categorical <- function(df, var) {
  df %>%
    group_by({{ var }}) %>%
    summarise(
      Count = n()
    ) %>%
    mutate(
      Percentage = round(100 * Count / sum(Count), 2)
    ) %>%
    arrange(desc(Count)) %>%
    rename(Category = {{ var }})
}

# summarize all categorical predictors
genres_summary <- summarize_categorical(netflix_clean, genres)
country_summary <- summarize_categorical(netflix_clean, country)
language_summary <- summarize_categorical(netflix_clean, language)

# combine them into one list of tables
categorical_tables <- list(
  "Genres" = genres_summary,
  "Country" = country_summary,
  "Language" = language_summary
)

# display each table neatly
for (name in names(categorical_tables)) {
  cat("\n\n")
  cat(paste0("### Table: ", name, "\n\n"))
  print(
    kable(categorical_tables[[name]],
          caption = paste("Distribution of", name),
          digits = 2) %>%
      kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
  )
}

# choosing interaction term
library(ggplot2)

top_genres <- names(sort(table(netflix_clean$genres), decreasing = TRUE))[1:6]


ggplot(filter(netflix_clean, genres %in% top_genres),
       aes(x = popularity, y = log(revenue), color = genres)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  scale_x_log10() +
  labs(
    title = "Interaction between Genre and Popularity on Log Revenue",
    x = "Popularity (log scale)",
    y = "Log Revenue (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())
