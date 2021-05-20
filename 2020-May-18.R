library(scales)
library(ggthemes)
library(lubridate)
library(tidytuesdayR)
library(tidyverse)

theme_set(theme_minimal())

tt_output <- tt_load("2021-05-18")

survey <- tt_output$survey

survey$timestamp <- mdy_hms(survey$timestamp)

survey %>% 
  mutate(how_old_are_you = how_old_are_you %>% 
           fct_infreq() %>% fct_rev()) %>% 
  ggplot(aes(how_old_are_you, fill = how_old_are_you)) +
  geom_bar() +
  labs(title = "Age distribution of respondents",
       x = "Age bracket (Years)",
       y = "No. of Respondents") +
  theme(legend.position = "drop") +
  scale_y_continuous(labels = comma, breaks = seq(0,12000, 1500 )) +
  scale_x_discrete() +
  expand_limits(y = 0)

## There were 10 under 18 respondents.

survey[which(survey$how_old_are_you == "under 18"),]

## Distribution of the times when the survey was submitted.

survey %>% 
  ggplot(aes(timestamp)) +
  geom_freqpoly(bins = 50) +
  scale_x_datetime(date_breaks = "2 days",
                   date_labels = "%b%d") +
  expand_limits(y = 0) +
  scale_y_continuous(labels = comma,
                     breaks = seq(0,6500, 1000)) +
  labs(title = "Distribution of Submissions",
       x = "Date Submitted",
       y = "No. of Submissions")

## Distribution of the salaries

survey %>% 
  filter(currency == "USD" & annual_salary <500000) %>%
  mutate(how_old_are_you = fct_reorder(how_old_are_you,
                                       annual_salary)) %>%
  ggplot(aes(x = how_old_are_you,
             y = annual_salary,
             fill = how_old_are_you)) + 
  geom_boxplot() +
  scale_y_continuous(labels = dollar) +
  theme(legend.position = "drop") +
  labs(title = "Annual income distribution (US)",
       subtitle = "for those with annual earnings under $500,000",
       x = "Age brackets",
       y = NULL)

survey %>% 
  filter(currency == "USD" & annual_salary > 500000) %>%
  filter(annual_salary < 25000000) %>% 
  mutate(how_old_are_you = fct_reorder(how_old_are_you,
                                       annual_salary)) %>%
  ggplot(aes(x = how_old_are_you,
             y = annual_salary,
             fill = how_old_are_you)) + 
  geom_boxplot() +
  scale_y_continuous(labels = dollar) +
  theme(legend.position = "drop") +
  labs(title = "Annual income distribution",
       subtitle = "for those with annual earnings over $500,000",
       x = "Age brackets",
       y = NULL)


survey %>% 
  mutate(highest_level_of_education_completed = 
           fct_reorder(highest_level_of_education_completed, annual_salary)) %>% 
  filter(!is.na(highest_level_of_education_completed) & currency == "USD") %>% 
  group_by(highest_level_of_education_completed) %>% 
  summarize(average_salaries = mean(annual_salary)) %>% 
  ggplot(aes(x = average_salaries, 
             y = highest_level_of_education_completed,
             fill = highest_level_of_education_completed)) +
  geom_col() +
  theme(legend.position = "drop") +
  labs(title = "Average annual salary compared the education level",
       x = "Average annual salary",
       y = NULL) +
  scale_x_continuous(labels = dollar)

