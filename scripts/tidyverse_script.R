library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")
str(surveys)

# let's select a columns
select(surveys, plot_id, species_id, weight)
# select all BUT the columns we don't want
select(surveys, -record_id, -species_id)

# let's filter rows
filter(surveys, year == 1995)
filter(surveys, year == 1995, sex == "M")

surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight)

surveys_sml2 <- select(filter(surveys, weight < 5),
                       species_id, sex, weight)

# Introducing THE PIPE: %>% 
surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight)

surveys %>% 
  filter(year<1995) %>% 
  select(year,sex,weight)

surveys %>% 
  mutate(weight_kg = weight/1000,
         weight_lb = weight_kg*2.2) %>% 
  View()

# The split-apply-combine paradigm
surveys %>% 
  group_by(sex) %>% 
  summarise(mean_weight = mean(weight, na.rm = T))

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight =  mean(weight, na.rm = T)) %>% 
  print(n = 15)

surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight =  mean(weight, na.rm = T), min_weight = min(weight)) %>%
  arrange(desc(min_weight))

surveys %>% 
  count(sex, species) %>% 
  arrange(species, desc(n))

# Challenge
#1
surveys %>% 
  count(plot_type)

#2
surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  group_by(species_id) %>% 
  summarise(mean_hfl = mean(hindfoot_length), 
            min_hfl = min(hindfoot_length),
            max_hfl = max(hindfoot_length),
            n = n())

#3
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  filter(weight == max(weight)) %>% 
  select(year, genus, species_id, weight) %>% 
  arrange(year)

# Pivoting
surveys_gw <- surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(plot_id, genus) %>% 
  summarise(mean_weight = mean(weight))

str(surveys_gw)

surveys_wide <- surveys_gw %>% 
                  pivot_wider(names_from = genus,
                  values_from = mean_weight,
                  values_fill = 0)

surveys_wide %>% 
  pivot_longer(names_to = "genus", values_to = "mean_weight", cols = -plot_id)

# Challenge
# 1
surveys_long <- surveys %>% 
  pivot_longer(names_to = "measurement", values_to = "value",
               cols = c(hindfoot_length,weight))

# 2
surveys_long %>% 
  group_by(year, measurement, plot_type) %>% 
  summarise(mean_value = mean(value, na.rm = T)) %>% 
  pivot_wider(names_from = measurement, values_from = mean_value)

# Create dataset for the next part
surveys_complete <- surveys %>% 
                    filter(!is.na(weight),
                           !is.na(hindfoot_length),
                           !is.na(sex))

write_csv(surveys_complete,
          file = "data/surveys_complete.csv") # or whatever folder you want to save it (but it needs to exist already)
  