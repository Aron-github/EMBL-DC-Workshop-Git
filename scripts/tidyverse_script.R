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
