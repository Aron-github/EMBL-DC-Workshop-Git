library(tidyverse)

surveys_complete <- read_csv("data/surveys_complete.csv")

plt <- ggplot(data = surveys_complete,
       mapping = aes(x = weight, y = hindfoot_length))

plt # plots an empty canvas
str(plt)

plt + 
  geom_point() # our first plot!

plt +
  geom_point() +
  ggtitle(label = "My first plot!")

# 1. define ggplot object
# plt <- ggplot(data = <data.frame>, mapping = <aesthetics>)
# x aesthetics
# y aesthetics
# color aesthetics
# shape aesthetics
# ....
# 2. add geometry layer(s)
# geometry functions have "predictable" names
# geom_{point, line, bar, histogram, violin, hex, ...}

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()

# let's save that in an object
plt <- ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()

plt +
  ggtitle("Weight VS hindfoot length")

install.packages("hexbin")
library(hexbin)
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_hex()

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = 'blue') # alpha sets the transparency

# what if I want to use color to map another variable? 
ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, aes(color = species_id)) 

# I can also set the color mapping in the overall ggplot call
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = weight, 
    y = hindfoot_length,
    color = species_id
    )
) +
  geom_point(alpha = 0.1, aes(color = species_id)) 

# Challenge
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight,
    color = plot_type)
) +
geom_point()
# but geom_point() is not really the best choice here...

ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight)
) +
  geom_boxplot()

# or also, by overlaying boxplots AND points
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight)
) +
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "salmon")

# even better
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight)
) +
  geom_jitter(alpha = 0.3, color = "salmon") + 
  geom_boxplot(outlier.shape = NA, fill = NA) # specify transparent boxplots and rm the outliers

# Challenge
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight)
) +
  geom_violin() 

# Another challenge
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = hindfoot_length)
) +
  geom_jitter(alpha = 0.3, aes(color = plot_id)) + 
  geom_boxplot(outlier.shape = NA) 
# plot_id is "seen" as a numeric value, but for us it's a discrete one (i.e. a factor)

# how to define colors:
# named: "red", "green", ...
# rgb values: rgb(red = .3, green = .3, blue = .3)
# hexadecimal code: "dedede"

yearly_count <- surveys_complete %>% 
  count(year, genus)


ggplot(data = yearly_count,
       mapping = aes(
         x = year,
         y = n,
         group = genus)) +
  geom_line()

ggplot(data = yearly_count,
       mapping = aes(
         x = year,
         y = n,
         color = genus)) +
  geom_line()
