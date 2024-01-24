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
         y = n)) +
  geom_line()
# This doesn't look right

ggplot(data = yearly_count,
       mapping = aes(
         x = year,
         y = n,
         group = genus)) +
  geom_line()
# this looks better!

ggplot(data = yearly_count,
       mapping = aes(
         x = year,
         y = n,
         color = genus)) +
  geom_line()


yearly_count %>% 
  ggplot(mapping = aes(x = year,
                       y = n,
                       color = genus)) +
  geom_line()

yearly_count_graph <- surveys_complete %>% 
  count(year, genus) %>% 
  ggplot(mapping = aes(x = year,
                       y = n,
                       color = genus)) +
  geom_line()

yearly_count_graph

# Maybe it's too crowded?
# Let's introduce faceting:
ggplot(data = yearly_count,
       mapping = aes(
         x = year,
         y = n)) +
  geom_line() +
  facet_wrap(facets = vars(genus))
   
surveys_complete %>% 
  count(year, genus, sex) %>%     
  ggplot(mapping = aes(
         x = year,
         y = n,
         color = sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

# faceting by 2 variables
surveys_complete %>% 
  count(year, genus, sex) %>%     
  ggplot(mapping = aes(
    x = year,
    y = n,
    color = sex)) +
  geom_line() +
  facet_grid(
    rows = vars(sex),
    cols = vars(genus)
  )

# if I only want rows
surveys_complete %>% 
  count(year, genus, sex) %>%     
  ggplot(mapping = aes(
    x = year,
    y = n,
    color = sex)) +
  geom_line() +
  facet_grid(
    rows = vars(genus),
  )

surveys_complete %>% 
  count(year, genus, sex) %>%     
  ggplot(mapping = aes(
    x = year,
    y = n,
    color = sex)) +
  geom_line() +
  facet_grid(
    cols = vars(genus),
  )

# let's change themes
surveys_complete %>% 
  count(year, genus, sex) %>%     
  ggplot(mapping = aes(
    x = year,
    y = n,
    color = sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus)) +
  theme_bw()

plt <- surveys_complete %>%
  count(year, genus, sex) %>%      
  ggplot(mapping = aes(
    x = year,
    y = n,
    color = sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus)) + # you can add scales = "free_y" to let every subplot have its own y axis
  theme_bw(base_size = 18)
ggsave(filename = "fig/plot.pdf", 
       plot = plt,
       width = 10,
       height = 10)

# change a lot of aspects of the plot through the theme command
plt + 
labs(title = "Observed genera through time",
     x = "Year of observation",
     y = "Number of individuals") +
  theme(legend.position = "bottom", # "none" f you want ot remove it
        aspect.ratio = 1,
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())
  
# as a general suggestion:
# work incrementally, start from an easy plot 
# and only later adjust every detail
# the plot code can become pretty cumbersome
# but google is your friend!
# Also, think that each additional "layer" allows you to add details
# x and y axis,
# color,
# shape,
# faceting, ...

# Our last super complicated plot
plt <- surveys_complete %>%
  count(year, genus, sex) %>%
  ggplot(
    mapping = aes(
      x=year,
      y=n,
      color = sex)) +
  geom_line() +
  facet_wrap(facet= vars(genus),
             scales = "free"
  ) +
  scale_color_manual(values = c("tomato", "dodgerblue"),
                     labels = c("female", "male"),
                     name = "Sex") +
  xlab("Years of observation") +
  ylab("Number of individuals") +
  ggtitle("Observed genera over time") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom", 
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 45,
                               hjust = 1),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    strip.background =  element_blank()
  )
plt
