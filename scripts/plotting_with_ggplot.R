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
