weight_g <- c(50, 60, 65, 82)

animals <- c("mouse", "rat", "dog")

# get the length of the vector
length(animals)

# get the type of data contained in the vector
class(animals)
class(weight_g)

# structure of the object
str(animals)

# How to add an element to the beginning of a vector
animals <- c("cincilla", animals)
animals <- c(animals, "frog")

typeof(animals)

# What happens in the next cases
num_char <- c(1, 2, 3, "a")
num_logical <- c(1, 2, 3, T)
char_logical <- c("a", "b", "c", TRUE)
tricky <- c(1, 2, 3, "4")
# logical -> numeric -> character
# logical -> character

# subsetting a vector
animals[2]
animals[c(1, 2)]

more_animals <- animals[c(1, 2, 3, 2, 1, 4)]

weight_g
weight_g[c(F, FALSE, T, TRUE)]
weight_g > 63
weight_g[weight_g > 63]
weight_g[weight_g > 63 & weight_g < 80]
weight_g[weight_g < 58 | weight_g > 80]
weight_g==65
# <, >, ==, !=, <=, >=, !

animals[animals == "rat" | animals == "frog"]
# animals[ animals == "rat" | animals == "frog" | animals ...]
# %in% helps us find all elements corresponding to a vector of elements of our choice
animals %in% c("rat", "frog", "cat", "duck", "dog")
animals[animals %in% c("rat", "frog", "cat", "duck", "dog")]

# An axample of a vector with missing data
heights <- c(2, 4, 4, NA, 6)
mean(heights)
mean(heights, na.rm = T)
max(heights, na.rm = T)
# "NA"
# NA
# TRUE
# identify which are the missing data (a.k.a NA)
is.na(heights)
heights[!is.na(heights)]
# omit the missing data
na.omit(heights)
# extract the complete cases
heights[complete.cases(heights)]

# challenge
heights <- c(63, 69, 60, 65, NA, 68, 61)
hieghts_no_na <- na.omit(heights)
heights_no_na <- heights[!is.na(heights)]

median(heights, na.rm = T)

heights_no_na[heights_no_na > 67]
length(heights_no_na[heights_no_na > 67])
sum(heights_no_na > 67)
