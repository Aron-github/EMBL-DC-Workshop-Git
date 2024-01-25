library(tidyverse)

# 1. Import data
raw_cts <- read_csv("data_rnaseq/counts_raw.csv")
trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")
sample_info <- read_csv("data_rnaseq/sample_info.csv")
test_result <- read_csv("data_rnaseq/test_result.csv")

# 2. number of samples is in the "sample_info" table
nrow(sample_info)

# 4. this can be taken from the table of counts
nrow(trans_cts)

# We need the data in long format for plotting
trans_cts_long <- trans_cts %>% 
  pivot_longer(wt_0_r1:mut_180_r3, names_to = "sample", values_to = "cts")

# To combine two tables we can use the join functions
trans_cts_long <- full_join(trans_cts_long, sample_info, by = ("sample"))

# Make the plot
trans_cts_long %>%
  # add pseudo-count of 1 because log(0) = -Inf
  ggplot(aes(x = cts, colour = replicate)) + 
  geom_freqpoly(binwidth = 1) + 
  facet_grid(rows = vars(strain), cols = vars(minute))

# challenge: produce a similar plot for the raw counts
raw_cts_long <- raw_cts %>% 
  pivot_longer(wt_0_r1:mut_180_r3, names_to = "sample", values_to = "cts")

# Join with sample information table
raw_cts_long <- full_join(raw_cts_long, sample_info, by = ("sample"))

# Make the plot
raw_cts_long %>%
  # add pseudo-count of 1 because log(0) = -Inf
  ggplot(aes(x = cts, colour = replicate)) + 
  scale_x_log10() + # this however makes the x axis a bit hard to read
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute))

raw_cts_long %>%
  # add pseudo-count of 1 because log(0) = -Inf
  ggplot(aes(x = log10(cts), colour = replicate)) +  # now the x axis will have the exponent
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute))

# to suppress the warning on the non-finite values (the log of 0 is -Inf)
raw_cts_long %>%
  # add pseudo-count of 1 because log(0) = -Inf
  ggplot(aes(x = log10(cts + 1), colour = replicate)) +  # now the x axis will have the exponent
  geom_freqpoly(binwidth = 1) + # NOTE: this binwith is now on log scale!!!
  facet_grid(rows = vars(strain), cols = vars(minute))
