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
  ggplot(aes(x = cts, colour = replicate)) + # for a frequency plot, we only need the x axis 
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

# instead of the frequency polygon, let's make a boxplot
raw_cts_long %>%
  # make sure minute is specified as a factor
  ggplot(aes(x = factor(minute), y = log10(cts + 1), fill = strain)) + 
  geom_boxplot() + 
  facet_grid(cols = vars(replicate))

# Correlation between wt samples at T0 and T5
# in this case is easier to use directly the wide data
trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_30_r1)) + 
  geom_point() +
  geom_abline(colour = "brown")

trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) + 
  geom_point() +
  geom_abline(colour = "brown")

# what if I want to look at ALLL correlations btw all samples in the data?
trans_cts_corr <- trans_cts %>% 
  select(-gene) %>% 
  cor(method = "spearman") # it's a correlation matrix

# Let's use a heatmap to visualize results
library(corrr)

rplot(trans_cts_corr) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Compare trans_cts and raw_cts
summary(raw_cts_long$cts)
summary(trans_cts_long$cts)

raw_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) + 
  geom_point() 

raw_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) + 
  geom_point() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

# let's look at the mean and variance of counts
raw_cts_long %>% 
  group_by(gene) %>% 
  summarise(mean_cts = mean(cts), var_cts =var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point() +
  geom_abline(colour = "brown") +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")
# rna seq counts are overdispersed
# (they are not distributed like Poisson, where mean = variance)

# that's why we transformed to normalize them.
# let's check that this is true by looking at the normalized (transformed) data
#  which have been manipulated with Deseq2
trans_cts_long %>% 
  group_by(gene) %>% 
  summarise(mean_cts = mean(cts), var_cts =var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point()
# the variance is stabilized, while the mean is free to change