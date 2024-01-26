# load packages
library(tidyverse)

# read the data
trans_cts <- read_csv("./data/counts_transformed.csv")
sample_info <- read_csv("./data/sample_info.csv")
test_result <- read_csv("./data_rnaseq/test_result.csv")

# gene column -> gene name
# baseMean column -> normalized expression level of a gene
# log2FoldChange -> amount of change btw the 2 conditions
# lcfSE -> stadard error associated to log2FoldChange
# stat -> statistics value computed as log2FoldChange/lcfSE
# pvalue -> p-value associated with the change
# padj -> p-value corrected for multiple hypotesis testing

# Challenge: making the MA plot
test_result %>% 
  ggplot(aes(log10(baseMean), log2FoldChange)) +
  geom_point(alpha = 0.1) +
  facet_wrap(vars(comparison))
# we want to use the pval to highlight the signif ones using a if test
test_result %>% 
  mutate(significant = ifelse(padj < 0.01, log2FoldChange, NA)) %>% 
  ggplot(aes(log10(baseMean), log2FoldChange)) +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = 0, color = 'steelblue') +
  geom_point(aes(y = significant), color = "firebrick", alpha = 0.2, size = 1) +
  facet_wrap(vars(comparison))

# Visualize expression trends
# 1. select significant gene names
candidate_gene <- test_result %>% 
  filter(padj < 0.01) %>% 
  pull(gene) %>%  # equivalent of test_result[,"gene"] a.k.a. test_result$gene but in the tidyverse world (which allows us to use the pipe "%>%" )
  unique()

# 2. we need to regenerate trans_cts_long as we did yesterday
trans_cts_long <- trans_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3,
               names_to = "sample",
               values_to = "cts") %>% 
  full_join(sample_info, by = "sample")

# 3. fiter trans_cts_long for candidate geans and compute mean
# expr. value
trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_gene) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts =  mean(cts), nrep = n()) %>% 
  ungroup() 

# 4. finally plot trends
trans_cts_mean %>% 
  ggplot(aes(x =  minute, y =  mean_cts)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  facet_grid(rows = vars(strain))

# Scaling data to improve visualization
trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_gene) %>% 
  group_by(gene) %>%
  mutate(cts_scaled = ( cts - mean(cts)) / sd(cts)) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts_scaled =  mean(cts_scaled), nrep = n()) %>% 
  ungroup() 

trans_cts_mean %>% 
  ggplot(aes(x =  minute, y =  mean_cts_scaled)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  geom_hline(yintercept = 0, color = 'brown', linetype = "dashed") +
  facet_grid(rows = vars(strain))

# Clustering:
# 1. Create a matrix of counts
hclust_matrix <- trans_cts %>% 
  select(-gene) %>% 
  as.matrix()

# assign rownames
rownames(hclust_matrix) <- trans_cts$gene

hclust_matrix <- hclust_matrix[candidate_gene, ]

hclust_matrix <- hclust_matrix %>% 
  # transpose the matrix so genes are as columns
  t() %>% 
  # apply scalling to each column of the matrix (genes)
  scale() %>% 
  # transpose back so genes are as rows again
  t()

gene_dist <- dist(hclust_matrix)

# Hierarchical clustering
gene_hclust <- hclust(gene_dist, method = "complete")
plot(gene_hclust, labels = F) 
abline(h = 10, col = "brown", lwd = 2)

# make clusters based on the number of clusters I want
cutree(gene_hclust, k = 5)

gene_cluster <- cutree(gene_hclust, k = 5) %>% 
  enframe() %>% # create a tibble out of it
  rename(gene = name, cluster = value) # rename default names

trans_cts_cluster <- trans_cts_mean %>% 
  inner_join(gene_cluster, by = "gene")

trans_cts_cluster %>% 
  ggplot(aes(x = minute, y = mean_cts_scaled)) +
  geom_line(aes(group = gene)) +
  facet_grid(cols = vars(cluster), rows = vars(strain))

# now we want a heatmap
library(ComplexHeatmap)

Heatmap(hclust_matrix, show_row_names = F)
