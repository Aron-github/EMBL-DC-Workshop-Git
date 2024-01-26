library(tidyverse)

trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")
sample_info <- read_csv("data_rnaseq/sample_info.csv")

pca_matrix <- trans_cts %>% 
  column_to_rownames("gene") %>% 
  #converts our dataframe to a matrix
  as.matrix() %>% 
  # t to transpose the matrix
  t()

sample_pca <- prcomp(pca_matrix) 
  
class(sample_pca)
str(sample_pca)
summary(sample_pca)

pca_matrix[1:5,1:5]

as_tibble(pca_matrix)
as_tibble(pca_matrix, rownames = "sample")

pc_eigenvalues <- sample_pca$sdev^2

# let's adapt our data into a tibble ready to be plotted with ggplot
pc_eigenvalues <- tibble(PC = factor(c(1:length(pc_eigenvalues))),
                         variance = pc_eigenvalues) %>% 
  mutate(pct = variance / sum(variance)*100 ) %>% 
  mutate(pct_cum = cumsum(pct))

# pareto plot / chart
pc_eigenvalues %>% 
  ggplot(aes(x = PC)) +
  geom_col(aes(y = pct)) +
  geom_line(aes(y = pct_cum, group = 1)) +
  geom_point(aes(y = pct_cum)) +
  # geom_hline(yintercept = 90) + # if you want to get an idea of how many PC you need to cover 90% of your variance
  labs(x= "Principal component",
       y = "Fraction of explained variance")
# this is plotting in BARS the variance explained by each PC
# the LINE + POINTS plots the cumulative variance explained

pc_scores <- sample_pca$x %>% 
  as_tibble(rownames = "sample")

pc_scores %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point()

pc_scores %>% 
  full_join(sample_info, by = "sample") %>% 
  ggplot(aes(x = PC1, y = PC2, color = factor(minute), shape = strain)) +
  geom_point()

# let's look at the loadings plot as well
pc_loadings <- sample_pca$rotation %>% 
  as_tibble(rownames = "gene")

top_genes <- pc_loadings %>% 
  select(gene, PC1, PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loadings") %>% 
  group_by(PC) %>% 
  arrange(desc(abs(loadings))) %>% 
  slice(1:10) %>% 
  pull(gene) %>% 
  unique()

top_loadings <- pc_loadings %>% 
  filter(gene %in% top_genes)

ggplot(data = top_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.1, "in")),
               color = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))

loadings_plot <- ggplot(data = top_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.1, "in")),
               color = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))

pca_plot <- pc_scores %>% 
  full_join(sample_info, by = "sample") %>% 
  ggplot(aes(x = PC1, y = PC2, color = factor(minute), shape = strain)) +
  geom_point()

library(patchwork)
# we can use it to arrange plots together in panels
# Side by side:
(pca_plot | loadings_plot)
# One on top of the others
(pca_plot / loadings_plot)

# or even more complex than this
(pca_plot | pca_plot | pca_plot)/loadings_plot +
  plot_annotation(tag_levels = "A")

# libraries with additional functions
library(ggfortify)
# pca plot without having to specify much
autoplot(sample_pca)
autoplot(sample_pca, data = sample_info, 
         colour = "minute", 
         shape = "strain")

library(broom)
# PC variances (eigen values) -> instead of having to extract it and convert it
tidy(sample_pca, matrix = "eigenvalues")
# variable loadings
tidy(sample_pca, matrix = "loadings")
