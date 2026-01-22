# To-Do
  # 1: Make a plot, table, etc. of the most common ten species in VA with number of trees
  # Specifically number of trees, over time
  # https://doserlab.com/files/rfia/articles/basicestimation
  # 2: Find number of dead trees each year (consult database guide)
  # 3: Connect tree and plot table, find the average number of trees per plot
  # Explore Condition table

#install/load rFIA
#setwd("FIA-exploration")
#install.packages("rFIA")
library(rFIA)
library(dplyr)
library(ggplot2)

rm(list = ls())

##### 0 - Data Prep #####

# Download FIA/read in data 

# options(timeout = 3600)
#VA_FIA <- getFIA("VA",dir ="data/", tables = c("TREE", "PLOT")) #download just the tree & plot table for VA
VA_FIA <- readFIA(dir = "data/", states = "VA")

# Read additional data
# Species codes and common names - sourced from "https://www.uvm.edu/femc/CI4/data/archive/project/federal-forest-inventory-analysis-data-for/dataset/species-codes-used-forest-inventory-analysis"
species_names <- read.csv("data/Z0311_FIA_REF_SPECIES.csv")
  
# Initial data prep
species_names <- species_names %>%
  mutate(SCIENTIFIC_NAME = paste(GENUS, SPECIES))
  
plot_table <- VA_FIA$PLOT 
tree_table <- VA_FIA$TREE 

tree_table_prep <- inner_join(tree_table, species_names, by = "SPCD")

tree_table_fia <- tree_table_prep %>% 
  filter(INVYR >= 2000) %>%
  mutate(TREE_UID = paste0(UNITCD,COUNTYCD,PLOT,SUBP,TREE)) %>%
  select(CN, TREE_UID, INVYR, STATUSCD, DIA, SPCD, SCIENTIFIC_NAME)

options(scipen = 99)


##### 1 - Top 10 species in Virginia #####

top_10 <- tree_table_fia %>%
  select(SPCD) %>% 
  count(SPCD, sort = TRUE) %>%
  slice_head(n = 10)

top_10_yr <- tree_table_fia %>%
  select(SPCD, INVYR, SCIENTIFIC_NAME) %>%
  group_by(INVYR) %>%
  count(SCIENTIFIC_NAME, sort = TRUE) %>%
  slice_head(n = 10)

top_10_line <- ggplot(top_10_yr, aes(x = INVYR, y = n, group = SCIENTIFIC_NAME, color = SCIENTIFIC_NAME)) + 
  geom_line() + 
  geom_point() +
  theme_minimal() + 
  labs(
    x = "Inventory Year", 
    y = "Number of Trees", 
    color = "Species",
    title = "Top 10 Species in Virginia"
  )


top_10_line

ggsave("display/graph1.png", width = 10, height = 8)


top_10_barplot_data <- top_10_yr %>%
  filter(INVYR %in% c(2000, 2005, 2010, 2015, 2020, 2024)) %>%
  slice_head(n = 5)

top_10_barplot <- ggplot(top_10_barplot_data, aes(x = SCIENTIFIC_NAME, y = n, fill = SCIENTIFIC_NAME)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ INVYR, scales = "free_x") +
  theme_minimal() + 
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "bottom") +
  #scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Species", 
    y = "Number of Trees", 
    title = "Top 5 Species in Virginia"
  )

top_10_barplot

ggsave("display/graph2.png", width = 10, height = 8)


##### 2 - Calculate the number of dead trees each year #####
# Tree status - STATUSCD
  # 1 - Live
  # 2 - Dead
  # 3 - Removed due to human activity

deceased_trees_data <- tree_table_fia %>%
  filter(STATUSCD == 2) %>%
  group_by(INVYR) %>%
  count(STATUSCD, sort = TRUE)
  

deceased_trees_line <- ggplot(deceased_trees_data, aes(x = INVYR, y = n)) + 
  geom_line(color = "steelblue", size = 2) + 
  geom_point(size = 3) +
  theme_minimal() + 
  labs(
    x = "Inventory Year", 
    y = "Number of Trees", 
    title = "Number of Deceased Trees in Virginia"
  )

deceased_trees_line

ggsave("display/graph3.png", height = 10, width = 8)

deceased_trees_bar <- ggplot(deceased_trees_data, aes(x = INVYR, y = n)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    x = "Inventory Year", 
    y = "Number of Trees", 
    title = "Number of Deceased Trees in Virginia"
  )

deceased_trees_bar

ggsave("display/graph4.png", height = 10, width = 8)

top_10_deceased <- tree_table_fia %>%
  filter(STATUSCD == 2) %>%
  select(INVYR, SCIENTIFIC_NAME) %>%
  count(SCIENTIFIC_NAME, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(SCIENTIFIC_NAME)

deceased_tree_sp <- tree_table_fia %>%
  mutate(
    SCIENTIFIC_NAME = case_when(
      SCIENTIFIC_NAME %in% top_10_deceased ~ SCIENTIFIC_NAME, 
      TRUE ~ "Other")
  )

deceased_sp_data <- deceased_tree_sp %>% 
  filter(STATUSCD == 2) %>%
  select(INVYR, SCIENTIFIC_NAME) %>%
  group_by(INVYR) %>%
  count(SCIENTIFIC_NAME, sort = TRUE)

deceased_sp_bar <- ggplot(deceased_sp_data, aes(x = INVYR, y = n, fill = SCIENTIFIC_NAME)) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +
  theme_minimal() + 
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Inventory Year", 
    y = "Number of Trees",
    fill = "Species", 
    title = "Number of Deceased Trees by Species"
  )

deceased_sp_bar

ggsave("display/graph5.png", height = 10, width = 8)


##### 3 - Find the average number of trees per plot #####

# CN test - Sanity check
tree_table_test <- tree_table %>%
  filter(PLT_CN == 163888739010854)

nrow(tree_table_test)

plot_table_test <- plot_table %>%
  filter(CN == 163888739010854) %>%
  mutate(
    PLT_CN = CN
  )

combined_df_test <- tree_table_test %>%
  left_join(plot_table, by = "CN")

# Data prep
plot_table_prep <- plot_table %>%
  mutate(PLT_CN = CN)

combined_df <- tree_table %>%
  left_join(plot_table_test)

# Calculation
avg_trees <- nrow(combined_df)/length(unique(combined_df$PLT_CN))
  