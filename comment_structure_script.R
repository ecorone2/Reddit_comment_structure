
load("comment_structure.RData")

library(tidyverse)

# Creating working copy
tmp1 <- usable_df %>% 
  select(link_id, id, author, parent_id, body, name, created_utc)

# Assigning levels 1 to N

tmp2 <- tmp1 %>% 
  mutate(depth = ifelse(link_id == parent_id, "level 1", ""))

level1 <- tmp2 %>% 
  filter(depth == "level 1") %>% 
  select(name)

tmp2 <- tmp2 %>% 
  mutate(depth = ifelse(parent_id %in% level1$name, "level 2", depth))

level2 <- tmp2 %>% 
  filter(depth == "level 2") %>% 
  select(name)

tmp2 <- tmp2 %>% 
  mutate(depth = ifelse(parent_id %in% level2$name, "level 3", depth))

level3 <- tmp2 %>% 
  filter(depth == "level 3") %>% 
  select(name)

tmp2 <- tmp2 %>% 
  mutate(depth = ifelse(parent_id %in% level3$name, "level 4", depth))

# Assigning comment 1 to N by parent_id
tmp3 <- tmp2 %>%
  group_by(parent_id) %>% 
  mutate(within_order = row_number())

# For level 2
tmp3$parent_order <- ifelse(tmp3$depth == "level 1", tmp3$within_order, plyr::mapvalues(tmp3$parent_id, from = tmp3$name, to = tmp3$within_order))

tmp4 <- tmp3 %>% 
  mutate(level2 = ifelse(depth == "level 2", 
                         paste(parent_order, within_order, sep = "_"), ""))

# For level 3
tmp4$parent_order2 <- ifelse(tmp4$depth == "level 3", 
                      plyr::mapvalues(tmp4$parent_id, from = tmp4$name, to = tmp4$level2), "")

tmp4 <- tmp4 %>% 
  mutate(level3 = ifelse(depth == "level 3", 
                         paste(parent_order2, within_order, sep = "_"), ""))

# For level 4
tmp4$parent_order3 <- ifelse(tmp4$depth == "level 4", 
                      plyr::mapvalues(tmp4$parent_id, from = tmp4$name, to = tmp4$level3), "")

tmp4 <- tmp4 %>% 
  mutate(level4 = ifelse(depth == "level 4", 
                         paste(parent_order3, within_order, sep = "_"), ""))

# Consolidating levels
tmp5 <- tmp4 %>% 
  mutate(structure = ifelse(depth == "level 1", parent_order, 
                            ifelse(depth == "level 2", level2, 
                                   ifelse(depth == "level 3", level3, 
                                          ifelse(depth == "level 4", level4, ""))))) %>% 
  select(-(depth:level4))