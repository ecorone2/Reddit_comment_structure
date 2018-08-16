
# Load dataset
load("comment_structure.RData")

# Package needed
library(tidyverse)

# Creating working copy
tmp1 <- usable_df %>% 
  select(link_id, name, parent_id, author, body, created_utc) %>% 
  arrange(created_utc) # important to get accurate comment order

# Assigning levels 1 to N
# Likely a mandatory start with level 1
tmp2 <- tmp1 %>% 
  mutate(depth = ifelse(link_id == parent_id, 1, 0),
         created_utc = as.POSIXct(created_utc, origin = "1970-01-01"))

# While loop to run this "i" times
i <- 1

# I already know that this thread has a maximum of 4 levels
while (i < 5) {
  tmp2 <- tmp2 %>%
    mutate(
      parent_level = replace_na(as.numeric(plyr::mapvalues(
        parent_id,
        from = name,
        to = depth,
        warn_missing = F)),
        0
      ),
      depth = ifelse(parent_level == max(parent_level), (parent_level+ 1), depth))
  i <- i + 1
  print(max(tmp2$depth))
  Sys.sleep(0.5)
}

# Check number of comments for each depth level
depth_levels <- tmp2 %>% 
  group_by(depth) %>% 
  summarize(n_obs = n())

# Assigning comment 1 to N by parent_id
tmp3 <- tmp2 %>%
  group_by(parent_id) %>% 
  mutate(within_order = row_number())

# Creating the structure
# Likely another mandatory start for level 1

# For level 1

tmp3$parent_order <- ifelse(tmp3$depth == 1, tmp3$within_order, 0L)

tmp3 <- tmp3 %>%
  mutate(structure = ifelse(depth == 1,
                            paste(within_order), ""))

# Start loop here?
# For level 2

tmp3$parent_order <- ifelse(tmp3$depth == 2, 
      plyr::mapvalues(tmp3$parent_id, from = tmp3$name, to = tmp3$within_order, warn_missing = F), tmp3$parent_order)

tmp3 <- tmp3 %>% 
  mutate(structure = ifelse(depth == 2, 
      paste(parent_order, within_order, sep = "_"), structure))

# For level 3
tmp3$parent_order <- ifelse(tmp3$depth == 3, 
      plyr::mapvalues(tmp3$parent_id, from = tmp3$name, to = tmp3$structure, warn_missing = F), tmp3$parent_order)

tmp3 <- tmp3 %>% 
  mutate(structure = ifelse(depth == 3, 
       paste(parent_order, within_order, sep = "_"), structure))

# For level 4
tmp3$parent_order <- ifelse(tmp3$depth == 4, 
       plyr::mapvalues(tmp3$parent_id, from = tmp3$name, to = tmp3$structure, warn_missing = F), tmp3$parent_order)

tmp3 <- tmp3 %>% 
  mutate(structure = ifelse(depth == 4, 
       paste(parent_order, within_order, sep = "_"), structure))

# Cleaning up excess variables
tmp4 <- tmp3 %>% 
  select(-(parent_level:parent_order))
