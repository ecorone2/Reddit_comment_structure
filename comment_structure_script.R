
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

# Initializing "i" variable to use as a counter and "y" as a data frame
i <- 1
y <- data.frame()

# It will iterate a maximum of 100 times (unlikely to be needed)
# Iterations will stop running once the maximum comment depth is reached
# Sys.sleep used for readability
while (i < 100) {
  tmp2 <- tmp2 %>%
    mutate(
      parent_level = replace_na(as.numeric(plyr::mapvalues(
        parent_id,
        from = name,
        to = depth,
        warn_missing = F)),
        0
      ),
      depth = ifelse(parent_level == max(parent_level), (parent_level + 1), depth))
  i <- i + 1
  print(max(tmp2$depth))
  temp <- max(tmp2$depth)
  y <- data.frame(rbind(y, temp))
  Sys.sleep(0.5)
  if (identical(y[nrow(y)-1,], y[nrow(y),])){
    break
  }
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

# Start looping here
# For levels 2 to N

# Initializing "i" as a counter variable
# It will iterate until the maximum comment depth level is reached 
i <- 1

while (i < (max(tmp3$depth) + 1)) {
  print(paste("starting level", i, sep = ": "))
  tmp3$parent_order <- ifelse(tmp3$depth == (i + 1), 
      plyr::mapvalues(tmp3$parent_id, from = tmp3$name, to = tmp3$structure, warn_missing = F), tmp3$parent_order)
  
  tmp3 <- tmp3 %>% 
    mutate(structure = ifelse(depth == (i + 1), 
      paste(parent_order, within_order, sep = "_"), structure))
  
  i <- i +1
  Sys.sleep(1)
}

# Cleaning up excess variables
reddit_thread_with_structure <- tmp3 %>% 
  select(-(parent_level:parent_order))

# Cleaning up excess dataframes
rm(tmp1, tmp2, tmp3)
