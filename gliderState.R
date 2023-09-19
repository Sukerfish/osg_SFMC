identify_casts <- function(data, surface_threshold = 0.1) {
  # Initialize vectors to store cast information
  casts <- character(nrow(data))
  
  # Find the index where depth data becomes available
  first_depth_index <- which(!is.na(data$osg_i_depth))[1]
  
  # Initialize casts for missing depth rows as "Unknown"
  casts[1:(first_depth_index - 1)] <- "Unknown"
  
  # Initialize the first cast based on the second data point with depth
  if (!is.na(data$osg_i_depth[first_depth_index + 1])) {
    if (data$osg_i_depth[first_depth_index + 1] > data$osg_i_depth[first_depth_index]) {
      casts[first_depth_index] <- "Downcast"
    } else {
      casts[first_depth_index] <- "Upcast"
    }
  }
  
  # Loop through the data to identify casts
  for (i in (first_depth_index + 1):nrow(data)) {
    if (!is.na(data$osg_i_depth[i]) && !is.na(data$osg_i_depth[i - 1])) {
      if (data$osg_i_depth[i] > data$osg_i_depth[i - 1]) {
        casts[i] <- "Downcast"
      } else if (data$osg_i_depth[i] < data$osg_i_depth[i - 1]) {
        casts[i] <- "Upcast"
      } else {
        casts[i] <- "Surface"
      }
    }
  }
  
  # Assign "Surface" to points with depth near zero or below the surface threshold
  casts[data$osg_i_depth < surface_threshold] <- "Surface"
  
  # Create a new column in the dataframe to store the cast information
  data$cast <- casts
  
  return(data)
}






library(tidyverse)
input <- gliderdf %>%
  #filter(!is.na(m_depth)) %>%
  filter(between(m_present_time, as.Date('2023-04-30'), as.Date('2023-05-01'))) %>%
  head(200) %>%
  select(m_present_time, m_pitch, osg_i_depth)

test <- identify_casts(gliderdf, surface_threshold = 1)

out <- test %>%
  #left_join(gliderdf) %>%
  select(m_present_time, cast, m_depth) %>%
  filter(!is.na(m_depth))


identify_casts <- function(data, surface_threshold = 0.1) {
  # Initialize vectors to store cast information and yo values
  casts <- character(nrow(data))
  yo <- numeric(nrow(data))
  
  # Find the index where depth data becomes available
  first_depth_index <- which(!is.na(data$osg_i_depth))[1]
  
  # Initialize casts for missing depth rows as "Unknown"
  casts[1:(first_depth_index - 1)] <- "Unknown"
  yo[1:(first_depth_index - 1)] <- NA
  
  # Initialize variables to keep track of the previous cast type and yo count
  prev_cast <- "Unknown"
  yo_count <- 0
  
  # Loop through the data to identify casts and update yo values
  for (i in first_depth_index:nrow(data)) {
    if (!is.na(data$osg_i_depth[i]) && !is.na(data$osg_i_depth[i - 1])) {
      if (data$osg_i_depth[i] > data$osg_i_depth[i - 1]) {
        casts[i] <- "Downcast"
        if (prev_cast == "Upcast") {
          yo_count <- yo_count + 1
        }
      } else if (data$osg_i_depth[i] < data$osg_i_depth[i - 1]) {
        casts[i] <- "Upcast"
      } else {
        casts[i] <- "Surface"
        yo_count <- yo_count  # Reset yo_count during surface
      }
    }
    prev_cast <- casts[i]
    yo[i] <- yo_count
  }
  
  # Assign "Surface" to points with depth near zero or below the surface threshold
  surface_indices <- which(data$osg_i_depth < surface_threshold)
  casts[surface_indices] <- "Surface"
  yo[surface_indices] <- NA
  
  # Create new columns in the dataframe to store the cast information and yo values
  data$cast <- casts
  data$yo <- yo
  
  return(data)
}










