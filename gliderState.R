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
  # Initialize vectors to store cast information and yo_number
  casts <- character(nrow(data))
  yo_number <- integer(nrow(data))
  
  # Find the index where depth data becomes available
  first_depth_index <- which(!is.na(data$osg_i_depth))[1]
  
  # Initialize casts and yo_number for missing depth rows as "Unknown" and NA
  casts[1:(first_depth_index - 1)] <- "Unknown"
  yo_number[1:(first_depth_index - 1)] <- NA
  
  current_yo_number <- 1  # Initialize current yo_number
  
  # Loop through the data to identify casts and assign yo_numbers
  for (i in (first_depth_index):nrow(data)) {
    if (!is.na(data$osg_i_depth[i]) && !is.na(data$osg_i_depth[i - 1])) {
      if (data$osg_i_depth[i] > data$osg_i_depth[i - 1]) {
        if (casts[i - 1] == "Upcast") {
          casts[i] <- "Downcast"
          yo_number[i] <- current_yo_number
        } else {
          casts[i] <- "Surface"
        }
      } else if (data$osg_i_depth[i] < data$osg_i_depth[i - 1]) {
        if (casts[i - 1] == "Downcast" || casts[i - 1] == "Surface") {
          casts[i] <- "Upcast"
          yo_number[i] <- current_yo_number
          current_yo_number <- current_yo_number + 1  # Increment yo_number
        } else {
          casts[i] <- "Surface"
        }
      } else {
        casts[i] <- "Surface"
        yo_number[i] <- NA
      }
    }
  }
  
  # Assign "Surface" to points with depth near zero or below the surface threshold
  casts[data$osg_i_depth < surface_threshold] <- "Surface"
  
  # Create new columns in the dataframe to store the cast information and yo_number
  data$cast <- casts
  data$yo_number <- yo_number
  
  return(data)
}


