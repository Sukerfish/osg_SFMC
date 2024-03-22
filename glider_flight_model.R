#glider model
library(lme4)
library(broom)
library(broom.mixed)
library(lmerTest)

#load(paste0("./Data/", "M123eng_usf-jaialai", ".RData"))

dailydf <- gliderdf %>%
  #add value key for status
  mutate(cast_fact = ifelse(cast == "Surface", 1, 0)) %>%
  group_by(yo_id) %>%
  mutate(ydepth = max(osg_i_depth, na.rm = TRUE),
         y_infdepth = min(osg_i_depth, na.rm = TRUE),
         bpump_delta = max(m_ballast_pumped, na.rm = TRUE) - min(m_ballast_pumped, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ddate = floor_date(m_present_time,
                            unit = "hours")) %>%
  group_by(ddate) %>%
  mutate(ahr_rate = (max(m_coulomb_amphr_total, na.rm = TRUE)-min(m_coulomb_amphr_total, na.rm = TRUE))/(as.numeric(max(m_present_time, na.rm = TRUE))-as.numeric(min(m_present_time, na.rm = TRUE)))*86400) %>%
  #summarise_if(is.numeric, mean, na.rm = TRUE)
  summarise(batt_avg = mean(m_battery, na.rm = TRUE),
            gps_avg = mean(m_gps_lat, na.rm = TRUE),
            #ahr_avg = mean(m_coulomb_amphr_total, na.rm = TRUE),
            ahr_rate_avg = mean(ahr_rate, na.rm = TRUE),
            water_depth_avg = mean(m_water_depth, na.rm = TRUE),
            dive_depth_avg = mean(ydepth, na.rm = TRUE),
            inf_depth_avg = mean(y_infdepth, na.rm = TRUE),
            roll_avg = mean(m_roll, na.rm = TRUE),
            dens_avg = mean(osg_rho, na.rm = TRUE),
            bpum_avg = mean(m_ballast_pumped, na.rm = TRUE),
            bpum_delta_avg = mean(bpump_delta, na.rm = TRUE),
            bpos_avg = mean(m_battpos, na.rm = TRUE),
            vac_avg = mean(m_vacuum, na.rm = TRUE),
            cast_avg = mean(cast_fact, na.rm = TRUE), #effectively percent time at surface in that hour
            date = mean(m_present_time),
            .groups = "keep") %>%
  #mutate(id = ifelse(hour(ddate) %in% c(3,7,11,15,19,23), "surf", "non")) %>%
  mutate(id = ifelse(is.nan(gps_avg), "non", "surf")) %>%
  mutate(id = as.factor(id))

full.model <- lm(ahr_rate_avg ~ dive_depth_avg + inf_depth_avg + bpos_avg + batt_avg
                   + dens_avg + bpum_delta_avg + cast_avg,
                   data = dailydf)

step.model<- step(full.model)   # n=16

step.model

AIC(step.model)

ggplot(data = dailydf,
       aes(x=bpum_delta_avg,
           y=dens_avg,
           color = id
       )) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  theme_bw() +
  labs(title = "Power Usage",
       y = "Ahrs/day",
       x = "Date")
