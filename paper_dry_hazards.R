################################################################################
# PAPER: Heatwaves, drought and wildfires: exploring compound and cascading ####
# events of dry hazards at the pan-European scale. #############################
################################################################################

library("raster")
library("lubridate")
library("caliver")
library("RColorBrewer")

# Define reanalysis dates to use to calculate danger thresholds
reDates <- seq.Date(from = as.Date("1990-01-01"),
                    to = as.Date("2018-12-31"),
                    by = "day")
# JJA dates
jjaDates <- seq.Date(from = as.Date("2016-06-01"),
                     to = as.Date("2016-08-31"),
                     by = "day")

# Keep only MJJAS dates from reanalysis
mjjasDates <- reDates[which(between(x = lubridate::month(reDates),
                                    left = 5, right = 9))]

# Europe mask: map of the geographical extent and default resolution
# europe <- as(raster::extent(-13, 33, 35, 72), "SpatialPolygons")

# Thresholds: high = 90th percentile of climatological period

################################################################################
### PREPARE DAILY CLIMATOLOGIES OF HAZARD DATA AND BINARY MAPS #################
################################################################################

### TEMPERATURE ################################################################
# Load nc file and name layers
tmax <- raster::brick("data_preparation/datacube_2mtpp_max_19902018_era5.nc")
names(tmax) <- mjjasDates
tmin <- raster::brick("data_preparation/datacube_2mtpp_min_19902018_era5.nc")
names(tmin) <- mjjasDates

# Calculate climatological thresholds over JJA
t2max90 <- caliver::daily_clima(r = tmax, dates = jjaDates, probs = 0.90)
writeRaster(t2max90,
            filename = "data_preparation/clima_tmax90.nc",
            format = "CDF", overwrite = TRUE)

t2min90 <- caliver::daily_clima(r = tmin, dates = jjaDates, probs = 0.90)
writeRaster(t2min90,
            filename = "data_preparation/clima_tmin90.nc",
            format = "CDF", overwrite = TRUE)

# CLAUDIA DI NAPOLI TO ADD HERE HOW TO GENERATE BINARY FILES FOR TEMPERATURE!
# writeRaster(t2m_binary, filename = "heat.nc",
#             format = "CDF", overwrite = TRUE)
heat <- brick("heat.nc")

### SOIL MOISTURE ##############################################################

# SONY TO ADD HERE HOW TO GENERATE BINARY FILES FOR DROUGHT!

# Sony's file is divided in levels, here we merge them all in one brick.
drought <- stack()
for (mylevel in 1:29){
  print(mylevel)
  temp <- brick("data_preparation/drought_levels.nc", level = mylevel)
  drought <- stack(drought, temp)
}
drought <- mask(drought, heat[[1]])

# change the number of binary file to make it comparable to others
drought[drought == 1] <- 2

# compareRaster(heat[[1]], drought[[1]])
# writeRaster(drought, filename="drought.nc", format="CDF", overwrite=TRUE)

### FWI ########################################################################
# Crop reanalysis (based on ERA5) over bbox
system("cdo --silent sellonlatbox,-13,33,35,72 fwi.nc fwi_eu.nc")

# Load fwi and extract only MJJAS to calculate thresholds from climatology
x <- brick("data_preparation/fwi_eu.nc")
myyear <- as.numeric(substr(names(x), 2, 5))
mymonth <- as.numeric(substr(names(x), 7, 8))
idx <- which(between(myyear, 1990, 2018) & between(mymonth, 5, 9))
fwi <- x[[idx]]
fwi90 <- caliver::daily_clima(r = fwi, dates = jjaDates, probs = 0.90)
writeRaster(fwi90, filename = "data_preparation/clima_fwi90.nc",
            format = "CDF", overwrite = TRUE)

# Extract only JJA
idx <- which(between(as.numeric(substr(names(fwi), 7, 8)), 6, 8))
fwi <- fwi[[idx]]
fwi # check number of layers should be 2668!

fwi_binary <- stack()
for (myyear in 1:29){
  print(myyear)
  start_idx <- seq(1, 2668, 92)
  end_idx <- seq(92, 2668, 92)
  temp <- fwi[[start_idx[myyear]:end_idx[myyear]]] >= fwi90
  fwi_binary <- stack(fwi_binary, temp)
}
# compareRaster(fwi_binary[[1]], heat[[1]])
writeRaster(fwi_binary, filename = "data_preparation/fire_01.nc",
            format = "CDF", overwrite = TRUE)

fwi_binary <- mask(fwi_binary, heat[[1]])
fwi_binary[fwi_binary == 1] <- 4
writeRaster(fwi_binary, filename = "fire.nc", format = "CDF", overwrite = TRUE)

rm(list = ls())

################################################################################
### OVERLAP HAZARDS ############################################################
################################################################################

heat <- brick("heat.nc")
drought <- brick("drought.nc")
fire <- brick("fire.nc")

# Heat + Drought ###############################################################

hd <- heat + drought
hd[hd < 3] <- 0
hd[hd == 3] <- 1

# Yearly average probability of occurrence
hd_yearly_sum <- stack()
for (i in 1:29){
  print(i)
  start_idx <- seq(1, 2668, 92)[i]
  stop_idx <- seq(92, 2668, 92)[i]
  temp <- hd[[start_idx:stop_idx]]
  temp_sum <- calc(temp, sum) # yearly sum
  hd_yearly_sum <- stack(hd_yearly_sum, temp_sum)
}
hd_yearly_percentage <- hd_yearly_sum / 92 * 100
hd_yearly_percentiles <- calc(hd_yearly_percentage,
                              fun = function(x) {quantile(x,
                                                          probs = c(.50,
                                                                    .75,
                                                                    .90),
                                                          na.rm=TRUE)} )
# writeRaster(hd_yearly_percentiles, filename = "../figures/Figure3_hd.nc",
#             format = "CDF", overwrite = TRUE)

# Fire + Heat ##################################################################

fh <- fire + heat
fh[fh < 5] <- 0
fh[fh == 5] <- 1

# Yearly average probability of occurrence
fh_yearly_sum <- stack()
for (i in 1:29){
  print(i)
  start_idx <- seq(1, 2668, 92)[i]
  stop_idx <- seq(92, 2668, 92)[i]
  temp <- fh[[start_idx:stop_idx]]
  temp_sum <- calc(temp, sum) # yearly sum
  fh_yearly_sum <- stack(fh_yearly_sum, temp_sum)
}
fh_yearly_percentage <- fh_yearly_sum / 92 * 100
fh_yearly_percentiles <- calc(fh_yearly_percentage,
                              fun = function(x) {quantile(x,
                                                          probs = c(.50,
                                                                    .75,
                                                                    .90),
                                                          na.rm=TRUE)} )
# writeRaster(fh_yearly_percentiles, filename = "../figures/Figure3_fh.nc",
#             format = "CDF", overwrite = TRUE)

# Fire + Drought ###############################################################

fd <- fire + drought
fd[fd < 6] <- 0
fd[fd == 6] <- 1

# Yearly average probability of occurrence
fd_yearly_sum <- stack()
for (i in 1:29){
  print(i)
  start_idx <- seq(1, 2668, 92)[i]
  stop_idx <- seq(92, 2668, 92)[i]
  temp <- fd[[start_idx:stop_idx]]
  temp_sum <- calc(temp, sum) # yearly sum
  fd_yearly_sum <- stack(fd_yearly_sum, temp_sum)
}
fd_yearly_percentage <- fd_yearly_sum / 92 * 100
fd_yearly_percentiles <- calc(fd_yearly_percentage,
                              fun = function(x) {quantile(x,
                                                          probs = c(.50,
                                                                    .75,
                                                                    .90),
                                                          na.rm=TRUE)} )
# writeRaster(fd_yearly_percentiles, filename = "../figures/Figure3_fd.nc",
#             format = "CDF", overwrite = TRUE)

# Fire + Drought + Heat ########################################################
all_combos <- fire + drought + heat
writeRaster(all_combos, filename = "all_combos.nc", format = "CDF", overwrite = TRUE)
all <- all_combos

all[all < 7] <- 0
all[all == 7] <- 1

# Yearly average probability of occurrence
all_yearly_sum <- stack()
for (i in 1:29){
  print(i)
  start_idx <- seq(1, 2668, 92)[i]
  stop_idx <- seq(92, 2668, 92)[i]
  temp <- all[[start_idx:stop_idx]]
  temp_sum <- calc(temp, sum) # yearly sum
  all_yearly_sum <- stack(all_yearly_sum, temp_sum)
}
all_yearly_percentage <- all_yearly_sum / 92 * 100
all_yearly_percentiles <- calc(all_yearly_percentage,
                               fun = function(x) {quantile(x,
                                                           probs = c(.50,
                                                                     .75,
                                                                     .90),
                                                           na.rm=TRUE)} )
# writeRaster(all_yearly_percentiles, filename="../figures/Figure3_all.nc",
#             format = "CDF", overwrite = TRUE)

# 90th percentile plot - all the plots together
plot_yearly_percentiles <- stack(hd_yearly_percentiles[[3]],
                                 fh_yearly_percentiles[[3]],
                                 fd_yearly_percentiles[[3]],
                                 all_yearly_percentiles[[3]])
# writeRaster(plot_yearly_percentiles, filename = "../figures/Figure3.nc",
#             format = "CDF", overwrite = TRUE)

# rm(list = ls())

################################################################################
### FIGURE 2 ###################################################################
################################################################################

df_all_years <- data.frame(matrix(NA, nrow = 0, ncol = 4))
for (i in 1:29){ # i <- 1
  print(i)
  start_idx <- seq(1, 2668, 92)[i]
  stop_idx <- seq(92, 2668, 92)[i]
  temp <- all_combos[[start_idx:stop_idx]]
  df <- data.frame(table(temp[]))
  df$Year <- 1989 + i
  df$Perc <- df$Freq / sum(df$Freq) * 100
  df_all_years <- rbind(df_all_years, df)
}

library("dplyr")
library("ggplot2")

super_sum <- as.numeric(df_all_years %>% 
                          #filter(Var1 != 0) %>% 
                          summarise(sum = sum(Freq)))

df_all_years %>%
  group_by(Var1) %>%
  #filter(Var1 != 0) %>%
  summarise(sum = sum(Freq)) %>%
  mutate(perc = round(sum / super_sum * 100, 2)) %>%
  ggplot(aes(x = Var1, y = perc)) +
  geom_bar(stat = "identity", color = "gray", fill = "white") +
  geom_text(aes(label = perc), vjust = -0.5) +
  scale_x_discrete(breaks=0:7, labels=c("NO HAZARD", "H", "D", "DH", "F", "HF", "DF", "DHF")) +
  xlab("") + ylab("Percentage of total number of cells")

df_sum <- apply(X = df, MARGIN = 2, FUN = sum)
df_percentage <- round(df_sum / sum(df_sum) * 100, 0)

barplot(df_percentage, col = "lightgray", border = NA,
        ylab = "Percentage of occurrence")

rm(list = ls())

################################################################################
### FIGURE 3 ###################################################################
################################################################################

hd_yearly_percentiles <- brick("../figures/Figure3_hd.nc")
fh_yearly_percentiles <- brick("../figures/Figure3_fh.nc")
fd_yearly_percentiles <- brick("../figures/Figure3_fd.nc")
all_yearly_percentiles <- brick("../figures/Figure3_all.nc")
plot_yearly_percentiles <- brick("../figures/Figure3.nc")

# Define the number of colors you want
nb.cols <- 16
mypalette <- rev(colorRampPalette(brewer.pal(11, "Spectral"))(nb.cols))

spplot(hd_yearly_percentiles, col.regions = mypalette) # Figure 3a
spplot(fh_yearly_percentiles, col.regions = mypalette) # Figure 3b
spplot(fd_yearly_percentiles, col.regions = mypalette) # Figure 3c
spplot(all_yearly_percentiles, col.regions = mypalette) # Figure 3d

spplot(plot_yearly_percentiles, col.regions = mypalette,
       names.attr = c("DH", "HF", "DF", "DHF")) # Figure 3

################################################################################
### FIGURE 4-5-6 ###############################################################
################################################################################

all <- brick("all_combos.nc")

# Initialize counts and lengths=durations
template <- all[[1:29]]
template[] <- NA

# Initialise count and length
raster_count <- raster_length <- template
# Initialise frequency
frequency_heat <- frequency_drought <- frequency_fire <- template
frequency_dh <- frequency_hf <- frequency_df <- frequency_dhf <- template
# Initialise starting hazard
starting_heat <- starting_drought <- starting_fire <- template
starting_dh <- starting_hf <- starting_df <- starting_dhf <- template
# Initialise ending hazard
ending_heat <- ending_drought <- ending_fire <- template
ending_dh <- ending_hf <- ending_df <- ending_dhf <- template
for (j in 1:template@nrows){ # j = 75; k = 70
  for (k in 1:template@ncols){
    print(paste(j, k))
    for (i in 1:29){ # i <- 1
      # Yearly initialisation of counters
      count_temp <- length_temp <- 0
      frequency_all <- c()
      counter_start_heat <- counter_start_drought <- counter_start_fire <- 0
      counter_start_dh <- counter_start_hf <- counter_start_df <- counter_start_dhf <- 0
      counter_end_heat <- counter_end_drought <- counter_end_fire <- 0
      counter_end_dh <- counter_end_hf <- counter_end_df <- counter_end_dhf <- 0
      temp <- as.numeric(getValues(all[j, k, drop=FALSE]))
      start_idx <- seq(1, 2668, 92)[i]
      stop_idx <- seq(92, 2668, 92)[i]
      temp_vector <- temp[start_idx:stop_idx]
      # Condition A: event is between two zeros
      temp_split <- split(temp_vector, cumsum(temp_vector==0))
      for (w in 1:length(temp_split)){ # w <- 48
        temp_seq0 <- as.numeric(unlist(temp_split[w]))
        if (length(temp_seq0) > 1){
          temp_seq <- temp_seq0[temp_seq0 != 0]
          if (length(temp_seq) > 1){
            # The sequences should not contain all the same number
            # for single hazards (1, 2, 4)
            if (!(all(temp_seq == 1) | all(temp_seq == 2) | all(temp_seq == 4))) {
              count_temp <- count_temp + 1
              length_temp <- length_temp + length(temp_seq)
              frequency_all <- c(frequency_all, temp_seq)
              # Starting type
              if (temp_seq[1] == 1){counter_start_heat <- counter_start_heat + 1}
              if (temp_seq[1] == 2){counter_start_drought <- counter_start_drought + 1}
              if (temp_seq[1] == 3){counter_start_dh <- counter_start_dh + 1}
              if (temp_seq[1] == 4){counter_start_fire <- counter_start_fire + 1}
              if (temp_seq[1] == 5){counter_start_hf <- counter_start_hf + 1}
              if (temp_seq[1] == 6){counter_start_df <- counter_start_df + 1}
              if (temp_seq[1] == 7){counter_start_dhf <- counter_start_dhf + 1}
              # Ending type
              if (temp_seq[length(temp_seq)] == 1){counter_end_heat <- counter_end_heat + 1}
              if (temp_seq[length(temp_seq)] == 2){counter_end_drought <- counter_end_drought + 1}
              if (temp_seq[length(temp_seq)] == 3){counter_end_dh <- counter_end_dh + 1}
              if (temp_seq[length(temp_seq)] == 4){counter_end_fire <- counter_end_fire + 1}
              if (temp_seq[length(temp_seq)] == 5){counter_end_hf <- counter_end_hf + 1}
              if (temp_seq[length(temp_seq)] == 6){counter_end_df <- counter_end_df + 1}
              if (temp_seq[length(temp_seq)] == 7){counter_end_dhf <- counter_end_dhf + 1}
            }
          }
        }
      }
      frequency_table <- data.frame(table(factor(frequency_all, levels = 1:7)))
      # Populate stacks
      raster_count[[i]][j, k] <- count_temp
      raster_length[[i]][j, k] <- length_temp
      # Frequency raster
      frequency_heat[[i]][j, k] <- frequency_table$Freq[1]
      frequency_drought[[i]][j, k] <- frequency_table$Freq[2]
      frequency_dh[[i]][j, k] <- frequency_table$Freq[3]
      frequency_fire[[i]][j, k] <- frequency_table$Freq[4]
      frequency_hf[[i]][j, k] <- frequency_table$Freq[5]
      frequency_df[[i]][j, k] <- frequency_table$Freq[6]
      frequency_dhf[[i]][j, k] <- frequency_table$Freq[7]
      # Starting raster
      starting_heat[[i]][j, k] <- counter_start_heat
      starting_drought[[i]][j, k] <- counter_start_drought
      starting_dh[[i]][j, k] <- counter_start_dh
      starting_fire[[i]][j, k] <- counter_start_fire
      starting_hf[[i]][j, k] <- counter_start_hf
      starting_df[[i]][j, k] <- counter_start_df
      starting_dhf[[i]][j, k] <- counter_start_dhf
      # Ending raster
      ending_heat[[i]][j, k] <- counter_end_heat
      ending_drought[[i]][j, k] <- counter_end_drought
      ending_dh[[i]][j, k] <- counter_end_dh
      ending_fire[[i]][j, k] <- counter_end_fire
      ending_hf[[i]][j, k] <- counter_end_hf
      ending_df[[i]][j, k] <- counter_end_df
      ending_dhf[[i]][j, k] <- counter_end_dhf
    }
  }
}

writeRaster(raster_count, filename="raster_count.nc", format="CDF", overwrite=TRUE)
writeRaster(raster_length, filename="raster_length.nc", format="CDF", overwrite=TRUE)
writeRaster(frequency_heat, filename="frequency_heat.nc", format="CDF", overwrite=TRUE)
writeRaster(frequency_drought, filename="frequency_drought.nc", format="CDF", overwrite=TRUE)
writeRaster(frequency_dh, filename="frequency_dh.nc", format="CDF", overwrite=TRUE)
writeRaster(frequency_fire, filename="frequency_fire.nc", format="CDF", overwrite=TRUE)
writeRaster(frequency_hf, filename="frequency_hf.nc", format="CDF", overwrite=TRUE)
writeRaster(frequency_df, filename="frequency_df.nc", format="CDF", overwrite=TRUE)
writeRaster(frequency_dhf, filename="frequency_dhf.nc", format="CDF", overwrite=TRUE)
# Starting raster
writeRaster(starting_heat, filename="starting_heat.nc", format="CDF", overwrite=TRUE)
writeRaster(starting_drought, filename="starting_drought.nc", format="CDF", overwrite=TRUE)
writeRaster(starting_dh, filename="starting_dh.nc", format="CDF", overwrite=TRUE)
writeRaster(starting_fire, filename="starting_fire.nc", format="CDF", overwrite=TRUE)
writeRaster(starting_hf, filename="starting_hf.nc", format="CDF", overwrite=TRUE)
writeRaster(starting_df, filename="starting_df.nc", format="CDF", overwrite=TRUE)
writeRaster(starting_dhf, filename="starting_dhf.nc", format="CDF", overwrite=TRUE)
# Ending raster
writeRaster(ending_heat, filename="ending_heat.nc", format="CDF", overwrite=TRUE)
writeRaster(ending_drought, filename="ending_drought.nc", format="CDF", overwrite=TRUE)
writeRaster(ending_dh, filename="ending_dh.nc", format="CDF", overwrite=TRUE)
writeRaster(ending_fire, filename="ending_fire.nc", format="CDF", overwrite=TRUE)
writeRaster(ending_hf, filename="ending_hf.nc", format="CDF", overwrite=TRUE)
writeRaster(ending_df, filename="ending_df.nc", format="CDF", overwrite=TRUE)
writeRaster(ending_dhf, filename="ending_dhf.nc", format="CDF", overwrite=TRUE)
