# Script for Calculation of Direct Beam (Clearsky-Model)
# Based on Naraghi 2010. A Demand Based Optimum Solar Panel Radiation. DOI: 10.1115/IMECE2010-37918


#############################################################################################################################
#### Used Packages ####
#############################################################################################################################
library(ggplot2)
library(plyr)

#############################################################################################################################
#### Selfmade Functions ####
#############################################################################################################################
# Change Sin/Cos-Functions from Rad to Deg
sindeg <- function(x){
  sin(x * pi / 180)
}

cosdeg <- function(x){
  cos(x * pi / 180)
}

asindeg <- function(x){
  asin(x) * 180 / pi
}

acosdeg <- function(x){
  acos(x) * 180 / pi
}

# Is number x divisible by another number?
is.divisible <- function(x, by){
  (x / by) == round(x / by)
}

# Calculation of Earth Declination Angle (Tilt of Earth during one Year)
EarthDecAngle <- function(x){
  23.45 * sin(2 * pi / 365 * (x + 284))
}

# Calculation of solar beam optical depths and solar diffuse optical depths
calctau <- function(x){
  # Values taken from: http://ashrae-meteo.info/v2.0/index.php?lat=40.97&lng=28.82&place=%27%27&wmo=170600&ashrae_version=2009
  # Values for Lindenberg, Germany
  taub <- c(0.359, 0.364, 0.424, 0.443, 0.451, 0.465, 0.487, 0.469, 0.400, 0.387, 0.361, 0.363)
  taud <- c(2.191, 2.142, 1.923, 1.915, 1.927, 1.932, 1.874, 1.936, 2.160, 2.146, 2.208, 2.206)
  list(taub = taub[as.numeric(format(as.Date(paste0("2000-", x), "%Y-%j"), "%m"))],
       taud = taud[as.numeric(format(as.Date(paste0("2000-", x), "%Y-%j"), "%m"))])
}

# Calculate the Equation of Time (SOURCE!)
EquationOfTime <- function(x){
  year_angle <- 360 * (x / 365)
  a <- 0.0066 + 7.3525 * cosdeg(year_angle + 85.9)
  b <- 9.9359 * cosdeg(2 * year_angle + 108.9)
  c <- 0.3387 * cosdeg(3 * year_angle + 105.2)
  a + b + c
}

GetFile <- function(FileName){
  paste0(sub(sub(".*\\/", "", rstudioapi::getSourceEditorContext()$path), "", rstudioapi::getSourceEditorContext()$path), FileName)
}




#############################################################################################################################
#### Now use input (Latitude, Hour and Day of Year) to calculate the clearsky radiation ####
#############################################################################################################################
# This is how we do it:
# 1. Calculate Sunrise and Sunset. -> Calculate only for the Hours between Sunrise and Sunset:
# 2. Calculate Extraterrestrial Solar Radiation.
# 3. Reduce Extraterrestrial Solar Radiation by Air Mass (Radiation is reduced by Particles in the Air)
lat <- 55   # Latitude
h <- 1:24   # Hour
doy <- 304  # Day of Year

# Calculate the daylength (half of it) after Amthor 1997
Daylength <- acos((sin(-0.0145439) - sindeg(lat) * sindeg(EarthDecAngle(doy))) /
                    (cosdeg(lat) * cosdeg(EarthDecAngle(doy)))) * 12 / pi

# Sunrise = 12 (solar noon) - Daylength - Elliptical Correction of Earth's Orbit
sunrise <- 12 - Daylength - EquationOfTime(doy) * pi / 180
sunset <- 12 + Daylength - EquationOfTime(doy) * pi / 180

# We only need to calculate Radiation between sunrise and sunset (Values are not correct for Hours outside of Sunrise and Sunset)
HoursWithDaylight <- h[h > sunrise & h < sunset]

# Earth Declination Angle (Eq. 3)
delta <- EarthDecAngle(doy)
hour_angle <- (12 - HoursWithDaylight) * 15

# Solar Altitude Angle (Beta, Eq. 1)
beta <- asindeg(cosdeg(lat) * cosdeg(hour_angle) * cosdeg(delta) + sindeg(lat) * sindeg(delta))

# Relative Air Mass (m, Eq. 5)
m <- 1 / (sindeg(beta) + 0.50572 * ((6.07995 + beta) ^ (-1.6364)))

# Extraterrestrial Solar Radiation (E0, Eq. 8)
E0 <- 1367 * (1 + 0.033 * cosdeg(360 * ((doy - 3) / 365)))

# Optical Depths:
rb <- calctau(doy)[[1]] # Solar Beam Optical Depth -> Values taken from Website (see Function)
rd <- calctau(doy)[[2]] # Diffuse Optical Depth
# Air Mass Exponents:
ab <- 1.219 - 0.043 * rb - 0.151 * rd - 0.204 * rb * rd  # can be found between Eq. 8 and Eq. 9
ad <- 0.202 + 0.852 * rb - 0.007 * rd - 0.357 * rb * rd

# Direct beam (Eq. 6)
Eb <- E0 * exp(-1 * rb * (m ^ ab))



# Next, we do this for all days and look at the Data.
sundata <- data.frame(DOY = 1, HOUR = 1, RAD = 1)[-1, ]
for(i in 1:365){
  h <- 1:24
  doy <- i

  # Calculate the daylength (half of it) after Amthor 1997
  Daylength <- acos((sin(-0.0145439) - sindeg(lat) * sindeg(EarthDecAngle(doy))) /
                      (cosdeg(lat) * cosdeg(EarthDecAngle(doy)))) * 12 / pi

  # Sunrise = 12 (solar noon) - Daylength - Elliptical Correction of Earth's Orbit
  sunrise <- 12 - Daylength - EquationOfTime(doy) * pi / 180
  sunset <- 12 + Daylength - EquationOfTime(doy) * pi / 180

  # We only need to calculate Radiation between sunrise and sunset (Values are not correct for Hours outside of Sunrise and Sunset)
  HoursWithDaylight <- h[h > sunrise & h < sunset]

  # Earth Declination Angle (Eq. 3)
  delta <- EarthDecAngle(doy)
  hour_angle <- (12 - HoursWithDaylight) * 15

  # Solar Altitude Angle (Beta, Eq. 1)
  beta <- asindeg(cosdeg(lat) * cosdeg(hour_angle) * cosdeg(delta) + sindeg(lat) * sindeg(delta))

  # Relative Air Mass (m, Eq. 5)
  m <- 1 / (sindeg(beta) + 0.50572 * ((6.07995 + beta) ^ (-1.6364)))

  # Extraterrestrial Solar Radiation (E0, Eq. 8)
  E0 <- 1367 * (1 + 0.033 * cosdeg(360 * ((doy - 3) / 365)))

  # Optical Depths:
  rb <- calctau(doy)[[1]] # Solar Beam Optical Depth -> Values taken from Website (see Function)
  rd <- calctau(doy)[[2]] # Diffuse Optical Depth
  # Air Mass Exponents:
  ab <- 1.219 - 0.043 * rb - 0.151 * rd - 0.204 * rb * rd  # can be found between Eq. 8 and Eq. 9
  ad <- 0.202 + 0.852 * rb - 0.007 * rd - 0.357 * rb * rd

  # Direct beam (Eq. 6)
  Eb <- E0 * exp(-1 * rb * (m ^ ab))

  sundata <- rbind(sundata, data.frame(DOY = i, HOUR = HoursWithDaylight, RAD = Eb))
}


p <- ggplot(sundata[is.divisible(sundata$DOY, by = 10), ])+
  geom_line(aes(x = HOUR, y = RAD, group = DOY, color = DOY), size = 0.5)+
  scale_color_gradient2(name = "Day of\nYear", high = "blue", low = "blue", mid = "red", midpoint = 365 / 2)+
  scale_x_continuous(name = "Hour")+
  scale_y_continuous(name = "Radiation [W / m²]", expand = c(0, 1))+
  theme_bw(); p


ggsave(p, file = GetFile("Plots/CalcRadiation/01_Radiation_Through_Year.png"), 
       height = 4, width = 6)



#############################################################################################################################
#### Calculation of proportion of Radiation for different minimum Angles ####
#############################################################################################################################
cuts <- c(0.05, 0.1, 0.2)

AngleData <- data.frame(DOY = 1, HOUR = 1, RAD = 1, BETA = 1)[-1, ]    # Create a new data frame with Radiation and Angle Data

# Iterate through all Hours and Angles
for(day in 1:365){
  h <- 1:24
  doy <- day
  
  # Calculate the daylength (half of it) after Amthor 1997
  Daylength <- acos((sin(-0.0145439) - sindeg(lat) * sindeg(EarthDecAngle(doy))) /
                      (cosdeg(lat) * cosdeg(EarthDecAngle(doy)))) * 12 / pi
  
  # Sunrise = 12 (solar noon) - Daylength - Elliptical Correction of Earth's Orbit
  sunrise <- 12 - Daylength - EquationOfTime(doy) * pi / 180
  sunset <- 12 + Daylength - EquationOfTime(doy) * pi / 180
  
  # We only need to calculate Radiation between sunrise and sunset (Values are not correct for Hours outside of Sunrise and Sunset)
  HoursWithDaylight <- h[h > sunrise & h < sunset]
  
  # Earth Declination Angle (Eq. 3)
  delta <- EarthDecAngle(doy)
  hour_angle <- (12 - HoursWithDaylight) * 15
  
  # Solar Altitude Angle (Beta, Eq. 1)
  beta <- asindeg(cosdeg(lat) * cosdeg(hour_angle) * cosdeg(delta) + sindeg(lat) * sindeg(delta))
  
  # Relative Air Mass (m, Eq. 5)
  m <- 1 / (sindeg(beta) + 0.50572 * ((6.07995 + beta) ^ (-1.6364)))
  
  # Extraterrestrial Solar Radiation (E0, Eq. 8)
  E0 <- 1367 * (1 + 0.033 * cosdeg(360 * ((doy - 3) / 365)))
  
  # Optical Depths:
  rb <- calctau(doy)[[1]] # Solar Beam Optical Depth -> Values taken from Website (see Function)
  rd <- calctau(doy)[[2]] # Diffuse Optical Depth
  # Air Mass Exponents:
  ab <- 1.219 - 0.043 * rb - 0.151 * rd - 0.204 * rb * rd  # can be found between Eq. 8 and Eq. 9
  ad <- 0.202 + 0.852 * rb - 0.007 * rd - 0.357 * rb * rd
  
  # Direct beam (Eq. 6)
  Eb <- E0 * exp(-1 * rb * (m ^ ab))
  
  AngleData <- rbind(AngleData, data.frame(DOY = day, HOUR = HoursWithDaylight, RAD = Eb, BETA = beta))
}

# Define Start and End Date of the Vegetation Period and only use the data inbetween
SeasonStart <- as.numeric(format(as.Date("2001-05-01"), "%j"))
SeasonEnd <- as.numeric(format(as.Date("2001-09-30"), "%j"))
AngleData <- AngleData[AngleData$DOY >= SeasonStart & AngleData$DOY <= SeasonEnd, ]

# Create a new data frame containing Information on how much Radiation of the Total Radiation (during Vegetation Period) is included using different 
# Minimum Angles to start the calculation
AngleCalc <- data.frame(Angle = 1, Prop = 1)[-1, ]
for(A in seq(0, 70, 0.05)){
  AngleCalc <- rbind(AngleCalc, data.frame(Angle = A, Prop = sum(AngleData$RAD[AngleData$BETA >= A]) / sum(AngleData$RAD)))
}



# Only for Visualization
MySegs <- data.frame(x = c(0, 0, 0,
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]), 
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])])),
                     xend = c(min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]), 
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]), 
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])])),
                     y = c((1 - cuts[1:3]) * 100, 0, 0, 0),
                     yend = c((1 - cuts[1:3]), (1 - cuts[1:3])) * 100,
                     color = c(1, 2, 3, 1, 2, 3))

# Plot The Proportional Radiation against the related Minimum Solar Altitude Angles
p <- ggplot(AngleCalc)+
  geom_hline(yintercept = seq(0, 100, 12.5), color = "grey80")+
  geom_vline(xintercept = seq(0, 60, 10), color = "grey80")+
  geom_segment(data = MySegs, aes(x = x, xend = xend, y = y, yend = yend, color = factor(color)), size = 1, linetype = "solid")+
  geom_line(aes(x = Angle, y = Prop * 100), size = 1)+
  scale_color_manual(values = c("#dca1a1", "#c25b5b", "#861010"), guide = "none")+
  scale_x_continuous(name = "Minimum Solar Altitude Angle [°] at which Shadows are calculated", expand = c(0, 0),
                     breaks = c(0, min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]), 
                                min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                                min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 20, 40, 60))+
  scale_y_continuous(name = "Proportion of Radiation (from total Radiation) [%]\nreceived using the Minimum Solar Altitude Angle", expand = c(0, 1),
                     breaks = c(0, 25, 50, 75, (1 - cuts[1:3]) * 100, 100))+
  theme_classic()+
  theme(axis.text = element_text(angle = 45, hjust = 1)); p

ggsave(p, file = GetFile("Plots/CalcRadiation/02_MinSolarAngle_PropRad.png"), 
       height = 4, width = 6)

# Plot which Hours of Radiation are excluded when using  the Minimum Solar Altitude Angles
p <- ggplot(AngleData[is.divisible(AngleData$DOY, by = 10) & AngleData$BETA > min(AngleCalc$Angle[AngleCalc$Prop < 0.9]), ])+
  geom_line(data = sundata[is.divisible(sundata$DOY, by = 10), ], aes(x = HOUR, y = RAD, group = DOY, color = DOY), size = 0.8, alpha = 0.1)+
  geom_line(aes(x = HOUR, y = RAD, group = DOY, color = DOY), size = 0.8)+
  scale_color_gradient2(name = "Day of\nYear", high = "blue", low = "blue", mid = "red", midpoint = 365 / 2)+
  scale_x_continuous(name = "Hour")+
  scale_y_continuous(name = "Radiation [W / m²]", expand = c(0, 1))+
  theme_bw(); p

ggsave(p, file = GetFile("Plots/CalcRadiation/03_UsedHours.png"), 
       height = 4, width = 6)


#############################################################################################################################
#### Maximum Shadow Length given these Minimum Angles ####
#############################################################################################################################
# Use Law of Sines to determine the Shadow-Length (a / sin(alpha) = b / sin(beta) = c / sin(gamma)) 
TreeHeight <- 30                                      # Use a maximum Tree Height of 30 m.
ShadowLength <- data.frame(Prop = 1, SL = 1)[-1, ]    # Create an empty data frame.
for(i in seq(0.005, 0.995, 0.005)){                   # Iterate Through all Proportions...
  MinAngle <- min(AngleCalc$Angle[AngleCalc$Prop < i])
  ShadowLength <- rbind(ShadowLength, data.frame(Prop = i,
                                                 SL = TreeHeight / sindeg(MinAngle) * sindeg(180 - 90 - MinAngle)))
}
ShadowLength$Prop <- round(ShadowLength$Prop, digits = 3) # Round here because somehow there is an error otherwise.

# This is for Visuals
MySegs <- data.frame(x =    c(0  , 0   , 0    , (1 - cuts[1:3])) * 100,
                     xend = c((1 - cuts[1:3]), (1 - cuts[1:3])) * 100,
                     y = c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])],
                           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])], 
                           0, 0, 0),
                     yend = c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])]),
                     color = c(1, 2, 3, 1, 2, 3))


p <- ggplot(ShadowLength)+
  geom_hline(yintercept = seq(0, 350, 100), color = "grey90")+
  geom_vline(xintercept = seq(0, 100, 25), color = "grey90")+
  geom_segment(data = MySegs, aes(x = x, xend = xend, y = y, yend = yend, color = factor(color)), size = 0.8, linetype = "dashed")+
  geom_line(aes(x = Prop * 100, y = SL), size = 1)+
  scale_color_manual(values = c("#dca1a1", "#c25b5b", "#861010"), guide = "none")+
  scale_x_continuous(name = "Proportion of used Radiation from total Radiation [%]", breaks = c(0, 25, 50, 75, (1 - cuts[1:3]) * 100, 100), expand = c(0, 0))+
  scale_y_continuous(name = "Shadow Length given a 30m Tree", 
                     breaks = round(c(0,  
                                ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])],
                                ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                                ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])], 200, 300), digits = 1), 
                     expand = c(0, 0))+
  theme_classic()+
  theme(axis.text = element_text(angle = 45, hjust = 1)); p

ggsave(p, file = GetFile("Plots/CalcRadiation/04_ShadowLength.png"), 
       height = 4, width = 6)


#############################################################################################################################
#### Calculate Maximum-Shadow-Length for 5 different Locations ####
#############################################################################################################################
cuts <- c(0.05, 0.1, 0.2)



#############################################################################################################################
#### Lindenburg ####
#############################################################################################################################
lat <- 52.217

calctau <- function(x){
  # Values taken from: http://ashrae-meteo.info/index.php?lat=52.217&lng=14.117&place=''&wmo=103930&si_ip=SI&ashrae_version=2017
  # Values for Lindenberg, Germany
  taub <- c(0.323, 0.350, 0.390, 0.412, 0.410, 0.407, 0.427, 0.421, 0.383, 0.373, 0.348, 0.317)
  taud <- c(2.314, 2.290, 2.199, 2.217, 2.267, 2.300, 2.254, 2.285, 2.367, 2.348, 2.352, 2.331)
  list(taub = taub[as.numeric(format(as.Date(paste0("2001-", x), "%Y-%j"), "%m"))],
       taud = taud[as.numeric(format(as.Date(paste0("2001-", x), "%Y-%j"), "%m"))])
}


AngleData <- data.frame(DOY = 1, HOUR = 1, RAD = 1, BETA = 1)[-1, ]    # Create a new data frame with Radiation and Angle Data
# Iterate through all Hours and Angles
for(day in 1:365){
  h <- 1:24
  doy <- day
  
  # Calculate the daylength (half of it) after Amthor 1997
  Daylength <- acos((sin(-0.0145439) - sindeg(lat) * sindeg(EarthDecAngle(doy))) /
                      (cosdeg(lat) * cosdeg(EarthDecAngle(doy)))) * 12 / pi
  
  # Sunrise = 12 (solar noon) - Daylength - Elliptical Correction of Earth's Orbit
  sunrise <- 12 - Daylength - EquationOfTime(doy) * pi / 180
  sunset <- 12 + Daylength - EquationOfTime(doy) * pi / 180
  
  # We only need to calculate Radiation between sunrise and sunset (Values are not correct for Hours outside of Sunrise and Sunset)
  HoursWithDaylight <- h[h > sunrise & h < sunset]
  
  # Earth Declination Angle (Eq. 3)
  delta <- EarthDecAngle(doy)
  hour_angle <- (12 - HoursWithDaylight) * 15
  
  # Solar Altitude Angle (Beta, Eq. 1)
  beta <- asindeg(cosdeg(lat) * cosdeg(hour_angle) * cosdeg(delta) + sindeg(lat) * sindeg(delta))
  
  # Relative Air Mass (m, Eq. 5)
  m <- 1 / (sindeg(beta) + 0.50572 * ((6.07995 + beta) ^ (-1.6364)))
  
  # Extraterrestrial Solar Radiation (E0, Eq. 8)
  E0 <- 1367 * (1 + 0.033 * cosdeg(360 * ((doy - 3) / 365)))
  
  # Optical Depths:
  rb <- calctau(doy)[[1]] # Solar Beam Optical Depth -> Values taken from Website (see Function)
  rd <- calctau(doy)[[2]] # Diffuse Optical Depth
  # Air Mass Exponents:
  ab <- 1.219 - 0.043 * rb - 0.151 * rd - 0.204 * rb * rd  # can be found between Eq. 8 and Eq. 9
  ad <- 0.202 + 0.852 * rb - 0.007 * rd - 0.357 * rb * rd
  
  # Direct beam (Eq. 6)
  Eb <- E0 * exp(-1 * rb * (m ^ ab))
  
  AngleData <- rbind(AngleData, data.frame(DOY = day, HOUR = HoursWithDaylight, RAD = Eb, BETA = beta))
}

# Define Start and End Date of the Vegetation Period and only use the data inbetween
SeasonStart <- as.numeric(format(as.Date("2001-05-01"), "%j"))
SeasonEnd <- as.numeric(format(as.Date("2001-09-30"), "%j"))
AngleData <- AngleData[AngleData$DOY >= SeasonStart & AngleData$DOY <= SeasonEnd, ]

# Create a new data frame containing Information on how much Radiation of the Total Radiation (during Vegetation Period) is included using different 
# Minimum Angles to start the calculation
AngleCalc <- data.frame(Angle = 1, Prop = 1)[-1, ]
for(A in seq(0, 70, 0.05)){
  AngleCalc <- rbind(AngleCalc, data.frame(Angle = A, Prop = sum(AngleData$RAD[AngleData$BETA >= A]) / sum(AngleData$RAD)))
}



# Only for Visualization
MySegs <- data.frame(x = c(0, 0, 0,
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])])),
                     xend = c(min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])])),
                     y = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), 0, 0, 0) * 100,
                     yend = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     color = c(1, 2, 3, 1, 2, 3))

# Plot The Proportional Radiation against the related Minimum Solar Altitude Angles
ggplot(AngleCalc)+
  geom_hline(yintercept = seq(0, 100, 12.5), color = "grey80")+
  geom_vline(xintercept = seq(0, 60, 10), color = "grey80")+
  geom_segment(data = MySegs, aes(x = x, xend = xend, y = y, yend = yend, color = factor(color)), size = 1, linetype = "solid")+
  geom_line(aes(x = Angle, y = Prop * 100), size = 1)+
  scale_color_manual(values = c("#dca1a1", "#c25b5b", "#861010"), guide = "none")+
  scale_x_continuous(name = "Minimum Solar Altitude Angle [°] at which Shadows are calculated", expand = c(0, 0),
                     breaks = c(0, min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                                min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                                min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]), 20, 40, 60))+
  scale_y_continuous(name = "Proportion of Radiation (from total Radiation) [%]\nreceived using the Minimum Solar Altitude Angle", expand = c(0, 1),
                     breaks = c(c(0, 25, 50, 75, 100), c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100))+
  theme_classic()


TreeHeight <- 30                                      # Use a maximum Tree Height of 30 m.
ShadowLength <- data.frame(Prop = 1, SL = 1)[-1, ]    # Create an empty data frame.
for(i in seq(0.005, 0.995, 0.005)){                   # Iterate Through all Proportions...
  MinAngle <- min(AngleCalc$Angle[AngleCalc$Prop < i])
  ShadowLength <- rbind(ShadowLength, data.frame(Prop = i,
                                                 SL = TreeHeight / sindeg(MinAngle) * sindeg(180 - 90 - MinAngle)))
}
ShadowLength$Prop <- round(ShadowLength$Prop, digits = 3) # Round here because somehow there is an error otherwise.

# This is for Visuals
MySegs <- data.frame(x =    c(0  , 0   , 0    , (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     xend = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     y = c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])], 
                           0, 0, 0),
                     yend = c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])]),
                     color = c(1, 2, 3, 1, 2, 3))


ggplot(ShadowLength)+
  geom_hline(yintercept = seq(0, 350, 100), color = "grey90")+
  geom_vline(xintercept = seq(0, 100, 25), color = "grey90")+
  geom_segment(data = MySegs, aes(x = x, xend = xend, y = y, yend = yend, color = factor(color)), size = 0.8, linetype = "dashed")+
  geom_line(aes(x = Prop * 100, y = SL), size = 1)+
  scale_color_manual(values = c("#dca1a1", "#c25b5b", "#861010"), guide = "none")+
  scale_x_continuous(name = "Proportion of used Radiation from total Radiation [%]", breaks = c(c(0, 25, 50, 75, 100), c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100), expand = c(0, 0))+
  scale_y_continuous(name = "Shadow Length given a 30m Tree", 
                     breaks = round(c(0,  
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])], 200, 300), digits = 1), 
                     expand = c(0, 0))+
  theme_classic()


Lat52 <- c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])])

AngleCalc_52 <- AngleCalc
AngleData_52 <- AngleData
ShadowLength_52 <- ShadowLength





#############################################################################################################################
#### Belize ####
#############################################################################################################################
lat <- 17.539

calctau <- function(x){
  # Values taken from: http://ashrae-meteo.info/index.php?lat=17.539&lng=-88.308&place=''&wmo=785830&si_ip=SI&ashrae_version=2017
  # Values for Lindenberg, Germany
  taub <- c(0.373, 0.383, 0.411, 0.477, 0.511, 0.472, 0.503, 0.470, 0.437, 0.411, 0.388, 0.386)
  taud <- c(2.555, 2.518, 2.421, 2.222, 2.143, 2.302, 2.176, 2.293, 2.392, 2.491, 2.531, 2.533)
  list(taub = taub[as.numeric(format(as.Date(paste0("2001-", x), "%Y-%j"), "%m"))],
       taud = taud[as.numeric(format(as.Date(paste0("2001-", x), "%Y-%j"), "%m"))])
}


AngleData <- data.frame(DOY = 1, HOUR = 1, RAD = 1, BETA = 1)[-1, ]    # Create a new data frame with Radiation and Angle Data
# Iterate through all Hours and Angles
for(day in 1:365){
  h <- 1:24
  doy <- day
  
  # Calculate the daylength (half of it) after Amthor 1997
  Daylength <- acos((sin(-0.0145439) - sindeg(lat) * sindeg(EarthDecAngle(doy))) /
                      (cosdeg(lat) * cosdeg(EarthDecAngle(doy)))) * 12 / pi
  
  # Sunrise = 12 (solar noon) - Daylength - Elliptical Correction of Earth's Orbit
  sunrise <- 12 - Daylength - EquationOfTime(doy) * pi / 180
  sunset <- 12 + Daylength - EquationOfTime(doy) * pi / 180
  
  # We only need to calculate Radiation between sunrise and sunset (Values are not correct for Hours outside of Sunrise and Sunset)
  HoursWithDaylight <- h[h > sunrise & h < sunset]
  
  # Earth Declination Angle (Eq. 3)
  delta <- EarthDecAngle(doy)
  hour_angle <- (12 - HoursWithDaylight) * 15
  
  # Solar Altitude Angle (Beta, Eq. 1)
  beta <- asindeg(cosdeg(lat) * cosdeg(hour_angle) * cosdeg(delta) + sindeg(lat) * sindeg(delta))
  
  # Relative Air Mass (m, Eq. 5)
  m <- 1 / (sindeg(beta) + 0.50572 * ((6.07995 + beta) ^ (-1.6364)))
  
  # Extraterrestrial Solar Radiation (E0, Eq. 8)
  E0 <- 1367 * (1 + 0.033 * cosdeg(360 * ((doy - 3) / 365)))
  
  # Optical Depths:
  rb <- calctau(doy)[[1]] # Solar Beam Optical Depth -> Values taken from Website (see Function)
  rd <- calctau(doy)[[2]] # Diffuse Optical Depth
  # Air Mass Exponents:
  ab <- 1.219 - 0.043 * rb - 0.151 * rd - 0.204 * rb * rd  # can be found between Eq. 8 and Eq. 9
  ad <- 0.202 + 0.852 * rb - 0.007 * rd - 0.357 * rb * rd
  
  # Direct beam (Eq. 6)
  Eb <- E0 * exp(-1 * rb * (m ^ ab))
  
  AngleData <- rbind(AngleData, data.frame(DOY = day, HOUR = HoursWithDaylight, RAD = Eb, BETA = beta))
}

# Define Start and End Date of the Vegetation Period and only use the data inbetween
SeasonStart <- as.numeric(format(as.Date("2001-05-01"), "%j"))
SeasonEnd <- as.numeric(format(as.Date("2001-09-30"), "%j"))
AngleData <- AngleData[AngleData$DOY >= SeasonStart & AngleData$DOY <= SeasonEnd, ]

# Create a new data frame containing Information on how much Radiation of the Total Radiation (during Vegetation Period) is included using different 
# Minimum Angles to start the calculation
AngleCalc <- data.frame(Angle = 1, Prop = 1)[-1, ]
for(A in seq(0, 70, 0.05)){
  AngleCalc <- rbind(AngleCalc, data.frame(Angle = A, Prop = sum(AngleData$RAD[AngleData$BETA >= A]) / sum(AngleData$RAD)))
}



# Only for Visualization
MySegs <- data.frame(x = c(0, 0, 0,
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])])),
                     xend = c(min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])])),
                     y = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), 0, 0, 0) * 100,
                     yend = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     color = c(1, 2, 3, 1, 2, 3))

# Plot The Proportional Radiation against the related Minimum Solar Altitude Angles
ggplot(AngleCalc)+
  geom_hline(yintercept = seq(0, 100, 12.5), color = "grey80")+
  geom_vline(xintercept = seq(0, 60, 10), color = "grey80")+
  geom_segment(data = MySegs, aes(x = x, xend = xend, y = y, yend = yend, color = factor(color)), size = 1, linetype = "solid")+
  geom_line(aes(x = Angle, y = Prop * 100), size = 1)+
  scale_color_manual(values = c("#dca1a1", "#c25b5b", "#861010"), guide = "none")+
  scale_x_continuous(name = "Minimum Solar Altitude Angle [°] at which Shadows are calculated", expand = c(0, 0),
                     breaks = c(0, min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                                min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                                min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]), 20, 40, 60))+
  scale_y_continuous(name = "Proportion of Radiation (from total Radiation) [%]\nreceived using the Minimum Solar Altitude Angle", expand = c(0, 1),
                     breaks = c(c(0, 25, 50, 75, 100), c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100))+
  theme_classic()


TreeHeight <- 30                                      # Use a maximum Tree Height of 30 m.
ShadowLength <- data.frame(Prop = 1, SL = 1)[-1, ]    # Create an empty data frame.
for(i in seq(0.005, 0.995, 0.005)){                   # Iterate Through all Proportions...
  MinAngle <- min(AngleCalc$Angle[AngleCalc$Prop < i])
  ShadowLength <- rbind(ShadowLength, data.frame(Prop = i,
                                                 SL = TreeHeight / sindeg(MinAngle) * sindeg(180 - 90 - MinAngle)))
}
ShadowLength$Prop <- round(ShadowLength$Prop, digits = 3) # Round here because somehow there is an error otherwise.

# This is for Visuals
MySegs <- data.frame(x =    c(0  , 0   , 0    , (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     xend = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     y = c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])], 
                           0, 0, 0),
                     yend = c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])]),
                     color = c(1, 2, 3, 1, 2, 3))


ggplot(ShadowLength)+
  geom_hline(yintercept = seq(0, 350, 100), color = "grey90")+
  geom_vline(xintercept = seq(0, 100, 25), color = "grey90")+
  geom_segment(data = MySegs, aes(x = x, xend = xend, y = y, yend = yend, color = factor(color)), size = 0.8, linetype = "dashed")+
  geom_line(aes(x = Prop * 100, y = SL), size = 1)+
  scale_color_manual(values = c("#dca1a1", "#c25b5b", "#861010"), guide = "none")+
  scale_x_continuous(name = "Proportion of used Radiation from total Radiation [%]", breaks = c(c(0, 25, 50, 75, 100), c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100), expand = c(0, 0))+
  scale_y_continuous(name = "Shadow Length given a 30m Tree", 
                     breaks = round(c(0,  
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])], 200, 300), digits = 1), 
                     expand = c(0, 0))+
  theme_classic()


Lat17 <- c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])])

AngleCalc_17 <- AngleCalc
AngleData_17 <- AngleData
ShadowLength_17 <- ShadowLength




#############################################################################################################################
#### USA Lake Charles ####
#############################################################################################################################
lat <- 30.125

calctau <- function(x){
  # Values taken from: http://ashrae-meteo.info/index.php?lat=30.125&lng=-93.228&place=''&wmo=722400&si_ip=SI&ashrae_version=2017
  # Values for Lindenberg, Germany
  taub <- c(0.342, 0.358, 0.376, 0.431, 0.466, 0.501, 0.501, 0.488, 0.458, 0.383, 0.367, 0.352)
  taud <- c(2.511, 2.476, 2.419, 2.263, 2.200, 2.139, 2.181, 2.218, 2.268, 2.462, 2.449, 2.484)
  list(taub = taub[as.numeric(format(as.Date(paste0("2001-", x), "%Y-%j"), "%m"))],
       taud = taud[as.numeric(format(as.Date(paste0("2001-", x), "%Y-%j"), "%m"))])
}

AngleData <- data.frame(DOY = 1, HOUR = 1, RAD = 1, BETA = 1)[-1, ]    # Create a new data frame with Radiation and Angle Data
# Iterate through all Hours and Angles
for(day in 1:365){
  h <- 1:24
  doy <- day
  
  # Calculate the daylength (half of it) after Amthor 1997
  Daylength <- acos((sin(-0.0145439) - sindeg(lat) * sindeg(EarthDecAngle(doy))) /
                      (cosdeg(lat) * cosdeg(EarthDecAngle(doy)))) * 12 / pi
  
  # Sunrise = 12 (solar noon) - Daylength - Elliptical Correction of Earth's Orbit
  sunrise <- 12 - Daylength - EquationOfTime(doy) * pi / 180
  sunset <- 12 + Daylength - EquationOfTime(doy) * pi / 180
  
  # We only need to calculate Radiation between sunrise and sunset (Values are not correct for Hours outside of Sunrise and Sunset)
  HoursWithDaylight <- h[h > sunrise & h < sunset]
  
  # Earth Declination Angle (Eq. 3)
  delta <- EarthDecAngle(doy)
  hour_angle <- (12 - HoursWithDaylight) * 15
  
  # Solar Altitude Angle (Beta, Eq. 1)
  beta <- asindeg(cosdeg(lat) * cosdeg(hour_angle) * cosdeg(delta) + sindeg(lat) * sindeg(delta))
  
  # Relative Air Mass (m, Eq. 5)
  m <- 1 / (sindeg(beta) + 0.50572 * ((6.07995 + beta) ^ (-1.6364)))
  
  # Extraterrestrial Solar Radiation (E0, Eq. 8)
  E0 <- 1367 * (1 + 0.033 * cosdeg(360 * ((doy - 3) / 365)))
  
  # Optical Depths:
  rb <- calctau(doy)[[1]] # Solar Beam Optical Depth -> Values taken from Website (see Function)
  rd <- calctau(doy)[[2]] # Diffuse Optical Depth
  # Air Mass Exponents:
  ab <- 1.219 - 0.043 * rb - 0.151 * rd - 0.204 * rb * rd  # can be found between Eq. 8 and Eq. 9
  ad <- 0.202 + 0.852 * rb - 0.007 * rd - 0.357 * rb * rd
  
  # Direct beam (Eq. 6)
  Eb <- E0 * exp(-1 * rb * (m ^ ab))
  
  AngleData <- rbind(AngleData, data.frame(DOY = day, HOUR = HoursWithDaylight, RAD = Eb, BETA = beta))
}

# Define Start and End Date of the Vegetation Period and only use the data inbetween
SeasonStart <- as.numeric(format(as.Date("2001-05-01"), "%j"))
SeasonEnd <- as.numeric(format(as.Date("2001-09-30"), "%j"))
AngleData <- AngleData[AngleData$DOY >= SeasonStart & AngleData$DOY <= SeasonEnd, ]

# Create a new data frame containing Information on how much Radiation of the Total Radiation (during Vegetation Period) is included using different 
# Minimum Angles to start the calculation
AngleCalc <- data.frame(Angle = 1, Prop = 1)[-1, ]
for(A in seq(0, 70, 0.05)){
  AngleCalc <- rbind(AngleCalc, data.frame(Angle = A, Prop = sum(AngleData$RAD[AngleData$BETA >= A]) / sum(AngleData$RAD)))
}



# Only for Visualization
MySegs <- data.frame(x = c(0, 0, 0,
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])])),
                     xend = c(min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])])),
                     y = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), 0, 0, 0) * 100,
                     yend = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     color = c(1, 2, 3, 1, 2, 3))

# Plot The Proportional Radiation against the related Minimum Solar Altitude Angles
ggplot(AngleCalc)+
  geom_hline(yintercept = seq(0, 100, 12.5), color = "grey80")+
  geom_vline(xintercept = seq(0, 60, 10), color = "grey80")+
  geom_segment(data = MySegs, aes(x = x, xend = xend, y = y, yend = yend, color = factor(color)), size = 1, linetype = "solid")+
  geom_line(aes(x = Angle, y = Prop * 100), size = 1)+
  scale_color_manual(values = c("#dca1a1", "#c25b5b", "#861010"), guide = "none")+
  scale_x_continuous(name = "Minimum Solar Altitude Angle [°] at which Shadows are calculated", expand = c(0, 0),
                     breaks = c(0, min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                                min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                                min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]), 20, 40, 60))+
  scale_y_continuous(name = "Proportion of Radiation (from total Radiation) [%]\nreceived using the Minimum Solar Altitude Angle", expand = c(0, 1),
                     breaks = c(c(0, 25, 50, 75, 100), c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100))+
  theme_classic()


TreeHeight <- 30                                      # Use a maximum Tree Height of 30 m.
ShadowLength <- data.frame(Prop = 1, SL = 1)[-1, ]    # Create an empty data frame.
for(i in seq(0.005, 0.995, 0.005)){                   # Iterate Through all Proportions...
  MinAngle <- min(AngleCalc$Angle[AngleCalc$Prop < i])
  ShadowLength <- rbind(ShadowLength, data.frame(Prop = i,
                                                 SL = TreeHeight / sindeg(MinAngle) * sindeg(180 - 90 - MinAngle)))
}
ShadowLength$Prop <- round(ShadowLength$Prop, digits = 3) # Round here because somehow there is an error otherwise.

# This is for Visuals
MySegs <- data.frame(x =    c(0  , 0   , 0    , (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     xend = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     y = c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])], 
                           0, 0, 0),
                     yend = c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])]),
                     color = c(1, 2, 3, 1, 2, 3))


ggplot(ShadowLength)+
  geom_hline(yintercept = seq(0, 350, 100), color = "grey90")+
  geom_vline(xintercept = seq(0, 100, 25), color = "grey90")+
  geom_segment(data = MySegs, aes(x = x, xend = xend, y = y, yend = yend, color = factor(color)), size = 0.8, linetype = "dashed")+
  geom_line(aes(x = Prop * 100, y = SL), size = 1)+
  scale_color_manual(values = c("#dca1a1", "#c25b5b", "#861010"), guide = "none")+
  scale_x_continuous(name = "Proportion of used Radiation from total Radiation [%]", breaks = c(c(0, 25, 50, 75, 100), c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100), expand = c(0, 0))+
  scale_y_continuous(name = "Shadow Length given a 30m Tree", 
                     breaks = round(c(0,  
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])], 200, 300), digits = 1), 
                     expand = c(0, 0))+
  theme_classic()


Lat30 <- c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])])

AngleCalc_30 <- AngleCalc
AngleData_30 <- AngleData
ShadowLength_30 <- ShadowLength






#############################################################################################################################
#### USA McMinnville Municipal ####
#############################################################################################################################
lat <- 45.195

calctau <- function(x){
  # Values taken from: http://ashrae-meteo.info/index.php?lat=45.195&lng=-123.134&place=''&wmo=726881&si_ip=SI&ashrae_version=2017
  # Values for Lindenberg, Germany
  taub <- c(0.322, 0.319, 0.331, 0.359, 0.370, 0.362, 0.352, 0.355, 0.345, 0.334, 0.327, 0.328)
  taud <- c(2.518, 2.535, 2.504, 2.392, 2.378, 2.411, 2.460, 2.467, 2.488, 2.527, 2.510, 2.463)
  list(taub = taub[as.numeric(format(as.Date(paste0("2001-", x), "%Y-%j"), "%m"))],
       taud = taud[as.numeric(format(as.Date(paste0("2001-", x), "%Y-%j"), "%m"))])
}



AngleData <- data.frame(DOY = 1, HOUR = 1, RAD = 1, BETA = 1)[-1, ]    # Create a new data frame with Radiation and Angle Data
# Iterate through all Hours and Angles
for(day in 1:365){
  h <- 1:24
  doy <- day
  
  # Calculate the daylength (half of it) after Amthor 1997
  Daylength <- acos((sin(-0.0145439) - sindeg(lat) * sindeg(EarthDecAngle(doy))) /
                      (cosdeg(lat) * cosdeg(EarthDecAngle(doy)))) * 12 / pi
  
  # Sunrise = 12 (solar noon) - Daylength - Elliptical Correction of Earth's Orbit
  sunrise <- 12 - Daylength - EquationOfTime(doy) * pi / 180
  sunset <- 12 + Daylength - EquationOfTime(doy) * pi / 180
  
  # We only need to calculate Radiation between sunrise and sunset (Values are not correct for Hours outside of Sunrise and Sunset)
  HoursWithDaylight <- h[h > sunrise & h < sunset]
  
  # Earth Declination Angle (Eq. 3)
  delta <- EarthDecAngle(doy)
  hour_angle <- (12 - HoursWithDaylight) * 15
  
  # Solar Altitude Angle (Beta, Eq. 1)
  beta <- asindeg(cosdeg(lat) * cosdeg(hour_angle) * cosdeg(delta) + sindeg(lat) * sindeg(delta))
  
  # Relative Air Mass (m, Eq. 5)
  m <- 1 / (sindeg(beta) + 0.50572 * ((6.07995 + beta) ^ (-1.6364)))
  
  # Extraterrestrial Solar Radiation (E0, Eq. 8)
  E0 <- 1367 * (1 + 0.033 * cosdeg(360 * ((doy - 3) / 365)))
  
  # Optical Depths:
  rb <- calctau(doy)[[1]] # Solar Beam Optical Depth -> Values taken from Website (see Function)
  rd <- calctau(doy)[[2]] # Diffuse Optical Depth
  # Air Mass Exponents:
  ab <- 1.219 - 0.043 * rb - 0.151 * rd - 0.204 * rb * rd  # can be found between Eq. 8 and Eq. 9
  ad <- 0.202 + 0.852 * rb - 0.007 * rd - 0.357 * rb * rd
  
  # Direct beam (Eq. 6)
  Eb <- E0 * exp(-1 * rb * (m ^ ab))
  
  AngleData <- rbind(AngleData, data.frame(DOY = day, HOUR = HoursWithDaylight, RAD = Eb, BETA = beta))
}

# Define Start and End Date of the Vegetation Period and only use the data inbetween
SeasonStart <- as.numeric(format(as.Date("2001-05-01"), "%j"))
SeasonEnd <- as.numeric(format(as.Date("2001-09-30"), "%j"))
AngleData <- AngleData[AngleData$DOY >= SeasonStart & AngleData$DOY <= SeasonEnd, ]

# Create a new data frame containing Information on how much Radiation of the Total Radiation (during Vegetation Period) is included using different 
# Minimum Angles to start the calculation
AngleCalc <- data.frame(Angle = 1, Prop = 1)[-1, ]
for(A in seq(0, 70, 0.05)){
  AngleCalc <- rbind(AngleCalc, data.frame(Angle = A, Prop = sum(AngleData$RAD[AngleData$BETA >= A]) / sum(AngleData$RAD)))
}



# Only for Visualization
MySegs <- data.frame(x = c(0, 0, 0,
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])])),
                     xend = c(min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])])),
                     y = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), 0, 0, 0) * 100,
                     yend = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     color = c(1, 2, 3, 1, 2, 3))

# Plot The Proportional Radiation against the related Minimum Solar Altitude Angles
ggplot(AngleCalc)+
  geom_hline(yintercept = seq(0, 100, 12.5), color = "grey80")+
  geom_vline(xintercept = seq(0, 60, 10), color = "grey80")+
  geom_segment(data = MySegs, aes(x = x, xend = xend, y = y, yend = yend, color = factor(color)), size = 1, linetype = "solid")+
  geom_line(aes(x = Angle, y = Prop * 100), size = 1)+
  scale_color_manual(values = c("#dca1a1", "#c25b5b", "#861010"), guide = "none")+
  scale_x_continuous(name = "Minimum Solar Altitude Angle [°] at which Shadows are calculated", expand = c(0, 0),
                     breaks = c(0, min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                                min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                                min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]), 20, 40, 60))+
  scale_y_continuous(name = "Proportion of Radiation (from total Radiation) [%]\nreceived using the Minimum Solar Altitude Angle", expand = c(0, 1),
                     breaks = c(c(0, 25, 50, 75, 100), c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100))+
  theme_classic()


TreeHeight <- 30                                      # Use a maximum Tree Height of 30 m.
ShadowLength <- data.frame(Prop = 1, SL = 1)[-1, ]    # Create an empty data frame.
for(i in seq(0.005, 0.995, 0.005)){                   # Iterate Through all Proportions...
  MinAngle <- min(AngleCalc$Angle[AngleCalc$Prop < i])
  ShadowLength <- rbind(ShadowLength, data.frame(Prop = i,
                                                 SL = TreeHeight / sindeg(MinAngle) * sindeg(180 - 90 - MinAngle)))
}
ShadowLength$Prop <- round(ShadowLength$Prop, digits = 3) # Round here because somehow there is an error otherwise.

# This is for Visuals
MySegs <- data.frame(x =    c(0  , 0   , 0    , (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     xend = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     y = c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])], 
                           0, 0, 0),
                     yend = c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])]),
                     color = c(1, 2, 3, 1, 2, 3))


ggplot(ShadowLength)+
  geom_hline(yintercept = seq(0, 350, 100), color = "grey90")+
  geom_vline(xintercept = seq(0, 100, 25), color = "grey90")+
  geom_segment(data = MySegs, aes(x = x, xend = xend, y = y, yend = yend, color = factor(color)), size = 0.8, linetype = "dashed")+
  geom_line(aes(x = Prop * 100, y = SL), size = 1)+
  scale_color_manual(values = c("#dca1a1", "#c25b5b", "#861010"), guide = "none")+
  scale_x_continuous(name = "Proportion of used Radiation from total Radiation [%]", breaks = c(c(0, 25, 50, 75, 100), c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100), expand = c(0, 0))+
  scale_y_continuous(name = "Shadow Length given a 30m Tree", 
                     breaks = round(c(0,  
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])], 200, 300), digits = 1), 
                     expand = c(0, 0))+
  theme_classic()


Lat45 <- c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])])

AngleCalc_45 <- AngleCalc
AngleData_45 <- AngleData
ShadowLength_45 <- ShadowLength



#############################################################################################################################
#### Canada Croker River ####
#############################################################################################################################
lat <- 69.280

calctau <- function(x){
  # Values taken from: http://ashrae-meteo.info/index.php?lat=45.195&lng=-123.134&place=''&wmo=726881&si_ip=SI&ashrae_version=2017
  # Values for Lindenberg, Germany
  taub <- c(0.158, 0.207, 0.246, 0.292, 0.332, 0.328, 0.355, 0.339, 0.303, 0.232, 0.155, 0.155)
  taud <- c(1.887, 2.049, 2.188, 2.127, 2.131, 2.357, 2.382, 2.480, 2.491, 2.170, 1.919, 1.919)
  list(taub = taub[as.numeric(format(as.Date(paste0("2001-", x), "%Y-%j"), "%m"))],
       taud = taud[as.numeric(format(as.Date(paste0("2001-", x), "%Y-%j"), "%m"))])
}



AngleData <- data.frame(DOY = 1, HOUR = 1, RAD = 1, BETA = 1)[-1, ]    # Create a new data frame with Radiation and Angle Data
# Iterate through all Hours and Angles
for(day in 1:365){
  h <- 1:24
  doy <- day
  
  # Calculate the daylength (half of it) after Amthor 1997
  if(((sin(-0.0145439) - sindeg(lat) * sindeg(EarthDecAngle(doy))) /
      (cosdeg(lat) * cosdeg(EarthDecAngle(doy)))) < -1){
    Daylength <- 12
  }else{
    Daylength <- acos((sin(-0.0145439) - sindeg(lat) * sindeg(EarthDecAngle(doy))) /
                        (cosdeg(lat) * cosdeg(EarthDecAngle(doy)))) * 12 / pi
  }
  
  
  # Sunrise = 12 (solar noon) - Daylength - Elliptical Correction of Earth's Orbit
  sunrise <- 12 - Daylength - EquationOfTime(doy) * pi / 180
  sunset <- 12 + Daylength - EquationOfTime(doy) * pi / 180
  
  # We only need to calculate Radiation between sunrise and sunset (Values are not correct for Hours outside of Sunrise and Sunset)
  HoursWithDaylight <- h[h > sunrise & h < sunset]
  
  # Earth Declination Angle (Eq. 3)
  delta <- EarthDecAngle(doy)
  hour_angle <- (12 - HoursWithDaylight) * 15
  
  # Solar Altitude Angle (Beta, Eq. 1)
  beta <- asindeg(cosdeg(lat) * cosdeg(hour_angle) * cosdeg(delta) + sindeg(lat) * sindeg(delta))
  
  # Relative Air Mass (m, Eq. 5)
  m <- 1 / (sindeg(beta) + 0.50572 * ((6.07995 + beta) ^ (-1.6364)))
  
  # Extraterrestrial Solar Radiation (E0, Eq. 8)
  E0 <- 1367 * (1 + 0.033 * cosdeg(360 * ((doy - 3) / 365)))
  
  # Optical Depths:
  rb <- calctau(doy)[[1]] # Solar Beam Optical Depth -> Values taken from Website (see Function)
  rd <- calctau(doy)[[2]] # Diffuse Optical Depth
  # Air Mass Exponents:
  ab <- 1.219 - 0.043 * rb - 0.151 * rd - 0.204 * rb * rd  # can be found between Eq. 8 and Eq. 9
  ad <- 0.202 + 0.852 * rb - 0.007 * rd - 0.357 * rb * rd
  
  # Direct beam (Eq. 6)
  Eb <- E0 * exp(-1 * rb * (m ^ ab))
  
  AngleData <- rbind(AngleData, data.frame(DOY = day, HOUR = HoursWithDaylight, RAD = Eb, BETA = beta))
}

# Define Start and End Date of the Vegetation Period and only use the data inbetween
SeasonStart <- as.numeric(format(as.Date("2001-05-01"), "%j"))
SeasonEnd <- as.numeric(format(as.Date("2001-09-30"), "%j"))
AngleData <- AngleData[AngleData$DOY >= SeasonStart & AngleData$DOY <= SeasonEnd, ]

# Create a new data frame containing Information on how much Radiation of the Total Radiation (during Vegetation Period) is included using different 
# Minimum Angles to start the calculation
AngleCalc <- data.frame(Angle = 1, Prop = 1)[-1, ]
for(A in seq(0, 70, 0.05)){
  AngleCalc <- rbind(AngleCalc, data.frame(Angle = A, Prop = sum(AngleData$RAD[AngleData$BETA >= A]) / sum(AngleData$RAD)))
}



# Only for Visualization
MySegs <- data.frame(x = c(0, 0, 0,
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                           min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])])),
                     xend = c(min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                              min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])])),
                     y = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), 0, 0, 0) * 100,
                     yend = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     color = c(1, 2, 3, 1, 2, 3))

# Plot The Proportional Radiation against the related Minimum Solar Altitude Angles
ggplot(AngleCalc)+
  geom_hline(yintercept = seq(0, 100, 12.5), color = "grey80")+
  geom_vline(xintercept = seq(0, 60, 10), color = "grey80")+
  geom_segment(data = MySegs, aes(x = x, xend = xend, y = y, yend = yend, color = factor(color)), size = 1, linetype = "solid")+
  geom_line(aes(x = Angle, y = Prop * 100), size = 1)+
  scale_color_manual(values = c("#dca1a1", "#c25b5b", "#861010"), guide = "none")+
  scale_x_continuous(name = "Minimum Solar Altitude Angle [°] at which Shadows are calculated", expand = c(0, 0),
                     breaks = c(0, min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[3])]), 
                                min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[2])]),
                                min(AngleCalc$Angle[AngleCalc$Prop < (1 - cuts[1])]), 20, 40, 60))+
  scale_y_continuous(name = "Proportion of Radiation (from total Radiation) [%]\nreceived using the Minimum Solar Altitude Angle", expand = c(0, 1),
                     breaks = c(c(0, 25, 50, 75, 100), c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100))+
  theme_classic()


TreeHeight <- 30                                      # Use a maximum Tree Height of 30 m.
ShadowLength <- data.frame(Prop = 1, SL = 1)[-1, ]    # Create an empty data frame.
for(i in seq(0.005, 0.995, 0.005)){                   # Iterate Through all Proportions...
  MinAngle <- min(AngleCalc$Angle[AngleCalc$Prop < i])
  ShadowLength <- rbind(ShadowLength, data.frame(Prop = i,
                                                 SL = TreeHeight / sindeg(MinAngle) * sindeg(180 - 90 - MinAngle)))
}
ShadowLength$Prop <- round(ShadowLength$Prop, digits = 3) # Round here because somehow there is an error otherwise.

# This is for Visuals
MySegs <- data.frame(x =    c(0  , 0   , 0    , (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     xend = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1]), (1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                     y = c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])], 
                           0, 0, 0),
                     yend = c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                              ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])]),
                     color = c(1, 2, 3, 1, 2, 3))


ggplot(ShadowLength)+
  geom_hline(yintercept = seq(0, 350, 100), color = "grey90")+
  geom_vline(xintercept = seq(0, 100, 25), color = "grey90")+
  geom_segment(data = MySegs, aes(x = x, xend = xend, y = y, yend = yend, color = factor(color)), size = 0.8, linetype = "dashed")+
  geom_line(aes(x = Prop * 100, y = SL), size = 1)+
  scale_color_manual(values = c("#dca1a1", "#c25b5b", "#861010"), guide = "none")+
  scale_x_continuous(name = "Proportion of used Radiation from total Radiation [%]", breaks = c(c(0, 25, 50, 75, 100), c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100), expand = c(0, 0))+
  scale_y_continuous(name = "Shadow Length given a 30m Tree", 
                     breaks = round(c(0,  
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
                                      ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])], 200, 300), digits = 1), 
                     expand = c(0, 0))+
  theme_classic()


Lat69 <- c(ShadowLength$SL[ShadowLength$Prop == (1 - cuts[3])],
           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[2])],
           ShadowLength$SL[ShadowLength$Prop == (1 - cuts[1])])

AngleCalc_69 <- AngleCalc
AngleData_69 <- AngleData
ShadowLength_69 <- ShadowLength

#############################################################################################################################
#### Combine Everything ####
#############################################################################################################################


AngleCalc <- rbind(data.frame(AngleCalc_17, "Lat" = 17),
                   data.frame(AngleCalc_30, "Lat" = 30),
                   data.frame(AngleCalc_45, "Lat" = 45),
                   data.frame(AngleCalc_52, "Lat" = 52),
                   data.frame(AngleCalc_69, "Lat" = 69))

AngleData <- rbind(data.frame(AngleData_17, "Lat" = 17),
                   data.frame(AngleData_30, "Lat" = 30),
                   data.frame(AngleData_45, "Lat" = 45),
                   data.frame(AngleData_52, "Lat" = 52),
                   data.frame(AngleData_69, "Lat" = 69))

ShadowLength <- rbind(data.frame(ShadowLength_17, "Lat" = 17),
                      data.frame(ShadowLength_30, "Lat" = 30),
                      data.frame(ShadowLength_45, "Lat" = 45),
                      data.frame(ShadowLength_52, "Lat" = 52),
                      data.frame(ShadowLength_69, "Lat" = 69))

p <- ggplot(ShadowLength)+
  geom_hline(yintercept = seq(0, 800, 100), color = "grey90")+
  geom_vline(xintercept = seq(0, 100, 25), color = "grey90")+
  geom_vline(xintercept = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100)+
  geom_line(aes(x = Prop * 100, y = SL, group = Lat, color = factor(Lat)), size = 1)+
  scale_x_continuous(name = "Proportion of used Radiation from total Radiation [%]", expand = c(0, 0), breaks = c(c(0, 25, 50, 75, 100), c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100))+
  scale_y_continuous(name = "Shadow Length given a 30m Tree", 
                     breaks = seq(0, 800, 100), 
                     expand = c(0, 0))+
  scale_color_discrete(name = "Latitude")+
  theme_classic(); p

ggsave(p, file = GetFile("Plots/CalcRadiation/05_Shadow_Lengths_diff_Latitudes.png"), 
       height = 4, width = 6)


MaximumShadowLengths <- data.frame(Percentage = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100,
                                   Latitude17 = Lat17, Latitude30 = Lat30, Latitude45 = Lat45, Latitude52 = Lat52, Latitude69 = Lat69); ceiling(MaximumShadowLengths)





ShadLeng <- function(x){
  sin((90 - x) * pi / 180) * 30 / sin(x * pi / 180)
}

ggplot()+
  geom_function(fun = ShadLeng, size = 0.8, n = 10001)+
  scale_x_continuous(name = "Solar Altitude Angle [°]", limits = c(0, 90), expand = c(0, 0))+
  scale_y_continuous(name = "Shadow Length [m] given a tree of 30m in height", limits = c(0, 1700), expand = c(0, 0))+
  theme_classic()




