# Script for Calculation of Direct Beam (Clearsky-Model)
# Based on Naraghi 2010. A Demand Based Optimum Solar Panel Radiation. DOI: 10.1115/IMECE2010-37918


#############################################################################################################################
#### Used Packages ####
#############################################################################################################################
library(ggplot2)
library(plyr)
library(ggforce)
library(cowplot)

#############################################################################################################################
#### Selfmade Functions ####
#############################################################################################################################
# Import Functions of Sun Position and Radiation
source("R_Scripts/Radiation_and_Angle_Functions.R")


#############################################################################################################################
#### Clear-Sky-Model ####
#############################################################################################################################
# Let's look at the Clear-Sky-Model. First, we pick a Latitude and then look at the incoming Radiation Energy throughout the Year
lat <- 55

sundata <- data.frame(DOY = 1, HOUR = 1, RAD = 1)[-1, ] # Create empty Data Frame.
for(i in 1:365){   # For each Day
  h <- 1:24        # And each Hour
  sundata <- rbind(sundata, data.frame(DOY = i, HOUR = h, RAD = DirectBeamLat(Hour = h, DOY = i, Latitude = lat))) # Calculate Radiation energy.
}

# If ye look at the Plot, we see that Radiation Energy changes throughout the Year.
p <- ggplot(sundata[is.divisible(sundata$DOY, by = 10), ])+
  geom_line(aes(x = HOUR, y = RAD, group = DOY, color = DOY), size = 0.5)+
  scale_color_gradient2(name = "Day of\nYear", high = "blue", low = "blue", mid = "red", midpoint = 365 / 2)+
  scale_x_continuous(name = "Hour")+
  scale_y_continuous(name = "Radiation [W / m²]", expand = c(0, 1))+
  theme_bw(); p



#############################################################################################################################
#### Calculation of proportion of Radiation for different minimum Angles ####
#############################################################################################################################
# In the Morning or Evening, if the Sun shines directly from the Horizon, Shadows are really long. 
# We can't simulate indefinitely long Shadows. We therefore need to cut these flat Angles, at which the Sun creates long Shadows.
# In the following, we used some Calculations to define this Minimum Angle, we use to limit Shadow-Calculation.

# We first wanted to know how much Radiation Energy we ignore at which "Minimum Angle".
cuts <- c(0.05, 0.1, 0.2)   # We define how much Radiation we ignore.

AngleData <- data.frame(DOY = 1, HOUR = 1, RAD = 1, BETA = 1)[-1, ]    # Create a new data frame with Radiation and Angle Data
# Iterate through all Hours and Angles
for(day in 1:365){
  h <- 1:24
  doy <- day
  AngleData <- rbind(AngleData, 
                     data.frame(DOY = day, 
                                HOUR = h, 
                                RAD = DirectBeamLat(Hour = h, DOY = day, Latitude = lat), 
                                BETA = SolarAltitude(Hour = h, DOY = day, Latitude = lat)))
}

# Define Start and End Date of the Vegetation Period and only use the data inbetween
SeasonStart <- as.numeric(format(as.Date("2001-05-01"), "%j"))
SeasonEnd <- as.numeric(format(as.Date("2001-09-30"), "%j"))
AngleData <- AngleData[AngleData$DOY >= SeasonStart & AngleData$DOY <= SeasonEnd, ]

# Create a new data frame containing Information on how much Radiation of the Total Radiation (during Vegetation Period) is included using different 
# Minimum Angles to start the calculation
AngleCalc <- data.frame(Angle = 1, Prop = 1)[-1, ]
for(A in seq(0, 70, 0.05)){
  AngleCalc <- rbind(AngleCalc, data.frame(Angle = A, Prop = sum(AngleData$RAD[AngleData$BETA >= A], na.rm = TRUE) / sum(AngleData$RAD, na.rm = TRUE)))
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



# Plot which Hours of Radiation are excluded when using  the Minimum Solar Altitude Angles
p <- ggplot(AngleData[is.divisible(AngleData$DOY, by = 10) & AngleData$BETA > min(AngleCalc$Angle[AngleCalc$Prop < 0.9]), ])+
  geom_line(data = sundata[is.divisible(sundata$DOY, by = 10), ], aes(x = HOUR, y = RAD, group = DOY, color = DOY), size = 0.8, alpha = 0.1)+
  geom_line(aes(x = HOUR, y = RAD, group = DOY, color = DOY), size = 0.8)+
  scale_color_gradient2(name = "Day of\nYear", high = "blue", low = "blue", mid = "red", midpoint = 365 / 2)+
  scale_x_continuous(name = "Hour")+
  scale_y_continuous(name = "Radiation [W / m²]", expand = c(0, 1))+
  theme_bw(); p



#############################################################################################################################
#### Maximum Shadow Length given these Minimum Angles ####
#############################################################################################################################
# So using a Minimum Angle cuts off some of the Radiation Energy. We want to know, how long Shadows are, if we assume these 
# Minimum Angles. We assume a maximum Tree Height of 30m.

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


#############################################################################################################################
#### Calculate Maximum-Shadow-Length for 5 different Locations ####
#############################################################################################################################
cuts <- c(0.05, 0.1, 0.2)
SeasonStart <- as.numeric(format(as.Date("2001-05-01"), "%j"))
SeasonEnd <- as.numeric(format(as.Date("2001-09-30"), "%j"))
TreeHeight <- 30

ShadowLengthLat <- data.frame(Prop = 1, SL = 1, Latitude = 1)[-1, ]
for(MyLat in c(17.539, 30.125, 45.18, 52.217, 69.28)){
  AngleData <- data.frame(DOY = 1, HOUR = 1, RAD = 1, BETA = 1)[-1, ]    # Create a new data frame with Radiation and Angle Data
  for(day in 1:365){
    h <- 1:24
    doy <- day
    AngleData <- rbind(AngleData, 
                       data.frame(DOY = day, 
                                  HOUR = h, 
                                  RAD = DirectBeamLat(Hour = h, DOY = day, Latitude = MyLat), 
                                  BETA = SolarAltitude(Hour = h, DOY = day, Latitude = MyLat)))
  }
  
  AngleData <- AngleData[AngleData$DOY >= SeasonStart & AngleData$DOY <= SeasonEnd, ]
  # Create a new data frame containing Information on how much Radiation of the Total Radiation (during Vegetation Period) is included using different 
  # Minimum Angles to start the calculation
  AngleCalc <- data.frame(Angle = 1, Prop = 1)[-1, ]
  for(A in seq(0, 90, 0.05)){
    AngleCalc <- rbind(AngleCalc, data.frame(Angle = A, Prop = sum(AngleData$RAD[AngleData$BETA >= A], na.rm = TRUE) / sum(AngleData$RAD, na.rm = TRUE)))
  }
  ShadowLength <- data.frame(Prop = 1, SL = 1)[-1, ]    # Create an empty data frame.
  for(i in seq(0.005, 0.995, 0.005)){                   # Iterate Through all Proportions...
    MinAngle <- min(AngleCalc$Angle[AngleCalc$Prop < i])
    ShadowLength <- rbind(ShadowLength, data.frame(Prop = i,
                                                   SL = TreeHeight / sindeg(MinAngle) * sindeg(180 - 90 - MinAngle)))
  }
  ShadowLength$Prop <- round(ShadowLength$Prop, digits = 3) # Round here because somehow there is an error otherwise.
  ShadowLengthLat <- rbind(ShadowLengthLat, cbind(ShadowLength, Latitude = MyLat))
}

p <- ggplot(ShadowLengthLat)+
  geom_hline(yintercept = seq(0, 800, 100), color = "grey90")+
  geom_vline(xintercept = seq(0, 100, 25), color = "grey90")+
  geom_vline(xintercept = c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100)+
  geom_line(aes(x = Prop * 100, y = SL, group = Latitude, color = factor(Latitude)), size = 1)+
  scale_x_continuous(name = "Proportion of used Radiation from total Radiation [%]", expand = c(0, 0), breaks = c(c(0, 25, 50, 75, 100), c((1 - cuts[3]), (1 - cuts[2]), (1 - cuts[1])) * 100))+
  scale_y_continuous(name = "Shadow Length given a 30m Tree", 
                     breaks = seq(0, 800, 100), 
                     expand = c(0, 0))+
  scale_color_discrete(name = "Latitude")+
  theme_classic(); p

# We see that the Proportion of Radiation that we exclude depends on Latitude. 
# So we can either define a fixed Minimum Angle and get Variability in the Proportion of excluded Radiation, or
# We define a fixed Proportion and get a Minimum Angle that depends on Latitude. 
# We went for Option Number 2, a fixed Proportion.

# First, get Radiation Data of all 5 Latitudes
RadData <- data.frame(Hour = 1, DOY = 1, Latitude = 1, SolarAltitude = 1, Radiation = 1)[-1, ]
for(lat in c(69.280, 52.217, 45.180, 30.125, 17.539)){
  for(doy in 121:273){
    RadData <- rbind(RadData, data.frame(Hour = 1:24, 
                                         DOY = doy, 
                                         Latitude = lat, 
                                         SolarAltitude = SolarAltitude(Hour = 1:24, DOY = doy, Latitude = lat),
                                         Radiation = DirectBeamLat(Hour = 1:24, DOY = doy, Latitude = lat)))
  }
}

# The Curve of Radiation widens so that at higher latitudes, Radiation Energy is spread throughout a longer time span
ggplot(RadData[RadData$DOY %in% seq(121, 273, 10), ])+
  geom_line(aes(x = Hour, y = Radiation, color = DOY, group = DOY))+
  facet_grid(. ~ Latitude)


# Plot the sum per year below the previous curves.
ggplot(ddply(RadData, ~ Hour + Latitude, summarise, TotRad = sum(Radiation, na.rm = T)))+
  geom_rect(aes(xmin = Hour - 0.5, xmax = Hour + 0.5, ymin = 0, ymax = TotRad), color =  "black")+
  geom_line(data = RadData[RadData$DOY %in% seq(121, 273, 10), ], aes(x = Hour, y = Radiation * 150, color = DOY, group = DOY))+
  facet_grid(. ~ Latitude)


# Now see how much Radiation Energy is cut, if we cut off at a certain Minimum Solar Altitude
RadData$SolarAltitude[is.na(RadData$SolarAltitude)] <- 0; RadData$Radiation[is.na(RadData$Radiation)] <- 0
RadData <- RadData[order(RadData$Latitude, RadData$SolarAltitude), ]

# Calculate the Cumulative Curve and see the Angles below which x Percent of Radiation is ignored.
RadCum <- data.frame(Latitude = 1, SolarAltitude = 1, Radiation = 1)[-1, ]
for(lat in c(69.280, 52.217, 45.180, 30.125, 17.539)){
  for(alt in seq(-0.2, 90, by = 0.1)){
    RadCum <- rbind(RadCum, data.frame(Latitude = lat, 
                                       SolarAltitude = alt, 
                                       Radiation = sum(RadData$Radiation[RadData$Latitude == lat & RadData$SolarAltitude <= alt], na.rm = T)))
  }
}
RadCum <- ddply(RadCum, ~ Latitude, transform, RadPerc = Radiation / max(Radiation, na.rm = T))  # Make if relaive for nicer visuals.

# To compare the Curves of all 5 Latitudes, it's best to view them in a Plot and highlight only the Curve of the Latitude you are watching.
# We therefore repeat the previous Data 5 Times. This is just for Visuals!
RadCum2 <- data.frame(Latitude = rep(c(17.539, 30.125, 45.180, 52.217, 69.280), each = nrow(RadCum)),
                      SolarAltitude = rep(RadCum$SolarAltitude, 5),
                      Radiation = rep(RadCum$Radiation, 5),
                      RadPerc = rep(RadCum$RadPerc, 5),
                      RealLat = rep(RadCum$Latitude, 5),
                      isReal = c(rep(17.539, nrow(RadCum) / 5), rep("unreal", nrow(RadCum) * 5 / 5),
                                 rep(30.125, nrow(RadCum) / 5), rep("unreal", nrow(RadCum) * 5 / 5),
                                 rep(45.180, nrow(RadCum) / 5), rep("unreal", nrow(RadCum) * 5 / 5),
                                 rep(52.217, nrow(RadCum) / 5), rep("unreal", nrow(RadCum) * 5 / 5),
                                 rep(69.280, nrow(RadCum) / 5)))


PercLines <- data.frame(Latitude = 1, Percentage = 1, p5 = 1, p10 = 1, p25 = 1, p50 = 1)[-1, ]
for(lat in c(69.280, 52.217, 45.180, 30.125, 17.539)){
  PercLines <- rbind(PercLines, data.frame(Latitude = lat, Percentage = c(5, 10, 20, 50),
                                           SolarAltitude = c(max(RadCum$SolarAltitude[RadCum$Latitude == lat & RadCum$RadPerc <= 0.05], na.rm = T),
                                                             max(RadCum$SolarAltitude[RadCum$Latitude == lat & RadCum$RadPerc <= 0.1], na.rm = T),
                                                             max(RadCum$SolarAltitude[RadCum$Latitude == lat & RadCum$RadPerc <= 0.2], na.rm = T),
                                                             max(RadCum$SolarAltitude[RadCum$Latitude == lat & RadCum$RadPerc <= 0.5], na.rm = T))))
}

LatColors <- c("17.539" = "#cc0000", "30.125" = "#e69138", "45.18" = "#6aa84f", "52.217" = "#45818e", "69.28" = "#3d85c6", "unreal" = "grey80")

p <- ggplot()+
  geom_line(data = RadCum2[RadCum2$isReal == "unreal", ], aes(x = SolarAltitude, y = RadPerc, group = paste0(isReal, RealLat), color = isReal))+
  geom_line(data = RadCum2[RadCum2$isReal != "unreal", ], aes(x = SolarAltitude, y = RadPerc, group = paste0(isReal, RealLat), color = isReal), size = 2)+
  geom_segment(data = PercLines, aes(x = SolarAltitude, xend = SolarAltitude, y = 0, yend = Percentage / 100), linetype = "dashed")+
  geom_segment(data = PercLines, aes(x = 0, xend = SolarAltitude, y = Percentage / 100, yend = Percentage / 100), linetype = "dashed")+
  scale_x_continuous(name = "Solar Altitude [°]", limits = c(0, 95), expand = c(0, 0), breaks = c(0, 30, 60, 90), labels = c("   0", 30, 60, "90    "))+
  scale_y_continuous(name = "Cumulative ratio of Radiation Energy\nbelow a given Solar Altitude [-]", breaks = c(0.05, 0.1, 0.2, 0.25, 0.5, 0.75, 1),
                     minor_breaks = c(seq(0.125, 0.875, by = 0.25)), limits = c(0, 1.01), expand = c(0, 0))+
  scale_color_manual(values = LatColors, guide = "none")+
  theme_bw()+
  # theme(axis.text.x = element_text(angle = 1:4))+
  facet_grid(. ~ Latitude, labeller = labeller(Latitude = c("17.539" = "Latitude 17", 
                                                            "30.125" = "Latitude 30", 
                                                            "45.18" = "Latitude 45", 
                                                            "52.217" = "Latitude 52",
                                                            "69.28" = "Latitude 69"))); p


# It's easier to read a number. So we made "Pie Charts", that show the Minimum Angle as Arcs.
Arcs <- PercLines
Arcs$x0 <- rep(5:1, each = 4); Arcs$y0 <- 0; Arcs$r0 <- 0; Arcs$r <- rep(seq(0.8, 0.4, length.out = 4), 5); Arcs$end <- 90 * pi / 180
Arcs$start <- (90 - Arcs$SolarAltitude) * pi / 180
Arcs <- ddply(Arcs, ~ Latitude, transform, Diff = c(0, SolarAltitude[1:3]) + diff(c(0, SolarAltitude)) / 2)


ArcLabels <- data.frame(Latitude = rep(rev(c(69.280, 52.217, 45.180, 30.125, 17.539)), each = 4),
                        Percentage = rep(c(5, 10, 20, 50), 5),
                        SolarAltitude = Arcs$SolarAltitude,
                        x = cos(Arcs$Diff * pi / 180) * (rep(seq(0.8, 0.4, length.out = 4), 5) + 0.03),
                        y = sin(Arcs$Diff * pi / 180) * (rep(seq(0.8, 0.4, length.out = 4), 5) + 0.03),
                        label = rep(c("5 %", "10 %", "20 %", "50 %"), 5))


p2 <- ggplot()+
  geom_arc_bar(data = Arcs[Arcs$Percentage == 50, ], aes(x0 = 0, y0 = y0, r0 = r0, r = r, start = start, end = end, fill = factor(Latitude)))+
  geom_arc_bar(data = Arcs[Arcs$Percentage == 20, ], aes(x0 = 0, y0 = y0, r0 = r0, r = r, start = start, end = end, fill = factor(Latitude)))+
  geom_arc_bar(data = Arcs[Arcs$Percentage == 10, ], aes(x0 = 0, y0 = y0, r0 = r0, r = r, start = start, end = end, fill = factor(Latitude)))+
  geom_arc_bar(data = Arcs[Arcs$Percentage ==  5, ], aes(x0 = 0, y0 = y0, r0 = r0, r = r, start = start, end = end, fill = factor(Latitude)))+
  geom_text(data = ArcLabels, aes(x = x, y = y, label = round(SolarAltitude, digits = 1), angle = SolarAltitude), hjust = 0)+
  scale_fill_manual(values = LatColors, guide = "none")+
  scale_x_continuous(limits = c(0, 1))+
  scale_y_continuous(limits = c(0, 0.6), name = "Solar Altitude [-]")+
  coord_fixed()+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  facet_grid(. ~ Latitude, switch = "x",
             labeller = labeller(Latitude = c("17.539" = "Latitude 17", 
                                              "30.125" = "Latitude 30", 
                                              "45.18" = "Latitude 45", 
                                              "52.217" = "Latitude 52",
                                              "69.28" = "Latitude 69"))); p2


p_all <- plot_grid(p, p2, align = "v", ncol = 1, rel_heights = c(0.7, 0.3)); p_all

# What you see here is the the Curve for each Latitude that shows the Solar Altitude on the X-Axis and the Proportion of Radiation we would lose, if we would cut at the respective
# Angle. Down below, the Angles of 5, 10, 20 and 50 Percent of Loss are visualised. We decided to cut at 5% Loss.
