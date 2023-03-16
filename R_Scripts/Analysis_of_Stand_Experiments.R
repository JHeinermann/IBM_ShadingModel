#################################################################################################################
#### Used Packages ####
#################################################################################################################
library(ggplot2)
library(ggforce)
library(cowplot)
library(ggpubr)
library(spatstat) 
library(viridis)
library(plyr)
library(lemon)
library(gtable)
library(grid)
library(MASS)

#################################################################################################################
#### Decide if you want to recalculate everything ####
#################################################################################################################
# Some calculations in this script are really time consuming. You can do these calculations again or decide 
# to load data from previous calculations.
CalcAll <- FALSE

#################################################################################################################
#### Selfmade Functions ####
#################################################################################################################
between <- function(x, Val1, Val2){
  if(Val1 > Val2){
    Val3 <- Val2
    Val2 <- Val1
    Val1 <- Val3
  }
  x > Val1 & x <= Val2
}

#################################################################################################################
#### Functions from Websites  ####
#################################################################################################################
# https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
shift_legend2 <- function(p) {
  # ...
  # to grob
  gp <- ggplotGrob(p)
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  
  # now we just need a simple call to reposition the legend
  reposition_legend(p, 'center', panel=names)
}

#################################################################################################################
#### MetaData  ####
#################################################################################################################
# Data Frame containing all conducted Experiments.
Experiments <- expand.grid(CrownShape = c("Disk", "Ellipsoid"), 
                           WeatherStation = c("Phillip SW Belize", "Lake Charles", "McMinnville", "Lindenberg", "Croker river"),
                           CrownTransmissibility = c(20, 15, 10, 5, 0) / 100,
                           ForestGapSize = c(5, 10, 15, 20, 25),
                           PlotID = 1:10)

Experiments2 <- expand.grid(CrownShape = c("Disk", "Ellipsoid"), 
                            WeatherStation = c("Phillip SW Belize", "Lake Charles", "McMinnville", "Lindenberg", "Croker river"),
                            CrownTransmissibility = c(20, 15, 10, 5, 0) / 100,
                            ForestGapSize = c(5, 10, 15, 20, 25))

# Maximum Sun Energy at each Plot:
MaxSun <- data.frame(WeatherStation = c("Phillip SW Belize", "Lake Charles", "McMinnville", "Lindenberg", "Croker river"),
                     MaxRad = c(1225497.8173469815, 1244507.4176427615, 1530820.8266382422, 1412842.3938802604, 1713089.8263555742))

# Facet Labels:
FLabels <- c("Phillip SW Belize" = "Latitude 17.539", "Lake Charles" = "Latitude 30.125", "McMinnville" = "Latitude 45.18", "Lindenberg" = "Latitude 52.217", "Croker river" = "Latitude 69.28",
             "5" = "Forest Gap Size: 5m", "10" = "Forest Gap Size: 10m", "15" = "Forest Gap Size: 15m", "20" = "Forest Gap Size: 20m", "25" = "Forest Gap Size: 25m")

# Latitudes of Stations:
StationLats <- data.frame(WeatherStation = c("Phillip SW Belize", "Lake Charles", "McMinnville", "Lindenberg", "Croker river"),
                          Latitude = c(17.539, 30.125, 45.18, 52.217, 69.28))

NNumbers <- c("Phillip SW Belize" = 17.539, "Lake Charles" = 30.125, "McMinnville" = 45.18, "Lindenberg" = 52.217, "Croker river" = 69.28, "Disk" = 1, "Ellipsoid" = 2)

FLabels2 <- c("WeatherStation" = "Latitude\n", "CrownShape" = "Crown Shape\n", "CrownTransmissibility" = "Crown\nTransmissibility", "ForestGapSize" = "Gap Size\n", 
             "TotalRad" = "Total\nRadiation", "TotalFull" = "Total\nSunhours", "WeightedRad" = "Centroid\nRadiation", "WeightedFull" = "Centroid\nSunhours", 
             "LightProp" = "most\nShaded", "FullSProp" = "most\nSunhours", "GapSpecies_low" = "intolerant\nArea",
             "GapSpecies_intermediate" = "intermediate\nArea", "GapSpecies_high" = "tolerant\nArea", "LRC_low" = "LRC\nlow", "LRC_medium" = "LRC\nmedium", "LRC_high" = "LRC\nhigh")
FLevels <- c("TotalRad", "WeightedRad", "LightProp", "TotalFull", "WeightedFull", "FullSProp", "LRC_low", "LRC_medium", "LRC_high")

FLabels3 <- c("WeatherStation" = "Latitude\n", "CrownShape" = "Crown Shape\n", "CrownTransmissibility" = "Crown\nTransmissibility", "ForestGapSize" = "Gap Size\n", 
             "TotalRad" = "Total\nRadiation", "TotalFull" = "Total\nSunhours", "WeightedRad" = "Centroid\nRadiation", "WeightedFull" = "Centroid\nSunhours", 
             "LightProp" = "most\nShaded", "FullSProp" = "most\nSunhours", "GapSpecies_low" = "intolerant\nArea",
             "GapSpecies_intermediate" = "intermediate\nArea", "GapSpecies_high" = "tolerant\nArea")

FLabels4 <- c("Phillip SW Belize" = "Latitude: 17.539", "Lake Charles" = "Latitude: 30.125", 
              "McMinnville" = "Latitude: 45.18", "Lindenberg" = "Latitude: 52.217", 
              "Croker river" = "Latitude: 69.28",
              "0" = "Crown Transmissibility: 0 %", "0.05" = "Crown Transmissibility: 5 %", 
              "0.1" = "Crown Transmissibility: 10 %", "0.15" = "Crown Transmissibility: 15 %", 
              "0.2" = "Crown Transmissibility: 20 %",
              "5" = "Forest Gap Size: 5m", "10" = "Forest Gap Size: 10m", "15" = "Forest Gap Size: 15m", 
              "20" = "Forest Gap Size: 20m", "25" = "Forest Gap Size: 25m",
              "Disk" = "Crown Shape: Disk", "Ellipsoid" = "Crown Shape: Ellipsoid")

SpecOption <- LETTERS[7]
SpecColors <- c("tolerant" = viridis(10, option = SpecOption)[1],
                "intermediate" = viridis(10, option = SpecOption)[5],
                "intolerant" = viridis(10, option = SpecOption)[8])


# Calculate Smoothing Polygon (needed for Edge Correction)
n <- 1
PolyX <- c()
PolyY <- c()
for(i in c(-50:50, 50:-50)){
  PolyX[n] <- i
  PolyY[n] <- sin((90 - (asin(i / 50) * 180 / pi)) * pi / 180) * 50.01 * ifelse(n <= 101, -1, 1)
  n <- n + 1
}

#################################################################################################################
#### Get Data  ####
#################################################################################################################
# Every Gridcell of every Simulation Run has been saved. We load all Simulation Runs individually 
# and calculate multiple Variables to compare Input Parameters. 
# The Data Frames of these Variables can be calculated again or just loaded into R.
# The Calculations may take a While.

#################################################################################################################
#### Read Data from Outside  ####
#################################################################################################################
if(CalcAll == FALSE){
  # Weighted Centroid of Radiation Energy inside the Observed Area (Area within 50m Radius of Gap Midpoint).
  Weighted_Rad <- read.table(file = "Data/Results_SA_StandModel/Weighted_Rad.csv", header = TRUE)
  # Weighted Centroid of Hours with Full Sunlight inside the Observed Area.
  Weighted_Full <- read.table(file = "Data/Results_SA_StandModel/Weighted_Full.csv", header = TRUE)
  # Summed up Total Radiation inside Observed Area.
  TotalRad <- read.table(file = "Data/Results_SA_StandModel/TotalRad.csv", header = TRUE)
  # Summed up Total Number of Hours with Full Sunlight inside Observed Area.
  TotalFull <- read.table(file = "Data/Results_SA_StandModel/TotalFull.csv", header = TRUE)
  # Number of Patches which contain 0, 1, 2, ... 100 % of Shading. 
  LightProp <- read.table(file = "Data/Results_SA_StandModel/LightProp.csv", header = TRUE)
  # Number of Patches which contain 0, 1, 2, ... 100 % of Hours with Full Sunlight. 
  FullSProp <- read.table(file = "Data/Results_SA_StandModel/FullSProp.csv", header = TRUE)
  # North-South-Line of Radiation inside the Observed Area.
  GapInter <- read.table(file = "Data/Results_SA_StandModel/GapInter.csv", header = TRUE)
  # Species Distribution inside Observed Area.
  GapSpecies <- read.table(file = "Data/Results_SA_StandModel/GapSpecies.csv", header = TRUE)
}


#################################################################################################################
#### Calculate Data by Yourself  ####
#################################################################################################################
if(CalcAll == TRUE){
  # Prepare empty Data Frames.
  Weighted_Rad <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
                             ForestGapSize = 1, PlotID = 1, CentroidX = 1, CentroidY = 1)[-1, ]
  Weighted_Full <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
                              ForestGapSize = 1, PlotID = 1, CentroidX = 1, CentroidY = 1)[-1, ]
  TotalRad <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
                         ForestGapSize = 1, PlotID = 1, TotalRad = 1)[-1, ]
  TotalFull <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
                          ForestGapSize = 1, PlotID = 1, TotalFull = 1)[-1, ]
  LightProp <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
                          ForestGapSize = 1, PlotID = 1, Proportion = 1, Area = 1)[-1, ]
  FullSProp <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
                          ForestGapSize = 1, PlotID = 1, Proportion = 1, Area = 1)[-1, ]
  GapInter <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
                         ForestGapSize = 1, PlotID = 1, yCoord = 1, Rad = 1)[-1, ]
  GapSpecies <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
                           ForestGapSize = 1, PlotID = 1, ShadeTolerance = "A", Proportion = 1)[-1, ]
  
  nEx <- 2500
  # For Loop for Analysis.
  for(Ex in 1:nEx){
    if(Ex == 1){
      MyTimes <- c()
      MyTimes <- c(MyTimes, Sys.time())
    }
    # Define all Experiment Variables.
    MyEx <- Experiments[Ex, ]
    CS <- MyEx$CrownShape
    WS <- MyEx$WeatherStation
    CT <- MyEx$CrownTransmissibility
    GS <- MyEx$ForestGapSize
    ID <- MyEx$PlotID
    MySun <- MaxSun$MaxRad[MaxSun$WeatherStation == WS]
    
    # Load Data.
    FileName <- paste0("StandModel/Experiment_", Ex, "_", MyEx$CrownShape, "_", MyEx$WeatherStation, "_", 
                       MyEx$CrownTransmissibility, "_", MyEx$ForestGapSize, "_", MyEx$PlotID, ".csv")
    MyData <- read.csv(GetFile(FileName))
    colnames(MyData) <- c("x", "y", "Rad", "FullL")
    
    # Start of Analysis:
    
    # Weigthed Centroid of Radiation.
    midx <- sum(MyData$x * MyData$Rad, na.rm = T) / sum(MyData$Rad, na.rm = T)
    midy <- sum(MyData$y * MyData$Rad, na.rm = T) / sum(MyData$Rad, na.rm = T)
    Weighted_Rad <- rbind(Weighted_Rad, data.frame(ExNum = Ex, CrownShape = CS, WeatherStation = WS, CrownTransmissibility = CT,
                                                   ForestGapSize = GS, PlotID = ID, CentroidX = midx, CentroidY = midy))
    
    # Weigthed Centroid of Hours with Full Sunlight.
    midx <- sum(MyData$x * MyData$FullL, na.rm = T) / sum(MyData$FullL, na.rm = T)
    midy <- sum(MyData$y * MyData$FullL, na.rm = T) / sum(MyData$FullL, na.rm = T)
    Weighted_Full <- rbind(Weighted_Full, data.frame(ExNum = Ex, CrownShape = CS, WeatherStation = WS, CrownTransmissibility = CT,
                                                     ForestGapSize = GS, PlotID = ID, CentroidX = midx, CentroidY = midy))
    
    # Total Radiation:
    TotalRad <- rbind(TotalRad, data.frame(ExNum = Ex, CrownShape = CS, WeatherStation = WS, CrownTransmissibility = CT,
                                           ForestGapSize = GS, PlotID = ID, TotalRad = sum(MyData$Rad, na.rm = TRUE)))
    
    # Total Hours with Full Sunlight
    TotalFull <- rbind(TotalFull, data.frame(ExNum = Ex, CrownShape = CS, WeatherStation = WS, CrownTransmissibility = CT,
                                             ForestGapSize = GS, PlotID = ID, TotalFull = sum(MyData$FullL, na.rm = TRUE)))
    
    # Area where x % of Radiation is located:
    MyData <- MyData[order(-MyData$Rad), ]
    MyData$Prop <- cumsum(MyData$Rad / sum(MyData$Rad, na.rm = TRUE))
    Props <- c()
    for(MyProp in 1:10/10){
      Props <- c(Props, sum(MyData$Prop <= MyProp))
    }
    LightProp <- rbind(LightProp, data.frame(ExNum = Ex, CrownShape = CS, WeatherStation = WS, CrownTransmissibility = CT,
                                             ForestGapSize = GS, PlotID = ID, Proportion = 1:10/10, Area = Props))
    
    # Area where x % of Full Sunlight is located:
    MyData <- MyData[order(-MyData$FullL), ]
    MyData$Prop <- cumsum(MyData$FullL / sum(MyData$FullL, na.rm = TRUE))
    Props <- c()
    for(MyProp in 1:10/10){
      Props <- c(Props, sum(MyData$Prop <= MyProp))
    }
    FullSProp <- rbind(FullSProp, data.frame(ExNum = Ex, CrownShape = CS, WeatherStation = WS, CrownTransmissibility = CT,
                                             ForestGapSize = GS, PlotID = ID, Proportion = 1:10/10, Area = Props))
    
    # Smooth Area to eaven out random differences in Area:
    MySmooth <- Smooth.ppp(as.ppp(MyData[, c("x", "y", "Rad")], 
                                  W = owin(poly = list(x = PolyX, y = PolyY))), 
                           edge = TRUE, 
                           dimyx = c(101, 101),
                           sigma = 3)
    SmoothedData <- data.frame(x = 1, y = 1, Rad = 1)[-1, ]
    for(xi in 1:101){
      for(yi in 1:101){
        SmoothedData <- rbind(SmoothedData, data.frame(x = xi, y = yi, Rad = MySmooth$v[yi, xi]))
      }
    }
    SmoothedData$x <- SmoothedData$x - 51
    SmoothedData$y <- SmoothedData$y - 51
    
    # Calculate Radiation gradient from North to South
    GapInter <- rbind(GapInter, data.frame(ExNum = Ex, CrownShape = CS, WeatherStation = WS, CrownTransmissibility = CT,
                                           ForestGapSize = GS, PlotID = ID, yCoord = SmoothedData$y[SmoothedData$x == 0], 
                                           Rad = SmoothedData$Rad[SmoothedData$x == 0]))
    
    # Calculate Species Distribution within Gap.
    GapSpecies <- rbind(GapSpecies, data.frame(ExNum = Ex, CrownShape = CS, WeatherStation = WS, CrownTransmissibility = CT,
                                               ForestGapSize = GS, PlotID = ID, 
                                               ShadeTolerance = c("high", "intermediate", "low"),
                                               Proportion = c(sum(between(SmoothedData$Rad / MySun, 0, 0.15), na.rm = TRUE) / sum(!is.na(SmoothedData$Rad)),
                                                              sum(between(SmoothedData$Rad / MySun, 0.15, 0.24), na.rm = TRUE) / sum(!is.na(SmoothedData$Rad)),
                                                              sum(between(SmoothedData$Rad / MySun, 0.24, 1), na.rm = TRUE) / sum(!is.na(SmoothedData$Rad)))))
    
    
    # Calculate how much times was spent and is still needed.
    MyTimes <- c(MyTimes, Sys.time())
    TimeNeeded <- MyTimes[Ex + 1] - MyTimes[1]
    EndTime <- as.POSIXct("1970-01-01 01:00:00 CET") + as.numeric(MyTimes[1]) + (TimeNeeded) / Ex * nEx
    cat("\r", paste0(round(Ex / nEx, digits = 2) * 100, " % done. || Time needed: ", 
                     format(round(TimeNeeded, digits = 2), nsmall = 2), "s. || Expected End Time: ", EndTime))
  }
}



#################################################################################################################
#### Visualization ####
#################################################################################################################
# Let's just Visualize the Results. These are just here so we get an Overview.

# Look at Weighted Centroids of Total Radiation
ggplot(Weighted_Rad)+
  geom_point(aes(x = CentroidX, y = CentroidY, color = CrownTransmissibility, shape = CrownShape))+
  annotate("point", x = 0, y = 0, size = 2, shape = 23, color = "grey30", fill = "grey90", stroke = 2)+
  scale_x_continuous(name = "X-Corrdinate [m]", limits = c(-10, 10))+
  scale_y_continuous(name = "Y-Corrdinate [m]", limits = c(-10, 10))+
  scale_color_viridis(name = "Crown\nTransmissibility")+
  facet_grid(ForestGapSize ~ WeatherStation, labeller = labeller(WeatherStation = FLabels, ForestGapSize = FLabels))+
  coord_fixed()+
  theme_bw()

# Weighted Centroid of Hours with Full Sunlight
ggplot(Weighted_Full[Weighted_Full$CrownTransmissibility == 0.05, ])+
  geom_point(aes(x = CentroidX, y = CentroidY))+
  annotate("point", x = 0, y = 0, size = 2, shape = 23, color = "grey30", fill = "grey90", stroke = 2)+
  scale_x_continuous(name = "X-Corrdinate [m]", limits = c(-25, 25))+
  scale_y_continuous(name = "Y-Corrdinate [m]", limits = c(-25, 25))+
  facet_grid(ForestGapSize ~ WeatherStation, labeller = labeller(WeatherStation = FLabels, ForestGapSize = FLabels))+
  coord_fixed()+
  theme_bw()

# Total Radiation Sum within 50m from Gap Midpoint
TotalRad2 <- merge(TotalRad, StationLats, by = "WeatherStation")
ggplot(TotalRad2)+
  geom_point(aes(x = Latitude, y = TotalRad, color = CrownShape))+
  scale_y_continuous(name = "Total Radiation within a 50m radius of the Gap Midpoint [W/m²]", limits = c(0, 5000000000))+
  facet_grid(ForestGapSize ~ CrownTransmissibility)
  
# Total Amount of Hours with Full Sunlight within 50m from Gap Midpoint:
TotalFull2 <- merge(TotalFull, StationLats, by = "WeatherStation")
ggplot(TotalFull2[TotalFull2$CrownTransmissibility == 0.05, ])+
  geom_point(aes(x = Latitude, y = TotalFull, color = CrownShape))+
  scale_y_continuous(name = "Total Radiation within a 50m radius of the Gap Midpoint [W/m²]", limits = c(0, 5000000))+
  facet_grid(. ~ ForestGapSize)

# Area where x Percent of the Radiation are located:
LightProp2 <- ddply(LightProp, ~ CrownShape + WeatherStation + CrownTransmissibility + ForestGapSize + Proportion, summarise, Area = mean(Area))
ggplot(LightProp2)+
  geom_line(aes(x = Proportion, y = Area / 7845, color = WeatherStation,
                group = paste0(CrownShape, WeatherStation, CrownTransmissibility, ForestGapSize)))+
  facet_grid(CrownTransmissibility ~ ForestGapSize)

# Area where x Percent of the Hours with Full Sunlight are located:
FullSProp2 <- ddply(FullSProp, ~ CrownShape + WeatherStation + CrownTransmissibility + ForestGapSize + Proportion, summarise, Area = mean(Area))
ggplot(FullSProp2[FullSProp2$Proportion != 1, ])+
  geom_line(aes(x = Proportion, y = Area / 7845, color = WeatherStation,
                group = paste0(CrownShape, WeatherStation, CrownTransmissibility, ForestGapSize)))+
  facet_grid(CrownTransmissibility ~ ForestGapSize)


# Radiation along a North-South Gradient from Gap
GapInter2 <- ddply(GapInter, ~ CrownShape + WeatherStation + CrownTransmissibility + ForestGapSize + yCoord, summarise, Rad = mean(Rad, na.rm = TRUE))
ggplot(GapInter2[GapInter2$CrownTransmissibility == 0, ])+
  geom_line(aes(x = yCoord, y = Rad, group = paste0(WeatherStation, CrownTransmissibility), color = WeatherStation))+
  facet_grid(CrownShape ~ ForestGapSize)


# Species Distribution inside Gap:
ggplot(GapSpecies)+
  geom_line(aes(x = ForestGapSize, y = Proportion, color = ShadeTolerance, 
                group = paste0(WeatherStation, CrownTransmissibility, ShadeTolerance, CrownShape, PlotID)))+
  facet_grid(WeatherStation ~ CrownTransmissibility)




#################################################################################################################
#### Sensitivity Analysis ####
#################################################################################################################
# Now we want to shed some Light on our Data. For this, we create 1 Data Frame containing all Results.
# We have conducted Standardized Linear Regression to evaluate Effects of these 4 Parameters, in the following defined as "Input":
# - Crown Shape
# - Latitude
# - Crown Transmissibility
# - Forest Gap Size
# To visualize Results, we first calculated a mean value for each Combination of Input Variable and Output Variable.
# For Example:
# We want to know how the Total Radiation inside the Observed Area changes with Forest Gap Size.
# We calculated for each Forest Gap Size the mean Total Radation over all 2500 (/ 5 different Forest Gap Sizes) Samples.
# Then we also calculated standard deviation to show Variability in the Data.
# we automated everything in a Loop and directly used Standardization:
SA_Standardized <- data.frame(ExplanatoryVariable = "A", ResponseVariable = "B", Input = 1, Mean = 1, SD = 1)[-1, ]
for(InVar in 1:4){
  ExVar <- colnames(Experiments2)[InVar]
  for(InVal in unique(Experiments2[, InVar])){
    MyVals <- which(Weighted_Rad[, ExVar] == InVal)
    InVal <- ifelse(is.numeric(InVal), InVal,
                    as.numeric(NNumbers[InVal]))
    
    MyOut <- Weighted_Rad[, "CentroidY"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    SA_Standardized <- rbind(SA_Standardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "WeightedRad", Input = InVal, 
                                        Mean = mean(MyOut), SD = sd(MyOut)))
    
    MyOut <- Weighted_Full[, "CentroidY"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    SA_Standardized <- rbind(SA_Standardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "WeightedFull", Input = InVal, 
                                        Mean = mean(MyOut), SD = sd(MyOut)))
    
    MyOut <- TotalRad[, "TotalRad"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    SA_Standardized <- rbind(SA_Standardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "TotalRad", Input = InVal, 
                                        Mean = mean(MyOut), SD = sd(MyOut)))
    
    MyOut <- TotalFull[, "TotalFull"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    SA_Standardized <- rbind(SA_Standardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "TotalFull", Input = InVal, 
                                        Mean = mean(MyOut), SD = sd(MyOut)))
    
    MyOut <- LightProp[LightProp$Proportion == 0.2, "Area"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    SA_Standardized <- rbind(SA_Standardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "LightProp", Input = InVal, 
                                        Mean = mean(MyOut), SD = sd(MyOut)))
    
    MyOut <- FullSProp[FullSProp$Proportion == 0.2, "Area"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    SA_Standardized <- rbind(SA_Standardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "FullSProp", Input = InVal, 
                                        Mean = mean(MyOut), SD = sd(MyOut)))
    
    MyOut <- GapSpecies[GapSpecies$ShadeTolerance == "high", "Proportion"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    SA_Standardized <- rbind(SA_Standardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "LRC_high", Input = InVal, 
                                        Mean = mean(MyOut), SD = sd(MyOut)))
    
    MyOut <- GapSpecies[GapSpecies$ShadeTolerance == "intermediate", "Proportion"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    SA_Standardized <- rbind(SA_Standardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "LRC_medium", Input = InVal, 
                                        Mean = mean(MyOut), SD = sd(MyOut)))
    
    MyOut <- GapSpecies[GapSpecies$ShadeTolerance == "low", "Proportion"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    SA_Standardized <- rbind(SA_Standardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "LRC_low", Input = InVal, 
                                        Mean = mean(MyOut), SD = sd(MyOut)))
  }
}
# Scale the Explanatory Variable between 0 and 1
SA_Standardized <- ddply(SA_Standardized, ~ ExplanatoryVariable, transform, 
                         Remin = min(Input), Remax = max(Input))
SA_Standardized$pExplanatory <- (1 / (SA_Standardized$Remax - SA_Standardized$Remin)) * SA_Standardized$Input + (-1 * (1 / (SA_Standardized$Remax - SA_Standardized$Remin)) * SA_Standardized$Remin)

# Also only for Crown-Shape-Boxplots, get all the Data:
BoxStandardized <- data.frame(ExplanatoryVariable = "A", ResponseVariable = "B", Input = 1, Output = 1)[-1, ]
for(InVar in 1){
  ExVar <- colnames(Experiments2)[InVar]
  for(InVal in unique(Experiments2[, InVar])){
    MyVals <- which(Weighted_Rad[, ExVar] == InVal)
    InVal <- ifelse(is.numeric(InVal), InVal,
                    as.numeric(NNumbers[InVal]))
    
    MyOut <- Weighted_Rad[, "CentroidY"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    BoxStandardized <- rbind(BoxStandardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "WeightedRad", Input = InVal, 
                                        Output = MyOut))
    
    MyOut <- Weighted_Full[, "CentroidY"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    BoxStandardized <- rbind(BoxStandardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "WeightedFull", Input = InVal, 
                                        Output = MyOut))
    
    MyOut <- TotalRad[, "TotalRad"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    BoxStandardized <- rbind(BoxStandardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "TotalRad", Input = InVal, 
                                        Output = MyOut))
    
    MyOut <- TotalFull[, "TotalFull"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    BoxStandardized <- rbind(BoxStandardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "TotalFull", Input = InVal, 
                                        Output = MyOut))
    
    MyOut <- LightProp[LightProp$Proportion == 0.2, "Area"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    BoxStandardized <- rbind(BoxStandardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "LightProp", Input = InVal, 
                                        Output = MyOut))
    
    MyOut <- FullSProp[FullSProp$Proportion == 0.2, "Area"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    BoxStandardized <- rbind(BoxStandardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "FullSProp", Input = InVal, 
                                        Output = MyOut))
    
    MyOut <- GapSpecies[GapSpecies$ShadeTolerance == "high", "Proportion"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    BoxStandardized <- rbind(BoxStandardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "LRC_high", Input = InVal, 
                                        Output = MyOut))
    
    MyOut <- GapSpecies[GapSpecies$ShadeTolerance == "intermediate", "Proportion"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    BoxStandardized <- rbind(BoxStandardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "LRC_medium", Input = InVal, 
                                        Output = MyOut))
    
    MyOut <- GapSpecies[GapSpecies$ShadeTolerance == "low", "Proportion"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    BoxStandardized <- rbind(BoxStandardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "LRC_low", Input = InVal, 
                                        Output = MyOut))
  }
}
BoxStandardized <- ddply(BoxStandardized, ~ ExplanatoryVariable, transform, 
                         Remin = min(Input), Remax = max(Input))
BoxStandardized$pExplanatory <- (1 / (BoxStandardized$Remax - BoxStandardized$Remin)) * BoxStandardized$Input + (-1 * (1 / (BoxStandardized$Remax - BoxStandardized$Remin)) * BoxStandardized$Remin)

# Some important Parameters for the Plots
TextSize <- 8
YSpace <- 0.1
SA_Standardized$ResponseVariable <- factor(SA_Standardized$ResponseVariable, levels = FLevels)
YLimits <- c(-1.5, 1.8)

# Now Plot everything
Plot1 <- ggplot(SA_Standardized[SA_Standardized$ExplanatoryVariable == "CrownShape", ])+
  geom_boxplot(data = BoxStandardized, aes(x = pExplanatory, y = Output, group = pExplanatory), width = 0.2, fill = "grey70")+
  geom_line(aes(x = pExplanatory, y = Mean))+
  geom_point(aes(x = pExplanatory, y = Mean))+
  scale_x_continuous(name = "Crown Shape", breaks = c(0, 1), labels = c("Disk", "Ellipsoid"), limits = c(-0.125, 1.125), minor_breaks = NULL)+
  scale_y_continuous(name = "Response Variable", breaks = c(-1, 1), labels = c("low", "high"))+
  facet_grid(ResponseVariable ~ ExplanatoryVariable, labeller = labeller(ResponseVariable = FLabels2, ExplanatoryVariable = FLabels2))+
  theme_bw()+
  coord_cartesian(ylim = YLimits)+
  theme(text = element_text(size = TextSize),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y = element_blank(),
        axis.text.y = element_text(vjust = c(0, 1)),
        strip.background.y = element_blank(),
        panel.spacing.y = unit(YSpace, "lines"),
        plot.margin = unit( c(0,0,0,0) , units = "lines" ) ); Plot1

Plot2 <- ggplot(SA_Standardized[SA_Standardized$ExplanatoryVariable == "WeatherStation", ])+
  geom_ribbon(aes(x = pExplanatory, ymin = Mean - SD, ymax = Mean + SD), fill = "grey70")+
  geom_line(aes(x = pExplanatory, y = Mean))+
  geom_point(aes(x = pExplanatory, y = Mean))+
  scale_x_continuous(name = "Latitude", breaks = unique(SA_Standardized$pExplanatory[SA_Standardized$ExplanatoryVariable == "WeatherStation"]), 
                     labels = unique(SA_Standardized$Input[SA_Standardized$ExplanatoryVariable == "WeatherStation"]), limits = c(-0.125, 1.125), minor_breaks = NULL)+
  scale_y_continuous(name = "Response Variable", breaks = c(-1, 1), labels = c("low", "high"))+
  facet_grid(ResponseVariable ~ ExplanatoryVariable, labeller = labeller(ResponseVariable = FLabels2, ExplanatoryVariable = FLabels2))+
  theme_bw()+
  coord_cartesian(ylim = YLimits)+
  theme(text = element_text(size = TextSize),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.y = element_blank(),
        panel.spacing.y = unit(YSpace, "lines"),
        plot.margin = unit( c(0,0,0,0) , units = "lines" ) ); Plot2

Plot3 <- ggplot(SA_Standardized[SA_Standardized$ExplanatoryVariable == "CrownTransmissibility", ])+
  geom_ribbon(aes(x = pExplanatory, ymin = Mean - SD, ymax = Mean + SD), fill = "grey70")+
  geom_line(aes(x = pExplanatory, y = Mean))+
  geom_point(aes(x = pExplanatory, y = Mean))+
  scale_x_continuous(name = "Crown Transmissibility", breaks = unique(SA_Standardized$pExplanatory[SA_Standardized$ExplanatoryVariable == "CrownTransmissibility"]), 
                     labels = unique(SA_Standardized$Input[SA_Standardized$ExplanatoryVariable == "CrownTransmissibility"]), limits = c(-0.125, 1.125), minor_breaks = NULL)+
  scale_y_continuous(name = "Response Variable", breaks = c(-1, 1), labels = c("low", "high"))+
  facet_grid(ResponseVariable ~ ExplanatoryVariable, labeller = labeller(ResponseVariable = FLabels2, ExplanatoryVariable = FLabels2))+
  theme_bw()+
  coord_cartesian(ylim = YLimits)+
  theme(text = element_text(size = TextSize),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.y = element_blank(),
        panel.spacing.y = unit(YSpace, "lines"),
        plot.margin = unit( c(0,0,0,0) , units = "lines" )); Plot3

Plot4 <- ggplot(SA_Standardized[SA_Standardized$ExplanatoryVariable == "ForestGapSize", ])+
  geom_ribbon(aes(x = pExplanatory, ymin = Mean - SD, ymax = Mean + SD), fill = "grey70")+
  geom_line(aes(x = pExplanatory, y = Mean))+
  geom_point(aes(x = pExplanatory, y = Mean))+
  scale_x_continuous(name = "Gap Size", breaks = unique(SA_Standardized$pExplanatory[SA_Standardized$ExplanatoryVariable == "ForestGapSize"]), 
                     labels = unique(SA_Standardized$Input[SA_Standardized$ExplanatoryVariable == "ForestGapSize"]), limits = c(-0.125, 1.125), minor_breaks = NULL)+
  scale_y_continuous(name = "Response Variable", breaks = c(-1, 1), labels = c("low", "high"))+
  facet_grid(ResponseVariable~ ExplanatoryVariable, labeller = labeller(ResponseVariable = FLabels2, ExplanatoryVariable = FLabels2))+
  theme_bw()+
  coord_cartesian(ylim = YLimits)+
  theme(text = element_text(size = TextSize),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(angle = 0),
        panel.spacing.y = unit(YSpace, "lines"),
        plot.margin = unit( c(0,0,0,0) , units = "lines" )); Plot4

# Align Plots so that they look nice if combined.
AlignedPlots <- align_plots(Plot1, Plot2, Plot3, Plot4, align = "h", axis = "b")
# Plot All Plots together
AllPlots <- plot_grid(AlignedPlots[[1]], AlignedPlots[[2]], AlignedPlots[[3]], AlignedPlots[[4]], nrow = 1, rel_widths = c(1.25, 1, 1, 1.4)); AllPlots


#################################################################################################################
#### Standardized Regression Analysis ####
#################################################################################################################
# We used Standardized Regression Analysis to investigate the Influence of our Model Parameters on Model Output. 
# We first Standardize Regression Coefficients and then calculate Standardized Regression for each Output Variable
# and then combine it into a Matrix.

# Create an empty Data Frame and fill it with Results:
Reg.Data <- data.frame(CrownShape = 1, Latitude = 1, CrownTransmissibility = 1, ForestGapSize = 1)[-1, ]

lm.Data <- TotalRad
lm.Data$CrownShape <- ifelse(lm.Data$CrownShape == "Disk", -1, 1)
lm.Data$Latitude <-  NNumbers[lm.Data$WeatherStation]
lm.Data$Latitude <- (lm.Data$Latitude - mean(lm.Data$Latitude)) / sd(lm.Data$Latitude)
lm.Data$CrownTransmissibility <- (lm.Data$CrownTransmissibility - mean(lm.Data$CrownTransmissibility)) / sd(lm.Data$CrownTransmissibility)
lm.Data$ForestGapSize <- (lm.Data$ForestGapSize - mean(lm.Data$ForestGapSize)) / sd(lm.Data$ForestGapSize)
lm.Data$TotalRad <- (lm.Data$TotalRad - mean(lm.Data$TotalRad)) / sd(lm.Data$TotalRad)
lm.TotalRad <- lm(TotalRad ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = lm.Data)
Reg.Data["TotalRad", ] <- coef(lm.TotalRad)[2:5]

lm.Data <- TotalFull
lm.Data$CrownShape <- ifelse(lm.Data$CrownShape == "Disk", -1, 1)
lm.Data$Latitude <-  NNumbers[lm.Data$WeatherStation]
lm.Data$Latitude <- (lm.Data$Latitude - mean(lm.Data$Latitude)) / sd(lm.Data$Latitude)
lm.Data$CrownTransmissibility <- (lm.Data$CrownTransmissibility - mean(lm.Data$CrownTransmissibility)) / sd(lm.Data$CrownTransmissibility)
lm.Data$ForestGapSize <- (lm.Data$ForestGapSize - mean(lm.Data$ForestGapSize)) / sd(lm.Data$ForestGapSize)
lm.Data$TotalFull <- (lm.Data$TotalFull - mean(lm.Data$TotalFull)) / sd(lm.Data$TotalFull)
lm.TotalFull <- lm(TotalFull ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = lm.Data)
Reg.Data["TotalFull", ] <- coef(lm.TotalFull)[2:5]
summary(lm.TotalFull)$coefficients[2:5, 4]

lm.Data <- Weighted_Rad
lm.Data$CrownShape <- ifelse(lm.Data$CrownShape == "Disk", -1, 1)
lm.Data$Latitude <-  NNumbers[lm.Data$WeatherStation]
lm.Data$Latitude <- (lm.Data$Latitude - mean(lm.Data$Latitude)) / sd(lm.Data$Latitude)
lm.Data$CrownTransmissibility <- (lm.Data$CrownTransmissibility - mean(lm.Data$CrownTransmissibility)) / sd(lm.Data$CrownTransmissibility)
lm.Data$ForestGapSize <- (lm.Data$ForestGapSize - mean(lm.Data$ForestGapSize)) / sd(lm.Data$ForestGapSize)
lm.Data$CentroidY <- (lm.Data$CentroidY - mean(lm.Data$CentroidY)) / sd(lm.Data$CentroidY)
lm.WeightedRad <- lm(CentroidY ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = lm.Data)
Reg.Data["WeightedRad", ] <- coef(lm.WeightedRad)[2:5]

lm.Data <- Weighted_Full
lm.Data$CrownShape <- ifelse(lm.Data$CrownShape == "Disk", -1, 1)
lm.Data$Latitude <-  NNumbers[lm.Data$WeatherStation]
lm.Data$Latitude <- (lm.Data$Latitude - mean(lm.Data$Latitude)) / sd(lm.Data$Latitude)
lm.Data$CrownTransmissibility <- (lm.Data$CrownTransmissibility - mean(lm.Data$CrownTransmissibility)) / sd(lm.Data$CrownTransmissibility)
lm.Data$ForestGapSize <- (lm.Data$ForestGapSize - mean(lm.Data$ForestGapSize)) / sd(lm.Data$ForestGapSize)
lm.Data$CentroidY <- (lm.Data$CentroidY - mean(lm.Data$CentroidY)) / sd(lm.Data$CentroidY)
lm.WeightedFull <- lm(CentroidY ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = lm.Data)
Reg.Data["WeightedFull", ] <- coef(lm.WeightedFull)[2:5]

lm.Data <- LightProp[LightProp$Proportion == 0.2, ]
lm.Data$CrownShape <- ifelse(lm.Data$CrownShape == "Disk", -1, 1)
lm.Data$Latitude <-  NNumbers[lm.Data$WeatherStation]
lm.Data$Latitude <- (lm.Data$Latitude - mean(lm.Data$Latitude)) / sd(lm.Data$Latitude)
lm.Data$CrownTransmissibility <- (lm.Data$CrownTransmissibility - mean(lm.Data$CrownTransmissibility)) / sd(lm.Data$CrownTransmissibility)
lm.Data$ForestGapSize <- (lm.Data$ForestGapSize - mean(lm.Data$ForestGapSize)) / sd(lm.Data$ForestGapSize)
lm.Data$Area <- (lm.Data$Area - mean(lm.Data$Area)) / sd(lm.Data$Area)
lm.LightProp <- lm(Area ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = lm.Data)
Reg.Data["LightProp", ] <- coef(lm.LightProp)[2:5]

lm.Data <- FullSProp[FullSProp$Proportion == 0.2, ]
lm.Data$CrownShape <- ifelse(lm.Data$CrownShape == "Disk", -1, 1)
lm.Data$Latitude <-  NNumbers[lm.Data$WeatherStation]
lm.Data$Latitude <- (lm.Data$Latitude - mean(lm.Data$Latitude)) / sd(lm.Data$Latitude)
lm.Data$CrownTransmissibility <- (lm.Data$CrownTransmissibility - mean(lm.Data$CrownTransmissibility)) / sd(lm.Data$CrownTransmissibility)
lm.Data$ForestGapSize <- (lm.Data$ForestGapSize - mean(lm.Data$ForestGapSize)) / sd(lm.Data$ForestGapSize)
lm.Data$Area <- (lm.Data$Area - mean(lm.Data$Area)) / sd(lm.Data$Area)
lm.FullSProp <- lm(Area ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = lm.Data)
Reg.Data["FullSProp", ] <- coef(lm.FullSProp)[2:5]

lm.Data <- GapSpecies[GapSpecies$ShadeTolerance == "high", ]
lm.Data$CrownShape <- ifelse(lm.Data$CrownShape == "Disk", -1, 1)
lm.Data$Latitude <-  NNumbers[lm.Data$WeatherStation]
lm.Data$Latitude <- (lm.Data$Latitude - mean(lm.Data$Latitude)) / sd(lm.Data$Latitude)
lm.Data$CrownTransmissibility <- (lm.Data$CrownTransmissibility - mean(lm.Data$CrownTransmissibility)) / sd(lm.Data$CrownTransmissibility)
lm.Data$ForestGapSize <- (lm.Data$ForestGapSize - mean(lm.Data$ForestGapSize)) / sd(lm.Data$ForestGapSize)
lm.Data$Proportion <- (lm.Data$Proportion - mean(lm.Data$Proportion)) / sd(lm.Data$Proportion)
lm.SpecHigh <- lm(Proportion ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = lm.Data)
Reg.Data["SpecHigh", ] <- coef(lm.SpecHigh)[2:5]

lm.Data <- GapSpecies[GapSpecies$ShadeTolerance == "intermediate", ]
lm.Data$CrownShape <- ifelse(lm.Data$CrownShape == "Disk", -1, 1)
lm.Data$Latitude <-  NNumbers[lm.Data$WeatherStation]
lm.Data$Latitude <- (lm.Data$Latitude - mean(lm.Data$Latitude)) / sd(lm.Data$Latitude)
lm.Data$CrownTransmissibility <- (lm.Data$CrownTransmissibility - mean(lm.Data$CrownTransmissibility)) / sd(lm.Data$CrownTransmissibility)
lm.Data$ForestGapSize <- (lm.Data$ForestGapSize - mean(lm.Data$ForestGapSize)) / sd(lm.Data$ForestGapSize)
lm.Data$Proportion <- (lm.Data$Proportion - mean(lm.Data$Proportion)) / sd(lm.Data$Proportion)
lm.SpecIntermediate <- lm(Proportion ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = lm.Data)
Reg.Data["SpecIntermediate", ] <- coef(lm.SpecIntermediate)[2:5]

lm.Data <- GapSpecies[GapSpecies$ShadeTolerance == "low", ]
lm.Data$CrownShape <- ifelse(lm.Data$CrownShape == "Disk", -1, 1)
lm.Data$Latitude <-  NNumbers[lm.Data$WeatherStation]
lm.Data$Latitude <- (lm.Data$Latitude - mean(lm.Data$Latitude)) / sd(lm.Data$Latitude)
lm.Data$CrownTransmissibility <- (lm.Data$CrownTransmissibility - mean(lm.Data$CrownTransmissibility)) / sd(lm.Data$CrownTransmissibility)
lm.Data$ForestGapSize <- (lm.Data$ForestGapSize - mean(lm.Data$ForestGapSize)) / sd(lm.Data$ForestGapSize)
lm.Data$Proportion <- (lm.Data$Proportion - mean(lm.Data$Proportion)) / sd(lm.Data$Proportion)
lm.SpecLow <- lm(Proportion ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = lm.Data)
Reg.Data["SpecLow", ] <- coef(lm.SpecLow)[2:5]

Reg.Data <- round(Reg.Data, digits = 4)


# Now we look at the Standardized Coefficients and can see which Parameters have big Influence on Output:
Reg.Data

# Let's see if the Results have statistically significant influence on our Output:
y <- as.matrix(data.frame(rbind(t(summary(lm.TotalRad)$coefficients[2:5, 4]),
                                t(summary(lm.TotalFull)$coefficients[2:5, 4]),
                                t(summary(lm.WeightedRad)$coefficients[2:5, 4]),
                                t(summary(lm.WeightedFull)$coefficients[2:5, 4]),
                                t(summary(lm.LightProp)$coefficients[2:5, 4]),
                                t(summary(lm.FullSProp)$coefficients[2:5, 4]),
                                t(summary(lm.SpecHigh)$coefficients[2:5, 4]),
                                t(summary(lm.SpecIntermediate)$coefficients[2:5, 4]),
                                t(summary(lm.SpecLow)$coefficients[2:5, 4]))))

ifelse(y < 0.05, "*", "")

# Most Variables have significan Influence, which is not surprising, considering the Sample Size.


#################################################################################################################
#### Interaction of Input ####
#################################################################################################################
# Interactions can have a big Influence on Regression Results. To evaluate Interaction-Effects, we made Heat Maps for 
# each Combination of Parameters.

# Empty Data Frame
HeatResults <- data.frame(Variable1 = "a", Value1 = 1, Variable2 = "a", Value2 = 1, ResponseVariable = "a", Output = 1)[-1, ]

# For each Combination of Input Parameters, calculate Mean Output
for(Var1 in 1:4){
  Variable1 <- colnames(Experiments2)[Var1]
  for(Var2 in (1:4)[-Var1]){
    Variable2 <- colnames(Experiments2)[Var2]
    
    # Weighted Radiation Centroid-Y-Coordinate
    MyData <- Weighted_Rad
    MyData <- MyData[, c(Var1 + 1, Var2 + 1, 8)]
    colnames(MyData) <- c("Vari1", "Vari2", "Out")
    MyData <- ddply(MyData, ~ Vari1 + Vari2, summarise, Out = mean(Out))
    if(!is.numeric(MyData$Vari1)){
      MyData$Vari1 <- as.numeric(NNumbers[as.character(MyData$Vari1)])
    }
    if(!is.numeric(MyData$Vari2)){
      MyData$Vari2 <- as.numeric(NNumbers[as.character(MyData$Vari2)])
    }
    HeatResults <- rbind(HeatResults, 
                         data.frame(Variable1 = Variable1,
                                    Value1 = MyData$Vari1,
                                    Variable2 = Variable2, 
                                    Value2 = MyData$Vari2,
                                    ResponseVariable = "WeightedRad", 
                                    Output = MyData$Out))
    
    # Weighted Full SunLight Centroid-Y-Coordinate
    MyData <- Weighted_Full
    MyData <- MyData[, c(Var1 + 1, Var2 + 1, 8)]
    colnames(MyData) <- c("Vari1", "Vari2", "Out")
    MyData <- ddply(MyData, ~ Vari1 + Vari2, summarise, Out = mean(Out))
    if(!is.numeric(MyData$Vari1)){
      MyData$Vari1 <- as.numeric(NNumbers[as.character(MyData$Vari1)])
    }
    if(!is.numeric(MyData$Vari2)){
      MyData$Vari2 <- as.numeric(NNumbers[as.character(MyData$Vari2)])
    }
    HeatResults <- rbind(HeatResults, 
                         data.frame(Variable1 = Variable1,
                                    Value1 = MyData$Vari1,
                                    Variable2 = Variable2, 
                                    Value2 = MyData$Vari2,
                                    ResponseVariable = "WeightedFull", 
                                    Output = MyData$Out))
    
    # Total Radiation in 50m radius of Gap Midpoint
    MyData <- TotalRad
    MyData <- MyData[, c(Var1 + 1, Var2 + 1, 7)]
    colnames(MyData) <- c("Vari1", "Vari2", "Out")
    MyData <- ddply(MyData, ~ Vari1 + Vari2, summarise, Out = mean(Out))
    if(!is.numeric(MyData$Vari1)){
      MyData$Vari1 <- as.numeric(NNumbers[as.character(MyData$Vari1)])
    }
    if(!is.numeric(MyData$Vari2)){
      MyData$Vari2 <- as.numeric(NNumbers[as.character(MyData$Vari2)])
    }
    HeatResults <- rbind(HeatResults, 
                         data.frame(Variable1 = Variable1,
                                    Value1 = MyData$Vari1,
                                    Variable2 = Variable2, 
                                    Value2 = MyData$Vari2,
                                    ResponseVariable = "TotalRad", 
                                    Output = MyData$Out))
    
    # Total Hours with Full Sunlight in 50m radius of Gap Midpoint
    MyData <- TotalFull
    MyData <- MyData[, c(Var1 + 1, Var2 + 1, 7)]
    colnames(MyData) <- c("Vari1", "Vari2", "Out")
    MyData <- ddply(MyData, ~ Vari1 + Vari2, summarise, Out = mean(Out))
    if(!is.numeric(MyData$Vari1)){
      MyData$Vari1 <- as.numeric(NNumbers[as.character(MyData$Vari1)])
    }
    if(!is.numeric(MyData$Vari2)){
      MyData$Vari2 <- as.numeric(NNumbers[as.character(MyData$Vari2)])
    }
    HeatResults <- rbind(HeatResults, 
                         data.frame(Variable1 = Variable1,
                                    Value1 = MyData$Vari1,
                                    Variable2 = Variable2, 
                                    Value2 = MyData$Vari2,
                                    ResponseVariable = "TotalFull", 
                                    Output = MyData$Out))
    
    # Area where 20 % of Shadow is located
    MyData <- LightProp[LightProp$Proportion == 0.2, ]
    MyData <- MyData[, c(Var1 + 1, Var2 + 1, 8)]
    colnames(MyData) <- c("Vari1", "Vari2", "Out")
    MyData <- ddply(MyData, ~ Vari1 + Vari2, summarise, Out = mean(Out))
    if(!is.numeric(MyData$Vari1)){
      MyData$Vari1 <- as.numeric(NNumbers[as.character(MyData$Vari1)])
    }
    if(!is.numeric(MyData$Vari2)){
      MyData$Vari2 <- as.numeric(NNumbers[as.character(MyData$Vari2)])
    }
    HeatResults <- rbind(HeatResults, 
                         data.frame(Variable1 = Variable1,
                                    Value1 = MyData$Vari1,
                                    Variable2 = Variable2, 
                                    Value2 = MyData$Vari2,
                                    ResponseVariable = "LightProp", 
                                    Output = MyData$Out))
    
    # Area where 20% of hours with full sunlight are located
    MyData <- FullSProp[FullSProp$Proportion == 0.2, ]
    MyData <- MyData[, c(Var1 + 1, Var2 + 1, 8)]
    colnames(MyData) <- c("Vari1", "Vari2", "Out")
    MyData <- ddply(MyData, ~ Vari1 + Vari2, summarise, Out = mean(Out))
    if(!is.numeric(MyData$Vari1)){
      MyData$Vari1 <- as.numeric(NNumbers[as.character(MyData$Vari1)])
    }
    if(!is.numeric(MyData$Vari2)){
      MyData$Vari2 <- as.numeric(NNumbers[as.character(MyData$Vari2)])
    }
    HeatResults <- rbind(HeatResults, 
                         data.frame(Variable1 = Variable1,
                                    Value1 = MyData$Vari1,
                                    Variable2 = Variable2, 
                                    Value2 = MyData$Vari2,
                                    ResponseVariable = "FullSProp", 
                                    Output = MyData$Out))
    
    # Species Distribution within 50m radius of Gap midpoint
    MyData <- GapSpecies[GapSpecies$ShadeTolerance == "high", ]
    MyData <- MyData[, c(Var1 + 1, Var2 + 1, 8)]
    colnames(MyData) <- c("Vari1", "Vari2", "Out")
    MyData <- ddply(MyData, ~ Vari1 + Vari2, summarise, Out = mean(Out))
    if(!is.numeric(MyData$Vari1)){
      MyData$Vari1 <- as.numeric(NNumbers[as.character(MyData$Vari1)])
    }
    if(!is.numeric(MyData$Vari2)){
      MyData$Vari2 <- as.numeric(NNumbers[as.character(MyData$Vari2)])
    }
    HeatResults <- rbind(HeatResults, 
                         data.frame(Variable1 = Variable1,
                                    Value1 = MyData$Vari1,
                                    Variable2 = Variable2, 
                                    Value2 = MyData$Vari2,
                                    ResponseVariable = "GapSpecies_high", 
                                    Output = MyData$Out))
    MyData <- GapSpecies[GapSpecies$ShadeTolerance == "intermediate", ]
    MyData <- MyData[, c(Var1 + 1, Var2 + 1, 8)]
    colnames(MyData) <- c("Vari1", "Vari2", "Out")
    MyData <- ddply(MyData, ~ Vari1 + Vari2, summarise, Out = mean(Out))
    if(!is.numeric(MyData$Vari1)){
      MyData$Vari1 <- as.numeric(NNumbers[as.character(MyData$Vari1)])
    }
    if(!is.numeric(MyData$Vari2)){
      MyData$Vari2 <- as.numeric(NNumbers[as.character(MyData$Vari2)])
    }
    HeatResults <- rbind(HeatResults, 
                         data.frame(Variable1 = Variable1,
                                    Value1 = MyData$Vari1,
                                    Variable2 = Variable2, 
                                    Value2 = MyData$Vari2,
                                    ResponseVariable = "GapSpecies_intermediate", 
                                    Output = MyData$Out))
    MyData <- GapSpecies[GapSpecies$ShadeTolerance == "low", ]
    MyData <- MyData[, c(Var1 + 1, Var2 + 1, 8)]
    colnames(MyData) <- c("Vari1", "Vari2", "Out")
    MyData <- ddply(MyData, ~ Vari1 + Vari2, summarise, Out = mean(Out))
    if(!is.numeric(MyData$Vari1)){
      MyData$Vari1 <- as.numeric(NNumbers[as.character(MyData$Vari1)])
    }
    if(!is.numeric(MyData$Vari2)){
      MyData$Vari2 <- as.numeric(NNumbers[as.character(MyData$Vari2)])
    }
    HeatResults <- rbind(HeatResults, 
                         data.frame(Variable1 = Variable1,
                                    Value1 = MyData$Vari1,
                                    Variable2 = Variable2, 
                                    Value2 = MyData$Vari2,
                                    ResponseVariable = "GapSpecies_low", 
                                    Output = MyData$Out))
  }
}

# We wanted to visualize Interactions in a Heat Map. As Tiles of the Map are unequal in Size, we just calculated them manually.
HeatResultsTiles <- data.frame(Variable1 = "a", Value1 = 1, Variable2 = "a", Value2 = 1, ResponseVariable = "a", Output = 1, PNr = 1, XValue = 1, YValue = 2)[-1, ]
for(i in 1:nrow(HeatResults)){
  Varia1 <- HeatResults$Variable1[i]
  Varia2 <- HeatResults$Variable2[i]
  Outmin <- min(HeatResults$Output[HeatResults$ResponseVariable == HeatResults$ResponseVariable[i]])
  Outmax <- max(HeatResults$Output[HeatResults$ResponseVariable == HeatResults$ResponseVariable[i]])
  All1 <- sort(unique(HeatResults$Value1[HeatResults$Variable1 == Varia1]))
  X1 <- which(All1 == HeatResults$Value1[i])
  if(X1 == 1){
    Xmin <- All1[1] - (All1[2] - All1[1]) / 2
    Xmax <- All1[1] + (All1[2] - All1[1]) / 2
  } else {
    if(X1 == length(All1)){
      Xmin <- All1[X1] - (All1[X1] - All1[X1 - 1]) / 2
      Xmax <- All1[X1] + (All1[X1] - All1[X1 - 1]) / 2
    } else {
      Xmin <- All1[X1] - (All1[X1] - All1[X1 - 1]) / 2
      Xmax <- All1[X1] + (All1[X1 + 1] - All1[X1]) / 2
    }
  }
  All2 <- sort(unique(HeatResults$Value2[HeatResults$Variable2 == Varia2]))
  X2 <- which(All2 == HeatResults$Value2[i])
  if(X2 == 1){
    Ymin <- All2[1] - (All2[2] - All2[1]) / 2
    Ymax <- All2[1] + (All2[2] - All2[1]) / 2
  } else {
    if(X2 == length(All2)){
      Ymin <- All2[X2] - (All2[X2] - All2[X2 - 1]) / 2
      Ymax <- All2[X2] + (All2[X2] - All2[X2 - 1]) / 2
    } else {
      Ymin <- All2[X2] - (All2[X2] - All2[X2 - 1]) / 2
      Ymax <- All2[X2] + (All2[X2 + 1] - All2[X2]) / 2
    }
  }
  HeatResultsTiles <- rbind(HeatResultsTiles, 
                            data.frame(Variable1 = Varia1, Value1 = HeatResults$Value1[i], Variable2 = Varia2, Value2 = HeatResults$Value2[i], 
                                       ResponseVariable = HeatResults$ResponseVariable[i], Output = 1 / (Outmax - Outmin) * HeatResults$Output[i] - Outmin * 1 / (Outmax - Outmin), 
                                       PNr = 1:4, XValue = c(Xmin, Xmax, Xmax, Xmin), YValue = c(Ymin, Ymin, Ymax, Ymax)))
}

# Plot Interactions in Heatmap
# Define some Parameters for Plots
MyMargin <- margin(l=0,unit="cm")
PlotLegend <- ggplot(data.frame(xmin = 1:10, xmax = 2:11, ymin = 1:10, ymax = 2:11, Resp = seq(from = 0, to = 1, length.out = 10)))+
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Resp))+
  scale_fill_viridis(name = "Response Variable", breaks = c(0, 1), labels = c("low", "high"))+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        plot.background = element_rect(fill = "transparent", color = "transparent"),
        legend.text = element_text(size = 6),
        plot.margin = MyMargin,
        legend.margin = margin(r = 0, l = 0, t = 0, b = 0, unit = "cm"),
        legend.box.margin = margin(r = 0, l = 0, t = 0, b = 0, unit = "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.title = element_text(size = 6))+
  guides(fill = guide_colorbar(title.vjust = 0.8))
MyLegend <- get_plot_component(PlotLegend, "guide-box")
TextSize <- 8
PlotWLeft <- 1.61
PlotWMid <- 1
PlotWRight <- 1.45
PlotHTop <- 1.3
PlotHMid <- 1
PlotHBot <- 1.45
for(Var1 in 1:4){
  Variable1 <- colnames(Experiments2)[Var1]
  minVar2 <- min((1:4)[-Var1])
  maxVar2 <- max((1:4)[-Var1])
  PlotNr <- 1
  for(Var2 in (1:4)[-Var1]){
    Variable2 <- colnames(Experiments2)[Var2]
    # for(Resp in c("TotalRad", "TotalFull", "WeightedRad", "WeightedFull", "LightProp", "FullSProp", "GapSpecies_low", "GapSpecies_intermediate", "GapSpecies_high")){
    for(Resp in c("TotalRad", "WeightedRad", "LightProp", "TotalFull", "WeightedFull", "FullSProp", "GapSpecies_low", "GapSpecies_intermediate", "GapSpecies_high")){
      if(Var2 == minVar2){
        if(Resp == "GapSpecies_high"){
          MyPlot <- ggplot(HeatResultsTiles[HeatResultsTiles$Variable1 == Variable1 &
                                              HeatResultsTiles$Variable2 == Variable2 &
                                              HeatResultsTiles$ResponseVariable == Resp, ])+
            geom_polygon(aes(x = XValue, y = YValue, fill = Output, group = paste0(Value1, Value2)))+
            scale_fill_viridis(guide = "none")+
            facet_grid(Variable2 ~ ResponseVariable , scales = "free", labeller = labeller(Variable2 = FLabels3, ResponseVariable = FLabels3))+
            theme_bw()+
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  text = element_text(size = TextSize),
                  plot.margin = MyMargin,
                  plot.background = element_rect(fill = "transparent", color = "transparent"))
          assign(paste0("Plot", PlotNr), MyPlot)
          
          PlotNr <- PlotNr + 1
        } else {
          if(Resp == "TotalRad"){
            Myyaxis <- scale_y_continuous(name = "", breaks = unique(HeatResultsTiles$Value2[HeatResultsTiles$Variable2 == Variable2]))
            if(Variable2 == "CrownShape"){
              Myyaxis <- scale_y_continuous(name = "", breaks = unique(HeatResultsTiles$Value2[HeatResultsTiles$Variable2 == Variable2]),
                                            labels = c("1" = "Disk", "2" = "Ellipsoid"))
            }
            MyPlot <- ggplot(HeatResultsTiles[HeatResultsTiles$Variable1 == Variable1 &
                                                HeatResultsTiles$Variable2 == Variable2 &
                                                HeatResultsTiles$ResponseVariable == Resp, ])+
              geom_polygon(aes(x = XValue, y = YValue, fill = Output, group = paste0(Value1, Value2)))+
              scale_fill_viridis(guide = "none")+
              Myyaxis+
              facet_grid(. ~ ResponseVariable , scales = "free", labeller = labeller(Variable2 = FLabels3, ResponseVariable = FLabels3))+
              theme_bw()+
              theme(axis.title = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    text = element_text(size = TextSize),
                    plot.margin = MyMargin,
                    plot.background = element_rect(fill = "transparent", color = "transparent"))
            assign(paste0("Plot", PlotNr), MyPlot)
            
            PlotNr <- PlotNr + 1
          } else {
            MyPlot <- ggplot(HeatResultsTiles[HeatResultsTiles$Variable1 == Variable1 &
                                                HeatResultsTiles$Variable2 == Variable2 &
                                                HeatResultsTiles$ResponseVariable == Resp, ])+
              geom_polygon(aes(x = XValue, y = YValue, fill = Output, group = paste0(Value1, Value2)))+
              scale_fill_viridis(guide = "none")+
              facet_grid(. ~ ResponseVariable , scales = "free", labeller = labeller(Variable2 = FLabels3, ResponseVariable = FLabels3))+
              theme_bw()+
              theme(axis.title = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    text = element_text(size = TextSize),
                    plot.margin = MyMargin,
                    plot.background = element_rect(fill = "transparent", color = "transparent"))
            assign(paste0("Plot", PlotNr), MyPlot)
            
            PlotNr <- PlotNr + 1
          }
        }
      } else {
        if(Var2 == maxVar2){
          Myxaxis <- scale_x_continuous(name = "", breaks = unique(HeatResultsTiles$Value1[HeatResultsTiles$Variable1 == Variable1]))
          if(Variable1 == "CrownShape"){
            Myxaxis <- scale_x_continuous(name = "", breaks = unique(HeatResultsTiles$Value1[HeatResultsTiles$Variable1 == Variable1]),
                                          labels = c("1" = "Disk", "2" = "Ellipsoid"))
          }
          if(Resp == "GapSpecies_high"){
            MyPlot <- ggplot(HeatResultsTiles[HeatResultsTiles$Variable1 == Variable1 &
                                                HeatResultsTiles$Variable2 == Variable2 &
                                                HeatResultsTiles$ResponseVariable == Resp, ])+
              geom_polygon(aes(x = XValue, y = YValue, fill = Output, group = paste0(Value1, Value2)))+
              Myxaxis+
              scale_fill_viridis(guide = "none")+
              facet_grid(Variable2 ~ . , scales = "free", labeller = labeller(Variable2 = FLabels3, ResponseVariable = FLabels3))+
              theme_bw()+
              theme(axis.title.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                    text = element_text(size = TextSize),
                    plot.margin = MyMargin,
                    plot.background = element_rect(fill = "transparent", color = "transparent"))
            assign(paste0("Plot", PlotNr), MyPlot)
            
            PlotNr <- PlotNr + 1
          } else {
            if(Resp == "TotalRad"){
              Myyaxis <- scale_y_continuous(name = "", breaks = unique(HeatResultsTiles$Value2[HeatResultsTiles$Variable2 == Variable2]),
                                            labels = function(x) format(x, nsmall = 2))
              if(Variable2 == "CrownShape"){
                Myyaxis <- scale_y_continuous(name = "", breaks = unique(HeatResultsTiles$Value2[HeatResultsTiles$Variable2 == Variable2]),
                                              labels = c("1" = "Disk", "2" = "Ellipsoid"))
              }
              MyPlot <- ggplot(HeatResultsTiles[HeatResultsTiles$Variable1 == Variable1 &
                                                  HeatResultsTiles$Variable2 == Variable2 &
                                                  HeatResultsTiles$ResponseVariable == Resp, ])+
                geom_polygon(aes(x = XValue, y = YValue, fill = Output, group = paste0(Value1, Value2)))+
                Myxaxis+
                scale_y_continuous(name = "", breaks = unique(HeatResultsTiles$Value2[HeatResultsTiles$Variable2 == Variable2]),
                                   labels = function(x) format(x, nsmall = 2))+
                scale_fill_viridis(guide = "none")+
                theme_bw()+
                theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                      text = element_text(size = TextSize),
                      plot.margin = MyMargin,
                      plot.background = element_rect(fill = "transparent", color = "transparent"))
              assign(paste0("Plot", PlotNr), MyPlot)
              
              PlotNr <- PlotNr + 1
            } else {
              MyPlot <- ggplot(HeatResultsTiles[HeatResultsTiles$Variable1 == Variable1 &
                                                  HeatResultsTiles$Variable2 == Variable2 &
                                                  HeatResultsTiles$ResponseVariable == Resp, ])+
                geom_polygon(aes(x = XValue, y = YValue, fill = Output, group = paste0(Value1, Value2)))+
                Myxaxis+
                scale_fill_viridis(guide = "none")+
                theme_bw()+
                theme(axis.title.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                      text = element_text(size = TextSize),
                      plot.margin = MyMargin,
                      plot.background = element_rect(fill = "transparent", color = "transparent"))
              
              if(Resp == "WeightedFull"){
                Myxaxis <- scale_x_continuous(name = c("WeatherStation" = "Latitude", "CrownShape" = "Crown Shape", "CrownTransmissibility" = "Crown Transmissibility", "ForestGapSize" = "Gap Size")[Variable1], 
                                              breaks = unique(HeatResultsTiles$Value1[HeatResultsTiles$Variable1 == Variable1]))
                if(Variable1 == "CrownShape"){
                  Myxaxis <- scale_x_continuous(name = c("WeatherStation" = "Latitude", "CrownShape" = "Crown Shape", "CrownTransmissibility" = "Crown Transmissibility", "ForestGapSize" = "Gap Size")[Variable1], 
                                                breaks = unique(HeatResultsTiles$Value1[HeatResultsTiles$Variable1 == Variable1]),
                                                labels = c("1" = "Disk", "2" = "Ellipsoid"))
                }
                MyPlot <- ggplot(HeatResultsTiles[HeatResultsTiles$Variable1 == Variable1 &
                                                    HeatResultsTiles$Variable2 == Variable2 &
                                                    HeatResultsTiles$ResponseVariable == Resp, ])+
                  geom_polygon(aes(x = XValue, y = YValue, fill = Output, group = paste0(Value1, Value2)))+
                  Myxaxis+
                  scale_fill_viridis(guide = "none")+
                  theme_bw()+
                  theme(axis.title.y = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                        text = element_text(size = TextSize),
                        plot.margin = MyMargin,
                        plot.background = element_rect(fill = "transparent", color = "transparent"))
              }
              assign(paste0("Plot", PlotNr), MyPlot)
              
              PlotNr <- PlotNr + 1
            }
          }
        } else {
          if(Resp == "GapSpecies_high"){
            MyPlot <- ggplot(HeatResultsTiles[HeatResultsTiles$Variable1 == Variable1 &
                                                HeatResultsTiles$Variable2 == Variable2 &
                                                HeatResultsTiles$ResponseVariable == Resp, ])+
              geom_polygon(aes(x = XValue, y = YValue, fill = Output, group = paste0(Value1, Value2)))+
              scale_fill_viridis(guide = "none")+
              facet_grid(Variable2 ~ . , scales = "free", labeller = labeller(Variable2 = FLabels3, ResponseVariable = FLabels3))+
              theme_bw()+
              theme(axis.title = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    text = element_text(size = TextSize),
                    plot.margin = MyMargin,
                    plot.background = element_rect(fill = "transparent", color = "transparent"))
            assign(paste0("Plot", PlotNr), MyPlot)
            
            PlotNr <- PlotNr + 1
          } else {
            if(Resp == "TotalRad"){
              MyPlot <- ggplot(HeatResultsTiles[HeatResultsTiles$Variable1 == Variable1 &
                                                  HeatResultsTiles$Variable2 == Variable2 &
                                                  HeatResultsTiles$ResponseVariable == Resp, ])+
                geom_polygon(aes(x = XValue, y = YValue, fill = Output, group = paste0(Value1, Value2)))+
                scale_y_continuous(name = "", breaks = unique(HeatResultsTiles$Value2[HeatResultsTiles$Variable2 == Variable2]),
                                   labels = function(x) format(x, nsmall = 2))+
                scale_fill_viridis(guide = "none")+
                theme_bw()+
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      text = element_text(size = TextSize),
                      plot.margin = MyMargin,
                      plot.background = element_rect(fill = "transparent", color = "transparent"))
              assign(paste0("Plot", PlotNr), MyPlot)
              
              PlotNr <- PlotNr + 1
            } else {
              MyPlot <- ggplot(HeatResultsTiles[HeatResultsTiles$Variable1 == Variable1 &
                                                  HeatResultsTiles$Variable2 == Variable2 &
                                                  HeatResultsTiles$ResponseVariable == Resp, ])+
                geom_polygon(aes(x = XValue, y = YValue, fill = Output, group = paste0(Value1, Value2)))+
                scale_fill_viridis(guide = "none")+
                theme_bw()+
                theme(axis.title = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      text = element_text(size = TextSize),
                      plot.margin = MyMargin,
                      plot.background = element_rect(fill = "transparent", color = "transparent"))
              assign(paste0("Plot", PlotNr), MyPlot)
              
              PlotNr <- PlotNr + 1
            }
          }
        }
      }
    }
  }
  AlignedPlots <- align_plots(Plot1, Plot10, Plot19, align = "v", axis = "l")
  PlotLine1 <- plot_grid(AlignedPlots[[1]], Plot2, Plot3, Plot4, Plot5, Plot6, Plot7, Plot8, Plot9, nrow = 1, rel_widths = c(PlotWLeft, rep(PlotWMid, 7), PlotWRight))
  PlotLine2 <- plot_grid(AlignedPlots[[2]], Plot11, Plot12, Plot13, Plot14, Plot15, Plot16, Plot17, Plot18, nrow = 1, rel_widths = c(PlotWLeft, rep(PlotWMid, 7), PlotWRight))
  PlotLine3 <- plot_grid(AlignedPlots[[3]], Plot20, Plot21, Plot22, Plot23, Plot24, Plot25, Plot26, Plot27, nrow = 1, rel_widths = c(PlotWLeft, rep(PlotWMid, 7), PlotWRight))
  AllPlots <- plot_grid(PlotLine1, PlotLine2, PlotLine3, nrow = 3, rel_heights = c(PlotHTop, PlotHMid, PlotHBot))
  AllPlots <- plot_grid(AllPlots, MyLegend, nrow = 2, rel_heights = c(1, 0.1))
  # AllPlots <- plot_grid(plot_grid(Plot1, Plot2, Plot3, Plot3, Plot4, Plot5, Plot6, Plot7, Plot8, Plot9, nrow = 1),
  #                       plot_grid(Plot10, Plot11, Plot12, Plot13, Plot14, Plot15, Plot16, Plot17, Plot18, nrow = 1),
  #                       plot_grid(Plot19, Plot20, Plot21, Plot22, Plot23, Plot24, Plot25, Plot26, Plot27, nrow = 1),
  #                       nrow = 3)
  assign(paste0("AllPlots_", Variable1), AllPlots)
}

# Let's look at Results:
AllPlots_CrownShape
AllPlots_WeatherStation
AllPlots_CrownTransmissibility
AllPlots_ForestGapSize

# Interactions seem to be limited. Interactions between Crown Shape and Latitude and Crown Shape and Forest Gap Size seem to be limited to Sunhours.
# We conclude that Interactions play only a minor Role in this Experiment.



#################################################################################################################
#### Species Distribution within the Gap ####
#################################################################################################################
# We wanted to take a closer Look to Species Distribution inside the Gap.
# We made a Plot showing Species Distribution and varied only one Input Parameter at a Time. The other Parameters were fixed to:
# - Crown Shape = Ellipsoid
# - Weather Station = Lindenberg
# - Crown Transmissibility = 0.05
# - Forest Gap Size = 10
MyPlotID <- 5   # Plot Number 5

# Latitude
LatData <- data.frame(CrownShape = "A", WeatherStation = "W", CrownTransmissibility = 1, ForestGapSize = 1, 
                      pxcor = 1, pycor = 1, Radiation = 1)[-1, ]
for(WS in unique(Experiments$WeatherStation)){
  Ex <- which(Experiments$CrownShape == "Ellipsoid" & 
                Experiments$WeatherStation == WS & 
                Experiments$CrownTransmissibility == 0.05 &
                Experiments$ForestGapSize == 10 &
                Experiments$PlotID == MyPlotID) 
  MyEx <- Experiments[Ex, ]
  FileName <- paste0("Data/Results_SA_StandModel/Experiment_", Ex, "_", MyEx$CrownShape, "_", MyEx$WeatherStation, "_", 
                     MyEx$CrownTransmissibility, "_", MyEx$ForestGapSize, "_", MyEx$PlotID, ".csv")
  MyData <- read.csv(FileName)
  colnames(MyData) <- c("x", "y", "Rad", "FullL")
  LatData <- rbind(LatData, 
                   data.frame(CrownShape = "Ellipsoid", WeatherStation = WS, CrownTransmissibility = 0.05, 
                              ForestGapSize = 10, pxcor = MyData$x, pycor = MyData$y, Radiation = MyData$Rad))
}

LatData <- merge(LatData, MaxSun, by = "WeatherStation")
LatData$Species <- ifelse(LatData$Radiation / LatData$MaxRad < 0.15, "tolerant", 
                          ifelse(LatData$Radiation / LatData$MaxRad < 0.24, "intermediate", "intolerant"))
LatData$WeatherStation <- factor(LatData$WeatherStation, 
                                 levels = c("Phillip SW Belize", "Lake Charles", "McMinnville", 
                                            "Lindenberg", "Croker river"))

P1 <- ggplot(LatData)+
  geom_tile(aes(x = pxcor, y = pycor, fill = Species))+
  scale_x_continuous(name = "X-Coordinate [m]", breaks = c(-25, 0, 25))+
  scale_y_continuous(name = "Y-Coordinate [m]", breaks = c(-25, 0, 25))+
  scale_fill_manual(values = SpecColors, breaks = c("intolerant", "intermediate", "tolerant"), guide = "none")+
  facet_grid(. ~ WeatherStation, labeller = labeller(WeatherStation = FLabels4))+
  coord_fixed()+
  theme_bw()+
  theme(text = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()); P1


# Crown Transmissibility
CTData <- data.frame(CrownShape = "A", WeatherStation = "W", CrownTransmissibility = 1, ForestGapSize = 1, 
                     pxcor = 1, pycor = 1, Radiation = 1)[-1, ]
for(CT in unique(Experiments$CrownTransmissibility)){
  Ex <- which(Experiments$CrownShape == "Ellipsoid" & 
                Experiments$WeatherStation == "Lindenberg" & 
                Experiments$CrownTransmissibility == CT &
                Experiments$ForestGapSize == 10 &
                Experiments$PlotID == MyPlotID) 
  MyEx <- Experiments[Ex, ]
  FileName <- paste0("Data/Results_SA_StandModel/Experiment_", Ex, "_", MyEx$CrownShape, "_", MyEx$WeatherStation, "_", 
                     MyEx$CrownTransmissibility, "_", MyEx$ForestGapSize, "_", MyEx$PlotID, ".csv")
  MyData <- read.csv(FileName)
  colnames(MyData) <- c("x", "y", "Rad", "FullL")
  CTData <- rbind(CTData, 
                  data.frame(CrownShape = "Ellipsoid", WeatherStation = MyEx$WeatherStation, 
                             CrownTransmissibility = CT, 
                             ForestGapSize = 10, pxcor = MyData$x, pycor = MyData$y, Radiation = MyData$Rad))
}

CTData <- merge(CTData, MaxSun, by = "WeatherStation")
CTData$Species <- ifelse(CTData$Radiation / CTData$MaxRad < 0.15, "tolerant", 
                         ifelse(CTData$Radiation / CTData$MaxRad < 0.24, "intermediate", "intolerant"))
CTData$WeatherStation <- factor(CTData$WeatherStation, 
                                levels = c("Phillip SW Belize", "Lake Charles", "McMinnville", 
                                           "Lindenberg", "Croker river"))

P2 <- ggplot(CTData)+
  geom_tile(aes(x = pxcor, y = pycor, fill = Species))+
  scale_x_continuous(name = "X-Coordinate [m]", breaks = c(-25, 0, 25))+
  scale_y_continuous(name = "Y-Coordinate [m]", breaks = c(-25, 0, 25))+
  scale_fill_manual(values = SpecColors, breaks = c("intolerant", "intermediate", "tolerant"), guide = "none")+
  facet_grid(. ~ CrownTransmissibility, labeller = labeller(CrownTransmissibility = FLabels4))+
  coord_fixed()+
  theme_bw()+
  theme(text = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()); P2


# Forest Gap Size
GSData <- data.frame(CrownShape = "A", WeatherStation = "W", CrownTransmissibility = 1, ForestGapSize = 1, 
                     pxcor = 1, pycor = 1, Radiation = 1)[-1, ]
for(GS in unique(Experiments$ForestGapSize)){
  Ex <- which(Experiments$CrownShape == "Ellipsoid" & 
                Experiments$WeatherStation == "Lindenberg" & 
                Experiments$CrownTransmissibility == 0.05 &
                Experiments$ForestGapSize == GS &
                Experiments$PlotID == MyPlotID) 
  MyEx <- Experiments[Ex, ]
  FileName <- paste0("Data/Results_SA_StandModel/Experiment_", Ex, "_", MyEx$CrownShape, "_", MyEx$WeatherStation, "_", 
                     MyEx$CrownTransmissibility, "_", MyEx$ForestGapSize, "_", MyEx$PlotID, ".csv")
  MyData <- read.csv(FileName)
  colnames(MyData) <- c("x", "y", "Rad", "FullL")
  GSData <- rbind(GSData, 
                  data.frame(CrownShape = "Ellipsoid", WeatherStation = MyEx$WeatherStation, 
                             CrownTransmissibility = 0.05, 
                             ForestGapSize = MyEx$ForestGapSize, pxcor = MyData$x, pycor = MyData$y, 
                             Radiation = MyData$Rad))
}

GSData <- merge(GSData, MaxSun, by = "WeatherStation")
GSData$Species <- ifelse(GSData$Radiation / GSData$MaxRad < 0.15, "tolerant", 
                         ifelse(GSData$Radiation / GSData$MaxRad < 0.24, "intermediate", "intolerant"))
GSData$WeatherStation <- factor(GSData$WeatherStation, 
                                levels = c("Phillip SW Belize", "Lake Charles", "McMinnville", 
                                           "Lindenberg", "Croker river"))

P3 <- ggplot(GSData)+
  geom_tile(aes(x = pxcor, y = pycor, fill = Species))+
  scale_x_continuous(name = "X-Coordinate [m]", breaks = c(-25, 0, 25))+
  scale_y_continuous(name = "Y-Coordinate [m]", breaks = c(-25, 0, 25))+
  scale_fill_manual(values = SpecColors, breaks = c("intolerant", "intermediate", "tolerant"), guide = "none")+
  facet_grid(. ~ ForestGapSize, labeller = labeller(ForestGapSize = FLabels4))+
  coord_fixed()+
  theme_bw()+
  theme(text = element_text(size = 8)); P3


# Forest Gap Size
CSData <- data.frame(CrownShape = "A", WeatherStation = "W", CrownTransmissibility = 1, ForestGapSize = 1, 
                     pxcor = 1, pycor = 1, Radiation = 1)[-1, ]
for(CS in unique(Experiments$CrownShape)){
  Ex <- which(Experiments$CrownShape == CS & 
                Experiments$WeatherStation == "Lindenberg" & 
                Experiments$CrownTransmissibility == 0.05 &
                Experiments$ForestGapSize == 10 &
                Experiments$PlotID == MyPlotID) 
  MyEx <- Experiments[Ex, ]
  FileName <- paste0("Data/Results_SA_StandModel/Experiment_", Ex, "_", MyEx$CrownShape, "_", MyEx$WeatherStation, "_", 
                     MyEx$CrownTransmissibility, "_", MyEx$ForestGapSize, "_", MyEx$PlotID, ".csv")
  MyData <- read.csv(FileName)
  colnames(MyData) <- c("x", "y", "Rad", "FullL")
  CSData <- rbind(CSData, 
                  data.frame(CrownShape = CS, WeatherStation = MyEx$WeatherStation, 
                             CrownTransmissibility = MyEx$CrownTransmissibility, 
                             ForestGapSize = MyEx$ForestGapSize, pxcor = MyData$x, pycor = MyData$y, 
                             Radiation = MyData$Rad))
}

CSData <- merge(CSData, MaxSun, by = "WeatherStation")
CSData$Species <- ifelse(CSData$Radiation / CSData$MaxRad < 0.15, "tolerant", 
                         ifelse(CSData$Radiation / CSData$MaxRad < 0.24, "intermediate", "intolerant"))
CSData$WeatherStation <- factor(CSData$WeatherStation, 
                                levels = c("Phillip SW Belize", "Lake Charles", "McMinnville", 
                                           "Lindenberg", "Croker river"))

P4 <- ggplot(CSData)+
  geom_tile(aes(x = pxcor, y = pycor, fill = Species))+
  scale_x_continuous(name = "X-Coordinate [m]", breaks = c(-25, 0, 25))+
  scale_y_continuous(name = "Y-Coordinate [m]", breaks = c(-25, 0, 25))+
  scale_fill_manual(values = SpecColors, breaks = c("intolerant", "intermediate", "tolerant"), guide = "none")+
  facet_grid(. ~ CrownShape, labeller = labeller(CrownShape = FLabels4))+
  coord_fixed()+
  theme_bw()+
  theme(text = element_text(size = 8)); P4

# Put All Data together to obtain one Plot and adjust Variable Names so that they are Shown in 
LatData$VarChanged <- FLabels4[LatData$WeatherStation]
CTData$VarChanged <- FLabels4[as.character(CTData$CrownTransmissibility)]
GSData$VarChanged <- FLabels4[as.character(GSData$ForestGapSize)]
CSData$VarChanged <- FLabels4[CSData$CrownShape]

AllData <- rbind(LatData, CTData, GSData, CSData)
AllData$VarChanged <- factor(AllData$VarChanged, levels = as.character(FLabels4))

# Make a Plot of all Observed Areas and the Species Distribution Within.
AllPlot <- ggplot(AllData)+
  geom_tile(aes(x = pxcor, y = pycor, fill = Species))+
  scale_x_continuous(name = "X-Coordinate [m]", breaks = c(-25, 0, 25))+
  scale_y_continuous(name = "Y-Coordinate [m]", breaks = c(-25, 0, 25))+
  scale_fill_manual(name = "Shade\ntolerance", values = SpecColors, 
                    breaks = c("intolerant", "intermediate", "tolerant"))+
  facet_wrap(. ~ VarChanged)+
  coord_fixed()+
  theme_bw()+
  theme(text = element_text(size = 8),
        legend.direction = "horizontal"); AllPlot

# Shift the Legend to not take so much Space.
AllPlot <- shift_legend2(AllPlot); AllPlot

