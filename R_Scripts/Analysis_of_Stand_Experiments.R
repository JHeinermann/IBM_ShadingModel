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
#### Selfmade Functions ####
#################################################################################################################
# Get path of this script. Used for github if you don't work with projects.
GetFile <- function(FileName){
  paste0(sub(sub(".*\\/", "", rstudioapi::getSourceEditorContext()$path), "", rstudioapi::getSourceEditorContext()$path), FileName)
}

between <- function(x, Val1, Val2){
  if(Val1 > Val2){
    Val3 <- Val2
    Val2 <- Val1
    Val1 <- Val3
  }
  x > Val1 & x <= Val2
}

Textwidth <- 6.47699

#################################################################################################################
#### Functions from Websites  ####
#################################################################################################################
# https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}

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
#### Read Data  ####
#################################################################################################################
Weighted_Rad <- read.table(file = GetFile("DataAnalyis/Weighted_Rad.csv"), header = TRUE)
Weighted_Full <- read.table(file = GetFile("DataAnalyis/Weighted_Full.csv"), header = TRUE)
TotalRad <- read.table(file = GetFile("DataAnalyis/TotalRad.csv"), header = TRUE)
TotalFull <- read.table(file = GetFile("DataAnalyis/TotalFull.csv"), header = TRUE)
LightProp <- read.table(file = GetFile("DataAnalyis/LightProp.csv"), header = TRUE)
FullSProp <- read.table(file = GetFile("DataAnalyis/FullSProp.csv"), header = TRUE)
GapInter <- read.table(file = GetFile("DataAnalyis/GapInter.csv"), header = TRUE)
GapSpecies <- read.table(file = GetFile("DataAnalyis/GapSpecies.csv"), header = TRUE)




# Calculate the following:
# - Weighted Centroid
# - Total Radiation in Observation Area
# - Number of Hours with full sunlight (total)
# - Weighted Centroid
# - Size of 10 %, ... Radiation Area
# - Smooth Data and:
# -> Calculate North-South Line
# -> Calculate Species Distribution


# Data Frame containing all conducted Experiments.
Experiments <- expand.grid(CrownShape = c("Disk", "Ellipsoid"), 
                           WeatherStation = c("Phillip SW Belize", "Lake Charles", "McMinnville", "Lindenberg", "Croker river"),
                           CrownTransmissibility = c(20, 15, 10, 5, 0) / 100,
                           ForestGapSize = c(5, 10, 15, 20, 25),
                           PlotID = 1:10)

# Maximum Sun Energy at each Plot:
MaxSun <- data.frame(WeatherStation = c("Phillip SW Belize", "Lake Charles", "McMinnville", "Lindenberg", "Croker river"),
                     MaxRad = c(1225497.8173469815, 1244507.4176427615, 1530820.8266382422, 1412842.3938802604, 1713089.8263555742))

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

# Calculate Smoothing Polygon (needed for Edge Correction)
n <- 1
PolyX <- c()
PolyY <- c()
for(i in c(-50:50, 50:-50)){
  PolyX[n] <- i
  PolyY[n] <- sin((90 - (asin(i / 50) * 180 / pi)) * pi / 180) * 50.01 * ifelse(n <= 101, -1, 1)
  n <- n + 1
}


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

#################################################################################################################
#### Save Data ####
#################################################################################################################
write.table(Weighted_Rad, file = GetFile("DataAnalyis/Weighted_Rad.csv"), row.names = FALSE)
write.table(Weighted_Full, file = GetFile("DataAnalyis/Weighted_Full.csv"), row.names = FALSE)
write.table(TotalRad, file = GetFile("DataAnalyis/TotalRad.csv"), row.names = FALSE)
write.table(TotalFull, file = GetFile("DataAnalyis/TotalFull.csv"), row.names = FALSE)
write.table(LightProp, file = GetFile("DataAnalyis/LightProp.csv"), row.names = FALSE)
write.table(FullSProp, file = GetFile("DataAnalyis/FullSProp.csv"), row.names = FALSE)
write.table(GapInter, file = GetFile("DataAnalyis/GapInter.csv"), row.names = FALSE)
write.table(GapSpecies, file = GetFile("DataAnalyis/GapSpecies.csv"), row.names = FALSE)




#################################################################################################################
#### Visualization ####
#################################################################################################################
# Facet Labels:
FLabels <- c("Phillip SW Belize" = "Latitude 17.539", "Lake Charles" = "Latitude 30.125", "McMinnville" = "Latitude 45.18", "Lindenberg" = "Latitude 52.217", "Croker river" = "Latitude 69.28",
             "5" = "Forest Gap Size: 5m", "10" = "Forest Gap Size: 10m", "15" = "Forest Gap Size: 15m", "20" = "Forest Gap Size: 20m", "25" = "Forest Gap Size: 25m")


# Latitudes of Stations:
StationLats <- data.frame(WeatherStation = c("Phillip SW Belize", "Lake Charles", "McMinnville", "Lindenberg", "Croker river"),
                          Latitude = c(17.539, 30.125, 45.18, 52.217, 69.28))

# Colors for Weather Stations:



# Weighted Centroid of Radiation Energy Sum.
Weighted_Rad2 <- ddply(Weighted_Rad, ~ CrownShape + WeatherStation + CrownTransmissibility + ForestGapSize, summarise,
                       CentroidX = mean(CentroidX),
                       CentroidY = mean(CentroidY))

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
NNumbers <- c("Phillip SW Belize" = 17.539, "Lake Charles" = 30.125, "McMinnville" = 45.18, "Lindenberg" = 52.217, "Croker river" = 69.28, "Disk" = 1, "Ellipsoid" = 2)

Experiments2 <- expand.grid(CrownShape = c("Disk", "Ellipsoid"), 
                            WeatherStation = c("Phillip SW Belize", "Lake Charles", "McMinnville", "Lindenberg", "Croker river"),
                            CrownTransmissibility = c(20, 15, 10, 5, 0) / 100,
                            ForestGapSize = c(5, 10, 15, 20, 25))

SA_Results <- data.frame(ExplanatoryVariable = "A", ResponseVariable = "B", Input = 1, Output = 1)[-1, ]
for(InVar in 1:4){
  ExVar <- colnames(Experiments2)[InVar]
  for(InVal in unique(Experiments2[, InVar])){
    MyVals <- which(Weighted_Rad[, ExVar] == InVal)
    InVal <- ifelse(is.numeric(InVal), InVal,
                    as.numeric(NNumbers[InVal]))
    SA_Results <- rbind(SA_Results, 
                        data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "WeightedRad", Input = InVal, 
                                   Output = mean(Weighted_Rad[MyVals, "CentroidY"])))
    SA_Results <- rbind(SA_Results, 
                        data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "WeightedFull", Input = InVal, 
                                   Output = mean(Weighted_Full[MyVals, "CentroidY"])))
    SA_Results <- rbind(SA_Results, 
                        data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "TotalRad", Input = InVal, 
                                   Output = mean(TotalRad[MyVals, "TotalRad"])))
    SA_Results <- rbind(SA_Results, 
                        data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "TotalFull", Input = InVal, 
                                   Output = mean(TotalFull[MyVals, "TotalFull"])))
    SA_Results <- rbind(SA_Results, 
                        data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "LightProp", Input = InVal, 
                                   Output = mean(LightProp[LightProp$ExNum %in% MyVals & LightProp$Proportion == 0.2, "Area"])))
    SA_Results <- rbind(SA_Results, 
                        data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "FullSProp", Input = InVal, 
                                   Output = mean(FullSProp[FullSProp$ExNum %in% MyVals & FullSProp$Proportion == 0.2, "Area"])))
    SA_Results <- rbind(SA_Results, 
                        data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "GapSpecies_high", Input = InVal, 
                                   Output = mean(GapSpecies[GapSpecies$ExNum %in% MyVals & GapSpecies$ShadeTolerance == "high", "Proportion"])))
    SA_Results <- rbind(SA_Results, 
                        data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "GapSpecies_intermediate", Input = InVal, 
                                   Output = mean(GapSpecies[GapSpecies$ExNum %in% MyVals & GapSpecies$ShadeTolerance == "intermediate", "Proportion"])))
    SA_Results <- rbind(SA_Results, 
                        data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "GapSpecies_low", Input = InVal, 
                                   Output = mean(GapSpecies[GapSpecies$ExNum %in% MyVals & GapSpecies$ShadeTolerance == "low", "Proportion"])))
  }
}

SA_Results <- ddply(SA_Results, ~ ResponseVariable, transform, 
                    Exmin = min(Output), Exmax = max(Output))
SA_Results <- ddply(SA_Results, ~ ExplanatoryVariable, transform, 
                    Remin = min(Input), Remax = max(Input))

SA_Results$pResponse <- (1 / (SA_Results$Exmax - SA_Results$Exmin)) * SA_Results$Output + (-1 * (1 / (SA_Results$Exmax - SA_Results$Exmin)) * SA_Results$Exmin)
SA_Results$pExplanatory <- (1 / (SA_Results$Remax - SA_Results$Remin)) * SA_Results$Input + (-1 * (1 / (SA_Results$Remax - SA_Results$Remin)) * SA_Results$Remin)

SA_Results$xmin <- NA
SA_Results$xmax <- NA
SA_Results$ymin <- NA
SA_Results$ymax <- NA
for(i in 1:nrow(SA_Results)){
  MyX <- SA_Results$pExplanatory[i]
  AllX <- sort(unique(SA_Results$pExplanatory[SA_Results$ExplanatoryVariable == SA_Results$ExplanatoryVariable[i]]))
  NX <- which(MyX == AllX)
  if(MyX == min(AllX)){
    SA_Results$xmin[i] <- AllX[NX] - 0.25 / 2
    SA_Results$xmax[i] <- AllX[NX] + (AllX[NX + 1] - AllX[NX]) / 2
  } else {
    if(MyX == max(AllX)){
      SA_Results$xmin[i] <- AllX[NX] - (AllX[NX] - AllX[NX - 1]) / 2
      SA_Results$xmax[i] <- AllX[NX] + 0.25 / 2
    } else {
      SA_Results$xmin[i] <- AllX[NX] - (AllX[NX] - AllX[NX - 1]) / 2
      SA_Results$xmax[i] <- AllX[NX] + (AllX[NX + 1] - AllX[NX]) / 2
    }
  }
  MyExp <- SA_Results$ExplanatoryVariable[i]
  SA_Results$ymin[i] <- which(MyExp == sort(unique(SA_Results$ExplanatoryVariable)))
  SA_Results$ymax[i] <- which(MyExp == sort(unique(SA_Results$ExplanatoryVariable))) + 1
}

ggplot(SA_Results)+
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = pResponse))+
  scale_fill_viridis()+
  facet_wrap(~ ResponseVariable)





FLabels <- c("WeatherStation" = "Latitude\n", "CrownShape" = "Crown Shape\n", "CrownTransmissibility" = "Crown\nTransmissibility", "ForestGapSize" = "Gap Size\n", 
             "TotalRad" = "Total\nRadiation", "TotalFull" = "Total\nSunhours", "WeightedRad" = "Centroid\nRadiation", "WeightedFull" = "Centroid\nSunhours", 
             "LightProp" = "most\nShaded", "FullSProp" = "most\nSunhours", "GapSpecies_low" = "intolerant\nArea",
             "GapSpecies_intermediate" = "intermediate\nArea", "GapSpecies_high" = "tolerant\nArea")
FLevels <- c("TotalRad", "TotalFull", "WeightedRad", "WeightedFull", "LightProp", "FullSProp", "GapSpecies_low", "GapSpecies_intermediate", "GapSpecies_high")
TextSize <- 8
YSpace <- 0.1
SA_Results$ResponseVariable <- factor(SA_Results$ResponseVariable, levels = FLevels)
Plot1 <- ggplot(SA_Results[SA_Results$ExplanatoryVariable == "CrownShape", ])+
  geom_line(aes(x = pExplanatory, y = pResponse))+
  geom_point(aes(x = pExplanatory, y = pResponse))+
  scale_x_continuous(name = "Crown Shape", breaks = c(0, 1), labels = c("Disk", "Ellipsoid"), limits = c(-0.125, 1.125), minor_breaks = NULL)+
  scale_y_continuous(name = "Response Variable", breaks = c(0, 1), labels = c("low", "high"), limits = c(-0.125, 1.125))+
  facet_grid(ResponseVariable ~ ExplanatoryVariable, labeller = labeller(ResponseVariable = FLabels, ExplanatoryVariable = FLabels))+
  theme_bw()+
  theme(text = element_text(size = TextSize),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y = element_blank(),
        axis.text.y = element_text(vjust = c(0, 1)),
        strip.background.y = element_blank(),
        panel.spacing.y = unit(YSpace, "lines"),
        plot.margin = unit( c(0,0,0,0) , units = "lines" ) ); Plot1

Plot2 <- ggplot(SA_Results[SA_Results$ExplanatoryVariable == "WeatherStation", ])+
  geom_line(aes(x = pExplanatory, y = pResponse))+
  geom_point(aes(x = pExplanatory, y = pResponse))+
  scale_x_continuous(name = "Latitude", breaks = unique(SA_Results$pExplanatory[SA_Results$ExplanatoryVariable == "WeatherStation"]), 
                     labels = unique(SA_Results$Input[SA_Results$ExplanatoryVariable == "WeatherStation"]), limits = c(-0.125, 1.125), minor_breaks = NULL)+
  scale_y_continuous(name = "Response Variable", breaks = c(0, 1), labels = c("low", "high"), limits = c(-0.125, 1.125))+
  facet_grid(ResponseVariable ~ ExplanatoryVariable, labeller = labeller(ResponseVariable = FLabels, ExplanatoryVariable = FLabels))+
  theme_bw()+
  theme(text = element_text(size = TextSize),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.y = element_blank(),
        panel.spacing.y = unit(YSpace, "lines"),
        plot.margin = unit( c(0,0,0,0) , units = "lines" ) ); Plot2

Plot3 <- ggplot(SA_Results[SA_Results$ExplanatoryVariable == "CrownTransmissibility", ])+
  geom_line(aes(x = pExplanatory, y = pResponse))+
  geom_point(aes(x = pExplanatory, y = pResponse))+
  scale_x_continuous(name = "Crown Transmissibility", breaks = unique(SA_Results$pExplanatory[SA_Results$ExplanatoryVariable == "CrownTransmissibility"]), 
                     labels = unique(SA_Results$Input[SA_Results$ExplanatoryVariable == "CrownTransmissibility"]), limits = c(-0.125, 1.125), minor_breaks = NULL)+
  scale_y_continuous(name = "Response Variable", breaks = c(0, 1), labels = c("low", "high"), limits = c(-0.125, 1.125))+
  facet_grid(ResponseVariable ~ ExplanatoryVariable, labeller = labeller(ResponseVariable = FLabels, ExplanatoryVariable = FLabels))+
  theme_bw()+
  theme(text = element_text(size = TextSize),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.y = element_blank(),
        panel.spacing.y = unit(YSpace, "lines"),
        plot.margin = unit( c(0,0,0,0) , units = "lines" )); Plot3

Plot4 <- ggplot(SA_Results[SA_Results$ExplanatoryVariable == "ForestGapSize", ])+
  geom_line(aes(x = pExplanatory, y = pResponse))+
  geom_point(aes(x = pExplanatory, y = pResponse))+
  scale_x_continuous(name = "Gap Size", breaks = unique(SA_Results$pExplanatory[SA_Results$ExplanatoryVariable == "ForestGapSize"]), 
                     labels = unique(SA_Results$Input[SA_Results$ExplanatoryVariable == "ForestGapSize"]), limits = c(-0.125, 1.125), minor_breaks = NULL)+
  scale_y_continuous(name = "Response Variable", breaks = c(0, 1), labels = c("low", "high"), limits = c(-0.125, 1.125))+
  facet_grid(ResponseVariable ~ ExplanatoryVariable, labeller = labeller(ResponseVariable = FLabels, ExplanatoryVariable = FLabels))+
  theme_bw()+
  theme(text = element_text(size = TextSize),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(angle = 0),
        panel.spacing.y = unit(YSpace, "lines"),
        plot.margin = unit( c(0,0,0,0) , units = "lines" )); Plot4

AlignedPlots <- align_plots(Plot1, Plot2, Plot3, Plot4, align = "h", axis = "b")

AllPlots <- plot_grid(AlignedPlots[[1]], AlignedPlots[[2]], AlignedPlots[[3]], AlignedPlots[[4]], nrow = 1, rel_widths = c(1.25, 1, 1, 1.4))

ggsave(AllPlots, file = "~/GitHub/ShadingModel/Plots/Stand_Experiment/SingleInfluence.png",
       width = Textwidth, height = Textwidth / 16 * 9, unit = "in", dpi = 300)



ggplot(TotalRad)+
  geom_point(aes(x = CrownTransmissibility, y = TotalRad))+
  geom_smooth(aes(x = CrownTransmissibility, y = TotalRad))




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
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "GapSpecies_high", Input = InVal, 
                                        Mean = mean(MyOut), SD = sd(MyOut)))
    
    MyOut <- GapSpecies[GapSpecies$ShadeTolerance == "intermediate", "Proportion"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    SA_Standardized <- rbind(SA_Standardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "GapSpecies_intermediate", Input = InVal, 
                                        Mean = mean(MyOut), SD = sd(MyOut)))
    
    MyOut <- GapSpecies[GapSpecies$ShadeTolerance == "low", "Proportion"]
    MyOut <- (MyOut - mean(MyOut)) / sd(MyOut)
    MyOut <- MyOut[MyVals]
    SA_Standardized <- rbind(SA_Standardized, 
                             data.frame(ExplanatoryVariable = ExVar, ResponseVariable = "GapSpecies_low", Input = InVal, 
                                        Mean = mean(MyOut), SD = sd(MyOut)))
  }
}
SA_Standardized <- ddply(SA_Standardized, ~ ExplanatoryVariable, transform, 
                    Remin = min(Input), Remax = max(Input))
SA_Standardized$pExplanatory <- (1 / (SA_Standardized$Remax - SA_Standardized$Remin)) * SA_Standardized$Input + (-1 * (1 / (SA_Standardized$Remax - SA_Standardized$Remin)) * SA_Standardized$Remin)



FLabels <- c("WeatherStation" = "Latitude\n", "CrownShape" = "Crown Shape\n", "CrownTransmissibility" = "Crown\nTransmissibility", "ForestGapSize" = "Gap Size\n", 
             "TotalRad" = "Total\nRadiation", "TotalFull" = "Total\nSunhours", "WeightedRad" = "Centroid\nRadiation", "WeightedFull" = "Centroid\nSunhours", 
             "LightProp" = "most\nShaded", "FullSProp" = "most\nSunhours", "GapSpecies_low" = "intolerant\nArea",
             "GapSpecies_intermediate" = "intermediate\nArea", "GapSpecies_high" = "tolerant\nArea")
FLevels <- c("TotalRad", "TotalFull", "WeightedRad", "WeightedFull", "LightProp", "FullSProp", "GapSpecies_low", "GapSpecies_intermediate", "GapSpecies_high")
FLevels <- c("TotalRad", "WeightedRad", "LightProp", "TotalFull", "WeightedFull", "FullSProp", "GapSpecies_low", "GapSpecies_intermediate", "GapSpecies_high")
TextSize <- 8
YSpace <- 0.1
SA_Standardized$ResponseVariable <- factor(SA_Standardized$ResponseVariable, levels = FLevels)
YLimits <- c(-1.5, 1.8)



Plot1 <- ggplot(SA_Standardized[SA_Standardized$ExplanatoryVariable == "CrownShape", ])+
  geom_ribbon(aes(x = pExplanatory, ymin = Mean - SD, ymax = Mean + SD), fill = "grey70")+
  geom_line(aes(x = pExplanatory, y = Mean))+
  geom_point(aes(x = pExplanatory, y = Mean))+
  scale_x_continuous(name = "Crown Shape", breaks = c(0, 1), labels = c("Disk", "Ellipsoid"), limits = c(-0.125, 1.125), minor_breaks = NULL)+
  scale_y_continuous(name = "Response Variable", breaks = c(-1, 1), labels = c("low", "high"))+
  facet_grid(ResponseVariable ~ ExplanatoryVariable, labeller = labeller(ResponseVariable = FLabels, ExplanatoryVariable = FLabels))+
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
  facet_grid(ResponseVariable ~ ExplanatoryVariable, labeller = labeller(ResponseVariable = FLabels, ExplanatoryVariable = FLabels))+
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
  facet_grid(ResponseVariable ~ ExplanatoryVariable, labeller = labeller(ResponseVariable = FLabels, ExplanatoryVariable = FLabels))+
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
  facet_grid(ResponseVariable~ ExplanatoryVariable, labeller = labeller(ResponseVariable = FLabels, ExplanatoryVariable = FLabels))+
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

AlignedPlots <- align_plots(Plot1, Plot2, Plot3, Plot4, align = "h", axis = "b")

AllPlots <- plot_grid(AlignedPlots[[1]], AlignedPlots[[2]], AlignedPlots[[3]], AlignedPlots[[4]], nrow = 1, rel_widths = c(1.25, 1, 1, 1.4)); AllPlots

ggsave(AllPlots, file = "~/GitHub/ShadingModel/Plots/Stand_Experiment/SingleInfluence_Standardized.pdf",
       width = Textwidth, height = Textwidth / 16 * 9, unit = "in", dpi = 300)




ggplot(SA_Standardized)+
  geom_ribbon(aes(x = Input, ymin = Mean - SD, ymax = Mean + SD), fill = "grey70")+
  geom_line(aes(x = Input, y = Mean))+
  geom_point(aes(x = Input, y = Mean))+
  facet_grid(. ~ ExplanatoryVariable, scales = "free_x")+
  theme_bw()

# 
# Weighted_Rad <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                            ForestGapSize = 1, PlotID = 1, CentroidX = 1, CentroidY = 1)[-1, ]
# Weighted_Full <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                             ForestGapSize = 1, PlotID = 1, CentroidX = 1, CentroidY = 1)[-1, ]
# TotalRad <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                        ForestGapSize = 1, PlotID = 1, TotalRad = 1)[-1, ]
# TotalFull <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                         ForestGapSize = 1, PlotID = 1, TotalFull = 1)[-1, ]
# LightProp <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                         ForestGapSize = 1, PlotID = 1, Proportion = 1, Area = 1)[-1, ]
# FullSProp <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                         ForestGapSize = 1, PlotID = 1, Proportion = 1, Area = 1)[-1, ]
# GapInter <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                        ForestGapSize = 1, PlotID = 1, yCoord = 1, Rad = 1)[-1, ]
# GapSpecies <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                          ForestGapSize = 1, PlotID = 1, ShadeTolerance = "A", Proportion = 1)[-1, ]


TotalRad$Latitude <- as.numeric(NNumbers[TotalRad$WeatherStation])
TotalFull$Latitude <- as.numeric(NNumbers[TotalFull$WeatherStation])
Weighted_Rad$Latitude <- as.numeric(NNumbers[Weighted_Rad$WeatherStation])
Weighted_Full$Latitude <- as.numeric(NNumbers[Weighted_Full$WeatherStation])
LightProp$Latitude <- as.numeric(NNumbers[LightProp$WeatherStation])
GapInter$Latitude <- as.numeric(NNumbers[GapInter$WeatherStation])
GapSpecies$Latitude <- as.numeric(NNumbers[GapSpecies$WeatherStation])

summary(lm(TotalRad ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = TotalRad))
summary(lm(TotalFull ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = TotalFull))
summary(lm(CentroidY ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = Weighted_Rad))
summary(lm(CentroidY ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = Weighted_Full))
summary(lm(Area ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = LightProp[LightProp$Proportion == 0.2, ]))
summary(lm(Proportion ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = GapSpecies[GapSpecies$ShadeTolerance == "high", ]))
summary(lm(Proportion ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = GapSpecies[GapSpecies$ShadeTolerance == "intermediate", ]))
summary(lm(Proportion ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = GapSpecies[GapSpecies$ShadeTolerance == "low", ]))

summary(lm(CentroidY ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = Weighted_Rad))
summary(lm(CentroidY ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = Weighted_Rad))
summary(lm(CentroidY ~ CrownShape + Latitude + CrownTransmissibility + ForestGapSize, data = Weighted_Rad))





ggplot(SA_Results)+
  geom_line(aes(x = Input, y = Output))+
  geom_point(aes(x = Input, y = Output))+
  facet_grid(ResponseVariable ~ ExplanatoryVariable, scales = "free")




SA_Tiles <- data.frame(ExPlanatryVariable = "a", ResponseVariable = "a", Input = 1, Output = 1)[-1, ]














# Heat map to plot Interactions:
HeatResults <- data.frame(Variable1 = "a", Value1 = 1, Variable2 = "a", Value2 = 1, ResponseVariable = "a", Output = 1)[-1, ]
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


# FLabels <- c("WeatherStation" = "Latitude\n(Wheather Station)", "CrownShape" = "Crown Shape\n", "CrownTransmissibility" = "Crown\nTransmissibility", "ForestGapSize" = "Gap Size [m]\n", 
#              "TotalRad" = "Total Radiation\n", "TotalFull" = "Total Hours with\nFull Sunlight", "WeightedRad" = "Centroid of\nRadiation Energy", "WeightedFull" = "Centroid of \nFull Sunlight Hours", 
#              "LightProp" = "Size of most\nshaded Area", "FullSProp" = "Size of Area with\nmost Full Sunlight Hours", "GapSpecies_low" = "Area suitable for\nshade intolerant species",
#              "GapSpecies_intermediate" = "Area suitable for\nintermediate shade tolerance", "GapSpecies_high" = "Area suitable for\nshade tolerant species")
FLabels <- c("WeatherStation" = "Latitude\n", "CrownShape" = "Crown Shape\n", "CrownTransmissibility" = "Crown\nTransmissibility", "ForestGapSize" = "Gap Size\n", 
             "TotalRad" = "Total\nRadiation", "TotalFull" = "Total\nSunhours", "WeightedRad" = "Centroid\nRadiation", "WeightedFull" = "Centroid\nSunhours", 
             "LightProp" = "most\nShaded", "FullSProp" = "most\nSunhours", "GapSpecies_low" = "intolerant\nArea",
             "GapSpecies_intermediate" = "intermediate\nArea", "GapSpecies_high" = "tolerant\nArea")
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
  guides(fill = guide_colorbar(title.vjust = 0.8)); PlotLegend
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
            facet_grid(Variable2 ~ ResponseVariable , scales = "free", labeller = labeller(Variable2 = FLabels, ResponseVariable = FLabels))+
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
              facet_grid(. ~ ResponseVariable , scales = "free", labeller = labeller(Variable2 = FLabels, ResponseVariable = FLabels))+
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
              facet_grid(. ~ ResponseVariable , scales = "free", labeller = labeller(Variable2 = FLabels, ResponseVariable = FLabels))+
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
              facet_grid(Variable2 ~ . , scales = "free", labeller = labeller(Variable2 = FLabels, ResponseVariable = FLabels))+
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
              facet_grid(Variable2 ~ . , scales = "free", labeller = labeller(Variable2 = FLabels, ResponseVariable = FLabels))+
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


ggsave(AllPlots_CrownShape, file = "~/GitHub/ShadingModel/Plots/Stand_Experiment/CrownShape.pdf",
       width = Textwidth, height = Textwidth / 16 * 9, unit = "in", dpi = 300)
ggsave(AllPlots_CrownTransmissibility, file = "~/GitHub/ShadingModel/Plots/Stand_Experiment/CrownTransmissibility.pdf",
       width = Textwidth, height = Textwidth / 16 * 9, unit = "in", dpi = 300)
ggsave(AllPlots_ForestGapSize, file = "~/GitHub/ShadingModel/Plots/Stand_Experiment/ForestGapSize.pdf",
       width = Textwidth, height = Textwidth / 16 * 9, unit = "in", dpi = 300)
ggsave(AllPlots_WeatherStation, file = "~/GitHub/ShadingModel/Plots/Stand_Experiment/WeatherStation.pdf",
       width = Textwidth, height = Textwidth / 16 * 9, unit = "in", dpi = 300)


#################################################################################################################
#### Species Distribution within the Gap ####
#################################################################################################################
# Load Data.
FLabels <- c("Phillip SW Belize" = "Latitude: 17.539", "Lake Charles" = "Latitude: 30.125", 
             "McMinnville" = "Latitude: 45.18", "Lindenberg" = "Latitude: 52.217", 
             "Croker river" = "Latitude: 69.28",
             "0" = "Crown Transmissibility: 0 %", "0.05" = "Crown Transmissibility: 5 %", 
             "0.1" = "Crown Transmissibility: 10 %", "0.15" = "Crown Transmissibility: 15 %", 
             "0.2" = "Crown Transmissibility: 20 %",
             "5" = "Forest Gap Size: 5m", "10" = "Forest Gap Size: 10m", "15" = "Forest Gap Size: 15m", 
             "20" = "Forest Gap Size: 20m", "25" = "Forest Gap Size: 25m",
             "Disk" = "Crown Shape: Disk", "Ellipsoid" = "Crown Shape: Ellipsoid")

MaxSun <- data.frame(WeatherStation = c("Phillip SW Belize", "Lake Charles", "McMinnville", "Lindenberg", "Croker river"),
                     MaxRad = c(1225497.8173469815, 1244507.4176427615, 1530820.8266382422, 1412842.3938802604, 1713089.8263555742))

SpecOption <- LETTERS[7]
SpecColors <- c("tolerant" = viridis(10, option = SpecOption)[1],
                "intermediate" = viridis(10, option = SpecOption)[5],
                "intolerant" = viridis(10, option = SpecOption)[8])



MyPlotID <- 5

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
  FileName <- paste0("StandModel/Experiment_", Ex, "_", MyEx$CrownShape, "_", MyEx$WeatherStation, "_", 
                     MyEx$CrownTransmissibility, "_", MyEx$ForestGapSize, "_", MyEx$PlotID, ".csv")
  MyData <- read.csv(GetFile(FileName))
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
  facet_grid(. ~ WeatherStation, labeller = labeller(WeatherStation = FLabels))+
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
  FileName <- paste0("StandModel/Experiment_", Ex, "_", MyEx$CrownShape, "_", MyEx$WeatherStation, "_", 
                     MyEx$CrownTransmissibility, "_", MyEx$ForestGapSize, "_", MyEx$PlotID, ".csv")
  MyData <- read.csv(GetFile(FileName))
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
  facet_grid(. ~ CrownTransmissibility, labeller = labeller(CrownTransmissibility = FLabels))+
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
  FileName <- paste0("StandModel/Experiment_", Ex, "_", MyEx$CrownShape, "_", MyEx$WeatherStation, "_", 
                     MyEx$CrownTransmissibility, "_", MyEx$ForestGapSize, "_", MyEx$PlotID, ".csv")
  MyData <- read.csv(GetFile(FileName))
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
  facet_grid(. ~ ForestGapSize, labeller = labeller(ForestGapSize = FLabels))+
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
  FileName <- paste0("StandModel/Experiment_", Ex, "_", MyEx$CrownShape, "_", MyEx$WeatherStation, "_", 
                     MyEx$CrownTransmissibility, "_", MyEx$ForestGapSize, "_", MyEx$PlotID, ".csv")
  MyData <- read.csv(GetFile(FileName))
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
  facet_grid(. ~ CrownShape, labeller = labeller(CrownShape = FLabels))+
  coord_fixed()+
  theme_bw()+
  theme(text = element_text(size = 8)); P4


LatData$VarChanged <- FLabels[LatData$WeatherStation]
CTData$VarChanged <- FLabels[as.character(CTData$CrownTransmissibility)]
GSData$VarChanged <- FLabels[as.character(GSData$ForestGapSize)]
CSData$VarChanged <- FLabels[CSData$CrownShape]


AllData <- rbind(LatData, CTData, GSData, CSData)
AllData$VarChanged <- factor(AllData$VarChanged, levels = as.character(FLabels))

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

AllPlot <- shift_legend2(AllPlot); AllPlot


ggsave(AllPlot, file = "~/GitHub/ShadingModel/Plots/Stand_Experiment/Species_Shift.pdf",
       width = Textwidth, height = Textwidth, unit = "in", dpi = 300)








#################################################################################################################
#### Sketch of the Design of the Experiment ####
#################################################################################################################
# Load Stand Data:
StandNumber <- 1
FileName <- paste0("StandModel/Experiment_", Ex, "_", MyEx$CrownShape, "_", MyEx$WeatherStation, "_", 
                   MyEx$CrownTransmissibility, "_", MyEx$ForestGapSize, "_", MyEx$PlotID, ".csv")
MyData <- read.csv(GetFile(FileName))
colnames(MyData) <- c("x", "y", "Rad", "FullL")
TreeData <- read.csv(GetFile(paste0("Data/Marks_Reconstructed", StandNumber, ".csv")), sep = "")

# Load Experiment Data
ExNr <- which(Experiments$CrownShape == "Ellipsoid" & 
                Experiments$WeatherStation == "Lindenberg" & 
                Experiments$CrownTransmissibility == 0.05 & 
                Experiments$ForestGapSize == 20 & 
                Experiments$PlotID == StandNumber)
ExData <- read.csv(GetFile(paste0("StandModel/Experiment_", ExNr, "_", "Ellipsoid", "_", "Lindenberg", "_", 0.05, "_", 20, "_", StandNumber, ".csv")))


P_Ex <- ggplot()+
  geom_tile(data = ExData, aes(x = pxcor, y = pycor, fill = Radiation), alpha = 0.8)+
  geom_point(data = TreeData[sqrt(TreeData$x^2 + TreeData$y^2) >= 20, ], aes(x = x, y = y), size = 0.005, color = "grey20")+
  geom_circle(data = data.frame(x0 = 0, y0 = 0, r = 20), aes(x0 = x0, y0 = y0, r = r), color = "black", size = 1)+
  scale_x_continuous(name = "X-Coordinate [m]", expand = c(0, 0))+
  scale_y_continuous(name = "Y-Coordinate [m]", expand = c(0, 0))+
  scale_fill_viridis(guide = "none")+
  coord_fixed()+
  theme_bw()+
  theme(text = element_text(size = 8)); P_Ex

ggsave(P_Ex, file = "~/GitHub/ShadingModel/Plots/Stand_Experiment/ExperimentalDesign.pdf",
       width = Textwidth / 2, height = Textwidth / 2, unit = "in", dpi = 300)















AllPlots_CrownShape
AllPlots_CrownTransmissibility
AllPlots_ForestGapSize
AllPlots_WeatherStation


AllPlots <- plot_grid(plot_grid(Plot1, Plot2, Plot3, Plot3, Plot4, Plot5, Plot6, Plot7, Plot8, Plot9, nrow = 1),
                      plot_grid(Plot10, Plot11, Plot12, Plot13, Plot14, Plot15, Plot16, Plot17, Plot18, nrow = 1),
                      plot_grid(Plot19, Plot20, Plot21, Plot22, Plot23, Plot24, Plot25, Plot26, Plot27, nrow = 1),
                      nrow = 3, align = 'h'); AllPlots


AlignedPlots <- align_plots(Plot1, Plot10, Plot19, align = "v", axis = "l")
PlotLine1 <- plot_grid(AlignedPlots[[1]], Plot2, Plot3, Plot4, Plot5, Plot6, Plot7, Plot8, Plot9, nrow = 1, rel_widths = c(1/4.1, rep(1/5.2, 7), 1/4.7))
PlotLine2 <- plot_grid(AlignedPlots[[2]], Plot11, Plot12, Plot13, Plot14, Plot15, Plot16, Plot17, Plot18, nrow = 1, rel_widths = c(1/4.1, rep(1/5.2, 7), 1/4.7))
PlotLine3 <- plot_grid(AlignedPlots[[3]], Plot20, Plot21, Plot22, Plot23, Plot24, Plot25, Plot26, Plot27, nrow = 1, rel_widths = c(1/4.1, rep(1/5.2, 7), 1/4.7))
plot_grid(PlotLine1, PlotLine2, PlotLine3, nrow = 3)



plot_grid(AlignedPlots[[1]], AlignedPlots[[2]], nrow = 2)


AllPlots <- plot_grid(Plot1, Plot2, Plot3, Plot3, Plot4, Plot5, Plot6, Plot7, Plot8, Plot9,
                      Plot10, Plot11, Plot12, Plot13, Plot14, Plot15, Plot16, Plot17, Plot18,
                      Plot19, Plot20, Plot21, Plot22, Plot23, Plot24, Plot25, Plot26, Plot27,
                      nrow = 3, ncol = 9, align = 'hv', axis = 'tblr')


?plot_grid







ggplot(HeatResultsTiles[HeatResultsTiles$Variable1 == "WeatherStation" &
                          HeatResultsTiles$Variable2 == "CrownShape" &
                          HeatResultsTiles$ResponseVariable == "FullSProp", ])+
  geom_polygon(aes(x = XValue, y = YValue, fill = Output, group = paste0(Value1, Value2)))+
  facet_grid(Variable2 ~ ResponseVariable , scales = "free")

ggplot(HeatResultsTiles[HeatResultsTiles$Variable1 == "WeatherStation", ])+
  geom_polygon(aes(x = XValue, y = YValue, fill = Output, group = paste0(Value1, Value2)))+
  facet_grid(Variable2 ~ ResponseVariable , scales = "free")



sort(unique(c(1, 3, 2, 4, 1, 2, 9)))


p1 <- ggplot(HeatResults[HeatResults$Variable1 == "WeatherStation", ])+
  geom_point(aes(x = Value1, y = Value2, fill = Output))+
  scale_y_continuous(name = "Y")+
  facet_grid(Variable2 ~ . , scales = "free"); p1
  
ylims <- list(c(0, 3), c(0, 0.3), c(0, 30))
scale_inidividual_facet_y_axes(p1, ylims = ylims)


as.numeric(NNumbers[c("Disk", "Disk", "Ellipsoid")])

data.frame(a = 1:10, b = 1:10, c = 1:10, d = 1:10)[, c(1, "c")]


Var1 <- 1
Var2 <- 2



p1$facet$draw_labels


install.packages("devtools")

library(devtools)
devtools::install_github("zeehio/facetscales")







install.packages("rlang")






Experiments3 <- Experiments
Experiments3$CrownShape <- ifelse(Experiments3$CrownShape == "Disk", 1, 2)
Experiments3$Latitude <- NNumbers[Experiments3$WeatherStation]


so <- sobol(model = NULL, X1 = Experiments3$CrownShape,
            X2 = Experiments3$Latitude, order = 2, nboot = 100)

?sobol



# Test case : the non-monotonic Sobol g-function

# The method of sobol requires 2 samples
# (there are 8 factors, all following the uniform distribution on [0,1])
library(boot)
n <- 1000
X1 <- data.frame(matrix(runif(8 * n), nrow = n))
X2 <- data.frame(matrix(runif(8 * n), nrow = n))

# sensitivity analysis
x <- sobol(model = sobol.fun, X1 = X1, X2 = X2, order = 2, nboot = 100)
print(x)
plot(x)


f99 <- fast99(model = NULL, factors = 1, n = 2500,
              q = c("qunif", "qunif", "qunif", "qunif"),
              q.arg = list(list(min = 1, max = 2),
                           list(min = 17.539, max = 69.28),
                           list(min = 0, max = 2),
                           list(min = 5, max = 25)))
f99$X$X1 <- Experiments3$CrownShape
f99$X$X2 <- Experiments3$Latitude
f99$X$X3 <- Experiments3$CrownTransmissibility
f99$X$X4 <- Experiments3$ForestGapSize






x <- fast99(model = NULL, factors = 4, n = 2500,
              q = c("qunif", "qunif", "qunif", "qunif"),
              q.arg = list(list(min = 1, max = 2),
                           list(min = 17.539, max = 69.28),
                           list(min = 0, max = 2),
                           list(min = 5, max = 25)))
x$X$X1 <- rep(Experiments3$CrownShape, 4)
x$X$X2 <- rep(Experiments3$Latitude, 4)
x$X$X3 <- rep(Experiments3$CrownTransmissibility, 4)
x$X$X4 <- rep(Experiments3$ForestGapSize, 4)

tell(x, rep(Weighted_Rad$CentroidY, 4))

y <- Weighted_Rad$CentroidY



id <- deparse(substitute(x))


if (! is.null(y)) {
  x$y <- y
} else if (is.null(x$y)) {
  stop("y not found")
}

p <- ncol(x$X)
n <- length(x$s)

V <- numeric(p)
D1 <- numeric(p)
Dt <- numeric(p)

for (i in 1 : p) {
  l <- seq((i - 1) * n + 1, i * n)
  f <- fft(x$y[l], inverse = FALSE)
  Sp <- ( Mod(f[2 : (n / 2)]) / n )^2
  V[i] <- 2 * sum(Sp)
  D1[i] <- 2 * sum(Sp[(1 : x$M) * x$omega[1]])
  Dt[i] <- 2 * sum(Sp[1 : (x$omega[1] / 2)])
}

x$V <- V
x$D1 <- D1
x$Dt <- Dt
assign(id, x, parent.frame())



f99 <- fast99(model = NULL, factors = 1, n = 2500,
              q = c("qunif"),
              q.arg = list(list(min = 1, max = 2)))
f99$X$X1 <- Experiments3$Latitude
f99$X$X1 <- Experiments3$CrownShape
f99$X$X1 <- Experiments3$CrownTransmissibility
f99$X$X4 <- Experiments3$ForestGapSize

tell(f99, Weighted_Rad[ , c("CentroidX", "CentroidY")])
tell(f99, Weighted_Rad$CentroidY)
plot(f99)



f99$y <- Weighted_Rad$CentroidY


# Test case : the non-monotonic Ishigami function
x <- fast99(model = ishigami.fun, factors = 3, n = 1000,
            q = "qunif", q.arg = list(min = -pi, max = pi))
print(x)
plot(x)



dd <- x$X
?tell

?fast99

GapSpecies[GapSpecies$ExNum %in% MyVals, "Proportion"]
GapSpecies[GapSpecies$ExNum == MyVals & GapSpecies$ShadeTolerance == "intermediate", "Proportion"]


Weighted_Rad

Weighted_Rad_Sens <- Weighted_Rad


InVal %>%
  mutate(InVal = case_when(
    is.numeric(InVal) ~ InVal,
    InVal == "Disk"~ 0,
    TRUE ~ 1
  ))



?FrF2
# implementation example using RNetLogo.
# sim <- function(params) {
#   ...
#   return(criteria)
# }






# Weighted_Rad <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                            ForestGapSize = 1, PlotID = 1, CentroidX = 1, CentroidY = 1)[-1, ]
# Weighted_Full <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                             ForestGapSize = 1, PlotID = 1, CentroidX = 1, CentroidY = 1)[-1, ]
# TotalRad <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                        ForestGapSize = 1, PlotID = 1, TotalRad = 1)[-1, ]
# TotalFull <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                         ForestGapSize = 1, PlotID = 1, TotalFull = 1)[-1, ]
# LightProp <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                         ForestGapSize = 1, PlotID = 1, Proportion = 1, Area = 1)[-1, ]
# FullSProp <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                         ForestGapSize = 1, PlotID = 1, Proportion = 1, Area = 1)[-1, ]
# GapInter <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                        ForestGapSize = 1, PlotID = 1, yCoord = 1, Rad = 1)[-1, ]
# GapSpecies <- data.frame(ExNum = 1, CrownShape = "A", WeatherStation = "A", CrownTransmissibility = 1,
#                          ForestGapSize = 1, PlotID = 1, ShadeTolerance = "A", Proportion = 1)[-1, ]




ggplot(GapSpecies)+
  geom_point(aes(x = ForestGapSize, y = Proportion, color = ShadeTolerance))

GapInter2 <- ddply(GapInter, ~ CrownShape + WeatherStation + CrownTransmissibility + ForestGapSize + yCoord, summarise, Rad = mean(Rad, na.rm = TRUE))

ggplot(GapInter2)+
  geom_line(aes(x = yCoord, y = Rad, group = paste0(WeatherStation, CrownTransmissibility), color = WeatherStation))+
  facet_grid(CrownShape ~ ForestGapSize)


ggplot(FullSProp)+
  geom_point(aes(x = Proportion, y = Area, color = WeatherStation))


ggplot(TotalRad)+
  geom_point(aes(x = ForestGapSize, y = TotalRad))
  scale_x_continuous(limits = c(-50, 50))+
  scale_y_continuous(limits = c(-50, 50))+
  coord_fixed()

ggplot(Weighted_Full)+
  geom_point(aes(x = CentroidX, y = CentroidY))+
  scale_x_continuous(limits = c(-50, 50))+
  scale_y_continuous(limits = c(-50, 50))+
  coord_fixed()
  


read.csv(GetFile("StandModel/Experiment_1_Disk_Phillip SW Belize_0.2_5_1.csv"))



LRC <- function(PPFD, ShadeTolerance = "intermediate"){
  if(ShadeTolerance == "intolerant"){
    A_sat <- 10.53
    K <- 246
    R_d <- 1.27
    out <- ((A_sat * PPFD) / (K + PPFD)) - R_d
  }
  if(ShadeTolerance == "intermediate"){
    A_sat <- 3.88
    K <- 252
    R_d <- 0.62
    out <- ((A_sat * PPFD) / (K + PPFD)) - R_d
  }
  if(ShadeTolerance == "tolerant"){
    A_sat <- 1.78
    K <- 146
    R_d <- 0.3
    out <- ((A_sat * PPFD) / (K + PPFD)) - R_d
  }
  out
}

LCP <- function(ShadeTolerance = "intermediate"){
  if(ShadeTolerance == "intolerant"){
    A_sat <- 10.53
    K <- 246
    R_d <- 1.27
    out <- (K * R_d) / (A_sat - R_d)
  }
  if(ShadeTolerance == "intermediate"){
    A_sat <- 3.88
    K <- 252
    R_d <- 0.62
    out <- (K * R_d) / (A_sat - R_d)
  }
  if(ShadeTolerance == "tolerant"){
    A_sat <- 1.78
    K <- 146
    R_d <- 0.62
    out <- (K * R_d) / (A_sat - R_d)
  }
  out
}


LCP(ShadeTolerance = "tolerant")





ggplot()+
  geom_function(fun = LRC, args = list(ShadeTolerance = "tolerant"), color = "red", size = 1.2)+
  geom_function(fun = LRC, args = list(ShadeTolerance = "intermediate"), color = "yellow", size = 1.2)+
  geom_function(fun = LRC, args = list(ShadeTolerance = "intolerant"), color = "green", size = 1.2)+
  scale_x_continuous(limits = c(0, 400))







LRC <- function(PPFD, ShadeTolerance = "intermediate"){
  if(ShadeTolerance == "intolerant"){
    A_sat <- 4.08
    K <- 85
    R_d <- 0.69
    out <- ((A_sat * PPFD) / (K + PPFD)) - R_d
  }
  if(ShadeTolerance == "intermediate"){
    A_sat <- 2.38
    K <- 121
    R_d <- 0.36
    out <- ((A_sat * PPFD) / (K + PPFD)) - R_d
  }
  if(ShadeTolerance == "tolerant"){
    A_sat <- 2.72
    K <-  117
    R_d <- 0.36
    out <- ((A_sat * PPFD) / (K + PPFD)) - R_d
  }
  out
}




LRC <- function(PPFD, ShadeTolerance = "intermediate"){
  if(ShadeTolerance == "intolerant"){
    A <- 2.24
    B <- -1.136
    C <- 0.08
    out <- (A * (1 - exp(B * (PPFD - C))))
  }
  if(ShadeTolerance == "intermediate"){
    A <- 1.371
    B <- -2.227
    C <- 0.05
    out <- (A * (1 - exp(B * (PPFD - C))))
  }
  if(ShadeTolerance == "tolerant"){
    A <- 1
    B <- -4.64
    C <- 0.05
    out <- (A * (1 - exp(B * (PPFD - C))))
  }
  out
}


LRC <- function(x, A, B, C){
  (A * (1 - exp(B * (x - C))))
}

ggplot()+
  geom_function(fun = LRC, args = list(A = 2, B = -2, C = 0.03), color = "red", size = 1.2)+
  geom_function(fun = LRC, args = list(A = 2, B = -2, C = 0.05), color = "yellow", size = 1.2)+
  geom_function(fun = LRC, args = list(A = 2, B = -2, C = 0.07), color = "green", size = 1.2)+
  scale_x_continuous(limits = c(0, 1))

ggplot()+
  geom_function(fun = LRC, args = list(A = 2, B = -1, C = 0.05), color = "red", size = 1.2)+
  geom_function(fun = LRC, args = list(A = 2, B = -2, C = 0.05), color = "yellow", size = 1.2)+
  geom_function(fun = LRC, args = list(A = 2, B = -3, C = 0.05), color = "green", size = 1.2)+
  scale_x_continuous(limits = c(0, 1))

ggplot()+
  geom_function(fun = LRC, args = list(A = 1, B = -2, C = 0.05), color = "red", size = 1.2)+
  geom_function(fun = LRC, args = list(A = 2, B = -2, C = 0.05), color = "yellow", size = 1.2)+
  geom_function(fun = LRC, args = list(A = 3, B = -2, C = 0.05), color = "green", size = 1.2)+
  scale_x_continuous(limits = c(0, 1))


ggplot()+
  geom_function(fun = LRC, args = list(A = 1, B = -1.4, C = 0.03), color = "red", size = 1.2)+
  geom_function(fun = LRC, args = list(A = 1, B = -2, C = 0.05), color = "yellow", size = 1.2)+
  geom_function(fun = LRC, args = list(A = 1, B = -2.5, C = 0.08), color = "green", size = 1.2)+
  geom_vline(xintercept = c(0.12, 0.24))+
  scale_x_continuous(limits = c(0, 1))




ggplot()+
  geom_function(fun = LRC, args = list(ShadeTolerance = "tolerant"), color = "red", size = 1.2)+
  geom_function(fun = LRC, args = list(ShadeTolerance = "intermediate"), color = "yellow", size = 1.2)+
  geom_function(fun = LRC, args = list(ShadeTolerance = "intolerant"), color = "green", size = 1.2)+
  scale_x_continuous(limits = c(0, 1))



Ix <- (ln((A2 - A1) / (A1 * e^(B2 * (x - C2)))) / B1) + C1

x = (ln((D - A) / (A * e^(E * (x - F)))) / B) + C










