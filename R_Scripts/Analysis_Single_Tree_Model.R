#################################################################################################################
#### Used Packages ####
#################################################################################################################
library(ggplot2)
library(plyr)
library(viridis)
library(scales)

#################################################################################################################
#### General Variables ####
#################################################################################################################




#################################################################################################################
#### Import Data from Sensitivity Analysis ####
#################################################################################################################
SingleTreeData <- data.frame(pxcor = 1, pycor = 1, Latitude = 1, CrownShape = 1, Radiation = 1, CrownTansmissibility = 1)[-1, ]
for(CS in c("Disk", "Ellipsoid")){
  for(lat in c(17.539, 30.125, 45.18, 52.217, 69.28)){
    for(CT in seq(0, 20, 5)){
      ForData <- read.csv(paste0("Data/Results_SA_SingleTree/", lat, "_", CS, "_", CT, ".csv"))
      ForData <- ForData[ForData$Radiation != 0, ]
      SingleTreeData <- rbind(SingleTreeData, ForData)
    }
  }
}


#################################################################################################################
#### Plot Shading Patterns ####
#################################################################################################################

# Plot Patterns of Disk-Shadows
ggplot(SingleTreeData[SingleTreeData$CrownShape == "Disk", ])+
  geom_tile(aes(x = pxcor, y = pycor, fill = Radiation), height = 1, width = 1)+
  scale_fill_viridis(name = "Blockend\nRadiation")+
  facet_grid(CrownTransmissibility ~ Latitude)+
  coord_fixed()+
  theme_bw()

# Plot Patterns of Ellipsoid-Shadows
ggplot(SingleTreeData[SingleTreeData$CrownShape == "Ellipsoid", ])+
  geom_tile(aes(x = pxcor, y = pycor, fill = Radiation), height = 1, width = 1)+
  scale_fill_viridis(name = "Blockend\nRadiation")+
  facet_grid(CrownTransmissibility ~ Latitude)+
  coord_fixed()+
  theme_bw()

#################################################################################################################
#### Data Analysis ####
#################################################################################################################
# We want to know how Shading is distributed. 
# We first highlight the top most shaded Patches. 
# For this, we order Patches by the Amount of Radiation the Tree Blocked from each Patch.
SingleTreeData <- SingleTreeData[order(SingleTreeData$Latitude, SingleTreeData$CrownShape, SingleTreeData$CrownTransmissibility, -SingleTreeData$Radiation), ]
# And we calculate the cumulative sum so that we know how much is blocked by the first X Patches.
SingleTreeData <- ddply(SingleTreeData, ~ Latitude + CrownShape + CrownTransmissibility, transform, RadProp = cumsum(Radiation) / sum(Radiation))
# For Visualization, we round to the next 0.1.
SingleTreeData$RadRound <- ceiling(SingleTreeData$RadProp / 0.1) * 0.1

# Now let's see the Results (but to make it easier to compare, only use 3 Latitudes and a Crown Transmissibility of 0)
P1 <- ggplot(SingleTreeData[SingleTreeData$Latitude %in% c(17.539, 45.18, 69.28) & 
                        SingleTreeData$CrownTransmissibility %in% c(0) &
                        SingleTreeData$CrownShape %in% c("Ellipsoid", "Disk") & SingleTreeData$pycor != -200, ])+
  geom_tile(aes(x = pxcor, y = pycor, fill = RadRound), height = 1, width = 1)+
  annotate("point", x = 0, y = 0, color = "black", size = 1.5, shape = 21, stroke = 1, fill = "grey80")+
  scale_x_continuous(name = "X-Coordinate [m]", limits = c(-201, 201))+
  scale_y_continuous(name = "Y-Coordinate [m]", limits = c(-201, 201))+
  scale_fill_viridis(name = "Proportion of\nshading")+
  facet_grid(CrownShape ~ Latitude, labeller = labeller(Latitude = c("17.539" = "Latitude 17.539", "45.18" = "Latitude 45.180", "69.28" = "Latitude 69.280")))+
  coord_fixed()+
  theme_bw()+
  theme(text = element_text(size = 8)); P1


# Now that we visualized everything, let's look at the Numbers and calculte The exact Number of Shaded Patches for each Proportion.
RadArea <- data.frame(Proportion = 1, nPatch = 1, CrownShape = "a", CrownTransmissibility = 1, Latitude = 1)[-1, ]
for(CS in c("Disk", "Ellipsoid")){
  for(lat in c(17.539, 30.125, 45.18, 52.217, 69.28)){
    for(CT in seq(0, 20, 5)){
      for(Prop in seq(0, 1, by = 0.1)){
        RadArea <- rbind(RadArea, 
                         data.frame(Proportion = Prop, 
                                    nPatch = nrow(SingleTreeData[SingleTreeData$Latitude == lat &
                                                                   SingleTreeData$CrownShape == CS &
                                                                   SingleTreeData$CrownTransmissibility == CT &
                                                                   SingleTreeData$RadProp <= Prop, ]), 
                                    CrownShape = CS, 
                                    CrownTransmissibility = CT, 
                                    Latitude = lat))
      }
    }
  }
}

# We see, that Ellipsoid Crowns shade more Patches.
P3 <- ggplot(RadArea[RadArea$CrownTransmissibility == 0, ])+
  geom_line(aes(x = Proportion, y = nPatch, color = CrownShape, group = CrownShape), linewidth = 1)+
  scale_x_continuous(name = "Proportion of shaded Area from total shaded Area [-]", 
                     limits = c(0, 1.1), breaks = c(0, 0.25, 0.5, 0.75, 1))+
  scale_y_continuous(name = "Number of shaded Grid Cells [n]")+
  scale_color_manual(name = "Crown Shape", values = c(viridis(10, option = "A")[4], 
                                                      viridis(10, option = "A")[8]))+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  facet_grid(. ~ Latitude,
             labeller = labeller(Latitude = c("17.539" = "Latitude 17.539", 
                                              "30.125" = "Latitude 30.125",
                                              "45.18" = "Latitude 45.180", 
                                              "52.217" = "Latitude 52.217",
                                              "69.28" = "Latitude 69.280"))); P3


# Where does the Tree shade the Ground? Is Shading mostly located North or maybe East and West?
# We define an Orientation to each Patch, relative to the Tree which is at 0/0.
SingleTreeData$Orientation <- ifelse(SingleTreeData$pycor <= SingleTreeData$pxcor &
                                       SingleTreeData$pycor * -1 < SingleTreeData$pxcor, "East", 
                                     ifelse(SingleTreeData$pycor <= SingleTreeData$pxcor * -1 &
                                              SingleTreeData$pycor < SingleTreeData$pxcor, "South", 
                                            ifelse(SingleTreeData$pycor < SingleTreeData$pxcor * -1 &
                                                     SingleTreeData$pycor >= SingleTreeData$pxcor, "West", "North")))

# It looks like this:
ggplot(SingleTreeData[SingleTreeData$Latitude %in% c(17.539) & 
                        SingleTreeData$CrownTransmissibility %in% c(0) &
                        SingleTreeData$CrownShape %in% c("Ellipsoid"), ])+
  geom_tile(aes(x = pxcor, y = pycor, fill = Orientation))+
  coord_fixed()

# And for each Orientation, we calculate the Amount of Blocked Radiation.
BlockedDirection <- ddply(SingleTreeData, ~ Latitude + CrownShape + CrownTransmissibility + Orientation, summarise, 
                          Blocked = sum(Radiation))
BlockedDirection$Orientation <- factor(BlockedDirection$Orientation, levels = c("North", "East", "South", "West"))
BlockedDirection$angle <- ifelse(BlockedDirection$Orientation == "East", 270, 
                                 ifelse(BlockedDirection$Orientation == "West", 90, 0))

# We see that Shading occurs mostly North at high Latitudes and mostly East and West near the Equator.
P2 <- ggplot(BlockedDirection[BlockedDirection$CrownTransmissibility == 0, ])+
  geom_col(aes(x = Orientation, y = Blocked, fill = Orientation), width = 1)+
  annotate("text", x = c("North"), y = max(BlockedDirection$Blocked) * 1.05, label = c("North"), angle = 0, vjust = 0, size = 8 / .pt)+
  annotate("text", x = c("East"), y = max(BlockedDirection$Blocked) * 1.05, label = c("East"), angle = 270, vjust = 0, size = 8 / .pt)+
  annotate("text", x = c("South"), y = max(BlockedDirection$Blocked) * 1.05, label = c("South"), angle = 0, vjust = 0, size = 8 / .pt)+
  annotate("text", x = c("West"), y = max(BlockedDirection$Blocked) * 1.05, label = c("West"), angle = 90, vjust = 0, size = 8 / .pt)+
  scale_x_discrete(expand = c(0,0), limits = c("North", "East", "South", "West"))+
  scale_y_continuous(name = "Amount of blocked Radiation Energy", breaks = c(0, 40000000), labels = c("low", "high"))+
  scale_fill_viridis(discrete = TRUE, option = "C")+
  coord_polar(theta = "x", direction = 1, start = pi / -4)+
  facet_grid(CrownShape ~ Latitude, 
             labeller = labeller(Latitude = c("17.539" = "Latitude 17.539", 
                                              "30.125" = "Latitude 30.125",
                                              "45.18" = "Latitude 45.180", 
                                              "52.217" = "Latitude 52.217",
                                              "69.28" = "Latitude 69.280")))+
  theme_bw()+
  theme(text = element_text(size = 8), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank()); P2



# We wanted to see at which distance from the Tree, Shade is falling. 
SingleTreeData$Distance <- sqrt(SingleTreeData$pxcor^2 + SingleTreeData$pycor^2)

# We calculated for the Shading inside a growing Radius.
Distance_Shading <- data.frame(Latitude = 1, CrownShape = "A", CrownTransmissibility = 1, Distance = 1, Radiation = 1)[-1, ]
for(CS in c("Disk", "Ellipsoid")){
  for(lat in c(17.539, 30.125, 45.18, 52.217, 69.28)){
    for(CT in seq(0, 20, 5)){
      for(Disti in seq(5, 200, by = 5)){
        Distance_Shading <- rbind(Distance_Shading, 
                                  data.frame(Latitude = lat,
                                             CrownShape = CS, 
                                             CrownTransmissibility = CT, 
                                             Distance = Disti,
                                             Radiation = sum(
                                               SingleTreeData$Radiation[SingleTreeData$Latitude == lat &
                                                                          SingleTreeData$CrownShape == CS &
                                                                          SingleTreeData$CrownTransmissibility == CT &
                                                                          # SingleTreeData$Distance > (Disti - 1) &
                                                                          SingleTreeData$Distance < Disti]
                                               )))
      }
    }
  }
}

# And we see, that Shading occurs farther from the Tree at high Latitudes and closer to the Tree near the Equator.
P4 <- ggplot(Distance_Shading[Distance_Shading$CrownTransmissibility == 0, ])+
  geom_line(aes(x = Distance, y = Radiation), lwd = 1)+
  scale_x_continuous(name = "Distance from the tree [m]", breaks = c(0, 100, 200))+
  scale_y_continuous(name = "Cumulative blocked Radiation Energy [W]", labels = comma)+
  facet_grid(CrownShape ~ Latitude,
             labeller = labeller(Latitude = c("17.539" = "Latitude 17.539", 
                                              "30.125" = "Latitude 30.125",
                                              "45.18" = "Latitude 45.180", 
                                              "52.217" = "Latitude 52.217",
                                              "69.28" = "Latitude 69.280")))+
  theme_bw()+
  theme(text = element_text(size = 8), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)); P4






