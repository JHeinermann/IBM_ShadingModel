library(ggplot2)
library(plyr)
library(viridis)
library(scales)



GetFile <- function(FileName){
  paste0(sub(sub(".*\\/", "", rstudioapi::getSourceEditorContext()$path), "", rstudioapi::getSourceEditorContext()$path), FileName)
}


Textwidth <- 6.47699


SingleTreeData <- data.frame(pxcor = 1, pycor = 1, Latitude = 1, CrownShape = 1, Radiation = 1, CrownTansmissibility = 1)[-1, ]
for(CS in c("Disk", "Ellipsoid")){
  for(lat in c(17.539, 30.125, 45.18, 52.217, 69.28)){
    for(CT in seq(0, 20, 5)){
      ForData <- read.csv(GetFile(paste0(lat, "_", CS, "_", CT, ".csv")))
      ForData <- ForData[ForData$Radiation != 0, ]
      SingleTreeData <- rbind(SingleTreeData, ForData)
    }
  }
}




ggplot(SingleTreeData[SingleTreeData$CrownShape == "Disk", ])+
  geom_tile(aes(x = pxcor, y = pycor, fill = Radiation), height = 1, width = 1)+
  facet_grid(CrownTransmissibility ~ Latitude)+
  coord_fixed()+
  theme_bw()


SingleTreeData <- SingleTreeData[order(SingleTreeData$Latitude, SingleTreeData$CrownShape, SingleTreeData$CrownTransmissibility, -SingleTreeData$Radiation), ]
SingleTreeData <- ddply(SingleTreeData, ~ Latitude + CrownShape + CrownTransmissibility, transform, RadProp = cumsum(Radiation) / sum(Radiation))
SingleTreeData$RadRound <- ceiling(SingleTreeData$RadProp / 0.1) * 0.1


Ellipses <- SingleTreeData[SingleTreeData$CrownShape == "Ellipsoid", ]


ggplot()+
  geom_tile(data = Ellipses[Ellipses$RadProp < 0.1, ], aes(x = pxcor, y = pycor), fill = viridis(10)[1], height = 1, width = 1)+
  geom_tile(data = Ellipses[Ellipses$RadProp >= 0.1 & Ellipses$RadProp < 0.2, ], aes(x = pxcor, y = pycor), fill = viridis(10)[2], height = 1, width = 1)+
  geom_tile(data = Ellipses[Ellipses$RadProp >= 0.2 & Ellipses$RadProp < 0.3, ], aes(x = pxcor, y = pycor), fill = viridis(10)[3], height = 1, width = 1)+
  geom_tile(data = Ellipses[Ellipses$RadProp >= 0.3 & Ellipses$RadProp < 0.4, ], aes(x = pxcor, y = pycor), fill = viridis(10)[4], height = 1, width = 1)+
  geom_tile(data = Ellipses[Ellipses$RadProp >= 0.4 & Ellipses$RadProp < 0.5, ], aes(x = pxcor, y = pycor), fill = viridis(10)[5], height = 1, width = 1)+
  geom_tile(data = Ellipses[Ellipses$RadProp >= 0.5 & Ellipses$RadProp < 0.6, ], aes(x = pxcor, y = pycor), fill = viridis(10)[6], height = 1, width = 1)+
  geom_tile(data = Ellipses[Ellipses$RadProp >= 0.6 & Ellipses$RadProp < 0.7, ], aes(x = pxcor, y = pycor), fill = viridis(10)[7], height = 1, width = 1)+
  geom_tile(data = Ellipses[Ellipses$RadProp >= 0.7 & Ellipses$RadProp < 0.8, ], aes(x = pxcor, y = pycor), fill = viridis(10)[8], height = 1, width = 1)+
  geom_tile(data = Ellipses[Ellipses$RadProp >= 0.8 & Ellipses$RadProp < 0.9, ], aes(x = pxcor, y = pycor), fill = viridis(10)[9], height = 1, width = 1)+
  geom_tile(data = Ellipses[Ellipses$RadProp >= 0.9 & Ellipses$RadProp <= 1, ], aes(x = pxcor, y = pycor), fill = viridis(10)[10], height = 1, width = 1)+
  facet_grid(CrownTransmissibility ~ Latitude)+
  coord_fixed()+
  theme_bw()



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

ggsave(plot = P1, file = "~/GitHub/ShadingModel/Plots/SingleTree_Experiment/Shading_Lat_CrownShape.pdf", 
       width = Textwidth, height = Textwidth / 1.8, dpi = 300)







TotalShaded <- ddply(SingleTreeData, ~ Latitude + CrownShape + CrownTransmissibility, summarise, TotalRad = length(Radiation))
ggplot(TotalShaded[TotalShaded$CrownTransmissibility == 0, ])+
  geom_line(aes(x = Latitude, y = TotalRad))+
  geom_point(aes(x = Latitude, y = TotalRad))+
  facet_grid(. ~ CrownShape)


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

ggsave(plot = P3, file = "~/GitHub/ShadingModel/Plots/SingleTree_Experiment/Shade_Area_Lat.pdf", 
       width = Textwidth, height = Textwidth / 2.3, dpi = 300)



SingleTreeData$Orientation <- ifelse(SingleTreeData$pycor <= SingleTreeData$pxcor &
                                       SingleTreeData$pycor * -1 < SingleTreeData$pxcor, "East", 
                                     ifelse(SingleTreeData$pycor <= SingleTreeData$pxcor * -1 &
                                              SingleTreeData$pycor < SingleTreeData$pxcor, "South", 
                                            ifelse(SingleTreeData$pycor < SingleTreeData$pxcor * -1 &
                                                     SingleTreeData$pycor >= SingleTreeData$pxcor, "West", "North")))

BlockedDirection <- ddply(SingleTreeData, ~ Latitude + CrownShape + CrownTransmissibility + Orientation, summarise, 
                          Blocked = sum(Radiation))
BlockedDirection$Orientation <- factor(BlockedDirection$Orientation, levels = c("North", "East", "South", "West"))
BlockedDirection$angle <- ifelse(BlockedDirection$Orientation == "East", 270, 
                                 ifelse(BlockedDirection$Orientation == "West", 90, 0))

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

ggsave(plot = P2, file = "~/GitHub/ShadingModel/Plots/SingleTree_Experiment/Shade_Orientation.pdf", 
       width = Textwidth, height = Textwidth / 2.3, dpi = 300)


SingleTreeData$Distance <- sqrt(SingleTreeData$pxcor^2 + SingleTreeData$pycor^2)

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

ggsave(plot = P4, file = "~/GitHub/ShadingModel/Plots/SingleTree_Experiment/Shade_Distance.pdf", 
       width = Textwidth, height = Textwidth / 2.3, dpi = 300)



?scale_color_viridis

ggplot(SingleTreeData[SingleTreeData$Latitude %in% c(17.539) & 
                        SingleTreeData$CrownTransmissibility %in% c(0) &
                        SingleTreeData$CrownShape %in% c("Ellipsoid"), ])+
  geom_tile(aes(x = pxcor, y = pycor, fill = Orientation))+
  coord_fixed()




for(lat in c(17.539, 30.125, 45.18, 52.217, 69.28)){
  for(CT in seq(0, 100, 25)){
    for(Prop in seq(0, 1, by = 0.01)){
      RadArea <- rbind(RadArea, 
                       data.frame(Proportion = Prop, 
                                  nPatch = nrow(Ellipses), CrownTransmissibility = 1, Latitude = 1)[-1, ])
    }
    ForData <- SingleTreeData[SingleTreeData$Latitude == lat & SingleTreeData$CrownShape == CS & SingleTreeData$CrownTransmissibility == TS, ]
    
    SingleTreeData <- rbind(SingleTreeData, ForData)
  }
}














cumsum(c(1, 2, 3))


for(CS in c("Disk", "Ellipsoid")){
  for(lat in c(17.539, 30.125, 45.18, 52.217, 69.28)){
    for(CT in seq(0, 100, 25)){
      ForData <- SingleTreeData[SingleTreeData$Latitude == lat & SingleTreeData$CrownShape == CS & SingleTreeData$CrownTransmissibility == TS, ]
      
      SingleTreeData <- rbind(SingleTreeData, ForData)
    }
  }
}




dd <- data.frame(a = sample(LETTERS[1:3], size = 1000, replace = TRUE),
                 b = sample(letters[1:3], size = 1000, replace = TRUE),
                 c = runif(1000))


dd[order(a, b, -c), ]
