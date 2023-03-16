# Script to describe Function of Ellipse:

#################################################################################################################
#### Used Packages ####
#################################################################################################################
library(ggplot2)  # Nice Plots
library(ggforce)  # Ellipse Plots


# Format of Pictures
PicForm <- "pdf"

#################################################################################################################
#### Ellipse Functions ####
#################################################################################################################
# Function of simple Ellipse, depending on Radius1 (a) and Radius2 (b).
# Two Functions are necessary in R: One for the top part of the Ellipse (yt) und one for the bottom (yb).
yt <- function(x, a, b){
  (b / a * sqrt(a ^ 2 - x ^ 2))
}

yb <- function(x, a, b){
  -1 * (b / a * sqrt(a ^ 2 - x ^ 2))
}

# The next two Functions (yt2, yb2) follow the same principle as the two Functions above, but are derived from Ellipsoid Crowns. They 
# are basically the Functions as implemented in NetLogo and are derived from Quesada (2021)
yt2 <- function(x, CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth){
  alph <- SolarAzimuth + 180
  u <- cos(alph * pi / 180)
  v <- sin(alph * pi / 180)
  a <- CrownRadius
  b <- sqrt(CrownHeight ^ 2 / (tan(SolarAltitude * pi / 180) ^ 2) + CrownRadius ^ 2)
  p <- b ^ 2 * v ^ 2 + a ^ 2 * u ^ 2
  q <- u * v * (b ^ 2 - a ^ 2)
  r <- b ^ 2 * u ^ 2 + a ^ 2 * v ^ 2
  s <- a ^ 2 * b ^ 2
  (q * x + sqrt((q ^ 2 - r * p) * x ^ 2 + s * p)) / p
}

yb2 <- function(x, CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth){
  alph <- SolarAzimuth + 180
  u <- cos(alph * pi / 180)
  v <- sin(alph * pi / 180)
  a <- CrownRadius
  b <- sqrt(CrownHeight ^ 2 / (tan(SolarAltitude * pi / 180) ^ 2) + CrownRadius ^ 2)
  p <- b ^ 2 * v ^ 2 + a ^ 2 * u ^ 2
  q <- u * v * (b ^ 2 - a ^ 2)
  r <- b ^ 2 * u ^ 2 + a ^ 2 * v ^ 2
  s <- a ^ 2 * b ^ 2
  (q * x - sqrt((q ^ 2 - r * p) * x ^ 2 + s * p)) / p
}

# Here is a Function that calculates the X-Limits of the Shades. This way they can be plotted from one Limit to the other.
limx <- function(CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth){
  alph <- SolarAzimuth + 180
  u <- cos(alph * pi / 180)
  v <- sin(alph * pi / 180)
  a <- CrownRadius
  b <- sqrt(CrownHeight ^ 2 / (tan(SolarAltitude * pi / 180) ^ 2) + CrownRadius ^ 2)
  p <- b ^ 2 * v ^ 2 + a ^ 2 * u ^ 2
  q <- u * v * (b ^ 2 - a ^ 2)
  r <- b ^ 2 * u ^ 2 + a ^ 2 * v ^ 2
  s <- a ^ 2 * b ^ 2
  c(-1 * sqrt(s * p / (r * p - q ^ 2)), sqrt(s * p / (r * p - q ^ 2)))
}

# Import Angle and Radiation Functions
source("R_Scripts/Radiation_and_Angle_Functions.R")

# Function that gives out a data.frame with Polygons of ShadeEllipses
getShadow <- function(x0, y0, CrownRadius, Height, CrownHeight, Hour, DOY, Latitude, npoints = 101){
  outdata <- data.frame(x = 1, y = 1, group = 1, Rad = 1)[-1, ]
  for(i in 1:length(Hour)){
    alph <- SolarAzimuth(Hour[i], DOY, Latitude) + 180
    u <- cosdeg(alph)
    v <- sindeg(alph)
    a <- CrownRadius
    b <- sqrt(CrownHeight ^ 2 / (tan(SolarAltitude(Hour[i], DOY, Latitude) * pi / 180) ^ 2) + CrownRadius ^ 2)
    p <- b ^ 2 * v ^ 2 + a ^ 2 * u ^ 2
    q <- u * v * (b ^ 2 - a ^ 2)
    r <- b ^ 2 * u ^ 2 + a ^ 2 * v ^ 2
    s <- a ^ 2 * b ^ 2
    # Dist <- Height
    Dist <- Height - CrownHeight / 2
    xS <- x0 - (sindeg(SolarAzimuth(Hour[i], DOY, Latitude)) * Dist / tandeg(SolarAltitude(Hour[i], DOY, Latitude)))
    yS <- y0 - (cosdeg(SolarAzimuth(Hour[i], DOY, Latitude)) * Dist / tandeg(SolarAltitude(Hour[i], DOY, Latitude)))
    pm <- s * p / (r * p - q ^ 2)
    pxmin <- xS - sqrt(pm)
    pxmax <- xS + sqrt(pm)
    xin <- seq(pxmin, pxmax, length.out = npoints)
    plusminus <- NA
    for(np in 1:npoints){
      if(((q ^ 2 - r * p) * (xin[np] - xS) ^ 2 + s * p) < 0){
        plusminus[np] <- 0
      }else {
        plusminus[np] <- sqrt((q ^ 2 - r * p) * (xin[np] - xS) ^ 2 + s * p)
      }
    }
    ytop <- yS + ((q * (xin - xS) + plusminus) / p)
    ybot <- yS + ((q * (xin - xS) - plusminus) / p)
    Radiation <- DirectBeam(Hour[i], DOY, Latitude)
    outdata <- rbind(outdata, data.frame(x = c(xin, rev(xin)), 
                                         y = c(ytop, rev(ybot)), 
                                         group = Hour[i], Rad = Radiation))
  }
  outdata
}


# Function to Plot world in R, just nice graphics.
MakeWorld <- function(xmax = 10, ymax = 10, angle = 45){
  sa <- sin(angle * pi / 180)
  ca <- cos(angle * pi / 180)
  
  PlotData <- data.frame(x = 1, y = 1, fill = "", group = 1)[-1, ]
  gr <- 1
  for(xi in 1:xmax){
    PlotData <- rbind(PlotData, data.frame(x = c(xi - 1, xi - 1, xi, xi), y = c(0, -1, -1, 0), fill = "Front", group = gr))
    gr <- gr + 1
  }
  for(yi in 1:ymax){
    xstart <- sa * (yi - 1)
    ystart <- ca * (yi - 1)
    for(xi in 1:xmax){
      PlotData <- rbind(PlotData, data.frame(x = c(xstart + (xi - 1), 
                                                   xstart + xi, 
                                                   xstart + xi + sa,
                                                   xstart + xi - 1 + sa),
                                             y = c(ystart, ystart, ystart + ca, ystart + ca),
                                             fill = "Top",
                                             group = gr))
      gr <- gr + 1
    }
    PlotData <- rbind(PlotData, data.frame(x = c(xstart + xmax,
                                                 xstart + xmax + sa,
                                                 xstart + xmax + sa,
                                                 xstart + xmax), 
                                           y = c(ystart, ystart + ca, ystart + ca - 1, ystart - 1), fill = "Side", group = gr))
    gr <- gr + 1
  }
  PlotData
}

# Function to transform Coordinates by a chosen angle.
transform.xy <- function(x, y, angle = 45){
  sa <- sin(angle * pi / 180)
  ca <- cos(angle * pi / 180)
  data.frame(x = x + y * sa, y = y * ca)
}

#################################################################################################################
#### Plot of a simple Ellipse ####
#################################################################################################################
a <- 0.7    # First Radius of Ellipse.
b <- 0.45   # Second Radius of Ellipse.
# We want the typical Axis of mathematical Plots, so these are the Axis + Ticks.
axisv <- data.frame(x    = c(0, rep(-0.02, 8)), 
                    xend = c(0, rep(0.02, 8)), 
                    y    = c(-1.1, seq(-1, 1, 0.25)[c(1:4, 6:9)]), 
                    yend = c(1.1, seq(-1, 1, 0.25)[c(1:4, 6:9)]))
axish <- data.frame(x    = c(-1.1, seq(-1, 1, 0.25)[c(1:4, 6:9)]), 
                    xend = c(1.1, seq(-1, 1, 0.25)[c(1:4, 6:9)]), 
                    y    = c(0, rep(-0.02, 8)), 
                    yend = c(0, rep(0.02, 8)))
# These are the Axis Labels.
axlabv <- data.frame(x = c(-0.02), y = seq(-1, 1, 0.25), label = seq(-1, 1, 0.25))
axlabh <- data.frame(x = seq(-1, 1, 0.25)[c(1:4, 6:9)], y = c(-0.02), label = seq(-1, 1, 0.25)[c(1:4, 6:9)])


p <- ggplot(axisv)+
  geom_vline(xintercept = seq(-1, 1, 0.25), color = "grey90")+                                     # X-Grid-Lines
  geom_hline(yintercept = seq(-1, 1, 0.25), color = "grey90")+                                     # Y-Grid-Lines
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend))+                                       # X-Ticks
  geom_segment(data = axish, aes(x = x, xend = xend, y = y, yend = yend))+                         # Y-Ticks
  annotate("segment", x = 1.1 - 0.05, xend = 1.1, y = -0.03, yend = 0)+                            # X-Axis Arrow
  annotate("segment", x = 1.1 - 0.05, xend = 1.1, y =  0.03, yend = 0)+                            
  annotate("segment", x = -0.03, xend = 0, y = 1.1 - 0.05, yend = 1.1)+                            # Y-Axis Arrow
  annotate("segment", x =  0.03, xend = 0, y = 1.1 - 0.05, yend = 1.1)+                          
  annotate("text", x = 1.1, y = -0.03, label = "x", vjust = 1, size = 5)+                          # X-Axis-Label
  annotate("text", x = -0.03, y = 1.1, label = "y", hjust = 1, size = 5)+                          # Y-Axis-Label
  geom_function(fun = yt, args = list(a = a, b = b), xlim = c(-1 * a, a), n = 1000, size = 1.5)+   # Top Part of Ellipse
  geom_function(fun = yb, args = list(a = a, b = b), xlim = c(-1 * a, a), n = 1000, size = 1.5)+   # Bottom Part of Ellipse
  geom_text(data = axlabv, aes(x = x, y = y, label = label, hjust = 1, vjust = 1))+                # X-Axis-Tick-Labels
  geom_text(data = axlabh, aes(x = x, y = y, label = label, hjust = 1, vjust = 1))+                # Y-Axis-Tick-Labels
  annotate("segment", x = 0, xend = a, y = 0, yend = 0, color = "red", linewidth = 2)+             # First Radius
  annotate("segment", x = 0, xend = 0, y = 0, yend = b, color = "blue", linewidth = 2)+            # Second Radius
  annotate("text", x = a / 2, y = 0.03, label = "a", vjust = 0, size = 7, color = "red")+          # Text for 1. Radius
  annotate("text", x = 0.03, y = b / 2, label = "b", hjust = 0, size = 7, color = "blue")+         # Text for 2. Radius
  coord_fixed()+
  theme_void()+
  theme(plot.background = element_rect(fill = "white", color = "white")); p


ggsave(p, file = paste0("Plots/Ellipse_Function/01_ab.", PicForm), 
       height = 4, width = 4)



#################################################################################################################
#### Plot of a Shadow of a Tree ####
#################################################################################################################
# Tree Parameter
CrownRadius <- 0.31
CrownHeight <- 0.5
SolarAltitude <- 35
SolarAzimuth <- 30

# 2 Ellipses as Examples (different sizes) to show what changes what.
firstEllipse <- data.frame(x = c(seq(limx(CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth)[1], 
                                     limx(CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth)[2], by = 0.001),
                                 rev(seq(limx(CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth)[1], 
                                         limx(CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth)[2], by = 0.001))))
firstEllipse$y <- c(yt2(firstEllipse$x[1:(nrow(firstEllipse) / 2)], CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth),
                    yb2(firstEllipse$x[(nrow(firstEllipse) / 2 + 1):nrow(firstEllipse)], CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth))

SolarAltitude <- 60
secEllipse <- data.frame(x = c(seq(limx(CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth)[1], 
                                   limx(CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth)[2], by = 0.001),
                                 rev(seq(limx(CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth)[1], 
                                         limx(CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth)[2], by = 0.001))))
secEllipse$y <- c(yt2(secEllipse$x[1:(nrow(secEllipse) / 2)], CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth),
                  yb2(secEllipse$x[(nrow(secEllipse) / 2 + 1):nrow(secEllipse)], CrownRadius, CrownHeight, SolarAltitude, SolarAzimuth))


p <- ggplot(axisv)+
  geom_vline(xintercept = seq(-1, 1, 0.25), color = "grey90")+                                     # X-Grid-Lines
  geom_hline(yintercept = seq(-1, 1, 0.25), color = "grey90")+                                     # Y-Grid-Lines
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend))+                                       # X-Ticks
  geom_segment(data = axish, aes(x = x, xend = xend, y = y, yend = yend))+                         # Y-Ticks
  annotate("segment", x = 1.1 - 0.05, xend = 1.1, y = -0.03, yend = 0)+                            # X-Axis Arrow
  annotate("segment", x = 1.1 - 0.05, xend = 1.1, y =  0.03, yend = 0)+                            
  annotate("segment", x = -0.03, xend = 0, y = 1.1 - 0.05, yend = 1.1)+                            # Y-Axis Arrow
  annotate("segment", x =  0.03, xend = 0, y = 1.1 - 0.05, yend = 1.1)+                          
  annotate("text", x = 1.1, y = -0.03, label = "x", vjust = 1, size = 5)+                          # X-Axis-Label
  annotate("text", x = -0.03, y = 1.1, label = "y", hjust = 1, size = 5)+                          # Y-Axis-Label
  geom_text(data = axlabv, aes(x = x, y = y, label = label, hjust = 1, vjust = 1))+                # X-Axis-Tick-Labels
  geom_text(data = axlabh, aes(x = x, y = y, label = label, hjust = 1, vjust = 1))+                # Y-Axis-Tick-Labels
  geom_polygon(data = firstEllipse, aes(x = x, y = y), fill = "grey40", color = "black", alpha = 0.4, size = 1)+  # First Ellipse
  geom_polygon(data = secEllipse, aes(x = x, y = y), fill = "grey40", color = "black", alpha = 0.5, size = 1)+    # Second Ellipse
  annotate("segment", x = 0, xend = -1 * CrownRadius * cos(SolarAzimuth * pi / 180),               # 1. Radius of Ellipse (Crown Radius)
           y = 0, yend = CrownRadius * sin(SolarAzimuth * pi / 180), size = 1.5)+
  geom_curve(data = data.frame(x = 0, xend = 0.9 * sin(SolarAzimuth * pi / 180), y = 0.9, yend = 0.9 * cos(SolarAzimuth * pi / 180)),   # Arrow for Solar Azimuth (Tilt of Ellipse)
             aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.08, "inch")), size = 1, curvature = -0.17)+
  annotate("segment", x = 0.45 * sin(SolarAzimuth * pi / 180), xend = 0.75 * sin(SolarAzimuth * pi / 180), size = 1,                    # Arrow for 2. Radius of Ellipse
           y = 0.45 * cos(SolarAzimuth * pi / 180), yend = 0.75 * cos(SolarAzimuth * pi / 180), arrow = arrow(length = unit(0.08, "inch")))+
  annotate("segment", x = 0.75 * sin(SolarAzimuth * pi / 180), xend = 0.45 * sin(SolarAzimuth * pi / 180), size = 1,                    # Arrow for 2. Radius of Ellipse
           y = 0.75 * cos(SolarAzimuth * pi / 180), yend = 0.45 * cos(SolarAzimuth * pi / 180), arrow = arrow(length = unit(0.08, "inch")))+
  annotate("text", x = -0.3, y = 0.2, label = "Crown Radius", hjust = 1, vjust = 0)+                       # Label of Crown Radius
  annotate("text", x = 0.25, y = 0.95, label = "Solar Azimuth", hjust = 0, vjust = 0)+                     # Label of Solar Azimuth
  annotate("label", x = 0.35, y = 0.55, label = "Crown Height &\nSolar Altitude", hjust = 0, vjust = 1)+   # Label of Crown Height and Solar Altitude
  coord_fixed()+
  theme_void()+
  theme(plot.background = element_rect(fill = "white", color = "white")); p


ggsave(p, file = paste0("Plots/Ellipse_Function/02_CrownRadHeightAzAlt.", PicForm),
       height = 4, width = 4)


#################################################################################################################
#### Plot the two different Crown Shapes at Trees ####
#################################################################################################################
textsize <- 8
ts <- textsize / .pt
XElli <- 1.5
XDisk <- 0
CS <- 1
AreaInBetween <- data.frame(x = c(seq(-0.3, 0.3, 0.005), seq(0.3, -0.3, by = -0.005)),
                            y = c(yb(seq(-0.3, 0.3, 0.005), a = 0.3, b = 0.15) + 2, yb(seq(0.3, -0.3, -0.005), a = 0.3, b = 0.15) + 2 - 0.015))
p <- ggplot()+
  geom_hline(yintercept = 0)+                                                                             # Line at Ground Level
  geom_ellipse(aes(x0 = XElli, y0 = 1.5, a = 0.3, b = 0.5, angle = 0), fill = "darkgreen", alpha = 0.5)+      
  annotate("segment", x = c(XElli - 0.05, XElli + 0.05), xend = c(XElli - 0.05, XElli + 0.05), y = 0, yend = 1)+
  geom_ellipse(aes(x0 = XDisk, y0 = 2, a = 0.05, b = 0.05 / 3, angle = 0))+
  geom_polygon(data = AreaInBetween, aes(x = x, y = y), fill = "darkgreen", alpha = 0.5)+
  geom_ellipse(aes(x0 = XDisk, y0 = 2-0.015, a = 0.3, b = 0.15, angle = 0))+
  geom_ellipse(aes(x0 = XDisk, y0 = 2, a = 0.3, b = 0.15, angle = 0), fill = "darkgreen", alpha = 0.5)+
  annotate("segment", x = c(XDisk - 0.05, XDisk + 0.05), xend = c(XDisk - 0.05, XDisk + 0.05), y = 0, yend = 2)+
  annotate("text", x = c(XElli, XDisk), y = 2.4, label = c("Ellipsoid Crown", "Disk Crown"))+
  annotate("segment", x = XElli+0.4*CS, xend = XElli+0.4*CS, y = 0, yend = 2, linetype = "dotted", lineend = "square", 
           arrow = arrow(angle = 90, length = unit(5, "points"), ends = "both"), color = "grey40")+
  annotate("segment", x = XElli+0.4*CS, xend = XElli+0.4*CS, y = 1, yend = 2, linetype = "dashed", lineend = "square", 
           arrow = arrow(angle = 90, length = unit(5, "points"), ends = "both"))+
  annotate("segment", x = XElli+0.15*CS, xend = XElli+0.15*CS, y = 0, yend = 1.5, linetype = "dashed", lineend = "square", 
           arrow = arrow(angle = 90, length = unit(5, "points"), ends = "both"))+
  annotate("segment", x = 0.75, xend = 0.75, y = 0, yend = 2, linetype = "dashed", lineend = "square", 
           arrow = arrow(angle = 90, length = unit(5, "points"), ends = "both"))+
  annotate("segment", x = c(XElli, XDisk), xend = c(XElli+0.3*CS*-1, XDisk+0.3), y = c(1.5, 2), yend = c(1.5, 2), linetype = "dashed", lineend = "square", 
           arrow = arrow(angle = 90, length = unit(5, "points"), ends = "both"))+
  annotate("text", x = XElli+0.05*CS*-1, y = 1.52, label = "r_crown", hjust = 0.5+0.5*CS, vjust = 0, size = ts)+
  annotate("text", x = XDisk+0.05, y = 2.02, label = "r_crown", hjust = 0, vjust = 0, size = ts)+
  annotate("text", x = XDisk+0.73, y = 1, label = "Height", hjust = 0.5, vjust = 0, angle = 90, size = ts)+
  annotate("text", x = XElli+0.42*CS, y = 1.5, label = "Proportion of Crowned Stem\nfrom Total Stem Height", hjust = 0.5, vjust = 0.5+0.5*CS, angle = 90, size = ts)+
  annotate("text", x = XElli+0.17*CS, y = 0.7, label = "Height of Crown Midpoint", hjust = 0.5, vjust = 0.5+0.5*CS, angle = 90, size = ts)+
  coord_fixed(xlim = c(-0.5, 2))+
  theme_void()+
  theme(plot.background = element_rect(fill = "white", color = "white"),
        text = element_text(size = textsize)); p

ggsave(p, file = paste0("Plots/Ellipse_Function/03_CrownShapes.", PicForm), 
       height = 5, width = 8)




#################################################################################################################
#### Plot of Shadows over a day ####
#################################################################################################################
# Define a Latitude and a Day of Year (DoY)
MyLat <- 52.217
MyDoy <- 128
# Calculate Shadow Ellipses (Polygons of these Shadows) overthe Day
MyShadow <- getShadow(x0 = 0, y0 = 0, CrownRadius = 5, Height = 30, CrownHeight = 15, 
                      Hour = seq(Sunrise(MyDoy, MyLat), Sunset(MyDoy, MyLat), length.out = 20), 
                      DOY = MyDoy, Latitude = MyLat, npoints = 1000)

p <- ggplot(MyShadow)+
  geom_vline(xintercept = seq(-400, 400, 5), color = "grey90", size = 0.1)+
  geom_hline(yintercept = seq(-400, 400, 5), color = "grey90", size = 0.1)+
  geom_polygon(aes(x = x, y = y, group = group, fill = Rad), alpha = 0.6)+
  geom_point(data = data.frame(x = 0, y = 0), aes(x = x, y = y), color = "#248a07", size = 1)+
  scale_fill_gradient(low = "white", high = "black", guide = "none",
                      limits = c(0, max(MyShadow$Rad)))+
  coord_fixed(xlim = c(-350, 350), ylim = c(-150, 50))+
  theme_void()+
  theme(plot.background = element_rect(fill = "white", color = "white")); p

ggsave(p, file = paste0("Plots/Ellipse_Function/04_Ellipses_over_day.", PicForm), 
       height = 2, width = 4)




#################################################################################################################
#### Plot Grid with Trees ####
#################################################################################################################
# First, define some Variables
MyAngle <- 60      # Rotation of the Grid
MyHour <- 14       # Current Hour
MyDOY <- 180       # Current Day of the Year
MyLat <- 52.217    # Latitude
MyFills <- c("Front" = "#87a306", "Top" = "#cfda9b", "Side" = "#6c8204")    # Colors of the Plot

# Coordinates and Height + Crown Radius of Trees
Trees <- data.frame(x           = c(4.1, 3.4, 6.3, 4.3, 9.8), 
                    y           = c(2.6, 6.1, 11.2, 10.1, 9.1), 
                    Height      = c(5.0, 6.0, 4.8, 5.4, 4.7), 
                    CrownRadius = c(1.0, 1.1, 0.9, 1.0, 0.9))
TreesTrans <- Trees
# Transform Tree Positions in the Following:
TreesTrans[, c("x", "y")] <- transform.xy(TreesTrans$x, TreesTrans$y, angle = MyAngle) # Transform Tree Positions
TreeStems <- data.frame(x = TreesTrans$x, xend = TreesTrans$x, 
                        y = TreesTrans$y, yend = TreesTrans$y + TreesTrans$Height / 2)
TreeCrowns <- data.frame(x0 = TreesTrans$x, y0 = TreesTrans$y + TreesTrans$Height *3 / 4, a = TreesTrans$CrownRadius, 
                         b = TreesTrans$Height / 4)
TreeShadow <- data.frame(x = 1, y = 1, group = 1, Rad = 1)[-1, ]
for(i in 1:nrow(Trees)){
  TS <- getShadow(x0 = Trees$x[i], 
                  y0 = Trees$y[i],
                  CrownRadius = Trees$CrownRadius[i],
                  Height = Trees$Height[i],
                  CrownHeight = Trees$Height[i] / 2,
                  Hour = MyHour,
                  DOY = MyDOY,
                  Latitude = MyLat)
  TS$group <- i
  TreeShadow <- rbind(TreeShadow, TS)
}
TreeShadow[, c("x", "y")] <- transform.xy(TreeShadow$x, TreeShadow$y, angle = MyAngle)

# Plot Grid + Trees + Shadows 
p <- ggplot(MakeWorld(xmax = 15, ymax = 15, angle = MyAngle))+
  geom_polygon(aes(x = x, y = y, group = group, fill = fill), color = "black")+
  geom_polygon(data = TreeShadow, aes(x = x, y = y, group = group), color = "black", alpha = 0.5)+
  geom_segment(data = TreeStems, aes(x = x, xend = xend, y = y, yend = yend), size = 2)+
  geom_ellipse(data = TreeCrowns, aes(x0 = x0, y0 = y0, a = a, b = b, angle = 0), fill = "darkgreen")+
  scale_fill_manual(values = MyFills, guide = "none")+
  coord_fixed()+
  theme_void(); p


ggsave(p, file = paste0("Plots/Ellipse_Function/05_ShadowAndGrid.", PicForm), 
       height = 2.5, width = 5)









#################################################################################################################
#### References ####
#################################################################################################################
# Quesada, F. (2021): Derivación de las fórmulas de sombras de árboles empleadas en el programa ShadeMotion http://hdl.handle.net/11554/11358 













