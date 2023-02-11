# For our Model, we use reconstructed Forests. The reconstructed Forests are based on real data. The Reconstruction happens in this script.

#################################################################################################################
#### Used Packages ####
#################################################################################################################
library(ggplot2)    # Plotting
library(ggforce)    # Make circles of exact size
library(spatstat)   # Spatial Analysis
library(shar)       # Reconstructions of Point Patterns

#################################################################################################################
#### Decide if you want to recalculate everything ####
#################################################################################################################
# Some calculations in this script are really time consuming. You can do these calculations again or decide 
# to load data from previous calculations.
CalcAll <- FALSE

#################################################################################################################
#### Selfmade Functions ####
#################################################################################################################
# Get path of this script. Used for github if you don't work with projects.
GetFile <- function(FileName){
  paste0(sub(sub(".*\\/", "", rstudioapi::getSourceEditorContext()$path), "", rstudioapi::getSourceEditorContext()$path), FileName)
}

# Altered reconstruction-function of reconstruct_pattern_homo of "shar"-Package.
# A Variable "r" is included that defines the maximum limit of the radius-input for Pair-Correlation-Function.
source(GetFile("Reconstruction_Functions.R"))


# Function made by Chris. This is not yet published:
source(GetFile("Chris_Funktion.R"))


#################################################################################################################
#### Load Data ####
#################################################################################################################
# Original Data
TreeData <- data.frame(x = c(-5.6, -8.9, -17.0, -30.9, -29.1, -33.4, -29.9, -29.2, -26.2, -27.0, -24.5, -21.3, -17.4, -16.8, -20.5, -20.5, 
                             -13.7, -5.7, -10.6, -14.2, -20.8, -28.1, -23.5, -18.0, -12.3, -9.1, -15.7, -10.6, -11.0, -6.7, -4.1, -4.8, 0.1, 
                             4.2, -2.0, -0.5, -4.3, -1.4, 2.6, 10.5, 16.0, 17.8, 20.6, 11.9, 11.8, 12.7, 6.0, 2.4, 9.2, 12.4, 16.0, 23.0, 25.4, 
                             29.8, 31.0, 26.4, 26.2, 21.0, 15.6, 12.5, 7.7, 1.0, 4.8, 8.9, 10.9, 17.3, 21.1, 25.8, 28.4, 25.8, 28.8, 25.2, 17.6, 
                             15.3, 19.1, 22.1, 16.0, 8.7, 10.0, 11.9, 14.3, 8.4, 4.7, 2.1, 3.8, 3.3, -1.7, -3.5, -12.0, -18.6, -8.4, -8.2, -4.9, 
                             -1.1, -1.3, -0.4, -1.0),
                       y = c(7.7, 11.2, 12.6, 19.7, 14.0, 9.6, 1.1, -1.8, -2.3, 2.3, 5.8, 8.5, 7.5, -1.2, -0.7, -4.5, -5.8, -5.9, -10.2, -14.0, 
                             -15.6, -18.2, -25.4, -26.1, -26.7, -27.0, -20.5, -21.1, -17.6, -16.2, -18.2, -13.3, -16.6, -21.3, -22.5, -27.6, -33.4, 
                             -33.6, -32.0, -27.4, -22.8, -19.9, -19.8, -21.7, -16.8, -13.8, -9.8, -6.7, -5.4, -8.6, -10.2, -14.3, -18.1, -13.6, -7.0, 
                             -4.6, -10.6, -6.9, -1.5, -0.3, -0.1, 0.1, 3.3, 6.8, 2.7, 4.0, 3.5, 5.6, 5.6, 11.8, 13.3, 22.0, 27.4, 28.8, 17.3, 8.8, 
                             18.0, 13.7, 16.2, 22.1, 23.9, 25.8, 20.7, 23.1, 25.2, 30.7, 25.4, 28.2, 23.7, 24.3, 20.8, 15.6, 16.1, 14.7, 11.7, 8.0, 5.4),
                       DBH = c(44.7, 27.6, 43.8, 38.0, 45.3, 44.1, 27.2, 30.5, 31.8, 33.0, 35.3, 47.3, 32.4, 35.8, 37.7, 39.8, 28.9, 43.9, 34.8, 29.9, 
                               30.1, 34.9, 30.8, 36.7, 32.1, 44.6, 41.6, 31.4, 31.8, 30.1, 29.2, 38.5, 30.1, 34.6, 35.9, 30.0, 31.5, 41.1, 34.1, 39.8, 
                               40.8, 34.8, 34.3, 40.4, 37.7, 35.6, 40.8, 37.3, 32.9, 35.2, 37.1, 32.3, 38.9, 32.6, 42.0, 41.7, 39.8, 41.7, 34.3, 36.1, 
                               34.1, 31.1, 33.1, 41.8, 39.1, 27.7, 35.9, 35.5, 38.2, 29.8, 34.2, 33.1, 29.0, 27.8, 39.6, 36.4, 35.0, 31.8, 27.4, 39.7, 26.1, 
                               27.5, 35.2, 35.7, 30.1, 29.2, 43.6, 32.1, 28.3, 39.9, 52.2, 33.0, 31.2, 28.9, 32.4, 30.5, 30.3),
                       Height = c(30, 24.6, 27.2, 25.4, 24.8, 26.4, 24.2, 28.1, 27.3, 28.2, 26.5, 29.6, 25.2, 30, 27.8, 30.1, 27, 30.5, 28.2, 27.2, 
                                  26.8, 29.5, 26.5, 22.2, 23.8, 23, 30.6, 23.2, 25.2, 26.8, 25.5, 25.2, 25, NA, 27.4, 27.1, 27, 28.8, 27.8, 27.7, 26, 
                                  27.4, 27.6, 25.4, 26, 30, 26.8, 28.2, 25.4, 29.3, 29.2, 27.5, 26.7, 29.4, 28.8, 30.6, 29.3, 28.6, 28.7, 29.6, 25.5, 
                                  28.7, 27.2, 32, 29, 29.6, 30.4, 29, 27.8, 27.4, 27.8, 28.6, 25.4, 29.3, 24.4, 30.7, 28, 27.3, 27.2, 26.9, 27.4, 25.6, 
                                  29.6, 29.4, 29.3, 25.5, 30.6, 30.4, 24.6, 30.2, 29.6, 26.2, 25.5, 26.8, 28.6, 26.9, 25.2),
                       CrownRadius = c(2.92, 1.53, 2.65, 2.83, 2.93, 3.87, 1.29, 2.21, 2.11, 1.75, 2.09, 3.12, 2.33, 3.32, 2.39, 2.43, 2.76, 2.83, 2.2, 
                                       1.84, 2.28, 2.37, 2.29, 2.46, 1.74, 3.34, 2.43, 1.68, 1.6, 1.83, 1.66, 2.67, 2.26, NA, 2.83, 1.97, 1.69, 2.26, 
                                       2.63, 2.33, 2.8, 1.97, 2.12, 2.03, 1.82, 1.73, 2.44, 2.4, 2.22, 2.23, 2.14, 1.8, 3.01, 2.28, 2.46, 3.32, 2.2, 
                                       3.51, 2.34, 2.26, 2.36, 2, 2.37, 3.43, 2.1, 1.48, 2.16, 2.37, 2.49, 2.1, 2.64, 1.71, 2.36, 1.37, 2.8, 1.98, 2.89, 
                                       2.13, 1.86, 1.92, 1.71, 1.84, 2.15, 2.18, 1.85, 2.39, 2.3, 1.92, 1.59, 2.84, 3.56, 1.89, 1.29, 1.89, 1.52, 2.02, 2.63))





#################################################################################################################
#### Analysis of Original Data ####
#################################################################################################################
# Stem Positions
p <- ggplot(TreeData)+
  geom_circle(aes(x0 = x, y0 = y, r = CrownRadius), fill = "darkgreen")+
  scale_x_continuous(name = "X-Coordinate")+
  scale_y_continuous(name = "Y-Coordinate")+
  coord_fixed(); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/01_Original_Plot_Tree_Positions.png"), 
       height = 4, width = 5)

# For the Reconstruction of the forest, we use marked-point-patterns. But we can only use one mark in the process.
# For the model, we need DBH, Height and Crown Radius. We reconstruct the DBH and calculate Height and CrownRadius using
# Models.
# For The Relationship between DBH and Height, we use the Petterson equation (SOURCE).
DBH_Height_Model <- nls(Height ~ ((DBH / (A + B * DBH)) ^ 2), start = list(A = -0.01, B = -0.2), 
                        data = TreeData)

# Save this model as a function to include it into ggplot.
fun_Petterson <- function(x, A = -0.01, B = -0.2){
  (x / (A + B * x)) ^ 2
}

# Plot the Relationship between DBH and Height with the Petterson equation.
MathExpression <- expression(paste("Height=", bgroup("(", frac(DBH, -0.51 - 0.18 %*% DBH), ")")^2))

p <- ggplot(TreeData)+
  geom_function(fun = fun_Petterson, args = list(A = coef(DBH_Height_Model)[1], B = coef(DBH_Height_Model)[2]), color = "red", size = 2)+
  geom_point(aes(x = DBH, y = Height))+
  annotate("text", x = 26, y = 32, hjust = 0, vjust = 1, 
           label = MathExpression, size = 2)+
  scale_x_continuous(name = "DBH [cm]", limits = c(25, 55), expand = c(0, 0))+
  scale_y_continuous(name = "Height [m]", limits = c(21.5, 32.5), expand = c(0, 0)); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/02_nls_DBH_Height.png"), 
       height = 4, width = 5)

# For the Relationship between DBH and Crown Radius we use this model.
# Because Stem Area might be better correlated with Crown Area, we use these parameters and then calculate Crown Radius later.
DBH_Crown_Area_Model <- nls((pi * CrownRadius^2) ~ B * (pi * DBH^2) ^ C, start = list(B = 1, C = 2), data = TreeData)

# Save this model as a function to include it into ggplot.
fun_Para <- function(x, A = 0, B = 1, C = 2){
  A + B * x ^ C
}

# Plot the Relationship between DBH and Crown Radius using the above model.
MathExpression <- expression("Crown Radius" == 0.0019 %*% DBH^-0.18)

p <- ggplot(TreeData)+
  geom_function(fun = fun_Para, args = list(A = 0, B = coef(DBH_Crown_Area_Model)[1], C = coef(DBH_Crown_Area_Model)[2]), 
                color = "red", size = 1)+
  geom_point(aes(x = pi * DBH^2, y = pi * CrownRadius^2))+
  annotate("text", x = 500, y = 45, hjust = 0, vjust = 1, 
           label = MathExpression, size = 2)+
  scale_x_continuous(name = "Stem area [cm²]", limits = c(0, 10000), expand = c(0, 0))+
  scale_y_continuous(name = "Crown area [m²]", limits = c(0, 50), expand = c(0, 0)); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/03_nls_DBH_r_crown.png"), 
       height = 4, width = 5)


#Point Pattern Analysis:

# We first have to define a Window of Coordinates that we want to use for Point Pattern Reconstruction
W <- owin(c(-25, 25), c(-25, 25))
# Now save the data as a ppp, First two columns must be x and y!
Tree.ppp <- as.ppp(TreeData, W = W)

# Let's look at the Pair-Correlation-Function:
Tree.pcf <- data.frame(pcf(Tree.ppp, r = seq(0, 25, length.out = 250)))

ggplot(Tree.pcf)+
  geom_line(aes(x = r, y = iso))


# And look at the Mark-Correlation-Function:
Tree.mcf <- data.frame(markcorr(subset(Tree.ppp, select = DBH), r = seq(0, 25, length.out = 250)))

ggplot(Tree.mcf)+
  geom_line(aes(x = r, y = iso))

# Calculate Clark-Evans-Index.
clarkevansCalc(Tree.ppp)


#################################################################################################################
#### Reconstruction of Data ####
#################################################################################################################

# Reconstruction of Point-Coordinates:

# There are three possibilities to reconstruct data.
# 1. Reconstruct data in a window of 400 x 400m.
# 2. Reconstruct data in windows of original size and put them together.
# 3. Reconstruct data in a window of 400 x 400m. Change the reconstruct-Function and define the radius for Pair-Correlation-Function from old window size.

# The problem with the first one is, that this is really time consuming. In shar, trees are first placed at random and a
# Pair-Correlation-Function is calculated.
# Then, trees are moved to a different location. If this results in an improved fit of the original Pair-Correlation-Function,
# the tree will stay on the new location. Otherwise it will be returned to it's old location.
# The bigger the area, the less influence a single tree has on the Pair-Correlation-Function. It is therefore 
# difficult to evaluate, if the new location is an improvement.

# The problem with option two is, that edge effects play a more important role and at the edges of the merged areas, 
# trees could be extremely close to each other.

# Option three seems to be the option that makes most sense. It claculates for a long time, if fast calculation is disabled.

# We tried all three. If you want to do the calculations yourself, turn CalcAll on. But be warned, that the calculations
# are really time consuming. We already did all the calculations. They are saved in GitHub and will be loaded.

# CalcAll <- TRUE

# Reconstruct data here again:
# Set World Size and calculate the number of Trees on the bigger Plot.
WorldSize <- c(-400, 400)
NNew <- ((WorldSize[2] - WorldSize[1]) ^ 2) / (50 ^ 2) * nrow(TreeData[TreeData$x > -25 & TreeData$x < 25 & TreeData$y > -25 & TreeData$y < 25, ])

# 1. Reconstruct data in a window of 400 x 400m
if(CalcAll){
  # Now we reconstruct the point positions.
  # This is done using the Shar-Package. It reconstructs the Pair-Correlation-function.
  Tree.recon <- reconstruct_pattern(Tree.ppp, 
                                    n_random = 1, 
                                    max_runs = 50000,
                                    simplify = TRUE, 
                                    window = owin(WorldSize, WorldSize),
                                    n_points = NNew,
                                    return_input = FALSE,
                                    plot = TRUE)
  AllInOne <- data.frame(x = Tree.recon$x, y = Tree.recon$y)
}else{
  # Reconstructed Data, Resized to 800 x 800m
  AllInOne <- read.csv("https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/AllInOne.csv")
}




# 2. Reconstruct data in windows of original size and put them together.
if(CalcAll){
  # Create an empty data frame.
  Merged <- data.frame(x = 0, y = 0, iteration = 0)[-1, ]
  NNew <- ((WorldSize[2] - WorldSize[1]) ^ 2) / (50 ^ 2)  # How many original Sites fit in the new World?
  # Iterate the process of creating a new world with the size of the original World.
  for(i in 1:NNew){
    Tree.recon <- reconstruct_pattern(Tree.ppp, 
                                      n_random = 1, 
                                      max_runs = 1000,
                                      simplify = TRUE, 
                                      window = owin(c(-25, 25), c(-25, 25)),
                                      n_points = nrow(TreeData),
                                      return_input = FALSE,
                                      plot = TRUE)
    Merged <- rbind(Merged, data.frame(x = Tree.recon$x, y = Tree.recon$y, iteration = i))
  }
  # Now merge them together.
  x <- 1
  y <- 1
  for(i in 1:NNew){
    Merged$x[Merged$iteration == i] <- Merged$x[Merged$iteration == i] + x * 50 - 425
    Merged$y[Merged$iteration == i] <- Merged$y[Merged$iteration == i] + y * 50 - 425
    x <- x + 1
    if(x > sqrt(NNew)){
      x <- 1
      y <- y + 1
    }
  }
  Merged <- Merged[, c("x", "y")]
}else{
  Merged <- read.csv("https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/Merged.csv")
}
write.table(Merged, file = "C:/Users/heinermann/Documents/GitHub/ShadingModelOpen/Merged.csv", sep = ",", row.names = FALSE)


if(CalcAll){
  # Reconstruct data here again:
  # Set World Size and calculate the number of Trees on the bigger Plot.
  NNew <- ((WorldSize[2] - WorldSize[1]) ^ 2) / (50 ^ 2) * nrow(TreeData[TreeData$x > -25 & TreeData$x < 25 & TreeData$y > -25 & TreeData$y < 25, ])
  # Now we reconstruct the point positions.
  # This is done using the Shar-Package. It reconstructs the Pair-Correlation-function.
  # The Function is altered (see above). You can now input the Radius-Range for the Pair-Correlation-Function.
  Tree.recon <- reconstruct_pattern_rcustom(Tree.ppp, 
                                            n_random = 1, 
                                            max_runs = 50000,
                                            simplify = TRUE, 
                                            window = owin(WorldSize, WorldSize),
                                            n_points = NNew,
                                            comp_fast = NNew,
                                            return_input = FALSE,
                                            r = 12.5,   # See here
                                            plot = TRUE)
  Adjusted_r <- data.frame(x = Tree.recon$x, y = Tree.recon$y)
}else{
  # Reconstructed Data, Resized to 800 x 800m
  Adjusted_r <- read.csv("https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/Adjusted_r.csv")
}



# Reconstruct Pattern without edge correction with 50.000 runs
if(CalcAll){
  # Now we reconstruct the point positions.
  # This is done using the Shar-Package. It reconstructs the Pair-Correlation-function.
  # The Function is altered (see above). You can now input the Radius-Range for the Pair-Correlation-Function.
  Tree.recon <- reconstruct_pattern_rcustom(Tree.ppp, 
                                            n_random = 1, 
                                            max_runs = 50000,
                                            simplify = TRUE, 
                                            window = owin(WorldSize, WorldSize),
                                            n_points = NNew,
                                            comp_fast = NNew - 1,
                                            return_input = FALSE,
                                            r = 12.5,   # See here
                                            plot = TRUE)
  No_Edge <- data.frame(x = Tree.recon$x, y = Tree.recon$y)
}else{
  # Reconstructed Data, Resized to 800 x 800m
  No_Edge <- read.csv("https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/No_Edge.csv")
}



# Reconstruct Pattern without edge correction with 50.000 runs
if(CalcAll){
  # Now we reconstruct the point positions.
  # This is done using the Shar-Package. It reconstructs the Pair-Correlation-function.
  # The Function is altered (see above). You can now input the Radius-Range for the Pair-Correlation-Function.
  Tree.recon <- reconstruct_pattern_rcustom(Tree.ppp, 
                                            n_random = 1, 
                                            max_runs = 50000 * 4,
                                            e_threshold = 0.0001,
                                            simplify = TRUE, 
                                            window = owin(WorldSize, WorldSize),
                                            n_points = NNew,
                                            comp_fast = NNew - 1,
                                            return_input = FALSE,
                                            r = 12.5,   # See here
                                            plot = TRUE)
  No_Edge_4 <- data.frame(x = Tree.recon$x, y = Tree.recon$y)
}else{
  # Reconstructed Data, Resized to 800 x 800m
  No_Edge_4 <- read.csv("https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/No_Edge_4.csv")
}



# Reconstruct Pattern without edge correction with 50.000 runs
if(CalcAll){
  # Now we reconstruct the point positions.
  # This is done using the Shar-Package. It reconstructs the Pair-Correlation-function.
  # The Function is altered (see above). You can now input the Radius-Range for the Pair-Correlation-Function.
  Tree.recon <- reconstruct_pattern_custom(Tree.ppp, 
                                           n_random = 1, 
                                           max_runs = 50000 * 4 + 10000,
                                           noEdge_runs = 50000 * 4,
                                           simplify = TRUE, 
                                           window = owin(WorldSize, WorldSize),
                                           n_points = NNew,
                                           comp_fast = NNew - 1,
                                           return_input = FALSE,
                                           r = 12.5,
                                           plot = TRUE)
  Edge_NoEdge <- data.frame(x = Tree.recon$x, y = Tree.recon$y)
} else {
  # Reconstructed Data, Resized to 800 x 800m
  Edge_NoEdge <- read.csv("https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/Edge_NoEdge.csv")
}






#################################################################################################################
#### Analysis of Reconstructed Data ####
#################################################################################################################
# PCF is calculated within a radius of 0 to 1/4 of the plot size. Plot size of the original Plot is 
# 50 x 50m. But Plot Size of the Reconstructed Plot is 800x800m. For Reconstruction the new Plot
# Size is used, which leads to weird Curves. We use the new PCF first and then use the Original one.

# Make PCFs
Tree.pcf <- data.frame(pcf(Tree.ppp, r = seq(0, 100, length.out = 250)))[-1, ]
AllInOne.pcf <- data.frame(pcf(as.ppp(AllInOne, W = owin(WorldSize, WorldSize)), r = seq(0, 100, length.out = 250)))[-1, ]
Merged.pcf <- data.frame(pcf(as.ppp(Merged, W = owin(WorldSize, WorldSize)), r = seq(0, 100, length.out = 250)))[-1, ]
Adjusted.pcf <- data.frame(pcf(as.ppp(Adjusted_r, W = owin(WorldSize, WorldSize)), r = seq(0, 100, length.out = 250)))[-1, ]
No_Edge.pcf <- data.frame(pcf(as.ppp(No_Edge, W = owin(WorldSize, WorldSize)), r = seq(0, 100, length.out = 250)))[-1, ]
No_Edge_4.pcf <- data.frame(pcf(as.ppp(No_Edge_4, W = owin(WorldSize, WorldSize)), r = seq(0, 100, length.out = 250)))[-1, ]
Edge_NoEdge.pcf <- data.frame(pcf(as.ppp(Edge_NoEdge, W = owin(WorldSize, WorldSize)), r = seq(0, 100, length.out = 250)))[-1, ]

# Merge All PCFs in one Dataset for Plotting.
pcf.All <- data.frame(r = seq(0, 100, length.out = 250),
                      data = rep(c("Original", "At Once", "Merging", "Adjusted r", "No Edge", "No Edge, increased iterations", "Mix Edge"), each = 500), 
                      method = rep(rep(c("iso", "trans"), each = 250), 7),
                      value = c(0, Tree.pcf$iso, 0, Tree.pcf$trans, 0, AllInOne.pcf$iso, 0, AllInOne.pcf$trans, 0, 
                                Merged.pcf$iso, 0, Merged.pcf$trans, 0, Adjusted.pcf$iso, 0, Adjusted.pcf$trans,
                                0, No_Edge.pcf$iso, 0, No_Edge.pcf$trans, 0, No_Edge_4.pcf$iso, 0, No_Edge_4.pcf$trans,
                                0, Edge_NoEdge.pcf$iso, 0, Edge_NoEdge.pcf$trans))

# Define Labels of Colors and Lines:
Colors <- c("Pois" = 3, "Original" = 1, "At Once" = 2, "Merging" = 4, "Adjusted r" = 7, "No Edge" = 8, "No Edge, increased iterations" = 6,
            "Mix Edge" = 5)
Labels <- expression("Pois" = g[Pois](r), "Original" = "Original Data", "At Once" = "At Once", "Merging" = "Merging",
                     "Adjusted r" = "Adjusted r", "No Edge" = "No Edge Correction", "No Edge Correction, increased iterations", 
                     "Mix Edge" = "Mix of with and without Edge Correction",
                     "iso" = g[Ripley](r), "trans" = g[Trans](r))
Linetypes = c("Pois" = "dashed", "iso" = "solid", "trans" = "dotted")

# Plot all PCFs:
p <- ggplot(pcf.All)+
  geom_line(aes(x = r, y = value, group = paste0(data, method), color = data, linetype = method), size = 1)+
  geom_hline(data = data.frame(y = c(1), col = c("Pois")),
             aes(yintercept = y, color = col, linetype = col))+
  scale_x_continuous(name = "r")+
  scale_y_continuous(name = "Pair Correlation Function [g(r)]")+
  scale_color_manual(name = "Analysed data", values = Colors, labels = Labels)+
  scale_linetype_manual(name = "Edge Correction", values = Linetypes, labels = Labels)+
  coord_cartesian(ylim = c(0, 1.25)); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/04_PCF_Pattern_in_One_r_100.png"), 
       height = 4, width = 8)

# Now look at the PCF for r = 0 to 12.5:
Tree.pcf <- data.frame(pcf(Tree.ppp, r = seq(0, 12.5, length.out = 250)))[-1, ]
AllInOne.pcf <- data.frame(pcf(as.ppp(AllInOne, W = owin(WorldSize, WorldSize)), r = seq(0, 12.5, length.out = 250)))[-1, ]
Merged.pcf <- data.frame(pcf(as.ppp(Merged, W = owin(WorldSize, WorldSize)), r = seq(0, 12.5, length.out = 250)))[-1, ]
Adjusted.pcf <- data.frame(pcf(as.ppp(Adjusted_r, W = owin(WorldSize, WorldSize)), r = seq(0, 12.5, length.out = 250)))[-1, ]
No_Edge.pcf <- data.frame(pcf(as.ppp(No_Edge, W = owin(WorldSize, WorldSize)), r = seq(0, 12.5, length.out = 250)))[-1, ]
No_Edge_4.pcf <- data.frame(pcf(as.ppp(No_Edge_4, W = owin(WorldSize, WorldSize)), r = seq(0, 12.5, length.out = 250)))[-1, ]
Edge_NoEdge.pcf <- data.frame(pcf(as.ppp(Edge_NoEdge, W = owin(WorldSize, WorldSize)), r = seq(0, 12.5, length.out = 250)))[-1, ]

pcf.All <- data.frame(r = seq(0, 12.5, length.out = 250),
                      data = rep(c("Original", "At Once", "Merging", "Adjusted r", "No Edge", "No Edge, increased iterations", "Mix Edge"), each = 500), 
                      method = rep(rep(c("iso", "trans"), each = 250), 7),
                      value = c(0, Tree.pcf$iso, 0, Tree.pcf$trans, 0, AllInOne.pcf$iso, 0, AllInOne.pcf$trans, 0, 
                                Merged.pcf$iso, 0, Merged.pcf$trans, 0, Adjusted.pcf$iso, 0, Adjusted.pcf$trans,
                                0, No_Edge.pcf$iso, 0, No_Edge.pcf$trans, 0, No_Edge_4.pcf$iso, 0, No_Edge_4.pcf$trans,
                                0, Edge_NoEdge.pcf$iso, 0, Edge_NoEdge.pcf$trans))

p <- ggplot(pcf.All)+
  geom_line(aes(x = r, y = value, group = paste0(data, method), color = data, linetype = method), size = 1)+
  geom_hline(data = data.frame(y = c(1), col = c("Pois")),
             aes(yintercept = y, color = col, linetype = col))+
  scale_x_continuous(name = "r")+
  scale_y_continuous(name = "Pair Correlation Function [g(r)]")+
  scale_color_manual(name = "Analysed data", values = Colors, labels = Labels)+
  scale_linetype_manual(name = "Edge Correction", values = Linetypes, labels = Labels)+
  coord_cartesian(ylim = c(0, 1.25)); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/05_PCF_Pattern_in_One_r_12_5.png"), 
       height = 4, width = 8)

# The merged Data looks much closer to the PCF than the data reconstructed with 800x800m. We can now look at the distribution of trees.
# For this we count Trees in squares and see, if we can see anything unusual.

# Combine Data in one data frame.
Reconstructed <- rbind(cbind(AllInOne, method = "At Once"), cbind(Merged, method = "Merging"), cbind(Adjusted_r, method = "Adjusted r"),
                       cbind(No_Edge, method = "No Edge"), cbind(No_Edge_4, method = "No Edge, increased iterations"),
                       cbind(Edge_NoEdge, method = "Mix Edge"))

# Define a Tile-Size (Edge Length)
TileSize <- 20
nTiles <- ((WorldSize[2] - WorldSize[1]) ^ 2) / (TileSize ^ 2)          # Calculate the Number of Tiles.
TileData <- data.frame(x = 1, y = 1, method = "AB", nTrees = 0)[-1, ]   # Create an empty data frame which is filled in the following.
x <- -400   # Define the starting Coordinates of the Tile Calculation.
y <- -400
# Iterate through the data and calculate the number of trees in each Tile.
for(i in 1:nTiles){
  for(m in c("At Once", "Merging", "Adjusted r", "No Edge", "No Edge, increased iterations", "Mix Edge")){
    TileData <- rbind(TileData, data.frame(x = x, y = y, method = m,
                                           nTrees = sum(Reconstructed$x >= x & Reconstructed$x < (x + TileSize) &
                                                          Reconstructed$y >= y & Reconstructed$y < (y + TileSize) & 
                                                          Reconstructed$method == m)))
  }
  x <- x + TileSize
  if(x >= WorldSize[2]){
    x <- -400
    y <- y + TileSize
  }
}

# Plot the Tiles.
p <- ggplot(TileData)+
  geom_raster(aes(x = x + TileSize / 2, y = y + TileSize / 2, fill = nTrees))+
  scale_x_continuous(name = "X-Coordinate", expand = c(0, 0))+
  scale_y_continuous(name = "Y-Coordinate", expand = c(0, 0))+
  scale_fill_continuous(name = paste0("Number of Trees\nin an area of\n", TileSize, " x ", TileSize, "m"))+
  facet_wrap(. ~ method, labeller = as_labeller(c("At Once" = "Method: At Once", "Merging" = "Method: Merging", "Adjusted r" = "Method: Adjusted r",
                                                  "No Edge" = "Method: No Edge Correction", 
                                                  "No Edge, increased iterations" = "Method: No Edge Correction, increased Iterations", 
                                                  "Mix Edge" = "Method: Mix with and without Edge Correction")))+
  theme(axis.text = element_text(angle = 45, hjust = 1),
        strip.text.x = element_text(size = 4),
        panel.spacing = unit(15, "pt"))+
  coord_fixed(); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/06_Reconstruct_Patter_in_One.png"), 
       height = 4, width = 10)

# Plot Density of Tile-Numbers.
p <- ggplot(TileData)+
  geom_density(aes(x = nTrees, color = method))+
  scale_color_manual(name = "Analysed data", values = Colors[3:8], labels = Labels); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/07_Tile_Density.png"), 
       height = 4, width = 8)
# As we see above, when we calculate all in one go, then we end up with a plot where all Trees are placed at the Edges of the plot. 
# This does not seem desirable. We should therefore continue with the merged Plots.




# Now look at the PCF for r = 0 to 12.5:
Tree.pcf <- data.frame(estimate_pcf_fast(Tree.ppp, correction = "none", method = "c", spar = 0.5, r = seq(0, 12.5, length.out = 250)))[-1, ]
AllInOne.pcf <- data.frame(estimate_pcf_fast(as.ppp(AllInOne, W = owin(WorldSize, WorldSize)), correction = "none", method = "c", spar = 0.5, r = seq(0, 12.5, length.out = 250)))[-1, ]
Merged.pcf <- data.frame(estimate_pcf_fast(as.ppp(Merged, W = owin(WorldSize, WorldSize)), correction = "none", method = "c", spar = 0.5, r = seq(0, 12.5, length.out = 250)))[-1, ]
Adjusted.pcf <- data.frame(estimate_pcf_fast(as.ppp(Adjusted_r, W = owin(WorldSize, WorldSize)), correction = "none", method = "c", spar = 0.5, r = seq(0, 12.5, length.out = 250)))[-1, ]
No_Edge.pcf <- data.frame(estimate_pcf_fast(as.ppp(No_Edge, W = owin(WorldSize, WorldSize)), correction = "none", method = "c", spar = 0.5, r = seq(0, 12.5, length.out = 250)))[-1, ]
No_Edge_4.pcf <- data.frame(estimate_pcf_fast(as.ppp(No_Edge_4, W = owin(WorldSize, WorldSize)), correction = "none", method = "c", spar = 0.5, r = seq(0, 12.5, length.out = 250)))[-1, ]
Edge_NoEdge.pcf <- data.frame(estimate_pcf_fast(as.ppp(Edge_NoEdge, W = owin(WorldSize, WorldSize)), correction = "none", method = "c", spar = 0.5, r = seq(0, 12.5, length.out = 250)))[-1, ]

pcf.All <- rbind(Tree.pcf, AllInOne.pcf, Merged.pcf, Adjusted.pcf, No_Edge.pcf, No_Edge_4.pcf, Edge_NoEdge.pcf)
pcf.All$data <- rep(c("Original", "At Once", "Merging", "Adjusted r", "No Edge", "No Edge, increased iterations", "Mix Edge"), each = 249)

pcf.All <- data.frame(r = seq(0, 12.5, length.out = 250),
                      data = rep(c("Original", "At Once", "Merging", "Adjusted r", "No Edge", "No Edge, increased iterations", "Mix Edge"), each = 500), 
                      method = rep(rep(c("iso", "trans"), each = 250), 7),
                      value = c(0, Tree.pcf$iso, 0, Tree.pcf$trans, 0, AllInOne.pcf$iso, 0, AllInOne.pcf$trans, 0, 
                                Merged.pcf$iso, 0, Merged.pcf$trans, 0, Adjusted.pcf$iso, 0, Adjusted.pcf$trans,
                                0, No_Edge.pcf$iso, 0, No_Edge.pcf$trans, 0, No_Edge_4.pcf$iso, 0, No_Edge_4.pcf$trans,
                                0, Edge_NoEdge.pcf$iso, 0, Edge_NoEdge.pcf$trans))

p <- ggplot(pcf.All)+
  geom_line(aes(x = r, y = pcf, group = data, color = data), size = 1)+
  geom_hline(data = data.frame(y = c(1), col = c("Pois")),
             aes(yintercept = y, color = col, linetype = col))+
  scale_x_continuous(name = "r")+
  scale_y_continuous(name = "Pair Correlation Function [g(r)]")+
  scale_color_manual(name = "Analysed data", values = Colors, labels = Labels)+
  scale_linetype_manual(name = "Edge Correction", values = Linetypes, labels = Labels)+
  coord_cartesian(ylim = c(0, 1.25)); p


estimate_pcf_fast(relocated, 
                  correction = "none", method = "c", 
                  spar = 0.5, r = r)










#################################################################################################################
#### Reconstruction of Marks ####
#################################################################################################################
WorldSize <- c(-400, 400)
Edge_NoEdge.ppp <- as.ppp(Edge_NoEdge, W = owin(WorldSize, WorldSize))


if(CalcAll){
  # Reconstruct data here again:
  # Set World Size and calculate the number of Trees on the bigger Plot.
  marks_sub <- spatstat.geom::subset.ppp(Tree.ppp, select = DBH)
  marks_recon <- reconstruct_pattern_marks_custom(pattern = Edge_NoEdge.ppp, 
                                                  marked_pattern = marks_sub,
                                                  r = 12.5,
                                                  n_random = 1, 
                                                  max_runs = 20000,
                                                  e_threshold = 0.002,
                                                  plot = TRUE)
  
  # Save x, y and DBH of reconstructed data.
  Edge_NoEdge.marks <- data.frame(x = marks_recon$randomized$randomized_1$x,
                                  y = marks_recon$randomized$randomized_1$y,
                                  DBH = marks_recon$randomized$randomized_1$marks)
} else {
  # Reconstructed Data, Resized to 800 x 800m
  Edge_NoEdge.marks <- read.csv("https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/Edge_NoEdge_marks.csv")
}


if(CalcAll){
  Tree.Chris <- cbind(TreeData[, c("x", "y", "DBH")], Species = c(rep("A", nrow(TreeData) - 1), "B"))
  Tree.Chris$Species <- factor(Tree.Chris$Species)
  Tree.Chris[, 1:2] <- Tree.Chris[, 1:2] + 25
  Tree.Chris <- Tree.Chris[Tree.Chris$x > 0 & Tree.Chris$x < 50 & Tree.Chris$y > 0 & Tree.Chris$y < 50, ]
  Tree.Chris <- as.ppp(Tree.Chris, W = owin(c(0, 50), c(0, 50)))
  
  reconstruction <- Pattern_reconstruction_with_two_marks(Tree.Chris,          # Markiertes Pattern
                                                          n_repetitions  = 1,  # Anzahl der rekonstruierten Flächen
                                                          max_runs = 230000,   # Anzahl der Runs
                                                          no_changes = 200,    # Wie viele Durchläufe ohne Ändernung?
                                                          rcount = 250,        # Wie fein Paarkorrelationsfunktion aufgelöst?
                                                          rmax = 12.5,         # Radius um den Baum für Berechnung G-Funktion, ...
                                                          issue = 1000,        # Wie oft wird eine Änderung angezeigt (als Message)?
                                                          use.g_func = TRUE,   # Mitberücksichtigung der G-Funktion
                                                          divisor = "r",       # Divisor der Kerne-Dichte (r oder d) # Lieber r
                                                          timing = TRUE,       # Abspeicherung der Simulationslänge
                                                          energy_evaluation = TRUE, # Genaue Auswertung der Energieanteile Paarkorrelationsfunktion etc.
                                                          show_graphic = FALSE, # Grafik während der Berechnung?
                                                          bw = 0.5,            # Kernelbreite
                                                          obs_window = owin(c(0, 800), c(0, 800)))
  
  ChrisData <- data.frame(x = reconstruction$reconstructed$x, 
                          y = reconstruction$reconstructed$y,
                          DBH = reconstruction$reconstructed$marks$diameter * 1000)
} else {
  # Reconstructed Data, Resized to 800 x 800m
  ChrisData <- read.csv("https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/ChrisData.csv")
}


ChrisData.pcf <- data.frame(pcf(as.ppp(ChrisData, W = owin(c(0, 800), c(0, 800))), 
                                r = seq(0, 12.5, length.out = 250)))

PCFData <- data.frame(r = seq(0, 12.5, length.out = 250),
                      data = rep(c("Original", "Reconstructed", "Chris Function"), each = 500),
                      method = rep(rep(c("iso", "trans"), each = 250), 3),
                      value = c(0, Tree.pcf$iso, 0, Tree.pcf$trans, 0, Edge_NoEdge.pcf$iso, 0, Edge_NoEdge.pcf$trans,
                                ChrisData.pcf$iso, ChrisData.pcf$trans))

Colors <- c("Pois" = 3, "Original" = 1, "Reconstructed" = 2, "Chris Function" = 6)
Labels <- expression("Pois" = g[Pois](r), "Original" = "Original Data", "Reconstructed" = "Reconstructed Data",
                     "Chris Function" = "Reconstructed with Chris Function",
                     "iso" = g[Ripley](r), "trans" = g[Trans](r))
Linetypes = c("Pois" = "dashed", "iso" = "solid", "trans" = "dotted")

p <- ggplot(PCFData)+
  geom_line(aes(x = r, y = value, color = data, linetype = method), size = 1)+
  geom_hline(data = data.frame(y = c(1), col = c("Pois")),
             aes(yintercept = y, color = col, linetype = col), size = 1)+
  scale_x_continuous(name = "r [m]")+
  scale_y_continuous(name = "Pair Correlation Function [g(r)]")+
  scale_color_manual(name = "Analysed data", values = Colors, labels = Labels)+
  scale_linetype_manual(name = "Edge Correction", values = Linetypes, labels = Labels)+
  coord_cartesian(ylim = c(0, 1.25)); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/08_PairCorr_Chris_Shar.png"), 
       height = 4, width = 8)



# 
Tree.pcf <-  data.frame(pcf(as.ppp(TreeData, W = owin(c(-25, 25), c(-25, 25))), 
                                 bw = 0.5, r = seq(0, 12.5, length.out = 250), 
                                 kernel = "epanechnikov", stoyan = 0, correction = "none", divisor = "d"))
Edge_NoEdge.pcf <-  data.frame(pcf(as.ppp(Edge_NoEdge, W = owin(c(-400, 400), c(-400, 400))), 
                                 bw = 0.5, r = seq(0, 12.5, length.out = 250), 
                                 kernel = "epanechnikov", stoyan = 0, correction = "none", divisor = "d"))
ChrisData.pcf <-  data.frame(pcf(as.ppp(ChrisData, W = owin(c(0, 800), c(0, 800))), 
                                 bw = 0.5, r = seq(0, 12.5, length.out = 250), 
                                 kernel = "epanechnikov", stoyan = 0, correction = "none", divisor = "d"))

PCFData <- data.frame(r = seq(0, 12.5, length.out = 250),
                      data = rep(c("Original", "Reconstructed", "Chris Function"), each = 250),
                      method = "un",
                      value = c(Tree.pcf$un, Edge_NoEdge.pcf$un, ChrisData.pcf$un))

Colors <- c("Pois" = 3, "Original" = 1, "Reconstructed" = 2, "Chris Function" = 6)
Labels <- expression("Pois" = g[Pois](r), "Original" = "Original Data", "Reconstructed" = "Reconstructed Data",
                     "Chris Function" = "Reconstructed with Chris Function",
                     "un" = "No Edge Correction")
Linetypes = c("Pois" = "dashed", "un" = "solid")

p <- ggplot(PCFData)+
  geom_line(aes(x = r, y = value, color = data, linetype = method), size = 1)+
  geom_hline(data = data.frame(y = c(1), col = c("Pois")),
             aes(yintercept = y, color = col, linetype = col), size = 1)+
  scale_x_continuous(name = "r [m]")+
  scale_y_continuous(name = "Pair Correlation Function [g(r)]")+
  scale_color_manual(name = "Analysed data", values = Colors, labels = Labels)+
  scale_linetype_manual(name = "Edge Correction", values = Linetypes, labels = Labels); p
  coord_cartesian(ylim = c(0, 1.25)); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/09_PairCorr_noEdgeCorrection.png"), 
       height = 4, width = 8)





Tree.m <- data.frame(markcorr(subset(Tree.ppp, select = DBH), r = seq(0, 12.5, length.out = 250)))
Edge_NoEdge.m <- data.frame(markcorr(as.ppp(Edge_NoEdge.marks, W = owin(c(-400, 400), c(-400, 400))), 
                                     r = seq(0, 12.5, length.out = 250)))
ChrisData.m <- data.frame(markcorr(as.ppp(ChrisData, W = owin(c(0, 800), c(0, 800))), 
                                     r = seq(0, 12.5, length.out = 250)))

MarkCorrData <- data.frame(r = seq(0, 12.5, length.out = 250),
                           data = rep(c("Original", "Reconstructed", "Chris Function"), each = 500),
                           method = rep(rep(c("iso", "trans"), each = 250), 3),
                           value = c(Tree.m$iso, Tree.m$trans, Edge_NoEdge.m$iso, Edge_NoEdge.m$trans,
                                     ChrisData.m$iso, ChrisData.m$trans))

Colors <- c("Pois" = 3, "Original" = 1, "Reconstructed" = 2, "Chris Function" = 6)
Labels <- expression("Pois" = g[Pois](r), "Original" = "Original Data", "Reconstructed" = "Reconstructed Data",
                     "Chris Function" = "Reconstructed with Chris Function",
                     "iso" = g[Ripley](r), "trans" = g[Trans](r))
Linetypes = c("Pois" = "dashed", "iso" = "solid", "trans" = "dotted")

p <- ggplot(MarkCorrData)+
  geom_line(aes(x = r, y = value, color = data, linetype = method))+
  geom_hline(data = data.frame(y = c(1), col = c("Pois")),
             aes(yintercept = y, color = col, linetype = col))+
  scale_x_continuous(name = "r [m]")+
  scale_y_continuous(name = "Mark Correlation Function [kmm(r)]")+
  scale_color_manual(name = "Analysed data", values = Colors, labels = Labels)+
  scale_linetype_manual(name = "Edge Correction", values = Linetypes, labels = Labels)+
  coord_cartesian(ylim = c(0.74, 1.05)); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/10_MarkCorr_Chris_Shar.png"), 
       height = 4, width = 8)







markcrosscorr(ppp_p, r=r, correction="none",        # spatstat computation
              bw=bw, kernel="gaussian",
              normalise=FALSE)

Tree.m <- data.frame(markcrosscorr(subset(Tree.ppp, select = DBH), r = seq(0, 12.5, length.out = 250),
                                   correction = "none", bw = 0.5, normalise = TRUE)[[1]])
Edge_NoEdge.m <- data.frame(markcrosscorr(as.ppp(Edge_NoEdge.marks, W = owin(c(-400, 400), c(-400, 400))), 
                                          r = seq(0, 12.5, length.out = 250),
                                          correction = "none", bw = 0.5, normalise = TRUE)[[1]])
ChrisData.m <- data.frame(markcrosscorr(as.ppp(ChrisData, W = owin(c(0, 800), c(0, 800))), 
                                        r = seq(0, 12.5, length.out = 250),
                                        correction = "none", bw = 0.5, normalise = TRUE)[[1]])

MarkData <- rbind(cbind(Tree.m, Method = "Shar"), 
                  cbind(Edge_NoEdge.m, Method = "Chris"),
                  cbind(ChrisData.m, Method = "Original"))


Colors <- c("Pois" = 3, "Original" = 1, "Shar" = 2, "Chris" = 5)
Labels <- expression("Pois" = g[Pois](r), "Original" = "Original Data", "Shar" = "Reconstructed Data with shar",
                     "Chris" = "Reconstructed with Chris Function",
                     "un" = "No Edge Correction")
Linetypes = c("Pois" = "dashed", "un" = "solid")

p <- ggplot(MarkData)+
  geom_line(aes(x = r, y = un, color = Method), size = 1)+
  geom_hline(data = data.frame(y = c(1), col = c("Pois")),
             aes(yintercept = y, color = col), linetype = "dashed", size = 1)+
  scale_x_continuous(name = "r [m]")+
  scale_y_continuous(name = "Mark Correlation Function [kmm(r)]")+
  scale_color_manual(name = "Analysed data", values = Colors, labels = Labels)+
  coord_cartesian(ylim = c(0.74, 1.05)); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/11_MarkCorr_noEdge.png"), 
       height = 4, width = 8)





bins <- seq(from = min(MarkData$DBH, na.rm = T), to = max(MarkData$DBH, na.rm = T), length.out = 31)
HistData <- data.frame(Method = "A", BinValue = 1, Count = 1)[-1, ]
for(i in 1:30){
  HistData <- rbind(HistData,
                    data.frame(Method = c("Original", "Shar", "Chris"),
                               BinValue = (bins[i] + bins[i + 1]) / 2,
                               Count = c(nrow(MarkData[MarkData$Method == "Original" & MarkData$DBH >= bins[i] & MarkData$DBH < bins[i + 1] + 0.000001, ]) / nrow(MarkData[MarkData$Method == "Original", ]),
                                         nrow(MarkData[MarkData$Method == "Shar" & MarkData$DBH >= bins[i] & MarkData$DBH < bins[i + 1] + 0.000001, ]) / nrow(MarkData[MarkData$Method == "Shar", ]),
                                         nrow(MarkData[MarkData$Method == "Chris" & MarkData$DBH >= bins[i] & MarkData$DBH < bins[i + 1] + 0.000001, ]) / nrow(MarkData[MarkData$Method == "Chris", ]))))
}

HistData$Method <- factor(HistData$Method, levels = c("Original", "Shar", "Chris"))

p <- ggplot(HistData)+
  geom_rect(aes(xmin = BinValue - (bins[2] - bins[1]) / 2,
                xmax = BinValue + (bins[2] - bins[1]) / 2,
                ymin = 0, ymax = Count * 100))+
  scale_x_continuous(name = "DBH [cm]")+
  scale_y_continuous(name = "relative Count [%]")+
  facet_grid(Method ~ .); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/12_MarkHist.png"), 
       height = 4, width = 8)











write.table(ChrisData, file = "C:/Users/heinermann/Documents/GitHub/ShadingModelOpen/ChrisData.csv", sep = ",", row.names = FALSE)




x <- Edge_NoEdge.marks$x
myx <- ""
for(i in 1:length(x)){
  myx <- paste(myx, round(x[i], digits = 2), sep = " ")
}; myx

write.table(myx, file = "C:/Users/heinermann/Documents/GitHub/ShadingModelOpen/x_Coords.txt", row.names = FALSE)


x <- Edge_NoEdge.marks$y
myx <- ""
for(i in 1:length(x)){
  myx <- paste(myx, round(x[i], digits = 2), sep = " ")
}; myx

write.table(myx, file = "C:/Users/heinermann/Documents/GitHub/ShadingModelOpen/y_Coords.txt", row.names = FALSE)


x <- Edge_NoEdge.marks$DBH
myx <- ""
for(i in 1:length(x)){
  myx <- paste(myx, round(x[i], digits = 2), sep = " ")
}; myx

write.table(myx, file = "C:/Users/heinermann/Documents/GitHub/ShadingModelOpen/DBH.txt", row.names = FALSE)









paste("a", "b", sep = "\t")

myx <- ""
curx <- ""
maxi <- 11
for(i in 1:maxi){
  curx <- paste0(curx, " ", LETTERS[i])
  if(nchar(curx) > 5 | i == maxi){
    myx <- paste0(myx, "\n", curx)
    curx <- ""
  }
}; writeLines(myx)

writeLines(paste("a", "b", sep = "\n"))
nchar("absf sf")




myx <- ""
curx <- ""
maxi <- length(Edge_NoEdge.marks$x)
for(i in 1:maxi){
  curx <- paste0(curx, " ", Edge_NoEdge.marks$x[i])
  if(nchar(curx) > 180 | i == maxi){
    myx <- paste0(myx, "\n", curx)
    curx <- ""
  }
}

writeLines(myx)












#################################################################################################################
#### This can be deleted WHEN finished. ####
#################################################################################################################

if(CalcAll){
  # Create an empty data frame.
  Merged <- data.frame(x = 0, y = 0, iteration = 0)[-1, ]
  WorldSize <- c(-400, 400)   # Set the extended World Size.
  NNew <- ((WorldSize[2] - WorldSize[1]) ^ 2) / (50 ^ 2)  # How many original Sites fit in the new World?
  # Iterate the process of creating a new world with the size of the original World.
  for(i in 1:NNew){
    Tree.recon <- reconstruct_pattern(Tree.ppp, 
                                      n_random = 1, 
                                      max_runs = 1000,
                                      simplify = TRUE, 
                                      window = owin(c(-25, 25), c(-25, 25)),
                                      n_points = nrow(TreeData),
                                      return_input = FALSE,
                                      plot = TRUE)
    Merged <- rbind(Merged, data.frame(x = Tree.recon$x, y = Tree.recon$y, iteration = i))
  }
  # Now merge them together.
  x <- 1
  y <- 1
  for(i in 1:NNew){
    Merged$x[Merged$iteration == i] <- Merged$x[Merged$iteration == i] + x * 50 - 425
    Merged$y[Merged$iteration == i] <- Merged$y[Merged$iteration == i] + y * 50 - 425
    x <- x + 1
    if(x > sqrt(NNew)){
      x <- 1
      y <- y + 1
    }
  }
  Merged <- Merged[, c("x", "y")]
}else{
  Merged <- read.csv("https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/Merged.csv")
}


WorldSize <- c(-400, 400)
NNew <- ((WorldSize[2] - WorldSize[1]) ^ 2) / (50 ^ 2) * nrow(TreeData)
Tree.recon <- reconstruct_pattern_rcustom(Tree.ppp, 
                                  n_random = 1, 
                                  max_runs = 50000,
                                  simplify = TRUE, 
                                  window = owin(WorldSize, WorldSize),
                                  n_points = NNew,
                                  comp_fast = NNew,
                                  return_input = FALSE,
                                  r = 12.5,
                                  plot = TRUE)


Adjusted_r <- data.frame(x = Tree.recon$x, y = Tree.recon$y)


write.table(Edge_NoEdge, file = "C:/Users/heinermann/Documents/GitHub/ShadingModelOpen/Edge_NoEdge.csv", 
            sep = ",", row.names = FALSE)



Adjusted_r <- read.csv("https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/Adjusted_r.csv")




Adjusted_r <- 1



if(CalcAll){
  # Reconstruct data here again:
  # Set World Size and calculate the number of Trees on the bigger Plot.
  WorldSize <- c(-400, 400)
  NNew <- ((WorldSize[2] - WorldSize[1]) ^ 2) / (50 ^ 2) * nrow(TreeData)
  # Now we reconstruct the point positions.
  # This is done using the Shar-Package. It reconstructs the Pair-Correlation-function.
  Tree.recon <- reconstruct_pattern_rcustom(Tree.ppp, 
                                            n_random = 1, 
                                            max_runs = 50000,
                                            simplify = TRUE, 
                                            window = owin(WorldSize, WorldSize),
                                            n_points = NNew,
                                            comp_fast = NNew,
                                            return_input = FALSE,
                                            r = 12.5,
                                            plot = TRUE)
  Adjusted_r <- data.frame(x = Tree.recon$x, y = Tree.recon$y)
}else{
  # Reconstructed Data, Resized to 800 x 800m
  Adjusted_r <- read.csv("https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/Adjusted_r.csv")
}








write.table(TreeData, file = "C:/Users/heinermann/Documents/GitHub/ShadingModelOpen/TreeData.csv", sep = ",", row.names = FALSE)





write.table(data.frame(x = 1:10, y = LETTERS[1:10]), file = "C:/Users/heinermann/Documents/GitHub/ShadingModelOpen/FetchData.csv", sep = ",", row.names = FALSE)


write.table(data.frame(x = 1:10, y = LETTERS[1:10]), 
            file = "C:/Users/heinermann/Documents/GitHub/ShadingModelOpen/FetchData.txt", 
            row.names = FALSE,
            quote = FALSE, fileEncoding = "UTF-8")





fileConn<-file("C:/Users/heinermann/Documents/GitHub/ShadingModelOpen/Fetch2.txt")
writeLines(c("x","y"), fileConn)
close(fileConn)








ggplot(Merged)+
  geom_point(aes(x = x, y = y))



# How many different point patterns do we want to construct?
n_pattern <- 1

# Set World Size and calculate the number of Trees on the bigger Plot.
WorldSize <- c(-50, 50)
NNew <- ((WorldSize[2] - WorldSize[1]) ^ 2) / (50 ^ 2) * nrow(TreeData)
# Now we reconstruct the point positions.
# This is done using the Shar-Package. It reconstructs the Pair-Correlation-function.
Tree.recon <- reconstruct_pattern(Tree.ppp, 
                                  n_random = 1, 
                                  max_runs = 1000,
                                  simplify = TRUE, 
                                  window = owin(WorldSize, WorldSize),
                                  n_points = NNew,
                                  return_input = FALSE,
                                  # method = "cluster",
                                  # r_length = 250,
                                  plot = TRUE)

# Show the Pair-Correlation-Function for the original and the reconstructed pattern.
pcf_Org <- data.frame(pcf(Tree.ppp, r = seq(0, 25, length.out = 250)))[-1, ]
pcf_recon <- data.frame(pcf(Tree.recon, r = seq(0, 25, length.out = 250)))[-1, ]
ggplot(pcf_Org)+
  geom_line(aes(x = r, y = iso))+
  geom_line(data = pcf_recon, aes(x = r, y = iso), color = "red")


marks_sub <- spatstat.geom::subset.ppp(Tree.ppp, select = DBH)
marks_recon <- reconstruct_pattern_marks(Tree.recon, 
                                         marks_sub,
                                         n_random = 1, 
                                         max_runs = 10000,
                                         e_threshold = 0.002,
                                         plot = TRUE)

# Save x, y and DBH of reconstructed data.
Tree.marks <- data.frame(x = marks_recon$randomized$randomized_1$x,
                         y = marks_recon$randomized$randomized_1$y,
                         DBH = marks_recon$randomized$randomized_1$marks)

# Show the Mark-Correlation-Function of the reconstructed and original data.
Tree.m <- data.frame(markcorr(subset(Tree.ppp, select = DBH), r = seq(0, 25, length.out = 250)))
Recon.m <- data.frame(markcorr(as.ppp(Tree.marks, W = owin(c(-50, 50), c(-50, 50))), 
                               r = seq(0, 25, length.out = 250)))
ggplot(Tree.m)+
  geom_line(aes(x = r, y = iso))+
  geom_line(data = Recon.m, aes(x = r, y = iso), color = "red")


# Calculate Clark-Evans-Index.
clarkevansCalc(Tree.ppp)
clarkevansCalc(Tree.recon)



NewWorld <- data.frame(x = 0, y = 0, iteration = 0)[-1, ]

WorldSize <- c(-50, 50)
NNew <- ((WorldSize[2] - WorldSize[1]) ^ 2) / (50 ^ 2) * nrow(TreeData)

for(i in 1:64){
  Tree.recon <- reconstruct_pattern(Tree.ppp, 
                                    n_random = 1, 
                                    max_runs = 1000,
                                    simplify = TRUE, 
                                    window = owin(WorldSize, WorldSize),
                                    n_points = NNew,
                                    return_input = FALSE,
                                    # method = "cluster",
                                    # r_length = 250,
                                    plot = TRUE)
  NewWorld <- rbind(NewWorld, data.frame(x = Tree.recon$x, y = Tree.recon$y, iteration = i))
}

x <- 1
y <- 1
for(i in 1:64){
  NewWorld$x[NewWorld$iteration == i] <- NewWorld$x[NewWorld$iteration == i] + x * 100 - 50
  NewWorld$y[NewWorld$iteration == i] <- NewWorld$y[NewWorld$iteration == i] + y * 100 - 50
  x <- x + 1
  if(x > 8){
    x <- 1
    y <- y + 1
  }
}

ggplot(NewWorld)+
  geom_point(aes(x = x, y = y))


pcf_NewWorld <- data.frame(pcf(as.ppp(NewWorld, W = owin(c(0, 800), c(0, 800))), r = seq(0, 25, length.out = 250)))[-1, ]

ggplot(pcf_Org)+
  geom_line(aes(x = r, y = iso))+
  geom_line(data = pcf_NewWorld, aes(x = r, y = iso), color = "red")


NewWorld$x <- NewWorld$x - 400
NewWorld$y <- NewWorld$y - 400
New.ppp <- as.ppp(NewWorld[, 1:2], W = owin(c(-400, 400), c(-400, 400)))

marks_sub <- spatstat.geom::subset.ppp(Tree.ppp, select = DBH)
marks_recon <- reconstruct_pattern_marks(New.ppp, 
                                         marks_sub,
                                         n_random = 1, 
                                         max_runs = 10000,
                                         e_threshold = 0.002,
                                         plot = TRUE)



# Set World Size and calculate the number of Trees on the bigger Plot.
WorldSize <- c(-400, 400)
NNew <- ((WorldSize[2] - WorldSize[1]) ^ 2) / (50 ^ 2) * nrow(TreeData)
# Now we reconstruct the point positions.
# This is done using the Shar-Package. It reconstructs the Pair-Correlation-function.
Tree.recon <- reconstruct_pattern(Tree.ppp, 
                                  n_random = 1, 
                                  max_runs = 50000,
                                  simplify = TRUE, 
                                  window = owin(WorldSize, WorldSize),
                                  n_points = NNew,
                                  return_input = FALSE,
                                  e_threshold = 0.002,
                                  # method = "cluster",
                                  # r_length = 250,
                                  plot = TRUE)



AllInOne <- data.frame(x = Tree.recon$x, y = Tree.recon$y)
ggplot(AllInOne)+
  geom_point(aes(x = x, y = y))


TileSize <- 40
nTiles <- ((WorldSize[2] - WorldSize[1]) ^ 2) / (TileSize ^ 2)
TileData <- data.frame(x = 1, y = 1, nTrees = 0)[-1, ]
x <- -400
y <- -400
for(i in 1:nTiles){
  TileData <- rbind(TileData, data.frame(x = x, y = y, 
                                         nTrees = sum(AllInOne$x >= x & AllInOne$x < (x + TileSize) &
                                                        AllInOne$y >= y & AllInOne$y < (y + TileSize))))
  x <- x + TileSize
  if(x >= WorldSize[2]){
    x <- -400
    y <- y + TileSize
  }
}

p <- ggplot(TileData)+
  geom_raster(aes(x = x + TileSize / 2, y = y + TileSize / 2, fill = nTrees))+
  scale_x_continuous(name = "X-Coordinate", expand = c(0, 0))+
  scale_y_continuous(name = "Y-Coordinate", expand = c(0, 0))+
  scale_fill_continuous(name = paste0("Number of Trees\nin an area of\n", TileSize, " x ", TileSize, "m"))+
  coord_fixed(); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/03_Reconstruct_Patter_in_One.png"), 
       height = 4, width = 6)


pcf_Org <- data.frame(pcf(Tree.ppp, r = seq(0, 100, length.out = 250)))[-1, ]
pcf_recon <- data.frame(pcf(as.ppp(AllInOne, W = owin(WorldSize, WorldSize)), r = seq(0, 100, length.out = 250)))[-1, ]

p <- ggplot(pcf_Org)+
  geom_line(aes(x = r, y = iso))+
  geom_line(data = pcf_recon, aes(x = r, y = iso), color = "red")+
  geom_hline(data = data.frame(y = c(1, -1), col = c("a", "b")),
             aes(yintercept = y, color = col), linetype = "dashed")+
  scale_x_continuous(name = "radius")+
  scale_y_continuous(name = "Pair Correlation Function [g(r)]")+
  scale_color_manual(name = "", values = c(1, 2), labels = c("original Data", "reconstructed Data"))+
  coord_cartesian(ylim = c(0, 1.25)); p

ggsave(p, file = GetFile("Plots/Pattern_Reconstruction/04_PCF_Patter_in_One.png"), 
       height = 4, width = 6)






Stacked <- data.frame(x = 0, y = 0, iteration = 0)[-1, ]
WorldSize <- c(-400, 400)
NNew <- ((WorldSize[2] - WorldSize[1]) ^ 2) / (50 ^ 2)
for(i in 1:NNew){
  Tree.recon <- reconstruct_pattern(Tree.ppp, 
                                    n_random = 1, 
                                    max_runs = 1000,
                                    simplify = TRUE, 
                                    window = owin(c(-25, 25), c(-25, 25)),
                                    n_points = NNew,
                                    return_input = FALSE,
                                    comp_fast = NNew,
                                    plot = TRUE)
  Stacked <- rbind(Stacked, data.frame(x = Tree.recon$x, y = Tree.recon$y, iteration = i))
}

x <- 1
y <- 1
for(i in 1:64){
  NewWorld$x[NewWorld$iteration == i] <- NewWorld$x[NewWorld$iteration == i] + x * 50 - 25
  NewWorld$y[NewWorld$iteration == i] <- NewWorld$y[NewWorld$iteration == i] + y * 50 - 25
  x <- x + 1
  if(x > sqrt(NNew)){
    x <- 1
    y <- y + 1
  }
}








Tree.recon <- reconstruct_pattern_custom(Tree.ppp, 
                                  n_random = 1, 
                                  max_runs = 20,
                                  noEdge_runs = 10,
                                  simplify = TRUE, 
                                  window = owin(c(-400, 400), c(-400, 400)),
                                  n_points = NNew,
                                  return_input = FALSE,
                                  comp_fast = NNew - 1,
                                  plot = TRUE)















myx <- ""
for(i in 1:length(NewTrees$x)){
  myx <- paste(myx, round(NewTrees$x[i], digits = 2), sep = ", ")
}; myx










Recon.ppp <- as.ppp(Tree.marks, W = owin(c(-50, 50), c(-50, 50)))
plot(markcorr(Recon.ppp))
lines(markcorr(subset(Tree.ppp, select = DBH)))
plot(markcorr(subset(Tree.ppp, select = DBH)))

clarkevansCalc(Tree.ppp)
clarkevansCalc(Tree.recon)

Tree.k <- Kest(Tree.ppp)
Recon.k <- Kest(Tree.recon)
##
plot(Tree.k$r, Tree.k$iso, type = "l")
lines(Recon.k$r, Recon.k$iso, type = "l", col = "red")


Tree.L <- Lest(Tree.ppp)
Recon.L <- Lest(Tree.recon)
plot(Tree.L$r, Tree.L$iso, type = "l")
lines(Recon.L$r, Recon.L$iso, type = "l", col = "red")
##
Tree.p <- pcf(Tree.ppp)
Recon.p <- pcf(Tree.recon)
plot(Tree.p$r, Tree.p$iso, type = "l")
lines(Recon.p$r, Recon.p$iso, type = "l", col = "red")


Tree.m <- markcorr(subset(Tree.ppp, select = DBH), r = seq(0, 25, length.out = 250))
Recon.m <- markcorr(as.ppp(Tree.marks, W = owin(c(-50, 50), c(-50, 50))), 
                    r = seq(0, 25, length.out = 250))
plot(Tree.m$r, Tree.m$iso, type = "l")
lines(Recon.m$r, Recon.m$iso, type = "l", col = "red")

plot(markcorr(subset(Tree.ppp, select = DBH), r = seq(0, 12.5, length.out = 250)))
plot(markcorr(Recon.ppp, r = seq(0, 12.5, length.out = 250)))


ggplot(TreeData)+
  geom_point(data = ReconData, aes(x = x, y = y, size = DBH))+
  geom_point(aes(x = x, y = y, size = DBH), color = "green")+
  coord_fixed()

ggplot(TreeData)+
  geom_density(aes(x = DBH), color = "green", size = 2)+
  geom_density(data = Tree.marks, aes(x = DBH), size = 2)





myx <- ""
for(i in 1:length(Tree.marks$x)){
  myx <- paste(myx, round(Tree.marks$DBH[i], digits = 1), sep = ", ")
}; myx








overstory_data <- read_excel("E:/Dokumente/VERMOS/Flaechenaufnahmen/Versuch.xlsx", 
                             sheet = "5138_Oberstand", col_types = c("text", 
                                                                     "text", "numeric", "numeric", "text", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "text"))

colnames(overstory_data) <- c("District", "Stand_ID", "Plot_ID", "Tree_ID", "Species", "Species_ID",
                              "X", "Y", "DBH", "Height", "Crown_Start", "r_crown_N", "r_crown_E",
                              "r_crown_S", "r_crown_W", "Comment")


ggplot(overstory_data)+
  geom_point(aes(x = X, y = Y, size = DBH))+
  geom_vline(xintercept = c(-25, 25))+
  geom_hline(yintercept = c(-25, 25))+
  geom_point(data = TreeData, aes(x = x, y = y, size = DBH), color = "red", alpha = 0.5)+
  coord_fixed()

ggplot(overstory_data)+
  geom_point(aes(x = DBH, y = Height))



overstory_data$X


long <- c("44.7	27.6	43.8	38.0	45.3	44.1	27.2	30.5	31.8	33.0	35.3	47.3	32.4	35.8	37.7	39.8	28.9	43.9	34.8	29.9	30.1	34.9	30.8	36.7	32.1	44.6	41.6	31.4	31.8	30.1	29.2	38.5	30.1	34.6	35.9	30.0	31.5	41.1	34.1	39.8	40.8	34.8	34.3	40.4	37.7	35.6	40.8	37.3	32.9	35.2	37.1	32.3	38.9	32.6	42.0	41.7	39.8	41.7	34.3	36.1	34.1	31.1	33.1	41.8	39.1	27.7	35.9	35.5	38.2	29.8	34.2	33.1	29.0	27.8	39.6	36.4	35.0	31.8	27.4	39.7	26.1	27.5	35.2	35.7	30.1	29.2	43.6	32.1	28.3	39.9	52.2	33.0	31.2	28.9	32.4	30.5	30.3")
long <- gsub("\t", ", ", long)
as.character(strsplit(long, "[, ]"))
as.numeric(strsplit(long, "\t")[[1]])


myx <- ""
for(i in 1:length(ReconData$x)){
  myx <- paste(myx, round(ReconData$DBH[i], digits = 1), sep = " ")
}; myx

ReconData$x


data("species_a")
pattern_recon <- reconstruct_pattern(species_a, n_random = 1, max_runs = 1000,
                                     simplify = TRUE, return_input = FALSE)
marks_sub <- spatstat.geom::subset.ppp(species_a, select = dbh)
marks_recon <- reconstruct_pattern_marks(pattern_recon, marks_sub,
                                         n_random = 19, max_runs = 1000)

plot(species_a)







.from <- 1
.to <- 3.2
.by <- 0.5

out <- .from
lower <- .from + floor((.to - .from) / .by) * .by
x <- .from
while(x < lower){
  x <- x + .by
  out <- c(out, x)
}



seq(.from, .to, .by)









