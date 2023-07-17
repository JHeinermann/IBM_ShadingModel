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
# Altered reconstruction-function of reconstruct_pattern_homo of "shar"-Package.
# A Variable "r" is included that defines the maximum limit of the radius-input for Pair-Correlation-Function.
source("R_Scripts/Reconstruction_Functions.R")



#################################################################################################################
#### Load Data ####
#################################################################################################################
# Artificial Data:
TreeData <- data.frame(x =           runif(n = 63, min = -25, max = 25),
                       y =           runif(n = 63, min = -25, max = 25),
                       DBH =         rnorm(n = 63, mean = 35, sd = 5),
                       Height =      rnorm(n = 63, mean = 28, sd = 2),
                       CrownRadius = rnorm(n = 63, mean = 2.3, sd = 0.5))


#################################################################################################################
#### Analysis of Original Data ####
#################################################################################################################
# Stem Positions
p <- ggplot(TreeData)+
  geom_circle(aes(x0 = x, y0 = y, r = CrownRadius), fill = "darkgreen")+
  scale_x_continuous(name = "X-Coordinate")+
  scale_y_continuous(name = "Y-Coordinate")+
  coord_fixed(); p


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
  AllInOne <- read.csv("Data/Reconstruction_PointPatterns/AllInOne.csv")
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
  Merged <- read.csv("Data/Reconstruction_PointPatterns/Merged.csv")
}

# 3. Reconstruct data and adjust the radius of Pair-Correlation-Function.
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
  Adjusted_r <- read.csv("Data/Reconstruction_PointPatterns/Adjusted_r.csv")
}


# We found that the third Option of Reconstruction resulted in good Fit of the Pair-Correlation-Function in general but 
# Trees were sometimes too close to each other. So we tried multiple things to have a better fit especially at 
# small distances. We found that ignoring edge-correction makes the process a lot faster and thus we
# could increase the runs of the function. This resulted in a better fit at small distances but worse fit 
# at bigger distances. We therefore increased the number of runs once more and then combined runs without
# edge correction with runs with edge correction which gave us the best fit.


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
  No_Edge <- read.csv("Data/Reconstruction_PointPatterns/No_Edge.csv")
}



# Reconstruct Pattern without edge correction with 200.000 runs
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
  No_Edge_4 <- read.csv("Data/Reconstruction_PointPatterns/No_Edge_4.csv")
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
  Edge_NoEdge <- read.csv("Data/Reconstruction_PointPatterns/Edge_NoEdge.csv")
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


# Plot Density of Tile-Numbers.
p <- ggplot(TileData)+
  geom_density(aes(x = nTrees, color = method))+
  scale_color_manual(name = "Analysed data", values = Colors[3:8], labels = Labels); p

# As we see above, when we calculate all in one go, then we end up with a plot where all Trees are placed at the Edges of the plot. 
# This does not seem desirable. We should therefore continue with the merged Plots.




#################################################################################################################
#### Reconstruction of Marks ####
#################################################################################################################
WorldSize <- c(-400, 400)
Edge_NoEdge.ppp <- as.ppp(Edge_NoEdge, W = owin(WorldSize, WorldSize))

# Reconstruct marks using shar and the previously reconstructed Point Pattern "Edge_noEdge".
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
  Edge_NoEdge.marks <- read.csv("Data/Reconstruction_PointPatterns/Edge_NoEdge_marks.csv")
}

# Now we want to see how the reconstructed Data looks like.
# We calculate Mark-Correlation-Functions of the Original and the Reconstructed Plot.

Tree.m <- data.frame(markcorr(subset(Tree.ppp, select = DBH), r = seq(0, 12.5, length.out = 250)))
Edge_NoEdge.m <- data.frame(markcorr(as.ppp(Edge_NoEdge.marks, W = owin(c(-400, 400), c(-400, 400))), 
                                     r = seq(0, 12.5, length.out = 250)))

MarkCorrData <- data.frame(r = seq(0, 12.5, length.out = 250),
                           data = rep(c("Original", "Reconstructed"), each = 500),
                           method = rep(rep(c("iso", "trans"), each = 250), 2),
                           value = c(Tree.m$iso, Tree.m$trans, Edge_NoEdge.m$iso, Edge_NoEdge.m$trans))

Colors <- c("Pois" = 3, "Original" = 1, "Reconstructed" = 2)
Labels <- expression("Pois" = g[Pois](r), "Original" = "Original Data", "Reconstructed" = "Reconstructed Data",
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


# The reconstructed Function looks really good. Let's look at the distribution of DBH's to see if the reconstructed DBH's are representative of the Original Plot.
MarkData <- rbind(cbind(TreeData[, 1:3], Method = "Original"), 
                  cbind(Edge_NoEdge.marks, Method = "Shar"))


bins <- seq(from = min(MarkData$DBH, na.rm = T), to = max(MarkData$DBH, na.rm = T), length.out = 31)
HistData <- data.frame(Method = "A", BinValue = 1, Count = 1)[-1, ]
for(i in 1:30){
  HistData <- rbind(HistData,
                    data.frame(Method = c("Original", "Shar"),
                               BinValue = (bins[i] + bins[i + 1]) / 2,
                               Count = c(nrow(MarkData[MarkData$Method == "Original" & MarkData$DBH >= bins[i] & MarkData$DBH < bins[i + 1] + 0.000001, ]) / nrow(MarkData[MarkData$Method == "Original", ]),
                                         nrow(MarkData[MarkData$Method == "Shar" & MarkData$DBH >= bins[i] & MarkData$DBH < bins[i + 1] + 0.000001, ]) / nrow(MarkData[MarkData$Method == "Shar", ]))))
}
HistData$Method <- ifelse(HistData$Method == "Original", "Original Data", "Reconstructed with Shar")
HistData$Method <- factor(HistData$Method, levels = c("Original Data", "Reconstructed with Shar"))

p <- ggplot(HistData)+
  geom_rect(aes(xmin = BinValue - (bins[2] - bins[1]) / 2,
                xmax = BinValue + (bins[2] - bins[1]) / 2,
                ymin = 0, ymax = Count * 100))+
  scale_x_continuous(name = "DBH [cm]")+
  scale_y_continuous(name = "relative Count [%]")+
  facet_grid(Method ~ .); p

# The reconstructed Data has a good enough fit and thus, this method is used to reconstruct 10 different Plots as Input for our Stand-Model.





