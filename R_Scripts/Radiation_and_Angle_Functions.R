# Script with Functions for Radiation and Angles

# We define Solar Position Functions and Functions of the Clear-Sky-Model in here. 
# You can use the Functions to calculate Sun Position, Sunrise and Sunset and Direct Beam Radiation for 5 Latitudes.

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

tandeg <- function(x){
  tan(x * pi / 180)
}


# Is number x divisible by another number?
is.divisible <- function(x, by){
  (x / by) == round(x / by)
}


#############################################################################################################################
#### Angle Functions (Functions are equal to NetLogo-Functions) ####
#############################################################################################################################

# Calculation of Earth Declination Angle (Tilt of Earth during one Year), (Naraghi, 2010)
EarthDecAngle <- function(DOY){
  23.45 * sin(2 * pi / 365 * (DOY + 284))
}

# Position of Earth relative to sun depending on the Day of Year.
YearAngle <- function(DOY){
  360 * (DOY / 365)
}

# Calculate the Equation of Time (Quaschning, 2015)
EquationOfTime <- function(DOY){
  year_angle <- YearAngle(DOY)
  a <- 0.0066 + 7.3525 * cosdeg(year_angle + 85.9)
  b <- 9.9359 * cosdeg(2 * year_angle + 108.9)
  c <- 0.3387 * cosdeg(3 * year_angle + 105.2)
  a + b + c
}

# Half-Length of the Day by Day of Year and Latitude (altered after Amthor, 1997)
Daylength <- function(DOY, Latitude){
  # At high latitudes, the Sun never goes down. In this case...
  ifelse(((sin(-0.0145439) - sindeg(Latitude) * sindeg(EarthDecAngle(DOY))) / (cosdeg(Latitude) * cosdeg(EarthDecAngle(DOY)))) < -1, 
         12,    # ... set Half-Daylength to 12 hours, else calculate Half-DayLength.
         acos((sin(-0.0145439) - sindeg(Latitude) * sindeg(EarthDecAngle(DOY))) /
                (cosdeg(Latitude) * cosdeg(EarthDecAngle(DOY)))) * 12 / pi)
}

# Calculation of the time of Sunrise [12 (solar noon) - Daylength - Elliptical Correction of Earth's Orbit] (Amthor, 1997)
Sunrise <- function(DOY, Latitude){
  12 - Daylength(DOY, Latitude) - EquationOfTime(DOY) * pi / 180
}

# Calculation of the time of Sunset (Amthor, 1997)
Sunset <- function(DOY, Latitude){
  12 + Daylength(DOY, Latitude) - EquationOfTime(DOY) * pi / 180
}

# At each hour, the sun moves 15° (360° / 24h). 0° is defined as noon.
HourAngle <- function(Hour, DOY, Latitude){
  HA <- (12 - Hour) * 15
  HA[Hour < Sunrise(DOY, Latitude) | Hour > Sunset(DOY, Latitude)] <- NA
  HA
}

# Angle of Sun above Horizon (Quaschning, 2015)
SolarAltitude <- function(Hour, DOY, Latitude){
  asindeg(cosdeg(Latitude) * cosdeg(HourAngle(Hour, DOY, Latitude)) * cosdeg(EarthDecAngle(DOY)) + sindeg(Latitude) * sindeg(EarthDecAngle(DOY)))
}

# Solar Azimuth (Sun's horizontal Position (0° = North))
SolarAzimuth <- function(Hour, DOY, Latitude){
  SA <- NA
  for(i in 1:length(Hour)){
    plusminus <- acosdeg(round((sindeg(SolarAltitude(Hour[i], DOY, Latitude)) * sindeg(Latitude) - sindeg(EarthDecAngle(DOY))) / 
                                 (cosdeg(SolarAltitude(Hour[i], DOY, Latitude)) * cosdeg(Latitude)), digits = 8))
    if(Hour[i] < 12){
      SA[i] <- 180 - plusminus
    }else {
      SA[i] <- 180 + plusminus
    }
  }
  SA
}

# Calculation of solar beam optical depths and solar diffuse optical depths (Only for Lindenberg, Germany)
calctau <- function(DOY){
  # Values taken from: http://ashrae-meteo.info/v2.0/index.php?lat=40.97&lng=28.82&place=%27%27&wmo=170600&ashrae_version=2009
  # Values for Lindenberg, Germany
  taub <- c(0.359, 0.364, 0.424, 0.443, 0.451, 0.465, 0.487, 0.469, 0.400, 0.387, 0.361, 0.363)
  taud <- c(2.191, 2.142, 1.923, 1.915, 1.927, 1.932, 1.874, 1.936, 2.160, 2.146, 2.208, 2.206)
  list(taub = taub[as.numeric(format(as.Date(paste0("2000-", DOY), "%Y-%j"), "%m"))],
       taud = taud[as.numeric(format(as.Date(paste0("2000-", DOY), "%Y-%j"), "%m"))])
}

# Calculation of solar beam optical depths and solar diffuse optical depths (Depending on Latitude)
calctauLat <- function(DOY, Latitude){
  # Values taken from: http://ashrae-meteo.info/v2.0/index.php?lat=40.97&lng=28.82&place=%27%27&wmo=170600&ashrae_version=2009
  # Values for Lindenberg, Germany
  MyInd <- which(abs(Latitude - c(69.280, 52.217, 45.180, 30.125, 17.539)) == min(abs(Latitude - c(69.280, 52.217, 45.180, 30.125, 17.539))))
  taub <- list(c(0.158, 0.207, 0.246, 0.292, 0.332, 0.328, 0.355, 0.339, 0.303, 0.232, 0.155, 0.000),  # Croker River
               c(0.323, 0.350, 0.390, 0.412, 0.410, 0.407, 0.427, 0.421, 0.383, 0.373, 0.348, 0.317),  # Lindenberg
               c(0.322, 0.319, 0.331, 0.359, 0.370, 0.362, 0.352, 0.355, 0.345, 0.334, 0.327, 0.328),  # MCMINNVILLE
               c(0.342, 0.358, 0.376, 0.431, 0.466, 0.501, 0.501, 0.488, 0.458, 0.383, 0.367, 0.352),  # LAKE CHARLES
               c(0.373, 0.383, 0.411, 0.477, 0.511, 0.472, 0.503, 0.470, 0.437, 0.411, 0.388, 0.386))  # Phillip SW BELIZE
  taud <- list(c(1.887, 2.049, 2.188, 2.127, 2.131, 2.357, 2.382, 2.480, 2.491, 2.170, 1.919, 0.000),  # Croker River
               c(2.191, 2.142, 1.923, 1.915, 1.927, 1.932, 1.874, 1.936, 2.160, 2.146, 2.208, 2.206),  # Lindenberg
               c(2.518, 2.535, 2.504, 2.392, 2.378, 2.411, 2.460, 2.467, 2.488, 2.527, 2.510, 2.463),  # MCMINNVILLE
               c(2.511, 2.476, 2.419, 2.263, 2.200, 2.139, 2.181,	2.218, 2.268,	2.462, 2.449, 2.484),  # LAKE CHARLES
               c(2.555,	2.518, 2.421,	2.222, 2.143, 2.302, 2.176, 2.293, 2.392, 2.491, 2.531, 2.533))  # Phillip SW BELIZE
  list(taub = taub[[MyInd]][as.numeric(format(as.Date(paste0("2000-", DOY), "%Y-%j"), "%m"))],
       taud = taud[[MyInd]][as.numeric(format(as.Date(paste0("2000-", DOY), "%Y-%j"), "%m"))])
}

# Calculation of Direct Beam Radiation after Naraghi (2010)
DirectBeam <- function(Hour, DOY, Latitude){
  m <- 1 / (sindeg(SolarAltitude(Hour, DOY, Latitude)) + 0.50572 * ((6.07995 + SolarAltitude(Hour, DOY, Latitude)) ^ (-1.6364)))
  E0 <- 1367 * (1 + 0.033 * cosdeg(360 * ((DOY - 3) / 365)))
  rb <- calctau(DOY)[[1]] # Solar Beam Optical Depth -> Values taken from Website (see Function)
  rd <- calctau(DOY)[[2]] # Diffuse Optical Depth
  ab <- 1.219 - 0.043 * rb - 0.151 * rd - 0.204 * rb * rd  # can be found between Eq. 8 and Eq. 9
  ad <- 0.202 + 0.852 * rb - 0.007 * rd - 0.357 * rb * rd
  E0 * exp(-1 * rb * (m ^ ab))
}

# Calculation of Direct Beam Radiation after Naraghi (2010) for different Latitudes
DirectBeamLat <- function(Hour, DOY, Latitude){
  m <- 1 / (sindeg(SolarAltitude(Hour, DOY, Latitude)) + 0.50572 * ((6.07995 + SolarAltitude(Hour, DOY, Latitude)) ^ (-1.6364)))
  E0 <- 1367 * (1 + 0.033 * cosdeg(360 * ((DOY - 3) / 365)))
  rb <- calctauLat(DOY, Latitude)[[1]] # Solar Beam Optical Depth -> Values taken from Website (see Function)
  rd <- calctauLat(DOY, Latitude)[[2]] # Diffuse Optical Depth
  ab <- 1.219 - 0.043 * rb - 0.151 * rd - 0.204 * rb * rd  # can be found between Eq. 8 and Eq. 9
  ad <- 0.202 + 0.852 * rb - 0.007 * rd - 0.357 * rb * rd
  E0 * exp(-1 * rb * (m ^ ab))
}



# Calculation of Shading of an Ellipsoid Crown (Quesada, 2012; Somarriba et al., 2023)
getShadow <- function(x0, y0, CrownRadius, Height, CrownHeight, Hour, DOY, Latitude, npoints = 101){
  outdata <- data.frame(x = 1, y = 1, group = 1, Rad = 1)[-1, ]
  for(i in 1:length(Hour)){
    alph <- SolarAzimuth(Hour[i], DOY, Latitude) + 180
    u <- cosdeg(alph)
    v <- sindeg(alph)
    a <- CrownRadius 
    b <- sqrt((CrownHeight / 2) ^ 2 / (tan(SolarAltitude(Hour[i], DOY, Latitude) * pi / 180) ^ 2) + CrownRadius ^ 2)
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




#############################################################################################################################
#### References: ####
#############################################################################################################################
# Amthor, J. S. (1997): Calculation of daylength. In: Computer applications in the biosciences: CABIOS 13 (4), S. 479–480. DOI: 10.1093/bioinformatics/13.4.479.
# Naraghi, Mohammed (2010). A Demand Based Optimum Solar Panel Orientation. DOI: 10.1115/IMECE2010-37918
# Quaschning, Volker (2015): Regenerative Energiesysteme Technologie - Berechnung - Simulation. ISBN: 978-3-446-44267-2
# Quesada, F. (2021): Derivaci ́on de las f ́ormulas de sombras de  ́arboles empleadas en el programa shademoton’. URL: http://hdl.handle.net/11554/1135
# Somarriba, E., Quesada, F., Barrantes, J., Zamora, R., Malek, M., Vargas, E., Sinclair, F., (2023): Shademotion. URL https://www.shademotion.n









