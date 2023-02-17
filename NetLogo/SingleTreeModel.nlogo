;____________________________________________________________________________________________________________________________________________________________
;_____ Define Variables of World and Entities
;____________________________________________________________________________________________________________________________________________________________

extensions [csv]

globals [
  DOY                 ; (Julian) Day of the Year [1 - 365]
  MOY                 ; Month of the Year        [1 - 12]
  hour                ; Hour                     [1-24]
  SolarAltitude       ; Solar Altitude           [°] 0° = Horizon
  SolarAzimuth        ; Solar Azimuth            [°] 0° = North
  Min_SolarAltitude   ; Minimum Solar Altitude Angle given the shadow-length of the Tree in the middle [°]
  Radiation           ; Radiation Intensity      [W/m²]
  RadiationTotal
  weatherStationV     ; Wheather Station Number  [0 - 4]

  PatchesDisk         ; Patches-Set that is shaded using Disk Crowns

  PatchEll
  pArea
  blocked
  Rad_rel
]

breed [trees tree]    ; Tree Breed

trees-own [
  Height              ; Height of the Tree        [m]
  CrownRadius         ; Crown Radius of the Tree  [m]
]

patches-own [
  plight              ; Radiation-Energy on a patch at one time step                            [W/m²]
  RadiationSum        ; Sum of the Radiation-Energy to calculate the total energy over a year  [W/m²]
  RadiationBlocked

]


;____________________________________________________________________________________________________________________________________________________________
;_____ Model Procedures start here!
;____________________________________________________________________________________________________________________________________________________________

;___________________________________________________________________________________________________________________________________________
;_____ Model Setup Procedures
;___________________________________________________________________________________________________________________________________________
to setup
  ca            ; Clear all previous variables.
  reset-ticks   ; Reset the time counter.
  setup-World   ; Create the world. Set global variables.
  setup-Trees   ; Create trees. Set tree variables.

end

; Set Global Variables
to setup-World
  set hour 0    ; Set the current hour to 0 (of 24).
  set DOY 1     ; Set (Julian) the Day of Year to 1 (of 365; XXXX-01-01).

  ; A Weather-Station is set in the User Interface. In this Model, the data needed from the Weather Stations is
  ; saved in a list (see at  end of model). To get access to this list, the Weather-Station is converted into a list index (from 0 to 4).
  if WeatherStation = "Phillip SW Belize" [set WeatherStationV 0]
  if WeatherStation = "Lake Charles" [set WeatherStationV 1]
  if WeatherStation = "McMinnville" [set WeatherStationV 2]
  if WeatherStation = "Lindenberg" [set WeatherStationV 3]
  if WeatherStation = "Croker river" [set WeatherStationV 4]

  ; Get the Latitude from the Weather-Station data.
  set Latitude item 1 item WeatherStationV StationData

  ; Then initialize patches...
  ask patches [
    set pcolor white          ; ... by setting their color to white (light)...
    set RadiationSum 0        ; ... and setting their total Radiation over the course of a year to 0.
    set plight 1
  ]
  set PatchesDisk (patch-set)  ; Create a default Patches-Set (it is used in a later procedure and produces errors if not defined here).

  calc-Min_SolarAltitude
  set PatchEll (patch-set)
end

; Create Trees and set Tree Variables
to setup-Trees
  create-trees 1 [                   ; Create one Tree...
    set shape "circle"
    set Height TreeHeight            ; ... set the height to a height defined in the User Interface...
    set CrownRadius TreeCrownRadius  ; ... set the crown radius to the crown radius defined in the UI.
    set color green
    set size CrownRadius * 2
  ]
end

; Calculate the Minimum Solar Altitude Angle at which the shadow-length does not exceed the world size.
to calc-Min_SolarAltitude
  let MaxShadowLength (max-pxcor - TreeCrownRadius)                                          ; The Maximum Shadow Length [m] given the World Size.
  ifelse Crown_Shape = "Disk" [
    set Min_SolarAltitude asin (TreeHeight / (sqrt (TreeHeight ^ 2 + MaxShadowLength ^ 2)))  ; Calculate the Minimum Solar Altitude Angle [°] below which the shadow exceeds the world boundaries.
  ][
    set Min_SolarAltitude 5
  ]

end

;__________________________________________________________________________________________________________________________________________
;_____ Update Procedures
;___________________________________________________________________________________________________________________________________________
; Updating procedures._
to go
  settime    ; update all time related variables.

  ; Calculate Shading.
  ;If the Day of Year is inside the Vegetation Period and the Hour is between sunrise and sunset...
  if DOY >= StartofGrowingSeason AND DOY <= EndofGrowingSeason AND hour > Sunrise AND hour < Sunset [
    calc-solar-angles-and-radiation                     ; ... calculate the Position of the Sun and the Radiation Intensity.
    if SolarAltitude > Min_SolarAltitude [
      ifelse Crown_Shape = "Disk" [                     ; In case the Shadow does not exceed the World Size, Calculate the Shadows.
        set-shadow
      ][
        set-shadow-ellipsoid
      ]
    ]
  ]

  tick
  ; If one year is over...
  if DOY = 365 AND hour > 23 - 0.99 * TimeIncrement / 60 [
    set DOY 1     ; ... reset the Day of Year to 1...
    set hour 0    ; ... and the hour to 0.
    PlotShadowOverYear  ; Plot the total Shading Pattern for the whole year.
    stop                ; Stop the Simulation.
  ]
end

; Update time related variables.
to settime
  set hour hour + TimeIncrement / 60             ; Ad the time-increment (set in the UI) to the hour.
  if hour >= (24 - 0.99 * TimeIncrement / 60) [  ; If the hour is above or equal to 24 (also taking little rounding errors into account)...
    set hour 0                                   ; ... Reset the hour to 0...
    set DOY DOY + 1                              ; ... and move to the next day.
  ]
  ; Monthly Wheather Data is used so we need the current Month of the Year.
  if DOY < 32 [set MOY 0]
  if DOY > 31 and DOY < 60 [set MOY 1]
  if DOY > 59 and DOY < 91 [set MOY 2]
  if DOY > 90 and DOY < 121 [set MOY 3]
  if DOY > 120 and DOY < 152 [set MOY 4]
  if DOY > 151 and DOY < 182 [set MOY 5]
  if DOY > 181 and DOY < 213 [set MOY 6]
  if DOY > 212 and DOY < 244 [set MOY 7]
  if DOY > 243 and DOY < 274 [set MOY 8]
  if DOY > 273 and DOY < 305 [set MOY 9]
  if DOY > 304 and DOY < 335 [set MOY 10]
  if DOY > 334 and DOY < 366 [set MOY 11]
end

; Calculate the Sun's Position and the Radiation Intensity
to calc-solar-angles-and-radiation
  ; Solar Angles and Radiation Intensities are estimated based on Naraghi (2010).
  if hour > sunrise and hour < sunset [        ; We only need to calculate Radiation between sunrise and sunset (Values are not correct for Hours outside of Sunrise and Sunset)
    set SolarAltitude (asin(cos(Latitude) * cos(HourAngle) * cos(EarthDecAngle) + sin(Latitude) * sin(EarthDecAngle))) ; How high the sun is above the horizon [°] (Quaschning, 2015)
    ; Calculate Solar Azimuth (Sun's horizontal Position (0° = North)).
    ifelse hour <= 12 + 0.01 * TimeIncrement / 60 [   ; Before noon (including rounding errors)...
      set SolarAzimuth 180 - acos(precision((sin(SolarAltitude) * sin(Latitude) - sin(EarthDecAngle)) / (cos(SolarAltitude) * cos(Latitude))) 8)  ;precision-function , otherwise values above 1 and below -1 can occur due to rounding errors
    ] [
      set SolarAzimuth 180 + acos((sin(SolarAltitude) * sin(Latitude) - sin(EarthDecAngle)) / (cos(SolarAltitude) * cos(Latitude)))
    ]
    ; Calculation of the direct beam radiation (Naraghi, 2010 - Equation 6)
    ; The Radiation is influenced by small particles in the air. Therefore, we need to take the length of the radiation path inside the atmosphere into account (which is longer at low angles)
    set radiation (ExtraTerrSolRad * exp(-1 * taub * (RelativeAirmass ^ AirMassEx_ab)))
    set RadiationTotal RadiationTotal + radiation
  ]
end

; Calculate the Shadow of Trees.
to set-shadow
  ; First, reset the Variables of the previously Shaded Patches...
  ask PatchesDisk [
    set plight 1
    if PlotShadow? [set pcolor white]
  ]
  ; ... Then cast a new shadow.
  ask trees [
    ; Move the Tree to a new location, so that the tree's position is the position of the shadow. Then cast shadow on the surrounding Patches and reset the Tree's position.
    let oldx xcor                                        ; Save the old Tree Position.
    let oldy ycor
    let shadow-dist (Height / tan(SolarAltitude))        ; Calculate the Shadow-Length or the distance between the shadow and the tree respectively.
    set xcor (oldx - (sin(SolarAzimuth) * shadow-dist))  ; Move the Tree to the Shaded Area.
    set ycor (oldy - (cos(SolarAzimuth) * shadow-dist))
    set PatchesDisk patches in-radius CrownRadius         ; Create a Patches-Set containing the surrounding Patches.
    set xcor oldx                                        ; Move the Tree back to it's original Location.
    set ycor oldy
    ask PatchesDisk [                                                 ; For the shaded patches:
      set plight plight * (CrownTransmissibility / 100) * Radiation    ; Calculate the Radiation reaching the patches.
      set RadiationSum RadiationSum + plight                         ; Add the Radiation to a variable counting the total Radiation.
      set RadiationBlocked RadiationBlocked + (Radiation - plight)
    ]
  ]
  if PlotShadow? [
    ask PatchesDisk [
      set pcolor 9.9 * plight / Radiation
    ]
  ]  ; If the shadows should be plotted, plot the radiation arriving at the patches.
end


to set-shadow-ellipsoid
  ask patches with [plight != 1] [
    set plight 1
    if PlotShadow? [set pcolor white]
  ]
  let Crown-Depth-Ratio 0.5                      ; Crowned Proportion of the stem.
  ask trees [
    let CrownHeight Height * (Crown-Depth-Ratio)   ; set the crown start height.
    let Crown_Middle_Height (1 - Crown-Depth-Ratio + (Crown-Depth-Ratio / 2)) * Height
    let Shadow-Dist (Crown_Middle_Height / tan(SolarAltitude))
    let PatchList ellfunc (-1 * xcor + xcor - (sin(SolarAzimuth) * Shadow-Dist)) (-1 * ycor + ycor - (cos(SolarAzimuth) * Shadow-Dist)) CrownHeight CrownRadius

    set PatchEll patches at-points PatchList
    set Rad_rel 1
    if (count PatchEll) != 0 [
      set blocked (1 - CrownTransmissibility / 100)
      set pArea 1
      ifelse (length PatchList) != 0 [
        set pArea pi * TreeCrownRadius ^ 2 / length PatchList
      ][
        set pArea 0
      ]

      ;print "________________________________________"
      ;print (word "pArea = " pArea)
      set blocked blocked * pArea
      ;print (word "blocked = " blocked)
      set Rad_rel 1 - blocked
      ;print (word "Rad_Rel = " Rad_Rel)
      if Rad_rel > 1 [set Rad_rel 1]
    ]
    let ncount 1
    ask PatchEll [
      if ncount = 1 [
       ; print (word "plight = " plight)
      ;  print (word "Rad_Rel = " Rad_Rel)
      ]
      set plight plight * Rad_rel
      if ncount = 1 [
       ; print (word "Rad_Rel = " Rad_Rel)
      ;  print (word "plight = " plight)
      ]
      set RadiationSum RadiationSum + plight * Radiation
      set RadiationBlocked RadiationBlocked + (1 - plight) * Radiation
      set ncount ncount + 1
    ]
  ]
  if PlotShadow? [
    ask patches [
      set pcolor 9.9 * plight
    ]
  ]
end



; Plot the Sum of Radiation that reached each patch during one year.
to PlotShadowOverYear
  let MaxShadow max [RadiationSum] of patches
  ifelse MaxShadow != 0 [
    ask patches [
      let Shading RadiationSum / MaxShadow
      set pcolor 9.9 * (1 - Shading)
    ]
  ] [
    ask patches [
      set pcolor white
    ]
  ]
end


to PlotShadowOverYear2
  let MaxShadow max [RadiationBlocked] of patches
  ifelse MaxShadow != 0 [
    ask patches [
      let Shading RadiationBlocked / MaxShadow
      set pcolor 9.9 * (1 - Shading)
    ]
  ] [
    ask patches [
      set pcolor white
    ]
  ]

end


;__________________________________________________________________________________________________________________________________________
;_____ Highlight Shadows
;___________________________________________________________________________________________________________________________________________

to highlight
  PlotShadowOverYear
  let RadTotal sum [RadiationSum] of patches
  let RadCounter 0
  foreach reverse sort-on [RadiationSum] patches [
    the-patch -> ask the-patch [
      set RadCounter RadCounter + RadiationSum
      ifelse RadCounter > RadTotal * Top_Patches_Percentage / 100  [
        stop
      ] [
        set pcolor red
      ]
    ]
  ]
end

to highlight2
  PlotShadowOverYear
  let RadTotal sum [RadiationBlocked] of patches
  let RadCounter 0
  foreach reverse sort-on [RadiationBlocked] patches [
    the-patch -> ask the-patch [
      set RadCounter RadCounter + RadiationBlocked
      ifelse RadCounter > RadTotal * Top_Patches_Percentage / 100  [
        stop
      ] [
        set pcolor red
      ]
    ]
  ]
end




;___________________________________________________________________________________________________________________________________________
;_____ Calculation of Important Variables
;___________________________________________________________________________________________________________________________________________
; To make the model code easy to read and to not fill the global variables with unnecessary stuff,
; we decided to include most calculations as report-functions.

; Calculation of Earth Declination Angle (Tilt of Earth during one Year) (Naraghi, 2010 - Equation 2)
to-report EarthDecAngle
  report (23.45 * sin((2 * pi / 365 * (doy + 284)) * 180 / pi))
end

; Conversion from 365 day per year in 360° in a circle.
to-report YearAngle
  report (360 * (doy / 365))
end

;Calculate the Equation of Time (Quaschning, 2015)
to-report EquationOfTime
  ; Basically Earth's orbit is not circular but elliptical. This is trying to correct this.
  let a (0.0066 + 7.3525 * cos(YearAngle + 85.9))
  let b (9.9359 * cos(2 * YearAngle + 108.9))
  let c (0.3387 * cos(3 * YearAngle + 105.2))
  report (a + b + c)
end

; Calculation of the time difference from sunrise and sunset to noon (Amthor, 1997)
to-report TimeDif
  ; If the sun does not go down...
  ifelse ((sin(-0.0145439 * Rad) - sin(Latitude) * sin(EarthDecAngle)) / (cos(Latitude) * cos(EarthDecAngle))) < -1 [
    report 12    ; ... set the amount of hours between sunrise and sunset to 12...
  ] [
    ; ... else calculate the amount of hours with an equation from Amthor (1997).
    report (acos((sin(-0.0145439 * Rad) - sin(Latitude) * sin(EarthDecAngle)) / (cos(Latitude) * cos(EarthDecAngle))) / pi / Rad * 12)
  ]
end

; Calculation of the time of sunrise [12 (solar noon) - Daylength - Elliptical Correction of Earth's Orbit] (Amthor, 1997)
to-report sunrise
  report (12 - TimeDif - EquationOfTime / Rad)
end

; Calculation of the time of sunset (Amthor, 1997)
to-report sunset
  report (12 + TimeDif - EquationOfTime / Rad)
end

; Each hour, the sun moves 15° (360° / 24h). 0° is defined as noon.
to-report HourAngle
  report ((12 - hour) * 15)
end

; Calculation of relative Airmass (Naraghi, 2010 - Equation 5)
to-report RelativeAirmass
  report (1 / (sin(SolarAltitude) + 0.50572 * ((6.07995 + SolarAltitude) ^ (-1.6364))))
end

; Extraterrestrial Solar Radiation (Naraghi, 2010 - Eq. 8)
to-report ExtraterrSolRad
  report (1367 * (1 + 0.033 * cos(360 * ((doy - 3) / 365))))
end

; Calculation of Air Mass Exponents (Naraghi, 2010)
to-report AirMassEx_ab
  report (1.219 - 0.043 * taub - 0.151 * taud - 0.204 * taub * taud)
end

to-report AirMassEx_ad
  report (0.202 + 0.852 * taub - 0.007 * taud - 0.357 * taub * taud)
end

; Report of solar beam optical depths and solar diffuse optical depths depending on current month and weather station set
to-report taub
 report item MoY item 2 item WeatherStationV StationData
end

to-report taud
  report item MoY item 3 item WeatherStationV StationData
end

;Value to change Sin/Cos-Functions from Deg to Rad
to-report Rad
  report (180 / pi)
end



to-report between [x lim1 lim2]
  let minval (min (list lim1 lim2))
  let maxval (max (list lim1 lim2))
  report x >= minval AND x <= maxval
end



to-report ellipse-in [.CrownRadius .CrownHeight]
  report patches in-radius (sqrt(.CrownRadius ^ 2 + (.CrownHeight * (1 / tan(SolarAltitude))) ^ 2)) with [
    1 >= (
      ((((pxcor - [xcor] of myself) * cos(SolarAzimuth + 180)) - ((pycor - [ycor] of myself) * sin(SolarAzimuth - 180))) ^ 2) /
        (.CrownRadius ^ 2)
      ) + (
      ((((pxcor - [xcor] of myself) * sin(SolarAzimuth + 180)) + ((pycor - [ycor] of myself) * cos(SolarAzimuth - 180))) ^ 2) /
        ((.CrownHeight ^ 2) * ((1 / tan(SolarAltitude)) ^ 2) + (.CrownRadius ^ 2))
      )
  ]
end


; Reports a list of Data about the Wheather Stations.
to-report StationData
  report (list
    (list "Phillip SW BELIZE" 17.539   ; PHILIP SW GOLDSON INTL (2017) http://ashrae-meteo.info/v2.0/?lat=17.53&lng=-88.30&place=%27%27&wmo=785830&si_ip=SI&ashrae_version=2017
      (list 0.373 0.383 0.411 0.477 0.511 0.472 0.503 0.470 0.437 0.411 0.388 0.386)
      (list 2.555	2.518 2.421	2.222 2.143	2.302	2.176	2.293	2.392	2.491	2.531 2.533))
    (list "LAKE CHARLES"      30.125   ; LAKE CHARLES REGIONAL (2017) http://ashrae-meteo.info/v2.0/?lat=30.13&lng=-93.23&place=%27%27&wmo=722400&si_ip=SI&ashrae_version=2017
      (list 0.342 0.358	0.376 0.431 0.466 0.501 0.501 0.488 0.458	0.383	0.367 0.352)
      (list 2.511	2.476	2.419 2.263	2.200 2.139 2.181	2.218 2.268	2.462	2.449 2.484))
    (list "MCMINNVILLE"       45.180   ; MCMINNVILLE MUNICIPAL (2017) http://ashrae-meteo.info/v2.0/?lat=45.18&lng=-123.13&place=%27%27&wmo=726881&si_ip=SI&ashrae_version=2017
      (list 0.322 0.319 0.331 0.359 0.370 0.362 0.352 0.355 0.345 0.334 0.327 0.328)
      (list 2.518 2.535 2.504 2.392 2.378 2.411	2.460 2.467	2.488	2.527 2.510 2.463))
    (list "Lindenberg"        52.217   ; Lindenberg (2017) http://ashrae-meteo.info/v2.0/?lat=52.22&lng=14.12&place=%27%27&wmo=093930&si_ip=SI&ashrae_version=2017
      (list 0.323 0.350 0.390 0.412 0.410 0.407 0.427 0.421 0.383 0.373 0.348 0.317)
      (list 2.314 2.290	2.199 2.217 2.267 2.300 2.254	2.285 2.367	2.348 2.352 2.331))
    (list "Croker river"      69.280   ; Croker river (2017) http://ashrae-meteo.info/v2.0/?lat=69.28&lng=-119.22&place=%27%27&wmo=710590&si_ip=SI&ashrae_version=2017 -- N/A for December was set to 0
      (list 0.158 0.207 0.246 0.292 0.332 0.328 0.355 0.339 0.303 0.232 0.155 0)
      (list 1.887 2.049	2.188 2.127 2.131 2.357 2.382	2.480	2.491 2.170 1.919 0))
    )
end

; Function to calculate Patches within an Ellipse.
to-report ellfunc [x0 y0 .CrownHeight .CrownRadius]
  ; In this Function we calculate which Patches are inside an Ellipse. The input Parameters are:
  ; x0 / y0 = Midpoint-Coordinates of the Ellipse
  ; .CrownHeight = Vertical Extent of the Crown Ellipsoid on the Tree
  ; .CrownRadius = Horizonal Extent of the Crown Ellipsoid on the Tree
  ; Technically there are two other Inputs:
  ; SolarAltitude = Angle of Sun above the Horizon
  ; SolarAzimuth  = Angle of Sun from North (180° -> Sun is South)

  ; The Output of this Function are Coordinate-Pairs of Patches inside of the Shadow-Ellipse.

  ; The Mechanics:
  ; 1. We calculate a function that defines the Circumference of the Ellipse on the Ground.
  ; 2. We calculate the Minimum and Maximum X-Values for which the Function is valid.
  ; 3. We insert all Integers between Minimum and Maximum X into the Function.
  ; 4. The Function provides us with two Y Outputs for each X Input: The Outer Y-Bounds of the Ellipse Circumference.
  ; 5. We create Coordinate-Pairs for each X and Y (Both Integer) inside the Bounds of the Ellipse.


  ; The Function of the Ellipse are taken from ShadeMotion (XXXXX).
  ; The Rearrangement of the Function can be found at XXXX. It was done by Schlicht, R. XXXX
  ; We first define the two squared Extends of the Ellipse (a2 and b2).
  let a2 .CrownRadius ^ 2
  let b2 ((.CrownHeight ^ 2) * ((1 / tan(SolarAltitude)) ^ 2) + (.CrownRadius ^ 2))
  ; We then define the tilting Angle alpha.
  let alpha SolarAzimuth + 180
  ; We simplefy the function by defining variables for parts of the function
  let u cos alpha
  let v sin alpha
  let s a2 * b2
  let p (a2 * u ^ 2 + b2 * v ^ 2)
  let q (b2 - a2) * u * v
  let r (a2 * v ^ 2 + b2 * u ^ 2)
  ; We can now calculate Minimum and Maxmimum possible X-Values
  let pxmin x0 - sqrt((s * p) / (r * p - q ^ 2))
  let pxmax x0 + sqrt((s * p) / (r * p - q ^ 2))
  ; When Shadows are long, they transverse the Edge of the World. To avoid that, reset the Limits of X to the Minimum and Maximum World-Coordinates.
  if pxmin < min-pxcor [set pxmin min-pxcor]
  if pxmax > max-pxcor [set pxmax max-pxcor]
  ; Create a List with all X-Values between Minimum and Maximum X-Coordinates.
  let possx seq (ceiling pxmin) (floor pxmax) 1
  let listxy (list)     ; Create an empty List to store Coordinate-Pairs.
  if (length possx) != 1 [
    foreach possx [       ; Foreach possible X-Coordinate ....
    ellx ->
    let plusminus sqrt((q ^ 2 - r * p) * (ellx - x0) ^ 2 + s * p) ; ... calculate the Y-Distance from the midpoint of the Ellipse...
    let miny (y0 + ((q * (ellx - x0) - plusminus) / p))           ; .... and then define Minimum....
    let maxy (y0 + ((q * (ellx - x0) + plusminus) / p))           ; ... and Maximum Y-Coordinates of the Ellipse.
    if miny < min-pycor [set miny min-pycor]                      ; Reset them in case they lay outside of the world.
    if maxy > max-pycor [set maxy max-pycor]
    foreach seq (ceiling miny) (floor maxy) 1 [                   ; For each Y-Coordinate inside the Y-Limits...
      elly ->
      set listxy lput (list ellx elly) listxy                     ; ... save the X and Y-Coordinate-Pair inside a List.
    ]
  ]
  ]
  report listxy                                                   ; Report that List.
end





to-report seq [.from .to .by]
  let lower .from + floor((.to - .from) / .by) * .by
  let out (list .from)
  let x .from
  while [x < lower] [
    set x (x + .by)
    set out lput x out
  ]
  report out
end



to-report rep [.val .n]
  let replist (list)
  foreach seq 1 .n 1 [
    the-n ->
    set replist lput .val replist
  ]
  report replist
end




to-report sort-self [LL]
  let LengthLL length LL
  let RestList LL
  let sortedList (list)
  repeat LengthLL [
    let minx max-pxcor
    let miny max-pycor
    foreach Restlist [
      theitem ->
      if item 0 theitem <= minx [
        if item 1 theitem < miny [
          set minx item 0 theitem
          set miny item 1 theitem
        ]
      ]
    ]
    let Lpos position (list minx miny) RestList
    let smlist sublist RestList 0 Lpos
    let bglist sublist RestList (Lpos + 1) length RestList
    set RestList sentence smlist bglist
    set sortedList lput (list minx miny) sortedList
;    print (word "minx = " minx " miny = " miny)
;    print (word "Lpos = " Lpos)
;    print (word "smlist = " smlist)
;    print (word "bglist = " bglist)
;    print (word "Restlist = " RestList)
;    print (word "sortedList = " sortedList)
;    print "_________________________"
;    print ""
  ]
  report sortedList
end










;___________________________________________________________________________________________________________________________________________
;_____ Literature
;___________________________________________________________________________________________________________________________________________
; Amthor, J. S. (1997): Calculation of daylength. In: Computer applications in the biosciences: CABIOS 13 (4), S. 479–480. DOI: 10.1093/bioinformatics/13.4.479.
; Naraghi, Mohammed (2010). A Demand Based Optimum Solar Panel Orientation. DOI: 10.1115/IMECE2010-37918
; Quaschning, Volker (2015): Regenerative Energiesysteme Technologie - Berechnung - Simulation. ISBN: 978-3-446-44267-2
; 2017 ASHRAE Handbook (2017):
; PHILIP SW GOLDSON INTL (2017) http://ashrae-meteo.info/v2.0/?lat=17.53&lng=-88.30&place=%27%27&wmo=785830&si_ip=SI&ashrae_version=2017
; LAKE CHARLES REGIONAL (2017) http://ashrae-meteo.info/v2.0/?lat=30.13&lng=-93.23&place=%27%27&wmo=722400&si_ip=SI&ashrae_version=2017
; MCMINNVILLE MUNICIPAL (2017) http://ashrae-meteo.info/v2.0/?lat=45.18&lng=-123.13&place=%27%27&wmo=726881&si_ip=SI&ashrae_version=2017
; Lindenberg (2017) http://ashrae-meteo.info/v2.0/?lat=52.22&lng=14.12&place=%27%27&wmo=093930&si_ip=SI&ashrae_version=2017
; Croker river (2017) http://ashrae-meteo.info/v2.0/?lat=69.28&lng=-119.22&place=%27%27&wmo=710590&si_ip=SI&ashrae_version=2017 -- N/A for December was set to 0


@#$#@#$#@
GRAPHICS-WINDOW
439
219
928
709
-1
-1
1.2
1
10
1
1
1
0
1
1
1
-200
200
-200
200
0
0
1
ticks
30.0

BUTTON
255
224
318
257
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
321
224
419
257
go one year
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
255
411
427
444
TimeIncrement
TimeIncrement
1
60
60.0
1
1
[Minutes]
HORIZONTAL

SLIDER
254
262
426
295
TreeHeight
TreeHeight
1
50
30.0
1
1
[m]
HORIZONTAL

SLIDER
254
296
426
329
TreeCrownRadius
TreeCrownRadius
1
5
5.0
0.1
1
NIL
HORIZONTAL

INPUTBOX
252
510
426
570
Latitude
17.539
1
0
Number

CHOOSER
252
463
426
508
WeatherStation
WeatherStation
"Phillip SW Belize" "Lake Charles" "McMinnville" "Lindenberg" "Croker river"
0

SLIDER
255
331
427
364
CrownTransmissibility
CrownTransmissibility
0
100
5.0
1
1
[%]
HORIZONTAL

MONITOR
898
107
955
152
Hour
hour
2
1
11

SLIDER
252
572
426
605
StartofGrowingSeason
StartofGrowingSeason
0
150
120.0
1
1
NIL
HORIZONTAL

SLIDER
252
606
426
639
EndofGrowingSeason
EndofGrowingSeason
150
365
272.0
1
1
NIL
HORIZONTAL

MONITOR
898
58
955
103
NIL
DOY
17
1
11

SWITCH
252
641
426
674
PlotShadow?
PlotShadow?
0
1
-1000

BUTTON
1011
608
1206
641
Highlight Top Shaded Patches
highlight
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1011
644
1206
677
Top_Patches_Percentage
Top_Patches_Percentage
0
100
20.0
1
1
NIL
HORIZONTAL

TEXTBOX
24
34
345
163
This is a shading model that calculates the shadow-casting of one tree to the ground. You can easily run the model if you follow the instructions.
20
0.0
1

TEXTBOX
26
223
230
713
First, pick:\n- a Tree Height\n- a Crown Radius\n- the Crown Transmissibility \n  (how much light [%] goes through the \n   Tree Crown?)\n- if the Crown is shape as a Disk on top of \n  the Tree or if it's an Ellipsoid \n  (basically a 3D Ellipse)\n- the Time Increment which defines the \n  amount of time that passes at every \n  time step\n\nSecond, choose a wheather Station.\nThere are 5 Weather Stations to choose from. They are located along a Latitude gradient from 17 (Belize) to 70 (Croker River). \nThen define the Start and End of the Growing Season. These are the (Julian) Day of Year (1 = 1st January).\n\nIf you want to plot the shadows during the model simulation, then turn \"PlotShadow\" on. This makes the model much slower!\n\nFinally, press \"setup\" and then \"go one year\".\n\nAt the end of the Model run, you can see the amount of light that reaches the ground. In areas shaded by the Tree, this amount is decreased. Heavily shaded areas appear black .
11
0.0
1

TEXTBOX
362
63
623
203
If the Simulation takes forever, increase the Model Speed using the Slider above!\n\nThe Model now simulates the Shading of one Tree to the Ground. If PlotShadow? is turned on, you can even see the shadow on the World Map below. If you don't see it, try to decrease the Model Speed a bit. \n\nThe Model is now running for one Year. You can see the Day of Year on the right.
11
0.0
1

CHOOSER
255
365
427
410
Crown_Shape
Crown_Shape
"Disk" "Ellipsoid"
0

TEXTBOX
24
185
229
229
Before the Simulation
18
0.0
1

TEXTBOX
360
32
561
76
During the Simulation
18
0.0
1

TEXTBOX
638
59
894
213
DOY 1 for example is January 1st. DOY 365 is December 31st.\n\nYou also see the current Time in Decimals. 15.50 for example is 15:30.\n\nThe model is stopping if DOY 365 and Hour 24 is reached. But you can also manually stop the Simulation if you press \"go on year\" again. You can resume the Simulation by pressing the Button again.
11
0.0
1

TEXTBOX
982
61
1207
105
After the Simulation
18
0.0
1

TEXTBOX
982
95
1241
585
The Simulation is finished, if DOY (Day of Year) 365 is reached. The DOY is then automatically reset to 1.\n\nOn the World Map you now see that some Areas are colored grey. The Shades of grey represent the Light that reached the Ground during one year. White means that this pieace of Ground was never shaded. The darker the Ground, the more Shade was cast on that piece of Ground during one year.\n\nYou can now start a new Simulation at another Weather Station. This Weather Station will be located at another Altitude. Different Altitudes lead to different Shading Patterns.\n\nYou can also change the Crown Shape. Disks will always cast Shadows of the same Size to the Ground. Ellipsoid Crowns will lead to different Shadow Sizes and thus change the Pattern of Shading over the year.\n\nWhere on the Ground cast a Tree the most Shadow? To analyse this, you can Highlight the Pieces of Ground that were shaded the most. First Choose a Percentage of the Amount of Shading that you want to highlight, then press \"Highlight Top Shaded Patches\".\n\nYou see that a lot of Shading is close to the Tree and the Influence of the Tree to Ground in a bigger Distance is relatively small.\n\nThis Analysis helps us to decide, how we want to implement the Shading into a final Model with many Trees.
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="9000"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Crown_Shape">
      <value value="&quot;Disk&quot;"/>
      <value value="&quot;Ellipsoid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CrownTransmissibility">
      <value value="20"/>
      <value value="15"/>
      <value value="10"/>
      <value value="5"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="WeatherStation">
      <value value="&quot;Phillip SW Belize&quot;"/>
      <value value="&quot;Lake Charles&quot;"/>
      <value value="&quot;McMinnville&quot;"/>
      <value value="&quot;Lindenberg&quot;"/>
      <value value="&quot;Croker river&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="9000"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Crown_Shape">
      <value value="&quot;Disk&quot;"/>
      <value value="&quot;Ellipsoid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CrownTransmissibility">
      <value value="100"/>
      <value value="75"/>
      <value value="50"/>
      <value value="25"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="WeatherStation">
      <value value="&quot;Croker river&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment3" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="9000"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Crown_Shape">
      <value value="&quot;Ellipsoid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CrownTransmissibility">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="WeatherStation">
      <value value="&quot;Croker river&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
