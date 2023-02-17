;_______________________________________________________________________________________________________________________________________________________________________________________________
;________ A Model to Calculate Shading of Trees to the Ground
;_______________________________________________________________________________________________________________________________________________________________________________________________


;_______________________________________________________________________________________________________________________________________________________________________________________________
;________ Used Extensions
;_______________________________________________________________________________________________________________________________________________________________________________________________
extensions [fetch csv]    ; Import External Data

;_______________________________________________________________________________________________________________________________________________________________________________________________
;________ Variables of Global Environment, Trees and Patches
;_______________________________________________________________________________________________________________________________________________________________________________________________
globals [
  DOY                 ; (Julian) Day of the Year [1 - 365]
  MOY                 ; Month of the Year        [1 - 12]
  hour                ; Hour                     [1-24]
  growingseason-start ; Start and...
  growingseason-end   ; ...End of the Vegetation Period  [1 - 365]
  SolarAltitude       ; Solar Altitude           [°] 0° = Horizon
  SolarAzimuth        ; Solar Azimuth            [°] 0° = North
  Min_SolarAltitude   ; Minimum Solar Altitude Angle given the shadow-length of the Tree in the middle [°]

  weatherStationV     ; Wheather Station Number  [0 - 4]
  Radiation           ; Radiation Energy         [W/m²]

  PatchesDisk         ; Patches-Set that is shaded using Disk Crowns
  PatchesEllipse      ; Patches-Set that is shaded using Ellipsoid Crowns
  PatchesObserved     ; Observed patches

  MinSize             ; Minimum Size for UI
  RadiationSum        ; Total Radiation per m² during the simulation

  Observation?        ; Variable indicating if Patch-Variables should be recorded
  Observation-Size    ; Radius from patch 0/0 in which Patches should be recorded
]

breed [results result]; There is only one Type of Agents, which is "Trees".
breed [trees tree]; There is only one Type of Agents, which is "Trees".
trees-own [
  DBH             ; Diameter at Breast Height   [cm]
  height          ; Tree Height                 [m]
  crown-radius    ; Radius of Tree Crown        [m]
  b2              ; Species specific constant needed for the JABOWA-growth-function  [-]
  b3              ; Species specific constant needed for the JABOWA-growth-function  [-]
  dmax            ; Species specific parameters needed for the JABOWA-growth function resembling the maximum DBH...        [cm]
  hmax            ; ... and Height a tree of the species can reach                                                         [cm]
]

patches-own [
  plight          ; Available light at patch, scaled between [0 - 1] 1 = Full light
  prad
  RadiationTotal  ; Summed Radiation Energy at one patch during one model run     [W/m²]
  FullSunlight    ; Number of instances in which the patch receives full sunlight [-]
]

;_______________________________________________________________________________________________________________________________________________________________________________________________
;________ Procedures start here
;_______________________________________________________________________________________________________________________________________________________________________________________________
; Setup Model
to setup
  ca                          ; Delete all previous Data.
  setup-world                 ; Create World (Resize World, set Time, set Location, set Environment).
  setup-Trees                 ; Create Trees (set Location of Tree, DBH, Species, ...).
  ask trees [allometerize]    ; From the Tree DBH, set all other size related Parameters (Height, Crown Radius, ...).
  reset-ticks                 ; Reset Ticks to 0.
end

; Procedures for every Time Step
to go
  settime                                                                                              ; Increase Time by an Hour...
  if doy > growingseason-start and doy < growingseason-end and hour > sunrise and hour < sunset [      ; ... and if the Sun is up during the Growing Season...
    calc-solar-angles-and-radiation                                                                    ; ... update Sun Position and Radiation.
    if SolarAltitude >= Min_SolarAltitude [
      ifelse CrownShape = "Disk" [       ; If the Crown is a Disk...
        set-shadow-disk                  ; ... Trees cast Disk-Shadows...
      ][                                 ; ... else ...
        set-shadow-ellipse               ; ... Trees cast Ellipse-Shadows.
      ]
      observe
    ]
  ]

  tick

  ; If one Year is over...
  if DOY = 365 AND Hour >= 23 [
    set DOY 1   ; Reset DOY...
    set Hour 0  ; ... and Hour...
    PlotShadowOverYear
    stop        ; ... and stop the simulation. This is important for NetLogoWeb because it cannot use <reapeat 365 [go]> inside a button.
  ]
end

;_______________________________________________________________________________________________________________________________________________________________________________________________
;________ Setup Procedures
;_______________________________________________________________________________________________________________________________________________________________________________________________
; Setup Time, Location and other Global Variables.
to setup-world
  ; Resize World
  let maxcor World-Size / 2                ; Set the Maximum and Minimum Patch-Coordinates.
  ifelse patch-size > 600 / World-Size [   ; If the World is too large for the screen, then there will be an error. To avoid this, check if the new world would be larger or smaller...
    set-patch-size 600 / World-Size        ; ... if the patch Size of the old World is smaller than the patch Size of the old world, then reduce the patch Size first...
    resize-world -1 * maxcor maxcor -1 * maxcor maxcor  ; ... and then change the World-Size...
  ][
    resize-world -1 * maxcor maxcor -1 * maxcor maxcor  ; ... else, change World Size first...
    set-patch-size 600 / World-Size        ; ... and then Patch Size.
  ]

  ; Set Time and Location
  ask patches [set pcolor white]           ; Set the color of Patches to White (Full Light).
  set doy 1                                ; Set Day of Year to 1 (1st January).
  set hour -1                              ; Set Hour to -1 (0 = 24:00), this is set +1 at the Start of each Time Step.
  set growingseason-start 121              ; Growing season is set between day 121 and 273 (First of May to 30th of September).
  set growingseason-end 273                ; Can be changed but the variable sumRad1Gs (defying the Total Radiation during one Growing Season) then has to be changed as well.

  if WeatherStation = "Phillip SW Belize" [set WeatherStationV 0 set Min_SolarAltitude 15.3]  ; set Weather Stations and Latitude. Belize is at Latitude 10, Croker River is at 69.
  if WeatherStation = "Lake Charles"      [set WeatherStationV 1 set Min_SolarAltitude 11.4]
  if WeatherStation = "McMinnville"       [set WeatherStationV 2 set Min_SolarAltitude 10.6]
  if WeatherStation = "Lindenberg"        [set WeatherStationV 3 set Min_SolarAltitude 11.2]
  if WeatherStation = "Croker river"      [set WeatherStationV 4 set Min_SolarAltitude  5.7]
  set Latitude item 1 item WeatherStationV taudb

  set MinSize 1 ; Set the Minimum Size of Trees in the UI.
  set Observation? TRUE  ; Observe Patches.
  set Observation-Size 50

  ; Create Empty Patches-Sets...
  set PatchesDisk (patch-set)    ; ... for Disk Crowns...
  set PatchesEllipse (patch-set) ; ... and for Ellipsoide Crowns.

  ask patch 0 0 [set PatchesObserved patches in-radius Observation-Size]
end

; Create Trees
to setup-Trees
  set-default-shape trees "circle"                     ; Set the Shape of Trees to Circle.

  ; Now Import Overstory Trees first.
  let TreeData import-Tree-Data                        ; Import Tree Data (Location and DBH) from External Source (GitHub).
  foreach (seq 0 ((length TreeData) - 1) 1) [   ; For each Tree (Element of the Data)...
    x ->                                               ; ... from 0 to the Number of Entries in the External Data...
    let TreeXCor item 0 item x TreeData                ; ... save X- and Y-Coordinates.
    let TreeYCor item 1 item x TreeData
    if abs TreeXCor < max-pxcor AND abs TreeYCor < max-pycor [   ; If the Tree is inside the Windows of our World...
      create-trees 1 [                                             ; Create a Tree...
        set color green
        set xcor TreeXCor                                          ; ... set X-...
        set ycor TreeYCor                                          ; ... and Y-Coordinates.
        set DBH item 2 item x TreeData                             ; ... and DBH.
        set size DBH / 20                                          ; Scale the Size of the Tree in the UI according to it's DBH.
        if size < 1 [set size MinSize]                             ; But the Size should be at least MinSize so we can still see it in the UI even if it's really small.
        set dmax 155                                               ; Set allometric parameters (from Bugmann 1994)
        set hmax 4500
        set b2 2 * (hmax - 137) / (dmax)
        set b3 (hmax - 137) / (dmax ^ 2)
      ]
    ]
  ]
  ask patch 0 0 [                                                  ; Kill all Trees in a User defined Radius from the Plot Midpoint to create a Forest Gap
    ask trees in-radius forest-gap-size [
      die
    ]
  ]
end


;_______________________________________________________________________________________________________________________________________________________________________________________________
;________ Go Procedures
;_______________________________________________________________________________________________________________________________________________________________________________________________
; Go one hour forward and change all time variables
to settime
  set hour hour + 1   ; Go one Hour forward.
  if hour = 24 [      ; If the Hour is 24...
    set hour 0        ; ... reset Hour to 0 and...
    set doy doy + 1   ; ... go one Day forward.
    if doy = 366 [    ; If we arrive at the End of the Year...
      set doy 1       ; ... reset Day of Year to 1.
    ]
  ]

  ; Set Month of the Year, which is necessary to determine the monthly taub and taud value
  if doy < 32 [set MoY 0]
  if doy > 31 and doy < 60 [set MoY 1]
  if doy > 59 and doy < 91 [set MoY 2]
  if doy > 90 and doy < 121 [set MoY 3]
  if doy > 120 and doy < 152 [set MoY 4]
  if doy > 151 and doy < 182 [set MoY 5]
  if doy > 181 and doy < 213 [set MoY 6]
  if doy > 212 and doy < 244 [set MoY 7]
  if doy > 243 and doy < 274 [set MoY 8]
  if doy > 273 and doy < 305 [set MoY 9]
  if doy > 304 and doy < 335 [set MoY 10]
  if doy > 334 and doy < 366 [set MoY 11]
end

; Calculate Solar Angles and Radiation Input Energy
to calc-solar-angles-and-radiation
  if hour > sunrise and hour < sunset [  ; We only need to calculate Radiation between sunrise and sunset (Values are not correct for Hours outside of Sunrise and Sunset)
                                         ; Calculate Solar Altitude (Solar Elevation) first.
    set SolarAltitude (asin(cos(Latitude) * cos(HourAngle) * cos(EarthDecAngle) + sin(Latitude) * sin(EarthDecAngle)))                            ; Calculate Height of the Sun above Horizon in degrees (Quaschning, 2015)
    ifelse hour <= 12                                                                                                                             ; We now calculate Solar Azimuth. 0° = Sun is North. 180 = Sun is South. 90 = Sun is East. We split this Calculation.
    [                                                                                                                                             ; At Hour 12 the Sun is always at 180°. If Hour is below 12, then we substract from 180°, otherwise we add to 180°.
      set SolarAzimuth 180 - acos(precision((sin(SolarAltitude) * sin(Latitude) - sin(EarthDecAngle)) / (cos(SolarAltitude) * cos(Latitude))) 8)  ; precision-function , otherwise values above 1 and below -1 can occur due to rounding errors and acos and asin are not possible to be calculated.
    ] [
      set SolarAzimuth 180 + acos((sin(SolarAltitude) * sin(Latitude) - sin(EarthDecAngle)) / (cos(SolarAltitude) * cos(Latitude)))
    ]                                                                                                                                             ; Next we calculate Radiation Energy [W/m²].
    set radiation (ExtraTerrSolRad * exp(-1 * taub * ( RelativeAirmass ^ AirMassEx_ab)))                                                          ; We calculate Direct Beam Radiation based on (Naraghi, 2010 - Equation 6).
    ]
end

; Shadow-Calculation if disk shadows are chosen.
to set-shadow-disk
  ask patches [set plight 1]                   ; Reset Light Availability back to Full Light.

                                               ; Shadow-Calculation is time consuming. We therefore implemented an Option to only calculate Shadows of Trees that potentially shade the Gap.
  let MaxRCrown max [Crown-Radius] of trees    ; For this calculation we need the maximum Crown Radius.

  let ShadingTreeList trees                    ; Create an Agentset containing all Shading Trees.
  if not PlotAllShadows? [                     ; If not all Trees are used for Shading...
    set ShadingTreeList ShadingTrees           ; ... only use the Trees that possibly shade the Gap in the middle.
  ]
  ask ShadingTreeList [                                                                 ; Then these Shading Trees...
    let oldx xcor                                                                       ; ... save their Coordinates...
    let oldy ycor
    let shadow-dist (height / tan(SolarAltitude))                                       ; ... and calculate their Shadow-Length.
    if oldx - (sin(SolarAzimuth) * shadow-dist) > min-pxcor and oldx - (sin(SolarAzimuth) * shadow-dist) < max-pxcor and oldy - (cos(SolarAzimuth) * shadow-dist) > min-pycor and oldy - (cos(SolarAzimuth) * shadow-dist) < max-pycor [   ; If the Shadow-Midpoint of this Tree is inside the Model-World...
      set xcor (oldx - (sin(SolarAzimuth) * shadow-dist))                               ; ... move the Tree to it's Shadow-Midpoint...
      set ycor (oldy - (cos(SolarAzimuth) * shadow-dist))
      ask patches in-radius crown-radius [set plight plight * crown-transmissibility]   ; ... and shade all Patches inside it's Crown Area.
      set xcor oldx                                                                     ; Then return the Tree to it's old Location.
      set ycor oldy
    ]
  ]
  if PlotShadow? [                            ; If shadows should be plotted...
    ask patches [set pcolor 9.9 * plight]     ; ... make non-shaded patches white and totally shaded patches black.
  ]
end

; Shadow-Casting of Ellipsoid-Crowns
to set-shadow-ellipse
                      ; First, we reset the light Available at all patches
  ask patches [
    set plight 1
    ;set pcolor white
  ]

                                                ; How much of the tree is crown? We calculate with 50% here.
  let Crown-Depth-Ratio 0.5                     ; Crowned Proportion of the stem.

                                                ; Shadow-Calculation is time consuming. We therefore implemented an Option to only calculate Shadows of Trees that potentially shade the Gap.
  let ShadingTreeList trees                     ; Create an Agentset containing all Shading Trees.
  if not PlotAllShadows? [                      ; If not all Trees are used for Shading...
    set ShadingTreeList ShadingTrees            ; ... only use the Trees that possibly shade the Gap in the middle.
  ]
  ask ShadingTreeList [
    ; To calculate the Ellipses on the Ground, we calculate the function, that defines the outer bounds of each Ellipse first and then
    ; shade all patches inside these bounds.
    ; To do so, we define a new function for each Tree using the reporter "FunctionEllipse".
    ; This Function needs 4 Inputs:
    ;   - A X-Coordinate (Middle of the Shadow). This must be relative to the tree and not an absolute Coordinate!
    ;   - A Y-Coordinate (Middle of the Shadow)
    ;   - The Crown Height (in [m])
    ;   - The Crown Radius (in [m])

    let CrownHeight Height * (Crown-Depth-Ratio)                                          ; The Crown Height is defines as a percentage of Tree Height.
    let Crown_Middle_Height (1 - Crown-Depth-Ratio + (Crown-Depth-Ratio / 2)) * Height    ; To calculate Shadow-Locations, we need to define the midpoint of the shadow and for this we first define the middle of the Crown (from the Ground).
    let Shadow-Dist (Crown_Middle_Height / tan(SolarAltitude))                            ; With this we can now calculate the Distance from the Tree Stem Position the Shadow-Midpoint.
                                                                                          ; This distance can be used to calculate Shadow-Midpoint-Positions based on Solar Azimuth. These serve as Input into the Ellipse-Function.
                                                                                          ; We later use patches at-points which takes points relative to a Tree. We therefore don't have to adjust for Tree Positions when calculateing the Shadow-Function.
    let PatchList FunctionEllipse (0 - xcor + xcor - (sin(SolarAzimuth) * Shadow-Dist))
      (0 - ycor + ycor - (cos(SolarAzimuth) * Shadow-Dist)) CrownHeight Crown-Radius
    set PatchesEllipse patches at-points PatchList                                        ; We create a patch-set with all Patches that are shaded by the respective Tree.
    let Rad_rel 1                                                                         ; We now only have to calculate how much light is blocked by that Tree. If Shadows are bigger, less light is blocked per Patch. The LAI is still the same as for Disk-Crowns.
    if (count PatchesEllipse) != 0 [                                                      ; If the number of Shaded Patches is greater than 0 (0 means all Shaded Patches are located outside of the Plot)...
      let blocked (1 - Crown-Transmissibility)                                            ; ... calculate how much Light is going through the Crown.
      let pArea pi * Crown-Radius ^ 2 / length PatchList                                  ; Crown Transmissibility is given for Disk Shadows. We assume that the proportion of blocked radiation per Tree stays constant regardless of Shadow-Size.
      set blocked blocked * pArea                                                         ; How much Light is blocked per Shadowed Patch?
      set Rad_rel 1 - blocked                                                             ; Calculate how much Proportion of Light is coming through per Shaded Patch.
      if Rad_rel > 1 [set Rad_rel 1]                                                      ; If this is more than 100 % (can be for really small crowns, because of rounding Errors), reset it back to 100 %.
    ]

    ask PatchesEllipse [                           ; Actually Shade Patches.
      set plight plight * Rad_rel
    ]
  ]
  if PlotShadow? [                                 ; If the Shadows should be plotted, set the color.
    ask patches [set pcolor 9.9 * plight]
  ]
end

; Calculate Tree Height and Crown Radius based on DBH
to allometerize
  set height (137 + b2 * (dbh) + b3 * ((dbh) ^ 2)) / 100   ; DBH-Height Relationship from Bugmann 1994.
  set crown-radius 0.385 * height / 2                      ; Height-CrownSize Relationship from SOEL-Model (Kellner & Swihart based on Kenefic & Nyland 1999)
  set size DBH / 20                                        ; Set size...
  if size < 1 [set size MinSize]                           ; ... but it still has to be visible!
end

; To write an Output-File, record all important Variables
to observe
  if Observation? [          ; In case Observation is turned on...
    set RadiationSum RadiationSum + radiation
    ask PatchesObserved [    ; ... for all Patches...
      set RadiationTotal RadiationTotal + plight * Radiation   ; Add the current Radiation to the summed Radiation. Note that before, Radiation and Shadowing were seperated. We combine it here!
      if plight = 1 [set FullSunlight FullSunlight + 1]        ; Also count the amount of Instances a Patch receives full Sunlight.
    ]
  ]
end

;______________________________________________________________________________________________________________________________________
;________ Plotting at the end of the Model Run
;______________________________________________________________________________________________________________________________________
; Plot the Sum of Radiation that reached each patch during one year.
to PlotShadowOverYear
  let MaxShadow max [RadiationTotal] of PatchesObserved
  ifelse MaxShadow != 0 [
    ask PatchesObserved [
      let Shading RadiationTotal / MaxShadow
      set pcolor 9.9 * (Shading)
    ]
  ] [
    ask patches [
      set pcolor white
    ]
  ]
end

to Plot-Results [mode]
  ask Results [die]
  ask PatchesObserved [set pcolor white]
  if mode = "TotalRadiation" OR mode = "WeightedRad" [
    let MaxShadow max [RadiationTotal] of PatchesObserved
    ifelse MaxShadow != 0 [
      ask PatchesObserved [
        let Shading RadiationTotal / MaxShadow
        set pcolor 9.9 * (Shading)
      ]
    ] [
      ask patches [
        set pcolor white
      ]
    ]
  ]
  if mode = "TotalFull" OR mode = "WeightedFullS" [
    let MaxShadow max [FullSunlight] of PatchesObserved
    ifelse MaxShadow != 0 [
      ask PatchesObserved [
        let Shading FullSunlight / MaxShadow
        set pcolor 9.9 * (Shading)
      ]
    ] [
      ask patches [
        set pcolor white
      ]
    ]
  ]
  if mode = "WeightedRad" [
    let WRx 0
    let WRy 0
    let RadSum sum [RadiationTotal] of PatchesObserved
    ask PatchesObserved [
      set WRx WRx + (RadiationTotal * pxcor)
      set WRy WRy + (RadiationTotal * pycor)
    ]
    create-results 1 [
      set shape "circle"
      set color red
      set size patch-size * World-Size / 100
        setxy (WRx / RadSum) (WRy / RadSum)
    ]
  ]
  if mode = "WeightedFullS" [
    let WRx 0
    let WRy 0
    let RadSum sum [FullSunlight] of PatchesObserved
    ask PatchesObserved [
      set WRx WRx + (FullSunlight * pxcor)
      set WRy WRy + (FullSunlight * pycor)
    ]
    create-results 1 [
      set shape "circle"
      set color red
      set size patch-size * World-Size / 100
        setxy (WRx / RadSum) (WRy / RadSum)
    ]
  ]
  if mode = "Species" [
    ask PatchesObserved [
      let pRadPatch RadiationTotal / RadiationSum
      ifelse pRadPatch < 0.15 [
        set pcolor 104
      ] [
        ifelse pRadPatch > 0.24 [
          set pcolor 14
        ] [
          set pcolor 44
        ]
      ]
    ]
  ]
end



;_______________________________________________________________________________________________________________________________________________________________________________________________
;________ Reporters and selfmade Functions
;_______________________________________________________________________________________________________________________________________________________________________________________________

;______________________________________________________________________________________________________________________________________
;________ Radiation related Reporters
;______________________________________________________________________________________________________________________________________
; Calculation of Earth Declination Angle (Tilt of Earth during one Year) (Naraghi, 2010 - Equation 2)
to-report EarthDecAngle
  report (23.45 * sin((2 * pi / 365 * (doy + 284)) * 180 / pi))
end

; Calculation of Earth's Position relative to the Sun.
to-report YearAngle
  report (360 * (doy / 365))
end

; Calculate the Equation of Time (Quaschning, 2015)
to-report EquationOfTime
  let a (0.0066 + 7.3525 * cos(YearAngle + 85.9))
  let b (9.9359 * cos(2 * YearAngle + 108.9))
  let c (0.3387 * cos(3 * YearAngle + 105.2))
  report (a + b + c)
end

; Calculation of the time difference from sunrise and sunset to noon (Amthor, 1997)
to-report TimeDif
  ifelse ((sin(-0.0145439 * Rad) - sin(Latitude) * sin(EarthDecAngle)) / (cos(Latitude) * cos(EarthDecAngle))) < -1 [
    report 12
  ] [
    report (acos((sin(-0.0145439 * Rad) - sin(Latitude) * sin(EarthDecAngle)) / (cos(Latitude) * cos(EarthDecAngle))) / pi / Rad * 12)
  ]
  ;report (acos((sin(-0.0145439 * Rad) - sin(Latitude) * sin(EarthDecAngle)) / (cos(Latitude) * cos(EarthDecAngle))) / pi / Rad * 12)
end

; Calculation of the time of sunrise [12 (solar noon) - Daylength - Elliptical Correction of Earth's Orbit] (Amthor, 1997)
to-report sunrise
  report (12 - TimeDif - EquationOfTime / Rad)
end

; Calculation of the time of sunset (Amthor, 1997)
to-report sunset
  report (12 + TimeDif - EquationOfTime / Rad)
end

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
 report item MoY item 2 item WeatherStationV taudb
end

to-report taud
  report item MoY item 3 item WeatherStationV taudb
end

; Value to change Sin/Cos-Functions from Deg to Rad
to-report Rad
  report (180 / pi)
end

; All Values for taub and taud in one Reporter
to-report taudb
  report (list
    (list "Phillip SW BELIZE" 17.539 (list 0.373 0.383 0.411 0.477 0.511 0.472 0.503 0.470 0.437 0.411 0.388 0.386) (list 2.555	2.518 2.421	2.222 2.143	2.302	2.176	2.293	2.392	2.491	2.531 2.533))   ;PHILIP SW GOLDSON INTL (2017) http://ashrae-meteo.info/v2.0/?lat=17.53&lng=-88.30&place=%27%27&wmo=785830&si_ip=SI&ashrae_version=2017
    (list "LAKE CHARLES"      30.125 (list 0.342 0.358 0.376 0.431 0.466 0.501 0.501 0.488 0.458 0.383 0.367 0.352) (list 2.511	2.476	2.419 2.263	2.200 2.139 2.181	2.218 2.268	2.462	2.449 2.484))   ;LAKE CHARLES REGIONAL (2017) http://ashrae-meteo.info/v2.0/?lat=30.13&lng=-93.23&place=%27%27&wmo=722400&si_ip=SI&ashrae_version=2017
    (list "MCMINNVILLE"       45.18  (list 0.322 0.319 0.331 0.359 0.370 0.362 0.352 0.355 0.345 0.334 0.327 0.328) (list 2.518 2.535 2.504 2.392 2.378 2.411	2.460 2.467	2.488	2.527 2.510 2.463))   ;MCMINNVILLE MUNICIPAL (2017) http://ashrae-meteo.info/v2.0/?lat=45.18&lng=-123.13&place=%27%27&wmo=726881&si_ip=SI&ashrae_version=2017
    (list "Lindenberg"        52.217 (list 0.323 0.350 0.390 0.412 0.410 0.407 0.427 0.421 0.383 0.373 0.348 0.317) (list 2.314 2.290	2.199 2.217 2.267 2.300 2.254	2.285 2.367	2.348 2.352 2.331))   ;Lindenberg (2017) http://ashrae-meteo.info/v2.0/?lat=52.22&lng=14.12&place=%27%27&wmo=093930&si_ip=SI&ashrae_version=2017
    (list "Croker river"      69.280 (list 0.158 0.207 0.246 0.292 0.332 0.328 0.355 0.339 0.303 0.232 0.155 0.000) (list 1.887 2.049	2.188 2.127 2.131 2.357 2.382	2.480	2.491 2.170 1.919 0.000))   ;Croker river (2017) http://ashrae-meteo.info/v2.0/?lat=69.28&lng=-119.22&place=%27%27&wmo=710590&si_ip=SI&ashrae_version=2017 -- N/A for December was set to 0
    )
end

;______________________________________________________________________________________________________________________________________
;________ Growth Related Reporters
;______________________________________________________________________________________________________________________________________
; seq()-Function similar to R
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

;______________________________________________________________________________________________________________________________________
;________ Shading related Functions
;______________________________________________________________________________________________________________________________________
to-report ShadingTrees
  ; We use three linear Functions (y = ax + b) that sourrund the Gap that we examine. With this, we can only calculate Shadows of Trees within these 3 Lines.
  ; We know "a" (from Solar Azimuth).
  ; We also know 3 different points. A vertical line between the Midpoint of the Forest Gap and ax + b is imagined.
  ; In a Distance of the Gap-Size plus the Maximum Crown-Radius, 2 Points are located.
  ; The last Point is located in the opposite Direction as the Sun, as seen from the Forest Gap.
  ; We know it's "a", its a Line vertical to the other two Lines and can be derived from Solar Azimuth.

  let TreeOut (turtle-set)                        ; Create an empty Agentset first.
  let MaxRCrown (max [Crown-Radius] of trees)     ; Save the maximum Crown Radius of all Trees.

  ; Now calculate Coordinates of the 3 previously mentioned Points. Coordinates are saved in a List [x y]
  let C1 (list ((cos SolarAzimuth) * (Observation-Size + MaxRCrown)) ((sin(180 + SolarAzimuth)) * (Observation-Size + MaxRCrown)))        ; The first Point is right of the Gap.
  let C2 (list ((cos SolarAzimuth) * -1 * (Observation-Size + MaxRCrown)) ((sin(SolarAzimuth)) * (Observation-Size + MaxRCrown)))         ; The second Point is left of the Gap.
  let C3 (list ((sin(180 + SolarAzimuth)) * (Observation-Size + MaxRCrown)) ((cos(SolarAzimuth)) * -1 * (Observation-Size + MaxRCrown)))  ; The third Point is on top or below the Gap.

  let a 1e10                                       ; Create an "a" near 0 in case SolarAzimuth is 0 or 180 which would result is sin(180) = 0 and in Calculation Error.
  if not member? SolarAzimuth (list 0 180) [       ; If SolarAzimuth is not 0 or 180...
    set a (cos SolarAzimuth) / sin(SolarAzimuth)   ; Calculate "a" from cos / sin.
  ]
  let bx1 (item 1 C1) - a * (item 0 C1)            ; Then use a and the previously calculated Coordinates to get "b"
  let bx2 (item 1 C2) - a * (item 0 C2)
  let bx3 (item 1 C3) + 1 / a * (item 0 C3)        ; for the third Point a = 1 / a
  if SolarAzimuth < 180 [                          ; In case the sun already passed South, we need to switch Point 1 and 2.
    let bx4 bx1                                    ; We later ask if Patches are between Line 1 and 2. For this we need the lower Line to be the first one.
    set bx1 bx2
    set bx2 bx4
  ]

  ifelse SolarAzimuth > 90 AND SolarAzimuth <= 270 [                                                            ; If the Sun is in the upper half of the Plot, we also need to look for Trees in the Upper half, we therefore change the Conditions.
    set TreeOut trees with [ycor <= a * xcor + bx1 AND ycor >= a * xcor + bx2 AND ycor <= -1 / a * xcor + bx3]  ; It is in the lower half, see if Trees are between Line 1 and 2 and below of Line 3.
  ] [
    set TreeOut trees with [ycor <= a * xcor + bx1 AND ycor >= a * xcor + bx2 AND ycor >= -1 / a * xcor + bx3]  ; If the Sun is in the upper half, see if Trees are between Line 1 and 2 and above Line 3.
  ]
  report TreeOut
end


to-report FunctionEllipse [x0 y0 .CrownHeight .CrownRadius]
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
  let aq .CrownRadius ^ 2
  let bq (((.CrownHeight / 2) ^ 2) * ((1 / tan(SolarAltitude)) ^ 2) + (.CrownRadius ^ 2))
  ; We then define the tilting Angle alpha.
  let alpha SolarAzimuth + 180
  ; We simplefy the function by defining variables for parts of the function
  let u cos alpha
  let v sin alpha
  let s aq * bq
  let p (aq * u ^ 2 + bq * v ^ 2)
  let q (bq - aq) * u * v
  let r (aq * v ^ 2 + bq * u ^ 2)
  ; We can now calculate Minimum and Maxmimum possible X-Values
  let pxmin x0 - sqrt((s * p) / (r * p - q ^ 2))
  let pxmax x0 + sqrt((s * p) / (r * p - q ^ 2))
  ; When Shadows are long, they transverse the Edge of the World. To avoid that, reset the Limits of X to the Minimum and Maximum World-Coordinates. ; Does not work with points-at
  ;if pxmin < min-pxcor [set pxmin min-pxcor]
  ;if pxmax > max-pxcor [set pxmax max-pxcor]
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




;______________________________________________________________________________________________________________________________________
;________ Importing of External Data
;______________________________________________________________________________________________________________________________________
to-report data.frame [InList ncol]
  let outdata (list)
  let xstart 0
  repeat ((length InList) / ncol) [
    set outdata lput (sublist InList (xstart) (xstart + ncol)) outdata
    set xstart xstart + ncol
  ]
  report outdata
end

to-report import-Tree-Data ; This has to be changed when we make the model public.
  report data.frame(read-from-string (word "[" fetch:url (word "https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/Marks_Reconstructed" StandNumber ".txt") "]")) 3
end

to-report import-Tree-Data2 ; This has to be changed when we make the model public.
  let xCoords read-from-string (word "[" fetch:url "https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/x_Coords.txt" "]")
  let yCoords read-from-string (word "[" fetch:url "https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/y_Coords.txt" "]")
  let DBHs read-from-string (word "[" fetch:url "https://raw.githubusercontent.com/JHeinermann/ShadingModelOpen/main/DBH.txt" "]")
  report (list xCoords yCoords DBHs)
end




;______________________________________________________________________________________________________________________________________
;________ Literature:
;______________________________________________________________________________________________________________________________________
; Bugmann, H. 1994. On the Ecology of mountainous Forests in a Changing Climate: A Simulation Study. 10.3929/ethz-a-000946508
; Kellner, K.; Swihart, R. 2017. Simulation of oak early life history and interactions with disturbance via an individual-based model, SOEL.
; Kenefic, L.S.; Nyland, R.D. 1999. Sugar Maple Height-Diameter and Age-Diameter Relationships in an Uneven-Agend Northern Hardwoord Stand.
; Naraghi, M. 2020. A Demand Based Optimal Solar Panel Orientation. 10.1115/IMECE2010-37918
; Quaschning, V. 2021. Regenerative Energiesysteme: Technologie–Berechnung–Klimaschutz. 3446472061



















@#$#@#$#@
GRAPHICS-WINDOW
525
215
1229
920
-1
-1
0.8571428571428571
1
10
1
1
1
0
0
0
1
-350
350
-350
350
1
1
1
ticks
30.0

BUTTON
425
170
510
203
go-one-Year
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

BUTTON
335
135
510
168
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
335
170
420
203
go-one-Hour
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
335
410
510
470
Latitude
17.539
1
0
Number

SLIDER
335
315
510
348
Crown-Transmissibility
Crown-Transmissibility
0
1
0.05
0.01
1
NIL
HORIZONTAL

CHOOSER
335
365
510
410
WeatherStation
WeatherStation
"Phillip SW Belize" "Lake Charles" "McMinnville" "Lindenberg" "Croker river"
0

SLIDER
335
535
510
568
Forest-Gap-Size
Forest-Gap-Size
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
335
485
510
518
World-Size
World-Size
50
800
700.0
50
1
NIL
HORIZONTAL

SWITCH
520
95
650
128
PlotShadow?
PlotShadow?
1
1
-1000

CHOOSER
335
270
510
315
CrownShape
CrownShape
"Disk" "Ellipsoid"
0

SWITCH
520
130
650
163
PlotAllShadows?
PlotAllShadows?
1
1
-1000

CHOOSER
335
225
510
270
StandNumber
StandNumber
1 2 3 4 5 6 7 8 9 10
0

TEXTBOX
20
20
345
135
This is a shading model that calculates shading of a forest to the ground. You can easily run the model if you follow the instructions.
20
0.0
1

TEXTBOX
20
140
215
160
Before the Simulation
18
0.0
1

TEXTBOX
20
170
310
701
Before you start the Simulation, you should choose some Settings. We will explain what you can choose from. You can also leave everything as it is.\n\nFirst, pick a forest stand. We prepared 10 different stands for you.\n\nSecond, choose the Tree Parameters. \nYou can set the Shape of Tree Crowns. At the moment, the Model supports 2D-Disk Crowns and 3D-Ellipsoid Crowns.\n\nAlso set the Crown Transmissibility. This defines how much Light goes through the Crown. 0 = 0% of Light from above the Canopy is transmitted by the Crown. 100 = 100% of Light is transmitted by the Crown. \n\nThird, choose a Weather Station. We implemented 5 Weather Stations along a latitudal gradient. Shading near the Equator is different from Shading in the North.\n\nFourth, set the Size of the Model World. You can set the Size between 50 x 50 m and 800 x 800 m. In small Worlds, Shadows are sometimes so long that they exit the Model World. Large Worlds take a long Time to simulate. We recommend a World-Size of 700 x 700 m.\n\nOur Model simulates a Gap in the Forest in the Middle of our World. Trees in the Middle of the World are cut before the Simulation. The Forest-Gap-Size defines the radius of this Gap. \n\nIf you finished setting everything up, press the \"Setup\"-Button. You will see our Model-World unfold.\nThen, press \"go-one-Year\" and the Model will simulate a whole year. You can stop the Model by pressing \"go-one-Year\" again and resume it by pressing the Button again. If you only want to simulate the next Hour, press \"go-one-Hour\".
11
0.0
1

TEXTBOX
660
20
890
50
During the Simulation
18
0.0
1

TEXTBOX
660
50
920
205
The Simulation now starts. If the Simulation Speed is slow, increase the Speed using the Slider above. \n\nThe Model now simulates Shading of a Forest to the Ground. You can see the Shading on the Ground. At the Moment, you see all Shadows but we are only interested at the Shading inside the Gap. You can plot only Shadows that are important for us if you switch off \"PlotAllShadows\". If you only want to see the Results and not the Shading Process, turn off \"PlotShadows\".
11
0.0
1

TEXTBOX
955
50
1160
195
The Model is now running for a Year. The current Date is available as Day of Year (DOY, 1 = January 1st, 365 = December 31st) and Time (1 = 01:00, 18 = 18:00).\n\nThe Model is stopping at DOY 365 and Hour 24. You cn also manually stop the Simulation by pressing \"go-one-Year\" and resume the Simulation by pressing the Button again.
11
0.0
1

MONITOR
1170
50
1227
95
DOY
DOY
0
1
11

MONITOR
1170
95
1227
140
Hour
Hour
0
1
11

TEXTBOX
1255
50
1490
75
After the Simulation
18
0.0
1

TEXTBOX
1255
75
1540
701
The Simulation is finished, if DOY 365 and Hour 24 is reached. DOY and Hour are reset automatically.\n\nOn the World-Map you can now see that the Middle of the Plot is colored. This is the amount of Light each m² in the Model World received ofer the course of a Year. Lighter Regions received more Light, dark Regions received less Light. \n\nBut you can also choose to see other Results.\n\n\n\n\n\nTrees grow better in direct Sunlight. Even if Trees only receive direct Sunlight for a small amount of Time, this Radiation can still contribute significantly to total amount of Light a Plant receives per Day. You can see the amount of Instances a m² of Ground received direct Sunlight if you click here:\n\n\n\n\n\nIt is also important to see the Distribution of Shading inside the Gap. A measurement for that is the Weighted Centroid of Radiation Energy inside the observed Area. You can also calculate the Weighted Centroid of Direct Sunlight.\n\n\n\n\n\n\n\nSpecies Distribution inside the Gap is very interesting. We split the Gap into 3 Categories: \n- Suitable for Light Demanding Species\n- Suitable for Intermediate Species\n- Suitable for Shade Tolerant Species\n\nClick here to find out the Distribution of Species inside the Gap:
11
0.0
1

BUTTON
1300
225
1470
258
Show Radiation
Plot-Results \"TotalRadiation\"
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
1300
360
1470
393
Show Full Sunlight
Plot-Results \"TotalFull\"
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
1300
475
1470
508
Show Radiation Centroid
Plot-Results \"WeightedRad\"
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
1300
510
1470
543
Show Sunlight Centroid
Plot-Results \"WeightedFullS\"
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
1300
660
1470
693
Show Species Distribution
Plot-Results \"Species\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
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
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8545"/>
    <metric>[pxcor] of patches</metric>
    <metric>[pycor] of patches</metric>
    <metric>[RadiationTotal] of patches</metric>
    <enumeratedValueSet variable="PlotAllShadows?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Observation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="World-Size">
      <value value="700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Forest-Gap-Size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PlotShadow?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CrownShape">
      <value value="&quot;Ellipsoid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Crown-Transmissibility">
      <value value="0.82"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="WeatherStation">
      <value value="&quot;Lindenberg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Observation-Size">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Latitude">
      <value value="52.217"/>
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
1
@#$#@#$#@
