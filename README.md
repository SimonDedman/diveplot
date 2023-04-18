# diveplot
ggPlot dive and other (temperature, light, location, etc.) data from electronic tags on marine species

Plot depth, internal and external temperature, light level, bathymetric depth at location, thermocline, animal position on map, animal residence/transit state, sunrise, sunset; and label various types of dive behaviour, adding depths for deep dives, depths and durations for U shaped dives, highlighting days of diel vertical migration (follows light to the mesopelagic). Provides one png image per ID (e.g. per tagged fish) per day.

![5199104_98504_NO99_W13D_1999-02-15](https://user-images.githubusercontent.com/4599748/232644091-2c004724-046a-45e0-854f-971a186bbf2f.png)

## Horizontal Elements

### Plotted on top panel

Internal temperature: from tag data.

External temperature: from tag data.

Excess temperature (internal minus external, smoothed).

### Plotted on bottom panel

Light level: from tag data

Location: provided or derived from tag data. Demarked as Transiting (open circle) or residing (closed circle) movement state: calculated from positional data. Plotted on full track of all animal's positions, on map background.

### Plotted on both panels

Depth: from tag data.

Ocean depth: from geoposition lookup against GEBCO bathymetry.

Thermocline depth: calculated from depth and temperature gradients from tag data.

## Vertical elements

### Plotted on top panel

Forage A dive behaviour: ~100m max, steep thermocline. Labelled per hour.

Forage B dive behaviour: ~300m max, thermocline to dive bottom. Labelled per hour.

Forage C dive behaviour: Bathymetrically limited. Labelled per hour.

General forage dive behaviour: energetic exploration of depths, derived from vertical velocity, from depth and time. Grey highlight for that hour; pertains to forage types A & B & General.

U-shaped dive behaviour: discrete dive lasting at least X minutes and past Ym depth. Depth and duration labelled at midpoint of dive.

Spike dive behaviour: animal dives before dawn and after dusk to crossreference apogee light angle against magnetic field to geolocate. Changes dawn and dusk lines from hatched to solid if present.

### Plotted on bottom panel

Dawn & dusk: lookup based on position and datetime. Bottom panel. [what causes them to be hatched vs solid?]

Deep dive behaviour: any dive past 500m depth. Labelled at bottom of plot, at time of depth nadir. Solid if over 500m else hatched.

## Whole panel elements

Diel Vertical Migration day: animal follows the light level deep during the day, presumably to hunt in the mesopelagic. Whole image frame bordered with a red line.

# More example images

Forage B dive behaviour
![5199104_98504_NO99_W13D_1999-02-16](https://user-images.githubusercontent.com/4599748/232647067-d73ddf7d-babf-49cc-9b5e-dc52df0cb44e.png)
Forage C dive behaviour & resident movement state
![5120077_20P1125_NO20_P02D_2021-02-10](https://user-images.githubusercontent.com/4599748/232647508-cd893a69-92e9-46d1-9cc3-53d4981fc531.png)
DVM day
