#create a file for each exlipse soundscape site with sun times 8/18-8/25
# manually copy paste site and lat lon... then run for each site

rm(list=ls())
library(V8)
library(RAtmosphere)
library(suncalc)
workingDir = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\ANALYSIS"

#ONE SITE....
site = "BUISE03916"
Lat = 17.78733
Lon = -64.62093
dy = as.Date("2017-08-18")

t = getSunlightTimes(date = dy, lat = Lat, lon = Lon, tz = "UTC")

out = as.matrix( getSunlightTimes(date = seq.Date(dy, dy+6, by = 1),
                 lat = Lat, lon = Lon, tz = "UTC")) 

outFileName <- paste0(workingDir, "\\", site,"_Eclipse_sunTimes.csv")
write.csv(out, file=outFileName, na ="NaN", quote=F, row.names=F)

#error with suncalc... one day off for all sites..
# checked with  https://www.esrl.noaa.gov/gmd/grad/solcalc/sunrise.html
# FODOE10882: 36.4901872	 36°29'24.67"N	-87.8611261	 87°51'40.05"W

site = "BUISE03916"
Lati =   17.78733
Long = -64.62093
dy = as.Date("2017-08-18")
dt = as.numeric(format(as.POSIXct(dy, format="%Y-%m-%d"),"%j"))
estsun = suncalc(dt, Lat = Lati, Lon = Long, UTC=T)
format(as.POSIXct(dy + estsun$sunrise ), "%H:%M", tz="UTC")
format(as.POSIXct(dy + estsun$sunset ),  "%H:%M", tz="UTC")

site = "FODOE10882"
Lat =   36.4901872
Lon = -87.8611261
dy = as.Date("2017-08-18")

t = getSunlightTimes(date = dy, lat = Lat, lon = Lon, tz = "UTC")
t$sunrise
t$sunset

