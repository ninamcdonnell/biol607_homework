
#1. Load earthquake data. Use str() and summary()
data(quakes)
?quakes

str(quakes)
summary(quakes)
View(quakes)
?quakes

#2. Show entirety of column "long"
quakes$long

#3. Try function unique() and length(). What unique stations are there?
unique(quakes$stations)
length(quakes$stations)
max(quakes$stations)
#'From "?quakes", I can see that "stations" tells us the number of stations reporting. 
#'We cannot tell how many unique stations there are, since some may never have reported.
#'However, from the max() function, we can see that there are at least 132-- since that is the greatest number of
#'stations that reported for any single seismic event.

#4. Use range() to find range of depths where earthquakes occur
range(quakes$depth)
#The range of depth for which earquakes occurred is 40-680 km

#E.C. Where was the earthquake of largest magnitude found?
max(quakes$mag) #max mag=6.4
View(quakes) #sorted by mag=6.4. Found location: lat=-15.56, long=167.62, depth=127 km

