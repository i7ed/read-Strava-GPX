##############################
# analyze STRAVA data
# (c) 2017-2019 Thomas Gredig
#############################

# read the GPX file downloaded from STRAVA
library(plotKML)
path.SOURCE = 'data'
file.list = file.path(path.SOURCE,dir(path.SOURCE, pattern='gpx$'))

# select a file
filename = file.list[3]
d = readGPX(filename)

# get the tracks information
d[['tracks']] -> df
df[[1]] -> m
df = m[[1]]

# reformat the time
df$timeGMT = as.POSIXct(strptime(df$time, "%Y-%m-%dT%H:%M:%S"))
# make elevation data numeric
df$ele = as.numeric(df$ele)
# compute x,y coordinates in units of meters
R.Earth = 6371000
min(df$lon) -> mn.lon
min(df$lat) -> mn.lat
df$x = (df$lon - mn.lon)/180*pi*R.Earth
df$y = (df$lat - mn.lat)/180*pi*R.Earth
# compute distance between steps (ignore z-axis) for now
df$dr = c(0,sqrt(diff(df$x)^2 + diff(df$y)^2))

plot(df$dr)
# find average sampling distance
mean(df$dr)
sd(df$dr)
start.time = min(as.numeric(df$timeGMT))
df$time = as.numeric(df$timeGMT) - start.time
df$dt = c(0,diff(df$time))
# find speed
df$v = df$dr / df$dt * 3.6
head(df)

# find places with stop 
plot(df$v)
q.stopped = which(df$v<2)
points(q.stopped, df$v[q.stopped], col='red', pch=20)


plot(df$x, df$y)
points(df$x[q.stopped], df$y[q.stopped], col='red', pch=20, cex=3)

# find total elevation gain
sum(abs(diff(df$ele)))/2

# find total length of trip
sum(df$dr)

df.stops = df[q.stopped,]
library(ggplot2)
ggplot(df, aes(x/1e3,y/1e3)) + 
    geom_point() +
    geom_point(data = df.stops, col='white', size = 4) + 
    geom_point(data = df.stops, col='red', size = 3, alpha=0.4) + 
    xlab('x (km)') + ylab('y (km)') 
ggsave('Map with Stops.png', width=6,height=4)
