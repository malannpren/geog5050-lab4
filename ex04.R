setwd("E:\\FA20\\Stats\\Exercise 4")

install.packages('sf')
library('sf')
install.packages('tmap')
library('tmap')
install.packages('GISTools')
library('GISTools')
install.packages('spatstat')
library('spatstat')

tornus.sf <- st_read("E:\\FA20\\Stats\\Exercise 4\\torn_tchdown.shp")
states.sf <- st_read("E:\\FA20\\Stats\\Exercise 4\\States.shp")
study_frame.sf <- st_read("E:\\FA20\\Stats\\Exercise 4\\StudyFrame.shp")

typeof(tornus.sf)
class(tornus.sf)

tornus.sp <- as(tornus.sf, "Spatial")
states.sp <- as(states.sf, "Spatial")
frame.sp <- as(study_frame.sf, "Spatial")

plot(tornus.sp, pch=16, cex=.05)

torn_kde <- kde.points(tornus.sp, 200000, n=500, lims=NULL)
level.plot(torn_kde)
plot(states.sp, alpha=0, add=T)

# part 3

tornus_clip <- tornus.sp[frame.sp,]
states_clip <- st_intersection(study_frame.sf, states.sf)
tm_shape(tornus_clip) + tm_borders(col='black') + tm_layout(frame = F) + tm_shape(states_clip) + tm_dots(col='#FB6a4A', size=0.2, shape=1, alpha=0.5)

tm_shape(frame.sp)+
  tm_borders(col="black") +
  tm_layout(frame=F) +
  tm_shape(tornus_clip) +
  tm_dots(col = NA, size = 0.02, shape = 16, title = NA,
          legend.show = TRUE, legend.is.portrait = TRUE) +
  tm_shape(states_clip) +
  tm_borders(col="grey70",lw=2) +
  tm_layout (frame=F)

tornus.sp <- as(tornus_clip, "Spatial")

tornus.ppp <- as(tornus.sp, "ppp")
class(tornus.ppp)

# k function

kf <- Kest(tornus.ppp, correction='border')
plot(kf)
kf.env <- envelope(tornus.ppp,Kest, correction='border')
plot(kf.env, main='Tornado Clustering')

lf <- Lest(tornus.ppp, correction='border')
plot(kf)
lf.env <- envelope(tornus.ppp,Lest, correction='border')
plot(lf.env, main='Tornado Clustering')

gf <- Gest(tornus.ppp, correction='border')
plot(gf)
gf.env <- envelope(tornus.ppp,Gest, correction='border')
plot(gf.env, main='Tornado Clustering')

mad.test(tornus.ppp, Kest)
