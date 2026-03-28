#Load packages
library(raster)
library(rgdal)
library(rgeos)
#
setwd("C:/timeseries")

#MODEL 1
###read parameters from EVI for site
sos.s1 <- raster("READ RASTER")#Raster layer for start of season
eos.s1 <- raster("READ RASTER")#Raster layer for end of season
ld.s1 <- raster("READ RASTER")#Raster layer for left derivative (greening rate)
rd.s1 <- raster("READ RASTER")#Raster layer for right derivative (browning rate)
mv.s1 <- raster("READ RASTER")#Raster layer for maximum value


para.s1 <- stack(mv.s1,sos.s1,eos.s1,ld.s1,rd.s1)
####defining constants
a <- 0.81
b <- 0.78
c <- 0.81
d <- 0.80
e <- 0.78

mod_1_fun <- function(x,y,z,q,v){
  ifelse(x >= a & y >= b & z >= c,1, ifelse(q <= d & v <= e, 2, 3))
}

mod_1_ndvi <- overlay (para.s1, fun=mod_1_fun)

#from EVI
###read parameters from EVI for site
sos.s1 <- raster("READ RASTERf")
eos.s1 <- raster("READ RASTER")
ld.s1 <- raster("READ RASTER")
rd.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")


para.s1 <- stack(mv.s1, sos.s1, eos.s1, ld.s1, rd.s1)

####defining constants
a <- 0.75
b <- 0.76
c <- 0.76
d <- 0.79
e <- 0.74

mod_1_fun <- function(x,y,z,q,v){
  ifelse(x >= a & y >= b & z >= c,1, ifelse(q <= d & v <= e, 2, 3))
}

mod_1_evi <- overlay (para.s1, fun=mod_1_fun)

#from NDWI
###read parameters from NDWI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTER")
ld.s1 <- raster("READ RASTER")
rd.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")

####defining constants
a <- 0.81
b <- 0.79
c <- 0.79
d <- 0.77
e <- 0.81

mod_1_fun <- function(x,y,z,q,v){
  ifelse(x >= a & y >= b & z >= c,1, ifelse(q <= d & v <= e, 2, 3))
}

mod_1_ndwi <- overlay (para.s1, fun=mod_1_fun)

mod_1_stack <- stack(mod_1_ndvi, mod_1_evi,mod_1_ndwi) 

###assigning thresholds/constants
a <- 1
b <- 2
c <- 3
####
mod_1_fun <- function(x,y,z){
  ifelse(x == a | y == a | z == a,1, ifelse(x == b | y == b | z == b,2,3))
}
model_1 <- overlay(mod_1_stack, fun=mod_1_fun)#creates raster 


#MODEL 2

###read parameters from EVI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTER")
ld.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")

para.s1 <- stack(mv.s1, sos.s1, eos.s1, ld.s1)
####defining constants
a <- 0.81
b <- 0.78
c <- 0.81
d <- 0.78


mod_2_fun <- function(x,y,z,q){
  ifelse(x >= a & y >= b & z >= c,1, ifelse(q <= d, 2, 3))
}

mod_2_ndvi <- overlay (para.s1, fun=mod_2_fun)

#from EVI

###read parameters from EVI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTER")
ld.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")


para.s1 <- stack(mv.s1, sos.s1, eos.s1, ld.s1)

####defining constants
a <- 0.75
b <- 0.76
c <- 0.77
d <- 0.79


mod_2_fun <- function(x,y,z,q){
  ifelse(x >= a & y >= b & z >= c,1, ifelse(q <= d, 2, 3))
}

mod_2_evi <- overlay(para.s1, fun=mod_2_fun)

mod_2_stack <- stack(mod_2_ndvi, mod_2_evi)
a <- 1
b <- 2
c <- 3

mod_2_fun <- function(x,y,z){
  ifelse(x == a & y == a,1,ifelse(x ==b | y == b,2,ifelse(x == c | y== c,3)))
}

model_2 <- overlay(mod_2_stack, fun = mod_2_fun)

#MODEL 3

#from  NDVI
###read parameters from EVI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")

para.s1 <- stack(mv.s1, sos.s1, eos.s1)
####defining constants
a <- 0.81
b <- 0.78
c <- 0.81

mod_3_fun <- function(x,y,z){
  ifelse(x >= a & y <= b, 1, ifelse (z <= c, 2, 3))
}
mod_3_ndwi <- overlay(para.s1, fun = mod_3_fun)
#From NDWI
###read parameters from EVI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")

para.s1 <- stack(mv.s1, sos.s1, eos.s1)
####defining constants
a <- 0.75
b <- 0.76
c <- 0.76

mod_3_fun <- function(x,y,z){
  ifelse(x >= a & y <= b, 1, ifelse(z <= c, 2, 3))
}
mod_3_evi <- overlay(para.s1, fun = mod_3_fun)

#combine
mod_3_stack <- stack(mod_3_ndvi, mod_3_evi)
a <- 1
b <- 2
c <- 3

mod_3_fun <- function(x,y){
  ifelse(x == a | y == a,1, ifelse(x == b | y == b,2,3))
}

model_3 <- overlay(mod_3_stack, fun = mod_3_fun)

#MODEL 4
#from EVI

###read parameters from EVI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTERf")
ld.s1 <- raster("READ RASTER")
rd.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")

para.s1 <- stack(mv.s1, sos.s1, eos.s1, ld.s1, rd.s1)

####defining constants
a <- 0.75
b <- 0.76
c <- 0.76
d <- 0.79
e <- 0.81

mod_4_fun <- function(x,y,z,q,v){
  ifelse(x >= a & y >= b & z >= c,1, ifelse(q <= d & v <= e, 2, 3))
}

mod_4_evi <- overlay (para.s1, fun=mod_4_fun)

#from NDWI
###read parameters from NDWI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTER")
ld.s1 <- raster("READ RASTER")
rd.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")

para.s1 <- stack(mv.s1, sos.s1, eos.s1, ld.s1, rd.s1)
####defining constants
a <- 0.81
b <- 0.79
c <- 0.79
d <- 0.77
e <- 0.81

mod_4_fun <- function(x,y,z,q,v){
  ifelse(x >= a & y >= b & z >= c,1, ifelse(q <= d & v <= e, 2, 3))
}

mod_4_ndwi <- overlay(para.s1, fun=mod_4_fun)

#combine
mod_4_stack <- stack(mod_4_ndwi,mod_4_evi)

a <- 1
b <- 2
c <- 3

mod_4_fun <- function(x,y,z){
  ifelse(x == a | y == a | z == a,1, ifelse(x == b | y == b | z == b,2,3))
}

model_4 <- overlay(mod_4_stack, fun = mod_4_fun)

#######KURESOI#######
####################
#MODEL 1
###read parameters from NDVI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTER")
ld.s1 <- raster("READ RASTER")
rd.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")

para.s1 <- stack(mv.s1,sos.s1,eos.s1,ld.s1,rd.s1)
####defining constants
a <- 0.54
b <- 0.55
c <- 0.55
d <- 0.58
e <- 0.56

mod_1_fun <- function(x,y,z,q,v){
  ifelse(x >= a & y >= b & z >= c,1, ifelse(q <= d & v <= e, 2, 3))
}

mod_1_ndvi <- overlay (para.s1, fun=mod_1_fun)

###read parameters from EVI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTER")
ld.s1 <- raster("READ RASTER")
rd.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")

para.s1 <- stack(mv.s1, sos.s1, eos.s1, ld.s1, rd.s1)

####defining constants
a <- 0.49
b <- 0.50
c <- 0.49
d <- 0.54
e <- 0.54

mod_1_fun <- function(x,y,z,q,v){
  ifelse(x >= a & y >= b & z >= c,1, ifelse(q <= d & v <= e, 2, 3))
}

mod_1_evi <- overlay (para.s1, fun=mod_1_fun)

#from NDWI
###read parameters from NDWI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTER")
ld.s1 <- raster("READ RASTER")
rd.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")

####defining constants
a <- 0.83
b <- 0.82
c <- 0.82
d <- 0.79
e <- 0.80

mod_1_fun <- function(x,y,z,q,v){
  ifelse(x >= a & y >= b & z >= c,1, ifelse(q <= d & v <= e, 2, 3))
}

mod_1_ndwi <- overlay (para.s1, fun=mod_1_fun)

mod_1_stack <- stack(mod_1_ndvi, mod_1_evi,mod_1_ndwi) 

###assigning thresholds/constants
a <- 1
b <- 2
c <- 3
####
mod_1_fun <- function(x,y,z){
  ifelse(x == a | y == a | z == a,1, ifelse(x == b | y == b | z == b,2,3))
}
model_1 <- overlay(mod_1_stack, fun=mod_1_fun)#creates raster 

##MODEL 3
##from  NDVI
###read parameters from EVI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")

para.s1 <- stack(mv.s1, sos.s1, eos.s1)
####defining constants
a <- 0.83
b <- 0.82
c <- 0.82

mod_3_fun <- function(x,y,z){
  ifelse(x >= a & y <= b, 1, ifelse (z <= c, 2, 3))
}
mod_3_ndwi <- overlay(para.s1, fun = mod_3_fun)
#From NDWI
###read parameters from EVI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")

para.s1 <- stack(mv.s1, sos.s1, eos.s1)
####defining constants
a <- 0.49
b <- 0.50
c <- 0.49

mod_3_fun <- function(x,y,z){
  ifelse(x >= a & y <= b, 1, ifelse(z <= c, 2, 3))
}
mod_3_evi <- overlay(para.s1, fun = mod_3_fun)

#combine
mod_3_stack <- stack(mod_3_ndvi, mod_3_evi)
a <- 1
b <- 2
c <- 3

mod_3_fun <- function(x,y){
  ifelse(x == a | y == a,1, ifelse(x == b | y == b,2,3))
}

model_3 <- overlay(mod_3_stack, fun = mod_3_fun)

#MODEL5
##read parameters from NDWI for site
sos.s1 <- raster("READ RASTER")
eos.s1 <- raster("READ RASTER")
mv.s1 <- raster("READ RASTER")

para.s1 <- stack(mv.s1, sos.s1, eos.s1)
####defining constants
a <- 0.83
b <- 0.82
c <- 0.82

mod_5_fun <- function(x,y,z){
  ifelse(x >= a & y <= b, 1, ifelse(z <= c, 2, 3))
}
mod_5_evi <- overlay(para.s1, fun = mod_5_fun) # creates raster for model 5, only one index
