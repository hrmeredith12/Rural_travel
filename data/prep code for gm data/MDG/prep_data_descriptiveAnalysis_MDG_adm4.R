## This script compiles data on Madagascar travel patterns for adm4 level. 
# inputs: 
   # districtcorres_2.csv             - key connecting names from Orange (network provider) and most maps (i.e. world pop or diva-gis)
   # MDG_adm4_pop_urb_coords.csv      - provides population size, urbanicity, and coordinates (centroid) for each region (adm2) 
   # MDG_CommuneDistrictRegionFile.csv - provides names for adm2-4
   # MDG_Commune_Mobility.csv         - provides daily trip counts from origin (adm2-4 provided for both origin and destination)
# Note: Adm2 = Region, adm4 = district, Adm4 = commune

# output: one master dataset that has the region, district, 
# and commune names and ID; population density, average number of trips made over the time
# normalized daily trips, distance between trips
  # the output will be used in a visualization script : " "

# by Hannah Meredith
# last updated Feb 6, 2020


setwd("C:/Users/Hannah/Dropbox/Rural Travel/data/prep code for gm data/MDG") 
      
library("geosphere")  # haverstine distance function
library("dplyr")
library("reshape2")

# 1. Import key for translating between census names and Orange names of districts and regions. Make sure all are capitalized
commune.match.file <- read.csv('districtcorres_2.csv', header = TRUE) 
commune.match.file$loc.index <- 1:nrow(commune.match.file)
commune.match.file[,1:4] <- t(apply(commune.match.file[,1:4], 1, function(x) toupper(x)))

# import data that links region and district names to coordinates, pop density and coordinates
MDG.details <- read.csv('MDG_adm4_pop_urb_coords.csv', header = TRUE)
MDG.details <- MDG.details[,c('NAME_1', 'NAME_2', 'NAME_3','NAME_4', 'ID_1', 'ID_2', 'ID_3', 'ID_4', 'X_coord', 'Y_coord', "pop2010sum", "urb19")]
MDG.details[,1:4] <- t(apply(MDG.details[,1:4], 1, function(x) toupper(x)))
MDG.details <- MDG.details[order(MDG.details$ID_4),]


commune.mobility.file <- read.csv('MDG_Commune_Mobility.csv', header = TRUE)
commune.mobility.file[,1:6] <- t(apply(commune.mobility.file[,1:6], 1, function(x) toupper(x)))
commune.mobility.file[,7:169] <- t(apply(commune.mobility.file[,7:169], 1, function(x) ifelse(x>100,NA,x)))

trip.data.long <- melt(commune.mobility.file, 
                              id.vars = c("OriginRegion", "OriginDistrict", "OriginCommune",
                                         "DestRegion", "DestDistrict", "DestCommune"),
                              variable.name = "date",
                              value.name = "trips")


# Convert dates from french/format to date format
dates.french <- as.character(trip.data.long$date)
dates.sep <- strsplit(dates.french, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE)

days <- sapply(dates.sep, function(x) x[2])  # pull day portion out of list
trip.data.long$d <- substr(days, 1, nchar(days) - 2)     # keep first date (i.e. date at start of trip)

months <- sapply(dates.sep, function(x) x[1])  # pull month portion out of list
trip.data.long$m <-  ifelse(months == "janv", 01,
                     ifelse(months == "fev", 02,
                            ifelse(months == "mars", 03,
                                   ifelse(months == "avr", 04,
                                          ifelse( months == "mai", 05,
                                                  ifelse(months == "juin",06,0))))))

trip.data.long$y <- rep(2010, dim(trip.data.long)[1])
trip.data.long$date <- as.Date(with(trip.data.long, paste(y, m, d,sep="-")), "%Y-%m-%d")

# trip.data.long <- trip.data.long[, c("OriginRegion","OriginDistrict", "OriginCommune", "DestRegion", "DestDistrict", "DestCommune","m", "d", "y", "date", "trip.adm4")]

## add in details about origin and destination

trip.data.long <- left_join(trip.data.long, MDG.details, by = c("OriginRegion" = "NAME_2", "OriginDistrict" = "NAME_3", "OriginCommune" = "NAME_4"))
trip.data.long <- left_join(trip.data.long, MDG.details, by = c("DestRegion" = "NAME_2", "DestDistrict" = "NAME_3", "DestCommune" = "NAME_4"))

colnames(trip.data.long) <- c("start.adm2.name","start.adm3.name","start.adm4.name", "end.adm2.name", "end.adm3.name", "end.adm4.name", 
                              "date", "trips", "d", "m", "y", "start.adm1.name", "start.adm1.code", "start.adm2.code", "start.adm3.code", "start.adm4.code", "X_start", "Y_start", "pop.start", "urb.start",
                                   "end.adm1.name", "end.adm1.code", "end.adm2.code", "end.adm3.code", "end.adm4.code", "X_end", "Y_end", "pop.end", "urb.end")


# some math to get trips into actual trip counts
trip.data.long$trip.count.adj <- 0.01*0.33*0.5*0.46*trip.data.long$pop.start*trip.data.long$trips  # back calculate the number of trips made (Orange reported % of trips made)


#3. Create distance, trip type, etc matrices
urbanicity <- MDG.details[ , c('ID_4','urb19')]
urbanicity$urb.cat.2 <- ifelse(urbanicity[2] <= 0.001,  ## double check this threshold???
                             1,
                             2)
levels(urbanicity$urb.cat.2) <- c('Rural', 'Urban')

urbanicity$urb.cat.3 <- as.integer(cut(urbanicity[,2],   # a way of binning subdistricts into districts for plotting
                                       breaks = c(-Inf, 0.0009, 0.0029, Inf),#breaks = c(-Inf, 0.0003, 0.001660, Inf),  ## determined by urb tertiles of long trip data later in code
                                     labels = c(1,2,3)))
levels(urbanicity$urb.cat.3) <- c('Low', 'Med', 'High')

population <- MDG.details[ , c('ID_4','pop2010sum')]
coordinates <- MDG.details[,c('ID_4', 'X_coord', 'Y_coord')]

NN <- dim(coordinates)[1]

D <- matrix(NA, nrow=NN, ncol=NN)
D.deg <- matrix(NA, nrow=NN, ncol=NN)
trip.type <- matrix(NA, nrow=NN, ncol=NN)
trip.type.9 <- matrix(NA, nrow=NN, ncol=NN)
in.out <- matrix(NA, nrow=NN, ncol=NN)
in.out.urb2 <- matrix(NA, nrow=NN, ncol=NN)
in.out.TT <- matrix(NA, nrow=NN, ncol=NN)

for (i in 1:NN){
  X_start <- coordinates$X_coord[i]
  Y_start <- coordinates$Y_coord[i]
  for (j in 1:NN){
    X_end <- coordinates$X_coord[j]
    Y_end <- coordinates$Y_coord[j]
    
    D[i,j] <- distHaversine(matrix(c(X_start, Y_start), ncol =2),
                            matrix(c(X_end, Y_end), ncol = 2))/1000
    
    D.deg[i,j] <- ifelse( i == j ,0, sqrt((X_end - X_start)^2 + (Y_end - Y_start)^2))
    
    trip.type[i,j]<- ifelse(urbanicity$ID_4[i] == urbanicity$ID_4[j], 0, #stay
                            ifelse(urbanicity$urb.cat.2[i]==1 & urbanicity$urb.cat.2[j]==1, 1,  # R-R
                                   ifelse(urbanicity$urb.cat.2[i]==1 & urbanicity$urb.cat.2[j]==2, 2, # R-U
                                          ifelse(urbanicity$urb.cat.2[i]==2 & urbanicity$urb.cat.2[j]==1, 3, #U-R
                                                 4)))) #U-U
    
    trip.type.9[i,j]<- ifelse(urbanicity$ID_4[i] == urbanicity$ID_4[j], 0, # stay
                              ifelse(urbanicity$urb.cat.3[i] == 1 & urbanicity$urb.cat.3[j]==1, 1,  # L-L
                                     ifelse(urbanicity$urb.cat.3[i] == 1 & urbanicity$urb.cat.3[j] == 2, 2, #L-M
                                            ifelse(urbanicity$urb.cat.3[i] == 1 & urbanicity$urb.cat.3[j]==3, 3,  #L-H
                                                   ifelse(urbanicity$urb.cat.3[i] == 2 & urbanicity$urb.cat.3[j]==1, 4, #M-L
                                                          ifelse(urbanicity$urb.cat.3[i] == 2 & urbanicity$urb.cat.3[j] == 2, 5, # M-M
                                                                 ifelse(urbanicity$urb.cat.3[i] == 2 & urbanicity$urb.cat.3[j]==3, 6, # M-H
                                                                        ifelse(urbanicity$urb.cat.3[i] == 3 & urbanicity$urb.cat.3[j]==1, 7,  #H-L
                                                                               ifelse(urbanicity$urb.cat.3[i] == 3 & urbanicity$urb.cat.3[j] == 2, 8, #H-M
                                                                                      9))))))))) # H-H
    
    in.out[i,j] <- ifelse(urbanicity$ID_4[i] == urbanicity$ID_4[j], 0, #stay
                          ifelse(MDG.details$ID_3[i] == MDG.details$ID_3[j], 1, # IN
                                 2)) # OUT
    
    in.out.urb2[i,j] <- ifelse(urbanicity$ID_4[i] == urbanicity$ID_4[j], 0, # stay
                               ifelse(in.out[i,j] == 1 && urbanicity$urb.cat.2[i] == 1, 1,   # IN and rural origin
                                      ifelse(in.out[i,j] == 1 && urbanicity$urb.cat.2[i] == 2, 2,  # IN and urb origin
                                             ifelse(in.out[i,j] == 2 && urbanicity$urb.cat.2[i] == 1, 3,  # Out and rur origin
                                                    4)))) # OUT and urb origin
    
    in.out.TT[i,j] <- ifelse(urbanicity$ID_4[i] == urbanicity$ID_4[j], 0,
                             ifelse(in.out[i,j] == 1 && trip.type[i,j] == 1, 1,   # IN and R-R trip
                                    ifelse(in.out[i,j] == 2 && trip.type[i,j] == 1, 2,  # OUT and R-R trip
                                           ifelse(in.out[i,j] == 1 && trip.type[i,j] == 2, 3,   # IN and R-U trip
                                                  ifelse(in.out[i,j] == 2 && trip.type[i,j] == 2, 4,  # OUT and R-U trip
                                                         ifelse(in.out[i,j] == 1 && trip.type[i,j] == 3, 5,   # IN and U-R trip
                                                                ifelse(in.out[i,j] == 2 && trip.type[i,j] == 3, 6,  # OUT and U-R trip
                                                                       ifelse(in.out[i,j] == 1 && trip.type[i,j] == 4, 7,   # IN and U-U trip
                                                                              8))))))))  # OUT and U-U trip
    
    
  }
  
}

colnames(D) <- rownames(D) <- seq(1, NN) 
colnames(D.deg) <- rownames(D.deg) <- seq(1, NN) 
colnames(trip.type) <- rownames(trip.type) <- seq(1, NN)
colnames(trip.type.9) <- rownames(trip.type.9) <- seq(1, NN)
colnames(in.out) <- rownames(in.out) <- seq(1, NN)
colnames(in.out.urb2) <- rownames(in.out.urb2) <- seq(1, NN)
colnames(in.out.TT) <- rownames(in.out.TT) <- seq(1, NN)

levels(trip.type) <- c('stay', 'rural-rural', 'rural-urban', 'urban-rural','urban-urban')
levels(trip.type.9) <- c('stay','low-low', 'low-mid', 'low-high', 'mid-low', 'mid-mid', 'mid-high', 'high-low', 'high-mid', 'high-high')
levels(in.out) <- c('stay','IN', 'OUT')
levels(in.out.urb2) <- c('stay', 'IN-rural.start', 'IN-urban.start', 'OUT-rural.start', 'OUT-urban.start')
levels(in.out.TT) <- c('stay','IN-rural-rural', 'OUT-rural-rural', 'IN-rural-urban', 'OUT-rural-urban', 'IN-urban-rural', 'OUT-urban-rural','IN-urban-urban', 'OUT-urban-urban' )

# save(in.out, file = "MDG_adm4_inOut.RData")
# save(in.out.urb2, file = "MDG_adm4_inOut_Urb2.RData")
# save(in.out.TT, file = "MDG_adm4_inOut_TT.RData")
# save(trip.type, file = "MDG_adm4_tripType.RData")
# save(trip.type.9, file = "MDG_adm4_tripType9.RData")
# save(D, file = "MDG_adm4_distance.RData")
# save(urbanicity, file = "MDG_adm4_urb.RData")
# save(population, file = "MDG_adm4_pop2010.RData")

#7. join other details (i.e. trip distance, trip type, population size, urbanicity)
distance.long <- reshape2::melt(D,
                                varnames = c("i","j"),
                                value.name = "distance")
distance.deg.long <- reshape2::melt(D.deg,
                                    varnames = c("i","j"),
                                    value.name = "distance.deg")
TT.long <- reshape2::melt(trip.type,
                          varnames = c("i","j"),
                          value.name = "trip.type")
TT9.long <- reshape2::melt(trip.type.9,
                           varnames = c("i","j"),
                           value.name = "trip.type9")
in.out.long <- reshape2::melt(in.out,
                              varnames = c("i","j"),
                              value.name = "in.out")
in.out.Urb2.long <- reshape2::melt(in.out.urb2,
                                   varnames = c("i","j"),
                                   value.name = "in.out.Urb2")
in.out.TT.long <- reshape2::melt(in.out.TT,
                                 varnames = c("i","j"),
                                 value.name = "in.out.TT")

trip.data.long <- left_join(trip.data.long, distance.long, by = c('start.adm4.code'='i','end.adm4.code'='j')) 
trip.data.long <- left_join(trip.data.long, distance.deg.long, by = c('start.adm4.code'='i','end.adm4.code'='j')) 
trip.data.long <- left_join(trip.data.long, urbanicity, by = c("start.adm4.code" = "ID_4"))
trip.data.long <- left_join(trip.data.long, TT.long, by = c('start.adm4.code'='i','end.adm4.code'='j')) 
trip.data.long <- left_join(trip.data.long, TT9.long, by = c('start.adm4.code'='i','end.adm4.code'='j')) 
trip.data.long <- left_join(trip.data.long, in.out.long, by = c('start.adm4.code'='i','end.adm4.code'='j')) 
trip.data.long <- left_join(trip.data.long, in.out.Urb2.long, by = c('start.adm4.code'='i','end.adm4.code'='j')) 
trip.data.long <- left_join(trip.data.long, in.out.TT.long, by = c('start.adm4.code'='i','end.adm4.code'='j')) 


#8. adm4 level summary - calculate monthly average trip count (and proportion) and variance
adm4.trip.month <- trip.data.long %>%                                           # sum trips between origin/destination for each month/year
  group_by(start.adm4.code, end.adm4.code, m, y ) %>%
  mutate(adm4.single.trip.sum = sum(trip.count.adj, na.rm = TRUE)) %>%
  distinct(start.adm4.code, end.adm4.code, m, y, .keep_all=TRUE) 

adm4.trip.month <- adm4.trip.month %>%
  group_by(start.adm4.code, m, y) %>%
  mutate(adm4.all.trip.sum = sum(adm4.single.trip.sum, na.rm = TRUE)) 

adm4.trip.month$adm4.single.trip.prop <- adm4.trip.month$adm4.single.trip.sum/adm4.trip.month$adm4.all.trip.sum

adm4.trip.month.avg <- adm4.trip.month %>%
  group_by(start.adm4.code) %>%
  mutate(adm4.all.trip.avg = ceiling(mean(adm4.all.trip.sum, na.rm = TRUE)))%>%
  mutate(adm4.all.trip.var = var(adm4.all.trip.sum, na.rm = TRUE))

adm4.trip.month.avg <- adm4.trip.month.avg %>%                                           # calculated average number of trips made each month between origin/destination
  group_by(start.adm4.code, end.adm4.code) %>%
  mutate(adm4.single.trip.avg = ceiling(mean(adm4.single.trip.sum, na.rm = TRUE))) %>%
  mutate(adm4.single.trip.var = var(adm4.single.trip.sum, na.rm = TRUE)) %>%
  mutate(adm4.single.trip.prop.avg = mean(adm4.single.trip.prop, na.rm = TRUE)) %>%
  mutate(adm4.single.trip.prop.var = var(adm4.single.trip.prop, na.rm = TRUE)) %>%
  distinct(start.adm4.code, end.adm4.code, .keep_all = TRUE)

adm4.trip.month.summary <- adm4.trip.month.avg[, c("start.adm1.name", "start.adm2.name", "start.adm3.name", "start.adm4.name", "start.adm1.code", "start.adm2.code", "start.adm3.code", "start.adm4.code", 
                                                   'end.adm1.name', 'end.adm2.name', 'end.adm3.name', 'end.adm4.name', 'end.adm1.code',"end.adm2.code", "end.adm3.code", "end.adm4.code",  
                                                   'pop.start', 'pop.end', 'urb.start', 'distance', 'distance.deg', 'urb.end', 'urb.cat.2', 'urb.cat.3', 'trip.type','trip.type9','in.out', 'in.out.Urb2', 'in.out.TT',
                                                   'adm4.all.trip.avg', 'adm4.all.trip.var', 'adm4.single.trip.avg', 'adm4.single.trip.var', 'adm4.single.trip.prop.avg', 'adm4.single.trip.prop.var')]

# save(adm4.trip.month.summary, file = "MDG_adm4_monthly_trips_details.RData")

## In vs out, stays included

# adm4.in.out.month <- adm4.trip.month %>%                                           # sum trips that are "IN" or "OUT" of adm1
#   group_by(start.adm4.code, in.out, m, y) %>%
#   mutate(adm4.trip.in.out.sum = sum(adm4.single.trip.sum)) 
# 
# adm4.in.out.month$adm4.in.out.prop <- adm4.in.out.month$adm4.trip.in.out.sum/adm4.in.out.month$adm4.all.trip.sum
# 
# adm4.in.out.month.avg <- adm4.in.out.month %>%
#   group_by(start.adm4.code, in.out) %>%
#   mutate(adm4.in.out.avg = mean(adm4.trip.in.out.sum)) %>%
#   mutate(adm4.in.out.var = var(adm4.trip.in.out.sum)) %>%
#   mutate(adm4.in.out.prop.avg = mean(adm4.in.out.prop)) %>%
#   mutate(adm4.in.out.prop.var = var(adm4.in.out.prop)) %>%
#   distinct(start.adm4.code, in.out, .keep_all = TRUE)
# 
# adm4.in.out.month.summary <- adm4.in.out.month.avg[, c("start.adm1.name", "start.adm2.name", "start.adm3.name", "start.adm4.name", "start.adm1.code", "start.adm2.code", "start.adm3.code","start.adm4.code",
#                                                        'end.adm1.name', 'end.adm2.name', 'end.adm3.name', 'end.adm4.name', 'end.adm1.code',"end.adm2.code", "end.adm3.code", "end.adm4.code",  
#                                                        'pop.start', 'pop.end', 'urb.start', 'urb.end',  'distance', 'distance.deg' ,'urb.end', 'urb.cat.2', 'urb.cat.3', 'trip.type','trip.type9','in.out', 'in.out.Urb2', 'in.out.TT', 
                                                       # 'adm3.in.out.avg', 'adm4.in.out.var', 'adm4.in.out.prop.avg', 'adm4.in.out.prop.var')]

# save(adm4.in.out.month.summary, file = "MDG_adm4_monthly_INvsOUT_trips_details.RData")

#9. Create categories for origins, urbanicity, distance, and trip frequency

adm4.trip.month.summary$urb.start.perc.col <- cut(adm4.trip.month.summary$urb.start,   # a way of binning subdistricts into districts for plotting
                                                  breaks = c(-Inf, 0.0009, 0.0029, Inf),#breaks = c(-Inf, 0.0003, 0.001660, Inf), #breaks = c(-Inf, exp(-10), exp(-2.5), Inf),
                                                  labels = c(1,2,3))
levels(adm4.trip.month.summary$urb.start.perc.col) <- c('Low', 'Medium', 'High')

adm4.trip.month.summary$urb.end.perc.col <- cut(adm4.trip.month.summary$urb.end,   # a way of binning subdistricts into districts for plotting
                                                breaks = c(-Inf, 0.0009, 0.0029, Inf),#breaks = c(-Inf, 0.0003, 0.001660, Inf),# breaks = c(-Inf, exp(-10), exp(-2.5), Inf),
                                                labels = c(1,2,3))
levels(adm4.trip.month.summary$urb.end.perc.col) <- c('Low', 'Medium', 'High')

adm4.trip.month.summary$distance.col <- cut(adm4.trip.month.summary$distance,
                                            breaks = c(-Inf, 50, 100, 250, 500, 750, 1000, Inf ),
                                            labels = c("0:50", "50:100", "100:250", "250:500", "500:750","750:1000", "> 1000"))
# 
adm4.trip.month.summary$trip.freq.col <- cut(adm4.trip.month.summary$adm4.single.trip.prop.avg,
                                             breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                             labels = c("< 0.001", "0.001 : 0.01", "0.01 : 0.1", "0.1 : 1"))


# save(adm4.trip.month.summary, file = "MDG_adm4_monthly_trips_details.RData")

#10. Repeat 8/9 for adm4 summary with NO STAYS included in calculations
trip.data.long.NS <- trip.data.long
trip.data.long.NS$trip.count.adj[trip.data.long.NS$start.adm4.code == trip.data.long.NS$end.adm4.code] = 0
trip.data.long.NS$trip.count[trip.data.long.NS$start.adm4.code == trip.data.long.NS$end.adm4.code] = 0

#11. adm4 level summary - calculate monthly average trip count (and proportion) and variance
adm4.trip.month_NS <- trip.data.long.NS %>%                                           # sum trips between origin/destination for each month/year
  group_by(start.adm4.code, end.adm4.code, m, y ) %>%
  mutate(adm4.single.trip.sum = sum(trip.count.adj, na.rm = TRUE)) %>%
  distinct(start.adm4.code, end.adm4.code, m, y, .keep_all=TRUE) 

adm4.trip.month_NS <- adm4.trip.month_NS %>%
  group_by(start.adm4.code, m, y) %>%
  mutate(adm4.all.trip.sum = sum(adm4.single.trip.sum)) 

adm4.trip.month_NS$adm4.single.trip.prop <- adm4.trip.month_NS$adm4.single.trip.sum/adm4.trip.month_NS$adm4.all.trip.sum

adm4.trip.month.avg_NS <- adm4.trip.month_NS %>%
  group_by(start.adm4.code) %>%
  mutate(adm4.all.trip.avg = as.integer(mean(adm4.all.trip.sum)))%>%
  mutate(adm4.all.trip.var = var(adm4.all.trip.sum))%>%
  mutate(adm4.all.trip.coeffVar = ifelse(adm4.all.trip.avg== 0, 0, as.integer(sd(adm4.all.trip.sum))/adm4.all.trip.avg))

adm4.trip.month.avg_NS <- adm4.trip.month.avg_NS %>%                                           # calculated average number of trips made each month between origin/destination
     group_by(start.adm4.code, end.adm4.code) %>%
     mutate(adm4.single.trip.avg = as.integer(mean(adm4.single.trip.sum))) %>%
     mutate(adm4.single.trip.var = var(adm4.single.trip.sum))%>%
     mutate(adm4.single.trip.coeffVar = ifelse(adm4.single.trip.avg == 0, 0, as.integer(sd(adm4.single.trip.sum))/adm4.single.trip.avg))%>%
     mutate(adm4.single.trip.prop.avg = mean(adm4.single.trip.prop)) %>%
     mutate(adm4.single.trip.prop.var = var(adm4.single.trip.prop)) %>%
     distinct(start.adm4.code, end.adm4.code, .keep_all = TRUE)



adm4.trip.month.summary_NoStays <- adm4.trip.month.avg_NS[, c("start.adm1.name", "start.adm2.name", "start.adm3.name", "start.adm4.name", "start.adm1.code", "start.adm2.code", "start.adm3.code", "start.adm4.code",
                                                              'end.adm1.name', 'end.adm2.name', 'end.adm3.name','end.adm4.name', 'end.adm1.code',"end.adm2.code", "end.adm3.code", "end.adm4.code", 
                                                              'pop.start', 'pop.end', 'urb.start', 'urb.end',  'distance', 'distance.deg' , 'urb.end', 'urb.cat.2', 'urb.cat.3', 'trip.type','trip.type9','in.out', 'in.out.Urb2', 'in.out.TT',
                                                              'adm4.all.trip.avg', 'adm4.all.trip.var', 'adm4.all.trip.coeffVar', 'adm4.single.trip.avg', 'adm4.single.trip.var', 'adm4.single.trip.coeffVar', 'adm4.single.trip.prop.avg', 'adm4.single.trip.prop.var')]

adm4.trip.month.summary_NoStays$urb.start.perc.col <- cut(adm4.trip.month.summary_NoStays$urb.start,   # a way of binning subdistricts into districts for plotting
                                                  breaks = c(-Inf, 0.0009, 0.0029, Inf),#breaks = c(-Inf, 0.0003, 0.001660, Inf), #breaks = c(-Inf, exp(-10), exp(-2.5), Inf),
                                                  labels = c(1,2,3))
levels(adm4.trip.month.summary_NoStays$urb.start.perc.col) <- c('Low', 'Medium', 'High')

adm4.trip.month.summary_NoStays$urb.end.perc.col <- cut(adm4.trip.month.summary_NoStays$urb.end,   # a way of binning subdistricts into districts for plotting
                                                breaks = c(-Inf, 0.0009, 0.0029, Inf),#breaks = c(-Inf, 0.0003, 0.001660, Inf),# breaks = c(-Inf, exp(-10), exp(-2.5), Inf),
                                                labels = c(1,2,3))
levels(adm4.trip.month.summary_NoStays$urb.end.perc.col) <- c('Low', 'Medium', 'High')

adm4.trip.month.summary_NoStays$distance.col <- cut(adm4.trip.month.summary_NoStays$distance,
                                            breaks = c(-Inf, 50, 100, 250, 500, 750, 1000, Inf ),
                                            labels = c("0:50", "50:100", "100:250", "250:500", "500:750","750:1000", "> 1000"))
# 
adm4.trip.month.summary_NoStays$trip.freq.col <- cut(adm4.trip.month.summary_NoStays$adm4.single.trip.prop.avg,
                                             breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                             labels = c("< 0.001", "0.001 : 0.01", "0.01 : 0.1", "0.1 : 1"))




# save(adm4.trip.month.summary_NoStays, file = "MDG_adm4_monthly_trips_details_NoStays.RData")


# ## No stays - in vs out
# # adm4.in.out.month_NS <- adm4.trip.month_NS %>%                                           # sum trips that are "IN" or "OUT" of adm1
# #   group_by(start.adm4.code, in.out, m, y) %>%
# #   mutate(adm4.trip.in.out.sum = sum(adm4.single.trip.sum))
# #
# # adm4.in.out.month_NS$adm4.in.out.prop <- adm4.in.out.month_NS$adm4.trip.in.out.sum/adm4.in.out.month_NS$adm4.all.trip.sum
# #
# # adm4.in.out.month.avg_NS <- adm4.in.out.month_NS %>%
# #   group_by(start.adm4.code, in.out) %>%
# #   mutate(adm4.in.out.avg = mean(adm4.trip.in.out.sum)) %>%
# #   mutate(adm4.in.out.var = var(adm4.trip.in.out.sum)) %>%
# #   mutate(adm4.in.out.prop.avg = mean(adm4.in.out.prop)) %>%
# #   mutate(adm4.in.out.prop.var = var(adm4.in.out.prop)) %>%
# #   distinct(start.adm4.code, in.out, .keep_all = TRUE)
# #
# # adm4.in.out.month.summary_NoStays <- adm4.in.out.month.avg[, c("start.adm1.name", "start.adm2.name", "start.adm3.name", "start.adm4.name", "start.adm1.code", "start.adm2.code", "start.adm3.code", "start.adm4.code",
# #                                                                'end.adm1.name', 'end.adm2.name', 'end.adm3.name', 'end.adm4.name', 'end.adm1.code',"end.adm2.code","end.adm3.code", "end.adm4.code",
# #                                                                'pop.start', 'pop.end', 'urb.start', 'urb.end',  'distance', 'distance.deg' ,'trip.type','trip.type9','in.out',
# #                                                                'adm4.in.out.avg', 'adm4.in.out.var', 'adm4.in.out.prop.avg', 'adm4.in.out.prop.var')]
# 
# # save(adm4.in.out.month.summary_NoStays, file = "MDG_adm4_monthly_INvsOUT_trips_details_NoStays.RData")

### trip matrices

trip.month.summary <- adm4.trip.month.avg[,c("start.adm4.code","end.adm4.code", "adm4.single.trip.avg")]
# save(trip.month.summary, file = "NAM_monthlytrips_counts_var_longform.RData")
M.monthly <- reshape::cast(trip.month.summary, start.adm4.code ~ end.adm4.code)                 # create wide-form matrix
rownames(M.monthly) <- M.monthly$start.adm4.code                           # label rows with district numbers
M.monthly <- M.monthly[ ,-1]
class(M.monthly) <- "data.frame"
M.monthly <- as.matrix(M.monthly)
names(dimnames(M.monthly)) <- c("origin", "destination")
M.monthly[is.na(M.monthly)] <- 0
M.monthly <- ceiling(M.monthly)
# save(M.monthly, file = "MDG_adm4_monthly_trips.RData")

M.monthly.no.stay <- M.monthly 
diag(M.monthly.no.stay) <- 0                                # set diagonol (stays) to 0
names(dimnames(M.monthly.no.stay)) <- c("origin", "destination")

communes <- colnames(M.monthly.no.stay)

# save(M.monthly.no.stay, file = "MDG_adm4_monthly_trips_nostay.RData")
# write.csv(M.monthly.no.stay, "NAM_monthly_trips_nostay.csv")
