##prepping data for gravity models
# Trip count matrix (M)
# population size (N)
# Distance matrix (D)
# Urbanicity (U)
# Trip type

# by Hannah Meredith
# last updated Jan 20, 2020

setwd("C:/Users/Hannah/Dropbox/Rural Travel/data/prep code for gm data/KEN")

library("geosphere")  # haverstine distance function
library("dplyr")       # joining
library("lubridate")  # parsing dates
library("reshape")    # for melting data
library("tidyr")

#1. Import trip data and save pop and urbanicity vectors
adm2.name<- read.csv('KEN_District_pop_coords_fid.csv')[c('fid','ADM2_NAME')] #  use different file because spelling of # 63 and 69 are different #details <- read.csv('KEN_adm2_coords_pop_urbanicity.csv')[c(1,5)]  
adm2.name$ADM2_NAME <- toupper(adm2.name$ADM2_NAME)
adm2.name <- adm2.name[order(adm2.name$fid),]

adm.details <- read.csv('KEN_adm2_coords_pop_urbanicity.csv')[c('fid','ADM1_NAME', 'X_coord', 'Y_coord')]#1,9,18,19)]
adm.details <- left_join(adm2.name, adm.details, by = "fid")

adm1.codes <- read.csv('KEN_adm1_coords.csv')[ , c('ID_1','NAME_1')]
adm.details <- left_join(adm.details, adm1.codes, by = c("ADM1_NAME" = "NAME_1"))
colnames(adm.details) <- c("adm2_ID", "adm2_name", "adm1_name", "X_coord", "Y_coord", "adm1_ID")
adm.details <- adm.details [ , c("adm1_ID", "adm1_name", "adm2_ID", "adm2_name", "X_coord", "Y_coord")]

#2. define  population, and urbanicity files
population <-  read.csv('KEN_adm2_coords_pop_urbanicity.csv')[c('fid','pop2010')]    # read.csv('KEN_District_pop_coords_fid.csv')[c(1,4)] #
population.ordered <- population[order(population$fid),]
urbanicity <- read.csv('KEN_adm2_coords_pop_urbanicity.csv')[c('fid','urbanicity')]
U.ordered <- urbanicity[order(urbanicity$fid),]
U.ordered$urb.cat.2 <- as.factor(ifelse(U.ordered$urbanicity <= 0.001,  ## updated since ran model on cluster Dec 5 2019
                            1,
                            2))
levels(U.ordered$urb.cat.2) <- c('Rural', 'Urban')

U.ordered$urb.cat.3 <- as.factor(cut(U.ordered$urbanicity,   # a way of binning subdistricts into districts for plotting
                                    breaks = c(-Inf, exp(-7.5), exp(-3.75), Inf),
                                    labels = c(1,2,3)))
levels(U.ordered$urb.cat.3) <- c('Low', 'Med', 'High')

#3.  create full distance matrix, including places that might be omitted due to no trips
coordinates <- adm.details[, c('adm2_ID', 'X_coord', 'Y_coord')] # read.csv('KEN_District_pop_coords_fid.csv')[c(1,5,6)] 
D <- matrix(NA, nrow=dim(coordinates)[1], ncol=dim(coordinates)[1])
D.deg <- matrix(NA, nrow=dim(coordinates)[1], ncol=dim(coordinates)[1])
trip.type <- matrix(NA, nrow=dim(coordinates)[1], ncol=dim(coordinates)[1])
trip.type.9 <- matrix(NA, nrow=dim(coordinates)[1], ncol=dim(coordinates)[1])
in.out <- matrix(NA, nrow=dim(coordinates)[1], ncol=dim(coordinates)[1])
in.out.urb2 <- matrix(NA, nrow=dim(coordinates)[1], ncol=dim(coordinates)[1])
in.out.TT <- matrix(NA, nrow=dim(coordinates)[1], ncol=dim(coordinates)[1])

for (i in 1:dim(coordinates)[1]){
     X_start <- coordinates$X_coord[coordinates$adm2_ID==i]
     Y_start <- coordinates$Y_coord[coordinates$adm2_ID==i]
     for (j in 1:dim(coordinates)[1]){
          X_end <- coordinates$X_coord[coordinates$adm2_ID==j]
          Y_end <- coordinates$Y_coord[coordinates$adm2_ID==j]
          
          D[i,j] <- distHaversine(matrix(c(X_start, Y_start), ncol =2),
                                  matrix(c(X_end, Y_end), ncol = 2))/1000
          
          D.deg[i,j] <- ifelse( i == j ,0, sqrt((X_end - X_start)^2 + (Y_end - Y_start)^2))
          
          
          trip.type[i,j]<- ifelse(U.ordered$fid[i] == U.ordered$fid[j], 0, # stay
                                  ifelse(U.ordered$urb.cat.2[i]==1 & U.ordered$urb.cat.2[j]==1, 1,   # R-R
                                  ifelse(U.ordered$urb.cat.2[i]==1 & U.ordered$urb.cat.2[j]==2, 2,  # R-U
                                         ifelse(U.ordered$urb.cat.2[i]==2 & U.ordered$urb.cat.2[j]==1, 3, # U-R
                                                4)))) # U-U
          
          trip.type.9[i,j]<- ifelse(U.ordered$fid[i] == U.ordered$fid[j], 0,  #stay
               ifelse(U.ordered$urb.cat.3[U.ordered$fid[i]] == 1 & U.ordered$urb.cat.3[U.ordered$fid[j]]==1, 1,  # L - L
               ifelse(U.ordered$urb.cat.3[U.ordered$fid[i]] == 1 & U.ordered$urb.cat.3[U.ordered$fid[j]] == 2, 2, #L - M
                    ifelse(U.ordered$urb.cat.3[U.ordered$fid[i]] == 1 & U.ordered$urb.cat.3[U.ordered$fid[j]]==3, 3, # L - H
                           ifelse(U.ordered$urb.cat.3[U.ordered$fid[i]] == 2 & U.ordered$urb.cat.3[U.ordered$fid[j]]==1, 4, # M-L
                                  ifelse(U.ordered$urb.cat.3[U.ordered$fid[i]] == 2 & U.ordered$urb.cat.3[U.ordered$fid[j]] == 2, 5, #M-M
                                         ifelse(U.ordered$urb.cat.3[U.ordered$fid[i]] == 2 & U.ordered$urb.cat.3[U.ordered$fid[j]]==3, 6, #M-H
                                                ifelse(U.ordered$urb.cat.3[U.ordered$fid[i]] == 3 & U.ordered$urb.cat.3[U.ordered$fid[j]]==1, 7, # H-L
                                                       ifelse(U.ordered$urb.cat.3[U.ordered$fid[i]] == 3 & U.ordered$urb.cat.3[U.ordered$fid[j]] == 2, 8, # H-M
                                                              9))))))))) #H-H
          
           in.out[i,j] <- ifelse(U.ordered$fid[i] == U.ordered$fid[j], 0,
                                 ifelse(adm.details$adm1_ID[i] == adm.details$adm1_ID[j], 1, 2 ))
           
           in.out.urb2[i,j] <- ifelse(U.ordered$fid[i] == U.ordered$fid[j], 0,
                                                ifelse(in.out[i,j] == 1 && U.ordered$urb.cat.2[i] == 1, 1,   # IN and rural origin
                                                       ifelse(in.out[i,j] == 1 && U.ordered$urb.cat.2[i] == 2, 2,  # IN and urb origin
                                                              ifelse(in.out[i,j] == 2 && U.ordered$urb.cat.2[i] == 1, 3,  # Out and rur origin
                                                                     4)))) # OUT and urb origin
           
           in.out.TT[i,j] <- ifelse(U.ordered$fid[i] == U.ordered$fid[j], 0,
                                              ifelse(in.out[i,j] == 1 && trip.type[i,j] == 1, 1,   # IN and urb-urb trip
                                                     ifelse(in.out[i,j] == 2 && trip.type[i,j] == 1, 2,  # OUT and urb-urb trip
                                                            ifelse(in.out[i,j] == 1 && trip.type[i,j] == 2, 3,   # IN and urb-urb trip
                                                                   ifelse(in.out[i,j] == 2 && trip.type[i,j] == 2, 4,  # OUT and urb-urb trip
                                                                          ifelse(in.out[i,j] == 1 && trip.type[i,j] == 3, 5,   # IN and urb-urb trip
                                                                                 ifelse(in.out[i,j] == 2 && trip.type[i,j] == 3, 6,  # OUT and urb-urb trip
                                                                                        ifelse(in.out[i,j] == 1 && trip.type[i,j] == 4, 7,   # IN and urb-urb trip
                                                                                               8))))))))  # OUT and urb-urb trip
           
          
     }
     
}

colnames(D) <- rownames(D) <- unique(adm.details$adm2_ID)#seq(1,69)
colnames(D.deg) <- rownames(D.deg) <- unique(adm.details$adm2_ID)#seq(1,69)
colnames(trip.type) <- rownames(trip.type) <- unique(adm.details$adm2_ID)#seq(1,69)
colnames(trip.type.9) <- rownames(trip.type.9) <- unique(adm.details$adm2_ID)#seq(1,69)
colnames(in.out) <- rownames(in.out) <- unique(adm.details$adm2_ID)#seq(1,69)
colnames(in.out.urb2) <- rownames(in.out.urb2) <- unique(adm.details$adm2_ID)#seq(1,69)
colnames(in.out.TT) <- rownames(in.out.TT) <- unique(adm.details$adm2_ID)#seq(1,69)

# levels(trip.type) <- c('stay', 'rural-rural', 'rural-urban', 'urban-rural','urban-urban')
# levels(trip.type.9) <- c('stay','low-low', 'low-mid', 'low-high', 'mid-low', 'mid-mid', 'mid-high', 'high-low', 'high-mid', 'high-high')
# levels(in.out) <- c('stay','IN', 'OUT')
# levels(in.out.urb2) <- c('stay', 'IN-rural.start', 'IN-urban.start', 'OUT-rural.start', 'OUT-urban.start')
# levels(in.out.TT) <- c('stay','IN-rural-rural', 'OUT-rural-rural', 'IN-rural-urban', 'OUT-rural-urban', 'IN-urban-rural', 'OUT-urban-rural','IN-urban-urban', 'OUT-urban-urban' )

# save(D, file = "KEN_distance_ordered.RData")
# save(D, file = "KEN_distanceDeg_ordered.RData")
# save(trip.type, file = "KEN_tripType_ordered.RData")
# save(trip.type.9, file = "KEN_tripType9_ordered.RData")
# save(in.out, file = "KEN_inOut.RData")
# save(in.out.urb2, file = "KEN_inOut_Urb2.RData")
# save(in.out.TT, file = "KEN_inOut_TT.RData")
# save(population.ordered, file = "KEN_pop2010_ordered.RData")
# save(U.ordered, file = "KEN_urb_ordered.RData")

# Create trip count matrix 
trip.data <- read.csv('KEN_entrances_per_day.csv', header = TRUE, check.names = FALSE)
trip.long <- melt(trip.data, id.vars = c("origin", "destination"))
colnames(trip.long) <- c("i", "j", "date", "trip.count")
trip.long <- merge(trip.long, adm.details, by.x = "j", by.y = "adm2_name")
trip.long <- merge(trip.long, adm.details, by.x = "i", by.y = "adm2_name")#[-c(1,2)]
colnames(trip.long) <- c("start.adm2.name", "end.adm2.name", "date", "trip.count", "end.adm1.code", "end.adm1.name", "end.adm2.code", "X_end", "Y_end", "start.adm1.code", "start.adm1.name", "start.adm2.code", "X_start", "Y_start")
trip.long <- trip.long [ , c("date", "start.adm1.name", "start.adm2.name","start.adm1.code","start.adm2.code","end.adm1.name", "end.adm2.name","end.adm1.code", "end.adm2.code","trip.count","X_end", "Y_end", "X_start", "Y_start")]

# rewrite dates
dates.sep <- strsplit(as.character(trip.long$date), "-", perl=TRUE)   # separate date string 
trip.long$d <- sapply(dates.sep, function(x) x[3])                    # define day of day
trip.long$m <- sapply(dates.sep, function(x) x[2])                    # month of date
trip.long$y <- sapply(dates.sep, function(x) x[1])                    # year of date

# import stay data and wrangle dates
stay.data <- read.csv('KEN_stays_per_day.csv', header = TRUE, check.names = FALSE)
stay.data$destination <- stay.data$origin
stay.long <- melt(stay.data, id.vars = c("origin", "destination"))
colnames(stay.long) <- c("i", "j", "date", "trip.count")
stay.long <- merge(stay.long, adm.details, by.x = "j", by.y = "adm2_name")
stay.long <- merge(stay.long, adm.details, by.x = "i", by.y = "adm2_name")#[-c(1,2)]
colnames(stay.long) <- c("date", "trip", "j", "i")
colnames(stay.long) <- c("start.adm2.name", "end.adm2.name", "date", "trip.count", "end.adm1.code", "end.adm1.name", "end.adm2.code", "X_end", "Y_end", "start.adm1.code", "start.adm1.name", "start.adm2.code", "X_start", "Y_start")
stay.long <- stay.long [ , c("date", "start.adm1.name", "start.adm2.name","start.adm1.code","start.adm2.code","end.adm1.name", "end.adm2.name","end.adm1.code", "end.adm2.code","trip.count","X_end", "Y_end", "X_start", "Y_start")]

dates.sep <- strsplit(as.character(stay.long$date), "/", perl=TRUE)   # separate date string 
stay.long$d <- sapply(dates.sep, function(x) x[2])                    # define day of day
stay.long$m <- sapply(dates.sep, function(x) x[1])                    # month of date
stay.long$y <- sapply(dates.sep, function(x) x[3])                    # year of date
stay.long$date <- as.Date(with(stay.long, paste(y, m, d,sep="-")), "%Y-%m-%d")

trip.data.long <- rbind.data.frame(stay.long, trip.long)


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

trip.data.long <- left_join(trip.data.long, population, by = c("start.adm2.code" = "fid"))
trip.data.long <- left_join(trip.data.long, population, by = c("end.adm2.code" = "fid"))
trip.data.long <- left_join(trip.data.long, U.ordered, by = c("start.adm2.code" = "fid"))
trip.data.long <- left_join(trip.data.long, U.ordered, by = c("end.adm2.code" = "fid"))
trip.data.long <- left_join(trip.data.long, distance.long, by = c('start.adm2.code'='i','end.adm2.code'='j')) 
trip.data.long <- left_join(trip.data.long, distance.deg.long, by = c('start.adm2.code'='i','end.adm2.code'='j')) 
trip.data.long <- left_join(trip.data.long, TT.long, by = c('start.adm2.code'='i','end.adm2.code'='j'))
trip.data.long <- left_join(trip.data.long, TT9.long, by = c('start.adm2.code'='i','end.adm2.code'='j'))
trip.data.long <- left_join(trip.data.long, in.out.long, by = c('start.adm2.code'='i','end.adm2.code'='j'))
trip.data.long <- left_join(trip.data.long, in.out.Urb2.long, by = c('start.adm2.code'='i','end.adm2.code'='j')) 
trip.data.long <- left_join(trip.data.long, in.out.TT.long, by = c('start.adm2.code'='i','end.adm2.code'='j')) 

colnames(trip.data.long) <- c('date', 'start.adm1.name', 'start.adm2.name', 'start.adm1.code','start.adm2.code', 
                              'end.adm1.name', 'end.adm2.name','end.adm1.code', 'end.adm2.code', 'trip.count', 'X_end', 'Y_end', 'X_start', 'Y_start', 'd','m','y',
                              'pop.start', 'pop.end', 'urb.start', 'urb.cat.2', 'urb.cat.3', 'urb.end', 'urb.end.cat.2','urb.end.cat.3',
                              'distance','distance.deg', 'trip.type', 'trip.type9', 'in.out', 'in.out.Urb2', 'in.out.TT')


#8. Adm2 level summary - calculate monthly average trip count (and proportion) and variance
adm2.trip.month <- trip.data.long %>%                                           # sum trips between origin/destination for each month/year
     group_by(start.adm2.code, end.adm2.code, m, y ) %>%
     mutate(adm2.single.trip.sum = sum(trip.count, na.rm = TRUE)) %>%
     distinct(start.adm2.code, end.adm2.code, m, y, .keep_all=TRUE) 

adm2.trip.month <- adm2.trip.month %>%                                           # sum trips from each origin for each month/year
     group_by(start.adm2.code, m, y) %>%
     mutate(adm2.all.trip.sum = sum(adm2.single.trip.sum, na.rm = TRUE)) 

adm2.trip.month$adm2.single.trip.prop <- adm2.trip.month$adm2.single.trip.sum/adm2.trip.month$adm2.all.trip.sum  # proportion of trips taken by i made up by a particular i->j 

adm2.trip.month.avg <- adm2.trip.month %>%
     group_by(start.adm2.code) %>%
     mutate(adm2.all.trip.avg = ceiling(mean(adm2.all.trip.sum, na.rm = TRUE)))%>%
     mutate(adm2.all.trip.var = var(adm2.all.trip.sum, na.rm = TRUE))

adm2.trip.month.avg <- adm2.trip.month.avg %>%                                           # calculated average number of trips made each month between origin/destination
     group_by(start.adm2.code, end.adm2.code) %>%
     mutate(adm2.single.trip.avg = ceiling(mean(adm2.single.trip.sum, na.rm = TRUE))) %>%
     mutate(adm2.single.trip.var = var(adm2.single.trip.sum, na.rm = TRUE)) %>%
     mutate(adm2.single.trip.prop.avg = mean(adm2.single.trip.prop, na.rm = TRUE)) %>%
     mutate(adm2.single.trip.prop.var = var(adm2.single.trip.prop, na.rm = TRUE)) %>%
     distinct(start.adm2.code, end.adm2.code, .keep_all = TRUE)

adm2.trip.month.summary <- adm2.trip.month.avg[, c("start.adm1.name", "start.adm2.name", "start.adm1.code", "start.adm2.code", 'X_start', 'Y_start', 
                                                   'end.adm1.name', 'end.adm2.name', 'end.adm1.code',"end.adm2.code", 'X_end', 'Y_end',  
                                                   'pop.start', 'pop.end', 'urb.start','urb.end', 'distance', 'distance.deg', 
                                                   'urb.cat.2', 'urb.cat.3', 'trip.type','trip.type9','in.out', 'in.out.Urb2', 'in.out.TT', 
                                                   'adm2.all.trip.avg', 'adm2.all.trip.var', 'adm2.single.trip.avg', 'adm2.single.trip.var', 
                                                   'adm2.single.trip.prop.avg', 'adm2.single.trip.prop.var')]

#9. Create categories for origins, urbanicity, distance, and trip frequency

adm2.trip.month.summary$distance.col <- cut(adm2.trip.month.summary$distance, 
                                            breaks = c(-Inf, 50, 100, 250, 500, 750, 1000, Inf ),
                                            labels = c("0:50", "50:100", "100:250", "250:500", "500:750","750:1000", "> 1000"))

adm2.trip.month.summary$trip.freq.col <- cut(adm2.trip.month.summary$adm2.single.trip.prop.avg,
                                             breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                             labels = c("< 0.001", "0.001 : 0.01", "0.01 : 0.1", "0.1 : 1"))

# save(adm2.trip.month.summary, file = "KEN_adm2_monthly_trips_details.RData")

## average trips for each month

adm2.single.monthly.trips <- adm2.trip.month %>%
     group_by(start.adm2.code, end.adm2.code, m) %>%
     mutate(adm2.monthly.trip.ave = mean(adm2.single.trip.sum, na.rm = TRUE)) %>%
     mutate(adm2.monthly.trip.sd = sd(adm2.single.trip.sum, na.rm = TRUE)) %>%
     mutate(adm2.monthly.trip.coeff.var = adm2.monthly.trip.sd/adm2.monthly.trip.ave) %>%
     distinct(start.adm2.code, end.adm2.code, m, .keep_all = TRUE)

# save(adm2.single.monthly.trips, file = "KEN_monthly_trips.RData")


#10. Repeat 8/9 for adm2 summary with NO STAYS included in calculations
trip.data.long.NS <- trip.data.long
trip.data.long.NS$trip.count[trip.data.long.NS$start.adm2.code == trip.data.long.NS$end.adm2.code] = 0


#11. Adm2 level summary - calculate monthly average trip count (and proportion) and variance
adm2.trip.month_NS <- trip.data.long.NS %>%                                           # sum trips between origin/destination for each month/year
     group_by(start.adm2.code, end.adm2.code, m, y ) %>%
     mutate(adm2.single.trip.sum = sum(trip.count, na.rm = TRUE)) %>%
     distinct(start.adm2.code, end.adm2.code, m, y, .keep_all=TRUE) 

adm2.trip.month_NS <- adm2.trip.month_NS %>%   # calculate all trips made from origin (regardless of destination) for each month/hear
     group_by(start.adm2.code, m, y) %>%
     mutate(adm2.all.trip.sum = sum(adm2.single.trip.sum)) 

adm2.trip.month_NS$adm2.single.trip.prop <- adm2.trip.month_NS$adm2.single.trip.sum/adm2.trip.month_NS$adm2.all.trip.sum

adm2.trip.month.avg_NS <- adm2.trip.month_NS %>%
     group_by(start.adm2.code) %>%
     mutate(adm2.all.trip.avg = as.integer(mean(adm2.all.trip.sum)))%>%
     mutate(adm2.all.trip.var = var(adm2.all.trip.sum)) %>%
     mutate(adm2.all.trip.coeffVar = ifelse(adm2.all.trip.avg== 0 ,0, as.integer(sd(adm2.all.trip.sum))/adm2.all.trip.avg))

adm2.trip.month.avg_NS <- adm2.trip.month.avg_NS %>%                                           # calculated average number of trips made each month between origin/destination
     group_by(start.adm2.code, end.adm2.code) %>%
     mutate(adm2.single.trip.avg = as.integer(mean(adm2.single.trip.sum))) %>%
     mutate(adm2.single.trip.var = var(adm2.single.trip.sum)) %>%
     mutate(adm2.single.trip.coeffVar = ifelse(adm2.single.trip.avg == 0 , 0, as.integer(sd(adm2.single.trip.sum))/adm2.single.trip.avg))%>%
     mutate(adm2.single.trip.prop.avg = ifelse(adm2.single.trip.avg == 0 , 0,mean(adm2.single.trip.prop))) %>%
     mutate(adm2.single.trip.prop.var = ifelse(adm2.single.trip.avg == 0 , 0,var(adm2.single.trip.prop))) %>%
     distinct(start.adm2.code, end.adm2.code, .keep_all = TRUE)

adm2.trip.month.summary_NoStays <- adm2.trip.month.avg_NS[, c("start.adm1.name", "start.adm2.name", "start.adm1.code", "start.adm2.code", 'X_start', 'Y_start',
                                                              'end.adm1.name', 'end.adm2.name', 'end.adm1.code', "end.adm2.code", 'X_end', 'Y_end', 
                                                              'pop.start', 'pop.end', 'urb.start', 'urb.end',  'distance', 'trip.type','trip.type9','in.out', 'in.out.Urb2', 'in.out.TT',
                                                              'adm2.all.trip.avg', 'adm2.all.trip.var', 'adm2.all.trip.coeffVar', 
                                                              'adm2.single.trip.avg', 'adm2.single.trip.var', 'adm2.single.trip.coeffVar',
                                                              'adm2.single.trip.prop.avg', 'adm2.single.trip.prop.var')]


adm2.trip.month.summary_NoStays$origin.breaks <- cut(adm2.trip.month.summary_NoStays$start.adm1.code,   # a way of binning subdistricts into districts for plotting
                                                     breaks = c(-Inf, 19, 44, 68, 90, Inf),
                                                     labels = c("1-19", "20-44", "45-69"))

adm2.trip.month.summary_NoStays$distance.col <- cut(adm2.trip.month.summary_NoStays$distance, 
                                                    breaks = c(-Inf, 50, 100, 250, 500, 750, 1000, Inf ),
                                                    labels = c("0:50", "50:100", "100:250", "250:500", "500:750","750:1000", "> 1000"))

adm2.trip.month.summary_NoStays$trip.freq.col <- cut(adm2.trip.month.summary_NoStays$adm2.single.trip.prop.avg,
                                                     breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                                     labels = c("< 0.001", "0.001 : 0.01", "0.01 : 0.1", "0.1 : 1"))


# save(adm2.trip.month.summary_NoStays, file = "KEN_adm2_monthly_trips_details_NoStays.RData")

### trip matrices

trip.month.summary <- adm2.trip.month.avg[,c('start.adm2.code', 'end.adm2.code', 'adm2.single.trip.avg')]
# save(trip.month.summary, file = "KEN_monthlytrips_counts_var_longform.RData")
M.monthly <- reshape::cast(trip.month.summary, start.adm2.code ~ end.adm2.code)                 # create wide-form matrix
rownames(M.monthly) <- M.monthly$start.adm2.code                           # label rows with district numbers
M.monthly <- M.monthly[ ,-1]
class(M.monthly) <- "data.frame"
M.monthly <- as.matrix(M.monthly)
names(dimnames(M.monthly)) <- c("origin", "destination")
M.monthly[is.na(M.monthly)] <- 0

# save(M.monthly, file = "KEN_monthly_trips.RData")

M.monthly.no.stay <- M.monthly 
diag(M.monthly.no.stay) <- 0                                     # set diagonol (stays) to 0
names(dimnames(M.monthly.no.stay)) <- c("origin", "destination")
M.monthly.no.stay[is.na(M.monthly.no.stay)] <- 0

# save(M.monthly.no.stay, file = "KEN_monthly_trips_nostay.RData")
# write.csv(M.monthly.no.stay, "KEN_monthly_trips_nostay.csv")


## Proportion of population that travel per month

adm2.monthly.movers <- adm2.trip.month[adm2.trip.month$start.adm2.code == adm2.trip.month$end.adm2.code, -c( 6:9, 11:12, 19, 23:32)]
adm2.monthly.movers <- adm2.monthly.movers[, c("start.adm1.name","start.adm2.name","start.adm1.code","start.adm2.code", "date", "d", "m", "y", "X_start", "Y_start", "pop.start", "urb.start", "urb.cat.2", "urb.cat.3", "adm2.single.trip.sum","adm2.all.trip.sum")]
colnames(adm2.monthly.movers) <- c("start.adm1.name", "start.adm2.name", "start.adm1.code", "start.adm2.code", "date", "d", "Month", "y", "X_start", "Y_start", "pop.start", "urb.start", "urb.cat.2", "urb.cat.3", "adm2.monthly.stays", "adm2.monthly.total")

adm2.monthly.movers$adm2.monthly.trips <- adm2.monthly.movers$adm2.monthly.total - adm2.monthly.movers$adm2.monthly.stays

adm2.monthly.movers.avg <- adm2.monthly.movers %>%
     group_by(start.adm2.code, Month) %>%
     mutate(adm2.stays.monthly.avg = mean(adm2.monthly.stays)) %>%
     mutate(adm2.trips.monthly.avg = mean(adm2.monthly.trips)) %>%
     mutate(adm2.total.monthly.avg = mean(adm2.monthly.total)) %>%
     mutate(adm2.stays.monthly.avg.prop = adm2.stays.monthly.avg/adm2.total.monthly.avg)%>%
     mutate(adm2.trips.monthly.avg.prop = adm2.trips.monthly.avg/adm2.total.monthly.avg)%>%
     mutate(season = ifelse(Month %in% (4:5), "Big rains",
                            ifelse(Month %in% (10:12), "Small rains", "dry")))%>%
     distinct(start.adm2.code, Month, .keep_all = TRUE)

adm2.monthly.movers.avg <- adm2.monthly.movers.avg[, c("start.adm1.name","start.adm2.name", "start.adm1.code", "start.adm2.code", "Month", "date", "season", "X_start", "Y_start", "pop.start", 
                                                       "urb.start", "urb.cat.2", "urb.cat.3", "adm2.stays.monthly.avg", "adm2.trips.monthly.avg", "adm2.total.monthly.avg",
                                                       "adm2.stays.monthly.avg.prop", "adm2.trips.monthly.avg.prop")]
adm2.monthly.movers.avg$urb.cat.2 <- as.factor(adm2.monthly.movers.avg$urb.cat.2); levels (adm2.monthly.movers.avg$urb.cat.2) <- c('Rural origin', 'Urban origin')
adm2.monthly.movers.avg$urb.cat.3 <- as.factor(adm2.monthly.movers.avg$urb.cat.3); levels (adm2.monthly.movers.avg$urb.cat.3) <- c('Low urban origin', 'Medium urban origin', 'High urban origin')
adm2.monthly.movers.avg$Month <- as.factor(adm2.monthly.movers.avg$Month)
adm2.monthly.movers.avg$Month <- factor(adm2.monthly.movers.avg$Month,
                                        levels = c("1", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
levels(adm2.monthly.movers.avg$Month) <- c("1"="Jan", "3" = "Mar", "4" = "Apr", "5"="May", "6"= "Jun", "7"= "Jul","8"= "Aug","9"= "Sep", "10"= "Oct", "11"="Nov", "12"="Dec")


# save(adm2.monthly.movers.avg, file = "KEN_monthly_mobile_props.RData")