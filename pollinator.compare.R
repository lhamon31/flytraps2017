#load pollinator data
pollinatordat<-read.csv("C:/Users/lhamo/Documents/flytraps.fall.2017/data/pollinators.2017.csv")

names(pollinatordat) <- as.matrix(pollinatordat[3, ]) #names column headers
pollinatordat <- pollinatordat[-c(1:3), ] #removes first 3 useless rows

#make header titles r-friendly
colnames(pollinatordat) <- gsub(" ","",colnames(pollinatordat))
colnames(pollinatordat) <- gsub("#","",colnames(pollinatordat))

###############################################################################################
#load sampling effort data
effortdat<-read.csv("C:/Users/lhamo/Documents/flytraps.fall.2017/data/sampling.effort.2017.csv")

names(effortdat) <- as.matrix(effortdat[2, ]) #names column headers
effortdat <- effortdat[-c(1:2), ] #removes first 3 useless rows

#make header titles r-friendly
colnames(effortdat) <- gsub(" ","",colnames(effortdat))
colnames(effortdat) <- gsub("#","",colnames(effortdat))

#convert certain columns to numbers
effortdat$Minobservation <- as.numeric(as.character(effortdat$Minobservation))

#exclude sites not included in the HP experiment
effortdat<-effortdat[ which( ! effortdat$Site %in% "BSL 1") , ]
effortdat<-effortdat[ which( ! effortdat$Site %in% "Croatan Pringle Rd") , ]

#make variable names uniform
effortdat$Site<-gsub(" ", "", effortdat$Site)

#total sampling effort
sum(effortdat$Minobservation)

#sum sampling effort by site and time
aggregate(Minobservation ~ Site, effortdat, sum)
aggregate(effortdat$Minobservation, list(effortdat$Site,effortdat$Date), sum )

