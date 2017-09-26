#load seedcount data
dat<-read.csv("C:/Users/lhamo/Documents/flytraps.fall.2017/data/seedcount.2017.csv")
names(dat) <- as.matrix(dat[2, ]) #names column headers
dat <- dat[-c(1:2), ] #removes first 2 useless rows

#make header titles r-friendly
colnames(dat) <- gsub(" ","",colnames(dat))
colnames(dat) <- gsub("#","",colnames(dat))
colnames(dat) <- gsub("/","",colnames(dat))

#convert data type
dat$blackseeds<-as.numeric(as.character(dat$blackseeds))
dat$`bulkweightblackseeds(mg)`<-as.numeric(as.character(dat$`bulkweightblackseeds(mg)`))
dat$plant<-as.numeric(as.character(dat$plant))

##insert fire regime column
  firelabels <- data.frame(c1 = c("HS1","HS12","HS15","HS16","HS2","HS3","HS6","HS9"), 
                           c2 = c("02/03/2016","03/29/2017","03/10/2014","06/20/2016","03/10/2014","05/13/2013","06/22/2016","06/22/2016"))
  dat$fire <- firelabels$c2[match(dat$site,firelabels$c1)]
  #convert fire date column to date
  dat$fire <- as.Date(dat$fire, "%m/%d/%Y")

##merge flower/bud/trap number and scape height info 
  plantdat<-read.csv("C:/Users/lhamo/Documents/flytraps.fall.2017/data/handpollination.2017.csv")
  #remove unncecessary columns for ease
  plantdat <- plantdat[c(4:6,8:9)]
  colnames(plantdat)<-c("plant","flowers","buds","stalkheightcm","traps")
  #combine flower/bud data
  plantdat$flowerbud <- plantdat$flowers + plantdat$buds
  dat<-merge(dat, plantdat, by.x=c('plant'), by.y=c('plant'), all.x = T, all.y = T)
  #note that merging this adds observations for 124 and 126 because these don't exist in the seedset dat
  
##create avg seed weight (over whole plant) column
  aggdat <- aggregate(`bulkweightblackseeds(mg)`~plant,dat,sum)
  aggdat2<-aggregate(blackseeds~plant,dat,sum)
  aggdat3<-merge(aggdat, aggdat2, by.x=c('plant'), by.y=c('plant'), all.x = T, all.y = T)
  aggdat3$perseedweight<-aggdat3$`bulkweightblackseeds(mg)`/aggdat3$blackseeds
  aggdat3<-aggdat3[c(1,4)]
  dat<-merge(dat, aggdat3, by.x=c('plant'), by.y=c('plant'), all.x = T, all.y = T)

#make sure grouping factors are uniform
  dat$treatment <- gsub("control ","control", dat$treatment)

#remove one where there wasn't even a capsule to be counted
  dat<-dat[ ! ( dat$plant == "62") , ] 
#omit the NAs that emerged from the merge
  dat<-dat[ ! ( dat$plant == "124.0") , ] 
  dat<-dat[ ! ( dat$plant == "126.0") , ] 
  
#########################################################################################################
#simple seed count statistics
summary(dat$blackseeds, na.rm=TRUE)

#mean = 20.16
  #sd=8.74
#median = 21
#1st-3rd Quartile = 14-26
#range = 0-44

##########################################################################################################
#comparing seed set between hand-pollinated and control flowers
#remove unncecessary columns for ease
HPdat <- dat[c(1,4,7)]

#sort data into t-test format
HPdat <- HPdat[order(HPdat$treatment, HPdat$plant), ]

#perform a paired t-test
t.test(formula = blackseeds ~ treatment,
       data=HPdat, 
       paired=TRUE, 
       id = HPdat$plant, 
       conf.level=0.95 )

#interpret results
#p-value = 0.03369 Reject null: difference in the means =/= 0
#t = -2.1425
#df=158
#mean of differences = -1.735849

#create a boxplot 
library(ggplot2)
ggplot(HPdat, aes(x = treatment, y = blackseeds)) +
  geom_boxplot(colour = "black") + 
  xlab("Treatment") +
  ylab("Seed Set")+
  scale_x_discrete(labels=c("control" = "control", "HP" = "hand-pollinated")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
#not a super impressive looking boxplot

#summary statistics each treatment
HP <- HPdat[ which(HPdat$treatment=='HP'), ]
summary(HP$blackseeds)
#mean = 21.03, sd=8.201128
#1st-3rd Quartile = 16-27
#range = 0-44

control <- HPdat[ which(HPdat$treatment=='control'), ]
summary(control$blackseeds)
#mean = 19.36, sd=9.184169
#1st-3rd Quartile = 13-25.5
#range = 0-41
#test change 3

########################################################################################################
#examine seedcount by fire regime
dat <- dat[ which(dat$treatment=='control'), ]
firedat<-dat

#get date to display chronologically in results
firedat$fire <- factor(firedat$fire, ordered = T)

#does seed count differ by fire regime?
fire.mod <- lm(perseedweight ~ fire, data = firedat)
summary(fire.mod)
anova(fire.mod)
#by fire: F(5,153)=1.198, p=0.31
#by site: F(7,151)=2.493, p=0.018)

#create a boxplot 
library(ggplot2)

ggplot(firedat, aes(x = fire, y = perseedweight)) +
  geom_boxplot(colour = "black") + 
  xlab("Date of Last Fire") +
  ylab("Average Seed Weight (mg)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#interpret results
#combined: fire no, site no
#control: fire no, site yes

#########################################################################################################
#a bunch of different linear models
#########################################################################################################
#question: should i use just control, or combo HP+control for subsequent analyses?
  #do seed number and seed weight have anything 2 do w/ each other
  mod <- lm(perseedweight ~ blackseeds, data = dat)
  summary(mod)
  plot(perseedweight~blackseeds,data=dat)
  #I guess not

#subsequent analyses done only w/ control flowers for now because it's a little cleaner
dat <- dat[ which(dat$treatment=='control'), ]

#note that if x is numerical, 'lm' runs a linear regression
#if x is categorical, 'lm' runs an ANOVA

#### seed set ####      
#does seed set vary by scape height?
  mod <- lm(blackseeds ~ stalkheightcm, data = dat)
  summary(mod)
  plot(blackseeds~stalkheightcm, data=dat)
    #combined: no
    #control: no F(1,155)=0.0849, p=0.77)
    #HP: no
#does seed set vary by number of traps? 
  mod <- lm(blackseeds ~ traps, data = dat)
  summary(mod)
  plot(blackseeds~traps, data=dat)
    #combined: no
    #control: no (F(1,155)=2.041, p=0.16)
    #HP: no
#does seed set vary by number of flowers + buds?
  mod <- lm(blackseeds ~ flowerbud, data = dat)
  summary(mod)
  plot(blackseeds~flowerbud, data=dat)
    #combined: yes
    #control: eh (F(1,155, p=0.049))
    #HP: yes

#### avg seed weight ####
#does avg seed weight vary by site
  mod <- lm(perseedweight ~ site, data = dat)
  summary(mod)
  boxplot(perseedweight~site, data=dat)
    #combined: yes
    #control: yes (F(7,150)=63.28, p<0.0001)
    #HP: yes
#does avg seed weight vary by fire regime?
  mod <- lm(perseedweight ~ fire, data = dat)
  summary(mod)
  boxplot(perseedweight~fire, data=dat)
    #combined: yes
    #control: yes (F(1,156)=7.231, p=0.01)
    #HP: yes
#does avg seed weight vary by trap number?
  mod <- lm(perseedweight ~ traps, data = dat)
  summary(mod)
  plot(perseedweight~traps, data=dat)
    #combined: yes
    #control:yes (F(1,154)=12.91, p<0.001)
    #yes

#### scape height ####
#does scape height vary by site?
  mod <- lm(stalkheightcm ~ site, data = dat)
  summary(mod)
  boxplot(stalkheightcm~site, data=dat)
    #combined: yes
    #control: yes (F(7,149)=14.58, p<0.0001)
    #HP: yes
#does scape height vary by fire regime?
  mod <- lm(stalkheightcm ~ fire, data = dat)
  summary(mod)
  boxplot(stalkheightcm~fire, data=dat)
    #combined: yes
    #control: no (F(1,155)=2.319, p=0.13)
    #HP: no
#does scape height vary by trap number?
  mod <- lm(stalkheightcm ~ traps, data = dat)
  summary(mod)
  plot(stalkheightcm~traps, data=dat)
    #combined: yes
    #control:yes (F(1,155)=19.13, p<0.0001)
    #HP: yes
  
#### trap number ####
#does trap number vary by site?
  mod <- lm(traps ~ site, data = dat)
  summary(mod)
  boxplot(traps~site, data=dat)
    #combined: yes
    #control:yes (F(7,149)=7.972, p<0.0001)
    #HP: yes
#does trap number vary by fire regime?
  mod <- lm(traps ~ fire, data = dat)
  summary(mod)
  boxplot(traps~fire, data=dat)
    #combined: yes
    #control:yes (F(1,155)=25.47, p<0.0001)
    #HP: yes
  
#### flower + bud number ####
#does flower + bud number vary by site?
  mod <- lm(flowerbud ~ site, data = dat)
  summary(mod)
  boxplot(flowerbud~site, data=dat)
    #combined: yes
    #control: yes  (F(7,149)=5.917,p<0.0001)
    #HP: yes
#does flower + bud number vary by fire regime?
  mod <- lm(flowerbud ~ fire, data = dat)
  summary(mod)
  boxplot(flowerbud~fire, data=dat)
    #combined: no
    #control: no (F(1,155)=1.51, p=0.22)
    #HP: no
#does flower + bud number vary by trap number?
  mod <- lm(flowerbud ~ traps, data = dat)
  summary(mod)
  plot(flowerbud~traps, data=dat)
  abline(lm(dat$flowerbud~dat$traps))
    #combined: yes
    #control:no (F(1,155)=2.082,p=0.15)
    #HP: no

#make a scatter plot
ggplot(scapedat, aes(x=stalkheightcm, y=blackseeds))+
      geom_point(shape=1)

  ggplot(dat, aes(x = fire, y = perseedweight)) +
    geom_boxplot(colour = "black") + 
    xlab("site") +
    ylab("seedweight")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  

#where's 93, 124.2, 