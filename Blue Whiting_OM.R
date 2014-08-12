#----------------------------------------------------------
#OPERATING MODEL
#Blue Whiting

#Nekane Alzorriz
#December 2013

# The Spanish blue whiting fishery is carried out mainly by bottom pair trawlers in a
# directed fishery (approx. one third of the fleet) and by single bottom otter trawlers in
# a by-catch fishery (approx. two thirds of the fleet). The fleet operates throughout the
# year. Small quantities are also caught by longliners. These coastal fisheries have trip
# durations of 1 or 2 days and catches are for human consumption. Thus, coastal land-
#   ings are driven mainly by market forces, and are rather stable. The fleet operates on-
#   ly in Spanish waters all year round and does not follow any blue whiting migration.
# The Spanish fleet has decreased from 279 vessels in the early 1990s to 135 vessels in
# 2008. After a period of decreasing trend, Spanish landings increased in 2011 to a total
# landing of 10 225 tonnes.

# Load the FLCore library
library(FLCore)
library(r4ss)
library(plyr)
library(reshape2)
library(FLa4a)
library(devtools)
library(ggplotFL)

# Load the data
catnage<- read.table ("~/Documents/BoB/MSE/OM/data/Blue Whiting/Catch number.csv", header=T, dec=".", sep=";")
catwage<- read.table ("~/Documents/BoB/MSE/OM/data/Blue Whiting/Catch weight.csv", header=T, dec=".", sep=";")
matu<- read.table ("~/Documents/BoB/MSE/OM/data/Blue Whiting/Mat Ogive.csv", header=T, dec=".", sep=";")
ind<- read.table ("~/Documents/BoB/MSE/OM/data/Blue Whiting/Index abundance.csv", header=T, dec=".", sep=";")
stkwage<- read.table ("~/Documents/BoB/MSE/OM/data/Blue Whiting/Stock weight.csv", header=T, dec=".", sep=";")
#indvar<- read.table ("~/Documents/BoB/MSE/OM/data/Blue Whiting/Index variance.csv", header=T, dec=".", sep=";")
disc<-read.table ("~/Documents/BoB/MSE/OM/data/Blue Whiting/Total Landings.csv", header=T, dec=".", sep=";")
fishmort<- read.table ("~/Documents/BoB/MSE/OM/data/Blue Whiting/F.csv", header=T, dec=".", sep=";")
biomass<- read.table ("~/Documents/BoB/MSE/OM/data/Blue Whiting/Biomass.csv", header=T, dec=".", sep=";")
stknage<-read.table ("~/Documents/BoB/MSE/OM/data/Blue Whiting/Stock number.csv", header=T, dec=".", sep=";")

# Create the FLQuant object
whb.stk <- FLQuant( dimnames = list(age = 1:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
                quant = "age")

#We can now transform the FLQuant object into an FLStock object.
whb.stk <- FLStock(whb.stk)

#To see the elements of the object newly created you just have to type: # Name: whb.stk <- "Blue Whiting"
summary(whb.stk)




#Filling of slots with data

# Landings number at age
Age <-c(1:10)
Year <- as.numeric (sub("X", "", names(catnage[-c(1)])))
flq <- FLQuant( dimnames = list(age = 1:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
                quant = "age",  units = '10^6')
flq[as.character(Age),as.character(Year)] <- as.matrix(catnage[,-c(1)])
landings.n (whb.stk)<-flq

# Landings mean weight at age
Year <- as.numeric (sub("X", "", names(catwage[-c(1)])))
flq<- FLQuant( dimnames = list(age = 1:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
               quant = "age", units = 'kg')
flq[as.character(Age),as.character(Year)] <- as.matrix(catwage[,-c(1)])
landings.wt(whb.stk)<-flq

# Total landings
#landings.n<- window(landings.n, start=1982, end=2012)
landings_intermediate <- landings.n (whb.stk)* landings.wt(whb.stk) 
landings (whb.stk)<- apply(landings_intermediate, 2, sum,na.rm=TRUE)
units(landings (whb.stk)) <- 'kgt'

# Total discards
discards (whb.stk)<- FLQuant(0, dimnames = list(age = 'all', year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'), 
                          quant = "age", units = 'kgt')
# Discards numbers at age
discards.n (whb.stk)<- FLQuant(0, dimnames = list(age = 1:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
                            quant = "age", units = '10^6')
# Discards weight at age
discards.wt (whb.stk)<- FLQuant(0, dimnames = list(age = 1:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
                             quant = "age", units = 'kg')

# Catch number at age
catch.n (whb.stk)<-landings.n(whb.stk) + discards.n(whb.stk)
units(catch.n (whb.stk)) <- '10^6'
# Catch weight at age, have to calculate the weighted average of the landings weight and discards weight.
catch.wt (whb.stk)<- landings.wt (whb.stk)
units(catch.wt (whb.stk)) <- 'kg'
# Total catch
catch (whb.stk)<- apply((catch.n(whb.stk)*catch.wt(whb.stk)), 2, sum,na.rm=TRUE)
units(catch (whb.stk)) <- 'kgt'



#Stock

# Total stock
stock(whb.stk)<- FLQuant( dimnames = list(age = 'all', year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
                        quant = "age", units = 'kgt')

# Stock number at age (# STOCK ASSESSMENT OUTPUT FROM THE WORKING GROUP)
Age<-c(1:10)
Year <- as.numeric (sub("X", "", names(stknage[-c(1)])))
flq <-FLQuant(dimnames = list(age = 1:10, year = 1981:2013, unit = 'unique', season = 'all', area = 'unique'),
              quant = "age", units = '10^6')
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(stknage[1:10,-c(1)]))
stock.n(whb.stk)<-flq[,ac(1981:2012)]

# Stock weight at age
Year <- as.numeric (sub("X", "", names(stkwage[-c(1)])))
flq<- FLQuant( dimnames = list(age = 1:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
               quant = "age", units = 'kg')
flq[as.character(Age),as.character(Year)] <- as.matrix(stkwage[,-c(1)])
stock.wt (whb.stk)<-flq
 

#Natural mortality rate, natural mortality rate before spawning and maturity

#Natural mortality is 0.2
#Natural mortality before spawning is 0

# Natural mortality rate
m (whb.stk)<- FLQuant(0.2, dimnames = list(age = 1:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'), quant = "age")
# Natural mortality rate before spawning
m.spwn (whb.stk)<- FLQuant(0,dimnames = list(age = 1:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'), quant = "age")
# Maturity
Year <- as.numeric (sub("X", "", names(matu[-c(1)])))
flq<- FLQuant( dimnames = list(age = 1:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
               quant = "age")
flq[as.character(matu$age)[2:11],as.character(Year)] <- as.matrix(matu[-c(1),-c(1)])
mat(whb.stk)<-flq


# Harvest rate and harvest rate before spawning 
# We have also assumed that we only have information about the harvest rate before spawning and we set harvest at any other time equal to 0.
# Information about harvest in the FLStock object will be used to calculate selectivity as described in (fbom.pdf) .
# If such information is not available but information on selectivity does exist an FLOgive object can still be created.

#Harvest before spawning is 0 along the ages and years

# Harvest rate (# STOCK ASSESSMENT OUTPUT FROM THE WORKING GROUP)
#Fishing mortality 
Age<-c(1:9)
Year <- as.numeric (sub("X", "", names(fishmort[-c(1)])))
flq <-FLQuant(dimnames = list(age = 1:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'), 
              quant = "age", units="f")
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(fishmort[1:9,-c(1)]))
harvest (whb.stk)<-flq

# Harvest rate before spawning
harvest.spwn (whb.stk) <- FLQuant(0, dimnames = list(age = 1:10, year = 1981:2012, unit = 'unique', season = 'all', 
                                                     area = 'unique'), quant = "age",units="NA")

# Fully selected ages
range(whb.stk,'minfbar')<-3 
range(whb.stk,'maxfbar')<-7 


# Control if everything has been filled properly
                                                                                                                       
# We can now check that all slots of the FLStock object have been fille:  summary(whb.stk)
                                                                                                                        
#  To check that “stock” is properly initialised, we can do it like this: catch(whb.stk)                                                                                                                         
                                                                                                            
# The last step in the source code saves the FLStock object into an Rdata file
# which you can load when you start an R session using the “load” command.
                                                                                                                        
 #save(whb.stk,file="~/Documents/BoB/MSE/OM/data/Blue Whiting/whb.stk.RData")




# Stock composition from survey 2010 is not satisfactory, WG has excluded it (value= -1)

#Index value, it is until 2013, so I remove the last year
ind[ind==-1]<-NA 
Age<-c(3:8)
Year <- as.numeric (sub("X", "", names(ind[-c(1,11)])))
flq <-FLQuant(dimnames = list(age = 3:8, year = 2004:2012, unit = 'unique', season = 'all', area = 'unique'), quant = "age")
flq[as.character(Age),as.character(Year)] <- as.matrix(ind[,-c(1,11)])
whb.idx<-FLIndex(index=flq)
range(whb.idx,'startf')<-0.25 #March
range(whb.idx,'endf')<-0.33 #April


save(whb.stk,whb.idx,file="~/Documents/BoB/MSE/OM/data/Blue Whiting/whb.RData")

#----------------------------------------------------------
## STOCK ASSESSMENT OUTPUT FROM THE WORKING GROUP
#----------------------------------------------------------
#Fishing mortality 
Age<-c(1:9)
Year <- as.numeric (sub("X", "", names(fishmort[-c(1)])))
flq <-FLQuant(dimnames = list(age = 1:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(fishmort[1:9,-c(1)]))
whb.SA_f<-flq

#BIOMASS
Param<-c('Recruits','RLow','RHigh','TSB ','TSBLow','TSBHigh','SSB ','SSBLow','SSBHigh', 'F(3-7)', 'F(3-7)Low', 'F(3-7)High')
Year <- as.numeric (sub("X", "", names(biomass[-c(1)])))
flq <-FLQuant(dimnames = list(Param, year = 1981:2013, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Param),as.character(Year)] <- as.numeric(as.matrix(biomass[1:12,-c(1)]))
whb.SA_Biomass<-flq

#STOCK.N
Age<-c(1:10)
Year <- as.numeric (sub("X", "", names(stknage[-c(1)])))
flq <-FLQuant(dimnames = list(age = 1:10, year = 1981:2013, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(stknage[1:10,-c(1)]))
whb.SA_stock.n<-flq

save(whb.SA_f, whb.SA_Biomass, whb.SA_stock.n,file="~/Documents/BoB/MSE/OM/data/Blue Whiting/whb.SAoutput.RData")