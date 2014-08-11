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
library(reshape)
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
WHB.stk <- FLQuant( dimnames = list(age = 0:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
                quant = "age")

#We can now transform the FLQuant object into an FLStock object.
WHB.stk <- FLStock(WHB.stk)

#To see the elements of the object newly created you just have to type: # Name: WHB.stk <- "Blue Whiting"
summary(WHB.stk)




#Filling of slots with data

# Landings number at age
Age <-c(1:10)
Year <- as.numeric (sub("X", "", names(catnage[-c(1)])))
flq <- FLQuant( dimnames = list(age = 0:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
                quant = "age",  units = '10^6')
flq[as.character(Age),as.character(Year)] <- as.matrix(catnage[,-c(1)])
landings.n (WHB.stk)<-flq

# Landings mean weight at age
Year <- as.numeric (sub("X", "", names(catwage[-c(1)])))
flq<- FLQuant( dimnames = list(age = 0:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
               quant = "age", units = 'kg')
flq[as.character(Age),as.character(Year)] <- as.matrix(catwage[,-c(1)])
landings.wt(WHB.stk)<-flq

# Total landings
#landings.n<- window(landings.n, start=1982, end=2012)
landings_intermediate <- landings.n (WHB.stk)* landings.wt(WHB.stk) 
landings (WHB.stk)<- apply(landings_intermediate, 2, sum,na.rm=TRUE)
units(landings (WHB.stk)) <- 'kgtn'

# Total discards
discards (WHB.stk)<- FLQuant(0, dimnames = list(age = 'all', year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'), 
                          quant = "age", units = 'kgtn')
# Discards numbers at age
discards.n (WHB.stk)<- FLQuant(0, dimnames = list(age = 0:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
                            quant = "age", units = '10^6')
# Discards weight at age
discards.wt (WHB.stk)<- FLQuant(0, dimnames = list(age = 0:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
                             quant = "age", units = 'kg')

# Catch number at age
catch.n (WHB.stk)<-landings.n(WHB.stk) + discards.n(WHB.stk)
units(catch.n (WHB.stk)) <- '10^6'
# Catch weight at age, have to calculate the weighted average of the landings weight and discards weight.
catch.wt (WHB.stk)<- landings.wt (WHB.stk)
units(catch.wt (WHB.stk)) <- 'kg'
# Total catch
catch (WHB.stk)<- apply((catch.n(WHB.stk)*catch.wt(WHB.stk)), 2, sum,na.rm=TRUE)
units(catch (WHB.stk)) <- 'kgtn'



#Stock

# Total stock
stock(WHB.stk)<- FLQuant( dimnames = list(age = 'all', year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
                        quant = "age", units = 'kgtn')

# Stock number at age (# STOCK ASSESSMENT OUTPUT FROM THE WORKING GROUP)
Age<-c(1:10)
Year <- as.numeric (sub("X", "", names(stknage[-c(1)])))
flq <-FLQuant(dimnames = list(age = 0:10, year = 1981:2013, unit = 'unique', season = 'all', area = 'unique'),
              quant = "age", units = '10^6')
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(stknage[1:10,-c(1)]))
stock.n(WHB.stk)<-flq[,ac(1981:2012)]

# Stock weight at age
Year <- as.numeric (sub("X", "", names(stkwage[-c(1)])))
flq<- FLQuant( dimnames = list(age = 0:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
               quant = "age", units = 'kg')
flq[as.character(Age),as.character(Year)] <- as.matrix(stkwage[,-c(1)])
stock.wt (WHB.stk)<-flq
 

#Natural mortality rate, natural mortality rate before spawning and maturity

#Natural mortality is 0.2
#Natural mortality before spawning is 0

# Natural mortality rate
m (WHB.stk)<- FLQuant(0.2, dimnames = list(age = 0:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'), quant = "age")
# Natural mortality rate before spawning
m.spwn (WHB.stk)<- FLQuant(0,dimnames = list(age = 0:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'), quant = "age")
# Maturity
Year <- as.numeric (sub("X", "", names(matu[-c(1)])))
flq<- FLQuant( dimnames = list(age = 0:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'),
               quant = "age")
flq[as.character(matu$age),as.character(Year)] <- as.matrix(matu[,-c(1)])
mat(WHB.stk)<-flq


# Harvest rate and harvest rate before spawning 
# We have also assumed that we only have information about the harvest rate before spawning and we set harvest at any other time equal to 0.
# Information about harvest in the FLStock object will be used to calculate selectivity as described in (fbom.pdf) .
# If such information is not available but information on selectivity does exist an FLOgive object can still be created.

#Harvest before spawning is 0 along the ages and years

# Harvest rate (# STOCK ASSESSMENT OUTPUT FROM THE WORKING GROUP)
#Fishing mortality 
Age<-c(1:9)
Year <- as.numeric (sub("X", "", names(fishmort[-c(1)])))
flq <-FLQuant(dimnames = list(age = 0:10, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'), 
              quant = "age", units="f")
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(fishmort[1:9,-c(1)]))
harvest (WHB.stk)<-flq

# Harvest rate before spawning
harvest.spwn (WHB.stk) <- FLQuant(0, dimnames = list(age = 0:10, year = 1981:2012, unit = 'unique', season = 'all', 
                                                     area = 'unique'), quant = "age",units="prop")


# Control if everything has been filled properly
                                                                                                                       
# We can now check that all slots of the FLStock object have been fille:  summary(WHB.stk)
                                                                                                                        
#  To check that “stock” is properly initialised, we can do it like this: catch(WHB.stk)                                                                                                                         
                                                                                                            
# The last step in the source code saves the FLStock object into an Rdata file
# which you can load when you start an R session using the “load” command.
                                                                                                                        
 save(WHB.stk,file="~/Documents/BoB/MSE/OM/data/Blue Whiting/WHB.stock.RData")

whb.om<-WHB.stk
save(whb.om,file="~/Documents/BoB/MSE/OM/data/Blue Whiting/whb.om.RData")


# Create the FLQuant object
WHB.idx <- FLQuant( dimnames = list(age = 'all', year = 2004:2013, unit = 'unique', season = 'all', area = 'unique'),
                 quant = "biomass")

#We can now transform the FLQuant object into an FLIndex object.
WHB.idx <- FLIndex(WHB.idx)
summary(WHB.idx)


# Stock composition from survey 2010 is not satisfactory, WG has excluded it (value= -1)

#Index value
ind[ind==-1]<-NA 
Age<-c(3:8)
Year <- as.numeric (sub("X", "", names(ind[-c(1)])))
flq <-FLQuant(dimnames = list(age = 0:10, year = 2004:2013, unit = 'unique', season = 'all', area = 'unique'), quant = "biomass")
flq[as.character(Age),as.character(Year)] <- as.matrix(ind[,-c(1)])
index(WHB.idx)<-flq
#Index variance
#flq[,as.character(Year)] <- as.matrix(indvar)
#index.var(WHB.idx)<-flq

save(WHB.idx,file="~/Documents/BoB/MSE/OM/data/Blue Whiting/WHB.idx.RData")

#----------------------------------------------------------
## STOCK ASSESSMENT OUTPUT FROM THE WORKING GROUP
#----------------------------------------------------------
#Fishing mortality 
Age<-c(1:9)
Year <- as.numeric (sub("X", "", names(fishmort[-c(1)])))
flq <-FLQuant(dimnames = list(age = 0:9, year = 1981:2012, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(fishmort[1:9,-c(1)]))
WHB.SA_f<-flq

#BIOMASS
Param<-c('Recruits','RLow','RHigh','TSB ','TSBLow','TSBHigh','SSB ','SSBLow','SSBHigh', 'F(3-7)', 'F(3-7)Low', 'F(3-7)High')
Year <- as.numeric (sub("X", "", names(biomass[-c(1)])))
flq <-FLQuant(dimnames = list(Param, year = 1981:2013, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Param),as.character(Year)] <- as.numeric(as.matrix(biomass[1:12,-c(1)]))
WHB.SA_Biomass<-flq

#STOCK.N
Age<-c(1:10)
Year <- as.numeric (sub("X", "", names(stknage[-c(1)])))
flq <-FLQuant(dimnames = list(age = 0:10, year = 1981:2013, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(stknage[1:10,-c(1)]))
WHB.SA_stock.n<-flq

save(WHB.SA_f, WHB.SA_Biomass, WHB.SA_stock.n,file="~/Documents/BoB/MSE/OM/data/Blue Whiting/WHB.SAoutput.RData")