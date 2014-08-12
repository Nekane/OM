#----------------------------------------------------------
#OPERATING MODEL
#L.Piscatorius (mon)

#Nekane Alzorriz
#February 2014



# Load the FLCore library
library(FLCore)
library(r4ss)
library(plyr)
library(reshape2)
library(FLa4a)
library(devtools)
library(ggplotFL)

# Load the data
catnage<- read.table ("~/Documents/BoB/MSE/OM/data/Piscatorious/Catch number.csv", header=T, dec=".", sep=";")
catwage<- read.table ("~/Documents/BoB/MSE/OM/data/Piscatorious/Catch weight.csv", header=T, dec=".", sep=";")
matu<- read.table ("~/Documents/BoB/MSE/OM/data/Piscatorious/Mat Ogive.csv", header=T, dec=".", sep=";")
ind<- read.table ("~/Documents/BoB/MSE/OM/data/Piscatorious/Index abundance.csv", header=T, dec=".", sep=";")
#stkwage<- read.table ("~/Documents/BoB/MSE/OM/data/Piscatorious/Stock weight.csv", header=T, dec=".", sep=";")
disc<-read.table ("~/Documents/BoB/MSE/OM/data/Piscatorious/Total Landings.csv", header=T, dec=".", sep=";")
fishmort<- read.table ("~/Documents/BoB/MSE/OM/data/Piscatorious/F.csv", header=T, dec=".", sep=";")
biomass<- read.table ("~/Documents/BoB/MSE/OM/data/Piscatorious/Biomass.csv", header=T, dec=".", sep=";")
stknage<-read.table ("~/Documents/BoB/MSE/OM/data/Piscatorious/Stock number.csv", header=T, dec=".", sep=";")

# Create the FLQuant object
mon.stk <- FLQuant( dimnames = list(age = 1:13, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),
                    quant = "age")

#We can now transform the FLQuant object into an FLStock object.
mon.stk <- FLStock(mon.stk)

#To see the elements of the object newly created you just have to type: # Name: mon.stk <- "mon.stkrim"
summary(mon.stk)


#Filling of slots with data
# Total catch
# Catch numbers at age
Age <-c(1:13)
Year <- as.numeric (sub("X", "", names(catnage[-c(1)])))
flq <- FLQuant( dimnames = list(age =1:13, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),units = '10^3')
flq[as.character(Age),as.character(Year)] <- as.matrix(catnage[1:13,-c(1)])
catch.n (mon.stk)<-flq

# Catch mean weight at age
Year <- as.numeric (sub("X", "", names(catwage[-c(1)])))
flq<- FLQuant( dimnames = list(age = 1:13, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'), units = 'kg')
flq[as.character(Age),as.character(Year)] <- as.matrix(catwage[1:13,-c(1)])
catch.wt(mon.stk)<-flq

# Total catches
#landings.n<- window(landings.n, start=1982, end=2012)
catch (mon.stk)<- apply((catch.n(mon.stk)*catch.wt(mon.stk)), 2, sum,na.rm=TRUE)


# Total catches as found in the report
Year <- as.numeric (sub("X", "", names(disc[-c(1)])))
flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),
              quant = "age", units = 't')
flq[,as.character(Year)] <- as.matrix(disc[1,-c(1)])
catch_report <- flq

#Combined FLQuant with the reporting total catches and the total caches from the SA data computation, 
#where unit 1 is for the one computed
flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = c(1,2), season = 'all', area = 'unique'),
              quant = "age", units = 't')
qq<-as.data.frame(catch(mon.stk))
flq[, as.character(qq$year),1]<-as.matrix(qq[,7])
#where unit 2 is for the one reported
aa<-as.data.frame(catch_report)
flq[, as.character(aa$year),2]<-as.matrix(aa[,7])
catch_tot <- flq


# Landings number at age
landings.n (mon.stk)<- NA
# Discards weight at age
landings.wt (mon.stk)<- NA
# Discards numbers at age
discards.n (mon.stk)<- NA
# Discards weight at age
discards.wt (mon.stk)<- NA

# Total landings
Year <- as.numeric (sub("X", "", names(disc[-c(1)])))
flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),
              quant = "age", units = 't')
flq[,as.character(Year)] <- as.matrix(disc[1,-c(1)])
landings (mon.stk)<- flq

# Total discards
discards (mon.stk)<-NA

#TAC



#----------------------------------------------------------
#Stock
#----------------------------------------------------------
# Stock number at age
stock.n (mon.stk)<- FLQuant( dimnames = list(age = 1:13, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),
                             units = '10^3')

# Stock weight at age, we assume the same weight as in the catch
stock.wt (mon.stk)<-catch.wt(mon.stk)

# Total stock
stock (mon.stk)<- FLQuant( dimnames = list(age = 1:13, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique')) 

# Natural mortality rate: Natural mortality is 0.15
m (mon.stk)<- 0.15
units(m(mon.stk))<- 'm'

# Natural mortality rate before spawning: Natural mortality before spawning is 0
m.spwn (mon.stk)<- 0
units(m.spwn(mon.stk))<- 'NA'

# Maturity
Age <-c(1:13)
flq<- FLQuant( dimnames = list(age = 1:13, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.matrix(matu[,-c(1)])
mat(mon.stk)<-flq


# Harvest rate
harvest (mon.stk)<- 0
# Harvest rate before spawning is 0 along the ages and years
harvest.spwn (mon.stk) <- 0
units(harvest.spwn(mon.stk))<- 'NA'

# Fully selected ages
range(mon.stk,'minfbar')<-3 
range(mon.stk,'maxfbar')<-8 

# Control if everything has been filled NAerly

# We can now check that all slots of the FLStock object have been fille:  summary(mon.stk)

#  To check that “stock” is NAerly initialised, we can do it like this: catch(mon.stk)                                                                                                                         

# The last step in the source code saves the FLStock object into an Rdata file
# which you can load when you start an R session using the “load” command.

save(mon.stk, catch_report, catch_tot,file="~/Documents/BoB/MSE/OM/data/Piscatorious/mon.stock.RData")



# Create the FLQuant object to generate the FLIndex

#create a new FLIndices object that is a subset of our data,
#We can now create different FLIndex objects to put them in a single FLIndices.

#1. Index value
Age<-c(0:7)
Year <- c(1997:2005)
flq <-FLQuant(dimnames = list(age = 0:13, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[47:54,2:10]))
idx<-flq
idx1<- FLIndex(index=idx, name='FR-EVHOE-S', desc='Survey index')
range(idx1,'startf')<-0.8
range(idx1,'endf')<-0.88


#2. Index value
Age<-c(5:12)
Year <- c(1987:2005)
flq <-FLQuant(dimnames = list(age = 0:13, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[22:29,2:20]))
idx<-flq
idx2<- FLIndex(index=idx, name='SP-VIGOTR7', desc='Vigo, LPUE numbers')
range(idx2,'startf')<-0
range(idx2,'endf')<-1
# flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
# flq[,as.character(Year)] <- as.numeric(as.matrix(ind[30,-c(1)]))
# effort(idx2)<-flq

#3. Index value
Age<-c(2:12)
Year <- c(1987:2005)
flq <-FLQuant(dimnames = list(age = 0:13, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[33:43,2:20]))
idx<-flq
idx3<- FLIndex(index=idx, name='SP-CORUTR7', desc='Coruna, LPUE numbers')
range(idx3,'startf')<-0
range(idx3,'endf')<-1
# flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
# flq[,as.character(Year)] <- as.numeric(as.matrix(ind[46,-c(1)]))
# effort(idx3)<-flq

#4. Index value
Age<-c(1:12)
Year <- c(1987:2005)
flq <-FLQuant(dimnames = list(age = 0:13, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[7:18,2:20]))
idx<-flq
idx4<- FLIndex(index=idx, name='FR_FU04', desc='French bentic trawlers in the Celtic Sea LPUE numbers')
range(idx4,'startf')<-0
range(idx4,'endf')<-1
# flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
# flq[,as.character(Year)] <- as.numeric(as.matrix(ind[13,-c(1)]))
# effort(idx1)<-flq

#5. Index value
Age<-c(3:9)
Year <- c(1993:2005)
flq <-FLQuant(dimnames = list(age = 0:13, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[58:64,2:14]))
idx<-flq
idx5<- FLIndex(index=idx, name='SP-BAKON8', desc='LPUE numbers')
range(idx5,'startf')<-0
range(idx5,'endf')<-1
# flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
# flq[,as.character(Year)] <- as.numeric(as.matrix(ind[82,2:14]))
# effort(idx5)<-flq


mon.idx<-FLIndices(ind1=idx1,ind2=idx2,ind3=idx3,ind4=idx4,ind5=idx5)

save(mon.stk,mon.idx,file="~/Documents/BoB/MSE/OM/data/Piscatorious/mon.RData")

# In the following lines of the data frame the TOTAL LPUE and effort data are available from all fleets



#----------------------------------------------------------
## STOCK ASSESSMENT OUTPUT FROM THE WORKING GROUP
#----------------------------------------------------------
#Fishing mortality
Age<-c(1:13)
Year <- as.numeric (sub("X", "", names(fishmort[2:21])))
flq <-FLQuant(dimnames = list(age = 0:13, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(fishmort[1:13,2:21]))
mon.SA_f<-flq

#BIOMASS
Param<-c('REC-age1','TOTALBIO','TOTSPBIO','LANDINGS','YIELD/SSB', 'FBAR3-8')
Year <- as.numeric (sub("X", "", names(biomass[2:21])))
flq <-FLQuant(dimnames = list(Param, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Param),as.character(Year)] <- as.numeric(as.matrix(biomass[1:6,2:21]))
mon.SA_Biomass<-flq

#STOCK.N
Age<-c(1:13)
Year <- as.numeric (sub("X", "", names(stknage[2:22])))
flq <-FLQuant(dimnames = list(age = 0:13, year = 1986:2006, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(stknage[1:13,2:22]))
mon.SA_stock.n<-flq


save(mon.SA_f, mon.SA_Biomass, mon.SA_stock.n,file="~/Documents/BoB/MSE/OM/data/Piscatorious/mon.SAoutput.RData")
