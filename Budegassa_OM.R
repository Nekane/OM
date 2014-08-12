#----------------------------------------------------------
#OPERATING MODEL
#L.Budegassa (ank)

#Nekane Alzorriz
#February 2014



# Load the FLCore library
library(FLCore)
library(r4ss)
library(plyr)
library(reshape)
library(FLa4a)
library(devtools)
library(ggplotFL)

# Load the data
catnage<- read.table ("~/Documents/BoB/MSE/OM/data/Budegassa/Catch number.csv", header=T, dec=".", sep=";")
catwage<- read.table ("~/Documents/BoB/MSE/OM/data/Budegassa/Catch weight.csv", header=T, dec=".", sep=";")
matu<- read.table ("~/Documents/BoB/MSE/OM/data/Budegassa/Mat Ogive.csv", header=T, dec=".", sep=";")
ind<- read.table ("~/Documents/BoB/MSE/OM/data/Budegassa/Index abundance.csv", header=T, dec=".", sep=";")
#stkwage<- read.table ("~/Documents/BoB/MSE/OM/data/Budegassa/Stock weight.csv", header=T, dec=".", sep=";")
disc<-read.table ("~/Documents/BoB/MSE/OM/data/Budegassa/Total Landings.csv", header=T, dec=".", sep=";")
fishmort<- read.table ("~/Documents/BoB/MSE/OM/data/Budegassa/F.csv", header=T, dec=".", sep=";")
biomass<- read.table ("~/Documents/BoB/MSE/OM/data/Budegassa/Biomass.csv", header=T, dec=".", sep=";")
stknage<-read.table ("~/Documents/BoB/MSE/OM/data/Budegassa/Stock number.csv", header=T, dec=".", sep=";")

# Create the FLQuant object
ank.stk <- FLQuant( dimnames = list(age = 2:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),
                    quant = "age")

#We can now transform the FLQuant object into an FLStock object.
ank.stk <- FLStock(ank.stk)

#To see the elements of the object newly created you just have to type: # Name: ank.stk <- "ank.stkrim"
summary(ank.stk)


#Filling of slots with data
# Total catch
# Catch numbers at age
Age <-c(2:14)
Year <- as.numeric (sub("X", "", names(catnage[-c(1)])))
flq <- FLQuant( dimnames = list(age =2:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),
                quant = "age",  units = '10^3')
flq[as.character(Age),as.character(Year)] <- as.matrix(catnage[1:13,-c(1)])
catch.n (ank.stk)<-flq

# Catch mean weight at age
Year <- as.numeric (sub("X", "", names(catwage[-c(1)])))
flq<- FLQuant( dimnames = list(age = 2:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),
               quant = "age", units = 'kg')
flq[as.character(Age),as.character(Year)] <- as.matrix(catwage[1:13,-c(1)])
catch.wt(ank.stk)<-flq

# Total catches
#landings.n<- window(landings.n, start=1982, end=2012)
catch (ank.stk)<- apply((catch.n(ank.stk)*catch.wt(ank.stk)), 2, sum,na.rm=TRUE)


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
qq<-as.data.frame(catch(ank.stk))
flq[, as.character(qq$year),1]<-as.matrix(qq[,7])
#where unit 2 is for the one reported
aa<-as.data.frame(catch_report)
flq[, as.character(aa$year),2]<-as.matrix(aa[,7])
catch_tot <- flq


# Landings number at age
landings.n (ank.stk)<- NA
# Discards weight at age
landings.wt (ank.stk)<- NA
# Discards numbers at age
discards.n (ank.stk)<- NA
# Discards weight at age
discards.wt (ank.stk)<- NA

# Total landings
Year <- as.numeric (sub("X", "", names(disc[-c(1)])))
flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),
              quant = "age", units = 't')
flq[,as.character(Year)] <- as.matrix(disc[1,-c(1)])
landings (ank.stk)<- flq

# Total discards
discards (ank.stk)<-NA

#TAC



#----------------------------------------------------------
#Stock
#----------------------------------------------------------
# Stock number at age
stock.n (ank.stk)<- FLQuant( dimnames = list(age = 2:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),
                             quant = "age", units = '10^3')

# Stock weight at age, we assume the same weight as in the catch
stock.wt (ank.stk)<-catch.wt(ank.stk)

# Total stock
stock (ank.stk)<- FLQuant( dimnames = list(age = 2:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),
                           quant = "age") 

# Natural mortality rate: Natural mortality is 0.15
m (ank.stk)<- 0.15
units(m(ank.stk))<- 'm'

# Natural mortality rate before spawning: Natural mortality before spawning is 0
m.spwn (ank.stk)<- 0
units(m.spwn(ank.stk))<- 'NA'

# Maturity
Age <-c(1:14)
flq<- FLQuant( dimnames = list(age = 2:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),
               quant = "age")
flq[as.character(Age),as.character(Year)] <- as.matrix(matu[,-c(1)])
mat(ank.stk)<-flq


# Harvest rate
harvest (ank.stk)<- 0
# Harvest rate before spawning is 0 along the ages and years
harvest.spwn (ank.stk) <- 0
units(harvest.spwn(ank.stk))<- 'NA'

# Fully selected ages
range(ank.stk,'minfbar')<-6 
range(ank.stk,'maxfbar')<-10 

# Control if everything has been filled properly

# We can now check that all slots of the FLStock object have been fille:  summary(ank.stk)

#  To check that “stock” is properly initialised, we can do it like this: catch(ank.stk)                                                                                                                         

# The last step in the source code saves the FLStock object into an Rdata file
# which you can load when you start an R session using the “load” command.

save(ank.stk, catch_report, catch_tot,file="~/Documents/BoB/MSE/OM/data/Budegassa/ank.stock.RData")



# Create the FLQuant object to generate the FLIndex

#create a new FLIndices object that is a subset of our data,
#We can now create different FLIndex objects to put them in a single FLIndices.

#1. Index value
Age<-c(2:13)
Year <- as.numeric (sub("X", "", names(ind[-c(1)])))
flq <-FLQuant(dimnames = list(age = 0:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[1:12,-c(1)]))
idx<-flq
idx1<- FLIndex(index=idx, name='FR_FU04', desc='French bentic trawlers in the Celtic Sea LPUE numbers')
range(idx1,'startf')<-0
range(idx1,'endf')<-1
flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[,as.character(Year)] <- as.numeric(as.matrix(ind[13,-c(1)]))
effort(idx1)<-flq

#2. Index value
Age<-c(2:14)
flq <-FLQuant(dimnames = list(age = 0:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[17:29,-c(1)]))
idx<-flq
idx2<- FLIndex(index=idx, name='SP-VIGOTR7', desc='Vigo, LPUE numbers')
range(idx2,'startf')<-0
range(idx2,'endf')<-1
flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[,as.character(Year)] <- as.numeric(as.matrix(ind[30,-c(1)]))
effort(idx2)<-flq

#3. Index value
Age<-c(2:13)
flq <-FLQuant(dimnames = list(age = 0:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[34:45,-c(1)]))
idx<-flq
idx3<- FLIndex(index=idx, name='FR-FU14', desc='French LPUE numbers')
range(idx3,'startf')<-0
range(idx3,'endf')<-1
flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[,as.character(Year)] <- as.numeric(as.matrix(ind[46,-c(1)]))
effort(idx3)<-flq

#4. Index value
Age<-c(0:14)
Year <- as.numeric (sub("X", "", names(ind[13:21])))
flq <-FLQuant(dimnames = list(age = 0:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[51:65,2:10]))
idx<-flq
idx4<- FLIndex(index=idx, name='FR-EVHOE-S', desc='Survey index')
range(idx4,'startf')<-0.8
range(idx4,'endf')<-0.88

#5. Index value
Age<-c(2:14)
Year <- as.numeric (sub("X", "", names(ind[9:21])))
flq <-FLQuant(dimnames = list(age = 0:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[69:81,2:14]))
idx<-flq
idx5<- FLIndex(index=idx, name='SP-BAKON7', desc='LPUE numbers')
range(idx5,'startf')<-0
range(idx5,'endf')<-1
flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[,as.character(Year)] <- as.numeric(as.matrix(ind[82,2:14]))
effort(idx5)<-flq

#6. Index value
Age<-c(2:14)
Year <- as.numeric (sub("X", "", names(ind[9:21])))
flq <-FLQuant(dimnames = list(age = 0:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.matrix(ind[86:98,2:14])
idx<-flq
idx6<- FLIndex(index=idx, name='SP-BAKON8', desc='LPUE numbers')
range(idx6,'startf')<-0
range(idx6,'endf')<-1
flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[,as.character(Year)] <- as.numeric(as.matrix(ind[99,2:14]))
effort(idx6)<-flq

ank.idx<-FLIndices(ind1=idx1,ind2=idx2,ind3=idx3,ind4=idx4,ind5=idx5,ind6=idx6)

save(ank.stk,ank.idx,file="~/Documents/BoB/MSE/OM/data/Budegassa/ank.RData")

# In the following lines of the data frame the TOTAL LPUE and effort data are available from all fleets
# #7. Index value
# Age<-c(1:9)
# Year <- as.numeric (sub("X", "", names(ind[-c(1)])))
# flq <-FLQuant(dimnames = list(fleet = 1:9, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'),
#               units='thousands' )
# flq[as.character(Age),as.character(Year)] <- as.matrix(ind[102:110,-c(1)])
# idx<-flq
# idx7<- FLIndex(index=idx, name='FR-FU14', desc='')
# 
# #8. Index value
# Unit<-as.character(ind[113:121,1])
# Year <- as.numeric (sub("X", "", names(ind[-c(1)])))
# flq <-FLQuant(dimnames = list(age = 'all', year = 1986:2005, unit = 1:9, season = 'all', area = 'unique'), 
#               quant = "LPUE")
# flq[as.character(Year),as.character(Unit)] <- as.matrix(ind[113:121,2:21])
# idx<-flq
# idx8<- FLIndex(index=idx, name='FR-EVHOE-S', desc='Abundance index, effort in hours')


#----------------------------------------------------------
## STOCK ASSESSMENT OUTPUT FROM THE WORKING GROUP
#----------------------------------------------------------
#Fishing mortality
Age<-c(2:14)
Year <- as.numeric (sub("X", "", names(fishmort[2:21])))
flq <-FLQuant(dimnames = list(age = 0:14, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(fishmort[1:13,2:21]))
ank.SA_f<-flq

#BIOMASS
Param<-c('RECRUITSage2','TOTALBIO','TOTSPBIO','LANDINGS','YIELD/SSB', 'FBAR6-10')
Year <- as.numeric (sub("X", "", names(biomass[2:21])))
flq <-FLQuant(dimnames = list(Param, year = 1986:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Param),as.character(Year)] <- as.numeric(as.matrix(biomass[1:6,2:21]))
ank.SA_Biomass<-flq

#STOCK.N
Age<-c(2:14)
Year <- as.numeric (sub("X", "", names(stknage[2:22])))
flq <-FLQuant(dimnames = list(age = 0:14, year = 1986:2006, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(stknage[1:13,2:22]))
ank.SA_stock.n<-flq


save(ank.SA_f, ank.SA_Biomass, ank.SA_stock.n,file="~/Documents/BoB/MSE/OM/data/Budegassa/ank.SAoutput.RData")
