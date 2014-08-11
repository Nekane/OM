#----------------------------------------------------------
#OPERATING MODEL
#Megrim

#Nekane Alzorriz
#December 2013

#Megrim in the Celtic Sea, west of Ireland, and in the Bay of Biscay are caught predominantly
#by Spanish and French vessels, which together have reported more than 65% of the total
#landings, and by Irish and UK demersal trawlers.
# Spanish fleets catch megrim targeting them and in mixed fisheries for hake, anglerfish,
#Nephrops and others. Otter trawlers account for the majority of Spanish landings from
#Subarea VII, the remainder, very low quantities, being taken by netters prosecuting a mixed
#fishery for anglerfish, hake and megrim on the shelf edge around the 200 m contour to the
#south and west of Ireland. The Vigo catches comprise around 50% of the total catches.


# Load the FLCore library
library(FLCore)
library(r4ss)
library(plyr)
library(reshape)
library(FLa4a)
library(devtools)
library(ggplotFL)

# Load the data
catnage<- read.table ("~/Documents/BoB/MSE/OM/data/Megrim/Catch number.csv", header=T, dec=".", sep=";")
catwage<- read.table ("~/Documents/BoB/MSE/OM/data/Megrim/Catch weight.csv", header=T, dec=".", sep=";")
matu<- read.table ("~/Documents/BoB/MSE/OM/data/Megrim/Mat Ogive.csv", header=T, dec=".", sep=";")
stkwage<- read.table ("~/Documents/BoB/MSE/OM/data/Megrim/Stock weight.csv", header=T, dec=".", sep=";")
disc<-read.table ("~/Documents/BoB/MSE/OM/data/Megrim/Total Landings.csv", header=T, dec=".", sep=";")
ind<- read.table ("~/Documents/BoB/MSE/OM/data/Megrim/Index abundance.csv", header=T, dec=".", sep=";")
fishmort<- read.table ("~/Documents/BoB/MSE/OM/data/Megrim/F.csv", header=T, dec=".", sep=";")
biomass<- read.table ("~/Documents/BoB/MSE/OM/data/Megrim/Biomass.csv", header=T, dec=".", sep=";")
stknage<-read.table ("~/Documents/BoB/MSE/OM/data/Megrim/Stock number.csv", header=T, dec=".", sep=";")

# Create the FLQuant object
MEG.stk <- FLQuant( dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'))

#We can now transform the FLQuant object into an FLStock object.
MEG.stk <- FLStock(MEG.stk)

#To see the elements of the object newly created you just have to type: # Name: MEG.stk <- "MEG.stkrim"
summary(MEG.stk)


#Filling of slots with data
# Total catch
# Catch numbers at age
Age <-c(1:10)
Year <- as.numeric (sub("X", "", names(catnage[-c(1)])))
flq <- FLQuant( dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'), 
                quant="age",units = '10^3')
flq[as.character(Age),as.character(Year)] <- as.matrix(catnage[1:10,-c(1)])
catch.n (MEG.stk)<-flq

# Catch mean weight at age
Year <- as.numeric (sub("X", "", names(catwage[-c(1)])))
flq<- FLQuant( dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'), 
               quant="age",units = 'kg')
flq[as.character(Age),as.character(Year)] <- as.matrix(catwage[1:10,-c(1)])
catch.wt(MEG.stk)<-flq

# Total catches
#landings.n<- window(landings.n, start=1982, end=2012)
catch (MEG.stk)<- apply((catch.n(MEG.stk)*catch.wt(MEG.stk)), 2, sum,na.rm=TRUE)

# Total catches as found in the report
Year <- as.numeric (sub("X", "", names(disc[-c(1)])))
flq <-FLQuant(dimnames = list(age = 'all', year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'), units = 't')
flq[,as.character(Year)] <- as.matrix(disc[3,-c(1)])
catch_report <- flq

#Combined FLQuant with the reporting total catches and the total caches from the SA data computation, 
#where unit 1 is for the one computed
flq <-FLQuant(dimnames = list(age = 'all', year = 1984:2005, unit = c(1,2), season = 'all', area = 'unique'), units = 't')
qq<-as.data.frame(catch(MEG.stk))
flq[, as.character(qq$year),1]<-as.matrix(qq[,7])
#where unit 2 is for the one reported
aa<-as.data.frame(catch_report)
flq[, as.character(aa$year),2]<-as.matrix(aa[,7])
catch_tot <- flq

## Landings: I have found some disimilarities between landings data. Marina explained me, that converting the landing and discards length to age, 
#they are not considering the age 0, so some of this data has dissapeared.

# Landings number at age
landings.n (MEG.stk)<- NA
# Discards weight at age
landings.wt (MEG.stk)<- NA
# Discards numbers at age
discards.n (MEG.stk)<- NA
# Discards weight at age
discards.wt (MEG.stk)<- NA

# Total landings
Year <- as.numeric (sub("X", "", names(disc[-c(1)])))
flq <-FLQuant(dimnames = list(age = 'all', year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'), units = 't')
flq[,as.character(Year)] <- as.matrix(disc[1,-c(1)])
landings (MEG.stk)<- flq

# Total discards
Year <- as.numeric (sub("X", "", names(disc[-c(1)])))
flq <-FLQuant(dimnames = list(age = 'all', year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'), units = 't')
flq[,as.character(Year)] <- as.matrix(disc[2,-c(1)])
discards (MEG.stk)<-flq

#TAC
Year <- as.numeric (sub("X", "", names(disc[-c(1)])))
flq <-FLQuant(dimnames = list(age = 'all', year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'), units = 't')
flq[,as.character(Year)] <- as.matrix(disc[4,-c(1)])
TAC<-flq


#----------------------------------------------------------
#Stock
#----------------------------------------------------------
# Stock number at age
stock.n (MEG.stk)<- FLQuant( dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'),
                             units = '10^3')

# Stock weight at age
Year <- as.numeric (sub("X", "", names(stkwage[-c(1)])))
flq<- FLQuant( dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'), units = 'kg')
flq[as.character(Age),as.character(Year)] <- as.matrix(stkwage[1:10,-c(1)])
stock.wt (MEG.stk)<-flq

# Total stock
stock (MEG.stk)<- FLQuant( dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique')) 


# Natural mortality rate: Natural mortality is 0.2
m (MEG.stk)<- 0.2
units(m(MEG.stk))<- 'm'

# Natural mortality rate before spawning: Natural mortality before spawning is 0
m.spwn (MEG.stk)<- 0
units(m.spwn(MEG.stk))<- 'prop'

# Maturity
Year <- as.numeric (sub("X", "", names(matu[-c(1)])))
flq<- FLQuant( dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(matu$Age),as.character(Year)] <- as.matrix(matu[,-c(1)])
mat(MEG.stk)<-flq

# Harvest rate
harvest (MEG.stk)<- 0
# Harvest rate before spawning is 0 along the ages and years
harvest.spwn (MEG.stk) <- 0
units(harvest.spwn(MEG.stk))<- 'prop'

                                                                                                                        
save(MEG.stk, catch_report, catch_tot,TAC,file="~/Documents/BoB/MSE/OM/data/Megrim/MEG.stock.RData")



# Create the FLQuant object to generate the FLIndex

#create a new FLIndices object that is a subset of our data,
#We can now create different FLIndex objects to put them in a single FLIndices.
#1. Index value
Age<-c(1:9)
Year <- c(1986:2005)
flq <-FLQuant(dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'),units='10^3' )
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[36:44,2:21]))
idx<-flq
idx1<- FLIndex(index=idx, name='SP-CORUTR7', desc='Coruna trawlers')
range(idx1,'startf')<-0
range(idx1,'endf')<-1

#2. Index value
Age<-c(1:9)
Year <- c(1986:2005)
flq <-FLQuant(dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'),units='10^3' )
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[47:55,2:21]))
idx<-flq
idx2<- FLIndex(index=idx, name='SP-CANTAB7', desc='Cantabrico trawlers')
range(idx2,'startf')<-0
range(idx2,'endf')<-1

#3. Index value
Age<-c(1:9)
Year <- c(1984:2005)
flq <-FLQuant(dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'),units='10^3' )
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[58:66,2:23]))
idx<-flq
idx3<- FLIndex(index=idx, name='SP-VIGOTR7', desc='Cantabrico trawlers VII, efforts in days by 10000 hp')
range(idx3,'startf')<-0
range(idx3,'endf')<-1

#4. Index value
Age<-c(1:9)
Year <- c(1988:2001)
flq <-FLQuant(dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'),units='10^3' )
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[69:77,2:15]))
idx<-flq
idx4<- FLIndex(index=idx, name='FR-FU04', desc='French bentic trawlers in the Celtic Sea, effort in 1000h*1000kW')
range(idx4,'startf')<-0
range(idx4,'endf')<-1

#5. Index value
Age<-c(1:9)
Year <- c(1997:2005)
flq <-FLQuant(dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(ind[23:31,2:10]))
idx<-flq
idx5<- FLIndex(index=idx, name='FR-EVHOES', desc='Relative abundance index')
range(idx5,'startf')<-0.83
range(idx5,'endf')<-0.92

#6. Index value
Age<-c(1:9)
Year <- c(1987:2004)
flq <-FLQuant(dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'))
aa<-as.numeric(as.matrix(ind[1:9,2:19]))
aa[aa==0]<-NA
flq[as.character(Age),as.character(Year)] <- aa
idx<-flq
idx6<- FLIndex(index=idx, name='UK-WCGFS-D', desc='Abundance index, effort in hours')
range(idx6,'startf')<-0.15
range(idx6,'endf')<-0.25

#7. Index value
Age<-c(1:9)
Year <- c(1987:2004)
flq <-FLQuant(dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'))
aa<-as.numeric(as.matrix(ind[12:20,2:19]))
aa[aa==0]<-NA
flq[as.character(Age),as.character(Year)] <- aa
idx<-flq
idx7<- FLIndex(index=idx, name='UK-WCGFS-S', desc='Abundance index, effort in hours')
range(idx7,'startf')<-0.15
range(idx7,'endf')<-0.25

MEG.idx<-FLIndices(ind1=idx1,ind2=idx2,ind3=idx3,ind4=idx4,ind5=idx5,ind6=idx6,ind7=idx7)

save(MEG.idx,file="~/Documents/BoB/MSE/OM/data/Megrim/MEG.idx.RData")


#----------------------------------------------------------
## STOCK ASSESSMENT OUTPUT FROM THE WORKING GROUP
#----------------------------------------------------------
#Fishing mortality
Age<-c(1:10)
Year <- as.numeric (sub("X", "", names(fishmort[2:23])))
flq <-FLQuant(dimnames = list(age = 0:10, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(fishmort[1:10,2:23]))
MEG.SA_f<-flq

#BIOMASS
Param<-c('RECRUITSage2','TOTALBIO','TOTSPBIO','CATCHES','LANDINGS','DISCARDS','YIELD/SSB', 'FBAR3-6')
Year <- as.numeric (sub("X", "", names(biomass[2:23])))
flq <-FLQuant(dimnames = list(Param, year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Param),as.character(Year)] <- as.numeric(as.matrix(biomass[1:8,2:23]))
MEG.SA_Biomass<-flq

#STOCK.N
Age<-c(1:10)
Year <- as.numeric (sub("X", "", names(stknage[2:24])))
flq <-FLQuant(dimnames = list(age = 0:10, year = 1984:2006, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(stknage[1:10,2:24]))
MEG.SA_stock.n<-flq


save(MEG.SA_f, MEG.SA_Biomass, MEG.SA_stock.n,file="~/Documents/BoB/MSE/OM/data/Megrim/MEG.SAoutput.RData")
