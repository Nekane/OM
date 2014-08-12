#----------------------------------------------------------
#OPERATING MODEL
#Western Horse Mackerel

#Nekane Alzorriz
#December 2013

# 

# Load the FLCore library
library(FLCore)
library(r4ss)
library(plyr)
library(reshape2)
library(FLa4a)
library(devtools)
library(ggplotFL)

# Load the data
catnage<- read.table ("~/Documents/BoB/MSE/OM/data/Horse Mackerel/Catch number.csv", header=T, dec=".", sep=";")
catwage<- read.table ("~/Documents/BoB/MSE/OM/data/Horse Mackerel/Catch weight.csv", header=T, dec=".", sep=";")
matu<- read.table ("~/Documents/BoB/MSE/OM/data/Horse Mackerel/Mat Ogive.csv", header=T, dec=".", sep=";")
ind<- read.table ("~/Documents/BoB/MSE/OM/data/Horse Mackerel/Index abundance.csv", header=T, dec=".", sep=";")
#indvar<- read.table ("~/Documents/BoB/MSE/OM/data/Horse Mackerel/Index variance.csv", header=T, dec=".", sep=";")
stkwage<- read.table ("~/Documents/BoB/MSE/OM/data/Horse Mackerel/Stock weight.csv", header=T, dec=".", sep=";")
disc<-read.table ("~/Documents/BoB/MSE/OM/data/Horse Mackerel/Total Landings.csv", header=T, dec=".", sep=";")
fishmort<- read.table ("~/Documents/BoB/MSE/OM/data/Horse Mackerel/F.csv", header=T, dec=".", sep=";")
biomass<- read.table ("~/Documents/BoB/MSE/OM/data/Horse Mackerel/Biomass.csv", header=T, dec=".", sep=";")
stknage<-read.table ("~/Documents/BoB/MSE/OM/data/Horse Mackerel/Stock number.csv", header=T, dec=".", sep=";")


# Create the FLQuant object
hom.stk <- FLQuant( dimnames = list(age = 0:11, year = 1982:2012, unit = 'unique', season = 'all', area = 'unique'),
                quant = "age")

#We can now transform the FLQuant object into an FLStock object.
hom.stk <- FLStock(hom.stk)

#To see the elements of the object newly created you just have to type: # Name: hom.stk <- "Horse Mackerel"
summary(hom.stk)




#Filling of slots with data
# Total catch
# Catch numbers at age
Age <-as.numeric (c(0:11))
Year <- as.numeric (sub("X", "", names(catnage[-c(1)])))
flq <- FLQuant( dimnames = list(age = 0:11, year = 1982:2012, unit = 'unique', season = 'all', area = 'unique'),
                quant = "age",  units = '10^3')
flq[as.character(catnage$Age),as.character(Year)] <- as.matrix(catnage[,-c(1)])
catch.n (hom.stk)<-flq

# Catch mean weight at age
# There is not data available in the stock, since we tried to gather it, I have used the 2012 mean weight for all years.
Year <- as.numeric (sub("X", "", names(catwage[-c(1)])))
flq<- FLQuant( dimnames = list(age = 0:11, year = 1982:2012, unit = 'unique', season = 'all', area = 'unique'),
               quant = "age", units = 'kg')
flq[as.character(Age),as.character(Year)] <- as.matrix(catwage[,-c(1)])
catch.wt(hom.stk)<-flq

# Total catches
#landings.n<- window(landings.n, start=1982, end=2012)
catch (hom.stk)<- apply((catch.n(hom.stk)*catch.wt(hom.stk)), 2, sum,na.rm=TRUE)

# Total catches as found in the report
Year <- as.numeric (sub("X", "", names(disc[-c(1)])))
flq <-FLQuant(dimnames = list(age = 'all', year = 1982:2012, unit = 'unique', season = 'all', area = 'unique'),
              quant = "age", units = 't')
flq[,as.character(Year)] <- as.matrix(disc[10,-c(1)])
catch_report <- flq

#Combined FLQuant with the reporting total catches and the total caches from the SA data computation, 
#where unit 1 is for the one computed
flq <-FLQuant(dimnames = list(age = 'all', year = 1982:2012, unit = c(1,2), season = 'all', area = 'unique'),
              quant = "age", units = 't')
qq<-as.data.frame(catch(hom.stk))
flq[, as.character(qq$year),1]<-as.matrix(qq[,7])
#where unit 2 is for the one reported
aa<-as.data.frame(catch_report)
flq[, as.character(aa$year),2]<-as.matrix(aa[,7])
catch_tot <- flq


# Total landings
Year <- as.numeric (sub("X", "", names(disc[-c(1)])))
flq <-FLQuant(dimnames = list(age = 'all', year = 1982:2012, unit = 'unique', season = 'all', area = 'unique'),
              quant = "age", units = 't')
flq[,as.character(Year)] <- as.matrix(disc[8,-c(1)])
landings (hom.stk)<- flq

# Total discards
Year <- as.numeric (sub("X", "", names(disc[-c(1)])))
flq <-FLQuant(dimnames = list(age = 'all', year = 1982:2012, unit = 'unique', season = 'all', area = 'unique'),
              quant = "age", units = 't')
flq[,as.character(Year)] <- as.matrix(disc[9,-c(1)])
discards (hom.stk)<-flq

#TAC
# Year <- as.numeric (sub("X", "", names(disc[-c(1)])))
# flq <-FLQuant(dimnames = list(age = 'all', year = 1984:2005, unit = 'unique', season = 'all', area = 'unique'),
#               quant = "age", units = 't')
# flq[,as.character(Year)] <- as.matrix(disc[4,-c(1)])
# TAC<-flq


#----------------------------------------------------------
#----------------------------------------------------------

#Stock

# Total stock
stock(hom.stk)<- FLQuant( dimnames = list(age = 'all', year = 1982:2012, unit = 'unique', season = 'all', area = 'unique'),
                        quant = "age", units = 't')

# Stock number at age (# STOCK ASSESSMENT OUTPUT FROM THE WORKING GROUP)
# tha stock.n is until 2013, I remove the last year
Age<-c(0:11)
Year <- as.numeric (sub("X", "", names(stknage[-c(1)])))
flq <-FLQuant(dimnames = list(age = 0:11, year = 1982:2013, unit = 'unique', season = 'all', area = 'unique'),
              quant = "age", units = '10^3')
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(stknage[,-c(1)]))
stock.n (hom.stk)<-flq[,ac(1982:2012)]

# Stock weight at age
Year <- as.numeric (sub("X", "", names(stkwage[-c(1)])))
flq<- FLQuant( dimnames = list(age = 0:11, year = 1982:2012, unit = 'unique', season = 'all', area = 'unique'),
               quant = "age", units = 'kilos')
flq[as.character(Age),as.character(Year)] <- as.matrix(stkwage[,-c(1)])
stock.wt (hom.stk)<-flq


#Natural mortality rate, natural mortality rate before spawning and maturity

#Natural mortality is 0.15
#Natural mortality before spawning is 0.45

# Natural mortality rate
m (hom.stk)<- 0.15
units(m(hom.stk))<- 'm'  

# Natural mortality rate before spawning
m.spwn (hom.stk)<- 0.45
units(m.spwn(hom.stk))<- 'NA'

# Maturity
Year <- as.numeric (sub("X", "", names(matu[-c(1)])))
flq<- FLQuant( dimnames = list(age = 0:11, year = 1982:2012, unit = 'unique', season = 'all', area = 'unique'),
               quant = "age")
flq[as.character(Age),as.character(Year)] <- as.matrix(matu[,-c(1)])
mat(hom.stk)<-flq


# Harvest rate and harvest rate before spawning 
# We have also assumed that we only have information about the harvest rate before spawning and we set harvest at any other time equal to 0.
# Information about harvest in the FLStock object will be used to calculate selectivity as described in (fbom.pdf) .
# If such information is not available but information on selectivity does exist an FLOgive object can still be created.

#Harvest before spawning is 0.45 along the ages and years

# Harvest rate (# STOCK ASSESSMENT OUTPUT FROM THE WORKING GROUP)
#Fishing mortality
Age<-c(0:11)
Year <- as.numeric (sub("X", "", names(fishmort[-c(1)])))
flq <-FLQuant(dimnames = list(age = 0:11, year = 1982:2012, unit = 'unique', season = 'all', area = 'unique'),
              quant="age", units="f")
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(fishmort[,-c(1)]))
harvest (hom.stk)<-flq

# Harvest rate before spawning
harvest.spwn (hom.stk) <- 0.45
units(harvest.spwn(hom.stk))<- 'NA'

# Fully selected ages
range(hom.stk,'minfbar')<-1 
range(hom.stk,'maxfbar')<-10 

# Control if everything has been filled properly
                                                                                                                       
# We can now check that all slots of the FLStock object have been fille:  summary(hom.stk)
                                                                                                                        
#  To check that “stock” is properly initialised, we can do it like this: catch(hom.stk)                                                                                                                         
                                                                                                            
# The last step in the source code saves the FLStock object into an Rdata file
# which you can load when you start an R session using the “load” command.
                                                                                                                        
save(hom.stk,catch_report, catch_tot,file="~/Documents/BoB/MSE/OM/data/Horse Mackerel/HOM.stock.RData")



# Stock composition from survey 2010 is not satisfactory, WG has excluded it (value= -1)

#Index value
ind[ind==-1]<-NA 
Year <- as.numeric (sub("X", "", names(ind[-c(1,11)])))
flq <-FLQuant(dimnames = list(age = 'all', year = 1983:2010, unit = 'unique', season = 'all', area = 'unique'), quant = "age")
flq[,as.character(Year)] <- as.numeric(as.matrix(ind[1,-c(1,11)]))
hom.idx<-FLIndex(index=flq)
units(index(hom.idx))<-'10^-12 eggs'
range(hom.idx,'startf')<-0.5 #
range(hom.idx,'endf')<-0.5 #

#Index variance
#flq[,as.character(Year)] <- as.matrix(indvar)
#index.var(hom.idx)<-flq

save(hom.stk,hom.idx,file="~/Documents/BoB/MSE/OM/data/Horse Mackerel/hom.RData")

#----------------------------------------------------------
## STOCK ASSESSMENT OUTPUT FROM THE WORKING GROUP
#----------------------------------------------------------
#Fishing mortality
Age<-c(0:11)
Year <- as.numeric (sub("X", "", names(fishmort[-c(1)])))
flq <-FLQuant(dimnames = list(age = 0:11, year = 1982:2012, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(fishmort[,-c(1)]))
HOM.SA_f<-flq

#BIOMASS
Param<-c('R (age 0)','SSB','TSB ','Catch','Yield/SSB', 'F(1-3)', 'F(4-8)', 'F(1-10)')
Year <- as.numeric (sub("X", "", names(biomass[-c(1)])))
flq <-FLQuant(dimnames = list(Param, year = 1982:2013, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Param),as.character(Year)] <- as.numeric(as.matrix(biomass[,-c(1)]))
HOM.SA_Biomass<-flq

#STOCK.N
Age<-c(0:11)
Year <- as.numeric (sub("X", "", names(stknage[-c(1)])))
flq <-FLQuant(dimnames = list(age = 0:11, year = 1982:2013, unit = 'unique', season = 'all', area = 'unique'))
flq[as.character(Age),as.character(Year)] <- as.numeric(as.matrix(stknage[,-c(1)]))
HOM.SA_stock.n<-flq

save(HOM.SA_f, HOM.SA_Biomass, HOM.SA_stock.n,file="~/Documents/BoB/MSE/OM/data/Horse Mackerel/HOM.SAoutput.RData")