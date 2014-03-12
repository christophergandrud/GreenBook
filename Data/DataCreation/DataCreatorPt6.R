############
# Clean Federal Reserve board members data and determine percent appointed by Dems.
# Christopher Gandrud
# 12 March 2012
############


# Load packages
library(DataCombine)
library(lubridate)
library(stringr)
library(foreign)
library(dplyr)

#### About raw data and load ####
# Basic data from http://www.federalreserve.gov/aboutthefed/bios/board/boardmembership.htm
# Table scraped using Komonolabs (https://www.kimonolabs.com/)
MembRaw <- read.csv('~/Dropbox/GreenBook/Base_Data/FedMembers.csv', 
                    stringsAsFactors = FALSE, na.strings = c('NA', ''))

#### Clean members data ####
# Standardise term start
ReplaceDF1 <- data.frame(from = c('Sept'), to = c('Sep'))
MembRaw <- FindReplace(MembRaw, Var = 'DateStart', 
                       replaceData = ReplaceDF1, exact = FALSE)
MembRaw$Start <- mdy(MembRaw$DateStart)

# Create seperate columns for each term
Split1 <- data.frame(str_split_fixed(MembRaw$ReappointEnd, pattern = '\\. [A-Z]', n = 2))
Split1$ID <- 1:nrow(Split1)
Split1$X2[Split1$X2 == ''] <- NA
Split2 <- data.frame(str_split_fixed(MembRaw$ReappointEnd, pattern = ';', n = 2))
Split2$ID <- 1:nrow(Split2)
Split2$X2[Split2$X2 == ''] <- NA
Split2 <- DropNA(Split2, 'X2')
Split2 <- Split2[, c('ID', 'X2')]

SplitComb <- FillIn(Split1, Split2, Var1 = 'X2', Var2 = 'X2', allow.cartesian = TRUE, KeyVar = 'ID')

# Clean up to get to spell quarter
SplitComb$X1 <- gsub(';.*', '', SplitComb$X1)
SplitComb$X2 <- gsub('eappointed in 1934 from the Richmond District.\n', '', SplitComb$X2)

Comb <- cbind(MembRaw, SplitComb[, -1])

# Clean up to get to dates
Comb$X1 <- gsub('Reappointed in.*', NA, Comb$X1)
Comb$X1 <- gsub('Term began.* ', NA, Comb$X1)
ReplaceDF2 <- data.frame(from = c('Term expired ', 'Resigned ', 'Died ', 'Served until ', 'Retired ',
                                      'Served through ', 'September', 'Sept'), 
                             to = c('', '', '', '', '', '', 'Sep', 'Sep'))
Comb <- FindReplace(Comb, Var = 'X1', replaceData = ReplaceDF2, exact = FALSE)

ReplaceDF3 <- data.frame(from = c('erved until ', 'Served until ', 'esigned ', 'ied ', 'erm expired ', 'term expired ',
                                  ' term expired ', ' resigned ', ' reappointed ', 'SFeb', ' rDec', 
                                  ' tJan', ' rApril', 'Sept'), 
                         to = c('', '', '', '', '', '', '', '', '', 'Feb', 'Dec', 'Jan', 'April', 'Sep'))
Comb <- FindReplace(Comb, Var = 'X2', replaceData = ReplaceDF3, exact = FALSE)

# Fill in if continuous spell
End1 <- Comb[, c('NameDistrict', 'X2')]
End1 <- DropNA(End1, 'X2')
Comb <- FillIn(Comb, End1, Var1 = 'X1', Var2 = 'X2', allow.cartesian = TRUE, KeyVar = 'NameDistrict')
Comb$X1[is.na(Comb$X1)] <- 'Mar 1 2014' # Ongoing assumed to 'End' in March 1 2014

Comb$End <- mdy(Comb$X1)

# Address Yellen and Bernanke discontinuity
Sub <- subset(Comb, NameDistrict == 'Ben S. Bernanke' | NameDistrict == 'Janet L. Yellen')
Sub$X2 <- gsub(' reappointed ', '', Sub$X2)
Sub <- VarDrop(Sub, c('Start', 'X1', 'End'))
Sub <- plyr::rename(Sub, c(X2 = 'Start'))
Sub$End <- c('Feb 3 2014', 'Mar 1 2014')  # Most recent month start for Yellen
Sub$Start <- mdy(Sub$Start)
Sub$End <- mdy(Sub$End)

# Final Clean Up before expansion
Comb <- VarDrop(Comb, c('X1', 'X2'))
Comb <- rbind(Comb, Sub)
Comb <- Comb[, c('NameDistrict', 'Start', 'End')]
names(Comb) <- c('name', 'Start', 'End')
Comb <- Comb[order(Comb$name, Comb$Start), ]

# Convert to quarters for merging with the main data set
Comb$Start <- quarter(Comb$Start, with_year = TRUE)
Comb$End <- quarter(Comb$End, with_year = TRUE)

rmExcept('Comb')


#### Find president appointed data ####
Pres <- read.dta('~/Dropbox/GreenBook/Base_Data/PresidentBase.dta')

RelevantQuart <- data.frame(dum = rep(1, nrow(Pres)), Quarter = Pres$Quarter)
Comb$dum <- 1

CombQ <- merge(Comb, RelevantQuart, by = 'dum', all = TRUE)
CombQ <- VarDrop(CombQ, 'dum')
CombQ <- subset(CombQ, (Quarter <= End & Quarter >= Start))

Appoint <- data.frame(Quarter = Pres$Quarter, pres_party = Pres$pres_party)
CombStart <- data.frame(name = Comb$name, Quarter = Comb$Start)
Appoint <- merge(CombStart, Appoint, by = 'Quarter', all.x = TRUE)
Appoint <- DropNA(Appoint, 'pres_party')
Appoint <- Appoint[!duplicated(Appoint$name), ]
Appoint <- VarDrop(Appoint, 'Quarter')

CombQ <- merge(CombQ, Appoint, by = 'name', all.x = TRUE)
CombQ <- VarDrop(CombQ, c('Start', 'End'))
CombQ <- DropNA(CombQ, 'pres_party')


# Create proportion Democratically appointed
CombQ$dum <- 1
CombQ <- group_by(CombQ, Quarter)
CombQ <- mutate(CombQ, Total = sum(dum))
CombQ <- mutate(CombQ, TotalRep = sum(pres_party))

CombQ$DemAppointPerc <- (1 - (CombQ$TotalRep/CombQ$Total)) * 100 
CombQ <- CombQ[!duplicated(CombQ$Quarter), ]
CombQ <- CombQ[order(CombQ$Quarter), ]
CombQ <- CombQ[, c('Quarter', 'DemAppointPerc')]
CombQ <- slide(CombQ, Var = 'DemAppointPerc', NewVar = 'DemAppointPerc_Lag3', slideBy = -3)

# Merge into main data set
cpi.data <- read.csv("/git_repositories/GreenBook/Data/GB_FRED_cpi_2007.csv")
cpi.data <- merge(cpi.data, CombQ, by = 'Quarter', all.x = TRUE)

write.table(x = cpi.data, file = "/git_repositories/GreenBook/Data/GB_FRED_cpi_2007.csv", sep = ",", row.names = FALSE)