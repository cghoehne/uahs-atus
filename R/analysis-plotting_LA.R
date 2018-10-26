
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%
#$%    Summary ATUS Statistics for each Metro   #$%
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%

# list of all dependant packages
list.of.packages <- c("chron",
                      "data.table",
                      "plyr",
                      "tidyverse",
                      "lubridate",
                      "broom",
                      "ggExtra",
                      "lfe",
                      "grid",
                      "gridExtra",
                      "spatstat",
                      "RColorBrewer",
                      "extrafont",
                      "weights",
                      "car",
                      "survey",
                      "MASS",
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
lapply(list.of.packages, library, character.only = TRUE)

#font_import()
loadfonts(device = "win")
options(scipen = 999) # force R not to use scientific notation
memory.limit(size=56000) # increase memory limit
my.font <- "Calibri Light"

# load cleaned ATUS data
#atus1 <- readRDS(here("data/export/atus1.rds"))
#atus2 <- readRDS(here("data/export/atus2.rds"))
#atus <- rbind(atus1,atus2)
#rm(atus1,atus2)

# load ATUS weights
atuswgts1 <- readRDS(here("data/import/atuswgts1.rds"))
atuswgts2 <- readRDS(here("data/import/atuswgts2.rds"))
atuswgts3 <- readRDS(here("data/import/atuswgts3.rds"))
atuswgts4 <- readRDS(here("data/import/atuswgts4.rds"))
atuswgts <- rbind(atuswgts1,atuswgts2,atuswgts3,atuswgts4)
rm(atuswgts1,atuswgts2,atuswgts3,atuswgts4)
names(atuswgts)[names(atuswgts) == 'TUCASEID'] <- 'id' # change id col to match rest 

# ** TEMP IMPORT OF OLD WORKSPACE WHILE UPDATING CODE ** #
load(here("data/temp/old_workspace.RData"))
#save.image(here("data/temp/old_workspace.RData"))
####

# Load list of MSAs
metros <-  read.csv("NCDC Global Hourly weather data/20180529_metros_list.csv", header = T, stringsAsFactors = F)
metros$Name <- gsub(" Metropolitan Statistical Area", "", metros$Name) # format name to remove "Metro..."
metros$Name <- gsub("-", "-", metros$Name) 
metros$Name <- gsub("-", "-", metros$Name) 

# Load full county list to get 2016 population estimates and dominant DOE BA climate zone
msa.pop.cz <- read.csv("Climate Zones/atus_msa_climate_zones_BA.csv", header = T, stringsAsFactors = F)
names(msa.pop.cz)[names(msa.pop.cz) == 'msa'] <- 'Name' # rename to merge
msa.pop.cz$Name <- gsub("--", "-", msa.pop.cz$Name) # extra formating to catch any unmatching names
metros$Name <- gsub("-", "-", metros$Name) # extra formating to catch any unmatching names
metros$Name <- gsub("-", "-", metros$Name) # extra formating to catch any unmatching names
not.msa.list <- metros[!(metros$Name %in% msa.pop.cz$Name),]  # make sure this is empty otherwise some metros are missing

print(Sys.time()) 

# once checked, merge
metros <- merge(metros, msa.pop.cz, by = "Name",drop=F) # add pop and climate zone to metros
rm(msa.pop.cz,not.msa.list)  # remove extra

metros.cz <- aggregate(respop72016~BA.Climate.Zone,data=metros,FUN=sum)

#atuswgts <- as.data.table(atuswgts)
#r.s <- nrow(atuswgts[1:floor(.N/4)])
#saveRDS(atuswgts[1:(r.s)], here("data/import/atuswgts1.rds"))
#saveRDS(atuswgts[(r.s+1):(r.s*2)], here("data/import/atuswgts2.rds"))
#saveRDS(atuswgts[(r.s*2+1):(r.s*3)], here("data/import/atuswgts3.rds"))
#saveRDS(atuswgts[(r.s*3+1):(.N)], here("data/import/atuswgts4.rds"))

# load activity intensity (MET) files
met.m <- read.csv("ATUS/2003-2015/MET_master.csv", header = T)
met.occ <- read.csv("ATUS/2003-2015/MET_occupation.csv", header = T)
met.trv <- read.csv("ATUS/2003-2015/MET_travel.csv", header = T)

# load sprawl data and merge to metros file
exp.sprawl.msa <-  read.csv("UAHS Outputs/exp_sprawl_MSA.csv", header = T, stringsAsFactors = F)
sprawl <- exp.sprawl.msa[,c('label','sprawl')]
metros <- merge(metros,sprawl, by.x = "abbr", by.y = "label")

dir.a <- file.path("UAHS Outputs","metros coded") # Create directory for ATUS output files

 
# Import ATUS files by MSA and name them by abbreviation and add relevenat new variables
for(a in 1:dim(metros)[1]){ 
  
  # read a metro file with temps, name it "df"
  df <- read.csv(file = file.path(dir.a,paste0("20170914_",metros$OutMerge[a])), header = T)
  
  df$msa_lab <- metros$abbr[a]    # add metro label column 
  df$cz_lab <- metros$BA.Climate.Zone[a]    # add climate zone label column 
  
  # rename variables
  names(df)[names(df) == 'TUDIARYDATE'] <- 'date'
  names(df)[names(df) == 'METFIPS'] <- 'msa'
  names(df)[names(df) == 'Month'] <- 'month'
  names(df)[names(df) == 'TUFNWGTP'] <- 'weight'
  names(df)[names(df) == 'TUCASEID'] <- 'id'
  names(df)[names(df) == 'TEAGE'] <- 'age'
  names(df)[names(df) == 'PESEX'] <- 'sex'
  names(df)[names(df) == 'Income'] <- 'income'
  names(df)[names(df) == 'PTDTRACE'] <- 'race'
  
  # calculate weighted exposure over 27 C for each activity
  df$exp27 <- as.numeric((abs(difftime(df$DateTimeStart,df$DateTimeMid.f, units = "mins")) * (df$temp1 - 27) * 0.5)
               + (abs(difftime(df$DateTimeMid.f,df$DateTimeMid, units = "mins")) * (df$temp2 - 27))
               + (abs(difftime(df$DateTimeMid,df$DateTimeMid.b, units = "mins")) * (df$temp3 - 27))
               + (abs(difftime(df$DateTimeMid.b,df$DateTimeEnd, units = "mins")) * (df$temp4 - 27))
               + (abs(difftime(df$DateTimeMid.b,df$DateTimeEnd, units = "mins")) * (df$temp5 - 27) * 0.5)
  )
  
  # calculate weighted avg temp for each activity
  #df$temp <- (df$exp27 / df$TUACTDUR24) + 27
  df$temp <- ((df$temp1 * 0.5) + (df$temp2) + (df$temp3) + (df$temp4) +  (df$temp5 * 0.5)) / 4
  
  # set all exp27 to 0 if exposure <= (27 C) * (activity duration)
  #df$exp27 <- ifelse(df$exp27 > 0, df$exp27, 0)
  df$exp27 <- (df$temp - 27) * df$TUACTDUR24
  # if exp27 is unnaturally high, adjust
  #df$exp27 <- ifelse((df$exp27 > 1.25 * (df$TUACTDUR24) * (df$temp - 27)) | (df$exp27 < 1.25 * (df$TUACTDUR24) * (df$temp - 27)) , df$TUACTDUR24 * (df$temp - 27), df$exp27)
    
  #df <- subset(df, !is.na(temp)) # remove NAs *** WARNING, REMOVES ALL INDOOR ACTIVITIES
  
  # add code for if weekday
  df$weekday <- 1
  df$weekday[df$TUDIARYDAY == 1 | df$TUDIARYDAY == 7] <- 0
  
  # add code for month and if summer
  df$month <- as.numeric(format(strptime(df$DateTimeMid, format="%Y-%m-%d %H:%M:%S"), format="%m"))
  df$summer <- 0
  df$summer[df$month == 6 | df$month == 7 | df$month == 8] <- 1
  
  # create time spent in various activities as seperate variables with a wieghted variable as well
  df$out_time    <- 0
  df$out_time_w  <- 0 

  df$trvl_time   <- 0
  df$trvl_time_w <- 0
  
  df$tot_time    <- df$TUACTDUR24 
  df$tot_time_w  <- df$TUACTDUR24 * df$weight
  
  # add Wieghted outdoor particiption variable
  df$outdoors_w <- df$outdoors * df$weight
  
  # Calculate total days in sample for use in weighting
  df$tot_days <- abs(difftime(min(as.Date(df$DateTime)),max(as.Date(df$DateTime)), units = "days"))

  # Reformat Year-Month to YYYYMM
  df$YrMth <- as.numeric(format(strptime(df$DateTimeMid, format="%Y-%m-%d %H:%M:%S"), format="%Y%m"))

  # cut temperature variable to binned ranges, USING: -5 C to 40 C (-#F to #F)  
  df$cuttemp <- cut(df$temp,c(-Inf, seq(from = -3, to = 39, by = 6),Inf))
  df$cuttemp2 <- cut(df$temp,c(seq(from = 21, to = 39, by = 3),Inf)) 
  
  # rename factor levels
  df$cuttemp <- revalue(df$cuttemp, c("(-Inf,-3]"="Below -3", "(-3,3]"="-3 to 3", "(3,9]"="3 to 9", "(9,15]"="9 to 15", "(15,21]"="15 to 21", "(21,27]"="21 to 27", "(27,33]"="27 to 33", "(33,39]"="33 to 39", "(39, Inf]"="Above 39"))  
  df$cuttemp2 <- revalue(df$cuttemp2, c("(21,24]"="21 to 24", "(24,27]"="24 to 27", "(27,30]"="27 to 30", "(30,33]"="30 to 33", "(33,36]"="33 to 36", "(36,39]"="36 to 39", "(39,Inf]"="Above 39")) 
  
  # loop through MSA df to populate time spent variables and thier weighted counterparts for wieghted calcs
  for(b in 1:dim(df)[1]){
    
    if(df$outdoors[b] == 1){
      df$out_time[b]   <- df$TUACTDUR24[b]
      df$out_time_w[b] <- df$TUACTDUR24[b] * df$weight[b]
    }

    if(df$TRTIER1P[b] == 18){
      df$trvl_time[b]   <- df$TUACTDUR24[b]
      df$trvl_time_w[b] <- df$TUACTDUR24[b] * df$weight[b]
    }
  }
  
  # assign season based on meterological seasons (months) and NOT astronomical seasons (equinox/solstice)
  season.id <- setNames(rep(c('winter', 'spring', 'summer','fall'),each=3), c(12,1:11))
  df$season <- unname(season.id[as.character(df$month)]) 

  # Convert Date, MSA, Season, Month to Factors 
  df$date <- as.factor(df$date)
  df$msa <- as.factor(df$msa)
  df$season <- as.factor(df$season)
  df$month <- as.factor(df$month)

  # Add new activity codes
  df$codes <- 9
  df$codes[df$TRTIER1P == 2] <- 1
  df$codes[df$TRTIER1P == 3 | df$TRTIER1P == 4 ] <- 2
  df$codes[df$TRTIER1P == 5] <- 3
  df$codes[df$TRTIER1P == 11] <- 4
  df$codes[df$TRTIER1P == 12] <- 5
  df$codes[df$TRTIER1P == 13] <- 6
  df$codes[df$TRTIER1P == 15] <- 7
  df$codes[df$TRTIER1P == 18] <- 8
  
  df$code.f[df$codes == 9] <- "Other"
  df$code.f[df$codes == 1] <- "HH Activities"
  df$code.f[df$codes == 2] <- "Care for Others"
  df$code.f[df$codes == 3] <- "Work"
  df$code.f[df$codes == 4] <- "Eat/Drink"
  df$code.f[df$codes == 5] <- "Social/Leisure"
  df$code.f[df$codes == 6] <- "Sports & Rec"
  df$code.f[df$codes == 7] <- "Volunteer"
  df$code.f[df$codes == 8] <- "Travel"
  
  df$code.f <- as.factor(df$code.f)
  
  # reorder activity factor
  df$code.f <- factor(df$code.f, levels = c("Care for Others", "Eat/Drink", "HH Activities","Social/Leisure","Sports & Rec","Travel","Volunteer","Work","Other"))
  
  # age group factor
  df$age.f[df$age >= 18 & df$age < 22] <- "18-22"
  df$age.f[df$age >= 23 & df$age < 27] <- "23-27"
  df$age.f[df$age >= 28 & df$age < 32] <- "28-32"
  df$age.f[df$age >= 33 & df$age < 37] <- "33-37"
  df$age.f[df$age >= 38 & df$age < 42] <- "38-42"
  df$age.f[df$age >= 43 & df$age < 47] <- "43-47"
  df$age.f[df$age >= 48 & df$age < 52] <- "48-52"
  df$age.f[df$age >= 53 & df$age < 57] <- "53-57"
  df$age.f[df$age >= 58 & df$age < 62] <- "58-62"
  df$age.f[df$age >= 63 & df$age < 67] <- "63-67"
  df$age.f[df$age >= 68 & df$age < 72] <- "68-72"
  df$age.f[df$age >= 73 & df$age < 77] <- "73-77"
  df$age.f[df$age >= 78 & df$age < 82] <- "78-82"
  df$age.f <- as.factor(df$age.f)
  
  # reorder age factor
  df$age.f <- factor(df$age.f, levels = c("18-22","23-27","28-32","33-37","38-42","43-47","48-52","53-57","58-62","63-67","68-72","73-77","78-80"))
  
  # age group factor 2
  df$age.f2[df$age >= 15 & df$age < 24] <- "15-24"
  df$age.f2[df$age >= 25 & df$age < 34] <- "25-34"
  df$age.f2[df$age >= 35 & df$age < 44] <- "35-44"
  df$age.f2[df$age >= 45 & df$age < 54] <- "45-54"
  df$age.f2[df$age >= 55 & df$age < 64] <- "55-64"
  df$age.f2[df$age >= 65] <- "65 and over"
  
  df$age.f2 <- as.factor(df$age.f2)
  
  # reorder age factor 2
  df$age.f2 <- factor(df$age.f2, levels = c("15-24","25-34","35-44","45-54","55-64","65 and over"))
  
  # add second activity code factor
  df$code.f2[df$code.f == "Other"] <- "Other"
  df$code.f2[df$code.f == "HH Activities"] <- "HH Activities"
  df$code.f2[df$code.f == "Care for Others"] <- "Care for Others"
  df$code.f2[df$code.f == "Work"] <- "Work"
  df$code.f2[df$code.f == "Social/Leisure"] <- "Social/Leisure"
  df$code.f2[df$code.f == "Eat/Drink"] <- "Other"
  df$code.f2[df$code.f == "Sports & Rec"] <- "Sports & Rec"
  df$code.f2[df$code.f == "Volunteer"] <- "Other"
  df$code.f2[df$code.f == "Travel"] <- "Travel"
  
  df$code.f2 <- as.factor(df$code.f2)
  
  # reorder activity factor 2
  df$code.f2 <- factor(df$code.f2, levels = c("Care for Others", "HH Activities","Social/Leisure","Sports & Rec","Travel","Work","Other"))
  
  # income group factor
  df$income.f[df$income == 1 | df$income == 2 | df$income == 3 | df$income == 4 | df$income == 5] <- "Less than $15k"
  df$income.f[df$income == 6 | df$income == 7 | df$income == 8] <- "$15k to $30k"
  df$income.f[df$income == 9 | df$income == 10 | df$income == 11] <- "$30k to $50k"
  df$income.f[df$income == 12 | df$income == 13] <- "$50k to $75k"
  df$income.f[df$income == 14] <- "$75k to $100k"
  df$income.f[df$income == 15 | df$income == 16] <- "$100k and over"
  
  df$income.f <- as.factor(df$income.f)
  
  # reorder income factor 
  df$income.f <- factor(df$income.f, levels = c("Less than $15k","$15k to $30k","$30k to $50k","$50k to $75k","$75k to $100k","$100k and over"))
  
  # sex factor
  df$sex.f <- factor(df$sex, levels = c(1,2),labels = c("Male","Female"))
  
  # race factor
  df$race2 <- df$race
  df$race2[df$race2 > 8] <- 9  # recode the less common mixed races to a single "other"
  df$race2 <- factor(df$race2, levels = c(1:9),labels = c("White only",
                                                                  "Black only",
                                                                  "American Indian only",
                                                                  "Asian only",
                                                                  "Pacific Islander only",
                                                                  "White-Black",
                                                                  "White-American Indian",
                                                                  "White-Asian",
                                                                  "Other Mixed Race"))
  # race factor 2
  df$race.f <- as.factor(df$race2)
  df$race.f <- factor(df$race.f, levels = c("White only","Black only","American Indian only","Asian only","Hawaiian/Pacific Islander only","White-Black","White-American Indian","White-Asian","Other Mixed Race"))
  
  df$race.f2 <- df$race
  df$race.f2[df$race.f2 > 5] <- 6
  df$race.f2 <- factor(df$race.f2, levels = c(1:6),labels = c("White",
                                                                      "Black",
                                                                      "American Indian",
                                                                      "Asian",
                                                                      "Pacific Islander",
                                                                      "Mixed Race"))
  # reorder 
  df$race.f2 <- factor(df$race.f2, levels = c("American Indian","Asian","Black","Pacific Islander","White","Mixed Race"))
  
  # elderly factor
  df$elderly <- ifelse(df$age > 65, "Over 65", "15 to 65")
  df$elderly <- factor(df$elderly, levels = c("15 to 65","Over 65")) # set factor order
  
  # black/non-black factor
  df$black_white <- ifelse(df$race == 2 | df$race == 6 ,"Black", "Non-black")
  df$black_white <- factor(df$black_white, levels = c("Black", "Non-black")) # set factor order
  
  # LOW INCOME & POVERTY FACTOR
  # low income defined by ~200% of 2017 federal poverty line.
  # HH size 1 = $24,120
  # HH size 2 = $32,480
  # HH size 3 = $40,840	
  # HH size 4 = $49,200
  # HH size 5 = $57,560
  # HH size 6 = $65,920
  # HH size 7 = $74,280
  # HH size 8+ = $82,640
  
  # low income factor
  df$low_inc <- 0
  df$low_inc[df$income <= 7] <- 1
  df$low_inc[df$TRNUMHOU == 2 & df$income <= 9] <- 1
  df$low_inc[df$TRNUMHOU == 3 & df$income <= 10] <- 1
  df$low_inc[df$TRNUMHOU == 4 & df$income <= 11] <- 1
  df$low_inc[df$TRNUMHOU == 5 & df$income <= 12] <- 1
  df$low_inc[df$TRNUMHOU == 6 & df$income <= 13] <- 1
  df$low_inc[df$TRNUMHOU == 7 & df$income <= 13] <- 1
  df$low_inc[df$TRNUMHOU >= 8 & df$income <= 14] <- 1

  # poverty factor
  df$poverty <- 0
  df$poverty[df$income <= 4] <- 1 # below ~12,500 is poverty no matter HH size
  df$poverty[df$TRNUMHOU == 2 & df$income <= 5] <- 1
  df$poverty[df$TRNUMHOU == 3 & df$income <= 6] <- 1
  df$poverty[df$TRNUMHOU == 4 & df$income <= 7] <- 1
  df$poverty[df$TRNUMHOU == 5 & df$income <= 8] <- 1
  df$poverty[df$TRNUMHOU == 6 & df$income <= 8] <- 1
  df$poverty[df$TRNUMHOU == 7 & df$income <= 9] <- 1
  df$poverty[df$TRNUMHOU == 8 & df$income <= 10] <- 1  
  df$poverty[df$TRNUMHOU == 9 & df$income <= 11] <- 1  
  df$poverty[df$TRNUMHOU == 10 & df$income <= 11] <- 1
  df$poverty[df$TRNUMHOU == 11 & df$income <= 11] <- 1
  df$poverty[df$TRNUMHOU == 12 & df$income <= 12] <- 1
  df$poverty[df$TRNUMHOU == 13 & df$income <= 12] <- 1
  df$poverty[df$TRNUMHOU == 14 & df$income <= 13] <- 1
  df$poverty[df$TRNUMHOU >= 15 & df$income <= 13] <- 1
  
  df$poverty[df$income < 0] <- NA
  
  df$poverty.f[df$poverty == 1] <- "Below Poverty"
  df$poverty.f[df$poverty == 0] <- "Above Poverty"
  
  df$poverty.f <- factor(df$poverty.f, levels = c("Below Poverty", "Above Poverty")) #order
  
  #add MSA and climate zones factors
  df$msa.f <- as.factor(df$msa_lab)
  df$cz.f <- as.factor(df$cz_lab)
  
  # reorder 
  df$cz.f <- factor(df$cz.f, levels = c("Marine", "Cold", "Mixed-Dry","Mixed-Humid","Hot-Dry","Hot-Humid"))
  
  # add MET values keeping all records from ATUS even if missing MET value
  df <- merge(df, met.m, by.x = "TRCODEP", by.y = "act_code_MET", all.x = TRUE)
  df <- merge(df, met.occ, by.x = "TRDTOCC1", by.y = "TRDTOCC1_MET", all.x = TRUE)
  df <- merge(df, met.trv, by.x = "TEWHERE", by.y = "loc_code_MET", all.x = TRUE)
  
  # if activity is WORK (TRTIER1P = 5) then keep MET.occ, otherwise, NA
  df$MET.occ[df$TRTIER1P != 5] <- NA
  
  # use the max of three MET values
  my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
  df$MET <- apply(df[c("MET.occ","MET.trv","MET.m")], 1, my.max)
  
  # adjust exp to be 0 if <= 27 C
  df$exp27 <- ifelse(df$exp27 > 0, df$exp27, NA)
  
  # add MET-dur, MET-exp, deg-min
  df$MET.dur <- ifelse(!is.na(df$MET), df$TUACTDUR24 * df$MET, NA)
  df$MET.exp27 <- ifelse(!is.na(df$MET), df$MET * df$exp27, NA)
  
  # add lawn time variable
  df$lawn_time <- ifelse(df$TRCODEP == 20501, df$TUACTDUR24, 0)

  assign(metros$abbr[a], df)  # assign abbr to name df for each metro
  
}

rm(list = ls(pattern = "^df"))       # remove objects that start with "df"
rm(met.m,met.occ,met.trv,exp.sprawl.msa,sprawl)

###########################################

#~<@>~~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~#
#       Summary ATUS Statistics        #
#~<@>~~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~#

# SUMMARY of SES
#metros <- metros[,c(1:12)]
for(a in 1:dim(metros)[1]){ 

  # Call MSA dataframe
  df <- get(metros$abbr[a])

  # merge replicate weights by person id
  df <- merge(df, atuswgts, by = "id") 
  
  # aggregate to person-day stats for people with at least 1 activity occuring above 27 C (sum specific variables over person-day)
  df.agg <- aggregate(cbind(tot_time,out_time,trvl_time,lawn_time)
                      ~(id)+weight,data = df, FUN = sum)
  
  # merge replicate weights to aggregated list of person-day stats
  df.p.agg <- merge(df.agg, atuswgts, by = "id")
  
  # create df with only outdoor activities in MSA 
  df.o <- df[df$outdoors == 1,]
  
  # create df with only activities that were outdoors AND above 27 C
  df.f <- df[df$temp >= 27 & df$outdoors == 1,]
  
  # filter to include all activies by people that had at least one outdoor activity above 27 C
  df.o.p <- df[df$id %in% df.f$id,]
  
  # create list of all people (1 row for each respondent) for SES estimates (exposure/activities not relevant here)
  df.p.unq <- df[!duplicated(df$id),] 
  
  # create list of people who had at least one outdoor activity for SES estimates (exposure/activities not relevant here)
  df.o.p.unq <- df.o.p[!duplicated(df.o.p$id),] 

  # Create a blank dataframe for error calcs
  err <- data.frame(matrix(vector(mode = 'numeric',length = 160), nrow = 160, ncol = 1))
  colnames(err) <- c('mean_day_time') # name first variable used later
  
  # calucate summary stats on outdoor/indoor fractions for all MSAs
  # SE for percentages = sq.rt ( p(1-p) / n)
  
  metros$act_tot_n[a] <- nrow(df)  # observed number of activities in sample MSA (indoor or outdoor)
  metros$act_out_n[a] <- nrow(df.o)  # observed number of outdoor activities in MSA
  metros$act_out_p[a] <- sum(df.o$weight, na.rm = T) / sum(df$weight, na.rm = T)  # Weighted percent of outdoor activities in MSA
  metros$act_out_p_SE[a] <- sqrt(metros$act_out_p[a] * (1 - metros$act_out_p[a]) / metros$act_out_n[a]) # Standard Error of outdoor % act > 27C
  
  metros$act_out_n_27[a] <- nrow(df.f)  # Report number of outdoor activities in MSA samples during 27 C or higher
  metros$act_out_p_27[a] <- sum(df.f$weight, na.rm = T) / sum(df$weight, na.rm = T)  # Report % of outdoor activities in MSA samples
  metros$act_out_p_27SE[a] <- sqrt(metros$act_out_p[a] * (1 - metros$act_out_p[a]) / metros$act_out_n[a]) # Standard Error of outdoor % act > 27C
  
  metros$per_tot_n[a] <- nrow(df.p.unq)  # Report number of people with activites occuring at or above 27C in MSA samples (indoor OR outdoor)
  metros$per_tot_n_wgt[a] <- sum(df.p.unq$weight) / ((31 + 31 + 30 + 31 + 30 + 31) + (365 * 9) + (366 * 2)) # est. number of weighted samples. total days in sample from June 04 to Dec 15
  #for(i in 1:160){j <- i + 303  
  #err$per_tot_n_wgt[i] <- ((sum(df.p.unq[,j])/4201) - (sum(df.p.unq$weight)/4201))^2}
  #metros$per_tot_n_wgt_SE[a] <- sqrt((4/160) * sum(err$per_tot_n_wgt)) / sqrt(nrow(df.p.agg))
  
  metros$per_out_n[a] <- nrow(df.p.unq[df.p.unq$outdoors == 1,])  # Report number of people with outdoor activites above 27C in MSA samples
  metros$per_out_n_wgt[a] <- sum(df.o.p.unq$weight) / ((31 + 31 + 30 + 31 + 30 + 31) + (365 * 9) + (366 * 2))
  
  metros$per_out_p[a] <- sum(df.o.p.unq$weight, na.rm = T) / sum(df.p.unq$weight, na.rm = T)  # Report % of people with at least one outdoor activity above 27C in MSA samples
  metros$per_out_p_SE[a] <- sqrt(metros$per_out_p[a] * (1 - metros$per_out_p[a]) / metros$per_out_n[a]) # Standard Error of outdoor % act > 27C
  
  metros$per_out_n_27[a] <- nrow(df.o.p.unq)  # Report number of people with outdoor activites above 27C in MSA samples
  metros$per_out_p_27[a] <- sum(df.o.p.unq$weight, na.rm = T) / sum(df.p.unq$weight, na.rm = T)  # Report % of people with at least one outdoor activity above 27C in MSA samples
  metros$per_out_p_27SE[a] <- sqrt(metros$per_out_p[a] * (1 - metros$per_out_p[a]) / metros$per_out_n[a]) # Standard Error of outdoor % act > 27C
  
  # calculate summary SES stats (% poverty, % white, % black, % male, % elderly)
  
  metros$poverty_p[a] <- sum(ifelse(df.p.unq$poverty.f == "Below Poverty", df.p.unq$weight, 0), na.rm = T) / sum(df.p.unq$weight) # WEIGHTED % of low income in MSA observed activites above 27 C (indoor or out)
  metros$poverty_p_SE[a] <- sqrt(metros$poverty_p[a] * (1 - metros$poverty_p[a]) / metros$per_tot_n_27[a]) # SE on percent poverty in population with activities above 27 C (indoor or out)
  metros$poverty_out_p[a] <- sum(ifelse(df.o.p.unq$poverty.f == "Below Poverty", df.o.p.unq$weight, 0), na.rm = T) / sum(df.o.p.unq$weight) # report % of low income in MSA observed activites above 27 C OUTDOOR ONLY
  metros$poverty_out_p_SE[a] <- sqrt(metros$poverty_out_p[a] * (1 - metros$poverty_out_p[a]) / metros$per_out_n_27[a]) # SE on percent poverty in population with outdoor activities above 27 C (indoor or out)
  
  metros$white_p[a] <- sum(ifelse(df.p.unq$race.f2 == "White", df.p.unq$weight, 0), na.rm = T) / sum(df.p.unq$weight) # WEIGHTED  % of white race in MSA observed activites above 27 C (indoor or out)
  metros$white_p_SE[a] <- sqrt(metros$white_p[a] * (1 - metros$white_p[a]) / metros$per_tot_n_27[a]) # SE on percent white in population with activities above 27 C (indoor or out)
  metros$white_out_p[a] <- sum(ifelse(df.o.p.unq$race.f2 == "White", df.o.p.unq$weight, 0), na.rm = T) / sum(df.o.p.unq$weight) # WEIGHTED % of white race in MSA observed activites above 27 C OUTDOOR ONLY
  metros$white_out_p_SE[a] <- sqrt(metros$white_out_p[a] * (1 - metros$white_out_p[a]) / metros$per_out_n_27[a]) # SE on percent white in population with outdoor activities above 27 C (indoor or out)
  
  metros$black_p[a] <- sum(ifelse(df.p.unq$race.f2 == "Black", df.p.unq$weight, 0), na.rm = T) / sum(df.p.unq$weight) # WEIGHTED  % of black race in MSA observed activites above 27 C (indoor or out)
  metros$black_p_SE[a] <- sqrt(metros$black_p[a] * (1 - metros$black_p[a]) / metros$per_tot_n_27[a]) # SE on percent black in population with activities above 27 C (indoor or out)
  metros$black_out_p[a] <- sum(ifelse(df.o.p.unq$race.f2 == "Black", df.o.p.unq$weight, 0), na.rm = T) / sum(df.o.p.unq$weight) # WEIGHTED % of black race in MSA observed activites above 27 C OUTDOOR ONLY
  metros$black_out_p_SE[a] <- sqrt(metros$black_out_p[a] * (1 - metros$black_out_p[a]) / metros$per_out_n_27[a]) # SE on percent black in population with outdoor activities above 27 C (indoor or out)
  
  metros$male_p[a] <- sum(ifelse(df.p.unq$sex.f == "Male", df.p.unq$weight, 0), na.rm = T) / sum(df.p.unq$weight) # WEIGHTED  % of males in MSA observed activites above 27 C (indoor or out)
  metros$male_p_SE[a] <- sqrt(metros$male_p[a] * (1 - metros$male_p[a]) / metros$per_tot_n_27[a]) # SE on percent male in population with activities above 27 C (indoor or out)
  metros$male_out_p[a] <- sum(ifelse(df.o.p.unq$sex.f == "Male", df.o.p.unq$weight, 0), na.rm = T) / sum(df.o.p.unq$weight) # WEIGHTED % of males in MSA observed activites above 27 C OUTDOOR ONLY
  metros$male_out_p_SE[a] <- sqrt(metros$male_out_p[a] * (1 - metros$male_out_p[a]) / metros$per_out_n_27[a]) # SE on percent male in population with outdoor activities above 27 C (indoor or out)
  
  metros$elder_p[a] <- sum(ifelse(df.p.unq$age.f2 == "65 and over", df.p.unq$weight, 0), na.rm = T) / sum(df.p.unq$weight) # WEIGHTED  % of elderly in MSA observed activites above 27 C (indoor or out)
  metros$elder_p_SE[a] <- sqrt(metros$elder_p[a] * (1 - metros$elder_p[a]) / metros$per_tot_n_27[a]) # SE on percent elder in population with activities above 27 C (indoor or out)
  metros$elder_out_p[a] <- sum(ifelse(df.o.p.unq$age.f2 == "65 and over", df.o.p.unq$weight, 0), na.rm = T) / sum(df.o.p.unq$weight) # WEIGHTED % of elderly in MSA observed activites above 27 C OUTDOOR ONLY
  metros$elder_out_p_SE[a] <- sqrt(metros$elder_out_p[a] * (1 - metros$elder_out_p[a]) / metros$per_out_n_27[a]) # SE on percent elder in population with outdoor activities above 27 C (indoor or out)
  
  # calculate wieghted mean time spent participaing in major activity groups by MSA
  # calculate (y_i - y_0)^2 term in variance eqn where i = 1 to 160 for each replicate weight
  # (adjust to call the correct column location of replicate weights e.g. columns 7 to 167 are replicate weights)
  # SE for mean = std.dev / sqrt(n_sample) .... or = sqrt(var) / sqrt(n)
  
  metros$mean_out_time[a]  <- sum(df.p.agg$out_time * df.p.agg$weight)  / sum(df.p.agg$weight)
  for(i in 1:160){j <- i + 6  
    err$mean_out_time[i] <- ((sum(df.p.agg$out_time * df.p.agg[,j]) / sum(df.p.agg[,j])) - metros$mean_out_time[a])^2}
  metros$mean_out_time_SE[a] <- sqrt((4/160) * sum(err$mean_out_time)) / sqrt(nrow(df.p.agg))
  
  metros$mean_trvl_time[a] <- sum(df.p.agg$trvl_time * df.p.agg$weight) / sum(df.p.agg$weight)
  for(i in 1:160){j <- i + 6 
    err$mean_trvl_time[i] <- ((sum(df.p.agg$trvl_time * df.p.agg[,j]) / sum(df.p.agg[,j])) - metros$mean_trvl_time[a])^2}
  metros$mean_trvl_time_SE[a] <- sqrt((4/160) * sum(err$mean_trvl_time)) / sqrt(nrow(df.p.agg))
  
  metros$mean_lawn_time[a] <- sum(df.p.agg$lawn_time * df.p.agg$weight) / sum(df.p.agg$weight)
  for(i in 1:160){j <- i + 6 
  err$mean_lawn_time[i] <- ((sum(df.p.agg$lawn_time * df.p.agg[,j]) / sum(df.p.agg[,j])) - metros$mean_lawn_time[a])^2}
  metros$mean_lawn_time_SE[a] <- sqrt((4/160) * sum(err$mean_lawn_time)) / sqrt(nrow(df.p.agg))

  # aggreage each individuals exposure from list of individuals with at least one outdoor activity above 27 C        #if(nrow(df.o.p) != 0){
  df.agg.o <- aggregate(cbind(exp27,MET.exp27,MET.dur) ~ id + weight, data = df.o.p, FUN = sum, na.action = na.omit) # NAs are omitted 
  
  # remerge replicate weights to determine standard error (rep weights lost during agg funciton)
  df.agg.o <- merge(df.agg.o, atuswgts, by = "id")
  
  # calucate the weighted mean, SEM, st.dev, and 10th/50th/90th percentiles for each exposure metric
  metros$n_o27[a] <- nrow(df.agg.o)  # number of samples (individuals with at least one activity outdoors above 27 C)
  quantiles.exp27 <- weighted.quantile(df.agg.o$exp27, df.agg.o$weight, probs=c(0.1,0.5,0.9)) # exp27 10th, 50th, and 90th percentiles
  quantiles.MET.exp27 <- weighted.quantile(df.agg.o$MET.exp27, df.agg.o$weight, probs=c(0.1,0.5,0.9)) # MET.exp27  10th, 50th, and 90th percentiles
  quantiles.MET.dur <- weighted.quantile(df.agg.o$MET.dur, df.agg.o$weight, probs=c(0.1,0.5,0.9)) # MET.dur 10th, 50th, and 90th percentiles
  
  # Weighted Outdoor Exposure Above 27 C per Person-Day (deg-min above 27 C) 
  metros$exp27_mean[a] <- weighted.mean(df.agg.o$exp27,df.agg.o$weight)   # weighted mean
  #metros$exp27_mean[a] <- sum(df.agg.o$exp27 * df.agg.o$weight) / sum(df.agg.o$weight) # same as above line, saving for backup/verification
  for(i in 1:160){j <- i + 5
  err$mean_exp27[i] <- (weighted.mean(df.agg.o$exp27,df.agg.o[,j]) - metros$exp27_mean[a])^2} # error calc
  metros$exp27_mean.SE[a] <- sqrt((4/160) * sum(err$mean_exp27)) / sqrt(metros$n_o27[a])  # SEM = sqrt(var) / sqrt(n)
  metros$exp27_SD[a]  <- sqrt(weighted.var(df.agg.o$exp27,df.agg.o$weight)) # weighted SD (deg-min above 27 C) 
  metros$exp27_SD2[a] <- sqrt((4/160) * sum(err$mean_exp27))
  metros$exp27_p10[a] <- quantiles.exp27[1] # Weighted 10th Percentile(deg-min above 27 C)
  metros$exp27_p50[a] <- quantiles.exp27[2] # Weighted 50th Percentile (deg-min above 27 C)
  metros$exp27_p90[a] <- quantiles.exp27[3] # Weighted 90th Percentile (deg-min above 27 C)
  
  # Weighted Outdoor Exposure Intensity Above 27 C per Person-Day (MET-deg-min above 27 C)
  metros$MET.exp27_mean[a] <- weighted.mean(df.agg.o$MET.exp27,df.agg.o$weight)   # Weighted Mean Outdoor Exposure Above 27 C per Person-Day (deg-min above 27 C)
  for(i in 1:160){j <- i + 5
  err$mean_MET.exp27[i] <- (weighted.mean(df.agg.o$MET.exp27,df.agg.o[,j]) - metros$MET.exp27_mean[a])^2}  # error calc
  metros$MET.exp27_mean.SE[a] <- sqrt((4/160) * sum(err$mean_MET.exp27)) / sqrt(metros$n_o27[a])  # SEM = sqrt(var) / sqrt(n)
  metros$MET.exp27_SD[a]  <- sqrt((4/160) * sum(err$mean_MET.exp27)) # Weighted Stand Dev of Outdoor Exposure Above 27 C per Person-Day (deg-min above 27 C) 
  metros$MET.exp27_p10[a] <- quantiles.MET.exp27[1] # Weighted 10th Percentile of Outdoor Expsoure Above 27 C per Person-Day (deg-min above 27 C)
  metros$MET.exp27_p50[a] <- quantiles.MET.exp27[2] # Weighted 50th Percentile of Outdoor Expsoure Above 27 C per Person-Day (deg-min above 27 C)
  metros$MET.exp27_p90[a] <- quantiles.MET.exp27[3] # Weighted 90th Percentile of Outdoor Expsoure Above 27 C per Person-Day (deg-min above 27 C)
  
  # Weighted Outdoor Activity Intensity Above 27 C per Person-Day (MET-min above 27 C)
  metros$MET.dur_mean[a] <- weighted.mean(df.agg.o$MET.dur,df.agg.o$weight)   # Weighted Mean Outdoor Exposure Above 27 C per Person-Day (deg-min above 27 C)
  for(i in 1:160){j <- i + 5
  err$mean_MET.dur[i] <- (weighted.mean(df.agg.o$MET.dur,df.agg.o[,j]) - metros$MET.dur_mean[a])^2} # error calc
  metros$MET.dur_mean.SE[a] <- sqrt((4/160) * sum(err$mean_MET.dur)) / sqrt(metros$n_o27[a])  # SEM = sqrt(var) / sqrt(n)
  metros$MET.dur_SD[a]  <- sqrt((4/160) * sum(err$mean_MET.dur)) # Weighted Stand Dev of Outdoor Exposure Above 27 C per Person-Day (deg-min above 27 C) 
  metros$MET.dur_p10[a] <- quantiles.MET.dur[1] # Weighted 10th Percentile of Outdoor Expsoure Above 27 C per Person-Day (deg-min above 27 C)
  metros$MET.dur_p50[a] <- quantiles.MET.dur[2] # Weighted 50th Percentile of Outdoor Expsoure Above 27 C per Person-Day (deg-min above 27 C)
  metros$MET.dur_p90[a] <- quantiles.MET.dur[3] # Weighted 90th Percentile of Outdoor Expsoure Above 27 C per Person-Day (deg-min above 27 C)
  
}

rm(list = ls(pattern = "^df"))       # remove rest of temporary objects by removing all that start with "df"

##############################################


# BOX PLOTS
#activity tier 1 names (TRTIER1P)
act.t1 <- c('1'="Personal Care",
            '2'="Household Activ.",
            '3'="Care for HH memb",
            '4'="Care for Non-HH memb",
            '5'="Work & Work Related",
            '6'="Education",
            '7'="Consumer Purchases",
            '8'="Personal/Professional care Services",
            '9'="HH Services",
            '10'="Gov't Service/Civic Obligation",
            '11'="Eating/Drinking",
            '12'="Social/Leisure",
            '13'="Sport/Exercise/Recreation",
            '14'="Religious/Spiritual",
            '15'="Volunteer",
            '16'="Telephone Calls",
            '18'="Traveling",
            '50'="Undefined")



# create a meta dataframe that has all Metro observations and add in replicate weights for err calcs
meta <- do.call(rbind, lapply(paste(metros$abbr), get) )
meta <- merge(meta, metros[,c('sprawl','abbr')], by.x = "msa.f", by.y = "abbr", all.x = T)

# create deg F temp and codes
meta$temp.F <- meta$temp * (9/5) + 32
meta$cuttemp.F <- cut(meta$temp.F,c(-Inf, seq(from = 0, to = 100, by = 10),Inf))
meta$cuttemp.F <- revalue(meta$cuttemp.F, c("(-Inf,0]"="Below 0", "(0,10]"="0 to 10", "(10,20]"="10 to 20", "(20,30]"="20 to 30", "(30,40]"="30 to 40","(40,50]"="40 to 50", "(50,60]"="50 to 60", "(60,70]"="60 to 70", "(70,80]"="70 to 80","(80,90]"="80 to 90","(90,100]"="90 to 100","(100, Inf]"="Above 100"))  

meta.s <- meta[,c('id','MET.dur','MET.exp27','exp27','MET',"TRCODEP","TUACTDUR24",'temp','cuttemp','code.f2','weight','age.f2','age','race.f2','sex.f','poverty.f','elderly','black_white','outdoors','sprawl','income.f','date','season','msa.f','temp.F','cuttemp.F')]
meta.s <- merge(meta.s,atuswgts, by = "id")

meta.s$exp27[is.na(meta.s$exp27)] <- 0
meta.s$MET.dur[is.na(meta.s$MET.dur)] <- 0
meta.s$MET.exp27[is.na(meta.s$MET.exp27)] <- 0
meta.s$MET[is.na(meta.s$MET)] <- 0

# create aggreagate subset of individuals with at least one outdoor activity above 21 C (21-27C is baseline)
meta.o   <- meta.s[                    meta.s$outdoors == 1 & !is.na(meta.s$outdoors),]                        # outdoors
meta.f   <- meta.s[meta.s$temp >= 21 & meta.s$outdoors == 1 & !is.na(meta.s$temp) & !is.na(meta.s$outdoors),]  # outdoors & above 21 C
meta.f27 <- meta.s[meta.s$temp >= 27 & meta.s$outdoors == 1 & !is.na(meta.s$temp) & !is.na(meta.s$outdoors),]  # outdoors & above 27 C

# aggregate to individual; different levels
meta.s.agg <- aggregate(cbind(MET.exp27,MET.dur,exp27,MET)~(id)+weight+age.f2+race.f2+sex.f+poverty.f+income.f+outdoors+date+season+msa.f,data = meta.s, FUN = sum)
meta.o.agg <- aggregate(cbind(MET.exp27,MET.dur,exp27,MET)~(id)+weight+age.f2+race.f2+sex.f+poverty.f+income.f+outdoors+date+season+msa.f,data = meta.o, FUN = sum)
meta.f.agg <- aggregate(cbind(MET.exp27,MET.dur,exp27,MET)~(id)+weight+age.f2+race.f2+sex.f+poverty.f+income.f+outdoors+date+season+msa.f,data = meta.f, FUN = sum)
meta.f27.agg <- aggregate(cbind(MET.exp27,MET.dur,exp27,MET)~(id)+weight+age.f2+age+race.f2+sex.f+poverty.f+income.f+sprawl+date+season+msa.f,data = meta.f27, FUN = sum)

# remerge replicate weights
meta.s.agg <- merge(meta.s.agg,atuswgts, by = "id")
meta.o.agg <- merge(meta.o.agg,atuswgts, by = "id")
meta.f.agg <- merge(meta.f.agg,atuswgts, by = "id")
meta.f27.agg <- merge(meta.f27.agg,atuswgts, by = "id")



# weighted proportions for major activity types and temperature thersholds
nrow(meta.s) # total unique IDs = total respondents

nrow(meta.s[meta.s$temp > 21 & meta.s$temp <= 27 & meta.s$outdoors == 0,])
nrow(meta.s[meta.s$temp > 21 & meta.s$temp <= 27 & meta.s$outdoors == 1,])

nrow(meta.s[meta.s$temp > 21 & meta.s$temp <= 27 & meta.s$outdoors == 0,])
nrow(meta.s[meta.s$temp > 27 & meta.s$temp <= 33 & meta.s$outdoors == 1,])

nrow(meta.s[meta.s$temp > 33 & meta.s$outdoors == 0,])
nrow(meta.s[meta.s$temp > 33 & meta.s$outdoors == 1,])

nrow(meta.f27.agg)


##################################
# *** BEGIN FIGURE FORMATING ***

# Function for formatting Log10 axis labels
fmt_dcimals <- function(decimals=0){
  # return a function responpsible for formatting the 
  # axis labels with a given number of decimals 
  function(x) prettyNum(as.character(round(x,decimals)),big.mark = ",",trim=TRUE)
}

# Function for minor breaks in Log10 plots
log10_minor_break = function (...){function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    minor_breaks = 
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }}

# boxplot cuttemp color palette
cols <- c("21 to 27" = "grey75", "27 to 33" = "#FFFE00", "33 to 39" = "#FF6701", "Above 39" = "#CC0001")
col.f <- c("70 to 80" = "grey75", "80 to 90" = "#FFFE00", "90 to 100" = "#FF6701", "Above 100" = "#CC0001")

# create population data for meta.f
# create blank df
meta.f.pop <- meta.f[,c('id','MET.dur','cuttemp','code.f2','weight','elderly','black_white','poverty.f','sex.f','msa.f','temp.F','cuttemp.F')]
meta.f2 <- meta.f[,c('id','MET.dur','cuttemp','code.f2','weight','elderly','black_white','poverty.f','sex.f','msa.f','temp.F','cuttemp.F')]

#x <- 0
#y <- 0
# loop through 
#for(a in 1:dim(meta.f2)[1]){ #
#  x <- y + 1
#  y <- y + round(meta.f2$weight[a]/50000,0)
#  meta.f.pop[x:y,] <- meta.f2[a,]
#}

# MET-ACTIVITY FIGURE 



# add labelers for number of observations by cuttemp and acitvity code.f2 and for percent of observations outdoors

meta.f.pop2 <- meta.f.pop[meta.f.pop$code.f2 != "Work",]
meta.f.pop2$code.f3 <- meta.f.pop2$code.f2
#meta.f.pop2$code.f3 <- ifelse(meta.f.pop2$code.f3 == 7, 6, meta.f.pop2$code.f3)
#meta.f.pop2$code.f3 <- factor(meta.f.pop2$code.f3, levels = c(1:6),labels = c("Care for Others", "Household","Social/Leisure","Sports & Rec","Travel","Other"))
meta.f.pop2$code.f3 <- factor(meta.f.pop2$code.f2)

meta.f$code.f3 <- ifelse(meta.f$code.f2 != "Work", meta.f$code.f2, NA)
meta.f$code.f3 <- factor(meta.f$code.f3)
#meta.f$code.f3 <- ifelse(meta.f$code.f3 == 7, 6, meta.f$code.f3)
#meta.f$code.f3 <- factor(meta.f$code.f3, levels = c(1:6),labels = c("Care for Others", "Household","Social/Leisure","Sports & Rec","Travel","Other"))


act.t <- plyr::ddply(.data=meta.f.pop2[!is.na(meta.f.pop2$MET.dur) & meta.f.pop2$temp.F > 80,], 
                 .(cuttemp.F,code.f3), 
                 plyr::summarize, 
                 n=length(!is.na(code.f3)))

act.t$n <- paste("n =",act.t$n)
act.t <- act.t[!is.na(act.t$code.f3),]

# create activity x MET-time by cuttemp plot
p26.1 <- ggplot(data=meta.f.pop2[!is.na(meta.f.pop2$MET.dur) & !is.na(meta.f.pop2$code.f3) & meta.f.pop2$temp.F > 80,], aes(x=cuttemp.F, y=MET.dur, fill=cuttemp.F)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "Activity Intensity-time (MET-min)", labels = fmt_dcimals(1), limits=c(1,5000), breaks=c(1,5,10,50,100,500,1000,5000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  scale_x_discrete(name = "Heat Index (°F, binned)") +
  ggtitle("") +
  theme_bw() +
  geom_text(data=act.t, aes(x=cuttemp.F, y=1.1, label=n), family = my.font,
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = my.font,
            #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  facet_grid(.~code.f3) +
  theme(plot.title = element_text(size = 12, family = my.font),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14, vjust = -0.9),
        text = element_text(size=12, family=my.font),
        axis.text.y=element_text(color="black",size=11),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.minor.y = element_line(colour="gray85"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 6, face = "bold"),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = col.f)
p26.1

ggsave(here("data/export/MET_exp_by_act_bxplt_300_weighted_F.tiff"), p26.1, device = "tiff",
       scale = 1, width = 5, height = 5, dpi = 300,units = "in")

### SOCIO PLOT (ELDERLY, BLACKS, GENDER) ###
#############################################

# ELDERLY VS NON ELDERLY
# labler
n.soc1 <- plyr::ddply(.data=meta.f[!is.na(meta.f$MET.dur) & meta.f$temp.F > 80,], 
                  .(cuttemp.F,elderly), 
                plyr::summarize, 
                  n=length(elderly))

n.soc1$n <- paste("n =",n.soc1$n)
n.soc1 <- n.soc1[!is.na(n.soc1$elderly),]

# create activity x MET-time by cuttemp plot
p27.1 <- ggplot(data=meta.f.pop[!is.na(meta.f.pop$MET.dur) & meta.f.pop$temp.F > 80,], aes(x=cuttemp.F, y=MET.dur, fill=cuttemp.F)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "Activity Intensity-time (MET-min)", labels = fmt_dcimals(1), limits=c(1,5000), breaks=c(1,5,10,50,100,500,1000,5000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  #scale_x_discrete(name = "Apparent Temperature (deg C)") +
  theme_bw() +
  #geom_text(data=n.soc1, aes(x=cuttemp.F, y=1.1, label=n), family = my.font,
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = my.font,
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  facet_grid(.~elderly) +
  theme(plot.title = element_text(size = 12, family = my.font),
        #plot.margin = unit(c(0,2,0,2),"cm"),
        text = element_text(size=12, family=my.font),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_text(color="black",size=11),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.minor.y = element_line(colour="gray85"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8, face = "bold"),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = col.f)

# BLACK VS NON BLACK
# labler
n.soc2 <- plyr::ddply(.data=meta.f[!is.na(meta.f$MET.dur) & meta.f$temp.F > 80,], 
                .(cuttemp.F,black_white), 
                plyr::summarize, 
                n=length(black_white))

n.soc2$n <- paste("n =",n.soc2$n)
n.soc2 <- n.soc2[!is.na(n.soc2$black_white),]

# create activity x MET-time by cuttemp plot 
p27.2 <- ggplot(data=meta.f.pop[!is.na(meta.f.pop$MET.dur) & meta.f.pop$temp.F > 80,], aes(x=cuttemp.F, y=MET.dur, fill=cuttemp.F)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(labels = fmt_dcimals(1), limits=c(1,5000), breaks=c(1,5,10,50,100,500,1000,5000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  scale_x_discrete(name = "Apparent Temperature (deg C)") +
  theme_bw() +
  #geom_text(data=n.soc2, aes(x=cuttemp.F, y=1.1, label=n), family = my.font,
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = my.font,
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  facet_grid(.~black_white) +
  theme(plot.title = element_text(size = 12, family = my.font),
        #plot.margin = unit(c(0,2,0,2),"cm"),
        text = element_text(size=12, family=my.font),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_text(color="black",size=11),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.minor.y = element_line(colour="gray85"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8, face = "bold"),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = col.f)

# MALE VS FEMALE
# labler
n.soc3 <- plyr::ddply(.data=meta.f[!is.na(meta.f$MET.dur) & meta.f$temp.F > 80,], 
                .(cuttemp.F,sex.f), 
                plyr::summarize, 
                n=length(sex.f))

n.soc3$n <- paste("n =",n.soc3$n)
n.soc3 <- n.soc3[!is.na(n.soc3$sex.f),]

# create activity x MET-time by cuttemp plot
p27.3 <- ggplot(data=meta.f.pop[!is.na(meta.f.pop$sex.f) & meta.f.pop$temp.F > 80,], aes(x=cuttemp.F, y=MET.dur, fill=cuttemp.F)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(labels = fmt_dcimals(1), limits=c(1,5000), breaks=c(1,5,10,50,100,500,1000,5000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  #scale_x_discrete(name = "Apparent Temperature (deg C)") +
  theme_bw() +
  #geom_text(data=n.soc3, aes(x=cuttemp.F, y=1.1, label=n), family = my.font,
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = my.font,
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  facet_grid(.~sex.f) +
  theme(plot.title = element_text(size = 12, family = my.font),
        #plot.margin = unit(c(0,2,0,2),"cm"),
        text = element_text(size=12, family=my.font),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_text(color="black",size=11),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.minor.y = element_line(colour="gray85"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8, face = "bold"),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = col.f)

require(gridExtra)
socio.plot <- grid.arrange(p27.1,p27.2,p27.3, ncol=3, 
                           bottom=textGrob("Heat Index (°F, binned)", gp=gpar(fontface="plain",fontfamily=my.font,fontsize=14)),
                           left=textGrob("Activity Intensity-time (MET-min)", gp=gpar(fontface="plain",fontfamily=my.font,fontsize=14), rot = 90))

socio.plot.an <- arrangeGrob(socio.plot, top = textGrob("(a)                                            (b)                                            (c)                 ", gp=gpar(fontfamily = my.font)))
 
ggsave(here("data/export/MET_exp_socio_bxplt_300_weighted_F.tiff"), socio.plot.an, device = "tiff",
       scale = 1, width = 5.5, height = 4, dpi = 300,units = "in")


# percenile summary of socio boxplots

median.summary <- rbind(as.data.frame(table(meta.f$elderly[meta.f$cuttemp != "21 to 27"])),
                        as.data.frame(table(meta.f$black_white[meta.f$cuttemp != "21 to 27"])),
                        as.data.frame(table(meta.f$poverty.f[meta.f$cuttemp != "21 to 27"])),
                        as.data.frame(table(meta.f$code.f2[meta.f$cuttemp != "21 to 27"]))
                        )

median.summary$median[1] <- median(meta.f.pop$MET.dur[meta.f.pop$elderly == "15 to 65" & meta.f.pop$cuttemp != "21 to 27"])
median.summary$median[2] <- median(meta.f.pop$MET.dur[meta.f.pop$elderly == "Over 65" & meta.f.pop$cuttemp != "21 to 27"])
median.summary$median[3] <- median(meta.f.pop$MET.dur[meta.f.pop$black_white == "Black" & meta.f.pop$cuttemp != "21 to 27"])
median.summary$median[4] <- median(meta.f.pop$MET.dur[meta.f.pop$black_white == "Non-black" & meta.f.pop$cuttemp != "21 to 27"])
median.summary$median[5] <- median(meta.f.pop$MET.dur[meta.f.pop$poverty.f == "Below Poverty" & !is.na(meta.f.pop$poverty.f ) & meta.f.pop$cuttemp != "21 to 27"])
median.summary$median[6] <- median(meta.f.pop$MET.dur[meta.f.pop$poverty.f == "Above Poverty" & !is.na(meta.f.pop$poverty.f) & meta.f.pop$cuttemp != "21 to 27"])
median.summary$median[7] <- median(meta.f.pop$MET.dur[meta.f.pop$code.f2 == "Care for Others" & meta.f.pop$cuttemp != "21 to 27"])
median.summary$median[8] <- median(meta.f.pop$MET.dur[meta.f.pop$code.f2 == "HH Activities" & meta.f.pop$cuttemp != "21 to 27"])
median.summary$median[9] <- median(meta.f.pop$MET.dur[meta.f.pop$code.f2 == "Social/Leisure" & meta.f.pop$cuttemp != "21 to 27"])
median.summary$median[10] <- median(meta.f.pop$MET.dur[meta.f.pop$code.f2 == "Sports & Rec" & meta.f.pop$cuttemp != "21 to 27"])
median.summary$median[11] <- median(meta.f.pop$MET.dur[meta.f.pop$code.f2 == "Travel" & meta.f.pop$cuttemp != "21 to 27"])
median.summary$median[12] <- median(meta.f.pop$MET.dur[meta.f.pop$code.f2 == "Work" & meta.f.pop$cuttemp != "21 to 27"])
median.summary$median[13] <- median(meta.f.pop$MET.dur[meta.f.pop$code.f2 == "Other" & meta.f.pop$cuttemp != "21 to 27"])

median.summary$P90[1] <- quantile(meta.f.pop$MET.dur[meta.f.pop$elderly == "15 to 65" & meta.f.pop$cuttemp != "21 to 27"], probs = .9)
median.summary$P90[2] <- quantile(meta.f.pop$MET.dur[meta.f.pop$elderly == "Over 65" & meta.f.pop$cuttemp != "21 to 27"], probs = .9)
median.summary$P90[3] <- quantile(meta.f.pop$MET.dur[meta.f.pop$black_white == "Black"  & meta.f.pop$cuttemp != "21 to 27"], probs = .9)
median.summary$P90[4] <- quantile(meta.f.pop$MET.dur[meta.f.pop$black_white == "Non-black"  & meta.f.pop$cuttemp != "21 to 27"], probs = .9)
median.summary$P90[5] <- quantile(meta.f.pop$MET.dur[meta.f.pop$poverty.f == "Below Poverty"  & meta.f.pop$cuttemp != "21 to 27" & !is.na(meta.f.pop$poverty.f)], probs = .9)
median.summary$P90[6] <- quantile(meta.f.pop$MET.dur[meta.f.pop$poverty.f == "Above Poverty"  & meta.f.pop$cuttemp != "21 to 27" & !is.na(meta.f.pop$poverty.f)], probs = .9)

median(meta.f.pop$MET.dur[meta.f.pop$cuttemp != "21 to 27"])
quantile(meta.f.pop$MET.dur[meta.f.pop$cuttemp != "21 to 27"], probs = .9)
quantile(meta.f.pop$MET.dur[meta.f.pop$cuttemp != "21 to 27"], probs = .95)
nrow(meta.f.pop[meta.f.pop$cuttemp != "21 to 27",])

###### MSA BOXPLOTS ###########

# create aggreagate subset of indivuiduals with at least one outdoor activity above 27
length(meta$outdoors[meta$outdoors == 100 & meta$temp > 27])
length(meta$outdoors[meta$exp27 > 1 & meta$outdoors == 100])

# filter to include only individuals with at least one outdoor activity above 27 C
meta$o27[meta$outdoors == 100 & meta$temp > 27] <- 1

# filter
meta.trim <- meta[,c('id','msa_lab','cz_lab','exp27','o27','TUACTDUR24','TRCODEP','temp','cuttemp','MET.exp27','MET.dur','MET','age','income.f','race.f2','weight')]
meta.f.p <- meta.trim[meta$id %in% meta.trim$id,]

#meta.f.p$exp27 <-  ifelse(meta.f.p$o27 == 0, 0, meta.f.p$exp27) # SET EXPOSURE TO 0 IF INSIDE 
#keep only outdoor activies of all
meta.f.p <- meta.f.p[meta.f.p$MET.exp27 > 0,]

# aggreage each individuals Exposure from list of individuals with at least one outdoor activity above 27 C
meta.p1 <- aggregate(cbind(exp27,MET.exp27,TUACTDUR24,MET.dur) ~ id+msa_lab+cz_lab+weight, data = meta.f.p, FUN = sum)
meta.p2 <- aggregate(cbind(temp,MET) ~ id, data = meta.f.p, FUN = mean)
meta.p <- merge(meta.p1,meta.p2,by="id")
meta.p <- meta.p[meta.p$MET.exp27 > 0,]  # WHY USE 1 NOT 0?
#meta.p$temp <- meta.p$deg.min / meta.p$TUACTDUR24

meta.p$msa.f <- as.factor(meta.p$msa_lab)
meta.p$cz.f <- as.factor(meta.p$cz_lab)

# long MSA names
long.names <- c("OKC" = "Oklahoma City, OK",
                "HOU" = "Houston, TX",
                "STL" = "St. Louis, MO",
                "CLB" = "Columbus, OH",
                "SA" = "San Antonio, TX",
                "RAL" = "Raleigh, NC",
                "LV" = "Las Vegas, NV",
                "BIR" = "Birmingham, AL",
                "TB" = "Tampa, FL",
                "PHX" = "Phoenix, AZ",
                "DAL" = "Dallas, TX",
                "NO" = "New Orleans, LA",
                "ORL" = "Orlando, FL",
                "MPH" = "Memphis, TN",
                "JAK" = "Jacksonville, FL",
                "MIA" = "Miami, FL",
                "KC" = "Kansas City, MO",
                "CLV" = "Cleveland, OH",
                "RCH" = "Richmond, VA",
                "VB" = "Virginia Beach, VA",
                "WAS" = "Washington, DC",
                "LOU" = "Louisville, KY",
                "BAL" = "Baltimore, MD",
                "NSH" = "Nashville, TN",
                "MIN" = "Minneapolis, MN",
                "IND" = "Indianapolis, IN",
                "AST" = "Austin, TX",
                "TUC" = "Tucson, AZ",
                "RIV" = "Riverside, CA",
                "ATL" = "Atlanta, GA",
                "SAC" = "Sacramento, CA",
                "DET" = "Detroit, MI",
                "CHI" = "Chicago, IL",
                "PHI" = "Philadelphia, PA",
                "PIT" = "Pittsburgh, PA",
                "MIL" = "Milwaukee, WI",
                "PRO" = "Providence, RI",
                "CHA" = "Charlotte, NC",
                "ELP" = "El Paso, TX",
                "DEN" = "Denver, CO",
                "CIN" = "Cincinnati, OH",
                "NYC" = "New York City, NY",
                "BOS" = "Boston, MA",
                "ABQ" = "Albuquerque, NM",
                "LA" = "Los Angeles, CA",
                "SJ" = "San Jose, CA",
                "SEA" = "Seattle, WA",
                "SF" = "San Francisco, CA",
                "POR" = "Portland, OR",
                "SD" = "San Diego, CA")
meta.p$msa.f <- revalue(meta.p$msa.f, long.names)

# order factor MSA and CZ labels quartiles of exposure
ord <- plyr::ddply(.data=meta.p, 
             .(msa.f), 
             plyr::summarize, 
             q75=quantile(exp27,probs = .75),
             q50=quantile(exp27,probs = .50),
             q25=quantile(exp27,probs = .25))

ord$msa.f <- as.factor(ord$msa.f)

meta.p$msa.f2 <- factor(meta.p$msa.f, levels=ord$msa.f[order(ord$q50,ord$q75,ord$q25)], ordered = TRUE)

meta.p$cz.f2 <- factor(meta.p$cz.f, levels = c("Hot-Humid","Hot-Dry","Mixed-Humid","Mixed-Dry","Cold","Marine"))

# create population size of data to show weighted results

# create blank df
meta.pop <- meta.p[0,]

x <- 0
y <- 0
# loop through 
for(a in 1:dim(meta.p)[1]){ #
  x <- y + 1
  y <- y + round(meta.p$weight[a]/10000,0)
  meta.pop[x:y,] <- meta.p[a,]
}


# add labelers for number of observations by cuttemp and race and for percent of observations outdoors
n.msa.t <- plyr::ddply(.data=meta.p, 
                  .(msa.f), 
                  plyr::summarize, 
                  n=length(msa.f))

n.msa.t$n <- paste("n =",n.msa.t$n)
n.msa.t <- n.msa.t[!is.na(n.msa.t$msa.f),]

cols2 <- c("Marine" = "#60BFC1", "Mixed-Dry" = "#EFF3BD", "Mixed-Humid" = "#F58980", "Cold" = "#466582", "Hot-Dry" = "#E1C97D", "Hot-Humid" = "#E24A2C")


# MET-minutes MSA boxplot

# order factor MSA and CZ labels quartiles of exposure
ord.met <- plyr::ddply(.data=meta.pop, 
             .(msa.f), 
             plyr::summarize, 
             q75=quantile(MET.exp27,probs = .75,na.rm = T),
             q50=quantile(MET.exp27,probs = .50,na.rm = T),
             q25=quantile(MET.exp27,probs = .25,na.rm = T))

ord.met$msa.f <- as.factor(ord.met$msa.f)

meta.pop$msa.f2.1 <- factor(meta.pop$msa.f, levels=ord.met$msa.f[order(ord.met$q50,ord.met$q75,ord.met$q25)], ordered = TRUE)


p40.1 <- ggplot(data=meta.pop[!is.na(meta.pop$MET.exp27),], aes(x=msa.f2.1, y=MET.exp27, fill=cz.f2)) +
  geom_boxplot(outlier.colour = "grey80", outlier.size = 0.2) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "MET-degree-minutes above 27?C per person per day", labels = fmt_dcimals(1), limits=c(1,80000), breaks=c(1,10,50,100,500,1000,5000,10000,50000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (person-deg-min above 27 deg C per day)", limits=c(0,20000), labels = fmt_dcimals(1)) + 
  scale_x_discrete(name = "Metropoltain Statistical Area", limits = rev(levels(meta.pop$msa.f2.1))) +
  ggtitle("") +
  labs(fill="Climate Zone") +
  theme_bw() +
  geom_text(data=n.msa.t, aes(x=msa.f, y=75000, label=n),family = my.font,
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 0, size = 2.5, vjust=.3) +
  #facet_grid(.~cz.f2) +
  theme(plot.title = element_text(size = 11, family = my.font),
        text = element_text(size=11, family=my.font),
        #axis.title = element_text(face="bold"),
        legend.text = element_text(size=9.5),
        legend.position = c(.45,-.16),
        plot.margin=unit(c(1, 1, 20, 1), units="mm"),
        axis.text.y=element_text(color="black",size=9),
        axis.title.x = element_text(hjust = 0.28),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.x = element_line(colour="gray78"),
        panel.grid.minor.x = element_line(colour="gray80"),
        panel.grid.major.y = element_line(colour="gray80"),
        strip.text.x = element_text(size = 8.5)) +
  #legend.position="none") +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE, byrow = TRUE)) +
  scale_fill_manual(values = cols2) +
  coord_flip()
p40.1

ggsave("20180529_msa_MET_exp_bxplt_300_weighted.tiff", p40.1, device = "tiff", path = "UAHS Outputs",
       scale = 1, width = 6.5, height = 8.2, dpi = 300,units = "in")


# drop MSAs with p value > 0.05 
msa.keeps <- metros[metros$p_val_MET.exp27_mean <= 0.05,]
meta.pop.keep <- meta.pop[meta.pop$msa_lab %in% msa.keeps$abbr,]
msa.keeps$n_eq <- paste("n =",msa.keeps$per_out_n_27)
msa.keeps$p_val <- paste("p = ",ifelse(msa.keeps$p_val_MET.exp27_mean < 0.001, "< 0.001",round(msa.keeps$p_val_MET.exp27_mean, 3)))

msa.keeps$abbr <- revalue(msa.keeps$abbr, long.names)
#msa.drops <- plyr::ddply(.data=meta.p, .(msa.f),plyr::summarize,n=length(msa.f))  # instead drop if n < 30
#msa.drops <- msa.drops[msa.drops$n > 30,]
#msa.drops$n_eq <- paste("n =",msa.drops$n)
#msa.drops <- msa.drops[!is.na(msa.drops$msa.f),]

# order factor MSA and CZ labels quartiles of exposure
ord.met.keep <- plyr::ddply(.data=meta.pop.keep, 
                 .(msa.f), 
                 plyr::summarize, 
                 q75=quantile(MET.exp27,probs = .75,na.rm = T),
                 q50=quantile(MET.exp27,probs = .50,na.rm = T),
                 q25=quantile(MET.exp27,probs = .25,na.rm = T))

ord.met.keep$msa.f <- as.factor(ord.met.keep$msa.f)

meta.pop.keep$msa.f2.1 <- factor(meta.pop.keep$msa.f, levels=ord.met.keep$msa.f[order(ord.met.keep$q50,ord.met.keep$q75,ord.met.keep$q25)], ordered = TRUE)

p40.1.keeps <- ggplot(data=meta.pop.keep[!is.na(meta.pop.keep$MET.exp27),], aes(x=msa.f2.1, y=MET.exp27, fill=cz.f2)) +
  geom_boxplot(outlier.colour = "grey80", outlier.size = 0.2) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "MET-degree-minutes above 27?C per person per day", labels = fmt_dcimals(1), limits=c(1,90000), breaks=c(1,10,50,100,500,1000,5000,10000,50000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (person-deg-min above 27 deg C per day)", limits=c(0,20000), labels = fmt_dcimals(1)) + 
  scale_x_discrete(name = "Metropoltain Statistical Area") +
  ggtitle("") +
  labs(fill="Climate Zone") +
  theme_bw() +
  geom_text(data=msa.keeps, aes(x=abbr, y=75000, label=n_eq),family = my.font,
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 0, size = 2.5, vjust=.3) +
  #geom_text(data=msa.keeps, aes(x=abbr, y=75000, label=p_val),family = my.font,
   #         colour="black", inherit.aes=FALSE, parse=FALSE, angle = 0, size = 2.5, vjust=.3) +
  #facet_grid(.~cz.f2) +
  theme(plot.title = element_text(size = 11, family = my.font),
        text = element_text(size=11, family=my.font),
        #axis.title = element_text(face="bold"),
        legend.text = element_text(size=9.5),
        legend.position = c(.45,-.16),
        plot.margin=unit(c(1, 1, 20, 1), units="mm"),
        axis.text.y=element_text(color="black",size=9),
        axis.title.x = element_text(hjust = 0.28),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.x = element_line(colour="gray78"),
        panel.grid.minor.x = element_line(colour="gray80"),
        panel.grid.major.y = element_line(colour="gray80"),
        strip.text.x = element_text(size = 8.5)) +
  #legend.position="none") +
  guides(fill = guide_legend(nrow = 1, reverse = FALSE, byrow = TRUE)) +
  scale_fill_manual(values = cols2) +
  coord_flip()

ggsave("20180529_msa_MET_exp_bxplt_300_weighted_keeps.tiff", p40.1.keeps, device = "tiff", path = "UAHS Outputs",
       scale = 1, width = 6.5, height = 8.2, dpi = 300,units = "in")



sum(ifelse(meta$poverty.f == "Below Poverty", meta$weight, 0), na.rm = T) / sum(meta$weight)

# percent of outdoor activites above 39 C
sum(ifelse(meta$temp > 39, meta$weight,0), na.rm = T) / sum(meta$weight)

# percentiles for travel durations
quantile(meta$TUACTDUR24[meta$code.f == "Travel"], probs = .9)
quantile(meta$TUACTDUR24[meta$code.f == "Travel"], probs = .5)
quantile(meta.f$TUACTDUR24[meta.f$code.f == "Travel"], probs = .9)
quantile(meta.f$TUACTDUR24[meta.f$code.f == "Travel"], probs = .5)

weighted.quantile(meta.f$MET.exp27[meta.f$outdoors == 1 & meta.f$temp > 27], w = meta.f$weight[meta.f$outdoors == 1 & meta.f$temp > 27], probs = 0.9 , na.rm = T)
#2563
length(meta.f$id[meta.f$MET.exp27 > 2563])

weighted.quantile(meta.f$MET.exp27[meta.f$outdoors == 1 & meta.f$temp > 27], w = meta.f$weight[meta.f$outdoors == 1 & meta.f$temp > 27], probs = 0.95 , na.rm = T)
#4352
length(meta.f$id[meta.f$MET.exp27 > 4352])

# fraction of work activities out of total outdoor activites
nrow(meta[meta$code.f2 == "Work",])
nrow(meta[meta$code.f2 == "Work",]) / nrow(meta) * 100

# frequency of travel above 27 C
nrow(meta.f[meta.f$code.f2 == "Travel" & meta.f$temp > 27,]) / nrow(meta.f[meta.f$temp > 27,]) * 100

# most common outdoor activities above 27 C
common.act <- as.data.frame(table(meta.f$TRCODEP))

wtd.cor(meta$sprawl[meta$TRCODEP == 20501], meta$TUACTDUR24[meta$TRCODEP == 20501])

#act.exp <- as.data.frame(table(meta$TRCODEP))

cor.test(metros$mean_lawn_time,metros$sprawl)

#Lawn, garden, and houseplant care
nrow(meta.f[meta.f$TRCODEP == 20501 & meta.f$temp > 27,]) / nrow(meta.f[meta.f$temp > 27,]) * 100

#walking
nrow(meta.f[meta.f$TRCODEP == 130131 & meta.f$temp > 27,]) / nrow(meta.f[meta.f$temp > 27,]) * 100

#Travel related to shopping (except grocery shopping)
nrow(meta.f[meta.f$TRCODEP == 180782 & meta.f$temp > 27,]) / nrow(meta.f[meta.f$temp > 27,]) * 100

#Travel related to socializing and communicating
nrow(meta.f[meta.f$TRCODEP == 181201 & meta.f$temp > 27,]) / nrow(meta.f[meta.f$temp > 27,]) * 100

#Care for animals and pets (not veterinary care)
nrow(meta.f[meta.f$TRCODEP == 20681 & meta.f$temp > 27,]) / nrow(meta.f[meta.f$temp > 27,]) * 100

# total activites above 27 C
nrow(meta.f[meta.f$temp > 27,])

nrow(meta.f[meta.f$temp > 33,]) / nrow(meta.f[meta.f$temp > 27,]) # percent activites above 33C
nrow(meta.f[meta.f$temp > 39,]) / nrow(meta.f[meta.f$temp > 27,]) # percent activites above 39C

meta.f$MET.exp27 <- ifelse(!is.na(meta.f$MET), meta.f$MET * meta.f$exp27, NA)

sum(meta.f$MET.dur[meta.f$temp > 33]) / sum(meta.f$MET.dur[meta.f$temp > 27]) # percent activity intenstiy above 33C
sum(meta.f$MET.dur[meta.f$temp > 39]) / sum(meta.f$MET.dur[meta.f$temp > 27]) # percent activity intenstiy above 39C

sum(meta.p$MET.exp27)
mean(meta.p$MET.exp27)


# quantile fuction
which.quantile <- function (x, probs, na.rm = FALSE){
  if (! na.rm & any (is.na (x)))
    return (rep (NA_integer_, length (probs)))
  
  o <- order (x)
  n <- sum (! is.na (x))
  o <- o [seq_len (n)]
  
  nppm <- n * probs - 0.5
  j <- floor(nppm)
  h <- ifelse((nppm == j) & ((j%%2L) == 0L), 0, 1)
  j <- j + h
  
  j [j == 0] <- 1
  o[j]
}


#time total
mean(meta.p$TUACTDUR24)
quantile(meta.p$TUACTDUR24, probs = .9)

#MET total
mean(meta.p$MET)
quantile(meta.p$MET, probs = .9)

#temp total
mean(meta.p$temp)
quantile(meta.p$temp, probs = .9)

#met-deg-min total
mean(meta.p$MET.exp27)
quantile(meta.p$MET.exp27, probs = .9)

# correlations
meta.cor <- meta.f[,c("temp", "TUACTDUR24", "MET")]
cor(meta.cor)




# ALL AGE PLOT
meta.f.p$age.all <- as.factor(meta.f.p$age)
meta.f.p$age.all <- factor(meta.f.p$age.all, levels = c("15","16","17","18","19",
                                                    "20","21","22","23","24","25","26","27","28","29",
                                                    "30","31","32","33","34","35","36","37","38","39",
                                                    "40","41","42","43","44","45","46","47","48","49",
                                                    "50","51","52","53","54","55","56","57","58","59",
                                                    "60","61","62","63","64","65","66","67","68","69",
                                                    "70","71","72","73","74","75","76","77","78","79",
                                                    "80","81","82","83","84","85"))

meta.f.p2 <- meta.f.p[meta.f.p$age <= 80,]

p27.9 <- ggplot(data=meta.f.p2, aes(x=age.all, y=MET.exp27)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "Exposure Intensity (MET-deg-min above 27 C per person per day)", labels = fmt_dcimals(1), limits=c(1,50000), breaks=c(1,5,10,50,100,500,1000,5000,10000,50000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  scale_x_discrete(name = "Age", limits = rev(levels(!is.na(meta.f.p2$age.all)))) +
  ggtitle("ATUS Metropolitan Outdoor MET-Exposure by Age Group") +
  theme_bw() +
  #geom_text(data=act.t, aes(x=cuttemp, y=1.1, label=n), family = my.font,
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = my.font,
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  #facet_grid(.~code.f2) +
  theme(plot.title = element_text(size = 12, family = my.font),
        text = element_text(size=12, family=my.font),
        axis.text.y=element_text(color="black",size=10),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.minor.y = element_line(colour="gray85"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = cols) +
  coord_flip()
p27.9



p28.9 <- ggplot(data=meta.f.p, aes(x=race.f2, y=MET.exp27)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "Exposure Intensity (MET-deg-min above 27 C per person-day", labels = fmt_dcimals(1), limits=c(1,50000), breaks=c(1,5,10,50,100,500,1000,5000,10000,50000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  scale_x_discrete(name = "Race") +
  ggtitle("ATUS Metropolitan Outdoor MET-Exposure by Race Group") +
  theme_bw() +
  #geom_text(data=act.t, aes(x=cuttemp, y=1.1, label=n), family = my.font,
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = my.font,
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  #facet_grid(.~code.f2) +
  theme(plot.title = element_text(size = 12, family = my.font),
        text = element_text(size=12, family=my.font),
        axis.title.y = element_text(size = 10),
        axis.text.y=element_text(color="black",size=11),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.minor.y = element_line(colour="gray85"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = cols)
p28.9


p29.9 <- ggplot(data=meta.f.p[!is.na(meta.f.p$income.f),], aes(x=income.f, y=MET.exp27)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "Exposure Intensity (MET-deg-min above 27 C per person-day", labels = fmt_dcimals(1), limits=c(1,50000), breaks=c(1,5,10,50,100,500,1000,5000,10000,50000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  scale_x_discrete(name = "Household Income") +
  ggtitle("ATUS Metropolitan Outdoor MET-Exposure by Income Group") +
  theme_bw() +
  #geom_text(data=act.t, aes(x=cuttemp, y=1.1, label=n), family = my.font,
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = my.font,
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  #facet_grid(.~code.f2) +
  theme(plot.title = element_text(size = 12, family = my.font),
        text = element_text(size=12, family=my.font),
        axis.title.y = element_text(size = 10),
        axis.text.y=element_text(color="black",size=11),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.minor.y = element_line(colour="gray85"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = cols)
p29.9


ggsave("20180529_MET_exp_by_age_bxplt_300.tiff", p27.9, device = "tiff", path = "UAHS Outputs",
       scale = 1, width = 6.5, height = 8.25, dpi = 300,units = "in")

ggsave("20180529_MET_exp_race_bxplt_300.tiff", p28.9, device = "tiff", path = "UAHS Outputs",
       scale = 1, width = 6.5, height = 5, dpi = 300,units = "in")

ggsave("20180529_MET_exp_inc_bxplt_300.tiff", p29.9, device = "tiff", path = "UAHS Outputs",
       scale = 1, width = 6.5, height = 5, dpi = 300,units = "in")


mean(meta.f$MET[meta.f$black_white == "Black"])
mean(meta.f$MET[meta.f$black_white == "Non-black"])

mean(meta.f$TUACTDUR24[meta.f$black_white == "Black"])
mean(meta.f$TUACTDUR24[meta.f$black_white == "Non-black"])

quantile(meta.f$MET[meta.f$black_white == "Black"], probs = .5)
quantile(meta.f$MET[meta.f$black_white == "Non-black"], probs = .5)

quantile(meta.f$TUACTDUR24[meta.f$black_white == "Black"], probs = .5)
quantile(meta.f$TUACTDUR24[meta.f$black_white == "Non-black"], probs = .5)


mean(meta.f$MET[meta.f$elderly == "15 to 65"])
mean(meta.f$MET[meta.f$elderly == "65 or Older"])

mean(meta.f$TUACTDUR24[meta.f$elderly == "15 to 65"])
mean(meta.f$TUACTDUR24[meta.f$elderly == "65 or Older"])

quantile(meta.f$MET[meta.f$elderly == "15 to 65"], probs = .5)
quantile(meta.f$MET[meta.f$elderly == "65 or Older"], probs = .5)

quantile(meta.f$TUACTDUR24[meta.f$elderly == "15 to 65"], probs = .5)
quantile(meta.f$TUACTDUR24[meta.f$elderly == "65 or Older"], probs = .5)


mean(meta.f$MET[!is.na(meta.f$poverty.f == "Below Poverty")])
mean(meta.f$MET[is.na(meta.f$poverty.f == "Above Poverty")])

mean(meta.f$TUACTDUR24[!is.na(meta.f$poverty.f == "Below Poverty")])
mean(meta.f$TUACTDUR24[is.na(meta.f$poverty.f == "Above Poverty")])

quantile(meta.f$MET[!is.na(meta.f$poverty.f == "Below Poverty")], probs = .5)
quantile(meta.f$MET[is.na(meta.f$poverty.f == "Above Poverty")], probs = .5)

quantile(meta.f$TUACTDUR24[!is.na(meta.f$poverty.f == "Below Poverty")], probs = .5)
quantile(meta.f$TUACTDUR24[is.na(meta.f$poverty.f == "Above Poverty")], probs = .5)


blacks.1 <- meta.f[meta.f$black_white == "Black" & meta.f$exp27 > 0,]
blacks.1 <- aggregate(MET.exp27~TRCODEP,data = blacks.1, FUN = sum)

nonblack.1 <-  meta.f[meta.f$black_white == "Non-black" & meta.f$exp27 > 0,]
nonblack.1 <- aggregate(MET.exp27~TRCODEP,data = nonblack.1, FUN = sum)


b.pov <- meta.f[meta.f$poverty.f == "Below Poverty" & meta.f$exp27 > 0,]
b.pov <- aggregate(MET.exp27~TRCODEP,data = b.pov, FUN = sum)

a.pov <-  meta.f[meta.f$poverty.f == "Above Poverty" & meta.f$exp27 > 0,]
a.pov <- aggregate(MET.exp27~TRCODEP,data = a.pov, FUN = sum)


# SI SOCIO FIGS

# Time outside hot temps by AGE
meta.f2 <- meta.f[!is.na(meta.f$age) & meta.f$age >= 18,] # remove NA ages
meta.f2$age.f2 <- NULL

# age group factor 2
meta.f2$age.f2[meta.f2$age >= 18 & meta.f2$age <= 25] <- "18-25"
meta.f2$age.f2[meta.f2$age > 25 & meta.f2$age <= 35] <- "26-35"
meta.f2$age.f2[meta.f2$age > 35 & meta.f2$age <= 45] <- "36-45"
meta.f2$age.f2[meta.f2$age > 45 & meta.f2$age <= 55] <- "46-55"
meta.f2$age.f2[meta.f2$age > 55 & meta.f2$age <= 65] <- "56-65"
meta.f2$age.f2[meta.f2$age > 65] <- "Over 65"
meta.f2$age.f2 <- as.factor(meta.f2$age.f2)


# add labelers for number of observations by cuttemp and age and for percent of observations outdoors
n.age.t <- plyr::ddply(.data=meta.f2, 
                 .(cuttemp,age.f2), 
                 summarize, 
                 n=length(age.f2))

p.age.t <- plyr::ddply(.data=meta, 
                 .(cuttemp,age.f2), 
                 summarize, 
                 p=length(age.f2))

#age.t <- merge(n.age.t,p.age.t,by=c('cuttemp','age.f2'))
#age.t$p <- paste("p =",sprintf("%0.2f", round(age.t$n/age.t$p,2)))
age.t <- n.age.t
age.t$n <- paste("n =",age.t$n)



p27 <- ggplot(meta.f2, aes(x=cuttemp, y=MET.dur, fill=cuttemp)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "Activity Intensity-time (MET-min)", labels = fmt_dcimals(1), limits=c(1,5000), breaks=c(1,5,10,50,100,500,1000,5000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  scale_x_discrete(name = "Apparent Temperature (deg C)") +
  ggtitle("ATUS Metropolitan Outdoor Exposure by Age Group") +
  theme_bw() +
  geom_text(data=age.t, aes(x=cuttemp, y=1.1, label=n), family = my.font,
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=0.25) +
  #geom_text(data=age.t, aes(x=cuttemp, y=1300, label=p), family = my.font,
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=1) +
  facet_grid(.~age.f2) +
  theme(plot.title = element_text(size = 11, family = my.font),
        text = element_text(size=12, family=my.font),
        axis.text.y=element_text(color="black",size=11),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray70"),
        panel.grid.minor.y = element_line(colour="gray75"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8.5),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = cols)
p27


# Time outside hot temps by INCOME
meta.f3 <- meta.f[!is.na(meta.f$income.f),] # create subset for only activities with non-NA income

# add labelers for number of observations by cuttemp and income and for percent of observations outdoors
n.income.t <- plyr::ddply(.data=meta.f3, 
                    .(cuttemp,income.f), 
                    summarize, 
                    n=length(income.f))

p.income.t <- plyr::ddply(.data=meta, 
                    .(cuttemp,income.f), 
                    summarize, 
                    p=length(income.f))

income.t <- merge(n.income.t,p.income.t,by=c('cuttemp','income.f'))
income.t$p <- paste("p =",sprintf("%0.2f", round(income.t$n/income.t$p,2)))
income.t$n <- paste("n =",income.t$n)
income.t <- income.t[!is.na(income.t$income.f),] # remove NAs

p28 <- ggplot(data=meta.f3, aes(x=cuttemp, y=MET.dur, fill=cuttemp)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "Activity Intensity-time (MET-min)", labels = fmt_dcimals(1), limits=c(1,5000), breaks=c(1,5,10,50,100,500,1000,5000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  scale_x_discrete(name = "Apparent Temperature (deg C)") +
  ggtitle("ATUS Metropolitan Outdoor Exposure by Income Group") +
  theme_bw() +
  geom_text(data=income.t, aes(x=cuttemp, y=1.1, label=n), family = my.font,
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=0) +
  #geom_text(data=income.t, aes(x=cuttemp, y=1300, label=p), family = my.font,
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=1) +
  facet_grid(.~income.f) +
  theme(plot.title = element_text(size = 11, family = my.font),
        text = element_text(size=12, family=my.font),
        axis.text.y=element_text(color="black",size=10),
        axis.text.x=element_text(color="black",size=10,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray70"),
        panel.grid.minor.y = element_line(colour="gray75"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8.5),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = cols)
p28

# RACE x EXP

# add labelers for number of observations by cuttemp and race and for percent of observations outdoors
n.race.t <- plyr::ddply(.data=meta.f, 
                  .(cuttemp,race.f2), 
                  summarize, 
                  n=length(race.f2))

p.race.t <- plyr::ddply(.data=meta, 
                  .(cuttemp,race.f2), 
                  summarize, 
                  p=length(race.f2))

race.t <- merge(n.race.t,p.race.t,by=c('cuttemp','race.f2'))
race.t$p <- paste("p =",sprintf("%0.2f", round(race.t$n/race.t$p,2)))
race.t$n <- paste("n =",race.t$n)
race.t <- race.t[!is.na(race.t$race.f2),]


p29 <- ggplot(data=meta.f, aes(x=cuttemp, y=MET.dur, fill=cuttemp)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "Activity Intensity-time (MET-min)", labels = fmt_dcimals(1), limits=c(1,5000), breaks=c(1,5,10,50,100,500,1000,5000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  scale_x_discrete(name = "Apparent Temperature (deg C)") +
  ggtitle("ATUS Metropolitan Outdoor Exposure by Race") +
  theme_bw() +
  geom_text(data=race.t, aes(x=cuttemp, y=1.1, label=n),family = my.font,
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=.25) +
  #geom_text(data=race.t, aes(x=cuttemp, y=1300, label=p), family = my.font,
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=1) +
  facet_grid(.~race.f2) +
  theme(plot.title = element_text(size = 11, family = my.font),
        text = element_text(size=11, family=my.font),
        #axis.title = element_text(face="bold"),
        axis.text.y=element_text(color="black",size=10),
        axis.text.x=element_text(color="black",size=10,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray70"),
        panel.grid.minor.y = element_line(colour="gray75"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8.5),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = cols)
p29

# Final exposure boxplots

# age
ggsave("20180529_exp_by_age_bxplt_300.tiff", p27, device = "tiff", path = "UAHS Outputs",
       scale = 1, width = 6.5, height = 5, dpi = 300,units = "in")

# income
ggsave("20180529_exp_by_inc_bxplt_300.tiff", p28, device = "tiff", path = "UAHS Outputs",
       scale = 1, width = 6.5, height = 5, dpi = 300,units = "in")

# race
ggsave("20180529_exp_by_race_bxplt_300.tiff", p29, device = "tiff", path = "UAHS Outputs",
       scale = 1, width = 6.5, height = 5, dpi = 300,units = "in")

# factoring climate zones
metros$BA.Climate.Zone <- factor(metros$BA.Climate.Zone, levels = c("Hot-Humid","Hot-Dry","Mixed-Humid","Mixed-Dry","Cold","Marine"))


# PAPER SUMMARIES
rbind(
paste( formatC(nrow(meta), big.mark = ","), "total activities (all 11.5 yrs, all 50 MSAs, all temps, all locations)" ),
paste( formatC(length(unique(meta$id)), big.mark = ","), "total respondents (all 11.5 yrs, all 50 MSAs, all temps, all locations)" ),
paste( round( nrow(meta) / length(unique(meta$id)) , digits = 1 ), "avergage activies per person (all 11.5 yrs, all 50 MSAs, all temps, all locations)" ),
paste0(round(sum(meta$TUACTDUR24[meta$outdoors == 1], na.rm = T) / sum(meta$TUACTDUR24, na.rm = T), digits = 4) * 100,"% of total time is spent outdoors by all respondents in all weather (all 11.5 yrs, all 50 MSAs, all temps, all locations)"),
paste0(round(sum(meta$TUACTDUR24[meta$outdoors == 1] * meta$weight[meta$outdoors == 1], na.rm = T)  / sum(meta$TUACTDUR24 * meta$weight, na.rm = T), digits = 4) * 100,"% (weighted) of total time is spent outdoors by all respondents in all weather (all 11.5 yrs, all 50 MSAs, all temps, all locations)"),
paste( formatC(length(meta$outdoors[meta$outdoors > 0 & !is.na(meta$outdoors)]), big.mark = ","), "total outdoor actitivies (all 11.5 yrs, all 50 MSAs, all temps, outdoors)" ),
paste( formatC(length(unique(meta$id[meta$outdoors > 0 & !is.na(meta$outdoors)])), big.mark = ","), "total respondents w/ at least one outdoor activity (all 11.5 yrs, all 50 MSAs, all temps, outdoors)" ),
paste( formatC(length(unique(meta$id[meta$outdoors > 0 & !is.na(meta$outdoors)])) / length(unique(meta$id)), big.mark = ","), "% of respondents w/ at least one outdoor activity (all 11.5 yrs, all 50 MSAs, all temps, outdoors)" ),
paste0(formatC(sum(meta$TUACTDUR24[meta$temp > 27 & !is.na(meta$temp)] / 60, na.rm = T), big.mark = ",")," total hrs spent outdoors above 27 C Ta (all 11.5 yrs, all 50 MSAs, > 27 C Ta, outdoors)"),
paste0(round(sum(meta$TUACTDUR24[meta$temp > 27 & !is.na(meta$temp)], na.rm = T) / sum(meta$TUACTDUR24, na.rm = T), digits = 4) * 100,"% of total time is spent outdoors above 27 C Ta (all 11.5 yrs, all 50 MSAs, > 27 C Ta, outdoors)"),
paste0(round(sum(meta$TUACTDUR24[meta$temp > 27 & !is.na(meta$temp)] * meta$weight[meta$temp > 27 & !is.na(meta$temp)], na.rm = T) / sum(meta$TUACTDUR24 * meta$weight, na.rm = T), digits = 4) * 100,"% (weighted) of total time is spent outdoors above 27 C Ta (all 11.5 yrs, all 50 MSAs, > 27 C Ta, outdoors)")

)

# LA TAC SUMMARIES
meta.la <- meta[meta$msa.f == "LA" | meta$msa.f == "RIV",]
rbind(
  paste( formatC(nrow(meta.la), big.mark = ","), "total activities (all 11.5 yrs, LA & RIV MSAs, all temps, all locations)" ),
  paste( formatC(length(unique(meta.la$id)), big.mark = ","), "total respondents (all 11.5 yrs, LA & RIV MSAs, all temps, all locations)" ),
  paste( round( nrow(meta.la) / length(unique(meta.la$id)) , digits = 1 ), "avergage activies per person (all 11.5 yrs, LA & RIV MSAs, all temps, all locations)" ),
  paste0(round(sum(meta.la$TUACTDUR24[meta.la$outdoors == 1], na.rm = T) / sum(meta.la$TUACTDUR24, na.rm = T), digits = 4) * 100,"% of total time is spent outdoors by all respondents in all weather (all 11.5 yrs, LA & RIV MSAs, all temps, all locations)"),
  paste0(round(sum(meta.la$TUACTDUR24[meta.la$outdoors == 1] * meta.la$weight[meta.la$outdoors == 1], na.rm = T)  / sum(meta.la$TUACTDUR24 * meta.la$weight, na.rm = T), digits = 4) * 100,"% (weighted) of total time is spent outdoors by all respondents in all weather (all 11.5 yrs, LA & RIV MSAs, all temps, all locations)"),
  paste( formatC(length(meta.la$outdoors[meta.la$outdoors > 0 & !is.na(meta.la$outdoors)]), big.mark = ","), "total outdoor actitivies (all 11.5 yrs, LA & RIV MSAs, all temps, outdoors)" ),
  paste( formatC(length(unique(meta.la$id[meta.la$outdoors > 0 & !is.na(meta.la$outdoors)])), big.mark = ","), "total respondents w/ at least one outdoor activity (all 11.5 yrs, LA & RIV MSAs, all temps, outdoors)" ),
  paste( formatC(length(unique(meta.la$id[meta.la$outdoors > 0 & !is.na(meta.la$outdoors)])) / length(unique(meta.la$id)), big.mark = ","), "% of respondents w/ at least one outdoor activity (all 11.5 yrs, LA & RIV MSAs, all temps, outdoors)" ),
  paste0(formatC(sum(meta.la$TUACTDUR24[meta.la$temp > 27 & !is.na(meta.la$temp)] / 60, na.rm = T), big.mark = ",")," total hrs spent outdoors above 27 C Ta (all 11.5 yrs, LA & RIV MSAs, > 27 C Ta, outdoors)"),
  paste0(formatC(sum(meta.la$TUACTDUR24[meta.la$temp > 33 & !is.na(meta.la$temp)] / 60, na.rm = T), big.mark = ",")," total hrs spent outdoors above 33 C Ta (all 11.5 yrs, LA & RIV MSAs, > 33 C Ta (extreme caution), outdoors)"),
  paste0(formatC(length(unique(meta.la$id[meta.la$temp > 33 & !is.na(meta.la$temp)] / 60, na.rm = T)), big.mark = ",")," total ppl  outdoors above 33 C Ta (all 11.5 yrs, LA & RIV MSAs, > 33 C Ta (extreme caution), outdoors)"),
  paste0(round(sum(meta.la$TUACTDUR24[meta.la$temp > 27 & !is.na(meta.la$temp)], na.rm = T) / sum(meta.la$TUACTDUR24, na.rm = T), digits = 4) * 100,"% of total time is spent outdoors above 27 C Ta (all 11.5 yrs, LA & RIV MSAs, > 27 C Ta, outdoors)"),
  paste0(round(sum(meta.la$TUACTDUR24[meta.la$temp > 27 & !is.na(meta.la$temp)] * meta.la$weight[meta.la$temp > 27 & !is.na(meta.la$temp)], na.rm = T) / sum(meta.la$TUACTDUR24 * meta.la$weight, na.rm = T), digits = 4) * 100,"% (weighted) of total time is spent outdoors above 27 C Ta (all 11.5 yrs, LA & RIV MSAs, > 27 C Ta, outdoors)")
  
)

min(meta$temp[meta$outdoors > 0], na.rm = T)
max(meta$temp[meta$outdoors > 0], na.rm = T)

# ALL MSAs
length(meta$TUACTDUR24[meta$temp > 27 & !is.na(meta$temp)])
length(unique(meta.la$id[meta.la$temp > 27]))

# LA & RIV
length(meta.la$TUACTDUR24[meta.la$temp > 27 & !is.na(meta.la$temp)])
length(unique(meta.la$id[meta.la$temp > 27]))


# LOOKING AT "LAWN, GARDEN, & HOUSEPLANT CARE"

weighted.quantile(meta.s$TUACTDUR24[meta.s$TRCODEP == 20501], meta.s$weight[meta.s$TRCODEP == 20501], probs=c(0.05,0.5,0.95)) # MET.dur 10th, 50th, and 90th percentiles
quantile(meta.s$TUACTDUR24[meta.s$TRCODEP == 20501], probs=c(0.05,0.5,0.95))

# IF every 'lawn, garden, houseplant care' activity included all time <= 20 min as houselplant care, how much exposure would be reduced?
x <- sum(meta.f27$MET.exp27[meta.f27$TRCODEP == 20501 & meta.f27$TUACTDUR24 <= 20], na.rm = T)
y <- sum((meta.f27$temp[meta.f27$TRCODEP == 20501 & meta.f27$TUACTDUR24 > 20] - 27) * 20 * meta.f27$MET[meta.f27$TRCODEP == 20501 & meta.f27$TUACTDUR24 > 20], na.rm = T)
z <- sum(meta.f27$MET.exp27[meta.f27$TRCODEP == 20501], na.rm = T) 

paste0(round(1 - (x + y) / z, digits = 2) * 100,"% of total exposure would be from 'lawn & garden care' ")

# what if half of all "lawn, garden, & houseplant care" activies dedicated 10 minutes of thier time explicity to (indoor) care of houseplants (or all time if < 10 min), then
p.act <- 1
t.time <- 20
x <- sum(meta.f27$MET.exp27[meta.f27$TRCODEP == 20501 & meta.f27$TUACTDUR24 <= t.time], na.rm = T) * p.act 
y <- sum((meta.f27$temp[meta.f27$TRCODEP == 20501 & meta.f27$TUACTDUR24 > t.time] - 27) * t.time * meta.f27$MET[meta.f27$TRCODEP == 20501 & meta.f27$TUACTDUR24 > t.time], na.rm = T) * p.act
z <- sum(meta.f27$MET.exp27[meta.f27$TRCODEP == 20501], na.rm = T) 

paste0(round(1 - (x + y) / z, digits = 2) * 100,"% of total exposure would be from 'lawn & garden care' ")

# TEST NEW MODEL FOR EXP ~ SPRAWL
sprawl.exp.model.all <- lm(formula = meta.f27.agg$MET.exp27.trns ~  meta.f27.agg$sprawl 
                           + meta.f27.agg$BA.Climate.Zone 
                           + meta.f27.agg$race.f2 
                           + meta.f27.agg$income.f 
                           + meta.f27.agg$age.f2 
                           + meta.f27.agg$season, 
                           weights = meta.f27.agg$per_out_n_27)
summary(sprawl.exp.model.all)

meta.f27.agg <- merge(meta.f27.agg, metros[,c("abbr","BA.Climate.Zone","per_out_n_27")], by.x = "msa.f", by.y = "abbr", all.x = T) 

felm_wtg = felm(MET.exp27.trns ~ sex.f + race.f2 + sprawl + BA.Climate.Zone +  0 | date , data = meta.f27.agg, weights = meta.f27.agg$weight)
summary(felm_wtg)

meta.f27.agg$male <- ifelse(meta.f27.agg$sex.f == "Male", 1, 0)
meta.f27.agg$asian <- ifelse(meta.f27.agg$race.f2 == "Asian", 1, 0)
meta.f27.agg$black <- ifelse(meta.f27.agg$race.f2 == "Black", 1, 0)
meta.f27.agg$pac.isle <- ifelse(meta.f27.agg$race.f2 == "Pacific Islander", 1, 0)
meta.f27.agg$mixed.humid <- ifelse(meta.f27.agg$BA.Climate.Zone == "Mixed-Humid", 1, 0)
meta.f27.agg$cold <- ifelse(meta.f27.agg$BA.Climate.Zone == "Cold", 1, 0)
meta.f27.agg$marine <- ifelse(meta.f27.agg$BA.Climate.Zone == "Marine", 1, 0)
meta.f27.agg$hot.humid <- ifelse(meta.f27.agg$BA.Climate.Zone == "Hot-Humid", 1, 0)
meta.f27.agg$hot.dry <- ifelse(meta.f27.agg$BA.Climate.Zone == "Hot-Dry", 1, 0)
meta.f27.agg$age25_34 <- ifelse(meta.f27.agg$age.f2 == "25-34", 1, 0)
meta.f27.agg$age55_64 <- ifelse(meta.f27.agg$age.f2 == "55-64", 1, 0)
meta.f27.agg$age65_over <- ifelse(meta.f27.agg$age.f2 == "65 and over", 1, 0)
meta.f27.agg$low.inc <- ifelse(meta.f27.agg$income.f == "Less than $15k", 1, 0)

meta.f27.agg$age.f2 <- relevel(meta.f27.agg$age.f2, ref = "35-44")
meta.f27.agg$race.f2 <- relevel(meta.f27.agg$race.f2, ref = "White")
meta.f27.agg$income.f <- relevel(meta.f27.agg$income.f, ref = "$50k to $75k")

# create only 4 race groupings
meta.f27.agg$race.f3 <- ifelse(meta.f27.agg$race.f2 == "White" | meta.f27.agg$race.f2 == "Black" | meta.f27.agg$race.f2 == "Asian", meta.f27.agg$race.f2, 7)
meta.f27.agg$race.f3 <- ifelse(meta.f27.agg$race.f3 == 3, 2, meta.f27.agg$race.f3) # asian
meta.f27.agg$race.f3 <- ifelse(meta.f27.agg$race.f3 == 4, 3, meta.f27.agg$race.f3) # black
meta.f27.agg$race.f3 <- ifelse(meta.f27.agg$race.f3 == 7, 4, meta.f27.agg$race.f3) # other
meta.f27.agg$race.f3 <- factor(meta.f27.agg$race.f3, levels = c(1:4), labels = c("White","Asian","Black","Other"))

# weighted fixed effects linear model on normalized exposure
AIC_adj <- function(mod){
  # Number of observations
  n.N   <- mod$N
  # Residuals vector
  u.hat <- residuals(mod)
  # Variance estimation
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  # Number of parameters (incl. constant) + one additional for variance estimation
  p     <-  length(coef(mod)) + 1
  
  # Note: minus sign cancels in log likelihood
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  
  return(aic)
} # function to return AIC for felm outputs

AIC_adj(felm(log(MET.exp27 + 1) ~ sex.f + race.f2 + sprawl + BA.Climate.Zone + 0, data = meta.f27.agg, weights = meta.f27.agg$weight)) # base w/ no fixed effects
AIC_adj(felm(log(MET.exp27 + 1) ~ sex.f + race.f2 + sprawl + BA.Climate.Zone + 0 | msa.f, data = meta.f27.agg, weights = meta.f27.agg$weight))  # using msa fixed improves
AIC_adj(felm(log(MET.exp27 + 1) ~ sex.f + race.f2 + sprawl + 0 | BA.Climate.Zone, data = meta.f27.agg, weights = meta.f27.agg$weight)) # climate zone as fixed effect doesn't change
AIC_adj(felm(log(MET.exp27 + 1) ~ sex.f + race.f2 + sprawl + BA.Climate.Zone + 0 | season + msa.f, data = meta.f27.agg, weights = meta.f27.agg$weight))  # more improves w season
AIC_adj(felm(log(MET.exp27 + 1) ~ sex.f + race.f2 + sprawl  + 0 | date + BA.Climate.Zone, data = meta.f27.agg, weights = meta.f27.agg$weight))  # using msa fixed improves
AIC_adj(felm(log(MET.exp27 + 1) ~ sex.f + race.f2 + sprawl + 0 | BA.Climate.Zone, data = meta.f27.agg, weights = meta.f27.agg$weight))  # using msa fixed improves

# exlude non-significant vars
AIC_adj(felm(log(MET.exp27 + 1) ~ sprawl + 0 | date + msa.f + race.f2 + BA.Climate.Zone, data = meta.f27.agg, weights = meta.f27.agg$weight))
AIC_adj(felm(log(MET.exp27 + 1) ~ male + asian + black + sprawl + 0 | date + msa.f, data = meta.f27.agg, weights = meta.f27.agg$weight))
AIC_adj(felm(log(MET.exp27 + 1) ~ sex.f + race.f2 + sprawl + 0 | date + msa.f, data = meta.f27, weights = meta.f27$weight))

AIC_adj(felm(log(MET.exp27 + 1) ~  sprawl + male + black + asian + age25_34 + age65_over | date + BA.Climate.Zone, data = meta.f27.agg, weights = meta.f27.agg$weight))
AIC_adj(felm(log(MET.exp27 + 1) ~  male + black + asian + age25_34 + age65_over | date + BA.Climate.Zone + msa.f, data = meta.f27.agg, weights = meta.f27.agg$weight))

summary(felm(log(MET.exp27 + 1) ~  sprawl + male + black + asian + age25_34 + age65_over | date + BA.Climate.Zone, data = meta.f27.agg, weights = meta.f27.agg$weight))
summary(felm(log(MET.exp27 + 1) ~  male + black + asian + age25_34 + age65_over | date + BA.Climate.Zone + msa.f, data = meta.f27.agg, weights = meta.f27.agg$weight))

#felm1 <- (felm(log(MET.exp27 + 1) ~  sex.f + race.f2 + income.f + age.f2 | date + season:msa.f | 0  | season + msa.f, data = meta.f27.agg[meta.f27.agg$msa.f != "SF",], weights = meta.f27.agg$weight[meta.f27.agg$msa.f != "SF"], exactDOF = T))
#felm2 <- (felm(log(MET.exp27 + 1) ~  sex.f + race.f2 + income.f + age.f2 | date + season + msa.f   | 0  | season + msa.f, data = meta.f27.agg[meta.f27.agg$msa.f != "SF",], weights = meta.f27.agg$weight[meta.f27.agg$msa.f != "SF"], exactDOF = T))
felm1 <- (felm(log(MET.exp27+1) ~  msa.f | BA.Climate.Zone + season | 0 | season, data = meta.f27.agg, weights = meta.f27.agg$weight, exactDOF = T))
felm2 <- (felm(log(MET.exp27+1) ~  sex.f + race.f3 + income.f + age.f2  | date + msa.f + BA.Climate.Zone + season | 0  | season + msa.f, data = meta.f27.agg, weights = meta.f27.agg$weight, exactDOF = T))

#felm2 <- (felm(log(MET.exp27 + 1) ~  male + black + pac.isle + low.inc + age25_34 | date + BA.Climate.Zone + msa.f + msa.f:season | 0  | season + msa.f, data = meta.f27.agg, weights = meta.f27.agg$weight, exactDOF = T))
summary(felm1)
summary(felm2)

AIC_adj(felm1)
AIC_adj(felm2)

# Xu 2003 R^2
1 - var(felm1$residuals) / var(felm1$response)
1 - var(felm2$residuals) / var(felm2$response)

# Nakagawa & Schielzeth's (2013)
r.squaredGLMM(felm2)

# format felm output to export
felm.out <- as.data.frame(cbind(c("Female","Asian","Black","Other","Less than $15k","$15k to $30k","$30k to $50k","$75k to $100k","$100k and over","15-24","25-34","45-54","55-64","65 and over"),
                                as.numeric(felm2$coefficients), as.numeric(felm2$cse), as.numeric(felm2$cpval), 
                                as.numeric(c(weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$sex.f == "Female"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$race.f3 == "Asian"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$race.f3 == "Black"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$race.f3 == "Other"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$income.f == "Less than $15k"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$income.f == "$15k to $30k"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$income.f == "$30k to $50k"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$income.f == "$75k to $100k"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$income.f == "$100k and over"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$age.f2 == "15-24"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$age.f2 == "25-34"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$age.f2 == "45-54"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$age.f2 == "55-64"]),
                                             weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$age.f2 == "65 and over"]))),
                                as.numeric(c(nrow(meta.f27.agg[meta.f27.agg$sex.f == "Female",]),
                                             nrow(meta.f27.agg[meta.f27.agg$race.f3 == "Asian",]),
                                             nrow(meta.f27.agg[meta.f27.agg$race.f3 == "Black",]),
                                             nrow(meta.f27.agg[meta.f27.agg$race.f3 == "Other",]),
                                             nrow(meta.f27.agg[meta.f27.agg$income.f == "Less than $15k",]),
                                             nrow(meta.f27.agg[meta.f27.agg$income.f == "$15k to $30k",]),
                                             nrow(meta.f27.agg[meta.f27.agg$income.f == "$30k to $50k",]),
                                             nrow(meta.f27.agg[meta.f27.agg$income.f == "$75k to $100k",]),
                                             nrow(meta.f27.agg[meta.f27.agg$income.f == "$100k and over",]),
                                             nrow(meta.f27.agg[meta.f27.agg$age.f2 == "15-24",]),
                                             nrow(meta.f27.agg[meta.f27.agg$age.f2 == "25-34",]),
                                             nrow(meta.f27.agg[meta.f27.agg$age.f2 == "45-54",]),
                                             nrow(meta.f27.agg[meta.f27.agg$age.f2 == "55-64",]),
                                             nrow(meta.f27.agg[meta.f27.agg$age.f2 == "65 and over",]
                                )))))
felm.out[, c(2:6)] <- sapply(felm.out[, c(2:6)], as.character)
felm.out[, c(2:6)] <- sapply(felm.out[, c(2:6)], as.numeric)
colnames(felm.out) <- c("variable","log.est", "c.se", "p.val", "w.mean","n")
felm.out$norm.per.chng <- signif((exp(felm.out$log.est) - 1) * 100, digits = 3)
felm.out$CI.L <- signif(exp(felm.out$log.est-(1.96*felm.out$c.se)) - 1, digits = 3) * 100
felm.out$CI.U <- signif(exp(felm.out$log.est+(1.96*felm.out$c.se)) - 1, digits = 3) * 100
felm.out$per.CI <- paste0(felm.out$norm.per.chng, "% (", felm.out$CI.L,"%, ", felm.out$CI.U,"%)")
felm.out$p.val <- ifelse(signif(felm.out$p.val, digits = 3) <= 0.001, paste0("< 0.001"), paste0(signif(felm.out$p.val, digits = 3)))

write.csv(felm.out,paste0("UAHS Outputs/",today(),"_felm_final_output.csv"))

nrow(meta.f27.agg[meta.f27.agg$sex.f == "Male",])
nrow(meta.f27.agg[meta.f27.agg$race.f3 == "White",])
nrow(meta.f27.agg[meta.f27.agg$age.f2 == "35-44",])
nrow(meta.f27.agg[meta.f27.agg$income.f == "$50k to $75k",])

######################

meta.f27.agg$date <- as.Date(meta.f27.agg$date, format = "%Y%m%d")
meta.f27.agg$n.weight <- meta.f27.agg$weight / mean(meta.f27.agg$weight)


# elderly expousre lawn and garden percent
sum(meta.f$MET.exp27[meta.f$TRCODEP == 20501 & meta.f$age.f2 == "65 and over"], na.rm = T) / sum(meta.f$MET.exp27[meta.f$age.f2 == "65 and over"], na.rm = T)
sum(meta.f$MET.exp27[meta.f$TRCODEP == 20501 & meta.f$age.f2 != "65 and over"], na.rm = T) / sum(meta.f$MET.exp27[meta.f$age.f2 != "65 and over"], na.rm = T)

nrow(meta.f[meta.f$TRCODEP == 20501 & meta.f$age.f2 == "65 and over",]) / nrow(meta.f[meta.f$age.f2 == "65 and over",])
nrow(meta.f[meta.f$TRCODEP == 20501 & meta.f$age.f2 != "65 and over",]) / nrow(meta.f[meta.f$age.f2 != "65 and over",])


# black expousre lawn and garden percent
sum(meta.f$MET.exp27[meta.f$TRCODEP == 20501 & meta.f$race.f2 == "Black"], na.rm = T) / sum(meta.f$MET.exp27[meta.f$race.f2 == "Black"], na.rm = T)
sum(meta.f$MET.exp27[meta.f$TRCODEP == 20501 & meta.f$race.f2 != "Black"], na.rm = T) / sum(meta.f$MET.exp27[meta.f$race.f2 != "Black"], na.rm = T)

sum(meta.f$TUACTDUR24[meta.f$code.f2 == "Work" & meta.f$age.f2 == "25-34"], na.rm = T) / sum(meta.f$TUACTDUR24[meta.f$age.f2 == "25-34"], na.rm = T)
sum(meta.f$TUACTDUR24[meta.f$code.f2 == "Work" & meta.f$age.f2 != "25-34"], na.rm = T) / sum(meta.f$TUACTDUR24[meta.f$age.f2 != "25-34"], na.rm = T)


# CREATE EXPOSURE x DAY PLOT
meta.f.pop2 <- merge(meta.f.pop2,meta.f[,c("id","date")], by = "id")
meta.f.pop2$date <- as.Date(meta.f.pop2$date, format = "%Y%m%d")
meta.f.pop2$day <- weekdays(meta.f.pop2$date)
meta.f.pop2$day <- factor(meta.f.pop2$day, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
meta.f$date <- as.Date(meta.f$date, format = "%Y%m%d")
meta.f$day <- weekdays(meta.f$date)
meta.f$day <- factor(meta.f$day, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
day.t <- plyr::ddply(.data=meta.f[!is.na(meta.f$MET.dur),], 
               .(cuttemp,day), 
               plyr::summarize, 
               n=length(day))

day.t$n <- paste("n =",day.t$n)
day.t <- day.t[!is.na(day.t$day),]

# create activity x MET-time by cuttemp plot
p.day <- ggplot(data=meta.f.pop2[!is.na(meta.f.pop2$MET.dur),], aes(x=cuttemp, y=MET.dur, fill=cuttemp)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "Activity Intensity-time (MET-min)", labels = fmt_dcimals(1), limits=c(1,5000), breaks=c(1,5,10,50,100,500,1000,5000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  scale_x_discrete(name = "Apparent Temperature (?C)") +
  ggtitle("") +
  theme_bw() +
  geom_text(data=day.t, aes(x=cuttemp, y=1.1, label=n), family = my.font,
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = my.font,
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  facet_grid(.~day) +
  theme(plot.title = element_text(size = 12, family = my.font),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        text = element_text(size=12, family=my.font),
        axis.text.y=element_text(color="black",size=11),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.minor.y = element_line(colour="gray85"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = cols)
p.day

ggsave(paste0(today(),"_MET_exp_by_day_bxplt_300_weighted.tiff"), p.day, device = "tiff", path = "UAHS Outputs",
       scale = 1, width = 6.5, height = 5, dpi = 300,units = "in")

# weighted N engaging above 90th perctile exposure (4147 days in sample)
nrow(meta[meta$MET.exp27 > 0 & !is.na(meta$MET.exp27),])

sum(ifelse(meta$MET.exp27 > 0 & !is.na(meta$MET.exp27), meta$weight, 0)) / 4147
sum(ifelse(meta$MET.exp27[meta$msa.f == "LA" | meta$msa.f == "RIV"] > 0 & !is.na(meta$MET.exp27[meta$msa.f == "LA" | meta$msa.f == "RIV"]), meta$weight[meta$msa.f == "LA" | meta$msa.f == "RIV"], 0)) / 4147

# weighted people LA metro engage in extreme caution heat index or above
sum(ifelse(meta$temp.F[meta$msa.f == "LA" | meta$msa.f == "RIV"] > 90 & !is.na(meta$MET.exp27[meta$msa.f == "LA" | meta$msa.f == "RIV"]), meta$weight[meta$msa.f == "LA" | meta$msa.f == "RIV"], 0)) / 4147


# LA metro region exposure 

# percent of all people 
(sum(ifelse(meta$MET.exp27 > 0 & !is.na(meta$MET.exp27), meta$weight, 0)) / 4147) / sum(metros$respop72016)
(sum(ifelse(meta$MET.exp27[meta$msa.f == "LA" | meta$msa.f == "RIV"] > 0 & !is.na(meta$MET.exp27[meta$msa.f == "LA" | meta$msa.f == "RIV"]), meta$weight[meta$msa.f == "LA" | meta$msa.f == "RIV"], 0)) / 4147) / sum(metros$respop72016[metros$abbr == "LA" | metros$abbr == "RIV"])
