
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

# load cleaned ATUS data
atus1 <- readRDS(here("data/export/atus1.rds"))
atus2 <- readRDS(here("data/export/atus2.rds"))
atus <- rbind(atus1,atus2)
rm(atus1,atus2)

# load ATUS weights
atuswgts1 <- readRDS(here("data/import/atuswgts1.rds"))
atuswgts2 <- readRDS(here("data/import/atuswgts2.rds"))
atuswgts3 <- readRDS(here("data/import/atuswgts3.rds"))
atuswgts4 <- readRDS(here("data/import/atuswgts4.rds"))
atuswgts <- rbind(atuswgts1,atuswgts2,atuswgts3,atuswgts4)
rm(atuswgts1,atuswgts2,atuswgts3,atuswgts4)
names(atuswgts)[names(atuswgts) == 'TUCASEID'] <- 'id' # change id col to match rest 

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



# split data.table ***WORKING***
#dt.split.saveRDS <- function(dt, splits){
#  if(class(dt) != "data.table") {
#    dt <- as.data.table(dt)
#  }
#  r <- floor(dt[, .N]/splits)
#}

# function to return p value
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

  



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

#~<@>~~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~#
#     Summary Weather statsictics      #
#~<@>~~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~#

dir.t <- file.path("NCDC Global Hourly weather data","metros")

# Import formatted weather data files by MSA code 
for(i in 1:dim(metros)[1]){
  df <- read.csv(file = file.path(dir.t,metros$FileOut[i]), header = T)
  assign(paste0("tmp",metros$MSA_code[i]), df)
}

# Loop through temperature files and re adjust AT using NWS forumula
for(x in 1:dim(metros)[1]){
  df <- get(paste0("tmp",metros$MSA_code[x]))   # call temperature subset for MSA
  df$temp <- df$TEMP
  # Calc relative humidity
  df$rh <- exp((17.625*df$DEWPC)/(243.04+df$DEWPC))/exp((17.625*df$TEMPC)/(243.04+df$TEMPC))
  df$rh <- df$rh*100
  df$A <- -10.3 + 1.1*df$TEMP + 0.047*df$rh
  df$B <- (-42.379 + 2.04901523*df$TEMP
           + 10.14333127*df$rh - 0.22475541*df$TEMP*df$rh 
           - 0.00683783*df$TEMP*df$TEMP - 0.05481717*df$rh*df$rh 
           + 0.00122874*df$TEMP*df$TEMP*df$rh + 0.00085282*df$TEMP*df$rh*df$rh 
           - 0.00000199*df$TEMP*df$TEMP*df$rh*df$rh)
  df$heat <- ifelse(df$TEMP <= 40, df$TEMP,
                    ifelse(df$A < 79, df$A,
                           ifelse(df$rh <= 0.13 & df$TEMP >= 80 & df$TEMP <= 112, df$B - ((13-df$rh)/4)*SQRT((17-ABS(df$TEMP-95.))/17),
                                  ifelse(df$rh > 0.85 & df$TEMP >= 80 & df$TEMP <= 87, df$B +0.02*(df$rh - 85)*(87 - df$TEMP), df$B))))


  df$heat2 <- 0.5 * (df$TEMP + 61 + ((df$TEMP - 68)*1.2) + (df$rh*0.094))
  df$heat2 <- (df$TEMP + df$heat2)/2
  df$temp <- ifelse(df$heat2 >= 80, df$heat, df$heat2)   # use temp if app temp, use heat if heat index (because 'heat' only overwrote 'temp').
  df$temp <- (df$temp - 32) * 5/9 # convert to Celcius
  assign(paste0("tmp",metros$MSA_code[x]), df)
}

#############

# Loop through assigning summary temp statistics
for(j in 1:dim(metros)[1]){
  tmp <- get(paste0("tmp",metros$MSA_code[j]))   # call temperature subset for MSA
  tmp$YrMth <- format(strptime(tmp$DateTime, format="%Y-%m-%d %H:%M:%S"), format="%Y%m")  # reformat Year-Month to YYYYMM
  df <- get(metros$abbr[j])              # call MSA df 
  df.o <- subset(df, outdoors == 100)
  minmonth <- min(df$YrMth)              # use called MSA df to find the min month 
  tmp <- subset(tmp, YrMth >= minmonth)  # subset to min month 
  metros$Tmin[j]  <- min(tmp$TEMP)                   # Min houlry obs temp over sample
  metros$T10[j] <- quantile(tmp$TEMP, probs = .1)    # 10th percentile hourly obs temp over sample 
  metros$T05[j] <- quantile(tmp$TEMP, probs = .05)    # 5th percentile hourly obs temp over sample
  metros$Tmax[j]  <- max(tmp$TEMP)                   # Max houlry obs temp over sample
  metros$T90[j] <- quantile(tmp$TEMP, probs = .9)    # 90th percentile hourly obs temp over sample 
  metros$T95[j] <- quantile(tmp$TEMP, probs = .95)    # 95th percentile hourly obs temp over sample
  metros$ATmax[j] <- (max(tmp$AT) * 1.8) + 32                # Max hourly app temp over sample
  metros$AT90[j] <- (quantile(tmp$AT, probs = .9) * 1.8) + 32     # 90th percentile hourly app temp over sample 
  metros$AT95[j] <- (quantile(tmp$AT, probs = .95) * 1.8) + 32     # 95th percentile hourly app temp over sample

}

rm(list = ls(pattern = "^tmp"))      # remove rest of temporary objects by removing all that start with "tmp"
rm(list = ls(pattern = "^df"))       # remove rest of temporary objects by removing all that start with "df"
gc()
##################################################

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
meta.s <- meta[,c('id','MET.dur','MET.exp27','exp27','MET',"TRCODEP","TUACTDUR24",'temp','cuttemp','code.f2','weight','age.f2','age','race.f2','sex.f','poverty.f','elderly','black_white','outdoors','sprawl','income.f','date','season','msa.f')]
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

# create summary stats on SES and exposure
summary.ses <- data.frame(matrix(vector(mode = 'numeric',length = 296), nrow = 4, ncol = 296))

# name columns
colnames(summary.ses) <- c('n_act','n_per','N_per',                                                                            # n totals
                           'n_poverty','n_non_pov',                                                                            # n poverty
                           'n_inc_less15k','n_inc_15to30k','n_inc_30to50k','n_inc_50to75k','n_inc_75to100k','n_inc_over100k',  # n income level
                           'n_15to24','n_25to34','n_35to44','n_45to54','n_55to64','n_65plus',                                  # n age group
                           'n_white','n_black','n_asian','n_race_oth',                                                         # n race
                           'n_male','n_female',                                                                                # n gender
                           
                           'P_per',                                                                                            # p totals
                           'P_poverty','P_non_pov',                                                                            # p ppverty
                           'P_inc_less15k','P_inc_15to30k','P_inc_30to50k','P_inc_50to75k','P_inc_75to100k','P_inc_over100k',  # p income level
                           'P_15to24','P_25to34','P_35to44','P_45to54','P_55to64','P_65plus',                                  # p age group
                           'P_white','P_black','P_asian','P_race_oth',                                                         # p race
                           'P_male','P_female',                                                                                # p gender
                           
                           'exp27_mean','exp27_SD','exp27_SEM',                                                                # exp27
                           'pov_exp27_mean','pov_exp27_SD','pov_exp27_SEM',                                        
                           'non_pov_exp27_mean','non_pov_exp27_SD','non_pov_exp27_SEM',
                           'inc_less15k_exp27_mean','inc_less15k_exp27_SEM','inc_less15k_exp27_SD',
                           'inc_15to30k_exp27_mean','inc_15to30k_exp27_SEM','inc_15to30k_exp27_SD',
                           'inc_30to50k_exp27_mean','inc_30to50k_exp27_SEM','inc_30to50k_exp27_SD',
                           'inc_50to75k_exp27_mean','inc_50to75k_exp27_SEM','inc_50to75k_exp27_SD',
                           'inc_75to100k_exp27_mean','inc_75to100k_exp27_SEM','inc_75to100k_exp27_SD',
                           'inc_over100k_exp27_mean','inc_over100k_exp27_SEM','inc_over100k_exp27_SD',
                           'age15to24_exp27_mean','age15to24_exp27_SEM','age15to24_exp27_SD',
                           'age25to34_exp27_mean','age25to34_exp27_SEM','age25to34_exp27_SD',
                           'age35to44_exp27_mean','age35to44_exp27_SEM','age35to44_exp27_SD',
                           'age45to54_exp27_mean','age45to54_exp27_SEM','age45to54_exp27_SD',
                           'age55to64_exp27_mean','age55to64_exp27_SEM','age55to64_exp27_SD',
                           'age65plus_exp27_mean','age65plus_exp27_SEM','age65plus_exp27_SD',
                           'wht_exp27_mean','wht_exp27_SEM','wht_exp27_SD',
                           'blk_exp27_mean','blk_exp27_SEM','blk_exp27_SD',
                           'asn_exp27_mean','asn_exp27_SEM','asn_exp27_SD',
                           'race_oth_exp27_mean','race_oth_exp27_SEM','race_oth_exp27_SD',
                           'men_exp27_mean','men_exp27_SEM','men_exp27_SD',
                           'wom_exp27_mean','wom_exp27_SEM','wom_exp27_SD',
                           
                           'MET.dur_mean','MET.dur_SD','MET.dur_SEM',                                                           # MET.dur
                           'pov_MET.dur_mean','pov_MET.dur_SD','pov_MET.dur_SEM',                                         
                           'non_pov_MET.dur_mean','non_pov_MET.dur_SD','non_pov_MET.dur_SEM',
                           'inc_less15k_MET.dur_mean','inc_less15k_MET.dur_SEM','inc_less15k_MET.dur_SD',
                           'inc_15to30k_MET.dur_mean','inc_15to30k_MET.dur_SEM','inc_15to30k_MET.dur_SD',
                           'inc_30to50k_MET.dur_mean','inc_30to50k_MET.dur_SEM','inc_30to50k_MET.dur_SD',
                           'inc_50to75k_MET.dur_mean','inc_50to75k_MET.dur_SEM','inc_50to75k_MET.dur_SD',
                           'inc_75to100k_MET.dur_mean','inc_75to100k_MET.dur_SEM','inc_75to100k_MET.dur_SD',
                           'inc_over100k_MET.dur_mean','inc_over100k_MET.dur_SEM','inc_over100k_MET.dur_SD',
                           'age15to24_MET.dur_mean','age15to24_MET.dur_SEM','age15to24_MET.dur_SD',
                           'age25to34_MET.dur_mean','age25to34_MET.dur_SEM','age25to34_MET.dur_SD',
                           'age35to44_MET.dur_mean','age35to44_MET.dur_SEM','age35to44_MET.dur_SD',
                           'age45to54_MET.dur_mean','age45to54_MET.dur_SEM','age45to54_MET.dur_SD',
                           'age55to64_MET.dur_mean','age55to64_MET.dur_SEM','age55to64_MET.dur_SD',
                           'age65plus_MET.dur_mean','age65plus_MET.dur_SEM','age65plus_MET.dur_SD',
                           'wht_MET.dur_mean','wht_MET.dur_SEM','wht_MET.dur_SD',
                           'blk_MET.dur_mean','blk_MET.dur_SEM','blk_MET.dur_SD',
                           'asn_MET.dur_mean','asn_MET.dur_SEM','asn_MET.dur_SD',
                           'race_oth_MET.dur_mean','race_oth_MET.dur_SEM','race_oth_MET.dur_SD',
                           'men_MET.dur_mean','men_MET.dur_SEM','men_MET.dur_SD',
                           'wom_MET.dur_mean','wom_MET.dur_SEM','wom_MET.dur_SD',
                           
                           'MET.exp27_mean','MET.exp27_SD','MET.exp27_SEM',                                                    # MET.exp27
                           'pov_MET.exp27_mean','pov_MET.exp27_SD','pov_MET.exp27_SEM',                                         
                           'non_pov_MET.exp27_mean','non_pov_MET.exp27_SD','non_pov_MET.exp27_SEM',
                           'inc_less15k_MET.exp27_mean','inc_less15k_MET.exp27_SEM','inc_less15k_MET.exp27_SD',
                           'inc_15to30k_MET.exp27_mean','inc_15to30k_MET.exp27_SEM','inc_15to30k_MET.exp27_SD',
                           'inc_30to50k_MET.exp27_mean','inc_30to50k_MET.exp27_SEM','inc_30to50k_MET.exp27_SD',
                           'inc_50to75k_MET.exp27_mean','inc_50to75k_MET.exp27_SEM','inc_50to75k_MET.exp27_SD',
                           'inc_75to100k_MET.exp27_mean','inc_75to100k_MET.exp27_SEM','inc_75to100k_MET.exp27_SD',
                           'inc_over100k_MET.exp27_mean','inc_over100k_MET.exp27_SEM','inc_over100k_MET.exp27_SD',
                           'age15to24_MET.exp27_mean','age15to24_MET.exp27_SEM','age15to24_MET.exp27_SD',
                           'age25to34_MET.exp27_mean','age25to34_MET.exp27_SEM','age25to34_MET.exp27_SD',
                           'age35to44_MET.exp27_mean','age35to44_MET.exp27_SEM','age35to44_MET.exp27_SD',
                           'age45to54_MET.exp27_mean','age45to54_MET.exp27_SEM','age45to54_MET.exp27_SD',
                           'age55to64_MET.exp27_mean','age55to64_MET.exp27_SEM','age55to64_MET.exp27_SD',
                           'age65plus_MET.exp27_mean','age65plus_MET.exp27_SEM','age65plus_MET.exp27_SD',
                           'wht_MET.exp27_mean','wht_MET.exp27_SEM','wht_MET.exp27_SD',
                           'blk_MET.exp27_mean','blk_MET.exp27_SEM','blk_MET.exp27_SD',
                           'asn_MET.exp27_mean','asn_MET.exp27_SEM','asn_MET.exp27_SD',
                           'race_oth_MET.exp27_mean','race_oth_MET.exp27_SEM','race_oth_MET.exp27_SD',
                           'men_MET.exp27_mean','men_MET.exp27_SEM','men_MET.exp27_SD',
                           'wom_MET.exp27_mean','wom_MET.exp27_SEM','wom_MET.exp27_SD',
                           
                           'MET_mean','MET_SD','MET_SEM',                                                                       # MET
                           'pov_MET_mean','pov_MET_SD','pov_MET_SEM',                                         
                           'non_pov_MET_mean','non_pov_MET_SD','non_pov_MET_SEM',
                           'inc_less15k_MET_mean','inc_less15k_MET_SEM','inc_less15k_MET_SD',
                           'inc_15to30k_MET_mean','inc_15to30k_MET_SEM','inc_15to30k_MET_SD',
                           'inc_30to50k_MET_mean','inc_30to50k_MET_SEM','inc_30to50k_MET_SD',
                           'inc_50to75k_MET_mean','inc_50to75k_MET_SEM','inc_50to75k_MET_SD',
                           'inc_75to100k_MET_mean','inc_75to100k_MET_SEM','inc_75to100k_MET_SD',
                           'inc_over100k_MET_mean','inc_over100k_MET_SEM','inc_over100k_MET_SD',
                           'age15to24_MET_mean','age15to24_MET_SEM','age15to24_MET_SD',
                           'age25to34_MET_mean','age25to34_MET_SEM','age25to34_MET_SD',
                           'age35to44_MET_mean','age35to44_MET_SEM','age35to44_MET_SD',
                           'age45to54_MET_mean','age45to54_MET_SEM','age45to54_MET_SD',
                           'age55to64_MET_mean','age55to64_MET_SEM','age55to64_MET_SD',
                           'age65plus_MET_mean','age65plus_MET_SEM','age65plus_MET_SD',
                           'wht_MET_mean','wht_MET_SEM','wht_MET_SD',
                           'blk_MET_mean','blk_MET_SEM','blk_MET_SD',
                           'asn_MET_mean','asn_MET_SEM','asn_MET_SD',
                           'race_oth_MET_mean','race_oth_MET_SEM','race_oth_MET_SD',
                           'men_MET_mean','men_MET_SEM','men_MET_SD',
                           'wom_MET_mean','wom_MET_SEM','wom_MET_SD')


# name rows
rownames(summary.ses) <- c('total population','outdoor activity pop','outdoor activity & > 27 C pop','p-value')
summary.ses[] <- NA

# weighted (N) and unweighted (n) sample sizes by demographic factors
summary.ses$n_act[1] <- nrow(meta) # total rows = activities
summary.ses$n_act[2] <- nrow(meta.o)
summary.ses$n_act[3] <- nrow(meta.f27)

summary.ses$n_per[1] <- nrow(meta.s.agg[!duplicated(meta.s.agg$id),]) # total unique IDs = total respondents
summary.ses$n_per[2] <- nrow(meta.o.agg)
summary.ses$n_per[3] <- nrow(meta.f27.agg)

summary.ses$N_per[1] <- sum(meta.s.agg$weight[!duplicated(meta.s.agg$id)]) / ((31 + 31 + 30 + 31 + 30 + 31) + (365 * 9) + (366 * 2))  # total person-days (sum of unique per weight) divided by total days in sample 
summary.ses$N_per[2] <- sum(meta.o.agg$weight) / ((31 + 31 + 30 + 31 + 30 + 31) + (365 * 9) + (366 * 2))
summary.ses$N_per[3] <- sum(meta.f27.agg$weight) / ((31 + 31 + 30 + 31 + 30 + 31) + (365 * 9) + (366 * 2))

summary.ses$n_poverty[1] <- nrow(meta.s.agg[meta.s.agg$poverty.f == "Below Poverty",])
summary.ses$n_poverty[2] <- nrow(meta.o.agg[meta.o.agg$poverty.f == "Below Poverty",])
summary.ses$n_poverty[3] <- nrow(meta.f27.agg[meta.f27.agg$poverty.f == "Below Poverty",])

summary.ses$n_non_pov[1] <- nrow(meta.s.agg[meta.s.agg$poverty.f == "Above Poverty",])
summary.ses$n_non_pov[2] <- nrow(meta.o.agg[meta.o.agg$poverty.f == "Above Poverty",])
summary.ses$n_non_pov[3] <- nrow(meta.f27.agg[meta.f27.agg$poverty.f == "Above Poverty",])

summary.ses$n_inc_less15k[1] <- nrow(meta.s.agg[meta.s.agg$income.f == "Less than $15k",])
summary.ses$n_inc_less15k[2] <- nrow(meta.o.agg[meta.o.agg$income.f == "Less than $15k",])
summary.ses$n_inc_less15k[3] <- nrow(meta.f27.agg[meta.f27.agg$income.f == "Less than $15k",])

summary.ses$n_inc_15to30k[1] <- nrow(meta.s.agg[meta.s.agg$income.f == "$15k to $30k",])
summary.ses$n_inc_15to30k[2] <- nrow(meta.o.agg[meta.o.agg$income.f == "$15k to $30k",])
summary.ses$n_inc_15to30k[3] <- nrow(meta.f27.agg[meta.f27.agg$income.f == "$15k to $30k",])

summary.ses$n_inc_30to50k[1] <- nrow(meta.s.agg[meta.s.agg$income.f == "$30k to $50k",])
summary.ses$n_inc_30to50k[2] <- nrow(meta.o.agg[meta.o.agg$income.f == "$30k to $50k",])
summary.ses$n_inc_30to50k[3] <- nrow(meta.f27.agg[meta.f27.agg$income.f == "$30k to $50k",])

summary.ses$n_inc_50to75k[1] <- nrow(meta.s.agg[meta.s.agg$income.f == "$50k to $75k",])
summary.ses$n_inc_50to75k[2] <- nrow(meta.o.agg[meta.o.agg$income.f == "$50k to $75k",])
summary.ses$n_inc_50to75k[3] <- nrow(meta.f27.agg[meta.f27.agg$income.f == "$50k to $75k",])

summary.ses$n_inc_75to100k[1] <- nrow(meta.s.agg[meta.s.agg$income.f == "$75k to $100k",])
summary.ses$n_inc_75to100k[2] <- nrow(meta.o.agg[meta.o.agg$income.f == "$75k to $100k",])
summary.ses$n_inc_75to100k[3] <- nrow(meta.f27.agg[meta.f27.agg$income.f == "$75k to $100k",])

summary.ses$n_inc_over100k[1] <- nrow(meta.s.agg[meta.s.agg$income.f == "$100k and over",])
summary.ses$n_inc_over100k[2] <- nrow(meta.o.agg[meta.o.agg$income.f == "$100k and over",])
summary.ses$n_inc_over100k[3] <- nrow(meta.f27.agg[meta.f27.agg$income.f == "$100k and over",])

summary.ses$n_15to24[1] <- nrow(meta.s.agg[meta.s.agg$age.f2 == "15-24",])
summary.ses$n_15to24[2] <- nrow(meta.o.agg[meta.o.agg$age.f2 == "15-24",])
summary.ses$n_15to24[3] <- nrow(meta.f27.agg[meta.f27.agg$age.f2 == "15-24",])

summary.ses$n_25to34[1] <- nrow(meta.s.agg[meta.s.agg$age.f2 == "25-34",])
summary.ses$n_25to34[2] <- nrow(meta.o.agg[meta.o.agg$age.f2 == "25-34",])
summary.ses$n_25to34[3] <- nrow(meta.f27.agg[meta.f27.agg$age.f2 == "25-34",])

summary.ses$n_35to44[1] <- nrow(meta.s.agg[meta.s.agg$age.f2 == "35-44",])
summary.ses$n_35to44[2] <- nrow(meta.o.agg[meta.o.agg$age.f2 == "35-44",])
summary.ses$n_35to44[3] <- nrow(meta.f27.agg[meta.f27.agg$age.f2 == "35-44",])

summary.ses$n_45to54[1] <- nrow(meta.s.agg[meta.s.agg$age.f2 == "45-54",])
summary.ses$n_45to54[2] <- nrow(meta.o.agg[meta.o.agg$age.f2 == "45-54",])
summary.ses$n_45to54[3] <- nrow(meta.f27.agg[meta.f27.agg$age.f2 == "45-54",])

summary.ses$n_55to64[1] <- nrow(meta.s.agg[meta.s.agg$age.f2 == "55-64",])
summary.ses$n_55to64[2] <- nrow(meta.o.agg[meta.o.agg$age.f2 == "55-64",])
summary.ses$n_55to64[3] <- nrow(meta.f27.agg[meta.f27.agg$age.f2 == "55-64",])

summary.ses$n_65plus[1] <- nrow(meta.s.agg[meta.s.agg$age.f2 == "65 and over",])
summary.ses$n_65plus[2] <- nrow(meta.o.agg[meta.o.agg$age.f2 == "65 and over",])
summary.ses$n_65plus[3] <- nrow(meta.f27.agg[meta.f27.agg$age.f2 == "65 and over",])

summary.ses$n_white[1] <- nrow(meta.s.agg[meta.s.agg$race.f2 == "White",])
summary.ses$n_white[2] <- nrow(meta.o.agg[meta.o.agg$race.f2 == "White",])
summary.ses$n_white[3] <- nrow(meta.f27.agg[meta.f27.agg$race.f2 == "White",])

summary.ses$n_black[1] <- nrow(meta.f27.agg[meta.f27.agg$race.f2 == "Black",])
summary.ses$n_black[2] <- nrow(meta.o.agg[meta.f27.agg$race.f2 == "Black",])
summary.ses$n_black[3] <- nrow(meta.f27.agg[meta.f27.agg$race.f2 == "Black",])

summary.ses$n_asian[1] <- nrow(meta.s.agg[meta.s.agg$race.f2 == "Asian",])
summary.ses$n_asian[2] <- nrow(meta.o.agg[meta.o.agg$race.f2 == "Asian",])
summary.ses$n_asian[3] <- nrow(meta.f27.agg[meta.f27.agg$race.f2 == "Asian",])

summary.ses$n_race_oth[1] <- nrow(meta.s.agg[meta.s.agg$race.f2 != "Asian" & meta.s.agg$race.f2 != "Black" & meta.s.agg$race.f2 != "White" & !is.na(meta.s.agg$race.f2),])
summary.ses$n_race_oth[2] <- nrow(meta.o.agg[meta.o.agg$race.f2 != "Asian" & meta.o.agg$race.f2 != "Black" & meta.o.agg$race.f2 != "White" & !is.na(meta.o.agg$race.f2),])
summary.ses$n_race_oth[3] <- nrow(meta.f27.agg[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2),])

summary.ses$n_male[1] <- nrow(meta.s.agg[meta.s.agg$sex.f == "Male",])
summary.ses$n_male[2] <- nrow(meta.o.agg[meta.o.agg$sex.f == "Male",])
summary.ses$n_male[3] <- nrow(meta.f27.agg[meta.f27.agg$sex.f == "Male",])

summary.ses$n_female[1] <- nrow(meta.s.agg[meta.s.agg$sex.f == "Female",])
summary.ses$n_female[2] <- nrow(meta.o.agg[meta.o.agg$sex.f == "Female",])
summary.ses$n_female[3] <- nrow(meta.f27.agg[meta.f27.agg$sex.f == "Female",])

# weighted proportions (P) of activities by demographic factors
summary.ses$P_per[1] <- summary.ses$N_per[1] / summary.ses$N_per[1]  # total pop is 100%
summary.ses$P_per[2] <- summary.ses$N_per[2] / summary.ses$N_per[1]  # % pop outdoor & > 21 C of total pop
summary.ses$P_per[3] <- summary.ses$N_per[3] / summary.ses$N_per[1]  # % pop outdoor & > 27 C of total pop

summary.ses$P_poverty[1] <- sum(ifelse(meta$poverty.f == "Below Poverty", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_poverty[2] <- sum(ifelse(meta.o$poverty.f == "Below Poverty", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_poverty[3] <- sum(ifelse(meta.f27$poverty.f == "Below Poverty", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_non_pov[1] <- sum(ifelse(meta$poverty.f == "Above Poverty", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_non_pov[2] <- sum(ifelse(meta.o$poverty.f == "Above Poverty", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_non_pov[3] <- sum(ifelse(meta.f27$poverty.f == "Above Poverty", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_inc_less15k[1] <- sum(ifelse(meta$income.f == "Less than $15k", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_inc_less15k[2] <- sum(ifelse(meta.o$income.f == "Less than $15k", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_inc_less15k[3] <- sum(ifelse(meta.f27$income.f == "Less than $15k", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_inc_15to30k[1] <- sum(ifelse(meta$income.f == "$15k to $30k", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_inc_15to30k[2] <- sum(ifelse(meta.o$income.f == "$15k to $30k", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_inc_15to30k[3] <- sum(ifelse(meta.f27$income.f == "$15k to $30k", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_inc_30to50k[1] <- sum(ifelse(meta$income.f == "$30k to $50k", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_inc_30to50k[2] <- sum(ifelse(meta.o$income.f == "$30k to $50k", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_inc_30to50k[3] <- sum(ifelse(meta.f27$income.f == "$30k to $50k", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_inc_50to75k[1] <- sum(ifelse(meta$income.f == "$50k to $75k", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_inc_50to75k[2] <- sum(ifelse(meta.o$income.f == "$50k to $75k", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_inc_50to75k[3] <- sum(ifelse(meta.f27$income.f == "$50k to $75k", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_inc_75to100k[1] <- sum(ifelse(meta$income.f == "$75k to $100k", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_inc_75to100k[2] <- sum(ifelse(meta.o$income.f == "$75k to $100k", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight) 
summary.ses$P_inc_75to100k[3] <- sum(ifelse(meta.f27$income.f == "$75k to $100k", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_inc_over100k[1] <- sum(ifelse(meta$income.f == "$100k and over", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_inc_over100k[2] <- sum(ifelse(meta.o$income.f == "$100k and over", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_inc_over100k[3] <- sum(ifelse(meta.f27$income.f == "$100k and over", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_15to24[1] <- sum(ifelse(meta$age.f2 == "15-24", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_15to24[2] <- sum(ifelse(meta.o$age.f2 == "15-24", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_15to24[3] <- sum(ifelse(meta.f27$age.f2 == "15-24", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_25to34[1] <- sum(ifelse(meta$age.f2 == "25-34", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_25to34[2] <- sum(ifelse(meta.o$age.f2 == "25-34", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_25to34[3] <- sum(ifelse(meta.f27$age.f2 == "25-34", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_35to44[1] <- sum(ifelse(meta$age.f2 == "35-44", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_35to44[2] <- sum(ifelse(meta.o$age.f2 == "35-44", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_35to44[3] <- sum(ifelse(meta.f27$age.f2 == "35-44", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_45to54[1] <- sum(ifelse(meta$age.f2 == "45-54", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_45to54[2] <- sum(ifelse(meta.o$age.f2 == "45-54", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_45to54[3] <- sum(ifelse(meta.f27$age.f2 == "45-54", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_55to64[1] <- sum(ifelse(meta$age.f2 == "55-64", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_55to64[2] <- sum(ifelse(meta.o$age.f2 == "55-64", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_55to64[3] <- sum(ifelse(meta.f27$age.f2 == "55-64", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_65plus[1] <- sum(ifelse(meta$age.f2 == "65 and over", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_65plus[2] <- sum(ifelse(meta.o$age.f2 == "65 and over", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_65plus[3] <- sum(ifelse(meta.f27$age.f2 == "65 and over", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_white[1] <- sum(ifelse(meta$race.f2 == "White", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_white[2] <- sum(ifelse(meta.o$race.f2 == "White", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_white[3] <- sum(ifelse(meta.f27$race.f2 == "White", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_black[1] <- sum(ifelse(meta$race.f2 == "Black", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_black[2] <- sum(ifelse(meta.o$race.f2 == "Black", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_black[3] <- sum(ifelse(meta.f27$race.f2 == "Black", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_asian[1] <- sum(ifelse(meta$race.f2 == "Asian", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_asian[2] <- sum(ifelse(meta.o$race.f2 == "Asian", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_asian[3] <- sum(ifelse(meta.f27$race.f2 == "Asian", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_race_oth[1] <- sum(ifelse(meta$race.f2 != "Asian" & meta$race.f2 != "Black" & meta$race.f2 != "White", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_race_oth[2] <- sum(ifelse(meta.o$race.f2 != "Asian" & meta.o$race.f2 != "Black" & meta.o$race.f2 != "White", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_race_oth[3] <- sum(ifelse(meta.f27$race.f2 != "Asian" & meta.f27$race.f2 != "Black" & meta.f27$race.f2 != "White", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_male[1] <- sum(ifelse(meta$sex.f == "Male", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_male[2] <- sum(ifelse(meta.o$sex.f == "Male", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_male[3] <- sum(ifelse(meta.f27$sex.f == "Male", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

summary.ses$P_female[1] <- sum(ifelse(meta$sex.f == "Female", meta$weight, 0), na.rm = T) / sum(meta$weight)
summary.ses$P_female[2] <- sum(ifelse(meta.o$sex.f == "Female", meta.o$weight, 0), na.rm = T) / sum(meta.o$weight)
summary.ses$P_female[3] <- sum(ifelse(meta.f27$sex.f == "Female", meta.f27$weight, 0), na.rm = T) / sum(meta.f27$weight)

rep.start <- 15

# Weighted Person-day Exposure & Exp Intenisty means, SD, and SEM  *Highlight then Alt+Shift+L to expand collpased regions*

# exp27
summary.ses$exp27_mean[3] <- sum(meta.f27.agg$exp27 * meta.f27.agg$weight)  / sum(meta.f27.agg$weight)
for(i in 1:160){j <- i + rep.start
err[i,8] <- (weighted.mean(meta.f27.agg$exp27,meta.f27.agg[,j]) - summary.ses$exp27_mean[3])^2} # error calc
summary.ses$exp27_SEM[3] <- sqrt((4/160) * sum(err[,8])) / sqrt(summary.ses$n_per[3])  # SEM = sqrt(var) / sqrt(n)
summary.ses$exp27_SD[3] <- sqrt((4/160) * sum(err[,8]))  # SD = sqrt(var)

summary.ses$pov_exp27_mean[3] <- sum(ifelse(meta.f27.agg$poverty.f == "Below Poverty", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$poverty.f == "Below Poverty", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,9] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$poverty.f == "Below Poverty" & !is.na(meta.f27.agg$poverty.f)],meta.f27.agg[meta.f27.agg$poverty.f == "Below Poverty" & !is.na(meta.f27.agg$poverty.f),j]) - summary.ses$pov_exp27_mean[3])^2} # error calc
summary.ses$pov_exp27_SEM[3] <- sqrt((4/160) * sum(err[,9])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$poverty.f == "Below Poverty",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$pov_exp27_SD[3] <- sqrt((4/160) * sum(err[,9]))   # SD = sqrt(var)

summary.ses$non_pov_exp27_mean[3] <- sum(ifelse(meta.f27.agg$poverty.f == "Above Poverty", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$poverty.f == "Above Poverty", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,10] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$poverty.f == "Above Poverty" & !is.na(meta.f27.agg$poverty.f)],meta.f27.agg[meta.f27.agg$poverty.f == "Above Poverty" & !is.na(meta.f27.agg$poverty.f),j]) - summary.ses$non_pov_exp27_mean[3])^2} # error calc
summary.ses$non_pov_exp27_SEM[3] <- sqrt((4/160) * sum(err[,10])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$poverty.f == "Above Poverty",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$non_pov_exp27_SD[3] <- sqrt((4/160) * sum(err[,10]))   # SD = sqrt(var)

summary.ses$inc_less15k_exp27_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "Less than $15k", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "Less than $15k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,11] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$income.f == "Less than $15k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "Less than $15k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_less15k_exp27_mean[3])^2} # error calc
summary.ses$inc_less15k_exp27_SEM[3] <- sqrt((4/160) * sum(err[,11])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "Less than $15k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_less15k_exp27_SD[3] <- sqrt((4/160) * sum(err[,11]))   # SD = sqrt(var)

summary.ses$inc_15to30k_exp27_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$15k to $30k", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$15k to $30k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,12] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$income.f == "$15k to $30k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$15k to $30k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_15to30k_exp27_mean[3])^2} # error calc
summary.ses$inc_15to30k_exp27_SEM[3] <- sqrt((4/160) * sum(err[,12])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$15k to $30k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_15to30k_exp27_SD[3] <- sqrt((4/160) * sum(err[,12]))   # SD = sqrt(var)

summary.ses$inc_30to50k_exp27_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$30k to $50k", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$30k to $50k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,13] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$income.f == "$30k to $50k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$30k to $50k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_30to50k_exp27_mean[3])^2} # error calc
summary.ses$inc_30to50k_exp27_SEM[3] <- sqrt((4/160) * sum(err[,13])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$30k to $50k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_30to50k_exp27_SD[3] <- sqrt((4/160) * sum(err[,13]))   # SD = sqrt(var)

summary.ses$inc_50to75k_exp27_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$50k to $75k", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$50k to $75k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,14] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$income.f == "$50k to $75k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$50k to $75k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_50to75k_exp27_mean[3])^2} # error calc
summary.ses$inc_50to75k_exp27_SEM[3] <- sqrt((4/160) * sum(err[,14])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$50k to $75k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_50to75k_exp27_SD[3] <- sqrt((4/160) * sum(err[,14]))   # SD = sqrt(var)

summary.ses$inc_75to100k_exp27_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$75k to $100k", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$75k to $100k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,15] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$income.f == "$75k to $100k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$75k to $100k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_75to100k_exp27_mean[3])^2} # error calc
summary.ses$inc_75to100k_exp27_SEM[3] <- sqrt((4/160) * sum(err[,15])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$75k to $100k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_75to100k_exp27_SD[3] <- sqrt((4/160) * sum(err[,15]))   # SD = sqrt(var)

summary.ses$inc_over100k_exp27_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$100k and over", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$100k and over", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,16] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$income.f == "$100k and over" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$100k and over" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_over100k_exp27_mean[3])^2} # error calc
summary.ses$inc_over100k_exp27_SEM[3] <- sqrt((4/160) * sum(err[,16])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$100k and over",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_over100k_exp27_SD[3] <- sqrt((4/160) * sum(err[,16]))   # SD = sqrt(var)

summary.ses$age15to24_exp27_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "15-24", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "15-24", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,17] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$age.f2 == "15-24" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "15-24" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age15to24_exp27_mean[3])^2} # error calc
summary.ses$age15to24_exp27_SEM[3] <- sqrt((4/160) * sum(err[,17])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "15-24",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age15to24_exp27_SD[3] <- sqrt((4/160) * sum(err[,17])) # SD = sqrt(var)

summary.ses$age25to34_exp27_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "25-34", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "25-34", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,18] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$age.f2 == "25-34" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "25-34" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age25to34_exp27_mean[3])^2} # error calc
summary.ses$age25to34_exp27_SEM[3] <- sqrt((4/160) * sum(err[,18])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "25-34",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age25to34_exp27_SD[3] <- sqrt((4/160) * sum(err[,18])) # SD = sqrt(var)

summary.ses$age35to44_exp27_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "35-44", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "35-44", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,19] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$age.f2 == "35-44" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "35-44" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age35to44_exp27_mean[3])^2} # error calc
summary.ses$age35to44_exp27_SEM[3] <- sqrt((4/160) * sum(err[,19])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "35-44",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age35to44_exp27_SD[3] <- sqrt((4/160) * sum(err[,19])) # SD = sqrt(var)

summary.ses$age45to54_exp27_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "45-54", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "45-54", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,20] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$age.f2 == "45-54" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "45-54" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age45to54_exp27_mean[3])^2} # error calc
summary.ses$age45to54_exp27_SEM[3] <- sqrt((4/160) * sum(err[,20])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "45-54",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age45to54_exp27_SD[3] <- sqrt((4/160) * sum(err[,20])) # SD = sqrt(var)

summary.ses$age55to64_exp27_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "55-64", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "55-64", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,21] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$age.f2 == "55-64" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "55-64" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age55to64_exp27_mean[3])^2} # error calc
summary.ses$age55to64_exp27_SEM[3] <- sqrt((4/160) * sum(err[,21])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "55-64",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age55to64_exp27_SD[3] <- sqrt((4/160) * sum(err[,21])) # SD = sqrt(var)

summary.ses$age65plus_exp27_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "65 and over", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "65 and over", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,22] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$age.f2 == "65 and over" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "65 and over" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age65plus_exp27_mean[3])^2} # error calc
summary.ses$age65plus_exp27_SEM[3] <- sqrt((4/160) * sum(err[,22])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "65 and over",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age65plus_exp27_SD[3] <- sqrt((4/160) * sum(err[,22])) # SD = sqrt(var)

summary.ses$wht_exp27_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 == "White", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 == "White", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,23] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$race.f2 == "White" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 == "White" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$wht_exp27_mean[3])^2} # error calc
summary.ses$wht_exp27_SEM[3] <- sqrt((4/160) * sum(err[,23])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 == "White",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$wht_exp27_SD[3] <- sqrt((4/160) * sum(err[,23]))

summary.ses$blk_exp27_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 == "Black", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 == "Black", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,24] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$race.f2 == "Black" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 == "Black" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$blk_exp27_mean[3])^2} # error calc
summary.ses$blk_exp27_SEM[3] <- sqrt((4/160) * sum(err[,24])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 == "Black",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$blk_exp27_SD[3] <- sqrt((4/160) * sum(err[,24]))

summary.ses$asn_exp27_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 == "Asian", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 == "Asian", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,25] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$race.f2 == "Asian" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 == "Asian" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$blk_exp27_mean[3])^2} # error calc
summary.ses$asn_exp27_SEM[3] <- sqrt((4/160) * sum(err[,25])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 == "Asian",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$asn_exp27_SD[3] <- sqrt((4/160) * sum(err[,25]))

summary.ses$race_oth_exp27_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,26] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$race_oth_exp27_mean[3])^2} # error calc
summary.ses$race_oth_exp27_SEM[3] <- sqrt((4/160) * sum(err[,26])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$race_oth_exp27_SD[3] <- sqrt((4/160) * sum(err[,26]))

summary.ses$men_exp27_mean[3] <- sum(ifelse(meta.f27.agg$sex.f == "Male", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$sex.f == "Male", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,27] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$sex.f == "Male" & !is.na(meta.f27.agg$sex.f)], meta.f27.agg[meta.f27.agg$sex.f == "Male" & !is.na(meta.f27.agg$sex.f),j]) - summary.ses$men_exp27_mean[3])^2} # error calc
summary.ses$men_exp27_SEM[3] <- sqrt((4/160) * sum(err[,27])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$sex.f == "Male",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$men_exp27_SD[3] <- sqrt((4/160) * sum(err[,27]))

summary.ses$wom_exp27_mean[3] <- sum(ifelse(meta.f27.agg$sex.f == "Female", meta.f27.agg$exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$sex.f == "Female", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,28] <- (weighted.mean(meta.f27.agg$exp27[meta.f27.agg$sex.f == "Female" & !is.na(meta.f27.agg$sex.f)], meta.f27.agg[meta.f27.agg$sex.f == "Female" & !is.na(meta.f27.agg$sex.f),j]) - summary.ses$wom_exp27_mean[3])^2} # error calc
summary.ses$wom_exp27_SEM[3] <- sqrt((4/160) * sum(err[,28])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$sex.f == "Female",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$wom_exp27_SD[3] <- sqrt((4/160) * sum(err[,28]))

# MET.exp27
summary.ses$MET.exp27_mean[3] <- sum(meta.f27.agg$MET.exp27 * meta.f27.agg$weight)  / sum(meta.f27.agg$weight)
for(i in 1:160){j <- i + rep.start
err[i,29] <- (weighted.mean(meta.f27.agg$MET.exp27,meta.f27.agg[,j]) - summary.ses$MET.exp27_mean[3])^2} # error calc
summary.ses$MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,29])) / sqrt(summary.ses$n_per[3])  # SEM = sqrt(var) / sqrt(n)
summary.ses$MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,29]))  # SD = sqrt(var)

summary.ses$pov_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$poverty.f == "Below Poverty", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$poverty.f == "Below Poverty", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,30] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$poverty.f == "Below Poverty" & !is.na(meta.f27.agg$poverty.f)],meta.f27.agg[meta.f27.agg$poverty.f == "Below Poverty" & !is.na(meta.f27.agg$poverty.f),j]) - summary.ses$pov_MET.exp27_mean[3])^2} # error calc
summary.ses$pov_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,30])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$poverty.f == "Below Poverty",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$pov_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,30]))   # SD = sqrt(var)

summary.ses$non_pov_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$poverty.f == "Above Poverty", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$poverty.f == "Above Poverty", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,31] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$poverty.f == "Above Poverty" & !is.na(meta.f27.agg$poverty.f)],meta.f27.agg[meta.f27.agg$poverty.f == "Above Poverty" & !is.na(meta.f27.agg$poverty.f),j]) - summary.ses$non_pov_MET.exp27_mean[3])^2} # error calc
summary.ses$non_pov_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,31])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$poverty.f == "Above Poverty",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$non_pov_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,31]))   # SD = sqrt(var)

summary.ses$inc_less15k_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "Less than $15k", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "Less than $15k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,32] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$income.f == "Less than $15k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "Less than $15k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_less15k_MET.exp27_mean[3])^2} # error calc
summary.ses$inc_less15k_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,32])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "Less than $15k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_less15k_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,32]))   # SD = sqrt(var)

summary.ses$inc_15to30k_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$15k to $30k", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$15k to $30k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,33] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$income.f == "$15k to $30k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$15k to $30k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_15to30k_MET.exp27_mean[3])^2} # error calc
summary.ses$inc_15to30k_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,33])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$15k to $30k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_15to30k_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,33]))   # SD = sqrt(var)

summary.ses$inc_30to50k_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$30k to $50k", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$30k to $50k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,34] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$income.f == "$30k to $50k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$30k to $50k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_30to50k_MET.exp27_mean[3])^2} # error calc
summary.ses$inc_30to50k_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,34])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$30k to $50k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_30to50k_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,34]))   # SD = sqrt(var)

summary.ses$inc_50to75k_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$50k to $75k", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$50k to $75k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,35] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$income.f == "$50k to $75k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$50k to $75k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_50to75k_MET.exp27_mean[3])^2} # error calc
summary.ses$inc_50to75k_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,35])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$50k to $75k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_50to75k_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,35]))   # SD = sqrt(var)

summary.ses$inc_75to100k_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$75k to $100k", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$75k to $100k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,36] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$income.f == "$75k to $100k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$75k to $100k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_75to100k_MET.exp27_mean[3])^2} # error calc
summary.ses$inc_75to100k_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,36])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$75k to $100k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_75to100k_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,36]))   # SD = sqrt(var)

summary.ses$inc_over100k_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$100k and over", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$100k and over", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,37] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$income.f == "$100k and over" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$100k and over" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_over100k_MET.exp27_mean[3])^2} # error calc
summary.ses$inc_over100k_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,37])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$100k and over",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_over100k_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,37]))   # SD = sqrt(var)

summary.ses$age15to24_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "15-24", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "15-24", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,38] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$age.f2 == "15-24" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "15-24" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age15to24_MET.exp27_mean[3])^2} # error calc
summary.ses$age15to24_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,38])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "15-24",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age15to24_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,38])) # SD = sqrt(var)

summary.ses$age25to34_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "25-34", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "25-34", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,39] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$age.f2 == "25-34" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "25-34" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age25to34_MET.exp27_mean[3])^2} # error calc
summary.ses$age25to34_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,39])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "25-34",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age25to34_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,39])) # SD = sqrt(var)

summary.ses$age35to44_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "35-44", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "35-44", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,40] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$age.f2 == "35-44" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "35-44" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age35to44_MET.exp27_mean[3])^2} # error calc
summary.ses$age35to44_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,40])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "35-44",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age35to44_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,40])) # SD = sqrt(var)

summary.ses$age45to54_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "45-54", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "45-54", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,41] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$age.f2 == "45-54" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "45-54" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age45to54_MET.exp27_mean[3])^2} # error calc
summary.ses$age45to54_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,41])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "45-54",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age45to54_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,41])) # SD = sqrt(var)

summary.ses$age55to64_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "55-64", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "55-64", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,42] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$age.f2 == "55-64" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "55-64" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age55to64_MET.exp27_mean[3])^2} # error calc
summary.ses$age55to64_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,42])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "55-64",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age55to64_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,42])) # SD = sqrt(var)

summary.ses$age65plus_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "65 and over", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "65 and over", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,43] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$age.f2 == "65 and over" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "65 and over" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age65plus_MET.exp27_mean[3])^2} # error calc
summary.ses$age65plus_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,43])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "65 and over",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age65plus_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,43])) # SD = sqrt(var)

summary.ses$wht_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 == "White", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 == "White", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,44] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$race.f2 == "White" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 == "White" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$wht_MET.exp27_mean[3])^2} # error calc
summary.ses$wht_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,44])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 == "White",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$wht_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,44]))

summary.ses$blk_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 == "Black", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 == "Black", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,45] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$race.f2 == "Black" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 == "Black" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$blk_MET.exp27_mean[3])^2} # error calc
summary.ses$blk_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,45])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 == "Black",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$blk_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,45]))

summary.ses$asn_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 == "Asian", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 == "Asian", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,46] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$race.f2 == "Asian" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 == "Asian" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$blk_MET.exp27_mean[3])^2} # error calc
summary.ses$asn_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,46])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 == "Asian",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$asn_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,46]))

summary.ses$race_oth_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,47] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$race_oth_MET.exp27_mean[3])^2} # error calc
summary.ses$race_oth_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,47])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$race_oth_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,47]))

summary.ses$men_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$sex.f == "Male", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$sex.f == "Male", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,48] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$sex.f == "Male" & !is.na(meta.f27.agg$sex.f)], meta.f27.agg[meta.f27.agg$sex.f == "Male" & !is.na(meta.f27.agg$sex.f),j]) - summary.ses$men_MET.exp27_mean[3])^2} # error calc
summary.ses$men_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,48])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$sex.f == "Male",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$men_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,48]))

summary.ses$wom_MET.exp27_mean[3] <- sum(ifelse(meta.f27.agg$sex.f == "Female", meta.f27.agg$MET.exp27 * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$sex.f == "Female", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,49] <- (weighted.mean(meta.f27.agg$MET.exp27[meta.f27.agg$sex.f == "Female" & !is.na(meta.f27.agg$sex.f)], meta.f27.agg[meta.f27.agg$sex.f == "Female" & !is.na(meta.f27.agg$sex.f),j]) - summary.ses$wom_MET.exp27_mean[3])^2} # error calc
summary.ses$wom_MET.exp27_SEM[3] <- sqrt((4/160) * sum(err[,49])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$sex.f == "Female",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$wom_MET.exp27_SD[3] <- sqrt((4/160) * sum(err[,49]))


# MET.dur
summary.ses$MET.dur_mean[3] <- sum(meta.f27.agg$MET.dur * meta.f27.agg$weight)  / sum(meta.f27.agg$weight)
for(i in 1:160){j <- i + rep.start
err[i,50] <- (weighted.mean(meta.f27.agg$MET.dur,meta.f27.agg[,j]) - summary.ses$MET.dur_mean[3])^2} # error calc
summary.ses$MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,50])) / sqrt(summary.ses$n_per[3])  # SEM = sqrt(var) / sqrt(n)
summary.ses$MET.dur_SD[3] <- sqrt((4/160) * sum(err[,50]))  # SD = sqrt(var)

summary.ses$pov_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$poverty.f == "Below Poverty", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$poverty.f == "Below Poverty", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,51] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$poverty.f == "Below Poverty" & !is.na(meta.f27.agg$poverty.f)],meta.f27.agg[meta.f27.agg$poverty.f == "Below Poverty" & !is.na(meta.f27.agg$poverty.f),j]) - summary.ses$pov_MET.dur_mean[3])^2} # error calc
summary.ses$pov_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,51])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$poverty.f == "Below Poverty",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$pov_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,51]))   # SD = sqrt(var)

summary.ses$non_pov_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$poverty.f == "Above Poverty", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$poverty.f == "Above Poverty", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,52] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$poverty.f == "Above Poverty" & !is.na(meta.f27.agg$poverty.f)],meta.f27.agg[meta.f27.agg$poverty.f == "Above Poverty" & !is.na(meta.f27.agg$poverty.f),j]) - summary.ses$non_pov_MET.dur_mean[3])^2} # error calc
summary.ses$non_pov_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,52])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$poverty.f == "Above Poverty",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$non_pov_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,52]))   # SD = sqrt(var)

summary.ses$inc_less15k_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "Less than $15k", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "Less than $15k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,53] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$income.f == "Less than $15k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "Less than $15k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_less15k_MET.dur_mean[3])^2} # error calc
summary.ses$inc_less15k_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,53])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "Less than $15k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_less15k_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,53]))   # SD = sqrt(var)

summary.ses$inc_15to30k_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$15k to $30k", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$15k to $30k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,54] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$income.f == "$15k to $30k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$15k to $30k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_15to30k_MET.dur_mean[3])^2} # error calc
summary.ses$inc_15to30k_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,54])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$15k to $30k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_15to30k_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,54]))   # SD = sqrt(var)

summary.ses$inc_30to50k_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$30k to $50k", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$30k to $50k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,55] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$income.f == "$30k to $50k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$30k to $50k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_30to50k_MET.dur_mean[3])^2} # error calc
summary.ses$inc_30to50k_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,55])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$30k to $50k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_30to50k_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,55]))   # SD = sqrt(var)

summary.ses$inc_50to75k_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$50k to $75k", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$50k to $75k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,56] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$income.f == "$50k to $75k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$50k to $75k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_50to75k_MET.dur_mean[3])^2} # error calc
summary.ses$inc_50to75k_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,56])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$50k to $75k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_50to75k_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,56]))   # SD = sqrt(var)

summary.ses$inc_75to100k_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$75k to $100k", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$75k to $100k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,57] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$income.f == "$75k to $100k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$75k to $100k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_75to100k_MET.dur_mean[3])^2} # error calc
summary.ses$inc_75to100k_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,57])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$75k to $100k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_75to100k_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,57]))   # SD = sqrt(var)

summary.ses$inc_over100k_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$100k and over", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$100k and over", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,58] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$income.f == "$100k and over" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$100k and over" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_over100k_MET.dur_mean[3])^2} # error calc
summary.ses$inc_over100k_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,58])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$100k and over",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_over100k_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,58]))   # SD = sqrt(var)

summary.ses$age15to24_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "15-24", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "15-24", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,59] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$age.f2 == "15-24" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "15-24" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age15to24_MET.dur_mean[3])^2} # error calc
summary.ses$age15to24_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,59])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "15-24",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age15to24_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,59])) # SD = sqrt(var)

summary.ses$age25to34_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "25-34", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "25-34", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,60] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$age.f2 == "25-34" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "25-34" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age25to34_MET.dur_mean[3])^2} # error calc
summary.ses$age25to34_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,60])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "25-34",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age25to34_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,60])) # SD = sqrt(var)

summary.ses$age35to44_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "35-44", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "35-44", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,61] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$age.f2 == "35-44" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "35-44" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age35to44_MET.dur_mean[3])^2} # error calc
summary.ses$age35to44_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,61])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "35-44",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age35to44_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,61])) # SD = sqrt(var)

summary.ses$age45to54_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "45-54", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "45-54", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,62] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$age.f2 == "45-54" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "45-54" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age45to54_MET.dur_mean[3])^2} # error calc
summary.ses$age45to54_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,62])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "45-54",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age45to54_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,62])) # SD = sqrt(var)

summary.ses$age55to64_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "55-64", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "55-64", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,63] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$age.f2 == "55-64" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "55-64" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age55to64_MET.dur_mean[3])^2} # error calc
summary.ses$age55to64_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,63])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "55-64",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age55to64_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,63])) # SD = sqrt(var)

summary.ses$age65plus_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "65 and over", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "65 and over", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,64] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$age.f2 == "65 and over" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "65 and over" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age65plus_MET.dur_mean[3])^2} # error calc
summary.ses$age65plus_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,64])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "65 and over",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age65plus_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,64])) # SD = sqrt(var)

summary.ses$wht_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 == "White", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 == "White", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,65] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$race.f2 == "White" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 == "White" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$wht_MET.dur_mean[3])^2} # error calc
summary.ses$wht_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,65])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 == "White",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$wht_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,65]))

summary.ses$blk_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 == "Black", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 == "Black", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,66] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$race.f2 == "Black" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 == "Black" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$blk_MET.dur_mean[3])^2} # error calc
summary.ses$blk_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,66])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 == "Black",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$blk_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,66]))

summary.ses$asn_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 == "Asian", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 == "Asian", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,67] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$race.f2 == "Asian" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 == "Asian" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$blk_MET.dur_mean[3])^2} # error calc
summary.ses$asn_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,67])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 == "Asian",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$asn_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,67]))

summary.ses$race_oth_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,68] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$race_oth_MET.dur_mean[3])^2} # error calc
summary.ses$race_oth_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,68])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$race_oth_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,68]))

summary.ses$men_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$sex.f == "Male", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$sex.f == "Male", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,69] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$sex.f == "Male" & !is.na(meta.f27.agg$sex.f)], meta.f27.agg[meta.f27.agg$sex.f == "Male" & !is.na(meta.f27.agg$sex.f),j]) - summary.ses$men_MET.dur_mean[3])^2} # error calc
summary.ses$men_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,69])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$sex.f == "Male",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$men_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,69]))

summary.ses$wom_MET.dur_mean[3] <- sum(ifelse(meta.f27.agg$sex.f == "Female", meta.f27.agg$MET.dur * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$sex.f == "Female", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,70] <- (weighted.mean(meta.f27.agg$MET.dur[meta.f27.agg$sex.f == "Female" & !is.na(meta.f27.agg$sex.f)], meta.f27.agg[meta.f27.agg$sex.f == "Female" & !is.na(meta.f27.agg$sex.f),j]) - summary.ses$wom_MET.dur_mean[3])^2} # error calc
summary.ses$wom_MET.dur_SEM[3] <- sqrt((4/160) * sum(err[,70])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$sex.f == "Female",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$wom_MET.dur_SD[3] <- sqrt((4/160) * sum(err[,70]))


# MET
summary.ses$MET_mean[3] <- sum(meta.f27.agg$MET * meta.f27.agg$weight)  / sum(meta.f27.agg$weight)
for(i in 1:160){j <- i + rep.start
err[i,71] <- (weighted.mean(meta.f27.agg$MET,meta.f27.agg[,j]) - summary.ses$MET_mean[3])^2} # error calc
summary.ses$MET_SEM[3] <- sqrt((4/160) * sum(err[,71])) / sqrt(summary.ses$n_per[3])  # SEM = sqrt(var) / sqrt(n)
summary.ses$MET_SD[3] <- sqrt((4/160) * sum(err[,71]))  # SD = sqrt(var)

summary.ses$pov_MET_mean[3] <- sum(ifelse(meta.f27.agg$poverty.f == "Below Poverty", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$poverty.f == "Below Poverty", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,72] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$poverty.f == "Below Poverty" & !is.na(meta.f27.agg$poverty.f)],meta.f27.agg[meta.f27.agg$poverty.f == "Below Poverty" & !is.na(meta.f27.agg$poverty.f),j]) - summary.ses$pov_MET_mean[3])^2} # error calc
summary.ses$pov_MET_SEM[3] <- sqrt((4/160) * sum(err[,72])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$poverty.f == "Below Poverty",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$pov_MET_SD[3] <- sqrt((4/160) * sum(err[,72]))   # SD = sqrt(var)

summary.ses$non_pov_MET_mean[3] <- sum(ifelse(meta.f27.agg$poverty.f == "Above Poverty", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$poverty.f == "Above Poverty", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,73] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$poverty.f == "Above Poverty" & !is.na(meta.f27.agg$poverty.f)],meta.f27.agg[meta.f27.agg$poverty.f == "Above Poverty" & !is.na(meta.f27.agg$poverty.f),j]) - summary.ses$non_pov_MET_mean[3])^2} # error calc
summary.ses$non_pov_MET_SEM[3] <- sqrt((4/160) * sum(err[,73])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$poverty.f == "Above Poverty",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$non_pov_MET_SD[3] <- sqrt((4/160) * sum(err[,73]))   # SD = sqrt(var)

summary.ses$inc_less15k_MET_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "Less than $15k", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "Less than $15k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,74] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$income.f == "Less than $15k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "Less than $15k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_less15k_MET_mean[3])^2} # error calc
summary.ses$inc_less15k_MET_SEM[3] <- sqrt((4/160) * sum(err[,74])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "Less than $15k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_less15k_MET_SD[3] <- sqrt((4/160) * sum(err[,74]))   # SD = sqrt(var)

summary.ses$inc_15to30k_MET_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$15k to $30k", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$15k to $30k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,75] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$income.f == "$15k to $30k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$15k to $30k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_15to30k_MET_mean[3])^2} # error calc
summary.ses$inc_15to30k_MET_SEM[3] <- sqrt((4/160) * sum(err[,75])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$15k to $30k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_15to30k_MET_SD[3] <- sqrt((4/160) * sum(err[,75]))   # SD = sqrt(var)

summary.ses$inc_30to50k_MET_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$30k to $50k", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$30k to $50k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,76] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$income.f == "$30k to $50k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$30k to $50k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_30to50k_MET_mean[3])^2} # error calc
summary.ses$inc_30to50k_MET_SEM[3] <- sqrt((4/160) * sum(err[,76])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$30k to $50k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_30to50k_MET_SD[3] <- sqrt((4/160) * sum(err[,76]))   # SD = sqrt(var)

summary.ses$inc_50to75k_MET_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$50k to $75k", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$50k to $75k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,77] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$income.f == "$50k to $75k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$50k to $75k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_50to75k_MET_mean[3])^2} # error calc
summary.ses$inc_50to75k_MET_SEM[3] <- sqrt((4/160) * sum(err[,77])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$50k to $75k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_50to75k_MET_SD[3] <- sqrt((4/160) * sum(err[,77]))   # SD = sqrt(var)

summary.ses$inc_75to100k_MET_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$75k to $100k", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$75k to $100k", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,78] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$income.f == "$75k to $100k" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$75k to $100k" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_75to100k_MET_mean[3])^2} # error calc
summary.ses$inc_75to100k_MET_SEM[3] <- sqrt((4/160) * sum(err[,78])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$75k to $100k",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_75to100k_MET_SD[3] <- sqrt((4/160) * sum(err[,78]))   # SD = sqrt(var)

summary.ses$inc_over100k_MET_mean[3] <- sum(ifelse(meta.f27.agg$income.f == "$100k and over", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$income.f == "$100k and over", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,79] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$income.f == "$100k and over" & !is.na(meta.f27.agg$income.f)],meta.f27.agg[meta.f27.agg$income.f == "$100k and over" & !is.na(meta.f27.agg$income.f),j]) - summary.ses$inc_over100k_MET_mean[3])^2} # error calc
summary.ses$inc_over100k_MET_SEM[3] <- sqrt((4/160) * sum(err[,79])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$income.f == "$100k and over",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$inc_over100k_MET_SD[3] <- sqrt((4/160) * sum(err[,79]))   # SD = sqrt(var)

summary.ses$age15to24_MET_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "15-24", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "15-24", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,80] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$age.f2 == "15-24" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "15-24" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age15to24_MET_mean[3])^2} # error calc
summary.ses$age15to24_MET_SEM[3] <- sqrt((4/160) * sum(err[,80])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "15-24",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age15to24_MET_SD[3] <- sqrt((4/160) * sum(err[,80])) # SD = sqrt(var)

summary.ses$age25to34_MET_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "25-34", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "25-34", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,81] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$age.f2 == "25-34" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "25-34" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age25to34_MET_mean[3])^2} # error calc
summary.ses$age25to34_MET_SEM[3] <- sqrt((4/160) * sum(err[,81])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "25-34",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age25to34_MET_SD[3] <- sqrt((4/160) * sum(err[,81])) # SD = sqrt(var)

summary.ses$age35to44_MET_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "35-44", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "35-44", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,82] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$age.f2 == "35-44" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "35-44" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age35to44_MET_mean[3])^2} # error calc
summary.ses$age35to44_MET_SEM[3] <- sqrt((4/160) * sum(err[,82])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "35-44",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age35to44_MET_SD[3] <- sqrt((4/160) * sum(err[,82])) # SD = sqrt(var)

summary.ses$age45to54_MET_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "45-54", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "45-54", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,83] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$age.f2 == "45-54" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "45-54" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age45to54_MET_mean[3])^2} # error calc
summary.ses$age45to54_MET_SEM[3] <- sqrt((4/160) * sum(err[,83])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "45-54",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age45to54_MET_SD[3] <- sqrt((4/160) * sum(err[,83])) # SD = sqrt(var)

summary.ses$age55to64_MET_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "55-64", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "55-64", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,84] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$age.f2 == "55-64" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "55-64" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age55to64_MET_mean[3])^2} # error calc
summary.ses$age55to64_MET_SEM[3] <- sqrt((4/160) * sum(err[,84])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "55-64",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age55to64_MET_SD[3] <- sqrt((4/160) * sum(err[,84])) # SD = sqrt(var)

summary.ses$age65plus_MET_mean[3] <- sum(ifelse(meta.f27.agg$age.f2 == "65 and over", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$age.f2 == "65 and over", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,85] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$age.f2 == "65 and over" & !is.na(meta.f27.agg$age.f2)], meta.f27.agg[meta.f27.agg$age.f2 == "65 and over" & !is.na(meta.f27.agg$age.f2),j]) - summary.ses$age65plus_MET_mean[3])^2} # error calc
summary.ses$age65plus_MET_SEM[3] <- sqrt((4/160) * sum(err[,85])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$age.f2 == "65 and over",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$age65plus_MET_SD[3] <- sqrt((4/160) * sum(err[,85])) # SD = sqrt(var)

summary.ses$wht_MET_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 == "White", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 == "White", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,86] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$race.f2 == "White" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 == "White" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$wht_MET_mean[3])^2} # error calc
summary.ses$wht_MET_SEM[3] <- sqrt((4/160) * sum(err[,86])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 == "White",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$wht_MET_SD[3] <- sqrt((4/160) * sum(err[,86]))

summary.ses$blk_MET_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 == "Black", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 == "Black", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,87] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$race.f2 == "Black" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 == "Black" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$blk_MET_mean[3])^2} # error calc
summary.ses$blk_MET_SEM[3] <- sqrt((4/160) * sum(err[,87])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 == "Black",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$blk_MET_SD[3] <- sqrt((4/160) * sum(err[,87]))

summary.ses$asn_MET_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 == "Asian", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 == "Asian", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,88] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$race.f2 == "Asian" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 == "Asian" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$blk_MET_mean[3])^2} # error calc
summary.ses$asn_MET_SEM[3] <- sqrt((4/160) * sum(err[,88])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 == "Asian",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$asn_MET_SD[3] <- sqrt((4/160) * sum(err[,88]))

summary.ses$race_oth_MET_mean[3] <- sum(ifelse(meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,89] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], meta.f27.agg[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2),j]) - summary.ses$race_oth_MET_mean[3])^2} # error calc
summary.ses$race_oth_MET_SEM[3] <- sqrt((4/160) * sum(err[,89])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$race_oth_MET_SD[3] <- sqrt((4/160) * sum(err[,89]))

summary.ses$men_MET_mean[3] <- sum(ifelse(meta.f27.agg$sex.f == "Male", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$sex.f == "Male", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,90] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$sex.f == "Male" & !is.na(meta.f27.agg$sex.f)], meta.f27.agg[meta.f27.agg$sex.f == "Male" & !is.na(meta.f27.agg$sex.f),j]) - summary.ses$men_MET_mean[3])^2} # error calc
summary.ses$men_MET_SEM[3] <- sqrt((4/160) * sum(err[,90])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$sex.f == "Male",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$men_MET_SD[3] <- sqrt((4/160) * sum(err[,90]))

summary.ses$wom_MET_mean[3] <- sum(ifelse(meta.f27.agg$sex.f == "Female", meta.f27.agg$MET * meta.f27.agg$weight, 0), na.rm = T) / sum(ifelse(meta.f27.agg$sex.f == "Female", meta.f27.agg$weight, 0), na.rm = T)
for(i in 1:160){j <- i + rep.start
err[i,91] <- (weighted.mean(meta.f27.agg$MET[meta.f27.agg$sex.f == "Female" & !is.na(meta.f27.agg$sex.f)], meta.f27.agg[meta.f27.agg$sex.f == "Female" & !is.na(meta.f27.agg$sex.f),j]) - summary.ses$wom_MET_mean[3])^2} # error calc
summary.ses$wom_MET_SEM[3] <- sqrt((4/160) * sum(err[,91])) / sqrt(nrow(meta.f27.agg[meta.f27.agg$sex.f == "Female",]))  # SEM = sqrt(var) / sqrt(n)
summary.ses$wom_MET_SD[3] <- sqrt((4/160) * sum(err[,91]))



# ***TEST VARIOUS MODELS TO DETERMINE METHODS FOR SIGNIFICANCE TESTING***

#atus survey design
atus.svy.o27 <- svrepdesign(id = ~1, data = meta.f27.agg, weights = meta.f27.agg$weight, repweights = meta.f27.agg[,16:175])
atus.svy.all <- svrepdesign(id = ~1, data = meta.s.agg, weights = meta.s.agg$weight, repweights = meta.s.agg[,16:175])

# create and run list of different models
models <- list( 
  lm1 = lm(MET.exp27.trns ~ poverty.f + age.f2 + sex.f + race.f2 + sprawl + 0, data = meta.f27.agg, weights = meta.f27.agg$weight), # base weighted least squares (WLS) linear model (LM)
  lm.new = lm(MET.exp27.trns ~ poverty.f + age.f2 + sex.f + race.f2 + sprawl + 0, data = meta.f27.agg, weights = meta.f27.agg$weight),
  glm1 = glm( MET.exp27.trns ~ poverty.f + age.f2 + sex.f + race.f2 + sprawl + 0, data = meta.f27.agg, weights = meta.f27.agg$weight, family=gaussian()), # WLS general linear model (GLM)
  svy_glm1 = svyglm(MET.exp27.trns ~ poverty.f + age.f2 + sex.f + race.f2 + sprawl + 0, design = atus.svy.o27, family = gaussian()), # WLS GLM with survey design adjustment
  svy_glm2 = svyglm(MET.exp27.trns ~ poverty.f + age.f2 + sex.f + race.f2 + sprawl + 0, design = atus.svy.o27, family = Gamma()), # WLS GLM with survey design adjustment and gamma distr
  felm1 = felm(MET.exp27.trns ~ poverty.f + age.f2 + sex.f + race.f2 + sprawl + 0 | date, data = meta.f27.agg), # fixed/mixed effects linear model (FELM)
  felm2 = felm(MET.exp27.trns ~ poverty.f + age.f2 + sex.f + race.f2 + sprawl + 0 | season, data = meta.f27.agg), # fixed/mixed effects linear model (FELM)
  felm3 = felm(MET.exp27.trns ~ poverty.f + age.f2 + sex.f + race.f2 + sprawl + 0 | msa.f, data = meta.f27.agg), # fixed/mixed effects linear model (FELM)
  felm4 = felm(MET.exp27.trns ~ poverty.f + age.f2 + sex.f + race.f2 + sprawl + 0 | date + season + msa.f, data = meta.f27.agg), # fixed/mixed effects linear model (FELM)
  felm_wtg = felm(MET.exp27.trns ~ age.f2 + sex.f + race.f2 + 0 | date + season + msa.f + sprawl + BA.Climate.Zone, data = meta.f27.agg, weights = meta.f27.agg$weight) # fixed/mixed effects linear model (FELM)
)

# gather results
results <- do.call("rbind", lapply( names(models), function(n) cbind(model=n, tidy(models[[n]])) )) %>%
  gather(stat, value, -model, -term)

# chekc if point estimates are same
results %>% filter(stat=="estimate") %>% 
  select(model, term, value) %>%
  spread(term, value)

# Standard Errors
results %>% filter(stat=="std.error") %>%
  select(model, term, value) %>%
  spread(term, value)

# p-values
results %>% filter(stat=="p.value") %>%
  mutate(p=format.pval(value)) %>%
  select(model, term, p) %>%
  spread(term, p)

# recheck survey glm approach
o27_svy_glm.MET.exp27  <- svyglm(MET.exp27 ~ poverty.f + age.f2 + sex.f + race.f2 + 0, design = atus.svy.o27, family = Gamma())
o27_svy_glm.MET.dur  <- svyglm(MET.dur ~ poverty.f + age.f2 + sex.f + race.f2 + 0, design = atus.svy.o27, family = Gamma())
o27_svy_glm.exp27  <- svyglm(exp27 ~ poverty.f + age.f2 + sex.f + race.f2 + 0, design = atus.svy.o27, family = Gamma())
o27_svy_glm.MET  <- svyglm(MET ~ poverty.f + age.f2 + sex.f + race.f2 + 0, design = atus.svy.o27, family = Gamma())


#all_svy_glm <- svyglm(outdoors ~ poverty.f + age.f2 + sex.f + race.f2 + 0, design = atus.svy.all, family = binomial())

summary(o27_svy_glm)
summary(all_svy_glm)
summary(anova(o27_svy_glm))
aov_all <- anova(all_svy_glm)
all_svy_glm$residuals

# check normality of residuals / Shapiro-Wilk test
par(mfrow = c(2, 2))
plot(o27_svy_glm)
par(mfrow = c(2, 2))
plot(all_svy_glm)
shapiro.test(o27_svy_glm$residuals) # REJECT HYP THAT DATA IS NORMALLY DISTRIBUTIED
shapiro.test(meta.f27.agg$MET.exp27) # REJECT HYP THAT DATA IS NORMALLY DISTRIBUTIED

# do transformation of MET.exp27 b/c it is non normal
bc.MET.exp27 <- boxcox(o27_svy_glm)
bc.MET.dur <- boxcox(o27_svy_glm)
bc.exp27 <- boxcox(o27_svy_glm)
bc.MET <- boxcox(o27_svy_glm)

lambda.MET.exp27 <- bc.MET.exp27$x[which.max(bc.MET.exp27$y)]
lambda.MET.dur <- bc.MET.dur$x[which.max(bc.MET.dur$y)]
lambda.exp27 <- bc.exp27$x[which.max(bc.exp27$y)]
lambda.MET <- bc.MET$x[which.max(bc.MET$y)]

# create new transform function
powerTransform2 <- function(y, lambda1, lambda2 = NULL, method = "boxcox") {
  
  boxcoxTrans <- function(x, lam1, lam2 = NULL) {
    
    # if we set lambda2 to zero, it becomes the one parameter transformation
    lam2 <- ifelse(is.null(lam2), 0, lam2)
    
    if (lam1 == 0L) {
      log(y + lam2)
    } else {
      (((y + lam2)^lam1) - 1) / lam1
    }
  }
  
  switch(method
         , boxcox = boxcoxTrans(y, lambda1, lambda2)
         , tukey = y^lambda1
  )
}

# Re-run linear models with transformations
# Box-Cox: ( ( y ^ lambda - 1) / lambda) ~ x 
# Blackman-Tukey: <- (y ^ lambda) ~ x
lm.old <- lm(MET.exp27 ~ poverty.f + age.f2 + sex.f + race.f2 + sprawl + 0, data = meta.f27.agg, weights = meta.f27.agg$weight)
lm.tukey <-  lm((MET.exp27^lambda) ~ poverty.f + age.f2 + sex.f + race.f2 + sprawl + 0, data = meta.f27.agg, weights = meta.f27.agg$weight)
lm.boxcox <- lm(((MET.exp27^lambda-1)/lambda) ~ poverty.f + age.f2 + sex.f + race.f2 + sprawl + 0, data = meta.f27.agg, weights = meta.f27.agg$weight)

# Check QQ-plots to see if normailty exists with transformations
op <- par(pty = "s", mfrow = c(1, 3))
qqnorm(lm.old$residuals); qqline(lm.old$residuals)
qqnorm(lm.tukey$residuals); qqline(lm.tukey$residuals)  # THIS IS BEST TRANSFORM
qqnorm(lm.boxcox$residuals); qqline(lm.boxcox$residuals)
par(op)

# plot transformed data
plot(meta.f27.agg$MET.exp27^lambda)

# RESULT: use TUKEY TRANSFORM ON MET.EXP27 TO NORMALIZE

###

# ***TEST FOR SIGNIFICANCE ON TRANSFORMED DATA****

# create transformed exp27
meta.f27.agg$exp27.trns <- meta.f27.agg$exp27 ^ lambda.exp27

#black vs else
summary.ses$blk_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$race.f2 == "Black"], y = meta.f27.agg$exp27.trns[meta.f27.agg$race.f2 != "Black"], 
           weight = meta.f27.agg$weight[meta.f27.agg$race.f2 == "Black"], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 != "Black" & !is.na(meta.f27.agg$race.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#white vs else
summary.ses$wht_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$race.f2 == "White"], y = meta.f27.agg$exp27.trns[meta.f27.agg$race.f2 != "White"], 
           weight = meta.f27.agg$weight[meta.f27.agg$race.f2 == "White"], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#asian vs else
summary.ses$asn_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$race.f2 == "Asian"], y = meta.f27.agg$exp27.trns[meta.f27.agg$race.f2 != "Asian"], 
                      weight = meta.f27.agg$weight[meta.f27.agg$race.f2 == "Asian"], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 != "Asian" & !is.na(meta.f27.agg$race.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#other race vs else
summary.ses$race_oth_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], y = meta.f27.agg$exp27.trns[meta.f27.agg$race.f2 == "Asian" | meta.f27.agg$race.f2 == "Black" | meta.f27.agg$race.f2 == "White"], 
                      weight = meta.f27.agg$weight[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 == "Asian" | meta.f27.agg$race.f2 == "Black" | meta.f27.agg$race.f2 == "White"], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#65+ age vs else
summary.ses$age65plus_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$age.f2 == "65 and over"], y = meta.f27.agg$exp27.trns[meta.f27.agg$age.f2 != "65 and over"], 
           weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "65 and over"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "65 and over" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 55 to 64 vs else
summary.ses$age55to64_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$age.f2 == "55-64"], y = meta.f27.agg$exp27.trns[meta.f27.agg$age.f2 != "55-64"], 
           weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "55-64"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "55-64" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 45 to 54 vs else
summary.ses$age45to54_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$age.f2 == "45-54"], y = meta.f27.agg$exp27.trns[meta.f27.agg$age.f2 != "45-54"], 
           weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "45-54"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "45-54" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 35 to 44 vs else
summary.ses$age35to44_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$age.f2 == "35-44"], y = meta.f27.agg$exp27.trns[meta.f27.agg$age.f2 != "35-44"], 
           weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "35-44"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "35-44" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 25 to 34 vs else
summary.ses$age25to34_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$age.f2 == "25-34"], y = meta.f27.agg$exp27.trns[meta.f27.agg$age.f2 != "25-34"], 
           weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "25-34"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "25-34" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 15 to 24 vs else
summary.ses$age15to24_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$age.f2 == "15-24"], y = meta.f27.agg$exp27.trns[meta.f27.agg$age.f2 != "15-24"], 
           weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "15-24"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "15-24" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#male vs female
summary.ses$men_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$sex.f == "Male"], y = meta.f27.agg$exp27.trns[meta.f27.agg$sex.f != "Male"], 
           weight = meta.f27.agg$weight[meta.f27.agg$sex.f == "Male"], weighty = meta.f27.agg$weight[meta.f27.agg$sex.f != "Male" & !is.na(meta.f27.agg$sex.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#female vs male
summary.ses$wom_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$sex.f == "Female"], y = meta.f27.agg$exp27.trns[meta.f27.agg$sex.f != "Female"], 
           weight = meta.f27.agg$weight[meta.f27.agg$sex.f == "Female"], weighty = meta.f27.agg$weight[meta.f27.agg$sex.f != "Female" & !is.na(meta.f27.agg$sex.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income < 15k
summary.ses$inc_less15k_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$income.f == "Less than $15k"], y = meta.f27.agg$exp27.trns[meta.f27.agg$income.f != "Less than $15k"], 
           weight = meta.f27.agg$weight[meta.f27.agg$income.f == "Less than $15k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "Less than $15k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income < 15k
summary.ses$inc_15to30k_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$income.f == "$15k to $30k"], y = meta.f27.agg$exp27.trns[meta.f27.agg$income.f != "$15k to $30k"], 
           weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$15k to $30k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$15k to $30k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 30k to 50k
summary.ses$inc_30to50k_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$income.f == "$30k to $50k"], y = meta.f27.agg$exp27.trns[meta.f27.agg$income.f != "$30k to $50k"], 
           weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$30k to $50k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$30k to $50k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 50k to 75k
summary.ses$inc_50to75k_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$income.f == "$50k to $75k"], y = meta.f27.agg$exp27.trns[meta.f27.agg$income.f != "$50k to $75k"], 
           weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$50k to $75k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$50k to $75k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 75k to 100k
summary.ses$inc_75to100k_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$income.f == "$75k to $100k"], y = meta.f27.agg$exp27.trns[meta.f27.agg$income.f != "$75k to $100k"], 
           weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$75k to $100k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$75k to $100k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 100k plus
summary.ses$inc_over100k_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$income.f == "$100k and over"], y = meta.f27.agg$exp27.trns[meta.f27.agg$income.f != "$100k and over"], 
           weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$100k and over"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$100k and over" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#poverty vs non-poverty
summary.ses$pov_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$poverty.f == "Below Poverty"], y = meta.f27.agg$exp27.trns[meta.f27.agg$poverty.f != "Below Poverty"], 
           weight = meta.f27.agg$weight[meta.f27.agg$poverty.f == "Below Poverty"], weighty = meta.f27.agg$weight[meta.f27.agg$poverty.f != "Below Poverty" & !is.na(meta.f27.agg$poverty.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#poverty vs non-poverty
summary.ses$non_pov_exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$exp27.trns[meta.f27.agg$poverty.f == "Above Poverty"], y = meta.f27.agg$exp27.trns[meta.f27.agg$poverty.f != "Above Poverty"], 
           weight = meta.f27.agg$weight[meta.f27.agg$poverty.f == "Above Poverty"], weighty = meta.f27.agg$weight[meta.f27.agg$poverty.f != "Above Poverty" & !is.na(meta.f27.agg$poverty.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])


# create transformed MET.dur
meta.f27.agg$MET.dur.trns <- meta.f27.agg$MET.dur ^ lambda.MET.dur

#black vs else
summary.ses$blk_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$race.f2 == "Black"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$race.f2 != "Black"], 
                                                       weight = meta.f27.agg$weight[meta.f27.agg$race.f2 == "Black"], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 != "Black" & !is.na(meta.f27.agg$race.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#white vs else
summary.ses$wht_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$race.f2 == "White"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$race.f2 != "White"], 
                                                       weight = meta.f27.agg$weight[meta.f27.agg$race.f2 == "White"], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#asian vs else
summary.ses$asn_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$race.f2 == "Asian"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$race.f2 != "Asian"], 
                                                       weight = meta.f27.agg$weight[meta.f27.agg$race.f2 == "Asian"], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 != "Asian" & !is.na(meta.f27.agg$race.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#other race vs else
summary.ses$race_oth_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$race.f2 == "Asian" | meta.f27.agg$race.f2 == "Black" | meta.f27.agg$race.f2 == "White"], 
                                                            weight = meta.f27.agg$weight[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 == "Asian" | meta.f27.agg$race.f2 == "Black" | meta.f27.agg$race.f2 == "White"], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#65+ age vs else
summary.ses$age65plus_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$age.f2 == "65 and over"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$age.f2 != "65 and over"], 
                                                             weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "65 and over"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "65 and over" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 55 to 64 vs else
summary.ses$age55to64_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$age.f2 == "55-64"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$age.f2 != "55-64"], 
                                                             weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "55-64"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "55-64" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 45 to 54 vs else
summary.ses$age45to54_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$age.f2 == "45-54"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$age.f2 != "45-54"], 
                                                             weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "45-54"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "45-54" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 35 to 44 vs else
summary.ses$age35to44_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$age.f2 == "35-44"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$age.f2 != "35-44"], 
                                                             weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "35-44"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "35-44" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 25 to 34 vs else
summary.ses$age25to34_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$age.f2 == "25-34"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$age.f2 != "25-34"], 
                                                             weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "25-34"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "25-34" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 15 to 24 vs else
summary.ses$age15to24_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$age.f2 == "15-24"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$age.f2 != "15-24"], 
                                                             weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "15-24"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "15-24" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#male vs female
summary.ses$men_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$sex.f == "Male"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$sex.f != "Male"], 
                                                       weight = meta.f27.agg$weight[meta.f27.agg$sex.f == "Male"], weighty = meta.f27.agg$weight[meta.f27.agg$sex.f != "Male" & !is.na(meta.f27.agg$sex.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#female vs male
summary.ses$wom_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$sex.f == "Female"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$sex.f != "Female"], 
                                                       weight = meta.f27.agg$weight[meta.f27.agg$sex.f == "Female"], weighty = meta.f27.agg$weight[meta.f27.agg$sex.f != "Female" & !is.na(meta.f27.agg$sex.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income < 15k
summary.ses$inc_less15k_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$income.f == "Less than $15k"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$income.f != "Less than $15k"], 
                                                               weight = meta.f27.agg$weight[meta.f27.agg$income.f == "Less than $15k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "Less than $15k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income < 15k
summary.ses$inc_15to30k_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$income.f == "$15k to $30k"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$income.f != "$15k to $30k"], 
                                                               weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$15k to $30k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$15k to $30k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 30k to 50k
summary.ses$inc_30to50k_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$income.f == "$30k to $50k"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$income.f != "$30k to $50k"], 
                                                               weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$30k to $50k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$30k to $50k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 50k to 75k
summary.ses$inc_50to75k_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$income.f == "$50k to $75k"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$income.f != "$50k to $75k"], 
                                                               weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$50k to $75k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$50k to $75k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 75k to 100k
summary.ses$inc_75to100k_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$income.f == "$75k to $100k"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$income.f != "$75k to $100k"], 
                                                                weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$75k to $100k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$75k to $100k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 100k plus
summary.ses$inc_over100k_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$income.f == "$100k and over"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$income.f != "$100k and over"], 
                                                                weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$100k and over"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$100k and over" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#poverty vs non-poverty
summary.ses$pov_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$poverty.f == "Below Poverty"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$poverty.f != "Below Poverty"], 
                                                       weight = meta.f27.agg$weight[meta.f27.agg$poverty.f == "Below Poverty"], weighty = meta.f27.agg$weight[meta.f27.agg$poverty.f != "Below Poverty" & !is.na(meta.f27.agg$poverty.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#poverty vs non-poverty
summary.ses$non_pov_MET.dur_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.dur.trns[meta.f27.agg$poverty.f == "Above Poverty"], y = meta.f27.agg$MET.dur.trns[meta.f27.agg$poverty.f != "Above Poverty"], 
                                                           weight = meta.f27.agg$weight[meta.f27.agg$poverty.f == "Above Poverty"], weighty = meta.f27.agg$weight[meta.f27.agg$poverty.f != "Above Poverty" & !is.na(meta.f27.agg$poverty.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

# create transformed MET
meta.f27.agg$MET.trns <- meta.f27.agg$MET ^ lambda.MET

#black vs else
summary.ses$blk_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$race.f2 == "Black"], y = meta.f27.agg$MET.trns[meta.f27.agg$race.f2 != "Black"], 
                                                         weight = meta.f27.agg$weight[meta.f27.agg$race.f2 == "Black"], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 != "Black" & !is.na(meta.f27.agg$race.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#white vs else
summary.ses$wht_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$race.f2 == "White"], y = meta.f27.agg$MET.trns[meta.f27.agg$race.f2 != "White"], 
                                                         weight = meta.f27.agg$weight[meta.f27.agg$race.f2 == "White"], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#asian vs else
summary.ses$asn_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$race.f2 == "Asian"], y = meta.f27.agg$MET.trns[meta.f27.agg$race.f2 != "Asian"], 
                                                         weight = meta.f27.agg$weight[meta.f27.agg$race.f2 == "Asian"], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 != "Asian" & !is.na(meta.f27.agg$race.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#other race vs else
summary.ses$race_oth_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], y = meta.f27.agg$MET.trns[meta.f27.agg$race.f2 == "Asian" | meta.f27.agg$race.f2 == "Black" | meta.f27.agg$race.f2 == "White"], 
                                                              weight = meta.f27.agg$weight[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 == "Asian" | meta.f27.agg$race.f2 == "Black" | meta.f27.agg$race.f2 == "White"], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#65+ age vs else
summary.ses$age65plus_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$age.f2 == "65 and over"], y = meta.f27.agg$MET.trns[meta.f27.agg$age.f2 != "65 and over"], 
                                                               weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "65 and over"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "65 and over" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 55 to 64 vs else
summary.ses$age55to64_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$age.f2 == "55-64"], y = meta.f27.agg$MET.trns[meta.f27.agg$age.f2 != "55-64"], 
                                                               weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "55-64"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "55-64" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 45 to 54 vs else
summary.ses$age45to54_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$age.f2 == "45-54"], y = meta.f27.agg$MET.trns[meta.f27.agg$age.f2 != "45-54"], 
                                                               weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "45-54"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "45-54" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 35 to 44 vs else
summary.ses$age35to44_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$age.f2 == "35-44"], y = meta.f27.agg$MET.trns[meta.f27.agg$age.f2 != "35-44"], 
                                                               weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "35-44"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "35-44" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 25 to 34 vs else
summary.ses$age25to34_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$age.f2 == "25-34"], y = meta.f27.agg$MET.trns[meta.f27.agg$age.f2 != "25-34"], 
                                                               weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "25-34"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "25-34" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 15 to 24 vs else
summary.ses$age15to24_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$age.f2 == "15-24"], y = meta.f27.agg$MET.trns[meta.f27.agg$age.f2 != "15-24"], 
                                                               weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "15-24"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "15-24" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#male vs female
summary.ses$men_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$sex.f == "Male"], y = meta.f27.agg$MET.trns[meta.f27.agg$sex.f != "Male"], 
                                                         weight = meta.f27.agg$weight[meta.f27.agg$sex.f == "Male"], weighty = meta.f27.agg$weight[meta.f27.agg$sex.f != "Male" & !is.na(meta.f27.agg$sex.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#female vs male
summary.ses$wom_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$sex.f == "Female"], y = meta.f27.agg$MET.trns[meta.f27.agg$sex.f != "Female"], 
                                                         weight = meta.f27.agg$weight[meta.f27.agg$sex.f == "Female"], weighty = meta.f27.agg$weight[meta.f27.agg$sex.f != "Female" & !is.na(meta.f27.agg$sex.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income < 15k
summary.ses$inc_less15k_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$income.f == "Less than $15k"], y = meta.f27.agg$MET.trns[meta.f27.agg$income.f != "Less than $15k"], 
                                                                 weight = meta.f27.agg$weight[meta.f27.agg$income.f == "Less than $15k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "Less than $15k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income < 15k
summary.ses$inc_15to30k_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$income.f == "$15k to $30k"], y = meta.f27.agg$MET.trns[meta.f27.agg$income.f != "$15k to $30k"], 
                                                                 weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$15k to $30k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$15k to $30k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 30k to 50k
summary.ses$inc_30to50k_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$income.f == "$30k to $50k"], y = meta.f27.agg$MET.trns[meta.f27.agg$income.f != "$30k to $50k"], 
                                                                 weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$30k to $50k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$30k to $50k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 50k to 75k
summary.ses$inc_50to75k_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$income.f == "$50k to $75k"], y = meta.f27.agg$MET.trns[meta.f27.agg$income.f != "$50k to $75k"], 
                                                                 weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$50k to $75k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$50k to $75k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 75k to 100k
summary.ses$inc_75to100k_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$income.f == "$75k to $100k"], y = meta.f27.agg$MET.trns[meta.f27.agg$income.f != "$75k to $100k"], 
                                                                  weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$75k to $100k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$75k to $100k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 100k plus
summary.ses$inc_over100k_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$income.f == "$100k and over"], y = meta.f27.agg$MET.trns[meta.f27.agg$income.f != "$100k and over"], 
                                                                  weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$100k and over"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$100k and over" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#poverty vs non-poverty
summary.ses$pov_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$poverty.f == "Below Poverty"], y = meta.f27.agg$MET.trns[meta.f27.agg$poverty.f != "Below Poverty"], 
                                                         weight = meta.f27.agg$weight[meta.f27.agg$poverty.f == "Below Poverty"], weighty = meta.f27.agg$weight[meta.f27.agg$poverty.f != "Below Poverty" & !is.na(meta.f27.agg$poverty.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#poverty vs non-poverty
summary.ses$non_pov_MET_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.trns[meta.f27.agg$poverty.f == "Above Poverty"], y = meta.f27.agg$MET.trns[meta.f27.agg$poverty.f != "Above Poverty"], 
                                                             weight = meta.f27.agg$weight[meta.f27.agg$poverty.f == "Above Poverty"], weighty = meta.f27.agg$weight[meta.f27.agg$poverty.f != "Above Poverty" & !is.na(meta.f27.agg$poverty.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])



# create transformed MET.exp27
meta.f27.agg$MET.exp27.trns <- meta.f27.agg$MET.exp27 ^ lambda.MET.exp27

#black vs else
summary.ses$blk_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$race.f2 == "Black"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$race.f2 != "Black"], 
                                                           weight = meta.f27.agg$weight[meta.f27.agg$race.f2 == "Black"], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 != "Black" & !is.na(meta.f27.agg$race.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#white vs else
summary.ses$wht_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$race.f2 == "White"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$race.f2 != "White"], 
                                                           weight = meta.f27.agg$weight[meta.f27.agg$race.f2 == "White"], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#asian vs else
summary.ses$asn_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$race.f2 == "Asian"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$race.f2 != "Asian"], 
                                                           weight = meta.f27.agg$weight[meta.f27.agg$race.f2 == "Asian"], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 != "Asian" & !is.na(meta.f27.agg$race.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#other race vs else
summary.ses$race_oth_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$race.f2 == "Asian" | meta.f27.agg$race.f2 == "Black" | meta.f27.agg$race.f2 == "White"], 
                                                                weight = meta.f27.agg$weight[meta.f27.agg$race.f2 != "Asian" & meta.f27.agg$race.f2 != "Black" & meta.f27.agg$race.f2 != "White" & !is.na(meta.f27.agg$race.f2)], weighty = meta.f27.agg$weight[meta.f27.agg$race.f2 == "Asian" | meta.f27.agg$race.f2 == "Black" | meta.f27.agg$race.f2 == "White"], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#65+ age vs else
summary.ses$age65plus_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$age.f2 == "65 and over"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$age.f2 != "65 and over"], 
                                                                 weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "65 and over"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "65 and over" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 55 to 64 vs else
summary.ses$age55to64_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$age.f2 == "55-64"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$age.f2 != "55-64"], 
                                                                 weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "55-64"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "55-64" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 45 to 54 vs else
summary.ses$age45to54_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$age.f2 == "45-54"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$age.f2 != "45-54"], 
                                                                 weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "45-54"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "45-54" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 35 to 44 vs else
summary.ses$age35to44_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$age.f2 == "35-44"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$age.f2 != "35-44"], 
                                                                 weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "35-44"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "35-44" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 25 to 34 vs else
summary.ses$age25to34_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$age.f2 == "25-34"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$age.f2 != "25-34"], 
                                                                 weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "25-34"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "25-34" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#age 15 to 24 vs else
summary.ses$age15to24_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$age.f2 == "15-24"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$age.f2 != "15-24"], 
                                                                 weight = meta.f27.agg$weight[meta.f27.agg$age.f2 == "15-24"], weighty = meta.f27.agg$weight[meta.f27.agg$age.f2 != "15-24" & !is.na(meta.f27.agg$age.f2)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#male vs female
summary.ses$men_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$sex.f == "Male"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$sex.f != "Male"], 
                                                           weight = meta.f27.agg$weight[meta.f27.agg$sex.f == "Male"], weighty = meta.f27.agg$weight[meta.f27.agg$sex.f != "Male" & !is.na(meta.f27.agg$sex.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#female vs male
summary.ses$wom_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$sex.f == "Female"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$sex.f != "Female"], 
                                                           weight = meta.f27.agg$weight[meta.f27.agg$sex.f == "Female"], weighty = meta.f27.agg$weight[meta.f27.agg$sex.f != "Female" & !is.na(meta.f27.agg$sex.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income < 15k
summary.ses$inc_less15k_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$income.f == "Less than $15k"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$income.f != "Less than $15k"], 
                                                                   weight = meta.f27.agg$weight[meta.f27.agg$income.f == "Less than $15k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "Less than $15k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income < 15k
summary.ses$inc_15to30k_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$income.f == "$15k to $30k"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$income.f != "$15k to $30k"], 
                                                                   weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$15k to $30k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$15k to $30k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 30k to 50k
summary.ses$inc_30to50k_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$income.f == "$30k to $50k"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$income.f != "$30k to $50k"], 
                                                                   weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$30k to $50k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$30k to $50k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 50k to 75k
summary.ses$inc_50to75k_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$income.f == "$50k to $75k"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$income.f != "$50k to $75k"], 
                                                                   weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$50k to $75k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$50k to $75k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 75k to 100k
summary.ses$inc_75to100k_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$income.f == "$75k to $100k"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$income.f != "$75k to $100k"], 
                                                                    weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$75k to $100k"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$75k to $100k" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#income 100k plus
summary.ses$inc_over100k_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$income.f == "$100k and over"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$income.f != "$100k and over"], 
                                                                    weight = meta.f27.agg$weight[meta.f27.agg$income.f == "$100k and over"], weighty = meta.f27.agg$weight[meta.f27.agg$income.f != "$100k and over" & !is.na(meta.f27.agg$income.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#poverty vs non-poverty
summary.ses$pov_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$poverty.f == "Below Poverty"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$poverty.f != "Below Poverty"], 
                                                           weight = meta.f27.agg$weight[meta.f27.agg$poverty.f == "Below Poverty"], weighty = meta.f27.agg$weight[meta.f27.agg$poverty.f != "Below Poverty" & !is.na(meta.f27.agg$poverty.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])

#poverty vs non-poverty
summary.ses$non_pov_MET.exp27_mean[4] <- as.numeric(wtd.t.test(meta.f27.agg$MET.exp27.trns[meta.f27.agg$poverty.f == "Above Poverty"], y = meta.f27.agg$MET.exp27.trns[meta.f27.agg$poverty.f != "Above Poverty"], 
                                                               weight = meta.f27.agg$weight[meta.f27.agg$poverty.f == "Above Poverty"], weighty = meta.f27.agg$weight[meta.f27.agg$poverty.f != "Above Poverty" & !is.na(meta.f27.agg$poverty.f)], alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])



for(a in 1:dim(metros)[1]){ 
  pop <- subset(meta.f27.agg, meta.f27.agg$msa.f %in% metros$abbr[a])
  pop_else <- subset(meta.f27.agg, !(meta.f27.agg$msa.f %in% metros$abbr[a]))
  pop <- pop[,c('MET.exp27','weight')]
  pop_else <- pop_else[,c('MET.exp27','weight')]
  pop$MET.exp27 <- pop$MET.exp27 ^ lambda # normalize
  pop_else$MET.exp27 <- pop_else$MET.exp27 ^ lambda # normalize
  
  # t test
  metros$p_val_MET.exp27_mean[a] <- as.numeric(wtd.t.test(x = pop$MET.exp27, y = pop_else$MET.exp27, 
                                                          weight = pop$weight, weighty = pop_else$weight, alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])
  
}
  
for(a in 1:dim(metros)[1]){ 
  pop <- subset(meta.f27.agg, meta.f27.agg$msa.f %in% metros$abbr[a])
  pop_else <- subset(meta.f27.agg, !(meta.f27.agg$msa.f %in% metros$abbr[a]))
  pop <- pop[,c('exp27','weight')]
  pop_else <- pop_else[,c('exp27','weight')]
  pop$exp27 <- pop$exp27 ^ lambda # normalize
  pop_else$exp27 <- pop_else$exp27 ^ lambda # normalize
  
  # t test
  metros$p_val_exp27_mean[a] <- as.numeric(wtd.t.test(x = pop$exp27, y = pop_else$exp27, 
                                                          weight = pop$weight, weighty = pop_else$weight, alternative="two.tailed", bootse = TRUE, bootp = TRUE, bootn=1000, drops="pairwise")$coefficients[3])
  
}



# weighted proportions for major activity types and temperature thersholds
nrow(meta.s) # total unique IDs = total respondents

nrow(meta.s[meta.s$temp > 21 & meta.s$temp <= 27 & meta.s$outdoors == 0,])
nrow(meta.s[meta.s$temp > 21 & meta.s$temp <= 27 & meta.s$outdoors == 1,])

nrow(meta.s[meta.s$temp > 21 & meta.s$temp <= 27 & meta.s$outdoors == 0,])
nrow(meta.s[meta.s$temp > 27 & meta.s$temp <= 33 & meta.s$outdoors == 1,])

nrow(meta.s[meta.s$temp > 33 & meta.s$outdoors == 0,])
nrow(meta.s[meta.s$temp > 33 & meta.s$outdoors == 1,])

nrow(meta.f27.agg)



# write output of data
write.csv(metros,"20180529_metros_summary.csv")
write.csv(summary.ses,"20180529_ses_summary.csv")


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

# create population data for meta.f
# create blank df
meta.f.pop <- meta.f[,c('id','MET.dur','cuttemp','code.f2','weight','elderly','black_white','poverty.f','sex.f','msa.f')]
meta.f2 <- meta.f[,c('id','MET.dur','cuttemp','code.f2','weight','elderly','black_white','poverty.f','sex.f','msa.f')]

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


act.t <- plyr::ddply(.data=meta.f.pop2[!is.na(meta.f.pop2$MET.dur) & (meta.f.pop2$msa.f == "LA" | meta.f.pop2$msa.f == "RIV"),], 
                 .(cuttemp,code.f3), 
                 plyr::summarize, 
                 n=length(!is.na(code.f3)))

act.t$n <- paste("n =",act.t$n)
act.t <- act.t[!is.na(act.t$code.f3),]

# create activity x MET-time by cuttemp plot
p26.1 <- ggplot(data=meta.f.pop2[!is.na(meta.f.pop2$MET.dur) & (meta.f.pop2$msa.f == "LA" | meta.f.pop2$msa.f == "RIV") & !is.na(meta.f.pop2$code.f3),], aes(x=cuttemp, y=MET.dur, fill=cuttemp)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "Activity Intensity-time (MET-min)", labels = fmt_dcimals(1), limits=c(1,5000), breaks=c(1,5,10,50,100,500,1000,5000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  scale_x_discrete(name = "Apparent Temperature (°C)") +
  ggtitle("") +
  theme_bw() +
  geom_text(data=act.t, aes(x=cuttemp, y=1.1, label=n), family = "Times New Roman",
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = "Times New Roman",
            #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  facet_grid(.~code.f3) +
  theme(plot.title = element_text(size = 12, family = "Times New Roman"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        text = element_text(size=12, family="Times New Roman"),
        axis.text.y=element_text(color="black",size=11),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.minor.y = element_line(colour="gray85"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = cols)
p26.1

ggsave(here("data/export/MET_exp_by_act_bxplt_300_weighted_LA_RIV_only.tiff"), p26.1, device = "tiff",
       scale = 1, width = 6.5, height = 5, dpi = 300,units = "in")

### SOCIO PLOT (ELDERLY, BLACKS, GENDER) ###
#############################################

# ELDERLY VS NON ELDERLY
# labler
n.soc1 <- plyr::ddply(.data=meta.f[!is.na(meta.f$MET.dur) & (meta.f$msa.f == "LA" | meta.f$msa.f == "RIV"),], 
                  .(cuttemp,elderly), 
                plyr::summarize, 
                  n=length(elderly))

n.soc1$n <- paste("n =",n.soc1$n)
n.soc1 <- n.soc1[!is.na(n.soc1$elderly),]

# create activity x MET-time by cuttemp plot
p27.1 <- ggplot(data=meta.f.pop[!is.na(meta.f.pop$MET.dur) & (meta.f.pop$msa.f == "LA" | meta.f.pop$msa.f == "RIV"),], aes(x=cuttemp, y=MET.dur, fill=cuttemp)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(name = "Activity Intensity-time (MET-min)", labels = fmt_dcimals(1), limits=c(1,5000), breaks=c(1,5,10,50,100,500,1000,5000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  #scale_x_discrete(name = "Apparent Temperature (deg C)") +
  theme_bw() +
  geom_text(data=n.soc1, aes(x=cuttemp, y=1.1, label=n), family = "Times New Roman",
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = "Times New Roman",
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  facet_grid(.~elderly) +
  theme(plot.title = element_text(size = 12, family = "Times New Roman"),
        #plot.margin = unit(c(0,2,0,2),"cm"),
        text = element_text(size=12, family="Times New Roman"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_text(color="black",size=11),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.minor.y = element_line(colour="gray85"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = cols)

# BLACK VS NON BLACK
# labler
n.soc2 <- plyr::ddply(.data=meta.f[!is.na(meta.f$MET.dur) & (meta.f$msa.f == "LA" | meta.f$msa.f == "RIV"),], 
                .(cuttemp,black_white), 
                plyr::summarize, 
                n=length(black_white))

n.soc2$n <- paste("n =",n.soc2$n)
n.soc2 <- n.soc2[!is.na(n.soc2$black_white),]

# create activity x MET-time by cuttemp plot
p27.2 <- ggplot(data=meta.f.pop[!is.na(meta.f.pop$MET.dur) & (meta.f.pop$msa.f == "LA" | meta.f.pop$msa.f == "RIV"),], aes(x=cuttemp, y=MET.dur, fill=cuttemp)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(labels = fmt_dcimals(1), limits=c(1,5000), breaks=c(1,5,10,50,100,500,1000,5000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  scale_x_discrete(name = "Apparent Temperature (deg C)") +
  theme_bw() +
  geom_text(data=n.soc2, aes(x=cuttemp, y=1.1, label=n), family = "Times New Roman",
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = "Times New Roman",
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  facet_grid(.~black_white) +
  theme(plot.title = element_text(size = 12, family = "Times New Roman"),
        #plot.margin = unit(c(0,2,0,2),"cm"),
        text = element_text(size=12, family="Times New Roman"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_text(color="black",size=11),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.minor.y = element_line(colour="gray85"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = cols)

# MALE VS FEMALE
# labler
n.soc3 <- plyr::ddply(.data=meta.f[!is.na(meta.f$MET.dur) & (meta.f$msa.f == "LA" | meta.f$msa.f == "RIV"),], 
                .(cuttemp,sex.f), 
                plyr::summarize, 
                n=length(sex.f))

n.soc3$n <- paste("n =",n.soc3$n)
n.soc3 <- n.soc3[!is.na(n.soc3$sex.f),]

# create activity x MET-time by cuttemp plot
p27.3 <- ggplot(data=meta.f.pop[!is.na(meta.f.pop$sex.f) & (meta.f.pop$msa.f == "LA" | meta.f.pop$msa.f == "RIV"),], aes(x=cuttemp, y=MET.dur, fill=cuttemp)) +
  geom_boxplot(alpha = 0.7,outlier.size = 0.1) +
  #geom_jitter(aes(group=cut_width(TRTIER1P, 1)), colour = line, size = .7) +
  scale_y_log10(labels = fmt_dcimals(1), limits=c(1,5000), breaks=c(1,5,10,50,100,500,1000,5000), minor_breaks=log10_minor_break()) + 
  #scale_y_continuous(name = "Exposure (deg-min above 27 deg C)", limits=c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  #scale_x_discrete(name = "Apparent Temperature (deg C)") +
  theme_bw() +
  geom_text(data=n.soc3, aes(x=cuttemp, y=1.1, label=n), family = "Times New Roman",
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = "Times New Roman",
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  facet_grid(.~sex.f) +
  theme(plot.title = element_text(size = 12, family = "Times New Roman"),
        #plot.margin = unit(c(0,2,0,2),"cm"),
        text = element_text(size=12, family="Times New Roman"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_text(color="black",size=11),
        axis.text.x=element_text(color="black",size=11,angle=90,hjust=0.95,vjust=0.5),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.minor.y = element_line(colour="gray85"),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 8),
        legend.position="none") +
  #scale_fill_brewer(palette = "Set1", direction = 1)
  scale_fill_manual(values = cols)

require(gridExtra)
socio.plot <- grid.arrange(p27.1,p27.2,p27.3, ncol=3, 
                           bottom=textGrob("Apparent Temperature (°C)", gp=gpar(fontface="plain",fontfamily="Times New Roman",fontsize=14)),
                           left=textGrob("Activity Intensity-time (MET-min)", gp=gpar(fontface="plain",fontfamily="Times New Roman",fontsize=14), rot = 90))

socio.plot.an <- arrangeGrob(socio.plot, top = textGrob("(a)                                            (b)                                            (c)                 ", gp=gpar(fontfamily = "Times New Roman")))
 
ggsave(here("data/export/MET_exp_socio_bxplt_300_weighted_LA_RIV_only.tiff"), socio.plot.an, device = "tiff",
       scale = 1, width = 6.5, height = 5, dpi = 300,units = "in")



####################
print(Sys.time())  



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

# create black df
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
  geom_text(data=n.msa.t, aes(x=msa.f, y=75000, label=n),family = "Times New Roman",
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 0, size = 2.5, vjust=.3) +
  #facet_grid(.~cz.f2) +
  theme(plot.title = element_text(size = 11, family = "Times New Roman"),
        text = element_text(size=11, family="Times New Roman"),
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
  geom_text(data=msa.keeps, aes(x=abbr, y=75000, label=n_eq),family = "Times New Roman",
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 0, size = 2.5, vjust=.3) +
  #geom_text(data=msa.keeps, aes(x=abbr, y=75000, label=p_val),family = "Times New Roman",
   #         colour="black", inherit.aes=FALSE, parse=FALSE, angle = 0, size = 2.5, vjust=.3) +
  #facet_grid(.~cz.f2) +
  theme(plot.title = element_text(size = 11, family = "Times New Roman"),
        text = element_text(size=11, family="Times New Roman"),
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

# create weighted frequency table of outdoor activity codes
library(questionr)
act.freq <- as.data.frame(prop.table(questionr::wtd.table(meta$TRCODEP[meta$temp > 27 & meta$outdoors == 1], weights = meta$weight[meta$temp > 27 & meta$outdoors == 1], na.rm = T)))
names(act.freq)[names(act.freq) == 'Freq'] <- 'o27_All'

act.freq <- merge(act.freq, as.data.frame(prop.table(questionr::wtd.table(meta$TRCODEP[meta$elderly == "Over 65" & meta$temp > 27 & meta$outdoors == 1], weights = meta$weight[meta$elderly == "Over 65" & meta$temp > 27 & meta$outdoors == 1], na.rm = T))), by = "Var1")
names(act.freq)[names(act.freq) == 'Freq'] <- 'o27_Elderly'

act.freq <- merge(act.freq, as.data.frame(prop.table(questionr::wtd.table(meta$TRCODEP[meta$elderly == "15 to 65" & meta$temp > 27 & meta$outdoors == 1], weights = meta$weight[meta$elderly == "15 to 65" & meta$temp > 27 & meta$outdoors == 1], na.rm = T))), by = "Var1")
names(act.freq)[names(act.freq) == 'Freq'] <- 'o27_Non_Elderly'

act.freq <- merge(act.freq, as.data.frame(prop.table(questionr::wtd.table(meta$TRCODEP[meta$race.f2 == "Black" & meta$temp > 27 & meta$outdoors == 1], weights = meta$weight[meta$race.f2 == "Black" & meta$temp > 27 & meta$outdoors == 1], na.rm = T))), by = "Var1")
names(act.freq)[names(act.freq) == 'Freq'] <- 'o27_Black'

act.freq <- merge(act.freq, as.data.frame(prop.table(questionr::wtd.table(meta$TRCODEP[meta$race.f2 != "Black" & meta$temp > 27 & meta$outdoors == 1], weights = meta$weight[meta$race.f2 != "Black" & meta$temp > 27 & meta$outdoors == 1], na.rm = T))), by = "Var1")
names(act.freq)[names(act.freq) == 'Freq'] <- 'o27_Non_Black'

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

# add median and 90 perctile data to metros
for(a in 1:dim(metros)[1]){
  #PER ACTIVITY

  metros$n_act[a] <- length(meta.f$TUACTDUR24[meta.f$msa_lab == metros$abbr[a] & meta.f$temp > 27])
  
  #time
  metros$med_time[a] <- median(meta.f$TUACTDUR24[meta.f$msa_lab == metros$abbr[a] & meta.f$temp > 27])
  metros$p90_time[a] <- quantile(meta.f$TUACTDUR24[meta.f$msa_lab == metros$abbr[a] & meta.f$temp > 27], probs = .9)
  
  #MET
  metros$med_MET[a] <- median(meta.f$MET[meta.f$msa_lab == metros$abbr[a] & meta.f$temp > 27])
  metros$p90_MET[a] <- quantile(meta.f$MET[meta.f$msa_lab == metros$abbr[a] & meta.f$temp > 27], probs = .9)
  
  #temp
  metros$med_temp[a] <- median(meta.f$temp[meta.f$msa_lab == metros$abbr[a] & meta.f$temp > 27])
  metros$p90_temp[a] <- quantile(meta.f$temp[meta.f$msa_lab == metros$abbr[a] & meta.f$temp > 27], probs = .9)
  
  #met-deg-min
  metros$mean_exp[a] <- mean(meta.f$MET.dur[meta.f$msa_lab == metros$abbr[a] & meta.f$temp > 27])
  metros$med_exp[a] <- median(meta.f$MET.dur[meta.f$msa_lab == metros$abbr[a] & meta.f$temp > 27])
  metros$p90_exp[a] <- quantile(meta.f$MET.dur[meta.f$msa_lab == metros$abbr[a] & meta.f$temp > 27], probs = .9)
  
  
  # PER PERSON
  
  metros$n_per[a] <- length(meta.p$TUACTDUR24[meta.p$msa_lab == metros$abbr[a]])
  
  #time
  metros$mean_time.p[a] <- mean(meta.p$TUACTDUR24[meta.p$msa_lab == metros$abbr[a]])
  metros$med_time.p[a] <- median(meta.p$TUACTDUR24[meta.p$msa_lab == metros$abbr[a]])
  metros$p90_time.p[a] <- quantile(meta.p$TUACTDUR24[meta.p$msa_lab == metros$abbr[a]], probs = .9)
  
  #MET
  metros$mean_MET.p[a] <- mean(meta.p$MET[meta.p$msa_lab == metros$abbr[a]])
  metros$med_MET.p[a] <- median(meta.p$MET[meta.p$msa_lab == metros$abbr[a]])
  metros$p90_MET.p[a] <- quantile(meta.p$MET[meta.p$msa_lab == metros$abbr[a]], probs = .9)
  
  #temp
  metros$mean_temp.p[a] <- mean(meta.p$temp[meta.p$msa_lab == metros$abbr[a]])
  metros$med_temp.p[a] <- median(meta.p$temp[meta.p$msa_lab == metros$abbr[a]])
  metros$p90_temp.p[a] <- quantile(meta.p$temp[meta.p$msa_lab == metros$abbr[a]], probs = .9)
  
  #met-deg-min
  metros$mean_exp.p[a] <- mean(meta.p$MET.exp27[meta.p$msa_lab == metros$abbr[a]])
  metros$p50_exp.p[a] <- quantile(meta.p$MET.exp27[meta.p$msa_lab == metros$abbr[a]], probs = .5)
  metros$p90_exp.p[a] <- quantile(meta.p$MET.exp27[meta.p$msa_lab == metros$abbr[a]], probs = .9)
  
  # CASE BY PERSON MEDIAN AND 90TH
  p.50 <- meta.p[meta.p$msa_lab == metros$abbr[a],]
  p.50 <- p.50[which.quantile(p.50$MET.exp27, probs = .5),]
  
  p.90 <- meta.p[meta.p$msa_lab == metros$abbr[a],]
  p.90 <- p.90[which.quantile(p.90$MET.exp27, probs = .9),]

  metros$p50_time.p.e[a] <- p.50$TUACTDUR24
  metros$p90_time.p.e[a] <- p.90$TUACTDUR24
    
  metros$p50_MET.p.e[a] <- p.50$MET
  metros$p90_MET.p.e[a] <- p.90$MET
    
  metros$p50_temp.p.e[a] <- p.50$temp
  metros$p90_temp.p.e[a] <- p.90$temp
  
  metros$p50_exp.p[a] <- p.50$MET.exp27
  metros$p90_exp.p[a] <- p.90$MET.exp27
  
}

for(a in 1:dim(metros.cz)[1]){
  #PER ACTIVITY
  
  metros.cz$n_act[a] <- length(meta.f$TUACTDUR24[meta.f$msa_lab == metros.cz$abbr[a] & meta.f$temp > 27])
  
  #time
  metros.cz$med_time[a] <- median(meta.f$TUACTDUR24[meta.f$msa_lab == metros.cz$abbr[a] & meta.f$temp > 27])
  metros.cz$p90_time[a] <- quantile(meta.f$TUACTDUR24[meta.f$msa_lab == metros.cz$abbr[a] & meta.f$temp > 27], probs = .9)
  
  #MET
  metros.cz$med_MET[a] <- median(meta.f$MET[meta.f$msa_lab == metros.cz$abbr[a] & meta.f$temp > 27])
  metros.cz$p90_MET[a] <- quantile(meta.f$MET[meta.f$msa_lab == metros.cz$abbr[a] & meta.f$temp > 27], probs = .9)
  
  #temp
  metros.cz$med_temp[a] <- median(meta.f$temp[meta.f$msa_lab == metros.cz$abbr[a] & meta.f$temp > 27])
  metros.cz$p90_temp[a] <- quantile(meta.f$temp[meta.f$msa_lab == metros.cz$abbr[a] & meta.f$temp > 27], probs = .9)
  
  #met-deg-min
  metros.cz$mean_exp[a] <- mean(meta.f$MET.dur[meta.f$msa_lab == metros.cz$abbr[a] & meta.f$temp > 27])
  metros.cz$med_exp[a] <- median(meta.f$MET.dur[meta.f$msa_lab == metros.cz$abbr[a] & meta.f$temp > 27])
  metros.cz$p90_exp[a] <- quantile(meta.f$MET.dur[meta.f$msa_lab == metros.cz$abbr[a] & meta.f$temp > 27], probs = .9)
  
  
  # PER PERSON
  
  metros.cz$n_per[a] <- length(meta.p$TUACTDUR24[meta.p$msa_lab == metros.cz$abbr[a]])
  
  #time
  metros.cz$mean_time.p[a] <- mean(meta.p$TUACTDUR24[meta.p$msa_lab == metros.cz$abbr[a]])
  metros.cz$med_time.p[a] <- median(meta.p$TUACTDUR24[meta.p$msa_lab == metros.cz$abbr[a]])
  metros.cz$p90_time.p[a] <- quantile(meta.p$TUACTDUR24[meta.p$msa_lab == metros.cz$abbr[a]], probs = .9)
  
  #MET
  metros.cz$mean_MET.p[a] <- mean(meta.p$MET[meta.p$msa_lab == metros.cz$abbr[a]])
  metros.cz$med_MET.p[a] <- median(meta.p$MET[meta.p$msa_lab == metros.cz$abbr[a]])
  metros.cz$p90_MET.p[a] <- quantile(meta.p$MET[meta.p$msa_lab == metros.cz$abbr[a]], probs = .9)
  
  #temp
  metros.cz$mean_temp.p[a] <- mean(meta.p$temp[meta.p$msa_lab == metros.cz$abbr[a]])
  metros.cz$med_temp.p[a] <- median(meta.p$temp[meta.p$msa_lab == metros.cz$abbr[a]])
  metros.cz$p90_temp.p[a] <- quantile(meta.p$temp[meta.p$msa_lab == metros.cz$abbr[a]], probs = .9)
  
  #met-deg-min
  metros.cz$mean_exp.p[a] <- mean(meta.p$MET.exp27[meta.p$msa_lab == metros.cz$abbr[a]])
  metros.cz$p50_exp.p[a] <- quantile(meta.p$MET.exp27[meta.p$msa_lab == metros.cz$abbr[a]], probs = .5)
  metros.cz$p90_exp.p[a] <- quantile(meta.p$MET.exp27[meta.p$msa_lab == metros.cz$abbr[a]], probs = .9)
  
  # CASE BY PERSON MEDIAN AND 90TH
  p.50 <- meta.p[meta.p$msa_lab == metros.cz$abbr[a],]
  p.50 <- p.50[which.quantile(p.50$MET.exp27, probs = .5),]
  
  p.90 <- meta.p[meta.p$msa_lab == metros.cz$abbr[a],]
  p.90 <- p.90[which.quantile(p.90$MET.exp27, probs = .9),]
  
  metros.cz$p50_time.p.e[a] <- p.50$TUACTDUR24
  metros.cz$p90_time.p.e[a] <- p.90$TUACTDUR24
  
  metros.cz$p50_MET.p.e[a] <- p.50$MET
  metros.cz$p90_MET.p.e[a] <- p.90$MET
  
  metros.cz$p50_temp.p.e[a] <- p.50$temp
  metros.cz$p90_temp.p.e[a] <- p.90$temp
  
  metros.cz$p50_exp.p[a] <- p.50$MET.exp27
  metros.cz$p90_exp.p[a] <- p.90$MET.exp27
  
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
  #geom_text(data=act.t, aes(x=cuttemp, y=1.1, label=n), family = "Times New Roman",
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = "Times New Roman",
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  #facet_grid(.~code.f2) +
  theme(plot.title = element_text(size = 12, family = "Times New Roman"),
        text = element_text(size=12, family="Times New Roman"),
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
  #geom_text(data=act.t, aes(x=cuttemp, y=1.1, label=n), family = "Times New Roman",
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = "Times New Roman",
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  #facet_grid(.~code.f2) +
  theme(plot.title = element_text(size = 12, family = "Times New Roman"),
        text = element_text(size=12, family="Times New Roman"),
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
  #geom_text(data=act.t, aes(x=cuttemp, y=1.1, label=n), family = "Times New Roman",
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = "Times New Roman",
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  #facet_grid(.~code.f2) +
  theme(plot.title = element_text(size = 12, family = "Times New Roman"),
        text = element_text(size=12, family="Times New Roman"),
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
  geom_text(data=age.t, aes(x=cuttemp, y=1.1, label=n), family = "Times New Roman",
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=0.25) +
  #geom_text(data=age.t, aes(x=cuttemp, y=1300, label=p), family = "Times New Roman",
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=1) +
  facet_grid(.~age.f2) +
  theme(plot.title = element_text(size = 11, family = "Times New Roman"),
        text = element_text(size=12, family="Times New Roman"),
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
  geom_text(data=income.t, aes(x=cuttemp, y=1.1, label=n), family = "Times New Roman",
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=0) +
  #geom_text(data=income.t, aes(x=cuttemp, y=1300, label=p), family = "Times New Roman",
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=1) +
  facet_grid(.~income.f) +
  theme(plot.title = element_text(size = 11, family = "Times New Roman"),
        text = element_text(size=12, family="Times New Roman"),
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
  geom_text(data=race.t, aes(x=cuttemp, y=1.1, label=n),family = "Times New Roman",
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=.25) +
  #geom_text(data=race.t, aes(x=cuttemp, y=1300, label=p), family = "Times New Roman",
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.2, vjust=1) +
  facet_grid(.~race.f2) +
  theme(plot.title = element_text(size = 11, family = "Times New Roman"),
        text = element_text(size=11, family="Times New Roman"),
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

# SPRAWL FIGURE / FIGURE 6

cz.color <- c("Marine" = "#60BFC1", "Mixed-Dry" = "#EFF3BD", "Mixed-Humid" = "#F58980", "Cold" = "#466582", "Hot-Dry" = "#E1C97D", "Hot-Humid" = "#E24A2C")

#meta.pop <- merge(meta.pop, sprawl, by.x = "msa_lab", by.y = "label")

metros$BA.Climate.Zone <- factor(metros$BA.Climate.Zone, levels = c("Hot-Humid","Hot-Dry","Mixed-Humid","Mixed-Dry","Cold","Marine"))
metros.adj <- metros[metros$n_o27 >= 30,]

msa.keeps <- metros[metros$p_val_MET.exp27_mean <= 0.05,]
msa.keeps$n_eq <- paste("n =",msa.keeps$per_out_n_27)
msa.keeps$p_val <- paste("p = ",ifelse(msa.keeps$p_val_MET.exp27_mean < 0.001, "< 0.001",round(msa.keeps$p_val_MET.exp27_mean, 3)))


# function to return p value
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

metros$BA.Climate.Zone <- factor(metros$BA.Climate.Zone, levels = c("Hot-Humid","Hot-Dry","Mixed-Humid","Mixed-Dry","Cold","Marine"))


sprawl.exp.model <- lm(formula = (msa.keeps$MET.exp27_mean ^ lambda) ~ msa.keeps$sprawl + msa.keeps$BA.Climate.Zone + msa.keeps, weights = msa.keeps$per_out_n_27)
summary(sprawl.exp.model)

R2lab <- paste("adj~R^2 == ", round(as.numeric(summary(lm(formula = (msa.keeps$MET.exp27_mean ^ lambda) ~ msa.keeps$sprawl + 
                                                        msa.keeps$BA.Climate.Zone, weights = msa.keeps$per_out_n_27))[10]),3))
pval <- lmp(lm(formula = (msa.keeps$MET.exp27_mean ^ lambda) ~ msa.keeps$sprawl + msa.keeps$BA.Climate.Zone, weights = msa.keeps$per_out_n_27))
plab <- ifelse(round(pval, 3) < 0.001, paste("p < 0.001"), paste("p ==", round(pval, 3)))

f6 <- ggplot(msa.keeps, aes(x = MET.exp27_mean ^ lambda, y = sprawl, label = abbr)) +
  scale_x_continuous(name = "Normalized Mean Person-Day Exposure", limits=c(1.6,2.4), labels = fmt_dcimals(1)) + 
  scale_y_continuous(name = "High Spawl Index                                    Low Sprawl Index", limits=c(0,200)) +  #Composite Sprawl Index Score
  geom_smooth(method = "lm", se = T, color = "black", size = .5, fill = "gray80") +
  geom_point(data = msa.keeps,aes(size = per_out_n_27, color = BA.Climate.Zone)) + 
  scale_size(range = c(2.5, 10)) +
  geom_text(aes(label = abbr), size = 2, hjust=.5, vjust=.5, family = "Times New Roman") +
  theme_bw() +
  labs(color="MSA Climate Zone") +
  labs(size="Sample Size (n)") +
  guides(colour = guide_legend(override.aes = list(size=7))) +
  scale_color_manual(values = cz.color) +
  annotate("text", x=2, y=30, label=R2lab, parse=TRUE, family = "Times New Roman", size = 3.5) +
  annotate("text", x=2, y=22, label=plab, parse=TRUE, family = "Times New Roman", size = 3.5) +
  theme(text = element_text(size=10, family="Times New Roman"),
        legend.text = element_text(size=8, family="Times New Roman"),
        legend.title = element_text(size=9),
        legend.key.size = unit(1,"line"),
        #legend.position="none",
        axis.text.y=element_text(color="black",size=9),
        axis.text.x=element_text(color="black",size=9)
        #strip.text.x = element_text(size = 8.5)
  ) 
f6
ggsave("20180529_msa_exp_sprawl_300_weighted_keeps.tiff", f6, device = "tiff", path = "UAHS Outputs",
       scale = 1, width = 6.5, height = 5, dpi = 300,units = "in")



#R2lab.lawn <- paste("R^2 == ", round(as.numeric(summary(lm(formula = metros.adj$mean_lawn_time ~ metros.adj$sprawl))[8]),3))
#plab.lawn <- paste("p == 0.015") # manual via summary

R2lab.lawn <- paste("R^2 == ", round(as.numeric(summary(lm(formula = (metros$mean_lawn_time) ~ metros$sprawl, weights = metros$per_out_n_27))[9]),3))
pval.lawn <- lmp(lm(formula = (metros$mean_lawn_time) ~ metros$sprawl, weights = metros$per_out_n_27))
plab.lawn <- ifelse(round(pval, 3) < 0.001, paste("p < 0.001"), paste("p ==", round(pval, 3)))

f6.2 <- ggplot(metros, aes(x = mean_lawn_time, y = sprawl, label = abbr)) +
  scale_x_continuous(name = "Mean Lawncare Exposure Time (min above 27 deg C per person per day)", limits=c(0,20), labels = fmt_dcimals(1)) + 
  scale_y_continuous(name = "High Spawl Index                                    Low Sprawl Index", limits=c(0,200)) +  #Composite Sprawl Index Score
  geom_smooth(method = "lm", se = T, color = "black", size = .5, fill = "gray80") +
  geom_point(data = metros,aes(size = per_out_n_27, color = BA.Climate.Zone)) + 
  scale_size(range = c(3.5, 13.5)) +
  geom_text(aes(label = abbr), size = 2, hjust=.5, vjust=.5, family = "Times New Roman") +
  theme_bw() +
  labs(color="MSA Climate Zone") +
  labs(size="MSA Sample Size") +
  guides(colour = guide_legend(override.aes = list(size=7))) +
  scale_color_manual(values = cz.color) +
  annotate("text", x=15, y=50, label=R2lab.lawn, parse=TRUE, family = "Times New Roman", size = 3) +
  annotate("text", x=15, y=42, label=plab.lawn, parse=TRUE, family = "Times New Roman", size = 3) +
  theme(text = element_text(size=10, family="Times New Roman"),
        legend.text = element_text(size=8, family="Times New Roman"),
        legend.title = element_text(size=9),
        legend.key.size = unit(1,"line"),
        axis.text.y=element_text(color="black",size=9),
        axis.text.x=element_text(color="black",size=9)
        #strip.text.x = element_text(size = 8.5)
  ) 

ggsave("20180529_msa_lawn_sprawl_300_weighted.tiff", f6.2, device = "tiff", path = "UAHS Outputs",scale = 1, width = 6.5, height = 5, dpi = 300,units = "in")


###############
#acs <- read.csv("ACS PUMS/2004-15 SES data/usa_00002.csv",header = TRUE)     #data
#########

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


min(meta$temp[meta$outdoors > 0], na.rm = T)
max(meta$temp[meta$outdoors > 0], na.rm = T)
meta$temp[meta$outdoors == 0]
formatC


length(meta$TUACTDUR24[meta$temp > 27])
length(meta$race[meta$temp > 27 & meta$outdoors > 0 & meta$race >= 0])
length(meta$TUACTDUR24[meta$temp > 27 & !is.na(meta$temp)])


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


library(MASS)
library(lme4)
library(MuMIn) 

meta.f27.agg$date <- as.Date(meta.f27.agg$date, format = "%Y%m%d")
meta.f27.agg$n.weight <- meta.f27.agg$weight / mean(meta.f27.agg$weight)

qqp(meta.f27.agg$MET.exp27, "lnorm")

# log transform linear mixed model w/ random nested effects of climate zone and MSA
lmm1 <- lmer(log(MET.exp27 + 1) ~ race.f2 + sex.f + income.f + age.f2 + sprawl + (1 | BA.Climate.Zone / msa.f), data = meta.f27.agg, REML = F)
lmm2 <- lmer(log(MET.exp27 + 1) ~ race.f2 + sex.f + income.f + age.f2 + sprawl + BA.Climate.Zone + (1 | msa.f), data = meta.f27.agg, REML = F)
lmm3 <- lmer(log(MET.exp27 + 1) ~ race.f2 + sex.f + income.f + age.f2 + sprawl + BA.Climate.Zone + msa.f + (1 | msa.f), data = meta.f27.agg, weights = n.weight, REML = F)
lmm4 <- lmer(log(MET.exp27 + 1) ~ race.f2 + sex.f + income.f * age.f2 + sprawl + msa.f + (1 | msa.f), data = meta.f27.agg, weights = n.weight, REML = F)
lmm5 <- lmer(log(MET.exp27 + 1) ~ race.f2 + sex.f + income.f * age.f2 + sprawl + msa.f + (1 + BA.Climate.Zone | msa.f), data = meta.f27.agg, weights = n.weight, REML = F)
lmm6 <- lmer(log(MET.exp27 + 1) ~ race.f2 + sex.f + income.f + age.f2 + sprawl + (1 | BA.Climate.Zone / msa.f), data = meta.f27.agg, REML = F, weights = n.weight)
lmm7 <- lmer(log(MET.exp27 + 1) ~ race.f2 + sex.f + income.f + age.f2 + sprawl + msa.f + (1 | BA.Climate.Zone / msa.f), data = meta.f27.agg, REML = F, weights = n.weight)
lmm8 <- lmer(log(MET.exp27 + 1) ~ race.f2 + sex.f + income.f + age.f2 + sprawl + msa.f + (1 | BA.Climate.Zone / msa.f), data = meta.f27.agg, REML = F)
lmm9 <- lmer(log(MET.exp27 + 1) ~ race.f2 + sex.f + income.f + age.f2 + sprawl + msa.f + (1 | BA.Climate.Zone / msa.f), data = meta.f27.agg, REML = F)
#lme1 <- lme(log(MET.exp27 + 1)  ~ race.f2 + sex.f + income.f + age.f2 + sprawl + msa.f, random = ~ 1 | BA.Climate.Zone / msa.f, data = meta.f27.agg, method = "ML")
lmm <- lmer(log(MET.exp27 + 1) ~ race.f2 + sex.f + income.f + age.f2 + sprawl + date + season + (1 | BA.Climate.Zone / msa.f), data = meta.f27.agg, REML = F)
lme <- lme(log(MET.exp27 + 1)  ~ race.f2 + sex.f + income.f + age.f2 + sprawl + season, random = ~ 1 |  msa.f, data = meta.f27.agg, method = "ML")

summary(lmm1)$AIC
summary(lmm2)$AIC
summary(lmm3)$AIC
summary(lmm4)$AIC
summary(lmm5)$AIC
summary(lmm6)$AIC
summary(lmm7)$AIC
summary(lmm8)$AIC
summary(lmm9)$AIC
summary(lmm)$AIC
summary(lme)


r.squaredGLMM(lmm1)
r.squaredGLMM(lmm9)

anova(lmm)

r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

r2.corr.mer(lme)
r2.corr.mer(lmm9)

# Xu 2003
1-var(residuals(lmm1))/(var(model.response(model.frame(lmm1))))
1-var(residuals(lmm9))/(var(model.response(model.frame(lmm9))))
1-var(residuals(lmm))/(var(model.response(model.frame(lmm))))
1-var(residuals(lme))/(var(model.response(model.frame(lme))))





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


recs2015 <- read.csv("recs2015_public_v3.csv")
recs2015$CLIMATE_REGION_PUB

# households with AC
sum(recs2015$AIRCOND * recs2015$NWEIGHT) / sum(recs2015$NWEIGHT)


climate.ac <- 
  recs2015 %>% 
  group_by(CLIMATE_REGION_PUB) %>% 
  summarise(percent = sum(AIRCOND[UATYP10 == "U"] * NWEIGHT[UATYP10 == "U"]) / sum(NWEIGHT[UATYP10 == "U"]))

division.ac <- 
  recs2015 %>% 
  group_by(DIVISION) %>% 
  summarise(percent = sum(AIRCOND[UATYP10 == "U"] * NWEIGHT[UATYP10 == "U"]) / sum(NWEIGHT[UATYP10 == "U"]))

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
  geom_text(data=day.t, aes(x=cuttemp, y=1.1, label=n), family = "Times New Roman",
            colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2.4, vjust=.25) +
  #geom_text(data=act.t, aes(x=cuttemp, y=4800, label=p), family = "Times New Roman",
  #colour="black", inherit.aes=FALSE, parse=FALSE, angle = 90, size = 2, vjust=.95) +
  facet_grid(.~day) +
  theme(plot.title = element_text(size = 12, family = "Times New Roman"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        text = element_text(size=12, family="Times New Roman"),
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

# percent of all people 
(sum(ifelse(meta$MET.exp27 > 0 & !is.na(meta$MET.exp27), meta$weight, 0)) / 4147) / sum(metros$respop72016)
