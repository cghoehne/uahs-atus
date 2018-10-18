
  #$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#
 #$%    ATUS DATA SYNTHESIS META CODE    #$%
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#

memory.limit(size = 56000)

# list of all dependant packages
list.of.packages <- c("lubridate",
                      "chron",
                      "tidyverse",
                      "data.table", 
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
lapply(list.of.packages, library, character.only = TRUE)

# GENERAL APPROACH
#   1. Load activity file, load ATUS-CPS file, and load CPS file. Match all three to get geocoded hourly activities. Filter to relevant MSAs.
#   2. Load temperature files for relevant MSAs, match temps to activities by midpoint activity times.
#   3. Load activity and location coding tables, match to meta file, and add a master activity code (1=in,2=int,3=out,4=unk).
#   4. Load all other necessary files and match any other releveant variables (income, race, age, sex, work industry, etc.)

#saveRDS(atus, "C:/Users/topher/Documents/GitHub Projects/uahs-atus/data/atus_filtered_0315.rds")

# load ATUS data filtered and geocoded (to relevant MSAs) for years 2003 to 2015
# file includes ATUS activity, ATUS CPS, and ATUS respondant, and original CPS file merged 
atus <- readRDS(here("data/atus_geoclean_0315.rds"))

# load list of metros for indexing
metros <- readRDS(here("data/msa_list.rds"))

# Recode activity start and stop time variables to minutes. 0 = 0:00:00, 1439.983 = 11:59:59.
atus$STARTMIN <- 60 * 24 * as.numeric(times(atus$TUSTARTTIM))
atus$STOPMIN <- 60 * 24 * as.numeric(times(atus$TUSTOPTIME))

# Creates a date + time column as YYYY-MM-DD HH:MM, R recognizes these values as time. 
atus$DateTime<- format(strptime(paste(atus$TUDIARYDATE,atus$TUSTARTTIM,sep =""), format="%Y%m%d%H:%M"), format="%Y-%m-%d %H:%M")

# Add TU Day of month variable
atus$TUDAY <- format(strptime(atus$TUDIARYDATE, format="%Y%m%d"), format="%d")

# Add TU midpoint time (in minutes from beginning of day). This represents the midpoint time of the activity
atus$MIDTIME <- (atus$STARTMIN + atus$STOPMIN)/2

# Creates a date + time column with the starting, midpoint, and ending activity timestamp
atus$DateTimeStart <- format(paste(strptime(atus$TUDIARYDATE, format="%Y%m%d"),times(atus$STARTMIN / 1440),sep=" "), format="%Y-%m-%d %H:%M:%S")
atus$DateTimeStart <- as.POSIXct(atus$DateTimeStart)

atus$DateTimeMid.f <- format(paste(strptime(atus$TUDIARYDATE, format="%Y%m%d"),times((((atus$MIDTIME - atus$STARTMIN) / 2 ) + atus$STARTMIN )/ 1440) ,sep=" "), format="%Y-%m-%d %H:%M:%S")
atus$DateTimeMid.f <- as.POSIXct(atus$DateTimeMid.f)

atus$DateTimeMid <- format(paste(strptime(atus$TUDIARYDATE, format="%Y%m%d"),times(atus$MIDTIME / 1440),sep=" "), format="%Y-%m-%d %H:%M:%S")
atus$DateTimeMid <- as.POSIXct(atus$DateTimeMid)

atus$DateTimeMid.b <- format(paste(strptime(atus$TUDIARYDATE, format="%Y%m%d"),times((atus$STOPMIN - ((atus$STOPMIN - atus$MIDTIME) / 2 )) / 1440) ,sep=" "), format="%Y-%m-%d %H:%M:%S")
atus$DateTimeMid.b <- as.POSIXct(atus$DateTimeMid.b)

atus$DateTimeEnd <- format(paste(strptime(atus$TUDIARYDATE, format="%Y%m%d"),times(atus$STOPMIN / 1440),sep=" "), format="%Y-%m-%d %H:%M:%S")
atus$DateTimeEnd <- as.POSIXct(atus$DateTimeEnd)


# create dummy temp variables for later
atus$temp <- NA   # new apparent temp variable
atus$TempC <- NA     # temp in C variable
atus$DewpC <- NA     # dewpt in C variable

# create a Year-Month-Day and a Year-Month variable to use for matching with temps
atus$YrMthDay <- format(strptime(atus$DateTimeMid, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d")
atus$YrMth <- format(strptime(atus$DateTimeMid, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m")

# Drop non-useful columns. NOTE: if later columns are deemed useful, remerge from orginal data set(s).
atus <- subset(atus, select=-c(TRWBELIG,SERIAL,PERNUM,HRMONTH,HRYEAR4,PULINENO,GEMETSTA,GESTFIPS,GTMETSTA,
                                   HEPHONEO,HETELAVL,HETELHHD,HRINTSTA,HRMIS,HUBUSL1,HUBUSL2,HUBUSL3,HUBUSL4,
                                   HUFINAL,HUINTTYP,PEERNRT,PEERNPER,PELKFTO,PELKLL1O,PRHERNAL,PRIOELG,PRWERNAL,
                                   PTNMEMP1,PTNMEMP2,PULAY6M,PULAYAVR,PULKDK1,PULKDK2,PULKDK3,PULKDK4,PULKM2,
                                   PULKM3,PULKM4,PULKM5,PULKM6,PULKPS1,PULKPS2,PULKPS3,PULKPS4))

# Remove activities with a missing or invalid timestamp
atus <- subset(atus, !is.na(atus$DateTimeMid))
atus$hr.s <- as.numeric(format(strptime(atus$DateTimeStart, format="%Y-%m-%d %H:%M:%S"), format="%H"))
atus$hr.e <- as.numeric(format(strptime(atus$DateTimeEnd, format="%Y-%m-%d %H:%M:%S"), format="%H"))

# income: HEFAMINC if year < 2010, HUFAMINC if year >= 2010
atus$Income <- NA

for(b in 1:nrow(atus)){
  if(atus$HUFAMINC[b] < 0){
    atus$Income[b] <- atus$HEFAMINC[b]
  } 
  if(atus$HEFAMINC[b] < 0){
    atus$Income[b] <- atus$HUFAMINC[b]
  } 
}



# remvoe any other extraneous variables
atus$HUHHNUM.y <- NULL
atus$HRSAMPLE.y <- NULL
atus$HRSERSUF.y <- NULL


# Remove unused data sets and clear up space  
rm(list=setdiff(ls(), c("atus","metros")))
gc()
memory.limit(size = 56000)

#~<@>~~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~#
#      STEP TWO: Match temps to activities by MSA      #
#~<@>~~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~#


# Divide activities data sets into seperate files for each region based on list. Names of df's will be "PHX", "LA", "NYC", etc.
for(j in 1:dim(metros)[1]){  #
  
  # Create subset for specific MSA
  dt <- as.data.table(subset(atus, METFIPS == metros$MSA_code[j]))
  
  # Assign dataframe name of MSA abbrevation
  assign(metros$abbr[j], dt)
}


# Directory for loading temp files
dir <- file.path("NCDC Global Hourly weather data","metros")

# CODE FOR RBINDLIST OF FILES
#temp.files <- list.files(here(path = "data/weather data/MCFCD/2017/Temp"), pattern="txt$", full.names = T) # full file path names
#temp.names <- gsub(".txt", "", list.files(here(path = "data/weather data/MCFCD/2017/Temp"), pattern = "txt$", full.names = F)) # names (stations) of files
#mcfcd.temp.data <- rbindlist(lapply(temp.files, fread), idcol = "station.id") # load all station data 
#mcfcd.temp.data[, station.id := factor(station.id, labels = basename(temp.names))] # add station names to column 'station'


# Load formatted temp files (formatting done in seperate script).
for(k in 3){  #1:dim(metros)[1]
  df <- read.csv(file = file.path(dir,metros$FileOut[k]), header = T)
  df$YrMthDay <- format(strptime(df$DateTime, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d")
  df$hr <- as.numeric(format(strptime(df$DateTime, format="%Y-%m-%d %H:%M:%S"), format="%H"))
  
  # Calc relative humidity
  df$rh <- exp((17.625*df$DEWPC)/(243.04+df$DEWPC))/exp((17.625*df$TEMPC)/(243.04+df$TEMPC))
  
  # calculate Heat Index via NWS equations
  df$rh <- df$rh*100
  df$TEMP <- df$TEMPC * 9/5 + 32
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
  
  df$rh <- df$rh/100 # re-adjust relative humidity
  
  # use non adjustment if below 80 based on NWS 
  df$heat2 <- 0.5 * (df$TEMP + 61 + ((df$TEMP - 68)*1.2) + (df$rh*0.094))
  df$heat2 <- (df$TEMP + df$heat2)/2
  df$temp <- ifelse(df$heat2 >= 80, df$heat, df$heat2)   # use temp if app temp, use heat if heat index (because 'heat' only overwrote 'temp').
  df$temp <- (df$temp - 32) * 5/9 # convert to Celcius
  
  # fill in missing hourly temps with avg for that hour in +/- 30 days
  for(i in 1:dim(df)[1]){
    if(is.na(df$temp[i])){
      df.s <- subset(df, (as.Date(YrMthDay) > as.Date(YrMthDay[i]) - 15) & (as.Date(YrMthDay) < as.Date(YrMthDay[i]) + 15) & (hr[i] == hr))
      df$temp[i] <- ifelse(!is.na(mean(df.s$temp, na.rm = T)), mean(df.s$temp, na.rm = T), NA)
    } 
  }
  dt <- as.data.table(df)
  assign(paste0("tmp",metros$MSA_code[k]), dt)
}


# Remove unused data sets and clear up space  
rm(act,a,b,df,j,k,t.difs,z)          # remove temporary objects by name
#rm(list = ls(pattern = "^tmp"))      # remove rest of temporary objects by removing all that start with "tmp"
gc()


  #$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%
 #$%     STEP THREE: Master Recode of Activities    #$%
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%

# Tag activity to be either Outside, Inside, or Intermediate

# location categories
# 1 = indoor
# 2 = intermediate
# 3 = outdoor
# 4 = unknown

# import activity and location code tables. Refrence LocationEnviMatch R code by David H to match codes by activity and/or location
# Assume that:
# If one of activity or location is "4 - Unknown" but the other is 1, 2, or 3, override to repective 1, 2, or 3.
# If location is outdoors (one of "Outdoors away from home", "Walking", or "biking"), assume that these always are outdoors and ignore if activty is coded 1 or 2.
# The only possible conflict is if someone is "walking" indoors, but they should select "place" if indoors.
# If an activity is "2 - intermediate" overwrite if the location is 1 or 3. (or 2 or 4 for that matter)
# Once activity location is recoded to 1 - 4, overwrite indoor activities temperatures with 21C. Intermediate should be (21C + act$AvgTemp) / 2


# Load newly formatted activities data sets and overwrite each region old df (PHX", "LA", "NYC", etc.).
#for(a in 3:dim(metros)[1]){
  
  # Create subset for specific MSA
  #df <- read.csv(file = file.path(dir,metros$OutMerge[a]), header = T)

  # Assign dataframe name of MSA abbrevation
  #assign(metros$abbr[a], df)
#}

# create output directory
dir.out <- file.path("UAHS Outputs","metros coded")

# load location and activity code files
loc.table <- read.csv("X:/Dropbox (ASU)/Data and Tools/ATUS/2003-2015/location_codes.csv", header = T)
act.table <- read.csv("X:/Dropbox (ASU)/Data and Tools/ATUS/2003-2015/activity_codes.csv", header = T)

# Loop through metro files, assign generalized indoor/outdoor activity codes, then assign temps based on indoor/outdoor activity --- dim(metros)[1]
print(Sys.time())
# *** WARNING - LONG SECTION - 5+ HOURS RUN TIME
for(a in 3){  #1:dim(metros)[1]
  
  # set a metro file with temps as dataframe "meta"
  meta <- get(metros$abbr[a])
  
  # merge activities and new locations codes
  meta <- merge(meta, loc.table, by.x="TEWHERE", by.y="loc_code")
  meta <- merge(meta, act.table, by.x="TRCODEP", by.y="act_code")
  
  # recode to master location code: loc_m. Assume that "unknown" is default to be overwritten.
  meta$loc_m <- 4
  
  # loop to assign master location code based on activity and location codes --- dim(meta)[1]
  for(b in 1:dim(meta)[1]){
    if(meta$loc_l[b] == meta$loc_a[b]){    # if the location and activity codes are equal
      meta$loc_m[b] <- meta$loc_l[b]   # assign loc_m to be their value
    }
    
    if(meta$loc_l[b] == 4){                # if location code is unknown
      meta$loc_m[b] <- meta$loc_a[b]   # assign loc_m based on activity code
    }
    
    if(meta$loc_a[b] == 4){                # if activity code is unknown
      meta$loc_m[b] <- meta$loc_l[b]   # assign loc_m based on location code
    }
    
    if(meta$loc_a[b] == 2){                # if activity code is intermediate
      meta$loc_m[b] <- 2               # assign loc_m to be intermediate 
    }
    
    if(meta$loc_a[b] == 3){                # if activity code is outdoors
      meta$loc_m[b] <- 3               # assign loc_m to be outdoors
    }
    # NOTE: there is no intermediate activity locations
    
    if(meta$loc_l[b] == 3){                # if location code is outdoors
      meta$loc_m[b] <- 3               # assign loc_m to be outdoors
    }
  }

  # and output the file to UAHS output folder "metros coded"
  #write.csv(meta, file = file.path(dir.out,metros$OutMerge[a]), row.names = F)
  
  assign(metros$abbr[a], meta)  # assign abbr to name 'meta' for each metro
}

print(Sys.time())

# Remove unused data sets and clear up space  
rm(loc.table,act.table,df,a,b,c,meta)   
gc()


# Split metro files into two files, one for matching temps (outdoors locations only), and one with everything else
for(j in 3){  #1:dim(metros)[1]
  
  # Create subset for specific MSA
  dt <- get(metros$abbr[j])
  
  # add code for if outdoors
  dt$outdoors <- 0
  dt$outdoors[dt$loc_m == 3] <- 1
  
  # add temp dummy varibles
  dt$temp1 <- NA
  dt$temp2 <- NA
  dt$temp3 <- NA
  dt$temp4 <- NA
  dt$temp5 <- NA
  
  # change timezone to be correct for each MSA 
  dt$DateTimeStart <- with_tz(dt$DateTimeStart, tz = metros$Timezone[j]) 
  dt$DateTimeMid.f <- with_tz(dt$DateTimeMid.f, tz = metros$Timezone[j]) 
  dt$DateTimeMid <- with_tz(dt$DateTimeMid, tz = metros$Timezone[j]) 
  dt$DateTimeMid.b <- with_tz(dt$DateTimeMid.b, tz = metros$Timezone[j]) 
  dt$DateTimeEnd <- with_tz(dt$DateTimeEnd, tz = metros$Timezone[j]) 
  
  # split inside/outside files
  dt.out <- dt[dt$outdoors == 1]
  dt.in <- dt[dt$outdoors == 0]
  
  # Assign dataframes name of MSA abbrevation
  assign(paste0(metros$abbr[j], ".out"), dt.out)
  assign(paste0(metros$abbr[j],".in"), dt.in)  
}


# Match NCDC temps to subsetted ATUS activity files for outdoor activities
t0 <- Sys.time()
for(a in 2){ # 1:dim(metros)[1] 
  
  act <- get(paste0(metros$abbr[a], ".out"))     # activity subset for each MSA
  tmp <- get(paste0("tmp",metros$MSA_code[a]))   # temperature subset for MSA
  
  for(b in 1:dim(act)[1]){  # 
    
    tmp2 <- tmp[((tmp$hr > (act$hr.s[b] - 3)) & (as.Date(tmp$YrMthDay) == as.Date(act$DateTimeStart[b]))) | ((tmp$hr < (act$hr.e[b] + 3)) & (as.Date(tmp$YrMthDay) == as.Date(act$DateTimeEnd[b]))),]
    
    z1 <- which.min(abs(difftime(act$DateTimeStart[b],tmp2$DateTime, units = "mins")))
    z2 <- which.min(abs(difftime(act$DateTimeMid.f[b],tmp2$DateTime, units = "mins")))
    z3 <- which.min(abs(difftime(act$DateTimeMid[b],tmp2$DateTime, units = "mins")))
    z4 <- which.min(abs(difftime(act$DateTimeMid.b[b],tmp2$DateTime, units = "mins")))
    z5 <- which.min(abs(difftime(act$DateTimeEnd[b],tmp2$DateTime, units = "mins")))
    
    act$temp1[b] <- ifelse(tmp2$temp[z1] > 61, NA, tmp2$temp[z1])
    act$temp2[b] <- ifelse(tmp2$temp[z2] > 61, NA, tmp2$temp[z2])
    act$temp3[b] <- ifelse(tmp2$temp[z3] > 61, NA, tmp2$temp[z3])
    act$temp4[b] <- ifelse(tmp2$temp[z4] > 61, NA, tmp2$temp[z4])
    act$temp5[b] <- ifelse(tmp2$temp[z5] > 61, NA, tmp2$temp[z5])
  }
  
  act <- rbind(act, get(paste0(metros$abbr[a], ".in")))
  write.csv(act, file = file.path(dir.out,paste0("20170925_beta_",metros$OutMerge[a])), row.names = F)
  assign(paste0(metros$abbr[a],".m"), act)  # assign abbr to name 'act' for each metro
}

t9 <- Sys.time()

difftime(t9,t0, units = "hours") 
                             
##############
# STEP FOUR #
############

# NOTES:
#   The person selected to be interviewed for ATUS (i.e. the respondent) is always TULINENO = 1 where it exists.
#   Because only one person is interviewed per household, each TUCASEID on the Activity, Respondent, and Summary file identifies a unique respondent.
#   TULINENO is NOT in the Activity, Respondent, or Summary files (because it would = 1 for all cases). 
#   In the Activity file, each line is a seperate activity. TUACTIVITY_N is the activity number for each respondent. Therefore, every respondent has multiple cases in the Activity file. 
#   PTDTRACE is race. On atuscps and atussum only.
#   PRDASIAN is detailed asian race. On atuscps only. 
#   PRDTHSP is detailed hispanic orgin group. On atuscps only.
#   HUFAMINC is family income (2009 and earlier). On atuscps only.
#   HEFAMINC is family income (2010 and greater). On atuscps only.
#   TEAGE is age (atusrost, atussum). Use over PRTAGE (age on atuscps).
#   TESEX (atusrost, atussum) or PRSEX (atuscps) is sex. Use either.
#   TUFNWGTP is final ATUS weight. On atusresp or atussum. Uses 2006 methodology (may be outdated).

#   Desirable variables not yet included in meta files include: TEAGE (atusrost or atussum), TUDIARYDAY and TUFNWGTP (atusresp or atussum).
#   Therefore we will only load the Summary file to get these. 


################################################################

