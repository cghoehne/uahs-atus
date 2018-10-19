
  #$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#
 #$%    ATUS DATA SYNTHESIS META CODE    #$%
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#

memory.limit(size = 56000)

# list of all dependant packages
list.of.packages <- c("lubridate",
                      "weathermetrics",
                      "tidyverse",
                      "data.table", 
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
lapply(list.of.packages, library, character.only = TRUE)

# load ATUS data filtered and geocoded (to relevant MSAs) for years 2003 to 2015
# file includes relevant columns from ATUS activity, CPS, respondant, & summary and original CPS file 
# merged on activities for all ATUS respondants in MSA sample
atus1 <- readRDS(here("data/atus1_filtered_0315.rds"))
atus2 <- readRDS(here("data/atus2_filtered_0315.rds"))
atus <- rbind(atus1,atus2)
rm(atus1,atus2)

# load list of metros for indexing
metros <- readRDS(here("data/msa_list.rds"))

# load binded NCDC weather data for metros
w.data <- readRDS(here("data/ncdc_filtered_0315.rds"))

# load atus location and acivity lookup tables
loc.table <- readRDS(here("data/location_table.rds"))
act.table <- readRDS(here("data/activity_table.rds"))

# Creates a date + time column as "YYYY-MM-DD HH:MM:SS TZ"
atus$DateTimeStart <- ymd_hms(paste(atus$TUDIARYDATE,atus$TUSTARTTIM))
atus$DateTimeEnd <- ymd_hms(paste(atus$TUDIARYDATE,atus$TUSTOPTIME))

# rename MSA IDs to "ID" for matching
setnames(atus, "METFIPS", "ID")
setnames(w.data, "station", "ID")

# calucate mid point time and the quarter and 3 quarters times (for calculating avg temp during act)
atus$DateTimeMid <- atus$DateTimeEnd + (difftime(atus$DateTimeEnd, atus$DateTimeStart, units = "mins") * 0.50)
atus$DateTimeMid.f <- atus$DateTimeEnd + (difftime(atus$DateTimeEnd, atus$DateTimeStart, units = "mins") * 0.25)
atus$DateTimeMid.b <- atus$DateTimeEnd + (difftime(atus$DateTimeEnd, atus$DateTimeStart, units = "mins") * 0.75)

# create a Year-Month-Day and a Year-Month variable to use for matching with temps
atus$YrMthDay <- format(strptime(atus$DateTimeMid, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d")
atus$YrMth <- format(strptime(atus$DateTimeMid, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m")

# Remove activities with a missing timestamp
atus <- atus[!is.na(DateTimeMid)]

# hour start and end for matching
atus$hr.s <- as.numeric(format(strptime(atus$DateTimeStart, format="%Y-%m-%d %H:%M:%S"), format="%H"))
atus$hr.e <- as.numeric(format(strptime(atus$DateTimeEnd, format="%Y-%m-%d %H:%M:%S"), format="%H"))

# income: HEFAMINC if year < 2010, HUFAMINC if year >= 2010
atus$Income <- NA
atus[HUFAMINC < 0, Income := atus$HEFAMINC]
atus[HEFAMINC < 0, Income := atus$HUFAMINC]

#~<@>~~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~#
#    Match temps to activities by MSA      #
#~<@>~~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~<@>~#

# change timezone to be correct for each MSA (cause respondants reprot local time)
# must split and assign because can't assign vectorized timezones (lame!)
atus <- merge(atus, metros[, .(abbr,Timezone)], by = "abbr")
atus.l <- split(atus, as.factor(atus$ID)) # split atus data into dt list by MSA
for(a in 1:length(atus.l)){
  atus.l[[a]][, DateTimeStart := with_tz(DateTimeStart, tzone = unique(atus.l[[a]]$Timezone))]
  atus.l[[a]][, DateTimeEnd := with_tz(DateTimeEnd, tzone = unique(atus.l[[a]]$Timezone))] 
}
atus <- rbindlist(atus.l) # rebind list of DTs

# formating NCDC weather data
w.data$hr <- as.numeric(format(strptime(w.data$DateTime, format="%Y-%m-%d %H:%M:%S"), format="%H"))

# Calc relative humidity
w.data$rh <- exp((17.625*w.data$DEWPC)/(243.04+w.data$DEWPC))/exp((17.625*w.data$TEMPC)/(243.04+w.data$TEMPC))

# calculate Heat Index via NWS equations
w.data[, heat := heat.index(t = TEMP, dp = DEWP, temperature.metric = "fahrenheit", output.metric = "fahrenheit", round = 0)]
w.data[, temp := heat]
w.data[heat < 80, temp := TEMP] # use air temp if below 80 F

# correct datetime format
w.data$DateTime <- ymd_hms(w.data$DateTime)

# fill in missing hourly temps with avg for that hour in +/- 30 days
for(i in 1:dim(w.data)[1]){
  if(is.na(w.data$temp[i])){
    w.data.s <- subset(w.data, (as.Date(YrMthDay) > as.Date(YrMthDay[i]) - 15) & (as.Date(YrMthDay) < as.Date(YrMthDay[i]) + 15) & (hr[i] == hr))
    w.data$temp[i] <- ifelse(!is.na(mean(w.data.s$temp, na.rm = T)), mean(w.data.s$temp, na.rm = T), NA)
  } 
}


  #$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%
 #$%   Master Recode of Activities   #$%
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%

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



# test matching
atus.t <- atus[ID %in% c(10740,46060)]
atus.t[, DateTime := DateTimeMid]
atus.t <- atus.t[, .(DateTime,ID)]
w.data.t <- w.data[, .(DateTime,ID,TEMP)]
w.data.t[, ID := as.integer(as.character(ID))]

setkey(atus.t, ID, DateTime) # setkey as ID and DateTime
setkey(w.data.t, ID, DateTime)[, t.Date.Time := DateTime] # same but also include a new named DateTime var to keep in matched

atus.m <- w.data.t[atus.t, roll = 'nearest']
atus.m[, time.dif := difftime(DateTime,t.Date.Time, units = "mins")]


                     
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

