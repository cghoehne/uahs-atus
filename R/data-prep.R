
  #$%#$%#$%#$%#$%#$%#$%#$%#$%#$%
 #$%     ATUS DATA PREP     #$%
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%

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
atus1 <- readRDS(here("data/import/atus1_filtered_0315.rds"))
atus2 <- readRDS(here("data/import/atus2_filtered_0315.rds"))
atus <- rbind(atus1,atus2)
rm(atus1,atus2)

# load list of metros for indexing
metros <- readRDS(here("data/import/msa_list.rds"))

# load binded NCDC weather data for metros
w.data <- readRDS(here("data/import/ncdc_filtered_0315.rds"))

# load atus location and acivity lookup tables
loc.table <- readRDS(here("data/import/location_table.rds"))
act.table <- readRDS(here("data/import/activity_table.rds"))

# rename MSA IDs to "ID" for matching
setnames(atus, "METFIPS", "ID")
setnames(w.data, "station", "ID")

# ADJUST WEATHER DATA
# calculate Heat Index via NWS equations for weather data
w.data[, heat := heat.index(t = TEMP, dp = DEWP, temperature.metric = "fahrenheit", output.metric = "fahrenheit", round = 1)]
w.data[, temp := heat]
w.data[heat < 80, temp := as.numeric(TEMP)] # use air temp if below 80 F

# correct the datetime format
w.data$DateTime <- ymd_hms(w.data$DateTime)

# fill in missing hourly temps with avg for that hour in +/- 30 days
w.data[, YrMthHr := paste0(year(w.data$DateTime),"-",month(w.data$DateTime),"-",hour(w.data$DateTime))] # unique character of year-month-hour for averages
w.data[, mtemp.YrMthHr := mean(temp, na.rm = T), by = .(ID,YrMthHr)] # calculate mean for each metro for each unique year-month-hour combo
w.data[is.na(temp), temp := mtemp.YrMthHr] # replace NAs w/ metro-year-month-hr mean

# PREP ATUS DATA
# Creates a date + time column as "YYYY-MM-DD HH:MM:SS TZ"
atus$DateTimeStart <- ymd_hms(paste(atus$TUDIARYDATE,atus$TUSTARTTIM))
atus$DateTimeEnd <- ymd_hms(paste(atus$TUDIARYDATE,atus$TUSTOPTIME))

# calucate mid point time and the quarter and 3 quarters times (for calculating avg temp during act)
atus$DateTimeMid <- atus$DateTimeEnd + (difftime(atus$DateTimeEnd, atus$DateTimeStart, units = "mins") * 0.50)
atus$DateTimeMid.f <- atus$DateTimeEnd + (difftime(atus$DateTimeEnd, atus$DateTimeStart, units = "mins") * 0.25)
atus$DateTimeMid.b <- atus$DateTimeEnd + (difftime(atus$DateTimeEnd, atus$DateTimeStart, units = "mins") * 0.75)

# Remove activities with a missing timestamp
atus <- atus[!is.na(DateTimeMid)]

# income: HEFAMINC if year < 2010, HUFAMINC if year >= 2010
atus$Income <- NA
atus[HUFAMINC < 0, Income := atus$HEFAMINC]
atus[HEFAMINC < 0, Income := atus$HUFAMINC]


# change timezone to be correct for each MSA (cause respondants reprot local time)
# must split and assign because can't assign vectorized timezones (lame!)
atus <- merge(atus, metros[, .(abbr,Timezone)], by = "abbr")
atus.l <- split(atus, as.factor(atus$ID)) # split atus data into dt list by MSA
for(a in 1:length(atus.l)){
  atus.l[[a]][, DateTimeStart := with_tz(DateTimeStart, tzone = unique(atus.l[[a]]$Timezone))]
  atus.l[[a]][, DateTimeEnd := with_tz(DateTimeEnd, tzone = unique(atus.l[[a]]$Timezone))] 
}
atus <- rbindlist(atus.l) # rebind list of DTs

# Tag activity to be either Outside, Inside, or Intermediate

# location categories
# 1 = indoor
# 2 = intermediate
# 3 = outdoor
# 4 = unknown

# Assume that:
# If one of activity or location is "4 - Unknown" but the other is 1, 2, or 3, override to repective 1, 2, or 3.
# If location is outdoors (one of "Outdoors away from home", "Walking", or "biking"), assume that these always are outdoors and ignore if activty is coded 1 or 2.
# The only possible conflict is if someone is "walking" indoors, but they should select "place" if indoors.
# If an activity is "2 - intermediate" overwrite if the location is 1 or 3. (or 2 or 4 for that matter)
# Once activity location is recoded to 1 - 4, overwrite indoor activities temperatures with 21C. Intermediate should be (21C + act$AvgTemp) / 2

# merge activities and new locations
atus <- merge(atus, loc.table, by.x="TEWHERE", by.y="loc_code")
atus <- merge(atus, act.table, by.x="TRCODEP", by.y="act_code")

# assign master location code (loc_m)
atus[, loc_m := 4][, loc_m := as.integer(loc_m)] # recode to master location code: loc_m. Assume that "unknown" (4) is default to be overwritten.
atus[loc_l == loc_a, loc_m := loc_l] # if the location and activity codes are equal, assign loc_m to be their value
atus[loc_l == 4, loc_m := loc_a] # if location code is unknown, assign loc_m based on activity code
atus[loc_a == 4, loc_m := loc_l] # if activity code is unknown, assign loc_m based on location code
atus[loc_a == 2 | loc_l == 2, loc_m := 2] # if activity code is intermediate, assign loc_m to be intermediate 
atus[loc_a == 3 | loc_l == 3, loc_m := 3] # if activity or location code is outdoors, assign loc_m to be outdoors (overwrites if 1 was unknown)

# assign activites as outdoors or not
atus[, outdoors := 0]
atus[loc_m == 3, outdoors := 1]

# prep for merging temps to atus data
atus.o <- atus[outdoors == 1]
w.data[, ID := as.integer(as.character(ID))] # set ID as integer to match
w.data[, DateTimeStart := DateTime][, DateTimeMid.f := DateTime][, DateTimeMid := DateTime][, DateTimeMid.b := DateTime][, DateTimeEnd := DateTime]

# match temperatures to outdoor activies by each of 5 time splits 
w.data.m1 <- w.data[, .(ID, temp, DateTimeStart)] # only keep relevant vars
setnames(w.data.m1, "temp", "tempStart") # rename to keep seperate after multiple merges
setkey(atus.o, ID, DateTimeStart)
setkey(w.data.m1, ID, DateTimeStart)[, w.Date.TimeStart := DateTimeStart] # same but also include a new named DateTime var to keep in matched
atus.o <- w.data.m1[atus.o, roll = 'nearest']

w.data.m2 <- w.data[, .(ID, temp, DateTimeMid.f)] # only keep relevant vars
setnames(w.data.m2, "temp", "tempMid.f") # rename to keep seperate after multiple merges
setkey(atus.o, ID, DateTimeMid.f)
setkey(w.data.m2, ID, DateTimeMid.f)[, w.DateTimeMid.f := DateTimeMid.f] # same but also include a new named DateTime var to keep in matched
atus.o <- w.data.m2[atus.o, roll = 'nearest']

w.data.m3 <- w.data[, .(ID, temp, DateTimeMid)] # only keep relevant vars
setnames(w.data.m3, "temp", "tempMid") # rename to keep seperate after multiple merges
setkey(atus.o, ID, DateTimeMid)
setkey(w.data.m3, ID, DateTimeMid)[, w.DateTimeMid := DateTimeMid] # same but also include a new named DateTime var to keep in matched
atus.o <- w.data.m3[atus.o, roll = 'nearest']

w.data.m4 <- w.data[, .(ID, temp, DateTimeMid.b)] # only keep relevant vars
setnames(w.data.m4, "temp", "tempMid.b") # rename to keep seperate after multiple merges
setkey(atus.o, ID, DateTimeMid.b)
setkey(w.data.m4, ID, DateTimeMid.b)[, w.DateTimeMid.b := DateTimeMid.b] # same but also include a new named DateTime var to keep in matched
atus.o <- w.data.m4[atus.o, roll = 'nearest']

w.data.m5 <- w.data[, .(ID, temp, DateTimeEnd)] # only keep relevant vars
setnames(w.data.m5, "temp", "tempEnd") # rename to keep seperate after multiple merges
setkey(atus.o, ID, DateTimeEnd)
setkey(w.data.m5, ID, DateTimeEnd)[, w.DateTimeEnd := DateTimeEnd] # same but also include a new named DateTime var to keep in matched
atus.o <- w.data.m5[atus.o, roll = 'nearest']

atus.o[, mid.time.dif := difftime(DateTimeMid, w.DateTimeMid, units = "mins")] # calculate time difference
max(atus.o$mid.time.dif, na.rm = T)
# Time difference of 515.5 mins 
# NEED TO FIND FIX FOR HIGH GAP SAMPLES

# calculate weighted temp and round 
atus.o[, temp := signif((tempStart / 8) + (tempMid.f / 4) + (tempMid / 4) + (tempMid.b / 4) + (tempEnd / 8), 3)]

# remove unneeded vars
atus.o[, `:=`(tempStart = NULL, tempMid.f = NULL, tempMid = NULL, tempMid.b = NULL, tempEnd = NULL)]

# bind back together, fill new cols with NAs
atus <- rbind(atus.o, atus[outdoors == 0], fill = T)

# remove more unneeded vars
atus[, `:=`(DateTimeMid.b = NULL, DateTimeMid.f = NULL)]

# save output, split into two files so it can sync to github
saveRDS(atus[1:floor(.N/2)], here("data/export/atus1.rds"))
saveRDS(atus[(floor(.N/2)+1):(.N)], here("data/export/atus2.rds"))


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