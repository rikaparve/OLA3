library(httr)
library(jsonlite)

# Get the URL
url <- "https://api.smk.dk/api/v1/art?object_number=KMS638"

rawres <- GET(url=url)
rawres$status_code

# Parse content
rawres$content
rescont <- httr::content(rawres, as="text")

rescontlist <- fromJSON(rescont)
awdf <- rescontlist$items


# Loop to get more info
baseurl <- "https://api.smk.dk/api/v1/art?object_number="
resdf2 <- getAW("KMS552")

getAW <- function(awid) {
  totalurl=paste0(baseurl,awid)
  rawres <- GET(url=totalurl)
  rawres$status_code
  rescont <- httr::content(rawres, as="text")
  rescontlist <- fromJSON(rescont)
  awdf <- rescontlist$items
  return(awdf)
}

# Jens Juels malerier with ID number
jensurl <- "https://api.smk.dk/api/v1/person?id="
resdf_JJ <- getJJ("1092_person")

getJJ <- function(arid) {
  totalurl=paste0(jensurl,arid)
  rawres <- GET(url=totalurl)
  rawres$status_code
  rescont <- httr::content(rawres, as="text")
  rescontlist <- fromJSON(rescont)
  ardf <- rescontlist$items
  return(ardf)
}

works <- unlist(resdf_JJ$works)
malerier <- grep("KMS", works, value = TRUE)
malerier

# Jens Juels malerier with name only
jensurl2 <- "https://api.smk.dk/api/v1/person/search?keys=Jens%20Juel&qfields=name&offset=0&rows=10"
resdf_JJ2 <- getJJ2("")

getJJ2 <- function(arid) {
  totalurl=paste0(jensurl2,arid)
  rawres <- GET(url=totalurl)
  rawres$status_code
  rescont <- httr::content(rawres, as="text")
  rescontlist <- fromJSON(rescont)
  ardf <- rescontlist$items
  return(ardf)
}

works2 <- unlist(resdf_JJ2$works)
malerier2 <- grep("KMS", works2, value = TRUE)
malerier2

lv <- malerier == malerier2
sum(lv)

alle_malerier <- paste0(malerier, collapse=",")
alle_malerier

# Get the info on the paintings
malerier_url <- "https://api.smk.dk/api/v1/art?object_number=KMS6251%2CKMS3635%2CKMS1729%2CKMS1560%2CKMSsp864%2CKMS4810%2CKMSsp868%2CKMS3277%2CKMS6151%2CKMS137%2CKMS3993%2CKMS4285%2CKMS3603%2CKMS6220%2CKMSst103%2CKMS396%2CKMS2098%2CKMSsp867%2CKMS4184%2CKMS6604%20verso%2CKMS6604%2CKMS3361%2CKMS3944%2CKMS7100%2CKMS7597%2CKMS1974%2CKMS247%2CKMS349a%2CKMS3191%2CKMSsp869%2CKMS1766%2CKMS6269%2CKMSsp865%2CKMS3764%2CKMS1073%2CKMS3980%2CKMS3080%2CKMS1114%2CKMSsp871%2CKMS3865%2CKMS1113%2CKMS2081%2CKMS1115%2CKMS349%2CKMS3398%2CKMS1824%2CKMS1816%2CKMS3282%2CKMS1765%2CKMS2084%2CKMS3786%2CKMS370%2CKMS1445%2CKMS4795%2CKMSsp870%2CKMS3943%2CKMS4127%2CKMS4806%2CKMS3499%2CKMS941%2CKMS1544%2CKMS816%2CKMS1768%2CKMSB%2026%2CKMS254%2CKMS6806%2CKMS1619%2CKMSsp872%2CKMS1929%2CKMS1308%2CKMS4508a%2CKMS1930%2CKMS3275%2CKMS4801%2CKMS3634%2CKMS1097%2CKMS1444%2CKMS3421%2CKMS1099%2CKMS3242%2CKMS1098%2CKMS1002%2CKMS797%2CKMS1732%2CKMSst735%2CKMSst734%2CKMS3990%2CKMS7458%2CKMS6980%2CKMS7661%2CKMSsp866"
resdf_mr <- getmr("")

getmr <- function(mrid) {
  totalurl=paste0(malerier_url,mrid)
  rawres <- GET(url=totalurl)
  rawres$status_code
  rescont <- httr::content(rawres, as="text")
  rescontlist <- fromJSON(rescont)
  mrdf <- rescontlist$items
  return(mrdf)
}

første <- min(resdf_mr$acquisition_date_precision)
sidste <- max(resdf_mr$acquisition_date_precision)



### Open sky
# Load lubridate package
library(lubridate)

# Define the date and time string
date_str <- "2017-07-14 04:40:00"

# Parse the date with the CEST timezone
date_time <- ymd_hms(date_str, tz = "CEST")

# Convert to UTC and then to numeric (Unix timestamp)
unix_time <- as.numeric(with_tz(date_time, tzone = "UTC"))

unix_time

options(scipen=999)
library(keyring)
key_set(service = "opensky", username = "redrika")
url="https://opensky-network.org/api/states/all"

# Send a GET request with authentication
rawres <- GET(
  url = url,
  authenticate(user = "redrika", password = key_get("opensky", username = "redrika"))
)

rawcontent=content(rawres, as="text")
stlist <- fromJSON(rawcontent)
stlist_df <- as.data.frame(stlist)
colnames(stlist_df) <- c("time_stamp",
  "icao24", "callsign", "origin_country", "time_position", "last_contact",
  "longitude", "latitude", "baro_altitude", "on_ground", "velocity", "vertical_rate", "sensors", "geo_altitude", "squawk",
  "spi", "position_source", "category"
)

freq_land <- as.data.frame(sort(table(stlist_df$origin_country), descending=T))

library(ggplot)
ggplot(data = freq_land, aes(x = Var1, y = Freq, fill=Var1)) +
  geom_bar(stat = "identity")

#### Retrieve country data

baseurl="https://opensky-network.org/api/states/all"
getSKY <- function(flyid) {
  totalurl=paste0(baseurl,flyid)
  rawres <- GET(url = totalurl,
                  authenticate(user = "redrika", password = key_get("opensky", username = "redrika")))
  rawcontent=content(rawres, as="text")
  fly_list <- fromJSON(rawcontent)
  return(fly_list)
}

string <- paste0(freq_land$Var1[1:10])
lv <- stlist_df$origin_country %in% string
stlist_df <- stlist_df[lv,]
icons <- unique(stlist_df$icao24)

getSKY(flyid)    




#########################

# ??
args <- commandArgs(trailingOnly=TRUE)

operator <- args[1]
operator <- "RYR"

url="https://opensky-network.org/api/states/all"

# Send a GET request with authentication
rawres <- GET(
  url = url,
  authenticate(user = "redrika", password = key_get("opensky", username = "redrika"))
)

rawcontent=content(rawres, as="text")
stlist <- fromJSON(rawcontent)
stlist_df <- as.data.frame(stlist)
colnames(stlist_df) <- c(
  "time", "icao24", "callsign", "origin_country", "time_position", 
  "last_contact", "longitude", "latitude", "baro_altitude", "on_ground", 
  "velocity", "true_track", "vertical_rate", "sensors", "geo_altitude", 
  "squawk", "spi", "position_source")

pattern <- paste0("^", operator)
flight <- stlist_df[grep(pattern, stlist_df$callsign),]
NoFlight <- nrow(flight)

output <- paste0("Timestamp is: ", as.character(as.numeric(Sys.time())),
                 " Operator is: ", operator, " Number of flights is: ", NoFlight)


## Append - add lines to the existing file and not create a new each time
write(output, file = "Output.txt", ncolumns = 1, append = TRUE)


