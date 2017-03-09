# Build data


## Build example ELL baseline data
ell <- read.csv("inst/ellDist.csv")
ell <- na.omit(ell)
ell <- ell[-1,]
saveRDS(ell, "data/ell.rds")


## Build example SDP/CEDS crosswalk
xwalk <- read.csv("inst/CEDS_SDP_map.csv", stringsAsFactors = FALSE)
xwalk$schema <- NA
for(i in 1:nrow(xwalk)){
  xwalk$schema[i] <- I(list(get_code_values(xwalk$Option.Set[i])))
}

saveRDS(xwalk, "data/sdp_ceds_map.rds")