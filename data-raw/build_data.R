# Build data
library(purrr)
library(tidyr)

## Build example ELL baseline data
ell <- read.csv("data-raw/ellDist.csv")
ell <- na.omit(ell)
ell <-  ell %>% gather(-age, key = "race", value = "prob")

## Build example SDP/CEDS crosswalk
xwalk <- read.csv("data-raw/CEDS_SDP_map.csv", stringsAsFactors = FALSE)
xwalk$schema <- NA
for(i in 1:nrow(xwalk)){
  xwalk$schema[i] <- I(list(OpenSDP.data:::get_code_values(xwalk$CEDS_Option_set[i])))
}

# SES data
ses <- data.frame(race = c("black", "asian", "hispanic", "amerind", "white",
                           "other", "multiracial", "hawaiian_pi"),
                  prob = c(0.65, 0.375, 0.6, 0.4, 0.4, 0.4, 0.4, 0.4))
ses$race <- as.character(ses$race)

# Program Baseline
prog_baseline <- read.csv("data-raw/program_baseline.csv")
prog_baseline[, 1] <- as.character(prog_baseline[, 1])
prog_baseline[, 2] <- as.character(prog_baseline[, 2])
prog_baseline[, 3] <- as.character(prog_baseline[, 3])
names(prog_baseline) <- tolower(names(prog_baseline))
prog_baseline$frpl[prog_baseline$frpl == "2"] <- "1"
prog_baseline <- prog_baseline %>% group_by(ell, iep, frpl) %>%
  summarize(count = sum(count), prob = sum(prob)) %>% as.data.frame()
# Map CEDS names and option names
# prog_baseline[, 1:3] <- recode_options(prog_baseline[, 1:3], from = "SDP")
# names(prog_baseline)[1:3] <- map_CEDS(names(prog_baseline)[1:3])

## Pull in age_grade baseline
age_grade <- read.csv("data-raw/age_grade_baseline.csv")
names(age_grade) <- c("age", paste0("g", -1:12), "total")
age_grade[, 2:15] <- round(age_grade[, 2:15] / rowSums(age_grade[, 2:15]), 5)
age_grade$total <- NULL

sch_names <- c("Jackson", "Wallaby", "Kendrick", "Willow Creek", "Cypress",
                    "Dover Hills", "Milton South", "Spring Port", "Dogwood", "Topshire",
                    "Hillside", "Donohue", "Upper Falls", "George", "Avondale", "East Valley",
                    "Township", "Clark", "Pollock", "Duvall", "Santiago", "Hickory",
                    "Robin", "Warren", "Alvin", "Allen", "Haskin", "Hawthorne", "Woodcliff",
                    "Wright", "Buchanan", "Oriole", "Diamond Lake", "Walnut", "Central",
                    "Arbor", "Gail Hill", "Southbridge", "Silver Oak", "Marsden",
                    "Crossroads", "Meridian", "Linden", "Success", "Jefferson", "Columbia",
                    "Falcon", "Apollo", "Hampshire", "Madison", "Briarfield", "Jubilee",
                    "Magnolia", "Bauer", "Stone Street", "Angelea", "Riva Ridge",
                    "Perez", "Lincoln", "Goldeneye", "Hazel", "Vidalia", "Lewis",
                    "Freedman", "Venice", "Wintergreen", "Holden", "Beebe", "Red Hills",
                    "Floral", "Pressley", "Winchester", "Oak Tree", "Laurel", "Kingfisher",
                    "Verndale", "Kennedy", "Flagstaff", "Laughlin", "East Harley",
                    "Rolling Knoll", "Elizabeth", "Meadow", "Wakeland", "Rockford",
                    "Hummingbird", "Castle Rock", "Sugarplum", "London Lane", "Yawkey",
                    "Sparrow", "Watercress", "Lowell", "Mountain", "Bilford", "Maple",
                    "Ritchie", "Dalton", "Homer", "Lower Falls", "Midlands", "Pike",
                    "Tucker", "Whitebridge", "Pinkney", "Athena", "Acorn", "Wilson",
                    "Common Way", "Eastbridge", "Blue Hills", "Onassis", "Capital City",
                    "Wildwood", "Noble", "John Brown", "Ferdinand", "Weeks", "Majestic",
                    "Starlight", "Hemingway", "Spellman", "Olivero", "Cornerstone",
                    "Van Dusen", "Emblem", "Peyton", "Pleasant Lake", "West Harley",
                    "Einstein", "Frederick", "Middlewood", "Burton", "Graham", "Lone Pine",
                    "Tyler", "Yellow Creek", "Sargent", "Cathedral", "Moseley", "Justice",
                    "Avocet", "Anderson", "James", "Barclay", "Marie Curie", "Hunt Circle",
                    "Eastbourne", "Sunset", "Seaside", "Paisley", "Jupiter", "Prentice",
                    "Lockland", "Canyon", "Mayer", "Westerley", "Sweetville", "Lindgren",
                    "Carmen", "Hawking", "Lindsey", "Hunnewell", "Eagle", "Holly",
                    "Reigh Count", "Valley Way", "Navigation", "Olsen", "Daisy Hill",
                    "Hope", "Eliot", "Coleman", "Milton North", "Sage", "Lang", "Sierra Lane",
                    "Commander", "Rosa Parks", "Cicero", "Chickamee", "Labyrinth",
                    "Calvin Lee", "Nightland", "Forbes", "Sterling", "Triumph", "Wellington",
                    "MacDonald", "Englander", "Tupelo", "Beckham", "Franklin", "Astro",
                    "Aristides", "Morris", "Shortleaf", "Cardinal", "Little Valley",
                    "Applewood", "Roosevelt", "Marvel", "Juniper Hill", "Esperanza",
                    "Touchstone", "Blakeville", "Shafer", "Sebastian", "Polaris",
                    "Ponderosa", "Mount Lyon", "Lafayette", "New Beacon", "Chesterfield",
                    "Harmony", "Rosebud", "Tremont", "Fortuna", "North Star", "Everett",
                    "Basswood", "Islander", "Martin", "Peabody", "Northbridge", "Hanover",
                    "Rainbow", "Bicknell", "Courtdale", "Miller", "Mulberry", "Hadley",
                    "Greenwich", "Ridley", "Birch", "Albert", "Culver", "Chelsea",
                    "Unity", "Brookfield", "Irving", "Acacia", "Alliance", "Douglass",
                    "Brennan", "Clivedale", "Shaheen", "Tesla", "Young Oak", "Duncan",
                    "Selwyn", "Kumar", "Woodland", "Cedar", "Goldfinch", "Sullivan",
                    "Buffalo Way", "Danehill", "Simpson", "Nyquist", "Chan", "Alewife",
                    "Galaxy", "Highland", "Marlowe", "Tower", "Alonzo", "Prairie",
                    "Eureka", "King", "Humphrey", "Knightwood", "Gallant", "Boundbrook",
                    "Cherry Hill", "Kirkland", "Nobscot", "Suarez", "Floyd", "Harrison",
                    "Greenfield", "Crane", "Rhodes", "Sea Glass", "Westminster",
                    "Horan", "Copper Cove", "Fairbanks", "Marshfield", "Ashmont",
                    "Heron", "Henderson", "Ridge Park", "Kent", "Friendship", "Pegasus",
                    "Garden", "Adler", "Shuster", "Dickens", "Liberty", "Edison",
                    "Blanchard", "Camino", "Baker", "Cilian", "Speedwell", "Bootes",
                    "Darwin", "Quincy", "Cold Springs", "Chestnut", "Secretariat",
                    "Gateway", "Dudley", "Horace Mann", "Glenbrook", "Pompano", "Park",
                    "Maverick", "Hyde", "Leeds", "Percival", "Davis", "Shirley",
                    "Redwood", "Murphy", "Poplar", "Fillmore", "Thompson", "Pharaoh",
                    "Bennett", "Kinney", "Gilbert", "Marshall", "Monarch", "Doyle",
                    "Walker", "Clarion", "Spinelli", "Inspiration", "Neptune", "Gibson",
                    "Independence", "Josephine", "Finnley", "Summit", "Bloomfield",
                    "Zola", "Ventura", "Lakeshore", "Phoenix", "Honeypot", "Chateau",
                    "Carter ", "Velocidad", "Utopia", "Owen", "Turcotte", "Sorrel Ridge",
                    "Fawzi", "Woodpecker", "Chisholm", "Hoffman", "Lever", "Ace Hall",
                    "Davidson", "Weston", "Forest Heights", "Finch", "Monroe", "Caldwell",
                    "Hamilton", "Vision", "Elk Grove", "Eleanor", "Quicksilver",
                    "Longleaf", "Chiswick", "Buckeye", "Winthrop", "Austen", "Reyes",
                    "Juneberry", "Waldron", "Adams", "Westbridge", "Fraser", "Truman",
                    "Old Steeple", "Turnstone", "Memorial", "Sycamore", "Sequoia",
                    "Sylvan", "Sandy Beach", "Hathaway", "Mandalay", "Deer Corner",
                    "Clearview", "Corcoran", "Cabot", "Lookout Point")

ps_names <- c("COMMUNITY COLLEGE 400", "DEF COMMUNITY COLLEGE", "D COMMUNITY COLLEGE",
              "UVW COMMUNITY COLLEGE", "COMMUNITY COLLEGE C", "A COMMUNITY COLLEGE",
              "COMMUNITY COLLEGE 4", "E COMMUNITY COLLEGE", "COMMUNITY COLLEGE 500",
              "COMMUNITY COLLEGE B", "HKL COMMUNITY COLLEGE", "XYZ COMMUNITY COLLEGE",
              "COMMUNITY COLLEGE A", "COMMUNITY COLLEGE 1", "B COMMUNITY COLLEGE",
              "MNO COMMUNITY COLLEGE", "C COMMUNITY COLLEGE", "COMMUNITY COLLEGE 6",
              "PRIVATE TECHNICAL INSTITUTE", "COMMUNITY COLLEGE 100", "COMMUNITY COLLEGE 7",
              "COMMUNITY COLLEGE 2", "COMMUNITY COLLEGE 200", "GHI COMMUNITY COLLEGE",
              "COMMUNITY COLLEGE 3", "PQR COMMUNITY COLLEGE", "COMMUNITY COLLEGE 5",
              "ABC COMMUNITY COLLEGE", "COMMUNITY COLLEGE 300", "STU COMMUNITY COLLEGE",
              "", "UNIVERSITY XYZ", "COLLEGE OF XYZ", "UNIVERSITY OF C", "ABC STATE UNIVERSITY",
              "STATE UNIVERSITY - NORTH CAMPUS", "PRIVATE COLLEGE A", "UNIVERSITY A",
              "UNIVERSITY - MAIN CAMPUS", "PUBLIC UNIVERSITY 1", "STATE UNIVERSITY - EAST CAMPUS",
              "PRIVATE COLLEGE B", "UNIVERSITY OF B", "UNIVERSITY - CAMPUS 2",
              "STATE UNIVERSITY - WEST CAMPUS", "AB COLLEGE", "UNIVERSITY OF GH",
              "COLLEGE OF ABC", "COLLEGE OF DEFGH", "PUBLIC UNIVERSITY 2",
              "STATE UNIVERSITY - SOUTH CAMPUS", "", "COMMUNITY COLLEGE 400",
              "DEF COMMUNITY COLLEGE", "COLLEGE OF XYZ", "D COMMUNITY COLLEGE",
              "UVW COMMUNITY COLLEGE", "UNIVERSITY XYZ", "A COMMUNITY COLLEGE",
              "COMMUNITY COLLEGE 4", "E COMMUNITY COLLEGE", "COMMUNITY COLLEGE 500",
              "ABC STATE UNIVERSITY", "COMMUNITY COLLEGE B", "STATE UNIVERSITY - NORTH CAMPUS",
              "HKL COMMUNITY COLLEGE", "PRIVATE COLLEGE A", "UNIVERSITY A",
              "UNIVERSITY - MAIN CAMPUS", "PUBLIC UNIVERSITY 1", "STATE UNIVERSITY - EAST CAMPUS",
              "PRIVATE COLLEGE B", "UNIVERSITY OF B", "UNIVERSITY - CAMPUS 2",
              "COMMUNITY COLLEGE 1", "STATE UNIVERSITY - WEST CAMPUS", "XYZ COMMUNITY COLLEGE",
              "AB COLLEGE", "B COMMUNITY COLLEGE", "MNO COMMUNITY COLLEGE",
              "UNIVERSITY OF GH", "C COMMUNITY COLLEGE", "COMMUNITY COLLEGE 6",
              "COMMUNITY COLLEGE A", "COLLEGE OF ABC", "COLLEGE OF DEFGH",
              "PUBLIC UNIVERSITY 2", "PRIVATE TECHNICAL INSTITUTE", "COMMUNITY COLLEGE 7",
              "UNIVERSITY OF C", "COMMUNITY COLLEGE 2", "COMMUNITY COLLEGE 200",
              "GHI COMMUNITY COLLEGE", "PQR COMMUNITY COLLEGE", "COMMUNITY COLLEGE C",
              "COMMUNITY COLLEGE 5", "COMMUNITY COLLEGE 100", "ABC COMMUNITY COLLEGE",
              "COMMUNITY COLLEGE 3", "STATE UNIVERSITY - SOUTH CAMPUS", "COMMUNITY COLLEGE 300",
              "STU COMMUNITY COLLEGE")

# saveRDS(xwalk, "data/sdp_ceds_map.rds")
devtools::use_data(ell, xwalk, ses, prog_baseline, age_grade, sch_names,
                   ps_names,
                   internal = TRUE, overwrite = TRUE)
