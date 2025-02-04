#if(!require(stringdist)) install.packages("stringdist")
library(stringdist)
library(data.table)
library(dplyr)

# Read DHIS2 data
dhis_data <- readRDS("data/dhis_data.rds")

# Filter unique rows for the specified columns
dhis_org_unit_data <- dhis_data %>%
  select(organisation_unit_name, region, zone, woreda, phcu, facility) %>%
  distinct()
process_dhis2_org_unit_data <- copy(dhis_org_unit_data)

# Read Woreda list from AHRI
woreda_from_ahri <- read.csv("data/woreda_from_ahri.csv")

# Merge and fuzzy match
for (i in 1:nrow(dhis_org_unit_data)) {
  # Merge based on region and zone
  merged_data <- merge(dhis_org_unit_data[i, ], woreda_from_ahri, 
                       by.x = c("region", "zone"), 
                       by.y = c("dhis2_region", "dhis2_zone"))
  
  # Check if merged_data has any rows
  if (nrow(merged_data) == 0) {
    print(paste("No matching rows found for row", i))
    process_dhis2_org_unit_data$visual_inspection[i] <- NA  # Mark as NA
    process_dhis2_org_unit_data$distance[i] <- NA
    next  # Skip this iteration if no rows are found
  }
  
  # Take a single woreda from dhis_org_unit_data and retrieve a list of all woredas within the same zone for the current iteration row
  woreda_dhis <- dhis_org_unit_data$woreda[i]
  ahri_districts <- merged_data$Woreda_1082
  
  # If woreda_dhis is NA or a health facility,  use organisation_unit_name and dhis2_woreda for fuzzy matching
  if (is.na(woreda_dhis) || grepl("Hospital$|Health Center$", woreda_dhis)) {
    woreda_dhis <- dhis_org_unit_data$organisation_unit_name[i]
    ahri_districts <- merged_data$dhis2_woreda
  }

  # Fuzzy matching
  distances <- stringdist::stringdist(woreda_dhis, ahri_districts, method = "jw")

  # Replace NA values with a large number
  distances[is.na(distances)] <- 1000
  
  min_distance <- min(distances)
  closest_match_index <- which.min(distances)
  
  # Assign closest match
  process_dhis2_org_unit_data[i, names(merged_data)] <- merged_data[closest_match_index, ]
  process_dhis2_org_unit_data$distance[i] <- min_distance
  # if min_distance is greater than 0.3 mark it for visual inspection
  if (min_distance > 0.3) {
    process_dhis2_org_unit_data$visual_inspection[i] <- TRUE  # Mark for Visual Inspection
  } else {
    process_dhis2_org_unit_data$visual_inspection[i] <- FALSE # Mark as OK
  }
  
}

# Export the merged org unit data to csv
write.csv(process_dhis2_org_unit_data, "data/dhis_org_unit_data.csv", row.names = FALSE)
print("Done")