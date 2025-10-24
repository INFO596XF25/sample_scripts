# ==============================================================================
# Biodiversity Data Science: Data Acquisition, Cleaning, and Management
# ==============================================================================

if (!require("rgbif")) install.packages("rgbif")
if (!require("here")) install.packages("here")

# Load libraries
library(rgbif)
library(here)

# ==============================================================================
# Accessing and downloading biodiversity data from GBIF
# ==============================================================================

# Search for occurrence data for monarch butterfly (Danaus plexippus)
# We will retrieve occurrence records from the United States

# First, we will get the species key from GBIF
species_name <- "Danaus plexippus" # Species name
species_key <- name_backbone(name = species_name)$usageKey # We'll find the taxonomic information in GBIF's backbone taxonomy

# Let's print species key for verification
print(paste("Species key for", species_name, ":", species_key)) #How do we fix that extra space?

# Now, we will download occurrence data from GBIF
# Timeframe: Limit to 500 records for demonstration purposes...
# Spatial context: Filter for US records from 2020-2023 with coordinates...
monarch_data <- occ_search(
    taxonKey = species_key,
    country = "US",
    hasCoordinate = TRUE,
    year = "2020,2023",
    limit = 500
)

# Now, let's look into the dataset. What type of object is this?
monarch_df <- monarch_data$data

# The structure of the downloaded data...
str(monarch_df)

# First few rows
head(monarch_df)

# ==============================================================================
# Exporting data to CSV
# ==============================================================================

# Create a data directory if it doesn't exist
if (!dir.exists(here("data"))) {
    dir.create(here("data"))
    print("Created data directory")
}

# Export the raw data to CSV
output_file <- here("data", "monarch_raw_data.csv")
write.csv(monarch_df, file = output_file, row.names = FALSE)

# ==============================================================================
# Reading data from CSV
# ==============================================================================

# Read the data back from CSV to simulate working with existing files
monarch_imported <- read.csv(output_file, stringsAsFactors = FALSE)

# Let's verify data import
print(dim(monarch_imported))

# What else can we do to verify if the dataset was imported correctly?

# ==============================================================================
# Secondary datasets (data simulation for environmental variables)
# ==============================================================================

# Usually, you would access secondary datasets from sources like
# WorldClim, PRISM, or other environmental databases
# For demonstration, we will create a simulated environmental dataset

# Create synthetic environmental data based on coordinates
set.seed(123)  # For reproducibility

# Extract unique coordinates from our biodiversity data
unique_coords <- unique(monarch_imported[, c("decimalLatitude", "decimalLongitude")])

# Simulate environmental variables
environmental_data <- data.frame(
    decimalLatitude = unique_coords$decimalLatitude,
    decimalLongitude = unique_coords$decimalLongitude,
    mean_temp = round(rnorm(nrow(unique_coords), mean = 20, sd = 5), 2),
    precipitation = round(rnorm(nrow(unique_coords), mean = 800, sd = 200), 2),
    elevation = round(rnorm(nrow(unique_coords), mean = 500, sd = 300), 2),
    habitat_quality = sample(c("high", "medium", "low"), nrow(unique_coords), replace = TRUE)
)

# Display environmental data structure
head(environmental_data)

# Export secondary dataset
env_file <- here("data", "environmental_data.csv")
write.csv(environmental_data, file = env_file, row.names = FALSE)

# ==============================================================================
# Data integration - merging multiple datasets
# ==============================================================================

# Merge biodiversity data with environmental data based on coordinates

# Let's ensure coordinate columns have consistent precision
# Round coordinates to 4 decimal places
monarch_imported$lat_rounded <- round(monarch_imported$decimalLatitude, 4)
monarch_imported$lon_rounded <- round(monarch_imported$decimalLongitude, 4)
environmental_data$lat_rounded <- round(environmental_data$decimalLatitude, 4)
environmental_data$lon_rounded <- round(environmental_data$decimalLongitude, 4)

# Merge datasets
integrated_data <- merge(
    monarch_imported,
    environmental_data,
    by = c("lat_rounded", "lon_rounded"),
    all.x = TRUE  # Why do we use this argument?
)

# Check integration results
print(dim(integrated_data))
print(sum(!is.na(integrated_data$mean_temp))) #What are we doing in this line?

# ==============================================================================
# Data cleaning and preprocessing
# ==============================================================================

# We're going to focus on key biodiversity and environmental variables
columns_to_keep <- c(
    "species", "scientificName", "decimalLatitude.x", "decimalLongitude.x",
    "eventDate", "year", "month", "day", "stateProvince", "locality",
    "coordinateUncertaintyInMeters", "basisOfRecord",
    "mean_temp", "precipitation", "elevation", "habitat_quality"
)

# Check which columns exist in our data
target_columns <- columns_to_keep[columns_to_keep %in% names(integrated_data)]
cleaned_data <- integrated_data[, target_columns]

# Remove duplicate records based on coordinates and date (what steps did I follow here?)
nrow(cleaned_data)

cleaned_data$unique_id <- paste(
    cleaned_data$decimalLatitude,
    cleaned_data$decimalLongitude,
    cleaned_data$eventDate,
    sep = "_"
)

cleaned_data <- cleaned_data[!duplicated(cleaned_data$unique_id), ]
nrow(cleaned_data)

# Let's the temporary column
cleaned_data$unique_id <- NULL

# Handle missing values in coordinate uncertainty
# Remove records with uncertainty greater than 10000 meters
if ("coordinateUncertaintyInMeters" %in% names(cleaned_data)) {
    print("Records before uncertainty filter:")
    print(nrow(cleaned_data))

    cleaned_data <- cleaned_data[
        is.na(cleaned_data$coordinateUncertaintyInMeters) |
            cleaned_data$coordinateUncertaintyInMeters <= 10000,
    ]

    print("Records after uncertainty filter:")
    print(nrow(cleaned_data))
}

# Check for and handle outlier coordinates (basic validation)
# Latitude should be between -90 and 90
# Longitude should be between -180 and 180
cleaned_data <- cleaned_data[
    cleaned_data$decimalLatitude.x >= -90 & cleaned_data$decimalLatitude.x <= 90 &
        cleaned_data$decimalLongitude.x >= -180 & cleaned_data$decimalLongitude.x <= 180,
]

# Standardize date format if eventDate exists
if ("eventDate" %in% names(cleaned_data)) {
    cleaned_data$eventDate <- as.Date(cleaned_data$eventDate)
}

# Create summary statistics
print("Summary of cleaned data:")
summary(cleaned_data)

# ==============================================================================
# Data quality assessment
# ==============================================================================

# Calculate completeness metrics
completeness <- data.frame(
    variable = names(cleaned_data),
    missing_count = sapply(cleaned_data, function(x) sum(is.na(x))),
    missing_percent = sapply(cleaned_data, function(x) round(sum(is.na(x)) / length(x) * 100, 2))
)
rownames(completeness) <- NULL #What is this for?

print("Data completeness assessment:")
print(completeness)

# Check spatial distribution
print("Spatial coverage by state:")
if ("stateProvince" %in% names(cleaned_data)) {
    state_summary <- table(cleaned_data$stateProvince)
    print(head(sort(state_summary, decreasing = TRUE), 10))
}

# Check temporal distribution
print("Temporal coverage by year:")
if ("year" %in% names(cleaned_data)) {
    year_summary <- table(cleaned_data$year)
    print(year_summary)
}

# ==============================================================================
# Export cleaned data
# ==============================================================================

# Export cleaned dataset
cleaned_file <- here("data", "monarch_cleaned_integrated.csv")
write.csv(cleaned_data, file = cleaned_file, row.names = FALSE)
print(paste("Cleaned data exported to:", cleaned_file))

# Create metadata file documenting the cleaning process
metadata <- data.frame(
    step = c(
        "1. Data acquisition",
        "2. Data integration",
        "3. Duplicate removal",
        "4. Coordinate uncertainty filter",
        "5. Missing coordinate removal",
        "6. Coordinate validation"
    ),
    records_before = c(
        nrow(monarch_imported),
        nrow(integrated_data),
        NA, # How do we track this?
        NA, # How do we track this?
        NA, # How do we track this?
        NA  # How do we track this?
    ),
    records_after = c(
        nrow(monarch_imported),
        nrow(integrated_data),
        NA, # How do we track this?
        NA, # How do we track this?
        NA, # How do we track this?
        nrow(cleaned_data)
    ),
    description = c(
        paste("Downloaded", nrow(monarch_imported), "records from GBIF"),
        paste("Merged with environmental data at", nrow(unique_coords), "unique locations"),
        "Removed duplicate records based on coordinates and date",
        "Removed records with coordinate uncertainty > 10000m",
        "Removed records with missing coordinates",
        "Validated coordinate ranges"
    )
)

metadata_file <- here("data", "data_processing_metadata.csv")
write.csv(metadata, file = metadata_file, row.names = FALSE)

# ==============================================================================
# Basic exploratory analysis
# ==============================================================================

# Plot 1: Spatial distribution of observations
plot(
    cleaned_data$decimalLongitude,
    cleaned_data$decimalLatitude,
    main = "??",
    xlab = "Longitude",
    ylab = "Latitude",
    pch = 19,
    col = rgb(0, 0, 1, 0.3), #What are these elements for?
    cex = 0.8
)

# Plot 2: Relationship between temperature and observations

hist(
    cleaned_data$mean_temp,
    main = "Distribution of mean temperature at observation sites",
    xlab = "Mean temperature (°C)",
    col = "coral",
    breaks = 20
)
