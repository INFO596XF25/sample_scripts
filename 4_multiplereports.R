# Simple loop to generate reports for multiple species
library(quarto)

# Define species list
species_list <- c("Danaus plexippus", "Libytheana carinenta")

# Loop through each species
for (species in species_list) {
  
  quarto_render(
    input = "3_quartosample.qmd",
    execute_params = list(
      species = species,
      region = "United States",
      n_records = 500
    ),
    output_file = paste0(gsub(" ", "_", species), "_report.html")
  )
}


