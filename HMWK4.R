###OBJECTIVE 1###

# Install palmerpenguins if not already installed
if (!require(palmerpenguins)) {
  install.packages("palmerpenguins")
}
library(palmerpenguins)

# Check the first few rows of the data
head(penguins)
binary_converter <- function(x, breakpoint, labels = c("low", "high")) {
  if(length(labels) != 2) stop("You must provide exactly 2 labels")
  result <- ifelse(x <= breakpoint, labels[1], labels[2])
  return(result)
}
# Remove NAs from body_mass_g
penguins$body_mass_g_binary <- binary_converter(penguins$body_mass_g,
                                                breakpoint = 4000, 
                                                labels = c("small", "large"))

# Check results
table(penguins$body_mass_g_binary)

###OBJECTIVE 2###
multi_converter <- function(x, breakpoints, labels) {
  if(length(labels) != (length(breakpoints) + 1)) {
    stop("Number of labels must be one more than number of breakpoints")
  }
  
  # Initialize output
  result <- cut(x, breaks = c(-Inf, breakpoints, Inf), labels = labels)
  return(result)
}

# Convert to 3 categories: small, medium, large
penguins$body_mass_g_cat <- multi_converter(penguins$body_mass_g,
                                            breakpoints = c(3500, 4500),
                                            labels = c("small", "medium", "large"))

table(penguins$body_mass_g_cat)

###OBJECTIVE 3###

# Calculate breakpoints for each species
species_breaks <- tapply(penguins$body_mass_g,
                         penguins$species,
                         function(x) quantile(x, probs = c(1/3, 2/3)))

species_breaks

species_bodymass_cat <- function(body_mass, species, break_list, labels = c("small", "medium", "large")) {
  
  if(length(labels) != (length(break_list[[1]]) + 1)) {
    stop("Number of labels must equal breakpoints + 1")
  }
  
  result <- vector("character", length(body_mass))
  
  for(sp in names(break_list)) {
    idx <- which(species == sp)
    brks <- break_list[[sp]]
    result[idx] <- as.character(cut(body_mass[idx], breaks = c(-Inf, brks, Inf), labels = labels))
  }
  
  return(result)
}
penguins$body_mass_cat_species <- species_bodymass_cat(
  body_mass = penguins$body_mass_g,
  species = penguins$species,
  break_list = species_breaks,
  labels = c("small", "medium", "large")
)

# Check results
table(penguins$species, penguins$body_mass_cat_species)


