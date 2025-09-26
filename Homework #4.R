#Objective 1
#a) Function to convert a continuous variable into binary
to_binary <- function(x, breakpoint, 
                      labels = c("low", "high")) {
  ifelse(x <= breakpoint, labels[1], labels[2])
}

#Important dataframe, create df variable
df <- palmerpenguins::penguins

#Assign median
bp <- median(df$body_mass_g, na.rm = TRUE)

#Create the size_class column
df$size_class <- to_binary(df$body_mass_g, breakpoint = bp, labels = c("small", "large"))

#Double checking
print(table(df$size_class, useNA = "ifany"))
print(head(df[, c("species", "body_mass_g", "size_class")], 10))

#Objective 2
#a)Generalize to N categories using breakpoints

to_category <- function(x, breakpoints, labels = NULL, right = TRUE, include_lowest = TRUE) {
  b <- sort(unique(breakpoints))
  cut(x, breaks = c(-Inf, b, Inf), labels = labels, right = right, include_lowest = include_lowest)
}

#b) Apply to body mass to get 'small', 'medium', 'large'

df <- palmerpenguins::penguins
bp3 <- c(3800, 4500)
df$size_class_3 <- to_category(
  df$body_mass_g,
  breakpoints = bp3,
  labels = c("small", "medium", "large")
)

#Double checking
print(table(df$size_class_3, useNA = "ifany"))
print(with(df, table(species, size_class_3, useNA = "ifany")))
head(df[, c("species", "body_mass_g", "size_class_3")], 10)

#Objective 3 — species-specific categorization

#Data and species-specific breakpoints (tertiles)
df <- palmerpenguins::penguins
bps_by_species <- tapply(df$body_mass_g, df$species,
                         quantile, probs = c(1/3, 2/3), na.rm = TRUE)

#Function: bin numeric x by species-specific breakpoints
to_category_by_group <- function(x, group, bps, labels) {
  out <- rep(NA_character_, length(x))
  for (sp in names(bps)) {
    sel <- group == sp & !is.na(x)
    out[sel] <- as.character(cut(x[sel],
                                 breaks = c(-Inf, sort(unique(bps[[sp]])), Inf),
                                 labels = labels,
                                 include.lowest = TRUE))
  }
  factor(out, levels = labels)
}

#Apply: 'small' / 'medium' / 'large'
df$size_class_sp <- to_category_by_group(df$body_mass_g, df$species,
                                         bps_by_species,
                                         labels = c("small", "medium", "large"))

# Objective 4 — boxplot by species and size category

library(ggplot2)

# Function: boxplot of body mass grouped by size class, faceted by species
plot_mass_by_species_size <- function(d) {
  ggplot(d, aes(x = size_class_sp, y = body_mass_g, fill = size_class_sp)) +
    geom_boxplot() +
    facet_wrap(~ species, scales = "free_y") +
    labs(x = "Size class", y = "Body mass (g)") +
    theme_minimal()
}

# Draw plot
plot_mass_by_species_size(df)