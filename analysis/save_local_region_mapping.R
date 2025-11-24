library(mermaidr)
library(jsonlite)

# Get regions reference from MERMAID
choices_data <- mermaid_get_endpoint("choices")

# Get regions data
regions_data <- choices_data$data[choices_data$name == "regions"][[1]]

# Create a simple named list for lookup
regions_lookup <- setNames(as.list(regions_data$name), regions_data$id)

# Save as JSON
write_json(regions_lookup, "mermaid_regions_lookup.json", auto_unbox = TRUE, pretty = TRUE)

# Verify it has the structure we need
print(names(regions_lookup)[1:3])  # Should show UUIDs
print(regions_lookup[[1]])          # Should show a region name