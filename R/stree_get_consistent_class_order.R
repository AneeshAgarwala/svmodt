get_consistent_class_order <- function(y, sort_method = "natural") {
  classes <- unique(as.character(y))

  switch(sort_method,
         "natural" = sort(classes),  # Alphabetical/numerical sort
         "frequency" = {
           # Sort by frequency (most common first)
           freq_table <- table(y)
           names(sort(freq_table, decreasing = TRUE))
         },
         "original" = classes,  # Order of first appearance
         sort(classes)  # Default to natural sort
  )
}
