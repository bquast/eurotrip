eurotrip <- list()  # collect all routes here

for (city_name in names(landmarks)) {
  
  cat("Processing", city_name, "...\n")
  points <- landmarks[[city_name]]
  n <- length(points)
  
  routes_city <- list()
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      
      start_coord <- points[[i]]$coord
      end_coord   <- points[[j]]$coord
      start_name  <- points[[i]]$name
      end_name    <- points[[j]]$name
      
      cat(sprintf("  %s → %s ... ", start_name, end_name))
      
      r <- NULL
      
      tryCatch({
        
        r <- ors_directions(
          coordinates = list(start_coord, end_coord),
          profile     = "foot-walking",
          preference  = "shortest",
          radiuses    = c(500, 500)
        )
        
        cat("[OK] ")
        
        # Debug: show top-level properties keys
        feat1 <- r$features[[1]]
        props <- feat1$properties
        cat("properties keys:", paste(names(props), collapse = ", "), " | ")
        
        # Find summary - try both common locations
        summ <- NULL
        seg  <- NULL
        
        # Case 1: summary directly under properties (sometimes seen)
        if (!is.null(props$summary) && !is.null(props$summary$distance)) {
          summ <- props$summary
          cat("summary at properties level | ")
        }
        
        # Case 2: summary under segments[[1]] (most common in GeoJSON)
        if (is.null(summ) && !is.null(props$segments) && length(props$segments) > 0) {
          seg <- props$segments[[1]]
          if (!is.null(seg$summary) && !is.null(seg$summary$distance)) {
            summ <- seg$summary
            cat("summary under segments[[1]] | ")
          }
        }
        
        if (is.null(summ) || is.null(summ$distance)) {
          cat("→ summary or distance missing → skipping\n")
          next
        }
        
        dist_m  <- summ$distance
        dur_min <- round(summ$duration / 60, 1)
        
        cat(sprintf("distance: %.0f m | ", dist_m))
        
        if (dist_m >= 300 && dist_m <= 2000) {
          
          route_item <- list(
            city            = city_name,
            route_id        = paste0(city_name, "_", i, "_to_", j),
            start_name      = start_name,
            end_name        = end_name,
            start_coord     = start_coord,
            end_coord       = end_coord,
            distance_m      = dist_m,
            duration_min    = dur_min,
            steps           = if (!is.null(seg) && !is.null(seg$steps)) seg$steps else list(),
            geometry_coords = feat1$geometry$coordinates
          )
          
          routes_city <- c(routes_city, list(route_item))
          cat(sprintf("SAVED (%.0f m, %.1f min)\n", dist_m, dur_min))
          
        } else {
          cat(sprintf("skipped (%.0f m)\n", dist_m))
        }
        
      }, error = function(e) {
        cat("→ ERROR:", conditionMessage(e), "\n")
        if (!is.null(r)) {
          cat("Partial response str:\n")
          str(r, max.level = 1)
        }
      })
      
      Sys.sleep(1.2)  # polite to API
      
    }  # end j
  }  # end i
  
  eurotrip <- c(eurotrip, routes_city)
  cat("  → Collected", length(routes_city), "usable routes for", city_name, "\n\n")
  
}  # end city

cat("Total routes collected:", length(eurotrip), "\n")

# Save as .RData (can load with load("eurotrip_named_landmarks.RData"))
save(eurotrip, landmarks, file = "eurotrip_named_landmarks.RData")
cat("Saved to eurotrip_named_landmarks.RData\n")

# Optional: quick look at first route if any exist
if (length(eurotrip) > 0) {
  first <- eurotrip[[1]]
  cat("\nExample route:\n")
  cat("From:", first$start_name, "to", first$end_name, "\n")
  cat("Distance:", first$distance_m, "m | Duration:", first$duration_min, "min\n")
}