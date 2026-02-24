# Full loop — paste and run after defining 'landmarks' and setting your API key

eurotrip <- list()

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
      
      r <- NULL  # reset
      
      tryCatch({
        
        r <- ors_directions(
          coordinates = list(start_coord, end_coord),
          profile     = "foot-walking",
          preference  = "shortest",
          radiuses    = c(500, 500)
        )
        
        # Debug: show what we actually got
        cat(" [response class:", class(r)[1], "]")
        
        if (is.na(r) || !is.list(r)) {
          cat(" → response is NA or not a list\n")
          next
        }
        
        features <- r$features
        if (is.na(features) || is.null(features)) {
          cat(" → features is NA or NULL\n")
          next
        }
        
        feat_len <- length(features)
        if (is.na(feat_len) || feat_len == 0) {
          cat(" → no features (length NA or 0)\n")
          next
        }
        
        feat1 <- features[[1]]
        if (is.na(feat1) || !is.list(feat1)) {
          cat(" → first feature is NA or invalid\n")
          next
        }
        
        props <- feat1$properties
        if (is.na(props) || is.null(props)) {
          cat(" → properties NA or missing\n")
          next
        }
        
        segments <- props$segments
        if (is.na(segments) || is.null(segments) || length(segments) == 0) {
          cat(" → no segments\n")
          next
        }
        
        seg     <- segments[[1]]
        summ    <- seg$summary
        
        dist_m  <- summ$distance
        dur_min <- round(summ$duration / 60, 1)
        
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
            steps           = seg$steps,
            geometry_coords = feat1$geometry$coordinates
          )
          
          routes_city <- c(routes_city, list(route_item))
          cat(sprintf("OK (%.0f m, %.1f min)\n", dist_m, dur_min))
          
        } else {
          cat(sprintf("skipped (%.0f m)\n", dist_m))
        }
        
      }, error = function(e) {
        cat(" → caught error:", conditionMessage(e), "\n")
        # Show partial object if it exists
        if (!is.null(r)) {
          cat("Partial response str:\n")
          str(r, max.level = 1)
        }
      })
      
      Sys.sleep(1.2)
      
    }
  }
  
  eurotrip <- c(eurotrip, routes_city)
  cat("  → Collected", length(routes_city), "usable routes for", city_name, "\n\n")
}

cat("Total routes collected:", length(eurotrip), "\n")

save(eurotrip, landmarks, file = "eurotrip_named_landmarks.RData")
cat("Saved to eurotrip_named_landmarks.RData\n")