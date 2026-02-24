library(openrouteservice)

eurotrip <- list()

for (city_name in names(landmarks)) {
  cat("\n=== ", city_name, " ===\n")
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
      
      tryCatch({
        
        r <- ors_directions(
          coordinates = list(start_coord, end_coord),
          profile     = "foot-walking",
          preference  = "shortest",
          radiuses    = c(500, 500)
        )
        
        cat("[OK] ")
        
        feat1 <- r$features[[1]]
        props <- feat1$properties
        cat("properties keys:", paste(names(props), collapse = ", "), " | ")
        
        summ <- NULL
        seg <- NULL
        
        # Summary directly under properties (your case)
        if (!is.null(props$summary) && !is.null(props$summary$distance)) {
          summ <- props$summary
          cat("summary at properties level | ")
        }
        
        # Fallback: under segments
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
        
        dist_m <- summ$distance
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
      })
      
      Sys.sleep(1.3)
    }
  }
  
  eurotrip <- c(eurotrip, routes_city)
  cat(" → Collected", length(routes_city), "usable routes for", city_name, "\n\n")
}

cat("Total routes:", length(eurotrip), "\n")

save(eurotrip, landmarks, file = "eurotrip.rdata")
cat("Saved to eurotrip.rdata\n")