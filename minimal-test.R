# Minimal test — one single route
r <- ors_directions(
  coordinates = list(c(8.5436, 47.3697), c(8.5405, 47.3712)),  # Grossmünster → Lindenhof
  profile     = "foot-walking",
  preference  = "shortest"
)

# Check what we actually got
print(length(r$features[[1]]$properties$segments[[1]]$steps))

# If > 0 → print instructions
steps <- r$features[[1]]$properties$segments[[1]]$steps
for (s in steps) {
  cat(sprintf("%s (%.0f m)\n", s$instruction, s$distance))
}