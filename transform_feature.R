transform_feat <- function(feature_use) {
  # Postavljanje imena značajke i mjerne jedinice koja se koristi

  original_name <- gsub(
    "metar_", "",
    gsub(
      "traj_", "", feature_use
    )
  )
  new_name <- original_name

  units_use <- ""

  if (new_name == "distance") {
    new_name <- "Diffusion distance"
    units_use <- "m"
  }

  if (new_name == "length") {
    new_name <- "Length"
    units_use <- "m"
  }

  if (new_name == "straightness") {
    new_name <- "Straightness"
  }

  if (new_name == "sinuosity2") {
    new_name <- "Sinuosity"
  }

  if (new_name == "fractal_dimension") {
    new_name <- "Fractal dimension"
  }

  if (new_name == "emax") {
    new_name <- "Maximum expected displacement"
  }

  if (new_name == "duration") {
    new_name <- "Duration"
    units_use <- "s"
  }

  if (new_name == "speed") {
    new_name <- "Speed"
    units_use <- "m/s"
  }

  if (new_name == "ff") {
    new_name <- "Wind speed"
    units_use <- "m/s"
  }

  if (new_name == "acceleration") {
    new_name <- "Acceleration"
  }

  if (new_name == "dc") {
    new_name <- "Direction change (arithemtic average)"
    units_use <- ""
  }

  if (new_name == "sddc") {
    new_name <- "Direction change (standard deviation)"
    units_use <- ""
  }

  if (new_name == "u") {
    new_name <- "Relative humidity"
    units_use <- "%"
  }

  if (new_name == "t") {
    new_name <- "Temperature"
    units_use <- "°C"
  }

  if (new_name == "td") {
    new_name <- "Dew point"
    units_use <- "°C"
  }

  if (new_name == "p") {
    new_name <- "Air pressure at sea level"
    units_use <- "mmHg"
  }

  if (new_name == "p0") {
    new_name <- "Air pressure at the measuring station"
    units_use <- "mmHg"
  }

  if (units_use != "") {
    units_use <- paste("(", units_use, ")", sep = "")
  }

  new_lab <- paste(new_name, units_use)

  if (new_name == "Acceleration") {
    new_lab <- expression(Akceleracija ~ (m / s^2))
  }

  return(new_lab)
}
