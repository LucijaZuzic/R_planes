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
    new_name <- "Difuzijska udaljenost"
    units_use <- "m"
  }

  if (new_name == "length") {
    new_name <- "Duljina"
    units_use <- "m"
  }

  if (new_name == "straightness") {
    new_name <- "Indeks pravocrtnosti"
  }

  if (new_name == "sinuosity2") {
    new_name <- "Indeks zakrivljenosti"
  }

  if (new_name == "fractal_dimension") {
    new_name <- "Fraktalna dimenzija"
  }

  if (new_name == "emax") {
    new_name <- "Maksimalni očekivani pomak"
  }

  if (new_name == "duration") {
    new_name <- "Trajanje"
    units_use <- "s"
  }

  if (new_name == "speed") {
    new_name <- "Brzina"
    units_use <- "m/s"
  }

  if (new_name == "ff") {
    new_name <- "Brzina vjetra"
    units_use <- "m/s"
  }

  if (new_name == "acceleration") {
    new_name <- "Akceleracija"
  }

  if (new_name == "dc") {
    new_name <- "Promjena smjera (prosjek)"
    units_use <- ""
  }

  if (new_name == "sddc") {
    new_name <- "Promjena smjera (standardna devijacija)"
    units_use <- ""
  }

  if (new_name == "u") {
    new_name <- "Relativni udio vlage u zraku"
    units_use <- "%"
  }

  if (new_name == "t") {
    new_name <- "Temperatura"
    units_use <- "°C"
  }

  if (new_name == "td") {
    new_name <- "Temperatura kondenzacije"
    units_use <- "°C"
  }

  if (new_name == "p") {
    new_name <- "Tlak zraka na razini mora"
    units_use <- "mmHg"
  }

  if (new_name == "p0") {
    new_name <- "Tlak zraka na nadmorskoj visini mjerne stanice"
    units_use <- "mmHg"
  }

  if (units_use != "") {
    units_use <- paste("(", units_use, ")", sep = "")
  }

  new_lab <- paste(new_name, units_use)

  if (new_name == "Akceleracija") {
    new_lab <- expression(Akceleracija ~ (m / s^2))
  }

  return(new_lab)
}
