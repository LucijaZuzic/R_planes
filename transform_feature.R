transform_feat <- function(feature_use) {
  # Postavljanje imena značajke i mjerne jedinice koja se koristi

  original_name <- gsub(
    "METAR_", "",
    gsub(
      "Traj", "",
      gsub("_all", "", feature_use)
    )
  )
  new_name <- original_name

  units_use <- ""

  if (new_name == "Distance") {
    new_name <- "Difuzijska udaljenost trajektorije"
    units_use <- "m"
  }

  if (new_name == "Length") {
    new_name <- "Duljina trajektorije"
    units_use <- "m"
  }

  if (new_name == "Straightness") {
    new_name <- "Indeks pravocrtnosti"
  }

  if (new_name == "Sinuosity2") {
    new_name <- "Indeks zakrivljenosti"
  }

  if (new_name == "FractalDimension") {
    new_name <- "Fraktalna dimenzija"
  }

  if (new_name == "Emax") {
    new_name <- "Maksimalni očekivani pomak trajektorije"
  }

  if (new_name == "Duration") {
    new_name <- "Trajanje"
    units_use <- "s"
  }

  if (new_name == "Speed") {
    new_name <- "Brzina"
    units_use <- "m/s"
  }

  if (new_name == "Ff") {
    new_name <- "Brzina vjetra"
    units_use <- "m/s"
  }

  if (new_name == "Acceleration") {
    new_name <- "Akceleracija"
  }

  if (new_name == "DC") {
    new_name <- "Promjena smjera (prosjek)"
    units_use <- "°/s"
  }

  if (new_name == "Sinuosity2") {
    new_name <- "Promjena smjera (standardna devijacija)"
    units_use <- "°/s"
  }

  if (new_name == "SDDC") {
    new_name <- "Promjena smjera (standardna devijacija)"
    units_use <- "°/s"
  }

  if (new_name == "U") {
    new_name <- "Relativni udio vlage u zraku"
    units_use <- "%"
  }

  if (new_name == "T") {
    new_name <- "Temperatura"
    units_use <- "°"
  }

  if (new_name == "Td") {
    new_name <- "Temperatura kondenzacije"
    units_use <- "°"
  }

  if (new_name == "P0") {
    new_name <- "Tlak zraka na razini mora"
    units_use <- "mmHg"
  }

  if (new_name == "P") {
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
