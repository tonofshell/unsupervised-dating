# This file checks for the the necessary data files and retrieves them if necessary, as they are too large to commit to GitHub

if (!("full_ok_cupid.csv" %in% list.files(here::here("Data")))) {
  message("Data CSV missing...")
  if (!("profiles.csv.zip" %in% list.files(here::here("Data")))) {
    message("Data ZIP missing, downloading from GitHub...")
    download.file("https://github.com/rudeboybert/JSE_OkCupid/raw/master/profiles.csv.zip", here::here("Data", "profiles.csv.zip"))
  }
  message("Extracting CSV from ZIP file...")
  uz_status = unzip(here::here("Data", "profiles.csv.zip"), exdir = here::here("Data"))
  success = file.rename(here::here("Data", "profiles.csv"), here::here("Data", "full_ok_cupid.csv"))
  if (success) {
    message("full_ok_cupid.csv successfully extracted")
  }
}
