library(here)
library(rnoaa)
library(tidyverse)
library(zoo)

# copy bottom temp gams from temp-manu --------------------------------------------------------

file.copy(here('../temp-manu/data/OTB_66_tempbot.RData'), here('data/'), overwrite = TRUE)

# copy mixmodprds from temp-manu --------------------------------------------------------------

file.copy(here('../temp-manu/data/mixmodprds.RData'), here('data/'), overwrite = TRUE)

# copy mix mods from temp-manu ----------------------------------------------------------------

file.copy(here('../temp-manu/data/mixmods.RData'), here('data/'), overwrite = TRUE)




noaa_key <- Sys.getenv('NOAA_KEY')

yrs <- seq(1930, 2022)

stameta <- ghcnd_stations()
sta <- 'USW00012842'
stalat <- stameta %>%
  filter(id %in% sta) %>%
  pull(latitude) %>%
  unique()

res <- yrs %>%
  tibble::enframe('name', 'year') %>%
  dplyr::group_by(name) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    ests = purrr::map(data, function(x){

      yr <- x$year
      cat(yr, '\n')

      start <- paste0(yr, "-01-01")
      end <- paste0(yr, "-12-31")

      # download NOAA UWS rainfall and temperature station data
      tia_dat <- meteo_pull_monitors(monitors = sta,
                                     date_min = start, date_max = end)

      out <- tia_dat

      return(out)

    })
  )

# tavg does not start until 1998, so reproduce from tmin/tmax
tiadat <- res %>%
  unnest('data') %>%
  unnest('ests') %>%
  ungroup() %>%
  select(date, tmax, tmin) %>%
  mutate(
    yr = year(date),
    mo = month(date),
    tavg_c = (tmax + tmin) / 2,
    tavg_c = tavg_c / 10,
    tavg_c = na.approx(tavg_c)
  ) %>%
  select(date, yr, mo, tavg_c)

save(tiadat, file = here('data/tiadat.RData'))
