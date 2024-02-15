library(tbeptools)
library(tidyverse)
library(here)
library(sf)
library(maps)
library(ggspatial)
library(patchwork)
library(EnvStats)
library(haven)
library(wqtrends)

# segment labels
seglabs <- data.frame(
    Longitude = c(-82.6, -82.64, -82.58, -82.42),
    Latitude = c(27.55, 27.81, 28, 27.925),
    bay_segment = c('LTB', 'MTB', 'OTB', 'HB')
  ) %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)

# chlorophyll trend ---------------------------------------------------------------------------

p <- show_thrplot(epcdata, bay_segment = 'MTB', yrrng = c(1975, 2023)) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    panel.grid.minor = element_blank()
    ) +
  scale_x_continuous(breaks = seq(1975, 2023, 10)) +
  labs(x = NULL)

png(here('figs/chlortrend.png'), width = 4.5, height = 3.5, units = 'in', res = 300)
print(p)
dev.off()

# sg coverage map -----------------------------------------------------------------------------

png(here('figs/seagrasscov.png'), width = 5, height = 3.75, units = 'in', res = 300)
show_seagrasscoverage(seagrass)
dev.off()

# air temp ------------------------------------------------------------------------------------

load(file = here('data/tiadat.RData'))

toplo <- tiadat %>%
  summarise(
    temp = mean(tavg_c),
    .by = yr
  ) %>%
  filter(yr >= 1940)
p <- ggplot(toplo, aes(x = yr, y = temp)) +
  geom_line(col = 'red', linewidth = 1) +
  geom_point(col = 'red', size = 2) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.background = element_rect(fill = 'transparent', color = NA),
    axis.text = element_text(size = 18)
  ) +
  # scale_x_continuous(breaks = seq(1975, 2023, 10)) +
  labs(
    x = NULL,
    title = 'Mean annual air temp, Tampa International Airport',
    y = '\u00B0C'
    )

ggsave(p, filename = here('figs/airtemp.png'), width = 10.5, height = 4.5, units = 'in', dpi = 200, bg= 'transparent')

# EPC map -------------------------------------------------------------------------------------

flpoly <- map_data('state', 'florida') %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

thm <- theme(
  panel.grid = element_blank(),
  axis.title = element_blank(),
  axis.text.y = element_text(size = 6),
  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6),
  axis.ticks = element_blank(),
  plot.background = element_rect(colour = NA, fill = 'transparent'),
  panel.background = element_rect(fill = 'transparent')
)

bbox <- st_bbox(tbseg)

minset <- ggplot() +
  geom_sf(data = flpoly, fill = 'grey', color = NA) +
  geom_sf(data = st_as_sfc(bbox), fill = NA, color = 'black', linewidth = 0.5) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = '#FFFFFF', colour = 'white'),
    panel.border = element_rect(colour = 'black', fill = 'transparent')
  )

epcpts <- epcdata %>%
  select(long = Longitude, lat = Latitude) %>%
  unique() %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

m <- ggplot() +
  ggspatial::annotation_map_tile(zoom = 11, type = 'cartolight', cachedir = system.file("rosm.cache", package = "ggspatial")) +
  geom_sf(data = tbseg, fill = 'grey', alpha = 0.5, inherit.aes = F, linewidth = 1) +
  # geom_sf(data = sgdat, fill = 'darkgreen', color = NA, inherit.aes = F) +
  geom_sf(data = tbseg, fill = NA, color = NA, inherit.aes = F) +
  geom_sf(data = epcpts, color = 'black', inherit.aes = F) +
  geom_label(data = seglabs, aes(label = bay_segment, geometry = geometry), stat = "sf_coordinates", inherit.aes = F, fill = rgb(1, 1, 1, 0.5), size = 6) +
  coord_sf(xlim = bbox[c('xmin', 'xmax')], ylim = bbox[c('ymin', 'ymax')], crs = 4326) +
  annotation_north_arrow(location = 'tl', style = north_arrow_orienteering(fill = c('black', 'black'), text_col = NA),
                         height = unit(0.5, "cm"), width = unit(0.5, "cm")) +
  annotation_scale(location = 'br', text_cex = 1) +
  annotation_custom(ggplotGrob(minset), xmin = bbox[3] - 0.1, xmax = bbox[3] + 0.015, ymin = bbox[4] - 0.1, ymax = bbox[4] + 0.06) +
  coord_sf(xlim = bbox[c('xmin', 'xmax')], ylim = bbox[c('ymin', 'ymax')], crs = 4326) +
  thm

ggsave(here('figs/epcmap.png'), m, width = 4, height = 6, units = 'in', dpi = 200, bg = 'transparent')

# EPC trend -----------------------------------------------------------------------------------

toplo <- epcdata %>%
  select(bay_segment, epchc_station, SampleTime, yr, matches('^Temp.*Top|^Temp.*Bottom')) %>%
  filter(yr >= 1975 & yr <= 2023) %>%
  pivot_longer(names_to = 'var', values_to = 'val', matches('Top|Bottom')) %>%
  mutate(
    var = factor(var,
                 levels = c(c("Temp_Water_Top_degC", "Temp_Water_Bottom_degC"
                 )),
                 labels = c("Temp_Top", "Temp_Bottom")
    ),
    bay_segment = factor(bay_segment, levels = c('OTB', 'HB', 'MTB', 'LTB'))
  ) %>%
  separate(var, c('var', 'loc')) %>%
  mutate(
    var = factor(var, levels = c('Temp'), labels = c('Water temp. (\u00B0C)')),
    loc = factor(loc, levels = c('Top', 'Bottom'))
  ) %>%
  filter(!is.na(val)) %>%
  summarise(
    avev = mean(val, na.rm = T),
    se = sd(val, na.rm = T)^2,
    lov = t.test(val, na.rm = T)$conf.int[1],
    hiv = t.test(val, na.rm = T)$conf.int[2],
    .by = c('bay_segment', 'yr', 'var', 'loc')
  ) %>%
  filter(!(yr %in% c(1982, 1985) & bay_segment == 'OTB')) %>%  # missing months create outliers
  filter(!(yr %in% 1975 & bay_segment == 'HB')) # missing months create outliers

wd <- 0.5

thm <- theme_minimal() +
  theme(
    strip.placement = 'outside',
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size= 12),
    legend.position = 'top'
  )

p <- ggplot(toplo, aes(x = yr, y = avev, group = loc, color = loc)) +
  geom_linerange(aes(ymin = lov, ymax = hiv), position = position_dodge2(width = wd), show.legend = F, alpha = 0.7) +
  geom_point(position = position_dodge2(width = wd), size = 0.5) +
  # scale_x_continuous(breaks = seq(min(toplo$yr), max(toplo$yr), by = 3)) +
  # geom_line(data = mixmets %>% filter(var == 'Water temp. (\u00B0C)'), aes(y = prd)) +
  geom_smooth(method = 'lm', se = F, formula = y ~ x) +
  facet_grid(~ bay_segment) +
  scale_color_manual(values = c( 'red2', 'red4')) +
  thm +
  theme(
    strip.text = element_text(size = 12)
  ) +
  labs(
    x = NULL,
    y = '\u00B0C',
    color = 'Water temp.',
    shape = NULL
  )

png(here('figs/epctrend.png'), width = 9, height = 3.5, units = 'in', res = 300)
print(p)
dev.off()

# EPC trend kendall ---------------------------------------------------------------------------

# kendall all years
sktres <- epcdata %>%
  filter(yr >= 1975 & yr <= 2023) %>%
  select(bay_segment, station = epchc_station, SampleTime, lon = Longitude, lat = Latitude, yr,
         mo, matches('Temp.*Bottom')) %>%
  pivot_longer(matches('Bottom'), names_to = 'var', values_to = 'val') %>%
  nest(.by = c('bay_segment', 'var', 'station', 'lon', 'lat')) %>%
  mutate(
    skt = purrr::pmap(list(station, var, data), function(station, var, data){

      cat(station, var, '\n')

      # yr selection
      tomod <- data %>%
        arrange(yr, mo) %>%
        na.omit()

      # run tests
      ests <- kendallSeasonalTrendTest(val ~ mo + yr, data = tomod)

      out <- tibble(
        pval = ests$p.value[2][[1]],
        slos = ests$estimate[2][[1]],
        n = nrow(tomod)
      )

      return(out)

    })
  ) %>%
  select(-data) %>%
  unnest(skt)

#kendall by month
ktres <- epcdata %>%
  filter(yr >= 1975 & yr <= 2023) %>%
  select(bay_segment, station = epchc_station, SampleTime, lon = Longitude, lat = Latitude, yr,
         mo, matches('Temp.*Bottom')) %>%
  pivot_longer(matches('Bottom'), names_to = 'var', values_to = 'val') %>%
  nest(.by = c('bay_segment', 'var', 'station', 'lon', 'lat', 'mo')) %>%
  mutate(
    kt = purrr::pmap(list(station, var, mo, data), function(station, var, mo, data){

      cat(station, var, mo, '\n')

      # yr selection
      tomod <- data %>%
        arrange(yr) %>%
        na.omit()

      out <- tibble(
        pval = NA,
        slos = NA,
        n = NA
      )

      # run tests
      ests <- try(kendallTrendTest(val ~ yr, data = tomod), silent = T)
      if(inherits(ests, 'try-error'))
        return(out)

      out$pval <- ests$p.value[1]
      out$slos <- ests$estimate[2]
      out$n <- nrow(tomod)

      return(out)

    })
  ) %>%
  select(-data) %>%
  unnest(kt)

# plots of segment summaries by month
ktresplo <- ktres %>%
  mutate(
    mo = month(mo, label = T),
    bay_segment = factor(bay_segment, levels = c('LTB', 'MTB', 'HB', 'OTB')),
    varsimp = gsub('^(.*?)_.*$', '\\1', var, perl = T),
    varsimp = factor(varsimp, levels = c('Temp'), labels = c('Temperature')),
  ) %>%
  nest(.by = c('bay_segment', 'mo', 'varsimp', 'var')) %>%
  mutate(
    sum = purrr::pmap(list(varsimp, data), function(varsimp, data){

      data %>%
        summarise(
          nsig = sum(pval < 0.05 & slos > 0),
          cnt = n(),
          nsigper = round(100 * nsig / cnt, 0),
          perlab = ifelse(nsigper == 0, '', as.character(abs(nsigper)))
        )

    })
  ) %>%
  select(-data) %>%
  unnest('sum')

p2 <- ggplot(ktresplo, aes(x = mo, y = bay_segment, fill = nsigper)) +
  geom_tile(color = 'darkgrey') +
  geom_text(aes(label = perlab), color = 'white', fontface = 'bold', size = 6) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradientn(limits = c(-100, 100), colors = c('blue', 'grey', 'tomato1')) +
  theme_bw(base_family = 'serif') +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12),
    legend.position = 'none',
    axis.text = element_text(size = 10),
    panel.background = element_rect(fill = 'transparent'),
    plot.background = element_rect(color = NA, fill = 'transparent')
  ) +
  labs(
    x = NULL,
    y = 'Bay segment',
    title = '(b) % stations with significant trends by month'
  )

leglab <- expression(paste('\u00B0C ', yr^{-1}))
colrng <- range(sktres$slos) %>%
  abs %>%
  max
colrng <- c(-1 * colrng, colrng)

toplo <- sktres %>%
  mutate(
    pvalcol = ifelse(pval < 0.05, T, F),
    coefsgn = sign(slos),
    coefsgn = factor(coefsgn, levels = c('1', '-1'), labels = c('inc', 'dec')),
    var = gsub('^(.*?)_.*$', '\\1', var, perl = T),
    var = factor(var, levels = c('Temp'), labels = c('Temperature')),
  ) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

pthm <- theme_bw(base_family = 'serif') +
  theme(
    legend.position = c(0.85, 0.15),
    # legend.box = 'horizontal',
    strip.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    legend.text = element_text(size = 8),
    legend.background = element_rect(fill = NA),
    panel.background = element_rect(fill = 'transparent'),
    plot.background = element_rect(color = NA, fill = 'transparent')
  )

p1 <- ggplot() +
  annotation_map_tile('cartolight', zoom = 11) +
  geom_sf(data = tbseg, fill = 'grey', alpha = 0.5, inherit.aes = F, linewidth = 0.5) +
  geom_sf(data = toplo, aes(size = abs(slos), fill = slos, shape = coefsgn, color = pvalcol), stroke = 1) +
  # scale_fill_gradient2(leglab, low = 'blue', mid = 'grey',  high = 'tomato1', midpoint = 0) +
  geom_label(data = seglabs, aes(label = bay_segment, geometry = geometry), stat = "sf_coordinates", inherit.aes = F, fill = rgb(1, 1, 1, 0.5), size = 4) +
  scale_fill_gradientn(leglab, limits = colrng, colors = c('blue', 'grey', 'tomato1')) +
  scale_color_manual(values = c('black', scales::alpha('black', 0)), guide = 'none', drop = FALSE) +
  # coord_map() +
  scale_x_continuous(breaks = seq(-82.7, -82.4, length = 4)) +
  scale_shape_manual(values = c(24, 25), drop = FALSE, guide = 'none') +
  pthm +
  scale_size(range = c(0.75, 4), guide = 'none') +
  guides(fill = guide_colourbar(barwidth = 0.4, barheight = 2.5)) +
  labs(
    title = paste0('(a) Change per year, 1975 - 2023'),
    caption = 'Outlines indicate p < 0.05'
  )

p <- p1 + p2 + plot_layout(ncol = 2, width = c(0.35, 1)) &
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    plot.background = element_rect(fill = 'transparent', color = NA)
  )

ggsave(here('figs/epckendall.png'), p, width = 11, height = 4.5, units = 'in', dpi = 200, bg = 'transparent')

# fim map -------------------------------------------------------------------------------------

fl <- paste0(tempdir(), '/fimsgtempdat.RData')
download.file('https://github.com/tbep-tech/temp-manu/raw/main/data/fimsgtempdat.RData', destfile = fl)
load(file = fl)

flpoly <- map_data('state', 'florida') %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

thm <- theme(
  panel.grid = element_blank(),
  axis.title = element_blank(),
  axis.text.y = element_text(size = 6),
  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6),
  axis.ticks = element_blank(),
  plot.background = element_rect(colour = NA, fill = 'transparent'),
  panel.background = element_rect(fill = 'transparent')
)

bbox <- st_bbox(tbseg)

minset <- ggplot() +
  geom_sf(data = flpoly, fill = 'grey', color = NA) +
  geom_sf(data = st_as_sfc(bbox), fill = NA, color = 'black', linewidth = 0.5) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = '#FFFFFF', colour = 'white'),
    panel.border = element_rect(colour = 'black', fill = 'transparent')
  )

fimpts <- fimsgtempdat %>%
  select(long = lon, lat) %>%
  unique() %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

m <- ggplot() +
  ggspatial::annotation_map_tile(zoom = 11, type = 'cartolight', cachedir = system.file("rosm.cache", package = "ggspatial")) +
  geom_sf(data = tbseg, fill = 'grey', alpha = 0.5, inherit.aes = F, linewidth = 1) +
  # geom_sf(data = sgdat, fill = 'darkgreen', color = NA, inherit.aes = F) +
  geom_sf(data = tbseg, fill = NA, color = NA, inherit.aes = F) +
  geom_sf(data = fimpts, color = 'black', inherit.aes = F) +
  geom_label(data = seglabs, aes(label = bay_segment, geometry = geometry), stat = "sf_coordinates", inherit.aes = F, fill = rgb(1, 1, 1, 0.5), size = 6) +
  coord_sf(xlim = bbox[c('xmin', 'xmax')], ylim = bbox[c('ymin', 'ymax')], crs = 4326) +
  annotation_north_arrow(location = 'tl', style = north_arrow_orienteering(fill = c('black', 'black'), text_col = NA),
                         height = unit(0.5, "cm"), width = unit(0.5, "cm")) +
  annotation_scale(location = 'br', text_cex = 1) +
  annotation_custom(ggplotGrob(minset), xmin = bbox[3] - 0.1, xmax = bbox[3] + 0.015, ymin = bbox[4] - 0.1, ymax = bbox[4] + 0.06) +
  coord_sf(xlim = bbox[c('xmin', 'xmax')], ylim = bbox[c('ymin', 'ymax')], crs = 4326) +
  thm

ggsave(here('figs/fimmap.png'), m, width = 4, height = 6, units = 'in', dpi = 200, bg = 'transparent')

# fim trend -----------------------------------------------------------------------------------

# import original SAS data
phyraw <- read_sas('https://github.com/tbep-tech/fim-macroalgae/raw/main/data/raw/tbm_physical.sas7bdat')
hydraw <- read_sas('https://github.com/tbep-tech/fim-macroalgae/raw/main/data/raw/tbm_hydrolab.sas7bdat')

phydat <- phyraw %>%
  mutate(
    date = ymd(date),
    starttime = as.character(starttime),
    hr = gsub('^([[:digit:]]+):.*$', '\\1', starttime),
    hr = as.numeric(hr)
  ) %>%
  filter(Zone %in% c('A', 'B', 'C', 'D', 'E')) %>%
  #filter(Project =='AM')
  filter(Gear == 20) %>%
  #filter (Stratum %in% c('A','B')) %>%
  filter(!is.na(Longitude) | !is.na(Latitude)) %>%
  filter(!is.na(BottomVegCover)) %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>%
  .[tbseg, ] %>%
  select(Reference, date, hr, BottomVegCover)

# sal, temp data (no location)
hyddat <- hydraw %>%
  filter(Beg_end %in% 'B') %>% # each location has a beginning and end log, take one
  filter(Depth %in% range(Depth), .by = 'Reference') %>% # some have depth profile, take bottom
  select(Reference, depth = Depth, temp = Temperature) %>%
  mutate(
    cnt = n(),
    .by = 'Reference'
  )

# combine sal, temp data with location data, specific to tb
fimtempdat <- phydat %>%
  inner_join(hyddat, by = 'Reference') %>%
  filter(year(date) > 1995) %>% # only spring/fall sampling prior to 1996
  st_intersection(tbseg[, 'bay_segment']) %>%
  st_set_geometry(NULL) %>%
  arrange(Reference) %>%
  filter(cnt == 2) %>% # get those with two depths
  mutate(
    loc = ifelse(depth == min(depth), 'Top', 'Bottom'),
    .by = Reference
  )

toplo <- fimtempdat %>%
  select(date, temp, loc, bay_segment) %>%
  mutate(
    yr = year(date),
    mo = month(date)
  ) %>%
  mutate(
    mocnt = length(unique(mo)),
    .by = c(yr, bay_segment, loc)
  ) %>%
  filter(mocnt > 11) %>%
  summarise(
    avev = mean(temp, na.rm = T),
    lov = tryCatch(t.test(temp, na.rm = T)$conf.int[1], error = function(err) NA),
    hiv = tryCatch(t.test(temp, na.rm = T)$conf.int[2], error = function(err) NA),
    .by = c(bay_segment, yr, loc)
  ) %>%
  mutate(
    avev = ifelse(is.na(lov), NA, avev),
    bay_segment = factor(bay_segment, levels = c('OTB', 'HB', 'MTB', 'LTB')),
    name = 'Water temp. (\u00B0C)'
  )

thm <- theme_minimal() +
  theme(
    strip.placement = 'outside',
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size= 12),
    legend.position = 'top'
  )

wd <- 0.5

p <- ggplot(toplo, aes(x = yr, y = avev, group = loc, color = loc)) +
  geom_linerange(aes(ymin = lov, ymax = hiv), position = position_dodge2(width = wd), show.legend = F, alpha = 0.7) +
  geom_point(position = position_dodge2(width = wd), size = 0.5) +
  # scale_x_continuous(breaks = seq(min(toplo$yr), max(toplo$yr), by = 3)) +
  # geom_line(data = mixmets %>% filter(var == 'Water temp. (\u00B0C)'), aes(y = prd)) +
  geom_smooth(method = 'lm', se = F, formula = y ~ x) +
  facet_grid(~ bay_segment) +
  scale_color_manual(values = c( 'red2', 'red4')) +
  thm +
  labs(
    x = NULL,
    y = '\u00B0C',
    color = 'Water temp.',
    shape = NULL
  )

png(here('figs/fimtrend.png'), width = 9, height = 3.5, units = 'in', res = 300)
print(p)
dev.off()

# log map -------------------------------------------------------------------------------------

load(file = url('https://github.com/tbep-tech/otb-temp/raw/main/data/metadat.RData'))

flpoly <- map_data('state', 'florida') %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

thm <- theme(
  panel.grid = element_blank(),
  axis.title = element_blank(),
  axis.text.y = element_text(size = 6),
  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6),
  axis.ticks = element_blank(),
  plot.background = element_rect(colour = NA, fill = 'transparent'),
  panel.background = element_rect(fill = 'transparent'),
  legend.position = c(0.82, 0.12),
  legend.background = element_rect(fill = 'transparent', colour = NA)
)

bbox <- st_bbox(tbseg)

minset <- ggplot() +
  geom_sf(data = flpoly, fill = 'grey', color = NA) +
  geom_sf(data = st_as_sfc(bbox), fill = NA, color = 'black', linewidth = 0.5) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = '#FFFFFF', colour = 'white'),
    panel.border = element_rect(colour = 'black', fill = 'transparent')
  )

logpts <- metadat %>%
  filter(grepl('^2023', yr_site_logger)) %>%
  mutate(
    stratum = factor(stratum, levels = c('SEAGRASS', 'BARE'), labels = c('Seagrass', 'Bare'))
  )

m <- ggplot() +
  ggspatial::annotation_map_tile(zoom = 11, type = 'cartolight', cachedir = system.file("rosm.cache", package = "ggspatial")) +
  geom_sf(data = tbseg, fill = 'grey', alpha = 0.5, inherit.aes = F, linewidth = 1) +
  # geom_sf(data = sgdat, fill = 'darkgreen', color = NA, inherit.aes = F) +
  geom_sf(data = tbseg, fill = NA, color = NA, inherit.aes = F) +
  geom_sf(data = logpts, aes(color = stratum), inherit.aes = F) +
  geom_label(data = seglabs, aes(label = bay_segment, geometry = geometry), stat = "sf_coordinates", inherit.aes = F, fill = rgb(1, 1, 1, 0.5), size = 6) +
  scale_color_manual(values = c('Seagrass' = 'seagreen2', 'Bare' = 'black')) +
  coord_sf(xlim = bbox[c('xmin', 'xmax')], ylim = bbox[c('ymin', 'ymax')], crs = 4326) +
  annotation_north_arrow(location = 'tl', style = north_arrow_orienteering(fill = c('black', 'black'), text_col = NA),
                         height = unit(0.5, "cm"), width = unit(0.5, "cm")) +
  annotation_scale(location = 'br', text_cex = 1) +
  annotation_custom(ggplotGrob(minset), xmin = bbox[3] - 0.1, xmax = bbox[3] + 0.015, ymin = bbox[4] - 0.1, ymax = bbox[4] + 0.06) +
  coord_sf(xlim = bbox[c('xmin', 'xmax')], ylim = bbox[c('ymin', 'ymax')], crs = 4326) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  theme_minimal() +
  thm +
  labs(
    color = NULL
  )

ggsave(here('figs/logmap.png'), m, width = 4, height = 6, units = 'in', dpi = 200, bg = 'transparent')

# log trend -----------------------------------------------------------------------------------

load(file = url('https://github.com/tbep-tech/otb-temp/raw/main/data/metadat.RData'))
load(file = url('https://github.com/tbep-tech/otb-temp/raw/main/data/tempdat.RData'))

orgs <- metadat %>%
  st_set_geometry(NULL) %>%
  select(deploy_date, yr_site_logger) %>%
  nest(.by = deploy_date) %>%
  arrange(deploy_date) %>%
  deframe() %>%
  lapply(function(x) x$yr_site_logger)

metastrat <- metadat %>%
  st_set_geometry(NULL) %>%
  select(site, stratum) %>%
  mutate(
    stratum = factor(stratum, levels = c('SEAGRASS', 'BARE'), labels = c('Seagrass', 'Bare'))
  )

toplo <- tempdat %>%
  filter(yr_site_logger %in% orgs[['2023-08-09']]) %>%
  left_join(metastrat, by = 'site')

thm <- theme_minimal() +
  theme(
    strip.placement = 'outside',
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size= 12),
    legend.position = 'top'
  )

p <- ggplot(toplo, aes(x = datetime, y = tempc, color = stratum, group = site)) +
  geom_line() +
  scale_color_manual(values = c('Seagrass' = 'seagreen2', 'Bare' = 'black')) +
  thm +
  labs(
    color = NULL,
    x = NULL,
    y = 'Temperature (\u00B0C)'
  )

ggsave(here('figs/logtrend.png'), p, width = 9, height = 3.5, units = 'in', dpi = 200, bg = 'transparent')

# log trend comb ------------------------------------------------------------------------------

load(file = url('https://github.com/tbep-tech/otb-temp/raw/main/data/metadat.RData'))
load(file = url('https://github.com/tbep-tech/otb-temp/raw/main/data/tempdat.RData'))

metastrat <- metadat %>%
  filter(yr == 2023) %>%
  st_set_geometry(NULL) %>%
  select(site, stratum) %>%
  mutate(
    stratum = factor(stratum, levels = c('SEAGRASS', 'BARE'), labels = c('Seagrass', 'Bare'))
  )

toproc <- tempdat %>%
  filter(yr == 2023) %>%
  left_join(metastrat, by = 'site', relationship = 'many-to-one') %>%
  mutate(
    hr = hour(datetime)
  )

toplo <- toproc %>%
  summarise(
    tempc = median(tempc, na.rm = T),
    .by = c(hr, stratum)
  ) %>%
  mutate(stratum = factor(stratum, levels = c('Bare', 'Seagrass')))

p1 <- ggplot(toplo, aes(x = hr, y = stratum, fill = tempc)) +
  geom_tile(color = 'black') +
  scale_fill_gradient2(low = 'green', mid = 'yellow', high = 'red', na.value = 'white', midpoint = median(toplo$tempc)) +
  scale_x_continuous(expand = c(0, 0), breaks = unique(toplo$hr)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(
    strip.background = element_blank(),
    legend.direction = 'horizontal',
    legend.position = c(0.5, 1.12),
    plot.margin = margin(30, 5.5, 5.5, 5.5),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.background = element_rect(fill = 'transparent', color = NA)
  ) +
  guides(fill = guide_colorbar(barheight = 0.5)) +
  labs(
    x = "Hour",
    fill = "Median temp (\u00B0C)",
    y = NULL,
  )

toplo <- toproc %>%
  mutate(
    date = date(datetime),
    min = minute(datetime) / 60,
    hrmin = hr + min
  ) %>%
  unite('grp', date, site)

thm <- theme_minimal() +
  theme(
    strip.placement = 'outside',
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size= 12),
    legend.position = 'top',
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.background = element_rect(fill = 'transparent', color = NA)
  )

p2 <- ggplot(toplo, aes(x = hrmin, y = tempc)) +
  geom_line(aes(group = grp), linewidth = 0.25, color = 'grey') +
  facet_grid( ~ stratum) +
  scale_color_manual(values = c('seagreen2', 'black')) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_smooth(aes(color = stratum), se = F, show.legend = F) +
  thm +
  labs(
    x = "Hour",
    y = "Temp (\u00B0C)",
  )

p <- p1 + p2 + plot_layout(ncol = 2, widths = c(2, 2))

ggsave(here('figs/logtrendcomb.png'), p, width = 9, height = 3.5, units = 'in', dpi = 200, bg = 'transparent')

# gam ex --------------------------------------------------------------------------------------

fl <- 'OTB_66_tempbot'
load(file = here(paste0('data/', fl, '.RData')))

toplo <- get(fl)

p <- show_prdseries(toplo, ylab = 'Bottom temp. (\u00B0C)', col = 'red2') +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.minor = element_blank()
  ) +
  labs(x = NULL)

ggsave(here('figs/gamex.png'), p, width = 5.5, height = 2, units = 'in', dpi = 200, bg = 'transparent')

# mixeff --------------------------------------------------------------------------------------

load(file = here('data/mixmodprds.RData'))

toplo1 <- mixmodprds %>%
  select(-mod, -fix, -slo) %>%
  unnest('data') %>%
  filter(
    !(cnt > 100 & thrtyp == paste('Temperature >', tmpthr)) # outliers
  ) %>%
  filter(thrtyp == 'Temperature > 30')
toplo2 <- mixmodprds %>%
  select(-mod, -data, -slo) %>%
  unnest('fix') %>%
  filter(thrtyp == 'Temperature > 30')

p <- ggplot(toplo1, aes(x = yr, y = cnt)) +
  geom_point(size = 0.7, alpha = 0.6, color = 'darkgrey') +
  geom_line(aes(y = prd, group = station), color = 'darkgrey', linewidth = 0.5) +
  geom_line(data = toplo2, aes(y = prd), col = 'red2', show.legend = F, linewidth = 1.5) +
  # coord_cartesian(ylim = c(-10, NA)) +
  facet_grid( ~ bay_segment, scales = 'free_y') +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    strip.text = element_text(size = 10),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.background = element_rect(fill = 'transparent', color = NA)
  ) +
  labs(
    x = NULL,
    y = expression(paste("Days ", Year^-1, " Temp > 30\u00B0C")),
  )

ggsave(here('figs/mixeff.png'), p, width = 8, height = 2.5, units = 'in', dpi = 200, bg = 'transparent')


