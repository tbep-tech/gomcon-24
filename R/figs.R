library(tbeptools)
library(tidyverse)
library(here)

# chlorophyll trend ---------------------------------------------------------------------------

p <- show_thrplot(epcdata, bay_segment = 'MTB', yrrng = c(1975, 2023)) +
  theme_bw() +
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

png(here('figs/seagrasscov.png'), width = 4.5, height = 3.5, units = 'in', res = 300)
show_seagrasscoverage(seagrass)
dev.off()


# air temp ------------------------------------------------------------------------------------

load(file = url("https://github.com/tbep-tech/temp-manu/raw/main/data/speidat.RData"))

toplo <- speidat %>%
  summarise(
    temp = mean(tavg_c),
    .by = yr
  )
p <- ggplot(toplo, aes(x = yr, y = temp)) +
  geom_line(col = 'red', linewidth = 1) +
  geom_point(col = 'red', size = 2) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = 'none',
    panel.grid.minor = element_blank()
  ) +
  # scale_x_continuous(breaks = seq(1975, 2023, 10)) +
  labs(
    x = NULL,
    title = 'Mean annual air temp, Tampa International Airport',
    y = '\u00B0C'
    )

png(here('figs/airtemp.png'), width = 10.5, height = 4.5, units = 'in', res = 300)
print(p)
dev.off()
