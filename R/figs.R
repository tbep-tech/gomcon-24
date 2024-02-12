library(tbeptools)
library(tidyverse)
library(here)

# chlorophyll trend ---------------------------------------------------------------------------

p <- show_thrplot(epcdata, bay_segment = 'MTB', yrrng = c(1975, 2023)) +
  theme(legend.position = 'none') +
  scale_x_continuous(breaks = seq(1975, 2023, 10))

png(here('figs/chlortrend.png'), width = 4.5, height = 3.5, units = 'in', res = 300)
print(p)
dev.off()

# sg coverage map -----------------------------------------------------------------------------

png(here('figs/seagrasscov.png'), width = 4.5, height = 3.5, units = 'in', res = 300)
show_seagrasscoverage(seagrass)
dev.off()
