library(tidyverse)
library(here)

source(here('R/funcs.R'))

# mixeff model summaries ----------------------------------------------------------------------

load(file = here('data/mixmods.RData'))

totab <- mixmods %>%
  filter(tempthr == 'temp_30') %>%
  filter(thrtyp == 'tempcnt') %>%
  select(-salithr, -tempthr, -thrtyp) %>%
  distinct() %>%
  mutate(
    pvl = p_ast(pvl),
    slo = as.character(round(slo, 2)),
    inc = yrend - yrstr,
    inc = round(inc, 0),
    yrstr = round(yrstr, 0),
    yrstrse = paste0('(', round(yrstrse, 1), ')'),
    yrend = round(yrend, 0),
    yrendse = paste0('(', round(yrendse, 1), ')'),
    bay_segment = factor(bay_segment, levels = c('OTB', 'HB', 'MTB', 'LTB'))
  ) %>%
  unite('slo', slo, pvl, sep = '') %>%
  unite('yrstr', yrstr, yrstrse, sep = ' ') %>%
  unite('yrend', yrend, yrendse, sep = ' ') %>%
  arrange(bay_segment)

names(totab) <- c('Bay segment', 'Increase per year', 'Start days', 'End days', 'Increase')

write.csv(totab, file = here('tabs/mixeff.csv'), row.names = FALSE)

