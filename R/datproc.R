library(here)

# copy bottom temp gams from temp-manu --------------------------------------------------------

fls <- list.files(here('../temp-manu/data/'), pattern = 'tempbot\\.RData$', full.names = TRUE)

file.copy(fls, here('data/'), overwrite = TRUE)

# copy mixmodprds from temp-manu --------------------------------------------------------------

file.copy(here('../temp-manu/data/mixmodprds.RData'), here('data/'), overwrite = TRUE)

# copy mix mods from temp-manu ----------------------------------------------------------------

file.copy(here('../temp-manu/data/mixmods.RData'), here('data/'), overwrite = TRUE)
