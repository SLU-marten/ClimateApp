library(readr)
nursery <- read_delim("Data/Nursery.csv", locale = locale(decimal_mark = ","),  show_col_types = FALSE)
potSpawn <- read_delim("Data/pot_spawn.csv", locale = locale(decimal_mark = ","), show_col_types = FALSE)
