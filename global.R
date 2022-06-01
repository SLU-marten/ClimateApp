nursery <- read.csv2("Data/Nursery.csv")
potSpawn <- read.csv2("Data/pot_spawn.csv")
names(potSpawn)[1] <- names(nursery)[1] <- "Species"