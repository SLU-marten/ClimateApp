# Gruppera på månader
dataMonthly <- function(data, modelInput){
  if(modelInput == "nursery"){
    data |>
      mutate("left" = AreaAfter/AreaBefore) |>
      group_by(Species, Year, Month) |>
      summarize(Mean = mean(left, na.rm=TRUE)) |> 
      mutate(date = as.Date(paste0(Year, "-", Month, "-15")))
  } else {
    data |> 
      filter(FAO %in% c("COD")) %>%
      ## filter(Stock == "cod.27.24-32") %>%
      filter(Stock != "ALL") %>%
      group_by(Stock,RCP,Year,Month) %>%
      summarise(AreaBefore=mean(AreaBefore),
                AreaAfter=mean(AreaAfter),
                AreaAfter_Temp=mean(AreaAfter_Temp),
                AreaAfter_Sal=mean(AreaAfter_Sal),
                AreaAfter_O2=mean(AreaAfter_O2)) %>%
      gather("Driver","Area", AreaAfter:AreaAfter_O2) %>%
      mutate(Driver = substring(Driver,11,14)) %>%
      unite(grp, Driver, RCP, remove=F) %>%
      filter(RCP == "rcp45") |> 
      mutate(date = as.Date(paste0(Year, "-", Month, "-15")))
  }
}
plotMonthly <- function(data, modelInput){
  if(modelInput == "nursery"){
    ggplot(dataMonthly(data, modelInput), aes(x = Year, y = as.factor(Month), height = Mean)) +
      geom_density_ridges(stat = "identity", scale = 0.95) +
      labs(y = "Month") +
      theme_classic()
  } else {
    tmp <- dataMonthly(data, modelInput) 
    ggplot() +
      geom_line(data = tmp %>% filter(Driver != ""), aes(Year,(Area-AreaBefore)/AreaBefore*100, group=grp, col=Driver)) +
      geom_ribbon(data = tmp %>% filter(Driver == ""), aes(Year,ymin=-100, ymax=(Area-AreaBefore)/AreaBefore*100, group=grp, fill="All"), col=1) +
      geom_line(data = tmp %>% filter(Driver == ""), aes(Year,(Area-AreaBefore)/AreaBefore*100, group=grp), col=1) +
      scale_fill_manual(name = NULL, values = c("#b3b3b3")) +
      ylab("% Area change???") +
      theme_bw() +
      facet_grid(Month~Stock) +
      guides(col  = guide_legend(order = 1),
             fill  = guide_legend(order = 2)) +
      theme(legend.margin = margin(0, 0, 0, 0),
            legend.spacing.y = unit(0, "lines"))
  }
}

# Timeseries
library(forecast)
dataMonthlyComplete <- function(data, modelInput){
  d <- dataMonthly(data, modelInput)
  tmp <- d[1:12,]
  tmp[1:12,] <- NA
  tmp[1:12,"Month"] <- 1:12
  d |>
    as_tibble() |> 
    add_row(tmp, .before = 1) |> 
    complete(
      Species, 
      Year,
      Month, 
      fill = list(Mean = 0, Sd = 0)) |> 
    filter(!Year %in% NA, 
           !Species %in% NA)
}

plotMonthly2 <- function(data, modelInput){
  ts(dataMonthlyComplete(data, modelInput) |> select(Mean),
     start = c(2006, 1),
     end = c(2059, 12),
     frequency = 12) |> 
    ggmonthplot()
}
plotSpiral <- function(data, modelInput){
  tmp <- dataMonthlyComplete(data, modelInput) |>  
    filter(Species== "Abborre") |>
    mutate(Mean = replace(Mean, date %in% NA, NA))
  tmp$month_num <- 0:(nrow(tmp)-1)
  ggplot(tmp, aes(month_num %% 12, month_num/12, height = 10, fill = Mean)) + 
    scale_fill_gradient(low = "#f5e4e8", high = "#fc033d", na.value = "grey70") +
    geom_tile() +
    geom_vline(xintercept = 11.5) +
    geom_vline(xintercept = seq(0.5, 10.5, by = 1), col="grey50", linetype = "dashed") +
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_continuous(breaks = 0:11, minor_breaks = NULL, labels = c(month.abb[1:11], month.abb[12])) +
    coord_polar()  +
    annotate(x = 11.5, 
             y = 1, 
             label = "2006 -", 
             geom = "text", 
             color = "gray12", 
             family = "Bell MT",
             size = 3,
             hjust = 1) +
    annotate(x = 11.5, 
             y = 17, 
             label = "2020 -", 
             geom = "text", 
             color = "gray12", 
             family = "Bell MT",
             size = 3,
             hjust = 1) +
    annotate(x = 11.5, 
             y = 38, 
             label = "2040 -", 
             geom = "text", 
             color = "gray12", 
             family = "Bell MT",
             size = 3,
             hjust = 1) +   
    annotate(x = 11.5, 
             y = 59, 
             label = "2059 -", 
             geom = "text", 
             color = "gray12", 
             family = "Bell MT",
             size = 3,
             hjust = 1) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank()) +
    guides(fill=guide_legend(title="Availible area"))
}
