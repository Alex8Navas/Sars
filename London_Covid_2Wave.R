library(data.table)
library(ggplot2)
data_url <- "https://c19downloads.azureedge.net/downloads/csv/coronavirus-cases_latest.csv"
raw_data <- fread(data_url, check.names = TRUE)

plot_lab_confirmed_cases <- function(raw_data, area_name, area_type){
  area_data <- raw_data[
    Area.name == area_name &
      Area.type == area_type,,
    ][,Specimen.date := as.Date(Specimen.date)
      ][,c("Specimen.date","Daily.lab.confirmed.cases")][
        order(Specimen.date)
        ]
  area_data <- merge(area_data,
                     data.table(Specimen.date = seq(
                       min(area_data[,Specimen.date]),
                       max(area_data[,Specimen.date]),
                       by = "1 day" 
                     )), all = TRUE, by = "Specimen.date")
  setkey(area_data, Specimen.date)
  setnafill(area_data, type = "const", fill = 0,
            cols = c("Daily.lab.confirmed.cases"))
  area_data[,roll_mean := frollmean(Daily.lab.confirmed.cases, n = 7, align = "right")]
  m_area_data <- melt(area_data, id.vars="Specimen.date",
                      measure.vars = c("Daily.lab.confirmed.cases","roll_mean"))
  area_plot <- ggplot(m_area_data, aes(x = Specimen.date, y = value, fill = variable, color = variable))+
    geom_bar(data = subset(m_area_data, variable == "Daily.lab.confirmed.cases"),
             stat = "identity") +
    geom_line(data = subset(m_area_data, variable == "roll_mean"), size = 0.5) +
    ylim(0,1100) +
    labs(x = "Fecha de la Muestra", y = "Número de Casos Confirmados",
         fill = "", color = "", caption = "Alejandro Navas González", 
         title = "Evolución de la Pandemia del Sars-Cov2", 
         subtitle = "Londres, Reino Unido; 2020") +
    scale_fill_manual(values = c("steelblue","darkblue"),
                                  labels = c(sprintf("Número de Casos Confirmados", area_name),
                                                       "Media Semanal")) +
                                    scale_color_manual(values = c("steelblue","darkblue"),
                                                                   labels = c(sprintf("Número de Casos Confirmados",area_name),
                                                                                        "Media Semanal")) +
                                                                     scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
                                                                     theme_bw() %+replace% theme(legend.position = "right",
                                                                                                 legend.justification = "right")
                                                                   area_plot
}


london_plot <- plot_lab_confirmed_cases(raw_data, "London", "region")
london_plot
ggsave(filename = "Rbloggers_results/Londres_Covid.png", london_plot, width = 13)
write.csv(x = raw_data, file = "Rbloggers_archives/Londres_2020Covid.csv")
