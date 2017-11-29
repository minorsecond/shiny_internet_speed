library(RPostgreSQL)
library(mgcv)
library(scales)
library(ggplot2)
library(reshape2)
library(lubridate)

source("pub_graphs.R")

give.n <- function(x){
  return(c(y = mean(x) + .5, label = length(x)))
}

data_download_time <- strptime(format(file.info("testResults.Rds")$ctime), format = "%Y-%m-%d %H:%M:%S")
file_age <- Sys.time() - data_download_time

print(paste("file age: ", file_age, sep = ""))
if (file_age > 900 | !file.exists("testResults.Rds")) {
  rm(list = ls())
  
  source("pub_graphs.R")
  query.start.time <- Sys.time()
  plots = list()
  means = list()
  pw <- {
    "Rward0232"
  }
  drv <- dbDriver("PostgreSQL")
  cat(file=stderr(), "testResults.Rds has expired. Downloading fresh data")
 # print("testResults.Rds has expired. Downloading fresh data")
  print(paste("Connecting to PG DB at ", 
              query.start.time, 
              sep = ""))
  
  con <- dbConnect(drv, 
                   dbname = "internetStatus",
                   host = "192.168.1.8",
                   user = "rwardrup",
                   password = pw)
  db.connect.time <- Sys.time() - query.start.time
  print(paste("DB connection took ", 
              db.connect.time, 
              " seconds", 
              sep = ""))
  rm(pw)
  print(paste("Connection successful: ", 
              dbExistsTable(con, "internetStatus")), 
        sep = "")
  print("Querying internetStatus table")
  test_results <- dbGetQuery(con, 'SELECT * FROM "internetStatus"')
  dbDisconnect(con)
  query.end.time <- Sys.time()
  print(paste("Query completed in ", 
              query.end.time - query.start.time, 
              " seconds", 
              sep = ""))
  
  test_results$Timestamp <- as.POSIXct(strptime(test_results$Timestamp, 
                                               "%Y-%m-%dT%H:%M", 
                                               tz="UTC"), tz = "America/Chicago") - (6*3600)
  
  test_results$Download_mb <- test_results$Download * 0.000000954
  test_results$Upload_mb <- test_results$Upload * 0.000000954
  test_results$Distance_mi <- test_results$Distance * 0.621371
  
  test_results$hour_of_day <- as.integer(format(test_results$Timestamp, "%H"))
  
  plots$download_speed <- ggplot(test_results, 
                                 aes(x = Timestamp,
                                     y = Download_mb)) +
    geom_hline(yintercept = 100, 
               colour = 'red', 
               aes(linetype = "Subscription Speed")) +
    annotate("text", 
             x = median(test_results$Timestamp), 
             y = 100, 
             vjust = -.5, 
             label = "Subscribed Download Speed") +
    geom_point(aes(color = Sponsor), size = 2) +
    #geom_smooth(method = 'lm', formula = y ~ poly(x, 2), size = 1) +
    stat_smooth(method = "gam", 
                formula = y ~ s(x, k = 6), 
                size = .75, 
                level = .99) +
    scale_y_sqrt() +
    scale_x_datetime(labels = date_format("%d-%m-%Y", 
                                          tz = "America/Chicago"),
                     breaks = date_breaks("1 day")) +
    theme_Publication() +
    scale_colour_Publication() +
    labs(title = "Charter Internet Download Speed",
         x = "Local Time (CST)",
         y = "Download Speed in Megabits per Second",
         col = "Speed Test Server Sponsor")
  
  plots$upload_speed <- ggplot(test_results, 
                               aes(x = Timestamp,
                                   y = Upload_mb)) +
    geom_hline(yintercept = 10, color = "red", aes(linetype = "Subscription Speed")) +
    annotate("text", x = median(test_results$Timestamp), y = 10, vjust = -.5, label = "Subscribed Upload Speed") +
    geom_point(aes(color = Sponsor), size = 2) +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 6), size = .75, level = .99) +
    scale_y_sqrt() +
    scale_x_datetime(labels = date_format("%d-%m-%Y", 
                                          tz = "America/Chicago"),
                     breaks = date_breaks("1 day")) +
    theme_Publication() +
    labs(title = "Charter Internet Upload Speed",
         x = "Local Time (CST)",
         y = "Upload Speed in Megabits per Second",
         col = "Speed Test Server Sponsor")
  
  plots$overall_down_speeds <- ggplot(test_results,
                                 aes(y = Download_mb, x = NA)) +
    geom_boxplot() +
    theme_Publication() +
    scale_colour_Publication() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(title = "Charter Internet Download Speed - All Dates",
         y = "Download Speed in Megabits per Second")
  
  plots$overall_up_speeds <- ggplot(test_results,
                                 aes(y = Upload_mb, x = NA)) +
    geom_boxplot() +
    theme_Publication() +
    scale_colour_Publication() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(title = "Charter Internet Upload Speed - All Dates",
         y = "Upload Speed in Megabits per Second")
  
  test_results$hour_group <- cut(hour(test_results$Timestamp), 
                                 breaks = c(0, 4, 8, 12, 16, 20, 24), 
                                 include.lowest = T)
  
  plots$time.of.day.down.speed <- ggplot(test_results, 
                                    aes(x = hour(Timestamp), 
                                        y = Download_mb)) + 
    stat_binhex(bins = 10) +
    theme_Publication() +
    scale_fill_gradient(low = "forestgreen", 
                        high = "firebrick") +
    scale_x_continuous(breaks = seq(0, 24, by = 4),
                       labels = c("12 AM", "4 AM", "8 AM",
                                  "12 PM", "4 PM", "8 PM",
                                  "12 AM")) +
    labs(title = "Charter Internet Download Speed - Time of Day",
         x = "Hour of Day",
         y = "Download Speed in Megabits per Second",
         fill = "Count per Bin") +
    theme(legend.key.height = unit(1, "line"))
  
  plots$time.of.day.up.speed <- ggplot(test_results, 
                                    aes(x = hour(Timestamp), 
                                        y = Upload_mb)) + 
    stat_binhex(bins = 10) + 
    theme_Publication() +
    scale_fill_gradient(low = "forestgreen", 
                        high = "firebrick") +
    scale_x_continuous(breaks = seq(0, 24, by = 4),
                       labels = c("12 AM", "4 AM", "8 AM",
                                  "12 PM", "4 PM", "8 PM",
                                  "12 AM")) +
    labs(title = "Charter Internet Upload Speed - Time of Day",
         x = "Hour of Day",
         y = "Upload Speed in Megabits per Second",
         fill = "Count per Bin") +
    theme(legend.key.height = unit(1, "line"))
  
  test_results$day.of.week <- factor(weekdays(test_results$Timestamp), 
                                     levels = c("Sunday", "Monday", "Tuesday",
                                                "Wednesday", "Thursday", "Friday",
                                                "Saturday"))
  
  plots$day.of.week.down <- ggplot(test_results, aes(day.of.week, Download_mb)) + 
    geom_bar(position = "dodge", 
             stat = "summary", 
             fun.y = "mean") +
    scale_x_discrete() +
    theme_Publication() +
    labs(title = "Charter Internet Download Speed - Day of Week",
         x = "Day of Week",
         y = "Download Speed in Megabits per Second") +
    theme(legend.key.height = unit(1, "line"))
  
  plots$day.of.week.up <- ggplot(test_results) + 
    geom_bar(aes(day.of.week, Upload_mb), 
             position = "dodge", stat = "summary", fun.y = "mean") +
    theme_Publication() +
    labs(title = "Charter Internet Upload Speed - Day of Week",
         x = "Day of Week",
         y = "Upload Speed in Megabits per Second") +
    theme(legend.key.height = unit(1, "line")) 
  
  saveRDS(test_results, file = "testResults.Rds")
  saveRDS(plots, "plots.Rds")
  #print("Saved new testResults.Rds file")
} else if (!exists("plots") & exists("plots.Rds")) {
  cat(file=stderr(), "testResults.Rds isn't expired and plots aren't loaded... loading plots from disk.")
  #print("testResults.Rds isn't expired and plots aren't loaded... loading plots from disk.")
  plots <- readRDS("plots.Rds")
  #print("Finished loading Plots")
} else if (!exists("plots") & !exists("plots.Rds") & file.exists("testResults.Rds")) {
  test_results <- readRDS("testResults.Rds")
  plots = list()
  
  plots$download_speed <- ggplot(test_results, 
                                 aes(x = Timestamp,
                                     y = Download_mb)) +
    geom_hline(yintercept = 100, colour = 'red', aes(linetype = "Subscription Speed")) +
    #scale_linetype_manual(values = c("Subscription Speed"), guide = guide_legend(override.aes = list(color = c("red")))) +
    annotate("text", x = sort(test_results$Timestamp)[2] + 20, y = 100, vjust = -.5, label = "Subscribed Download Speed") +
    geom_point(aes(color = Sponsor)) +
    #geom_smooth(method = 'lm', formula = y ~ poly(x, 2), size = 1) +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 6), alpha = .15) +
    #scale_y_sqrt() +
    scale_x_datetime(labels = date_format("%d-%m-%Y", 
                                          tz = "America/Chicago")) +
    theme_Publication() +
    scale_colour_Publication() +
    labs(title = "Charter Internet Download Speed",
         x = "Local Time (CST)",
         y = "Download Speed in Megabits per Second",
         col = "Speed Test Server Sponsor")
  
  plots$upload_speed <- ggplot(test_results, show.legend = F,
                               aes(x = Timestamp,
                                   y = Upload_mb)) +
    geom_hline(yintercept = 10, color = "red", aes(linetype = "Subscription Speed"), show.legend = F) +
    annotate("text", x = sort(test_results$Timestamp)[2], y = 10, vjust = -.5, label = "Subscribed Upload Speed") +
    geom_point(aes(color = Sponsor), show.legend = F) +
    #geom_smooth(method = 'lm', formula = y ~ poly(x, 2), size = 1) +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 6), alpha = .15, show.legend = F) +
    scale_y_sqrt() +
    scale_x_datetime(labels = date_format("%d-%m-%Y", 
                                          tz = "America/Chicago")) +
    theme_Publication() + 
    labs(title = "Charter Internet Upload Speed",
         x = "Local Time (CST)",
         y = "Upload Speed in Megabits per Second") +
    theme(legend.position = "none")
  
  plots$overall_down_speeds <- ggplot(test_results,
                                      aes(y = Download_mb, x = NA)) +
    geom_boxplot() +
    theme_Publication() +
    scale_colour_Publication() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(title = "Charter Internet Download Speed - All Dates",
         y = "Download Speed in Megabits per Second")
  
  plots$overall_up_speeds <- ggplot(test_results,
                                    aes(y = Upload_mb, x = NA)) +
    geom_boxplot() +
    theme_Publication() +
    scale_colour_Publication() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(title = "Charter Internet Upload Speed - All Dates",
         y = "Upload Speed in Megabits per Second")
  
  plots$time.of.day.speed <- ggplot(test_results, 
                                    aes(x = hour_of_day, 
                                        y = Download_mb)) + 
    stat_binhex(bins = 10) + 
    theme_Publication() +
    scale_fill_gradient(low = "forestgreen", 
                        high = "firebrick") +
    labs(title = "Charter Internet Download Speed - Time of Day",
         x = "Hour of Day",
         y = "Download Speed in Megabits per Second",
         fill = "Count per Bin") +
    theme(legend.key.height = unit(1, "line"))
  
} else {
  rm(plots)
  cat(file=stderr(), "Plots already loaded - displaying them now.")
  plots <- readRDS("plots.Rds")
}
  
