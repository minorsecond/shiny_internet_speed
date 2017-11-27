library(RPostgreSQL)
library(mgcv)
library(scales)
source("pub_graphs.R")

pw <- {
  "Rward0232"
}
drv <- dbDriver("PostgreSQL")

data_download_time <- strptime(format(file.info("testResults.Rds")$ctime), format = "%Y-%m-%d %H:%M:%S")
file_age <- Sys.time() - data_download_time
query.start.time <- Sys.time()
if (file_age > 930 | !file.exists("testResults.Rds")) {
  plots = list()
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
                                               tz="UTC"), tz = "UTC")
  
  test_results$Download_mb <- test_results$Download * 0.000000954
  test_results$Upload_mb <- test_results$Upload * 0.000000954
  test_results$Distance_mi <- test_results$Distance * 0.621371
  
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
  
  plots$upload_speed <- ggplot(test_results, 
                               aes(x = Timestamp,
                                   y = Upload_mb)) +
    geom_hline(yintercept = 10, color = "red", aes(linetype = "Subscription Speed")) +
    annotate("text", x = sort(test_results$Timestamp)[2], y = 10, vjust = -.5, label = "Subscribed Upload Speed") +
    geom_point(aes(color = Sponsor)) +
    #geom_smooth(method = 'lm', formula = y ~ poly(x, 2), size = 1) +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 6), alpha = .15) +
    scale_y_sqrt() +
    scale_x_datetime(labels = date_format("%d-%m-%Y", 
                                          tz = "America/Chicago")) +
    theme_Publication() +
    scale_colour_Publication() +
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
  
  saveRDS(test_results, file = "testResults.Rds")
  saveRDS(plots, "plots.Rds")
  #print("Saved new testResults.Rds file")
} else if (!exists("plots")) {
  cat(file=stderr(), "testResults.Rds isn't expired and plots aren't loaded... loading plots from disk.")
  #print("testResults.Rds isn't expired and plots aren't loaded... loading plots from disk.")
  plots <- readRDS("plots.Rds")
  #print("Finished loading Plots")
} else {
  cat(file=stderr(), "Plots already loaded - displaying them now.")
  #print("Plots already loaded - displaying them now.")
}
  
