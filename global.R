library(RPostgreSQL)
library(Cairo)
library(mgcv)
library(scales)
library(ggplot2)
library(reshape2)
library(lubridate)

print(getwd())

source("./pub_graphs.R")

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

give.n <- function(x){
  return(c(y = mean(x) + .5, label = length(x)))
}

data_download_time <- strptime(format(file.info("./testResults.Rds")$ctime), format = "%Y-%m-%d %H:%M:%S")
file_age <- Sys.time() - data_download_time

print(paste("file age: ", file_age, sep = ""))
#if (file_age > 15 | !file.exists("testResults.Rds")) {
if (T){
  #rm(list = ls())
  
  source("./pub_graphs.R")
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
                   host = "192.168.3.65",
                   user = "rwardrup",
                   password = pw)
  db.connect.time <- Sys.time() - query.start.time
  print(paste("DB connection took ", 
              db.connect.time, 
              " seconds", 
              sep = ""))
  rm(pw)
  print(paste("Connection successful: ", 
              dbExistsTable(con, "internetstatus")), 
        sep = "")
  print("Querying internetstatus table")
  test_results <- dbGetQuery(con, 'SELECT * FROM "internetstatus"')
  dbDisconnect(con)
  query.end.time <- Sys.time()
  print(paste("Query completed in ", 
              query.end.time - query.start.time, 
              " seconds", 
              sep = ""))
  
  test_results$timestamp <- as.POSIXct(strptime(test_results$timestamp, 
                                               "%Y-%m-%d %H:%M:%S", 
                                               tz="UTC"), tz = "America/Denver") - (6*3600)
  
  test_results$download_mb <- test_results$download * 0.000000954
  test_results$upload_mb <- test_results$upload * 0.000000954
  test_results$distance_mi <- test_results$distance * 0.621371
  
  test_results$hour_of_day <- as.integer(format(test_results$timestamp, "%H"))
  
  plots$download_speed <- ggplot(test_results, 
                                 aes(x = timestamp,
                                     y = download_mb)) +
    geom_hline(yintercept = 1000, 
               colour = 'red', 
               aes(linetype = "Subscription Speed")) +
    annotate("text", 
             x = median(test_results$timestamp), 
             y = 1000, 
             vjust = -.5, 
             label = "Subscribed Download Speed") +
    geom_point(aes(color = sponsor), size = 2) +
    #geom_smooth(method = 'lm', formula = y ~ poly(x, 2), size = 1) +
    stat_smooth(method = "gam", 
                formula = y ~ s(x, k = 12), 
                size = .75, 
                level = .99) +
    scale_y_sqrt() +
    scale_x_datetime(labels = date_format("%b %d %Y")) +
    theme_Publication() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    scale_colour_Publication() +
    labs(title = "XFinity Internet Download Speed",
         x = "Local Time (MST)",
         y = "Download Speed in Megabits per Second",
         col = "Speed Test Server Sponsor")
  
  plots$upload_speed <- ggplot(test_results, 
                               aes(x = timestamp,
                                   y = upload_mb)) +
    geom_hline(yintercept = 35, color = "red", aes(linetype = "Subscription Speed")) +
    annotate("text", x = median(test_results$timestamp), y = 35, vjust = -.5, label = "Subscribed Upload Speed") +
    geom_point(aes(color = sponsor), size = 2) +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 12), size = .75, level = .99) +
    scale_y_sqrt() +
    scale_x_datetime(labels = date_format("%b %d %Y")) +
    theme_Publication() +
    labs(title = "XFinity Internet Upload Speed",
         x = "Local Time (MST)",
         y = "Upload Speed in Megabits per Second",
         col = "Speed Test Server Sponsor")
  
  plots$overall_down_speeds <- ggplot(test_results,
                                 aes(y = download_mb, x = NA)) +
    geom_boxplot() +
    theme_Publication() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    scale_colour_Publication() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(title = "XFinity Internet Download Speed - All Dates",
         y = "Download Speed in Megabits per Second")
  
  plots$overall_up_speeds <- ggplot(test_results,
                                 aes(y = upload_mb, x = NA)) +
    geom_boxplot() +
    theme_Publication() +
    scale_colour_Publication() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(title = "XFinity Internet Upload Speed - All Dates",
         y = "Upload Speed in Megabits per Second")
  
  test_results$hour_group <- cut(hour(test_results$timestamp), 
                                 breaks = c(0, 4, 8, 12, 16, 20, 24), 
                                 include.lowest = T,
                                 right = F,
                                 ordered_result = T)
  
  plots$time.of.day.down.speed <- ggplot(test_results, 
                                    aes(x = hour(timestamp), 
                                        y = download_mb), alpha = ..count..) + 
    stat_bin_hex(bins = 10) +
    theme_Publication() +
    scale_fill_gradient(low = "forestgreen", 
                        high = "firebrick") +
    scale_x_continuous(breaks = seq(0, 24, by = 4),
                       labels = c("12 AM", "4 AM", "8 AM",
                                  "12 PM", "4 PM", "8 PM",
                                  "12 AM")) +
    labs(title = "XFinity Internet Download Speed - Time of Day",
         x = "Hour of Day",
         y = "Download Speed in Megabits per Second",
         fill = "Count per Bin") +
    theme(legend.key.height = unit(1, "line"))
  
  plots$time.of.day.up.speed <- ggplot(test_results, 
                                    aes(x = hour(timestamp), 
                                        y = upload_mb)) + 
    stat_bin_hex(bins = 10) + 
    theme_Publication() +
    scale_fill_gradient(low = "forestgreen", 
                        high = "firebrick") +
    scale_x_continuous(breaks = seq(0, 24, by = 4),
                       labels = c("12 AM", "4 AM", "8 AM",
                                  "12 PM", "4 PM", "8 PM",
                                  "12 AM")) +
    labs(title = "XFinity Internet Upload Speed - Time of Day",
         x = "Hour of Day",
         y = "Upload Speed in Megabits per Second",
         fill = "Count per Bin") +
    theme(legend.key.height = unit(1, "line"))
  
  test_results$day.of.week <- factor(weekdays(test_results$timestamp), 
                                     levels = c("Sunday", "Monday", "Tuesday",
                                                "Wednesday", "Thursday", "Friday",
                                                "Saturday"))
  means.dl <- summarySE(test_results, measurevar = "download_mb", groupvars = "day.of.week")
  plots$day.of.week.down <- ggplot(means.dl, aes(day.of.week, download_mb)) + 
    geom_bar(position = "dodge", 
             stat = "summary", 
             fun.y = "mean") +
    geom_errorbar(aes(ymin = download_mb - ci,
                      ymax = download_mb + ci),
                  width = .2,
                  position = position_dodge(.9)) +
    geom_text(aes(label = paste(round(download_mb, 2), 
                                " Mb/s\n",
                                "n = ",
                                N,
                                sep = "")), 
              position = position_dodge(width = 0.9),
              vjust = 5,
              color = "white",
              size=3) +
    scale_x_discrete() +
    theme_Publication() +
    labs(title = "XFinity Internet Download Speed - Day of Week",
         x = "Day of Week",
         y = "Download Speed in Megabits per Second") +
    theme(legend.key.height = unit(1, "line"))
  
  means.ul <- summarySE(test_results, measurevar = "upload_mb", groupvars = "day.of.week")
  plots$day.of.week.up <- ggplot(means.ul,
                                 aes(day.of.week, 
                                     upload_mb)) + 
    geom_bar(position = "dodge", 
             stat = "summary", 
             fun.y = "mean") +
    geom_errorbar(aes(ymin = upload_mb - ci,
                      ymax = upload_mb + ci),
                  width = .2,
                  position = position_dodge(.9)) +
    geom_text(aes(label = paste(round(upload_mb, 2), 
                                " Mb/s\n",
                                "n = ",
                                N,
                                sep = "")), 
              position = position_dodge(width = 0.9),
              vjust = 5,
              color = "white",
              size = 3) +
    scale_x_discrete() +
    theme_Publication() +
    labs(title = "XFinity Internet Upload Speed - Day of Week",
         x = "Day of Week",
         y = "Upload Speed in Megabits per Second") +
    theme(legend.key.height = unit(1, "line")) 
  
  saveRDS(test_results, file = "./testResults.Rds")
  saveRDS(plots, "./plots.Rds")
  #print("Saved new testResults.Rds file")
} else if (!exists("plots") & exists("./plots.Rds")) {
  cat(file=stderr(), "testResults.Rds isn't expired and plots aren't loaded... loading plots from disk.")
  #print("testResults.Rds isn't expired and plots aren't loaded... loading plots from disk.")
  plots <- readRDS("./plots.Rds")
  #print("Finished loading Plots")
} else if (!exists("plots") & !exists("./plots.Rds") & file.exists("/srv/shiny-server/InternetSpeeds/testResults.Rds")) {
  test_results <- readRDS("./testResults.Rds")
  plots = list()
  
  plots$download_speed <- ggplot(test_results, 
                                 aes(x = timestamp,
                                     y = download_mb)) +
    geom_hline(yintercept = 1000, colour = 'red', aes(linetype = "Subscription Speed")) +
    #scale_linetype_manual(values = c("Subscription Speed"), guide = guide_legend(override.aes = list(color = c("red")))) +
    annotate("text", x = sort(test_results$timestamp)[2] + 20, y = 100, vjust = -.5, label = "Subscribed Download Speed") +
    geom_point(aes(color = sponsor)) +
    #geom_smooth(method = 'lm', formula = y ~ poly(x, 2), size = 1) +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 6), alpha = .15) +
    #scale_y_sqrt() +
    scale_x_datetime(labels = date_format("%d-%m-%Y", 
                                          tz = "America/Denver")) +
    theme_Publication() +
    scale_colour_Publication() +
    labs(title = "XFinity Internet Download Speed",
         x = "Local Time (MST)",
         y = "Download Speed in Megabits per Second",
         col = "Speed Test Server Sponsor")
  
  plots$upload_speed <- ggplot(test_results, show.legend = F,
                               aes(x = timestamp,
                                   y = upload_mb)) +
    geom_hline(yintercept = 1000, color = "red", aes(linetype = "Subscription Speed"), show.legend = F) +
    annotate("text", x = sort(test_results$timestamp)[2], y = 35, vjust = -.5, label = "Subscribed Upload Speed") +
    geom_point(aes(color = sponsor), show.legend = F) +
    #geom_smooth(method = 'lm', formula = y ~ poly(x, 2), size = 1) +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 6), alpha = .15, show.legend = F) +
    scale_y_sqrt() +
    scale_x_datetime(labels = date_format("%d-%m-%Y", 
                                          tz = "America/Denver")) +
    theme_Publication() + 
    scale_colour_Publication() +
    labs(title = "XFinity Internet Upload Speed",
         x = "Local Time (MST)",
         y = "Upload Speed in Megabits per Second") +
    theme(legend.position = "none")
  
  plots$overall_down_speeds <- ggplot(test_results,
                                      aes(y = download_mb, x = NA)) +
    geom_boxplot() +
    theme_Publication() +
    scale_colour_Publication() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(title = "XFinity Internet Download Speed - All Dates",
         y = "Download Speed in Megabits per Second")
  
  plots$overall_up_speeds <- ggplot(test_results,
                                    aes(y = upload_mb, x = NA)) +
    geom_boxplot() +
    theme_Publication() +
    scale_colour_Publication() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(title = "XFinity Internet Upload Speed - All Dates",
         y = "Upload Speed in Megabits per Second")
  
  plots$time.of.day.speed <- ggplot(test_results, 
                                    aes(x = hour_of_day, 
                                        y = download_mb)) + 
    stat_binhex(bins = 10) + 
    theme_Publication() +
    scale_fill_gradient(low = "forestgreen", 
                        high = "firebrick") +
    labs(title = "XFinity Internet Download Speed - Time of Day",
         x = "Hour of Day",
         y = "Download Speed in Megabits per Second",
         fill = "Count per Bin") +
    theme(legend.key.height = unit(1, "line"))
  
} else {
  rm(plots)
  cat(file=stderr(), "Plots already loaded - displaying them now.")
  plots <- readRDS("./plots.Rds")
}
