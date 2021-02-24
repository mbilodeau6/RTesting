library(lubridate)
library(ggplot2)
library(reshape)

loaddata <- function() {
  data <- read.csv("./data/Lisa/AllSalesDataSelectedColumns.csv")
  data$COGS <- abs(data$COGS)
  data$eBay.Fee <- abs(data$eBay.Fee)
  data$Paypal.fee <- abs(data$Paypal.fee)
  data$Actual.ship <- abs(data$Actual.ship)
  data$Date.Purch <- as.Date(data$Date.Purch, format="%m/%d/%Y")
  data$Date.Sold <- as.Date(data$Date.Sold, format="%m/%d/%Y")

  data  
}

filter_out_ours <- function(data) {
  personalItems <- c("ours", "", "mine / ebay", "ours / Bookmans", "mom's", "mine")
  data[!(data$How.Aquired %in% personalItems), ]
}

loadpurchasedonly <- function() {
  data <- loaddata()
  sold_data <- data[data$Sheet != 0 & data$Sheet != 13 & !is.na(data$Date.Sold), ]
  
  filter_out_ours(sold_data)
}

daystosell <- function(year = 0) {
  data <- loadpurchasedonly()
  
  if (year > 0) {
    data <- data[as.integer(format(data$Date.Purch, "%Y")) == year, ]
  }
  
  days_to_sell <- data$Date.Sold - data$Date.Purch
  
  total_sales <- length(days_to_sell)

  results <- data.frame(Days.To.Sell = integer(), Percentage = numeric())
  
  ranges <- seq(30, by=30, length=15)
  
  for (i in ranges) {
    sales <- length(days_to_sell[!is.na(days_to_sell) & days_to_sell <= i])
  
    new_data <- list(Days.To.Sell = i, Percentage = sales/total_sales)
    results <- rbind(results, new_data)
  }
  
  results
}

daystosell_allyears <- function() {
  results <- daystosell(2016)
  names(results)[names(results) == "Percentage"] <- "Prc.2016"
  
  years <- c(2017, 2018, 2019, 2020, 2021)
  
  for(i in years) {
    results <- cbind(results, Prc.New = daystosell(i)$Percentage)
    names(results)[names(results) == "Prc.New"] <- paste("Prc.", as.character(i))
  }
  
  results <- cbind(results, Prc.New = daystosell()$Percentage)
  names(results)[names(results) == "Prc.New"] <- "Prc.All"
  
  results
}

safe_profit <-
  function(dateSold,
           priceSold,
           postageSold,
           cogs,
           eBay,
           paypal,
           postageActual) {
    ifelse(is.na(priceSold) & !is.na(dateSold), 0, priceSold) +
      ifelse(is.na(postageSold), 0, postageSold) -
      cogs -
      ifelse(is.na(eBay), 0, eBay) -
      ifelse(is.na(paypal), 0, paypal) -
      ifelse(is.na(postageActual), 0, postageActual)
  }

fix_donation_date <- function(data) {
  data$Date.Sold <- if_else(!is.na(data$Date.Sold), 
                           data$Date.Sold, 
                           if_else(data$Sheet == 13, as.Date(paste(data$Year, "/12/31", sep="")), as.Date(NA)))
  
  data
}

fix_available_amounts <- function(data) {
  data$Selling.Price <- ifelse(data$Sheet == 0, NA, data$Selling.Price)
  data$Selling.Postage <- ifelse(data$Sheet == 0, NA, data$Selling.Postage)
  data$eBay.Fee <- ifelse(data$Sheet == 0, NA, data$eBay.Fee)
  data$Paypal.fee <- ifelse(data$Sheet == 0, NA, data$Paypal.fee)
  data$Actual.ship <- ifelse(data$Sheet == 0, NA, data$Actual.ship)
  data$COGS <- ifelse(is.na(data$COGS), 0, data$COGS)
  data
}

week_from_date <- function(date) {
  posix_date = as.POSIXlt(date)
  round(((posix_date$year-116)*365 + posix_date$yday + 1)  / 7)  
}

date_from_week <- function(week) {
  date_offset <- week * 7
  date_year <- floor(date_offset / 365) + 116 + 1900
  day_of_year <- date_offset %% 365 - 1
  date_string <- paste(date_year, "-", day_of_year, sep="")
  as.POSIXlt(strptime(date_string, "%Y-%j"))
}

stats_by_week <- function() {
  data <- loaddata() %>% 
    filter_out_ours() %>% 
    fix_available_amounts %>%
    mutate(Profit = safe_profit(Date.Sold, Selling.Price, Selling.Postage, COGS, eBay.Fee, Paypal.fee, Actual.ship)) %>%
    mutate(Week.Purch = week_from_date(Date.Purch)) %>%
    mutate(Week.Sold = week_from_date(Date.Sold))
  
  purch_data <- data[data$Sheet != 13, ]

  # TODO: Need to add observations for weeks that had no purchases or sales
  data_by_week <- data.frame(week = data$Week.Purch, type = "Purch", value = data$COGS)
  data_by_week <- rbind(data_by_week, data.frame(week = purch_data$Week.Sold, type = "Sold", value = purch_data$Profit))

  data_by_date <- mutate(data_by_week, date=date_from_week(week)) %>%
    select(date, type, value)
  
  data_by_date
}

graph_count_purch_sale_by_week <- function() {
  data <- stats_by_week()
  data_to_graph <- count(data, date, type)
  data_to_graph$date <- ymd(data_to_graph$date)
  data_to_graph$date <- ymd(data_to_graph$date)
  ggplot(data_to_graph, aes(x = date, y = n, colour = type)) + geom_line() + scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m-%y",
    limits = c(as.Date("2017-01-01"), as.Date("2021-03-01"))
  ) + theme(axis.text.x = element_text(angle = 90)) + ylim(0, 80)
}

scratch <- function() {
  rm(list=ls())
  
  good <- complete.cases(data$COGS, data$Date.Purch)
  cogs <- data$COGS[good]
  years <- as.integer(format(data$Date.Purch[good], "%Y"))
  tapply(cogs, years, sum)
  
  s <- split(data, data$Date.Purch)
  s2 <- sapply(s, function(x) colMeans(x[, c("COGS", "Selling.Price")]))
  
  library(ggplot2)
  install.packages("reshape")
  library(reshape)
  all_results <- daystosell_allyears()
  molten <- melt(all_results, id.vars = "Days.To.Sell")
  ggplot(molten, aes(x = Days.To.Sell, y = value, colour = variable)) + geom_line()
  
  write.csv(all_results, "DaysToSell.csv", row.names = FALSE)
  
  data <- loadpurchasedonly()
  sold_data <- data[data$Sheet != 0 & data$Sheet != 13 & !is.na(data$Date.Sold), ]
  sold_data$COGS[is.na(sold_data$COGS)] <- 0
  sold_data <- cbind(sold_data, Selling.Total = sold_data$Selling.Price + sold_data$Selling.Postage)
  sold_data <- cbind(sold_data, Expense.Total = sold_data$COGS + sold_data$eBay.Fee + sold_data$Paypal.fee + sold_data$Actual.ship)
  plot(sold_data$COGS, sold_data$Selling.Total - sold_data$Expense.Total, main="Profit $ by COGS", xlab="COGS $", ylab="Profit $")
  plot(sold_data$COGS, (sold_data$Selling.Total - sold_data$Expense.Total)/sold_data$COGS, main="Profit % by COGS", xlab="COGS $", ylab="Profit %")
  plot(sold_data$Actual.ship, sold_data$Selling.Total - sold_data$Expense.Total, main="Profit $ by Actual Ship $", xlab="Actual Ship $", ylab="Profit $")
  plot(sold_data$Actual.ship, (sold_data$Selling.Total - sold_data$Expense.Total)/sold_data$COGS, main="Profit % by Actual Ship $", xlab="Actual Ship $", ylab="Profit %")
  
  table(weekdays(data$Date.Purch))
  
  plot(sold_data$COGS, sold_data$Selling.Total - sold_data$Expense.Total, main="Limited Profit $ by COGS", xlab="COGS $", ylab="Profit $", xlim = c(1,15))
  plot(sold_data$Actual.ship, sold_data$Selling.Total - sold_data$Expense.Total, main="Limited Profit $ by Actual Ship $", xlab="Actual Ship $", ylab="Profit $", xlim = c(1, 15))

  data_to_graph <- count(data_by_date, date, type)
  ggplot(data_to_graph, aes(x = date, y = n, colour = type)) + geom_line()
  ggplot(data_by_date, aes(x = date, y = value, colour = type)) + geom_line()
  data_to_graph <- aggregate(.~date+type, data_by_date, sum)
  ggplot(data_to_graph, aes(x = date, y = value, colour = type)) + geom_line()
}