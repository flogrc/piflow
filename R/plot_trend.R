#'Plot data and Mann-Kendall, Pettitt, Sen, Sequential Mann-Kendall tests
#'
#'Plot two graphs, the first one with chronologically data and
#'or not Mann-Kendall test results (trend), Pettitt test
#'(breaking point) and Sen slope test (regression).\cr
#'Second one with Sequential Mann-Kendall test.
#'
#'@param data (zoo) Vector with daily, monthly, seasonnal or annual
#'with date in \%Y-\%m-\%d
#'@param trend (boolean) Add or not results of different trend and
#'breaking point test
#'@param data_kind (character) Indicate the kind of input data
#'(temperature, precipitation, discharge...)
#'@param name (character) Indicate the name of input data
#'(France, World, n°81124...)
#'@param axis_name_x (character) Indicate axis x title name
#'(Year, Month, Season, Day)
#'@param axis_name_y (character) Indicate axis y title name
#'(Precipitation, Total discharge, Mean annual temperature...)
#'
#'@return  If (trend == TRUE): two grpahs, first one with data and trend results, second one with sequential Mann-Kendall test
#'@return  If (trend == FALSE): one graph with data only
#'
#'@author Pierre L'HERMITE
#'
#'@examples
#'## Data preparation
#'load("data/Prec_data.Rdata")
#'prec <- zoo(PluvioData$TabCompleteP, PluvioData$TabDatesR)
#'
#'## Plot without trends
#'#'plot_trend(prec, trend = FALSE, data_kind = "Precipitation", name = "Paris",
#'axis_name_x = "Date", axis_name_y = Monthly precipitation (mm/month),
#'midvalue = 0)
#'
#'## Plot with trends
#'plot_trend(prec, trend = TRUE, data_kind = "Precipitation", name = "Paris",
#'axis_name_x = "Date", axis_name_y = Monthly precipitation (mm/month),
#'midvalue = 0)


plot_trend <- function(data, trend = TRUE, data_kind ="", name ="",
                       axis_name_x = "Time", axis_name_y = "Data [unit]",
                       mid_value = 0)
{
  ##__Checking______________________________________________________________####
  # Checking zoo data
  if (!is.zoo(data)) { stop("Data must be a zoo"); return(NULL) }

  # Checking presence in data
  index_NA <- which(!is.na(coredata(data)))
  if(length(index_NA) == 0) { stop("Data must have values"); return(NULL)}

  ##__Preparation of data to plot it________________________________________####
  datatest <- data[index_NA]

  if (trend == TRUE){
    res_test <- mk_sen_pettitt(datatest)
    breaking_year <- format(index(datatest[res_test[[3]]$estimate]),
                            format = "%Y-%m")
  }

  # Extract year
  ystart <- as.numeric(format(start(datatest), format="%Y"))
  yend <- as.numeric(format(end(datatest), format="%Y"))

  # Modification to plot bicolor graph
  coredata(datatest)[is.na(coredata(datatest))] <- mid_value
  indexlabel <- format(index(datatest), format = "%Y-%m")
  datatest <- c(mid_value, coredata(datatest), mid_value)

  ##__Plot__________________________________________________________________####
  # Plot data
  if (trend == TRUE){
    plot(datatest, type = "l", xaxt = "n",
         ylim = c(round(min(coredata(datatest), na.rm = TRUE),5),
                  round(max(coredata(datatest), na.rm = TRUE),5)),
         xlab = axis_name_x, ylab = axis_name_y,
         main = paste0(data_kind, " in ", name, " from ", ystart, " to ", yend,
                       " with Sen's regression in purple, the Pettitt's
                        breaking year in blue and Mann-Kendall results"),
         cex.main = 0.9, cex.lab = 1, cex.axis = 1)
  } else {
    plot(datatest, type = "l", xaxt = "n",
         ylim = c(round(min(coredata(datatest), na.rm = TRUE),5),
                  round(max(coredata(datatest), na.rm = TRUE),5)),
         xlab = axis_name_x, ylab = axis_name_y,
         main = paste0(data_kind, " in ", name, " from ", ystart, " to ", yend),
         cex.main = 0.9, cex.lab = 1, cex.axis = 1)
  }
  grid()
  axis(1, at = seq(1, length(datatest), 60),
       labels = indexlabel[seq(1, length(datatest), 60)])
  datatest.pos <- ifelse(coredata(datatest) >= mid_value, datatest, mid_value)
  datatest.neg <- ifelse(coredata(datatest) < mid_value, datatest, mid_value)
  polygon(datatest.pos, col = "blue", border = NA)
  polygon(datatest.neg, col = "red3", border = NA)
  lines(datatest, col = "dark grey")
  abline(h = mid_value)

  # Boolean to add or not results of different tests
  if (trend == TRUE){
    legend(1,round(max(coredata(datatest), na.rm = TRUE), 5),
           paste("Sen's slope test\nY=",
                 as.character(round(res_test[[5]], 4)), "*X"), bty = "n",
           cex = 0.6)
    abline(a = mid_value, b = res_test[[5]], col = "mediumpurple3")
    legend((length(datatest)/3),round(max(coredata(datatest), na.rm = TRUE),5),
           paste("Mann-Kendall's test\nTau =",
                 as.character(round(as.numeric(res_test[[1]]$estimates[3]),3)),
                 "and pvalue =", as.character(round(res_test[[1]]$p.value,3)),
                 "(", res_test[[2]], ")"), bty = "n", cex = 0.6)
    if(res_test[[3]]$p.value < 0.1){
      abline(v = res_test[[3]]$estimate, col = "darkblue")
      legend((2*(length(datatest))/3),round(max(coredata(datatest),
                                                na.rm = TRUE),5),
             bty = "n", cex = 0.6,
             paste("Pettitt's breaking point test: pvalue= ",
                   as.character(round(res_test[[3]]$p.value,4)),
                   "(", res_test[[4]], ")","\nBreaking year (dark blue):",
                   breaking_year))
    }


    # Sequential test of Mann-Kendall
    sqmk <- seqMK(coredata(datatest))

    # SQMK plot
    plot(c(1:length(datatest)), sqmk$prog, type = "l",
         ylim = c(round(min(sqmk$prog, sqmk$retr, na.rm = TRUE),0) - 2,
                  round(max(sqmk$prog, sqmk$retr, na.rm = TRUE),0) + 2),
         ylab = c("u(t) [SU] et u'(t) [SU]"),
         xlab = axis_name_x , xaxt = "n", cex.main = 1, cex.lab = 1,
         cex.axis = 1,
         main = paste0("Sequential Mann-Kendall test of ", data_kind,
                       "\nin ", name," from ", ystart," to ", yend))
    lines(c(1:length(datatest)), sqmk$retr, lty = 2)
    axis(1, at = seq(1, length(datatest), 60),
         labels = indexlabel[seq(1, length(datatest), 60)])
    legend("topright", legend = c("u(t)", "u'(t)"), lty = 1:2, bty = "n",
           cex = 1)
    abline(h = 1.96, lty = 3, col = "darkblue")
    abline(h = -1.96, lty = 3, col = "darkblue")
    abline(h = 0, col = "gray64")
  }

  return(NULL)
}
