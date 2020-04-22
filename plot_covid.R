#!/usr/bin/env Rscript

list.of.packages <- c("optparse", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("optparse")
library("data.table")

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file name", metavar="character"),
  make_option(c("-t", "--type"), type="character", default="log", 
              help="log type - log or linear", metavar="character"),
  make_option(c("-r", "--rmean"), type="integer", default="7", 
              help="rolling mean: yes or no", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$file)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

png("cv.png")

csv_file <- opt$file
plot_type <- opt$type
rmean <- opt$rmean

colors <- c('#5D8AA8', '#E32636', '#FFBF00', '#8DB600', '#4B5320', '#E9D66B', '#B2BEB5', '#007FFF', '#6D351A', '#CC0000')
states <- c('SP', 'RJ', 'AM', 'CE', 'PE')

if (length(states) > length(colors)) {
    stop ("too many states")
}

cv <- read.csv(csv_file, header = TRUE, sep=";")

colnames(cv) <- c("region", "state_code", "date", "newCases", "accCases", "newDeaths", "accDeaths")

first_it <- TRUE 
idx <- 1 
for (state in states) {
    cv_state <- subset(cv, cv$state_code == state, select = c("date", "accDeaths"))
    cv_state_x <- as.Date(cv_state$date, format='%Y-%m-%d')
    if (any(is.na(cv_state_x))) {
        cv_state_x <- as.Date(cv_state$date, format='%d/%m/%Y')
    }
    cv_state_y <- cv_state$accDeaths

    if (rmean > 0 && rmean < 30) {
        cv_state_y <- frollmean(cv_state_y, rmean)
    }
   
    if (isTRUE (first_it)) {
        first_it <- FALSE
        if (plot_type == "log") {
            plot(cv_state_x, cv_state_y, log="y", type="l", col=colors[idx], main='Log Death by State', xlab='Date', ylab='Deaths')
        } else {
            plot(cv_state_x, cv_state_y, type="l", col=colors[idx], main='Log Death by State', xlab='Date', ylab='Deaths')
        }
    } else {
        lines(cv_state_x, cv_state_y, type="l", col=colors[idx])
    }
    idx = idx + 1
}

state_colors <- colors[1:length(states)]

legend(x='topleft', y=0.7, legend=states, col=state_colors, lty=1, cex=0.9)

dev.off()
browseURL("cv.png") 
