# a simple convenience wrapper function for read.table() with sensible defaults
read_table_data <- function(file = 'household_power_consumption.txt', sep = ';', 
                            commentchar = '', header = T, nrows = 1, ...) {
  read.table(file = file, sep = sep, comment.char = commentchar, header = header, nrows = nrows, ...)
}

# a function to get a value for the colClasses argument to read.table() for optimized reads
get_hpc_colclasses <- function () {
  sample_data <- read_table_data(header = F)
  sapply(sample_data, class)
}

# a function to get the column names as a vector, which is passed in the colnames argument 
# to read.table() in the get_hpc_subset() function below
get_hpc_colnames <- function() {
  colclasses <- get_hpc_colclasses()
  sample_data <- read_table_data(colClasses = colclasses)
  names(sample_data)
}

# a function to combine the Date and Time columns into a single Datetime column of class POSIXct
format_hpc_datetimes <- function(dataframe) {
  # format Date and Time column into a single Datetime column of class Date
  dates <- as.character(dataframe$Date)
  times <- as.character(dataframe$Time)
  datetimestrings <- paste(dates, times)
  datetimes <- strptime(datetimestrings, '%d/%m/%y %H:%M:%S')
  
  # overwrite and return table_data as a new data frame using our new datetimes
  # nasty that I'm mutating the data frame :( but it does save us some memory :)
  table_data <- dataframe[3:9]
  table_data$datetimes <- datetimes
}

# a function to read in our subset of data (per the assignment instructions)
# the grep argument is used to get the data starting at 1/2/2007;00:00:00 
# (see ?read.table documentation on the skip argument) and, since each row is
# one minute, the nrows default to an upper limit of 2 days * 24 hours * 60 minutes = 2880 rows
get_hpc_subset <- function(file = 'household_power_consumption.txt', grep = '31/1/2007;23:59:00', limit = 2880) {
  colclasses <- get_hpc_colclasses()
  colnames <- get_hpc_colnames()
  hpc_data <- read_table_data(header = F, skip = grep(grep, readLines(file)), nrows = limit, 
                  colClasses = colclasses, col.names = colnames)
}

# read our data into memory
hpc_data <- get_hpc_subset()

# subset of hpc_data for plot 2 containing global active power
#global_active_power <- as.numeric(as.character(hpc_data$Global_active_power))

# open graphics device
#png(filename = 'plot1.png', width = 480, height = 480, units = 'px')

# create lines object
#gap_lines <- lines(c('Thu', 'Fri', 'Sat'), global_active_power)

# plot histogram
#plot(gap_hist, main = 'Global Active Power', col = 'red', xlab = 'Global Active Power (kilowatts)')

# close graphics device
#dev.off()
