
# Plot histogram for Global Active Power
# Input: Directory containing the Household Power Consumption # Data
# Output: PNG file containing above plot

plot1 <- function(dir) {
        
        # Read the data file
        pData <- ReadData(dir)
        
        # Plot the histogram and write to a PNG file
        # Assignment asks to save the plot to plot1.png file 
        # with a width of 480 pixels and a height of 480 
        # pixels.  
        png(file="plot1.png", width = 480, height = 480)
        hist(pData$Global_active_power, col="red", 
             xlab="Global Active Power (kilowatts)", 
             main = "Global Active Power")
        
        #Close the device.
        dev.off()
}

# ReadData: Helper function to read the data set and extract
# the rows of interest
ReadData <- function(dir) {
        
        # Set working directory to the directory containing
        # the data set
        setwd(dir)
        
        # Is the data file present?
        file <- "./household_power_consumption.txt"
        if (file.exists(file) == FALSE) {
                stop("Missing Data File")
        }
        # The data set is very large. Specifying ColClasses                         
        # will make read.table much faster.
        # Read few rows initially to find the colClasses
        initial <- read.table(file, sep=";", header=TRUE, 
                              na.strings="?", 
                              comment.char="",
                              nrows = 10)
        classes <- sapply(initial, class)
        
        # Now read the entire data set
        pData <- read.table(file, sep=";", header=TRUE, 
                            na.strings="?", 
                            colClasses=classes, 
                            comment.char="")
        
        #Convert the Date column to Date Class
        pData$Date <- as.Date(pData$Date, "%d/%m/%Y")
        
        # We are interested only in data for dates
        # 2007-02-01 and 2007-02-02
        pData <- pData[(pData$Date == "2007-02-01" | 
                        pData$Date == "2007-02-02"),]
        
        return(pData)
}

