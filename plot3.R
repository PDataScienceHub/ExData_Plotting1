
# Plot Daily Enegry Sub Metering 
# Input: Directory containing the Household Power 
# Consumption Data
# Output: PNG file containing above plot

plot3 <- function(dir) {
        
        # Read the data file
        pData <- ReadData(dir)
        
        # Add a column to combine Data and Time
        pData$datetime <- paste(pData$Date, pData$Time)
        pData$datetime <- strptime(pData$datetime, 
                                   "%Y-%m-%d %H:%M:%S")
        
        # Generate the plot and save it to a PNG file
        # Assignment asks to save the plot to plot3.png file 
        # with a width of 480 pixels and a height of 480 
        # pixels.  
        png(file="plot3.png", width = 480, height = 480)
        
        # Draw the blank canvas. Use the max of the
        # three submeters for y-axis scale
        max_sub1 <- max(pData$Sub_metering_1)
        max_sub2 <- max(pData$Sub_metering_2)
        max_sub3 <- max(pData$Sub_metering_3)
        
        if ((max_sub1 > max_sub2) && 
                    (max_sub1 > max_sub3)) {
                with(pData, plot(datetime, Sub_metering_1,
                                type ="n",
                                ylab ="Energy sub metering",
                                xlab = ""))
        } else if ((max_sub2 > max_sub1) && 
                    (max_sub2 > max_sub3)) {
                with(pData, plot(datetime, Sub_metering_2,
                                type="n",
                                ylab ="Energy sub metering",
                                xlab = ""))
        } else {
                with(pData, plot(datetime, Sub_metering_3,
                                type="n",
                                ylab ="Energy sub metering",
                                xlab=""))
        }
        
        # Now draw the lines for each sub meter
        with(pData, lines(datetime, Sub_metering_1, 
                          type="l", col="black"))
        with(pData, lines(datetime, Sub_metering_2, 
                          type="l", col="red"))
        with(pData, lines(datetime, Sub_metering_3, 
                          type="l", col="blue"))
        
        legend("topright", lty=1, 
               col = c("black", "red", "blue"), 
               legend = c("Sub_metering_1", 
                          "Sub_metering_2",
                          "Sub_metering_3"))
        
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
