y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
      g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
      g <- g  + scale_size(range = c(2, 20), guide = "none" )
      g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
      g <- g + geom_point(aes(colour=freq, size = freq))
      g <- g + scale_colour_gradient(low = "lightblue", high="white")                     
      g <- g + geom_abline(intercept = 0, slope = beta, size = 1)
      mse <- mean( (y - beta * x) ^2 )
      g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
      g
}
