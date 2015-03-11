## Libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)

## Data ----
load("orange_report_2014_cover/plot-data-omslag.RData")


## Canvas ----
pagewidth = 210
pageheight = 297
binwidth = 20

# Define canvas lines along which to plot
pdf = data_frame(
  x1 = numeric(),
  y1 = numeric(),
  x2 = numeric(),
  y2 = numeric(),
  length = numeric()
)

# Draw diagonal lines from bottom-left to top-right.
# Move from the top left corner, down along the left side margin,
# then along the bottom margin until we reach the bottom right corner.
x1 <- 0
y1 <- pageheight + binwidth/2 * sqrt(2)

while (x1 < pagewidth - binwidth/2 * sqrt(2)) {
  if (y1 > binwidth * sqrt(2)) {
    ## If we haven't yet reached the bottom of the page

    # Starting x
    x1 <- 0
    
    # Starting y
    y1 <- y1 - binwidth * sqrt(2)
    
    # Target x to plot diagonal line
    dist <- pageheight - y1
    x2 <- min(dist, pagewidth)
    
    # Target y
    y2 <- min(pageheight, pageheight - (dist - pagewidth))
    
  } else {
    ## When we have reached the bottom of the page
    
    # Starting x
    x1 <- x1 + binwidth * sqrt(2) + y1
    
    # Starting y
    y1 <- 0
    
    # Target x
    dist <- (pagewidth - x1)
    x2 <- x1 + dist
    
    # Target y
    y2 <- min(dist, pageheight)
    
  }
  
  pdf[nrow(pdf) + 1,] <- c(x1, y1, x2, y2, sqrt((y2 - y1)^2 + (x2 - x1)^2))
}

pdf <- pdf %>% mutate(segment_id = 1:n())

# Plot grid lines
ggplot(pdf, aes(x = x1, xend = x2, y = y1, yend = y2)) +
  geom_segment() +
  xlim(0, pagewidth) +
  ylim(0, pageheight)


## Map plot data to grid data ----
pd <- plot.data %>% 
  tbl_df() %>% 
  filter(sex == "män" & value > 50) %>% 
  # Normalize data to total length of segments
  mutate(value = value * sum(pdf$length)/sum(value))


plotdata <- data_frame(
  segment_id = integer(),
  color = factor(levels = levels(plot.data$variable)),
  dist = numeric()
)

segment <- 1
datacount <- 1
seglen <- 0
# while (count < sum(pdf$length)) {
while (segment <= max(pdf$segment_id)) {
  seglen <- ifelse(seglen > 0, seglen, pdf$length[segment])
  val <- pd$value[datacount]
  
  if (val < seglen) {
    seglen <- seglen - val
    newrow <- list(segment_id = segment, color = pd$variable[datacount], dist = val)
    plotdata[nrow(plotdata) + 1,] <- newrow
    
    datacount <- datacount + 1
  } else {
    newrow <- list(segment_id = segment, color = pd$variable[datacount], dist = seglen)
    plotdata[nrow(plotdata) + 1,] <- newrow
    
    segment <- segment + 1
    newrow <- list(segment_id = segment, color = pd$variable[datacount], dist = val - seglen)
    plotdata[nrow(plotdata) + 1,] <- newrow
    
    seglen <- pdf$length[segment] - (val - seglen)
  }
}

## Create plot coords ----
plotdata <- plotdata %>% 
  left_join(pdf, by = "segment_id") %>% 
  mutate(starting_point = 0)

for (i in 1:nrow(plotdata)) {
  segment <- plotdata$segment_id[i]
  previous_segment <- plotdata$segment_id[i-1]
  if (length(previous_segment) == 0)
    previous_segment <- 0
  
  # Start a new line
  if (previous_segment != segment) {
    plotdata$x1[i] <- 0
    plotdata$y1[i] <- plotdata$y1[i]
  } else {
    plotdata$x1[i] <- plotdata$x2[i-1]
    plotdata$y1[i] <- plotdata$y2[i-1]
  }
  
  plotdata$x2[i] <- plotdata$x1[i] + plotdata$dist[i] / sqrt(2)
  plotdata$y2[i] <- plotdata$y1[i] + plotdata$dist[i] / sqrt(2)

}

ggplot(plotdata, aes(x = x1, xend = x2, y = y1, yend = y2, color = color)) +
  geom_segment() +
  xlim(0, pagewidth) +
  ylim(0, pageheight)





