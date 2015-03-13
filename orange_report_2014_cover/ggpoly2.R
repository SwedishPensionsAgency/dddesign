## Libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(pmreports)

## Data ----
load("orange_report_2014_cover/plot-data-omslag.RData")


## Canvas ----
pagewidth = 216
pageheight = 303
binwidth = 16
SEX = "män"

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
    x1 <- x1 + binwidth * sqrt(2) - y1
    
    # Starting y
    y1 <- 0
    
    # Target x
    dist <- (pagewidth - x1)
    y2 <- min(dist, pageheight)
    
    # Target y
    x2 <- min(pagewidth, pagewidth - (dist - pageheight))
  }
  
  pdf[nrow(pdf) + 1,] <- c(x1, y1, x2, y2, sqrt((y2 - y1)^2 + (x2 - x1)^2))
}

pdf <- pdf %>% mutate(segment_id = 1:nrow(.))

# Plot grid lines
ggplot(pdf, aes(x = x1, xend = x2, y = y1, yend = y2)) +
  geom_segment() +
  xlim(0, pagewidth) +
  ylim(0, pageheight)


## Map plot data to grid data ----
pd <- plot.data %>% 
  tbl_df() %>% 
  filter(sex == SEX & value > 50) %>% 
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
plotdata_coords <- plotdata %>% 
  left_join(pdf, by = "segment_id") %>% 
  mutate(starting_point = 0)

for (i in 1:nrow(plotdata_coords)) {
  segment <- plotdata_coords$segment_id[i]
  previous_segment <- plotdata_coords$segment_id[i-1]
  if (length(previous_segment) == 0)
    previous_segment <- 0
  
  # Start a new line
  if (previous_segment == segment) {
    plotdata_coords$x1[i] <- plotdata_coords$x2[i-1]
    plotdata_coords$y1[i] <- plotdata_coords$y2[i-1]
  }
  
  plotdata_coords$x2[i] <- plotdata_coords$x1[i] + plotdata_coords$dist[i] / sqrt(2)
  plotdata_coords$y2[i] <- plotdata_coords$y1[i] + plotdata_coords$dist[i] / sqrt(2)

}

# ggplot(plotdata_coords, aes(x = x1, xend = x2, y = y1, yend = y2, color = color)) +
#   geom_segment(size = binwidth) +
#   xlim(0, pagewidth) +
#   ylim(0, pageheight) +
#   scale_color_manual(values = pmreports::pm_colors()[c(7, 9, 8, 6, 5, 3, 2, 1, 4)])


plotdata_coords$xa <- numeric(1)
plotdata_coords$xb <- numeric(1)
plotdata_coords$xc <- numeric(1)
plotdata_coords$xd <- numeric(1)
plotdata_coords$ya <- numeric(1)
plotdata_coords$yb <- numeric(1)
plotdata_coords$yc <- numeric(1)
plotdata_coords$yd <- numeric(1)

for (i in 1:nrow(plotdata_coords)) {
  plotdata_coords$xa[i] <- plotdata_coords$x1[i] + binwidth / 2 * sqrt(2)
  plotdata_coords$xb[i] <- plotdata_coords$x1[i] - binwidth / 2 * sqrt(2)
  
  plotdata_coords$ya[i] <- plotdata_coords$y1[i] - binwidth / 2 * sqrt(2)
  plotdata_coords$yb[i] <- plotdata_coords$y1[i] + binwidth / 2 * sqrt(2)
  
  plotdata_coords$xc[i] <- plotdata_coords$x2[i] - binwidth / 2 * sqrt(2)
  plotdata_coords$xd[i] <- plotdata_coords$x2[i] + binwidth / 2 * sqrt(2)
  
  plotdata_coords$yc[i] <- plotdata_coords$y2[i] + binwidth / 2 * sqrt(2)
  plotdata_coords$yd[i] <- plotdata_coords$y2[i] - binwidth / 2 * sqrt(2)
  
  # Canvas borders correction
  # if ()
}

# Convert wide data to long data
pd_long <- plotdata_coords %>%
  mutate(id = 1:nrow(.)) %>% 
  gather(rowidx, x, xa:xd) %>%
  select(id, color, x) %>%
  bind_cols(
    plotdata_coords %>%
      gather(rowidy, y, ya:yd) %>%
      select(y)
  ) %>% tbl_df %>% arrange(id)


## Final plot ----
SEX <- gsub("ä", "ae", SEX)

ggplot(pd_long, aes(x = x, y = y, fill = color, group = id)) +
  geom_polygon() +
  scale_fill_manual(values = pm_colors_rgb()[c(6, 7, 9, 5, 1, 3, 4, 2, 8)]) +
  # geom_vline(xintercept = c(0, pagewidth)) +
  # geom_hline(yintercept = c(0, pageheight)) +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, pagewidth), ylim = c(0, pageheight)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
ggsave(sprintf("or_cover_v1_%s.pdf", SEX), width = pageheight, height = pageheight, units = "mm")
ggsave(sprintf("or_cover_v1_%s.png", SEX), width = pageheight, height = pageheight, units = "mm")

