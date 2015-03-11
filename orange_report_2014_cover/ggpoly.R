
## Libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)


## Run variables ----
binwidth <- 5
pageheight <- 297
pagewidth <- 210

## Data ----
# pd <- diamonds %>%
#   tbl_df() %>%
#   filter(table < 63 & table > 55) %>%
#   select(table, color) %>%
#   count(table, color) %>%
#   ungroup() %>%
#   mutate(id = 1:n())

# Base plot data
pd <- plot.data %>%
  dplyr::rename(table = alder, color = variable) %>%
  # group_by(id) %>%
  # mutate(value = value * (pagewidth*sqrt(2)/max(value))) %>%
  # mutate(value = value * (pagewidth*sqrt(2)/max(value))) %>%
  # mutate(value = min(value, pagewidth * sqrt(2))) %>%
  filter(value > 100) %>%
  filter(sex == "män") %>%  # ersätt detta med totaler senare
  mutate(value = log(value, base = 1.5) * 2)

## Function to split big values into smaller chunks
makerows <- function(value, pagewidth) {
  retvals <- numeric(0)
  if (value > pagewidth * sqrt(2)) {
    part <- pagewidth * sqrt(2)
    retvals <- append(part, makerows(value - pagewidth * sqrt(2), pagewidth))
  } else {
    retvals <- value
  }

  return(retvals)
}


## Create plot data ----
# Target DF for plot data
pdf <- data_frame(
  table = 0L,
  color = factor(1, levels=levels(pd$color)),
  value = numeric(1),
  cumsum = numeric(1),
  i = integer(1),
  j = integer(1)
)

# Fill up target DF
for (i in 1:nrow(pd)) {
  ci <- pd$color[i]
  vi <- pd$value[i]
  newval <- pdf$cumsum[nrow(pdf)] + vi

  valsum <- makerows(newval, pagewidth)
  valsum[1] <- valsum[1] - pdf$cumsum[nrow(pdf)]

  for(j in 1:length(valsum)) {
    ti <- max(pdf$table) + 1
    insert_to_row <- ifelse(i == 1 & j == 1, 1, nrow(pdf) + 1)
    cs <- ifelse(i == 1 & j == 1, 0, pdf$cumsum[insert_to_row - 1] + valsum[j]) %% (pagewidth * sqrt(2))

    pdf[insert_to_row,] <- list(ti, ci, valsum[j], cs, i, j)
  }
}

pdf <- pdf %>%
  # filter(value > 500) %>%
  mutate(id = 1:n())


## Calculate coordinates ----
# Coordinates in DF
pdf$x1 <- numeric(1)
pdf$x2 <- numeric(1)
pdf$x3 <- numeric(1)
pdf$x4 <- numeric(1)
pdf$y1 <- numeric(1)
pdf$y2 <- numeric(1)
pdf$y3 <- numeric(1)
pdf$y4 <- numeric(1)

# First iteration
x1 <- 0
y1 <- pageheight - binwidth * sqrt(2)
x2 <- -binwidth / sqrt(2)
y2 <- pageheight - binwidth / sqrt(2)
x3 <- x2 + pdf$value[1] / sqrt(2)
y3 <- y2 + pdf$value[1] / sqrt(2)
x4 <- x1 + pdf$value[1] / sqrt(2)
y4 <- y1 + pdf$value[1] / sqrt(2)

pdf$x1[1] <- x1; pdf$y1[1] <- y1
pdf$x2[1] <- x2; pdf$y2[1] <- y2
pdf$x3[1] <- x3; pdf$y3[1] <- y3
pdf$x4[1] <- x4; pdf$y4[1] <- y4


# The rest of the values
colnumber <- 1
for (i in 2:317) {
  # If
  if (pdf$x3[i-1] > pagewidth | pdf$y4[i-1] > pageheight) {
    colnumber <- colnumber + 1
    x1 <- 0
    y1 <- pageheight - colnumber * binwidth * sqrt(2)
    x2 <- -binwidth / sqrt(2)
    y2 <- pageheight - (colnumber * binwidth * sqrt(2)) + binwidth / sqrt(2)
  } else {
    x1 <- pdf$x4[i-1]
    y1 <- pdf$y4[i-1]
    x2 <- pdf$x3[i-1]
    y2 <- pdf$y3[i-1]
  }

  x3 <- x2 + pdf$value[i] / sqrt(2)
  y3 <- y2 + pdf$value[i] / sqrt(2)
  x4 <- x1 + pdf$value[i] / sqrt(2)
  y4 <- y1 + pdf$value[i] / sqrt(2)

  pdf$x1[i] <- x1
  pdf$x2[i] <- x2
  pdf$x3[i] <- x3
  pdf$x4[i] <- x4
  pdf$y1[i] <- y1
  pdf$y2[i] <- y2
  pdf$y3[i] <- y3
  pdf$y4[i] <- y4
}


# Convert wide data to long data
pd_vals <- pdf %>%
  gather(rowidx, x, x1:x4) %>%
  select(id, color, x) %>%
  bind_cols(
    pdf %>%
      gather(rowidy, y, y1:y4) %>%
      select(y)
  ) %>% tbl_df %>% arrange(id)

## Draw graph ----
ggplot(pd_vals, aes(x = x, y = y, fill = color, group = id)) +
  geom_polygon() +
  scale_fill_brewer() +
  geom_vline(xintercept = c(0, pagewidth)) +
  geom_hline(yintercept = c(0, pageheight))
# xlim(0, 500) +
# ylim(0, 500)
# xlim(0, 210) +
# ylim(-210, 600)
# coord_equal()



# ## Correction for coordinates outside boundary area ----
# pd_corr <- within(pd, {
#   for (i in 1:nrow(pd)) {
#
#     # Left margin
#     if (x2[i] < 0) {
#       y2[i] <- y2[i] + x2[i]
#       x2[i] <- 0
#     }
#     if (x3[i] < 0) {
#       y3[i] <- y3[i] + x3[i]
#       x3[i] <- 0
#     }
#
#     # Top margin
# #     if (y2[i] > pageheight) {
# #       x2[i] <- x2[i] - (y2[i] - pageheight)
# #       y2[i] <- pageheight
# #     }
#     if (y3[i] > pageheight) {
#       x3[i] <- x3[i] - (y3[i] - pageheight)
#       y3[i] <- pageheight
#     }
#     if (y4[i] > pageheight) {
#       x4[i] <- x4[i] - (y4[i] - pageheight)
#       y4[i] <- pageheight
#     }
#
#     # Right margin
# #     if (x1[i] > pagewidth) {
# #       y1[i] <- y1[i] - (x1[i] - pagewidth)
# #       x1[i] <- pagewidth
# #     }
# #     if (x3[i] > pagewidth) {
# #       y3[i] <- y3[i] - (x3[i] - pagewidth)
# #       x3[i] <- pagewidth
# #     }
# #     if (x4[i] > pagewidth) {
# #       y4[i] <- y4[i] - (x4[i] - pagewidth)
# #       x4[i] <- pagewidth
# #     }
#
#     # Bottom margin
# #     if (y1[i] < 0) {
# #       x1[i] <- x1[i] - y1[i]
# #       y1[i] <- 0
# #     }
# #     if (y2[i] < 0) {
# #       x2[i] <- x2[i] - y2[i]
# #       y2[i] <- 0
# #     }
# #     if (y4[i] > pageheight) {
# #       x4[i] <- x4[i] - (y4[i] - pageheight)
# #       y4[i] <- pageheight
# #     }
#
#
#   }
# })
#
# # Convert wide data to long data
# pd_vals <- pd_corr %>%
#   gather(rowidx, x, x1:x4) %>%
#   select(id, table, color, x) %>%
#   bind_cols(
#     pd_corr %>%
#       gather(rowidy, y, y1:y4) %>%
#       select(y)
#   ) %>% tbl_df() %>% arrange(id)
#
# ## Draw graph ----
# ggplot(pd_vals, aes(x = x, y = y, fill = color, group = id)) +
#   geom_polygon() +
#   scale_fill_brewer() +
#   xlim(0, pageheight) +
#   ylim(0, pageheight)
