## ------------------------------------------------------------------------
patchiness <- function( fire.intensity ) {
  # using a Burr XII distribution to map non-zero intensity to 
  # proporition burnt
  p <- woodland::pburr(fire.intensity, c=2, k=1, scale=1.5)
  
  # zero intensity values map to zero proportion
  p[ fire.intensity == 0 ] <- 0
  
  p
}

## ----, echo=FALSE, fig.width=5-------------------------------------------
library(ggplot2)

x <- seq(0, 10, 0.1)
dat <- data.frame(intensity = x, prop = patchiness(x))

ggplot(dat, aes(intensity, prop)) + 
  geom_line() +
  theme_bw() +
  labs(x="fire intensity", y="proportion burnt")



