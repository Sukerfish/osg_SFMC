profilePlot <- function(dataIn, x, y, colorIn){

profilePlot <- ggplot(dataIn,
                      aes(x={{x}},
                          y={{y}}
                      )) +
  geom_point(aes(
             color = {{colorIn}})
  ) +
  scale_y_reverse() +
  scale_color_viridis_c()
}