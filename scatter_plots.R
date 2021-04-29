# Scatter Plots Soil Data

describe(dfSoilTr[-c(1)])
dfSoilNot0 <- dfSoilTr[dfSoilTr$score!=0, ] # Non Zero only


ggplot <- dfSoilNot0[1:50000,-c(1)]

ggplot(aes(x = WS10M_MAX, y = T2M_MAX, color = score),
       data = ggplot) + geom_point() +
  scale_color_gradientn(colors = rainbow(5))
