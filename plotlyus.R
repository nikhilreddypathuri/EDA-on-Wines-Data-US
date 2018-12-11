library(plotly)
df <- read.csv("2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
booo <- booo[-c(10),]
usavgscore <- cbind(usavgscore,booo)
usavgscore <- usavgscore[,-c(2,3)]
usavgscore <- usavgscore[,c(1,4,2,3)]
usavgscore<-usavgscore[,-c(3)]

usavgscore$hover <- with(usavgscore, paste(province, '<br>', "CancerDeaths",  CancerDeaths,'<br>'))
usavgscore <- usavgscore[-c(9),]
usavgscore <- cbind(usavgscore, df[,c("code")])
colnames(usavgscore) [2] <- "CancerDeaths"
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(usavgscore, locationmode = 'USA-states') %>%
  add_trace(
    z = ~CancerDeaths, text = ~hover, locations = ~code,
    color = ~CancerDeaths, colors = 'Reds'
  ) %>%
  colorbar(title = "No. of Deaths") %>%
  layout(
    title = 'Deaths in US due to Cancer',
    geo = g
  )

ggplotly(p)
