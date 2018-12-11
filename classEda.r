classEda <- function(
  x, 
  lab1 = "Variable Name",
  units= "Variable Units",
  bw="bw.SJ", adjust=1,kernel="g",
  dist = "norm", QQxlab="Normal Quantiles",
  xloc=c(.33,.67),
  yloc=c(.6,.38,.16),
  sep=.09,
  textLines = c(.86,.76),
  textFonts = c(2,2,1),  
  cexS = 1, cexL = 1.3,
  panelCol = rgb(.95, 1.00, .95),
  densityCol = rgb(0,.85,1),
  densityLineCol = rgb(.3,.3,.3)
)


{ require(car)
  layout(mat=matrix(1:4 , ncol=2))
  oldpar <- par(mai=c(.72, .68, .4, .2))
  plot(c(0, 1), c(0, 1), axes=FALSE, type='n',
    xlab='', ylab='', main='')
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], density=-1,
    col=panelCol, border="black")
  labs <- c(lab1,units)
  cex <- c(cexL,cexL)
  text(.5,textLines, labs, cex=, font=textFonts)
  stat<- summary(x)
  nam <- names(stat)
  text(xloc,rep(yloc[1],2),nam[c(3,4)])
  text(xloc,rep(yloc[1],2)-sep,stat[c(3,4)])
  text(xloc,yloc[2],nam[c(2,5)])
  text(xloc,yloc[2]-sep,stat[c(2,5)])
  text(xloc,yloc[3],nam[c(1,6)])
  text(xloc,yloc[3]-sep,stat[c(1,6)])

  qqPlot(x, dist=dist, las=1, 
    xlab= QQxlab, ylab=units,
    main="Q-Q Plot")

  classDensity(x, col=densityCol,
    border=densityLineCol, xlab=units) 

  boxplot(x, horizontal=TRUE, col=densityCol,
    tck=.05, mgp=c(2, 0, 0), main="Box Plot", xlab=units)
 
  par(oldpar)
}

# windows(w=7,h=7)
# x <- rnorm(100, mean=100, sd=16)
# classEda(x)

#windows(w=5,h=5)
#x <- rexp(100)
#classEda(x,dist="exp",QQxlab="Exponential Quantiles")

 

