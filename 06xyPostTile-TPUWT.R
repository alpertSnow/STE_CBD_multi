### contour plots of location marginal, using ggplot2
### date: 2017-05-01
library(ggplot2)
library(RColorBrewer)
library(scales)
### reshape data
m.HPD <- melt(HPD)
## count x first, then count y

xv <- rep(xc, times = nj)
yv <- rep(yc, each = ni)
w <- rep(dx, times = nj)
h <- rep(dy, each = ni)
m.prob <- melt(probMat)/w/h
#m.prob0 <- melt(prob0)

### contour plot of HPD
### pxls: 1200*1000
italic.text <- element_text(face = "italic", size = 32)
## define jet colormap
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
Gr2Bl <- colorRampPalette(c('white',"#f1eef6", "#74a9cf", "#00007f"))
HPD.plot <- ggplot(data= NULL) +
        labs(x="x/H", y="y/H") +
        geom_tile(aes(x=xv, y=yv, fill=m.HPD$value), width = w, height = h) +
#        stat_contour(aes(z=m.HPD$value), breaks = c(0,1,2), colour = 'black',size = 0.5, linejoin='bevel') +
        geom_point(aes(x=x.wt,y=y.wt),pch=21,colour='black',fill='white', size=4.5)+
        geom_point(aes(x=x.real,y=y.real),pch=19,colour='red', size=4)+
        coord_equal() +
        scale_fill_gradientn(name='p(x,y|μ)',colors = Gr2Bl(4), guide=FALSE) +
        scale_x_continuous(expand = c(0, 0), limits=c(1800,2200)) +
        scale_y_continuous(expand = c(0, 0), limits=c(1400,1700)) +
        theme_bw() +
        theme(axis.title = italic.text, axis.text = element_text(size = 32))+
        theme(panel.border = element_rect(fill=NA, colour = "black", size=1.2))+
        theme(axis.ticks = element_line(size = 1.2))+
        theme(legend.text = element_text(size = 24))+
        theme(legend.title = element_text(size = 24, face = 'italic'))+
        theme(legend.key.size = unit(1,"cm"))+
        theme(axis.text = element_text(colour = 'black'))
plot(HPD.plot)

### contour plot of location posterior
### pxls: 1200*1000
locationMargin.plot <- ggplot(data= NULL) +
        labs(x="x/H", y="y/H") +
        geom_tile(aes(x=xv, y=yv, fill=m.prob$value), width = w, height = h) +
        geom_point(aes(x=x.wt,y=y.wt),pch=21,colour='black',fill='white', size=4.5)+
        geom_point(aes(x=x.real,y=y.real),pch=19,colour='red', size=4)+
        coord_equal() +
        scale_fill_gradientn(name='p(x,y|μ)',colors = Gr2Bl(4),values = c(0,0.01,0.2,1), guide=FALSE) +
        #scale_fill_gradientn(name='p(x,y|μ)',colors = brewer.pal(7,'PuBu')) +
        scale_x_continuous(expand = c(0, 0), limits=c(1800,2200)) +
        scale_y_continuous(expand = c(0, 0), limits=c(1400,1700)) +
        theme_bw() +
        theme(axis.title = italic.text, axis.text = element_text(size = 32))+
        theme(panel.border = element_rect(fill=NA, colour = "black", size=1.2))+
        theme(axis.ticks = element_line(size = 1.2))+
        theme(legend.text = element_text(size = 24))+
        theme(legend.title = element_text(size = 24, face = 'italic'))+
        theme(legend.key.size = unit(1,"cm"))+
        theme(axis.text = element_text(colour = 'black'))
        
plot(locationMargin.plot)
