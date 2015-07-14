library("ggplot2")

poz <- rnorm(10000, 0.6, 0.1)
neg <- rnorm(10000, 0.4, 0.1)

dpoz <- density(poz)
dneg <- density(neg)

prog <- 0.6

danepoz <- data.frame(x=dpoz$x, 
                      y=dpoz$y, 
                      xx=c(rep(prog, length(which(dpoz$x<prog))), dpoz$x[which(dpoz$x>=prog)]), 
                      yy=c(rep(0, length(which(dpoz$x<prog))), dpoz$y[which(dpoz$x>=prog)]),
                      klasa="poz",
                      prog=prog)
daneneg <- data.frame(x=dneg$x, 
                      y=dneg$y, 
                      xx=c(rep(prog, length(which(dneg$x>prog))), dneg$x[which(dneg$x<=prog)]), 
                      yy=c(rep(0, length(which(dneg$x>prog))), dneg$y[which(dneg$x<=prog)]), 
                      klasa="neg",
                      prog=prog)
dane <- rbind(danepoz, daneneg)


ggplot(dane)+
  geom_polygon(aes(x=xx, y=yy, fill=klasa), alpha=0.5)+
  geom_line(aes(x=x, y=y, color=klasa), size=1)+
  geom_vline(xintercept=prog, size=1.3, colour="black", linetype = "longdash")+
  geom_text(aes(x=prog, 
                label="\nPRÓG ODCIĘCIA", y=2), 
            colour="black", 
            angle=90, 
            text=element_text(size=11, family="serif"))+
  theme_bw()+
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=14, family="serif", colour="grey20"),
        axis.title.x = element_text(size=20, family="serif", colour="grey20"),
        legend.text = element_text(size=20, family="serif", colour="grey20"),
        legend.title = element_text(size=20, family="serif", colour="grey20"))+
  xlab("Prawdopodobieństwo należenia do klasy pozytywnej")+
  scale_fill_manual(values = c("red", "midnightblue"),
                    name="Experimental\nCondition",
                    breaks=c("ctrl", "trt1"))+
  scale_colour_manual(values = c("red", "midnightblue"),
                      name="Rodzaj obserwacji",
                      labels=c("pozytywne", "negatywne"))+
  annotate("segment", x = 0.67, xend = 0.85, y = 2, yend = 3,
           colour = "black", size=1)+
  annotate("segment", x = 0.15, xend = 0.35, y = 3, yend = 2,
           colour = "black", size=1)+
  annotate("text", x = 0.1, y = 3.05, label="TNR", family="serif", size=15)+
  annotate("text", x = 0.9, y = 3.05, label="TPR", family="serif", size=15)

####################################################################################
####################################################################################
####################################################################################

poz <- rnorm(10000, 0.7, 0.09)
sr <- rnorm(10000, 0.5, 0.12)
neg <- rnorm(10000, 0.3, 0.1)

dpoz <- density(poz)
dsr <- density(sr)
dneg <- density(neg)

prog1 <- 0.4
prog2 <- 0.6

danepoz <- data.frame(x=dpoz$x, 
                      y=dpoz$y, 
                      xx=c(rep(prog2, length(which(dpoz$x<prog2))), dpoz$x[which(dpoz$x>=prog2)]), 
                      yy=c(rep(0, length(which(dpoz$x<prog2))), dpoz$y[which(dpoz$x>=prog2)]),
                      klasa="poz",
                      prog1=prog1,
                      prog2=prog2)
danesr <- data.frame(x=dsr$x, 
                     y=dsr$y, 
                     xx=c(rep(prog1, length(which(dsr$x>prog2 | dsr$x<prog1))-1), 
                          dsr$x[which(dsr$x<=prog2 & dsr$x>=prog1)],
                          prog2), 
                     yy=c(rep(0, length(which(dsr$x>prog2 | dsr$x<prog1))-1), 
                          dsr$y[which(dsr$x<=prog2 & dsr$x>=prog1)], 
                          0), 
                     klasa="sr",
                     prog1=prog1,
                     prog2=prog2)
daneneg <- data.frame(x=dneg$x, 
                      y=dneg$y, 
                      xx=c(rep(prog1, length(which(dneg$x>prog1))), dneg$x[which(dneg$x<=prog1)]), 
                      yy=c(rep(0, length(which(dneg$x>prog1))), dneg$y[which(dneg$x<=prog1)]), 
                      klasa="neg",
                      prog1=prog1,
                      prog2=prog2)
dane <- rbind(danepoz, danesr, daneneg)


ggplot(dane)+
  geom_polygon(aes(x=xx, y=yy, fill=klasa), alpha=0.5)+
  geom_line(aes(x=x, y=y, color=klasa), size=1)+
  geom_vline(xintercept=prog1, size=1.3, colour="black", linetype = "longdash")+
  geom_text(aes(x=prog1, 
                label="\nPRÓG ODCIĘCIA NR1", y=3.5), 
            colour="black", 
            angle=90, 
            text=element_text(size=11, family="serif"))+
  geom_vline(xintercept=prog2, size=1.3, colour="black", linetype = "longdash")+
  geom_text(aes(x=prog2, 
                label="\nPRÓG ODCIĘCIA NR 2", y=3.5), 
            colour="black", 
            angle=90, 
            text=element_text(size=11, family="serif"))+
  theme_bw()+
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=14, family="serif", colour="grey20"),
        axis.title.x = element_text(size=20, family="serif", colour="grey20"),
        legend.text = element_text(size=20, family="serif", colour="grey20"),
        legend.title = element_text(size=20, family="serif", colour="grey20"))+
  xlab("Prawdopodobieństwo przynależności do klas")+
  scale_fill_manual(values = c("red", "green", "midnightblue"),
                    name="Experimental\nCondition",
                    breaks=c("ctrl", "trt1", "blabla"))+
  scale_colour_manual(values = c("red", "green", "midnightblue"),
                      name="Rodzaj obserwacji",
                      labels=c("pozytywne", "średnie","negatywne"))+
  annotate("segment", x = 0.1, xend = 0.25, y = 3, yend = 2,
           colour = "black", size=1)+
  annotate("segment", x = 0.75, xend = 0.9, y = 2, yend = 3,
           colour = "black", size=1)+
  annotate("segment", x = 0.48, xend = 0.5, y = 2.5, yend = 3.5,
           colour = "black", size=1)+
  annotate("text", x = 0.03, y = 3.1, label="TPR1", family="serif", size=15)+
  annotate("text", x = 0.5, y = 3.75, label="TPR2", family="serif", size=15)+
  annotate("text", x = 0.97, y = 3.1, label="TPR3", family="serif", size=15)



