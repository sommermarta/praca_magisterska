# install.packages("devtools")  # so we can install from GitHub
# devtools::install_github("ropensci/plotly")  # plotly is part of rOpenSci

library("plotly")
library("dplyr")
library("akima")
library("HUM")

set_credentials_file(username = "sommerm", api_key = "6gmzvehymp") 

##########################################################################################
##########################################################################################

# dane:

data(sim)
indexF=names(sim[,c(3),drop = FALSE])
indexClass=2
label=unique(sim[,indexClass])
indexLabel=label[1:3]
out=CalculateHUM_seq(sim,indexF,indexClass,indexLabel)
HUM<-out$HUM
seq<-out$seq
out=CalculateHUM_ROC(sim,indexF,indexClass,indexLabel,seq)

z <- matrix(c(out$Sp$CD15, out$Sn$CD15, out$S3$CD15), 400, 3)
s <- interp(z[,1],z[,2],z[,3], duplicate="strip")

# wykres:

data <- list(
  list(
    x = s$x,
    y = s$y,
    z = s$z,
    type = "surface"
  )
)
layout <- list(
  #title = "Mt Bruno Elevation",
  autosize = TRUE,
  width = 800,
  height = 800,
  margin = list(
    l = 20,
    r = 20,
    b = 20,
    t = 20
  )
)
py <- plotly()
response <- py$plotly(data, 
                      kwargs=list(layout=layout, 
                                  filename="roc2", 
                                  fileopt="overwrite"))
url <- response$url



