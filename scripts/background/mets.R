#using MetBrewer
#https://github.com/BlakeRMills/MetBrewer
#https://www.metmuseum.org/art/collection/search/644324
#^this werk particularly moving

library(MetBrewer)

#for contrast of 2 things (with light and dark shade of each)

met_vg <- function (x) {
  set.seed(92)
  vg <- c()
  for (i in 1:4) {
  vg[i] <- met.brewer("VanGogh1", 4)[i]
  }
  return(vg[x])
}

met_morg <- function (x) {
  set.seed(92)
  morg <- c()
  for (i in 1:4) {
    morg[i] <- met.brewer("Morgenstern", 6)[i]
  }
  return(morg[x])
}

#archambault pull (for groups NOT along gradient)
met_jodi <- function (x,ng) {
  set.seed(92)
  sample(met.brewer("Archambault", n=ng)[1:ng],ng)
  met.brewer("Archambault", n=ng)[1:ng][x]
}

##9 colors for villages
##https://sashamaps.net/docs/resources/20-colors/

rgb9 <- data.frame(r = c(230,60,255,40,245,145,70,240,210), #0 bumped 40
               rd = c(230,60,255,40,245,145,70,240,210) - 40,
               b = c(25,180,225,130,130,30,240,50,245) ,
               g = c(75,75,25,200,48,180,240,230,60)
) /255

lc9 <- function (x) {rgb(rgb9$r[x],rgb9$b[x],rgb9$g[x])}
dc9 <- function (x) {rgb(rgb9$rd[x],rgb9$b[x],rgb9$g[x])}

#hex9
#replaced #bfef45 with #b6e376
#replaced #42d4f4 with #b0e6ff

hex9 <- c("#800000", "#ffd8b1", "#ffe119",
          "#b6e376", "#3cb44b", "#b0e6ff", 
          "#4363d8","#dcbeff", "#f032e6")

lhex9 <- function(x) {hex9[x]}

#set colours for correlation gradient

rgb2hex <- function(x){
  hex <- as.hexmode(x)
  hex <- as.character(hex)
  #   hex <- substr(hex,2,3)
  hex <- matrix(hex, nrow = nrow(x), ncol = ncol(x))
  hex <- apply(hex,1, function(x){paste0(x,collapse = '')})
  hex <- paste0('#',hex)
  hex
}

col2hex <- function(col){
  rgb <- col2rgb(col)
  hex <- rgb2hex(t(rgb))
  hex
}

colorInterpolate <- function(val, color=cln, valRange=range(val)){
  appx <- approx(valRange, seq(0, 1, length.out = length(valRange)), val, yleft = 0, yright = 1)
  cc <- colorRamp(color)(appx$y)
  cc <- rgb2hex(floor(cc))
  cc
}

cln <- c("#67a9cf","#f7f7f7","#ef8a62")
ColorRamp <- colorInterpolate(1:100, color = cln)

#if you need levels
#min <- -1
#max <- 1
#ColorLevels <- seq(min, max, length=length(ColorRamp))

