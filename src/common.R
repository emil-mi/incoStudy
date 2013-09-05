library(lattice)
library(grid)
library(xtable)
library(rstan)
library(ggplot2)
library(latticeExtra)
library(coin)
library(boot)
library(bootstrap)
library(MASS)
library(KernSmooth)
library(ks)
library(reshape2)
library(plyr)

graph.bg<-"white"

styleGraph<-function (x, ..., ylab = expression(NULL), xlab = expression(NULL), 
          par.settings = theEconomist.theme(with.bg = with.bg, box = "transparent"), 
          with.bg = FALSE, par.strip.text = list(font = 2)) 
{
  ans <- x
  title <- ans$main
  if (is.null(title)) 
    title <- ans$ylab
  if (is.null(title)) 
    title <- ans$ylab.default
  ans <- update(ans, main = title, ylab = ylab, 
                xlab = xlab, par.settings = par.settings, par.strip.text = par.strip.text, 
                between = list(x = 0.8, y = 0.8), scales = list(y = list(axs = "i", 
                                                                         alternating = 2)), skip.boundary.labels = 0, lattice.options = list(layout.widths = list(axis.left = list(x = 0, 
                                                                                                                                                                                   units = "char"), axis.right = list(x = 6, units = "char"))))
  ans$axis <- theEconomist.axis
  ans$xscale.components <- xscale.components.subticks
  ans$call <- match.call()
  ans
}

