\name{waterfallplot}

\alias{waterfallplot}
\alias{waterfallchart}

\title{Waterfall Charts in R}

\description{
  Waterfall provides a traditional graphics and lattice
  implementations of waterfall plots.
}

\usage{waterfallplot(height, width=1, space=NULL, names.arg=NULL, horiz=FALSE,
           density=NULL, angle=45, col=NULL, border=par("fg"), main=NA,
           sub=NA, xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, xpd=TRUE,
           axes=TRUE, axisnames=TRUE, cex.axis=par("cex.axis"),
           cex.names=par("cex.axis"), plot=TRUE, axis.lty=0, offset = 0,
           add = FALSE, summary=FALSE, rev=FALSE, ...)
       waterfallchart(x, y, box.ratio = 1, box.width = box.ratio / (1 + box.ratio),
           horizontal = FALSE, origin = 0, reference = TRUE,
           groups = NULL, summaryname = NULL,
           col = if(is.null(groups)) plot.polygon$col
                 else superpose.polygon$col,
           border = if(is.null(groups)) plot.polygon$border
                    else superpose.polygon$border,
           lty = if(is.null(groups)) plot.polygon$lty
                 else superpose.polygon$lty,
           lwd = if(is.null(groups)) plot.polygon$lwd
                 else superpose.polygon$lwd, ...)}

	       
