## Copyright (c) 2008-2010 James P. Howard, II. All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
## 1. Redistributions of source code must retain the above copyright
##    notice, this list of conditions and the following disclaimer.
## 2. Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
## 
## THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
## ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
## ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
## OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
## HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
## LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
## OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
## SUCH DAMAGE.

`waterfallplot` <-
		function (height, ...)
	UseMethod("waterfallplot")

`waterfallplot.default` <-
		function (height, width = 1, space = NULL, names.arg = NULL,
				horiz = FALSE, density = NULL, angle = 45, col = NULL, border = par("fg"),
				main = NA, sub = NA, xlab = NULL, ylab = NULL, xlim = NULL,
				ylim = NULL, xpd = TRUE, axes = TRUE, axisnames = TRUE, cex.axis = par("cex.axis"),
				cex.names = par("cex.axis"), plot = TRUE, axis.lty = 0, offset = 0,
				add = FALSE, summary = FALSE, rev = FALSE, level.lines = TRUE, ...)
{
	##  Set up the initial environment
	respaxis <- 2
	expaxis <- 1
	l <- length(height)
	if (summary == TRUE) {
		height[l + 1] = -sum(height)
		l = l + 1
	}
	if (rev == TRUE) {
		height = rev(height)
		if (summary == TRUE)
			height = -height
	}
	if (!is.null(names.arg) && length(names.arg) != l)
		stop("incorrect number of names")
	if (!is.null(names(height)))
		names.arg <- names(height)
	density.vec <- rep(density, l, length.out = l)
	angle.vec <- rep(angle, l, length.out = l)
	if (is.null(col))
		col <- "grey"
	col.vec <- rep(col, l, length.out = l)
	border.vec <- rep(border, l, length.out = l)
	
	##  Assemble the baseline and topline vectors.  Default the spacing
	##  between bars to 1/5 of the average bar width.  Create the same
	##  initial offset as present in barplot()
	width.vec <- rep(width, l, length.out = l)
	if (is.null(space))
		space <- 0.2
	space.vec <- rep(space, l + 1, length.out = l + 1)
	space.vec <- space.vec * mean(width.vec)
	leftline <- rightline <- topline <- baseline <- rep(offset, l)
	leftline[1] <- space.vec[1]
	for (i in 1:l) {
		topline[i] <- baseline[i] + height[i]
		baseline[i + 1] <- topline[i]
		rightline[i] <- leftline[i] + width.vec[i]
		leftline[i + 1] <- rightline[i] + space.vec[i + 1]
	}
	ticks.vec = rep(0, times = l)
	
	##  Wait, do we need to turns this on its side?  Barplot uses an
	##  internal function that rearranges the order of arguments.  This is
	##  a bit less hassle to do it once, though the line drawing functions
	##  will need to be checked later.
	if (horiz == TRUE) {
		oldtopline <- topline
		topline <- leftline
		leftline <- oldtopline
		oldbaseline <- baseline
		baseline <- rightline
		rightline <- oldbaseline
		respaxis <- 1
		expaxis <- 2
		for (i in 1:l) ticks.vec[i] <- (baseline[i] + topline[i])/2
	}
	else {
		for (i in 1:l) ticks.vec[i] <- (leftline[i] + rightline[i])/2
	}
	if (is.null(xlim))
		xlim <- c(min(leftline), max(rightline))
	if (is.null(ylim))
		ylim <- c(min(topline, baseline), max(topline, baseline))
	if (plot == TRUE) {
		if (add == FALSE)
			plot.new()
		plot.window(xlim = xlim, ylim = ylim, ...)
		for (i in 1:l) rect(leftline[i], baseline[i], rightline[i],
					topline[i], density = density.vec[i], angle = angle.vec[i],
					col = col.vec[i], border = border.vec[i], xpd = xpd, ...)
		if (level.lines == TRUE)
			if (horiz == TRUE)
				for (i in 1:(l - 1)) lines(c(leftline[i], leftline[i]),
							c(baseline[i], topline[i + 1]), ...)
			else for (i in 1:(l - 1)) lines(c(rightline[i], leftline[i +
											1]), c(topline[i], baseline[i + 1]), ...)
		if (!is.null(names.arg) && axisnames)
			axis(expaxis, at = ticks.vec, labels = names.arg,
					lty = axis.lty, cex.axis = cex.names, ...)
		if (axes == TRUE)
			axis(respaxis, cex.axis = cex.axis, ...)
		title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
	}
	invisible(ticks.vec)
}
