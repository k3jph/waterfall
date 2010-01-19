##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.

##  This file is largely based on bwplot.R, part of the Lattice package
##  for R.  Use the internal Lattice functions for keeping track of
##  options.
lattice.options(panel.waterfallchart = "panel.waterfallchart")
lattice.options(prepanel.waterfallchart = "prepanel.waterfallchart")
lattice.options(waterfallchart.summaryname = "Total")

`panel.waterfallchart` <-
function (x, y, box.ratio = 1, box.width = box.ratio/(1 + box.ratio),
    horizontal = FALSE, origin = 0, reference = TRUE, groups = NULL,
    summaryname = NULL, col = if (is.null(groups)) plot.polygon$col else superpose.polygon$col,
    border = if (is.null(groups)) plot.polygon$border else superpose.polygon$border,
    lty = if (is.null(groups)) plot.polygon$lty else superpose.polygon$lty,
    lwd = if (is.null(groups)) plot.polygon$lwd else superpose.polygon$lwd,
    ...)
{
    plot.polygon <- trellis.par.get("plot.polygon")
    superpose.polygon <- trellis.par.get("superpose.polygon")
    reference.line <- trellis.par.get("reference.line")
    keep <- (function(x, y, groups, subscripts, ...) {
        !is.na(x) & !is.na(y)
    })(x = x, y = y, ...)
    if (!any(keep))
        return()
    x <- as.numeric(x[keep])
    y <- as.numeric(y[keep])
    if (is.null(groups))
        if (is.null(summaryname))
            groups = rep(lattice.getOption("waterfallchart.summaryname"),
                length(x))
        else groups = rep(summaryname, length(x))
    grplst <- sort(unique(groups))
    baseline <- rep(origin, (l = length(x) + length(grplst)) +
        1)

    ##  The following block of code reorganizes the data into the final
    ##  format.  The same block is used above.
    if (horizontal) {
        data <- merge(data.frame(x, y, groups, groupsy = y)[order(groups,
            y), ], data.frame(x = rep(NA, length(grplst)), y = rep(NA,
            length(grplst)), groups = grplst, groupsy = grplst),
            all = TRUE)
        data <- data[order(data$groups, data$y), ]
        if (is.null(origin)) {
            origin <- current.panel.limits()$xlim[1]
            reference <- FALSE
        }
        if (reference)
            panel.abline(h = origin, col = reference.line$col,
                lty = reference.line$lty, lwd = reference.line$lwd)
        for (i in 1:l) {
            if (is.na(data[i, ]$y)) {
                height <- origin - (baseline[i + 1] = baseline[i])
                color <- col[2]
            }
            else {
                baseline[i + 1] <- baseline[i] + (height = data[i,
                  ]$x)
                color <- col[1]
            }
            panel.rect(y = i, x = baseline[i], col = color, lty = lty,
                lwd = lwd, height = box.width, width = height,
                just = c("left", "centre"))
        }
        for (i in 2:l - 1) panel.lines(y = c(i, i + 1), x = baseline[i +
            1], col = border, lty = lty, lwd = lwd)
    }
    else {
        data <- merge(data.frame(x, y, groups, groupsx = x)[order(groups,
            x), ], data.frame(x = rep(NA, length(grplst)), y = rep(NA,
            length(grplst)), groups = grplst, groupsx = grplst),
            all = TRUE)
        data <- data[order(data$groups, data$x), ]
        if (is.null(origin)) {
            origin <- current.panel.limits()$ylim[1]
            reference <- FALSE
        }
        if (reference)
            panel.abline(h = origin, col = reference.line$col,
                lty = reference.line$lty, lwd = reference.line$lwd)
        for (i in 1:l) {
            if (is.na(data[i, ]$y)) {
                height <- origin - (baseline[i + 1] = baseline[i])
                color <- col[2]
            }
            else {
                baseline[i + 1] <- baseline[i] + (height = data[i,
                  ]$y)
                color <- col[1]
            }
            panel.rect(x = i, y = baseline[i], col = color, lty = lty,
                lwd = lwd, width = box.width, height = height,
                just = c("centre", "bottom"))
        }
        for (i in 2:l - 1) panel.lines(x = c(i, i + 1), y = baseline[i +
            1], col = border, lty = lty, lwd = lwd)
    }
}

`prepanel.waterfallchart` <-
function (x, y, horizontal = FALSE, origin = 0, groups = NULL,
    summaryname = NULL, ...)
{
    if (any(!is.na(x) & !is.na(y))) {
        if (is.null(groups))
            if (is.null(summaryname))
                groups = rep(lattice.getOption("waterfallchart.summaryname"),
                  length(x))
            else groups = rep(summaryname, length(x))
        grplst <- sort(unique(groups))
        baseline <- rep(origin, (l = length(x) + length(grplst)))

        ##  The following block of code reorganizes the data into the final
        ##  format.  The same block is used above.
        if (horizontal) {
            data <- merge(data.frame(x, y, groups, groupsy = y)[order(groups,
                y), ], data.frame(x = rep(NA, length(grplst)),
                y = rep(NA, length(grplst)), groups = grplst,
                groupsy = grplst), all = TRUE)
            data <- data[order(data$groups, data$y), ]
            list(ylim = levels(factor(data$groupsy, levels = data$groupsy)),
                yat = sort(unique(as.numeric(data$groupsy, levels = data$groupsy))),
                xlim = {
                  for (i in 1:(l - 1)) {
                    if (!is.na(data[i, ]$x)) baseline[i + 1] <- baseline[i] +
                      data[i, ]$x else baseline[i + 1] <- baseline[i]
                  }
                  range(baseline)
                }, dx = 1, dy = 1)
        }
        else {
            data <- merge(data.frame(x, y, groups, groupsx = x)[order(groups,
                x), ], data.frame(x = rep(NA, length(grplst)),
                y = rep(NA, length(grplst)), groups = grplst,
                groupsx = grplst), all = TRUE)
            data <- data[order(data$groups, data$x), ]
            list(xlim = levels(factor(data$groupsx, levels = data$groupsx)),
                xat = sort(unique(as.numeric(data$groupsx, levels = data$groupsx))),
                ylim = {
                  for (i in 1:(l - 1)) {
                    if (!is.na(data[i, ]$y)) baseline[i + 1] <- baseline[i] +
                      data[i, ]$y else baseline[i + 1] <- baseline[i]
                  }
                  range(baseline)
                }, dx = 1, dy = 1)
        }
    }
    else list(xlim = c(NA, NA), ylim = c(NA, NA), dx = 1, dy = 1)
}

`waterfallchart` <-
function (x, data, ...)
UseMethod("waterfallchart")

`waterfallchart.formula` <-
function (x, data = NULL, groups = NULL, horizontal = FALSE,
    panel = lattice.getOption("panel.waterfallchart"), prepanel = lattice.getOption("prepanel.waterfallchart"),
    box.ratio = 2, origin = 0, ...)
{
    ocall <- sys.call(sys.parent())
    ocall[[1]] <- quote(waterfallchart)
    ccall <- match.call()
    ccall$data <- data
    ccall$panel <- panel
    ccall$prepanel <- prepanel
    ccall$box.ratio <- box.ratio

    ## Let lattice do the hard work
    ccall[[1]] <- quote(bwplot)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}
