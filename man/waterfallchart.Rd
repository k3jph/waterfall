\name{waterfallchart}
\alias{waterfallchart}
\alias{waterfallchart.formula}
\alias{panel.waterfallchart}
\alias{prepanel.waterfallchart}
\title{Waterfall Charts in R using Lattice}

\description{Creates a waterfall chart using the 'Lattice' package.}

\usage{
waterfallchart(x, data, ...)

\method{waterfallchart}{formula}(x, data=NULL, groups=NULL,
horizontal=FALSE, panel=lattice.getOption("panel.waterfallchart"),
prepanel=lattice.getOption("prepanel.waterfallchart"), box.ratio = 2,
origin = 0, ...) 
}

\arguments{
  \item{x}{a formula describing the form of conditioning plot. The
    formula is generally of the form 'y ~ x | g1 * g2 * ...', indicating
    that plots of 'y' (on the y axis) versus 'x' (on the x axis) should be
    produced conditional on the variables 'g1, g2, ...'. However, the
    conditioning variables 'g1,g2,...' may be omitted.
  }

  \item{data}{
    a data frame containing values (or more precisely, anything that is
    a valid 'envir' argument in 'eval', e.g. a list or an environment)
    for any variables in the formula, as well as 'groups' and 'subset'
    if applicable. If not found in 'data', or if 'data' is unspecified,
    the variables are looked for in the environment of the formula.
  }

  \item{groups}{
    a variable or expression to be evaluated in the data frame specified
    by 'data', expected to act as a grouping variable within each panel,
    typically used to distinguish different groups by varying graphical
    parameters like color and line type.  Unlike with the
    \link{barchart} function, groups specifies where subtotals columns,
    should appear.  There is a subtotal created for each group
    specified.  If no groups are given, a summary column is still
    reported.
  }

  \item{horizontal}{
    This argument is used to process the arguments to these high level
    functions, but more importantly, it is passed as an argument to the
    panel function, which is supposed to use it as appropriate.
  }
  
  \item{panel}{
    This draws the actual plot after \link{bwplot} has done the
    difficult work of processing the formula.
  }

  \item{prepanel}{
    This function returns the \link{bwplot} information on the number of
    columns to display and where to place labels.
  }

  \item{box.ratio}{
    specifies the ratio of the width of the rectangles to the
    interrectangle space.
  }

  \item{origin}{
    initial offset relative to the x axis.  The value serves as the
    logical starting point for the first column and any summary	column.
    Defaults to 0.
  }

  \item{...}{further arguments}
  
}

\details{
  This function closely mimics the \link{barchart} interface, but
  provides a type of chart called a waterfall plot, showing how multiple
  subvalues contirbute to a total sum.
  
  The bulk of the work is actually processed in \link{bwplot} which
  defines where tickmarks and other information outside the plot
  itself.  Only a formula method is provided.
}

\references{
  James Howard, FIXME.
  
  Andrew Jaquith, \emph{Security Metrics: Replacing Fear, Uncertainty,
    and Doubt} (Boston:  Addison-Wesley Professional, 2007), 170-172.
 
  Ethan M. Rasiel, \emph{The McKinsey Way: Using the Techniques of the
    World's Top Strategic Consultants to Help You and Your Business}
  (New York:  McGraw-Hill, 1999), 113-118.   
}

\seealso{
  \code{\link{barchart}}, \code{\link{waterfallplot}}
}

\examples{
data(rasiel)
data(jaquith)
waterfallchart(value~label, data=rasiel, groups=subtotal)
waterfallchart(factor~score, data=jaquith)
}

\keyword{hplot}
  
