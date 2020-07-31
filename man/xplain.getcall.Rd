\name{xplain.getcall}
\alias{xplain.getcall}

\title{
Generating function calls for xplain wrapper functions
}

\description{
This function returns a string containing the call to the explained function to be used in an xplain wrapper function.
}

\usage{
xplain.getcall(fun)
}

\arguments{
\item{fun}{Name of the explained function as string}
}


\details{
\code{xplain.getcall()} can be called from an xplain wrapper function. It returns a string containing the call to the explained function with all arguments provided to the wrapper function. With xplain.getcall() it is very easy to write xplain wrapper functions.

Wrapper functions are an elegant way to provide access to xplain interpretation/explanation information. The wrapper function takes the sames arguments as the explained function and then calls \code{xplain()}. \code{xplain.getcall()} identifies the arguments provided to the wrapper function and returns the complete call of the explained function as a string which can then be handed over to \code{xplain()} as argument \code{call}.

The Examples section illustrates how to build a wrapper function using \code{xplain.getcall()}.
}

\value{
The full call of the explained function as a string, including the arguments provided by the user to the wrapper function.
}


\author{
Joachim Zuckarelli, \email{joachim@zuckarelli.de}
}

\seealso{
\code{\link{xplain-package}}, \code{\link{xplain}}, \code{\link{xplain.overview}}
}

\section{More material on the internet}{
Web tutorial on how to work with xplain: \url{https://www.zuckarelli.de/xplain/index.html}\cr
xplain cheat sheet: \url{https://www.zuckarelli.de/xplain/xplain_cheatsheet.pdf}
}


\examples{
library(car)

xml.path <- system.file("", "example_lm.xml", package = "xplain")

# Example of a wrapper function for lm()
lm.xplain <- function(formula, data, subset, weights, na.action,
                      method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
                      singular.ok = TRUE, contrasts = NULL, offset, ...) {
  call<-xplain.getcall("lm")
  xplain(call, xml=xml.path)
}

lm.xplain(education ~ young + income + urban, data=Anscombe)

}
