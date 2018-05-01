\name{xplain.overview}
\alias{xplain.overview}

\title{
Summarizing the content of xplain XML files
}

\description{
\code{xplain.overview} summarizes the content of an xplain XML file.
}

\usage{
xplain.overview(xml, show.text=FALSE, preserve.seq=FALSE)
}

\arguments{
  \item{xml}{
Path to the xplain XML file. Can be either a local path or an URL.
}
  \item{show.text}{
Indicates if the full interpretation/explanation texts shall be included in the summary (optional). Default: \code{FALSE}.
}
  \item{preserve.seq}{
Indicates if the overview results for the interpretation/explanation texts shall be shown in the same sequence as they appear in the XML file (optional). If \code{FALSE}, the results are sorted, e.g. by package, function, language and complexity level. Default: \code{FALSE}.
}
}


\value{
A data frame summarizing the XML file. Each row corresponds to a \code{<title>} or \code{<text>} element in the xplain XML file.\cr
The column structure is as follows:
\itemize{
\item \bold{\code{Package}}: The package to which the explained function belongs.
\item \bold{\code{Function}}: The explained function.
\item \bold{\code{Type}}: Indicates whether the element is a \code{<title>} or a \code{<text>} element.
\item \bold{\code{Language}}: Language of the element (also considering inheritance from higher-level XML elements).
\item \bold{\code{Level}}: Complexity level of the element (also considering inheritance from higher-level XML elements).
\item \bold{\code{Result object}}: Element of the explained function's return object to which the \code{<title>} or  \code{<text>} element relates (if any).
\item \bold{\code{Iteration}}: Type of iteration (if any). Value of the \code{foreach} attribute of the \code{<text>} element.
\item \bold{\code{Has R code}}: Indicates if the \code{<text>} element includes R code.
\item \bold{\code{Uses return obj.}}: Indicates if the \code{<text>} element refers to the explained function's return object.
\item \bold{\code{Text}}: The text of the respective \code{<text>} or \code{<title>} element (including R code). This column is only included if \code{show.text=TRUE}.
}
To learn more about the structure of xplain XML files, go to the \code{\link{xplain}} help page or consult the \href{http://www.zuckarelli.de/xplain/index.html}{web tutorial}.
}


\author{
Joachim Zuckarelli, \email{joachim@zuckarelli.de}
}

\seealso{
\code{\link{xplain-package}}, \code{\link{xplain}}, \code{\link{xplain.getcall}}
}

\section{More material on the internet}{
Web tutorial on how to work with xplain: \url{http://www.zuckarelli.de/xplain/index.html}\cr
xplain cheat sheet: \url{http://www.zuckarelli.de/xplain/xplain_cheatsheet.pdf}
}


\examples{
xplain.overview("http://www.zuckarelli.de/xplain/example_lm.xml")
}
