\name{xplain}
\alias{xplain}

\title{
Showing interpretation information for the results of a function
}

\description{
Interprets/explains the results of a function call. Main function of the \pkg{xplain} package.
}

\usage{
xplain(call, xml="", lang = "", level = -1, filename="", sep="\n", title.char="-",
    before=TRUE, addfun="", addfun.args="", addfun.title="")
}

\arguments{
  \item{call}{
  Function call (as string) to be explained/interpreted.
}
  \item{xml}{
Path to the xplain XML file containing the interpretation/explanation information (optional). Can be either a local path or an URL. See below for more details on how \code{xplain()} tries to find xplain XML files if no file is specified with \code{xml}.
}
  \item{lang}{
  ISO country code of the language of the interpretations/explanations that shall be shown (optional). If none is specified, \code{xplain()} tries to determine the language of the user's current work environment. If that is not possible then English is taken as defualt value (same effect as \code{lang="EN"}).
}
  \item{level}{
Integer number indicating the complexity level of the interpretations/explanations that shall be shown (optional). \code{level} is cumulative: All interpretations/explanations with a level number up to the number provided are shown. Default: -1, i.e. all interpretations/explanations are shown.
}
  \item{filename}{
File to write the \code{xplain()} output to (optional). If no file name is provided the \code{xplain()} output is shown in the console.
}
  \item{sep}{
Separator used to separate the outputs from consecutive XML text elements (<text>...</text>) (optional). Default: \code{"\\n"}.
}
  \item{title.char}{
Character used for underlining titles (optional). Default: \code{"-"}.
}
	\item{before}{
Indicates if the results of the call of the explained function shall be shown before the xplain interpretations/explanations, or after (optional). Default: \code{TRUE}, i.e. function output is shown before the interpretations/explanations.
}
	\item{addfun}{
Vector of names of additional functions that shall be called (e.g. \code{summary()}), without brackets (optional). It is assumed that these functions take the return object of the explained function as their first argument. Further arguments can be specified with \code{addfun.args}. Results of the additional functions are shown right after the output of the explained function.
}
  \item{addfun.args}{
Vector of arguments (apart from the return object of the explained function) for the additional functions (optional).
Example: \code{addfun.args = "trim = 0, na.rm = FALSE"}. Argument must be of the same length as \code{addfun}; \code{addfun.args} must be \code{""} (empty string) if the respective additional function does not take any additional arguments.
}
  \item{addfun.title}{
Vector of titles that will be shown as headers to the outputs of the addional functions (optional). Argument must be of the same length as \code{addfun}; \code{addfun.args} must be \code{""} (empty string) if the respective the output of the additional function shall have no title.
}
}

\details{


\code{xplain} interprets/explains the results of a function call (argument \code{call}) by using the information provided in the xplain XML file specified by the \code{xml} argument.


\subsection{1. xplain XML files}{
xplain XML files follow a simple structure (here an example for the \code{lm()} function from the \code{stats} package):\cr


<xml>\cr
____<xplain>\cr
________<package name = "stats">\cr
____________<function name = "lm">\cr
________________<title>...</title>\cr
________________<text>...</text>\cr
________________<result name = "coefficients">\cr
____________________<title>...<title>\cr
____________________<text>...</text>\cr
________________</result>\cr
____________</function>\cr
________</package>\cr
____</xplain>\cr
</xml>\cr

\code{<title>} elements contain plain text and can be used to structure the output of \code{xplain()}. They are underlined by the character given in the \code{title.char} argument.

\code{<text>} elements contain the actual explanations and interpretations. They may consist of plain text as well as R code. R code must be enclosed by special opening and closing tags like this: \code{!\%\% Here comes the R code \%\%!}. The placeholder \code{@} can be used to access the explained function's return object, e.g. \code{!\%\% summary(@) \%\%!}.

With a \code{<result>} block you can interpret specific elements of the explained function's return object. The element of the current \code{<result>} block can be accessed with the \code{##} placeholder from within an R code section delimited by \code{!\%\%} and \code{\%\%!}. Example: \code{<text> The mean is: !\%\% mean(##) \%\%!</text>}.

If you use certain R code expressions multiple times and want to save typing effort and reduce error-proness, you can work with a \code{<define>} block. Like a \code{<text>} block, \code{<define>} can encompass both, plain text and R code, e.g.\cr
\code{<define name="my.summary">The summary is: !\%\% summary(@) \%\%! </define>}. After having defined an expression this way, you can call it from within an R code section by using its name and placing it between the special placeholder tags \code{!**} and \code{**!}, like \code{<text>Let us have a lookt at the summary: !** my.summary **! </text>}.
}

Sometimes you will want to apply a \code{<text>} block not only to one element of the explained function's return object. Consider, for example, the case in which the return object contains a vector and you want to run through each element of that vector. In this case, you can use a \code{<text>} element with the \code{foreach} attribute, e.g. \code{<text foreach="rows">}. The attribute's value defines how xplain iterates over the object. Possible values are \code{"rows"}, \code{"columns"}, \code{"rows, columns"}, \code{"columns, rows"} and \code{"items"} for list items. Within R code included in your \code{<text>} block you can then refer to the index of the current object with the \code{$} placeholder, e.g. \code{!\%\% The current element is: @$coefficients[$,1] \%\%!}. If two different indices are in play (e.g. when \code{foreach="rows, columns"}) then you can work with two index placeholders \code{$}, e.g. \code{coefficients[$,$]}.\cr
Because \code{xplain()} needs to know which object to iterate over, you can use \code{<text>}  with the \code{foreach} attribute only form within a \code{<result>} block.

\code{xplain()} can access XML files both locally and from the internet. xplain XML fiels are not case-sensitive.


\subsection{2. XML Attributes}{
\code{<package>}, \code{<function>}, \code{<result>} and \code{<define>} blocks always need a \code{name} attribute.

\code{<title>} and \code{<text>} blocks can have \code{lang} and \code{level} attributes, for the language and the complexity level of the explanations, respectively. \code{lang} is an ISO country code and \code{level} an integer number (for details, see the explanantion of the corresponding arguments of \code{xplain()} above). The values of these two attributes are inherited from higher-level XML elements, e.g. from \code{<package>} or \code{<function>}. Attributes defined at lower levels (e.g. in an individual \code{<text>} element) overrule these inherited attributes.
}

\subsection{3. Search paths from xplain XML files}{
If no path is provided with the \code{xml} argument of \code{xplain()} or the provided file does not exist then \code{xplain()} searches for a suitable XML file in various locations:\cr
\enumerate{
\item in the path of the package containing the function from which \code{xplain()} was called for a file of the name "package_of_the_calling_function.xml";\cr
\item in the same path for a file with the name "package_of_the_explained_function.xml" (the function given in the \code{call} argument);\cr
\item in the path of the package containing the explained function for a file with the name "package_of_the_explained_function.xml";\cr
\item in the current working directory for a file with the name "package_of_the_explained_function.xml"; and\cr
\item in the current working directory for a file with the name "explained_function.xml".
}
}

\subsection{4. More information on xplain XML files}{
For more details on the structure of xplain XML files, please consult the web tutorial on \url{http://www.zuckarelli.de/xplain/index.html}.
}
}

\value{
\code{xplain()} returns the return value of the explained function call (argument \code{call}) as if the function were called without \code{xplain()}. The interpretation/explanation information is either shown on the screen or written to a file (depending on the \code{filename} argument).
}


\author{
Joachim Zuckarelli, \email{joachim@zuckarelli.de}
}


\seealso{
\code{\link{xplain-package}}, \code{\link{xplain.overview}}, \code{\link{xplain.getcall}}
}

\section{More material on the internet}{
Web tutorial on how to work with xplain: \url{http://www.zuckarelli.de/xplain/index.html}\cr
xplain cheat sheet: \url{http://www.zuckarelli.de/xplain/xplain_cheatsheet.pdf}
}


\examples{

library(car)

xplain(call="lm(education ~ young + income + urban, data=Anscombe)",
  xml="http://www.zuckarelli.de/xplain/example_lm.xml")
}
