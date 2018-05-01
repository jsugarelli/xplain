###   PACKAGE XPLAIN
###
###   Author and maintainer: Joachim Zuckarelli (joachim@zuckarelli.de)
###   Version 0.1.0
###
###   Web tutorial: http://www.zuckarelli.de/xplain/index.html



getXMLFile <- function(xml, fun, package.name, package.calling) {
  # Internal function that tries to find an xplain XML file. If no xplain XML file is provided or the file provided does not exist
  # then it runs through
  #   - the path of the package of the function from which xplain() is called to find a file with the name "package_of_the_calling_function.xml",
  #   - the path of the package of the function from which xplain() is called to find a file with the name "package_of_the_explained_function.xml",
  #   - the path of the package of the explained function to find a file with the name "package_of_the_explained_function.xml",
  #   - the current path (working directory) to find a file with the name "package_of_the_explained_function.xml",
  #   - the current path (working directory) to find a file with the name "explained_function.xml".
  #
  # Args:
  #   xml:              Path to the XML file as given as xml argument to xplain().
  #   fun:              Name of the function that is being explained.
  #   package.name:     The name of the package of the explained function.
  #   package.calling:  The name of the package of the function from which xplain() is called.
  #
  # Returns: The path to the XML file or "" if no XML file was provided and none could be found or "-" if an XML file was provided but
  # couldn't be found.

  if(xml == "" || (!file.exists(xml) && !RCurl::url.exists(xml))) {
    if(xml != "" && (!file.exists(xml) && !RCurl::url.exists(xml))) xml <- "-"
    else xml <- ""
    if(package.calling != "") {
      if(file.exists(file.path(utils::installed.packages()[package.calling,"LibPath"],package.calling, paste(package.calling, ".xml", sep="")))) {
        xml <- file.path(utils::installed.packages()[package.calling,"LibPath"],package.calling, paste(package.calling, ".xml", sep=""))
      }
      else {
        if(file.exists(file.path(utils::installed.packages()[package.calling,"LibPath"],package.calling, paste(package.name, ".xml", sep="")))) {
          xml <- file.path(utils::installed.packages()[package.calling,"LibPath"],package.calling, paste(package.name, ".xml", sep=""))
        }
      }
    }
    if(xml == "" || xml == "-") {
      if(file.exists(file.path(utils::installed.packages()[package.name,"LibPath"],package.name, paste(package.name, ".xml", sep="")))) {
        xml <- file.path(utils::installed.packages()[package.name,"LibPath"],package.name, paste(package.name, ".xml", sep=""))
      }
      else {
        if(file.exists(file.path(getwd(), paste(package.name, ".xml", sep="")))) {
          xml <- file.path(getwd(), paste(package.name, ".xml", sep=""))
        }
        else {
          if(file.exists(file.path(getwd(), paste(fun, ".xml", sep="")))) {
            xml <- file.path(getwd(), paste(fun, ".xml", sep=""))
          }
        }
      }
    }
  }
  return(xml)
}



getLanguage <- function() {
  # Internal function that tries to identify the language of the user's work environment by accessing a non-existing object and
  # analysing the resulting error message.

  # Returns: Capitalized ISO country code of the language or "EN" as default if no language could be identified.

  lang <- Sys.getenv("LANGUAGE")
  if(lang == "") {
    lang <- "EN"
    # Access an object that does not exist (hopefully!) and capture the error message
    res <- tryCatch( { xhajakjkula/1 }, error = function(err) { return(err) })
    # Determine the language based on the error message
    if(res$message == "Objekt 'xhajakjkula' nicht gefunden") lang <- "DE"
    if(res$message == "objet 'xhajakjkula' introuvable") lang <- "FR"
    if(res$message == "oggetto \"xhajakjkula\" non trovato") lang <- "IT"
    if(res$message == "nie znaleziono obiektu 'xhajakjkula'") lang <- "PL"
    if(res$message == "objeto 'xhajakjkula' no encontrado") lang <- "ES"
    if(res$message == "'xhajakjkula' nesnesi bulunamadi") lang <- "TR"
  }
  return(toupper(lang))
}



getHierarchyLang <- function(h) {
  # Internal function that returns the current language from the hierarchy of languages produced by language inheritance
  # from hierarchically higher XML elements.
  #
  # Args:
  #   h:            Vector with the current hierarchy of languages (ISO codes).
  #
  # Returns: Language (ISO code) that is currently at the bottom in the hierarchy of languages, "" if no appicable language can be found.

  res <- ""
  for(i in length(h):1) {
    if(h[i] != "") {
      res <- h[i]
      break
    }
  }
  return(res)
}



getHierarchyLevel <- function(h) {
  # Internal function that returns the current complexity level from the hierarchy of complexity levels produced by complexity level inheritance
  # from hierarchically higher XML elements.
  #
  # Args:
  #   h:            Vector with the current hierarchy of complexity levels.
  #
  # Returns: Complexity level that is currently at the bottom in the hierarchy of complexity levels, -1 if no appicable complexity level can be found.

  res <- -1
  for(i in length(h):1) {
    if(h[i] != 1000) {
      res <- h[i]
      break
    }
  }
  return(res)
}



outputTitle <- function(filename="", text, title.char) {
  # Internal function that produces the output for a xplain <title>...</title> XML element.
  #
  # Args:
  #   filename:     Name of the file to print the title to; if no filename is given the output is printed to the screen.
  #   text:         Text of the title.
  #   title.char:   Character used to underline the title.
  #
  # Returns: None.

  cat(file=filename, "\n", text,"\n", sep="")
  for(b in 1:nchar(text)) {
    cat(file=filename, title.char, sep="")
  }
  cat(file=filename, "\n")
}



outputText <- function(filename="", text, res, obj.name="", foreach="", sep="\n", top=FALSE, def=FALSE, def.frame=NULL) {
  # Internal function that produces the outputs for a xplain <text>...</text> XML element (including executing R code, dissolving placeholders etc.).
  #
  # Args:
  #   filename:     Name of the file to print the text to; if no filename is given the output is printed to the screen.
  #   text:         Text (value of a <text>...</text> XML element) to process.
  #   res:          Return object of the explained function.
  #   obj.name:     Name of the element of the return object of the explained function that xplain shall run through in foreach mode.
  #   foreach:      Value of the attribute "foreach" of the <text>...</text> XML element (optional). Default: "" (no foreach mode).
  #   sep:          Separator used for concluding the output (optional). Default: "\n".
  #   top:          Indicates if in foreach mode the object to be iterated through is the return object of the explained function itself
  #                 (and not the element obj.name of the return object of the explained function).
  #   def:          Indicates if the call of outputText() is made to to process a definition (a <define>...</define> XML element) (optional). In this case, no
  #                 visual output is produced. Default: FALSE.
  #   def.frame:    Dataframe holding the definitions (optional). This dataframe is appended if def=TRUE.
  #
  # Returns:  The evaluated text.

  def.text <- ""
  # Determine the type of 'foreach' mode we're in
  if(foreach != "") {
    if(regexpr(".*[Cc][Oo][Ll][Ss].*[Rr][Oo][Ww][Ss].*", foreach) != -1) mode <- 1
    else {
      if(regexpr(".*[Rr][Oo][Ww][Ss].*[Cc][Oo][Ll][Ss].*", foreach) != -1) mode <- 2
      else {
        if(regexpr(".*[Rr][Oo][Ww][Ss].*", foreach) != -1) mode <- 3
        else {
          if(regexpr(".*[Cc][Oo][Ll][Ss].*", foreach) != -1) mode <- 4
          else
          {
            if(regexpr(".*[Ii][Tt][Ee][Mm][Ss].*", foreach) != -1) mode <- 5
          }
        }
      }
    }
  }
  else mode <- 0

  # Set the dimensions to be run through based on the 'foreach' mode
  dim1 <- c(1)
  dim2 <- c(1)
  if(mode %in% c(2,3,5)) {
    if(top == FALSE) {
      dim1 <- c(1:NROW(res[[obj.name]]))
      dim2 <- c(1:NCOL(res[[obj.name]]))
    }
    else {
      dim1 <- c(1:NROW(res))
      dim2 <- c(1:NCOL(res))
    }
  }
  if(mode %in% c(1,4)) {
    if(top == FALSE) {
      dim1 <- c(1:NCOL(res[[obj.name]]))
      dim2 <- c(1:NROW(res[[obj.name]]))
    }
    else {
      dim1 <- c(1:NCOL(res))
      dim2 <- c(1:NROW(res))
    }
  }

  for(d1 in dim1) {
    for(d2 in dim2) {
      # Split text in pieces with and without R code
      raw <- strsplit(text,"!%|%!")[[1]]
      for(z in 1:length(raw)) {
        # Replace placeholders with their definition
        pat <- "!\\*\\*\\s*([^\\*]+)\\s*\\*\\*!"
        w <- gregexpr(pat, raw[z])
        if(w[[1]][1] != -1) {
          w1 <- regmatches(raw[z],w)
          # Eliminate blanks as well as the opening and closing tags (!** and **!)
          w2 <- gsub("\\s", "", gsub("\\*\\*!", "", gsub("!\\*\\*", "", w1[[1]])))

          pat2 <- gsub("\\*\\*", "\\\\*\\\\*",w1[[1]])
          for(i in 1:length(w2)) {
            if(length(def.frame$definition[def.frame$name==w2[i]]) != 0) raw[z] <- gsub(pat2[i], def.frame$definition[def.frame$name==w2[i]], raw[z])
          }
        }

        # Replace @ and ## placeholders
        raw[z] <- gsub("@", "res", raw[z])
        raw[z] <- gsub("##", paste0("res$",obj.name), raw[z])
        raw[z] <- gsub("\\\\n", "\n", raw[z])
        if(substr(raw[z], 1, 1)=="%" && substr(raw[z], nchar(raw[z]), nchar(raw[z]))=="%") {
          # Replace $ placeholders in iterations
          if(mode == 1) raw[z] <- gsub("\\$([[:blank:]])*]", paste0(d1, "\\]"), gsub("\\[([[:blank:]])*\\$",paste0("\\[", d2), raw[z]))
          if(mode == 2) raw[z] <- gsub("\\$([[:blank:]])*]", paste0(d2, "\\]"), gsub("\\[([[:blank:]])*\\$",paste0("\\[", d1), raw[z]))
          if(mode == 3 || mode == 5) raw[z] <- gsub("\\[([[:blank:]])*\\$",paste0("\\[", d1), raw[z])
          if(mode == 4) raw[z] <- gsub("\\$([[:blank:]])*\\]",paste0(d1, "\\]"), raw[z])

          # Execute expression
          exeval <- eval(parse(text=substr(raw[z], 2, nchar(raw[z])-1)))
          if(def == FALSE) cat(exeval, file=filename, sep="", append=TRUE)
          else def.text <- paste0(def.text, exeval)
        }
        else {
          if(def == FALSE) cat(raw[z], file=filename, sep="", append=TRUE)
          else def.text <- paste0(def.text, raw[z])
        } # if xplain text is regular text (no R-code)
      } # for each component of the current xplain XML text
    } # Run through object along dimension 1
    if(def == FALSE) cat(sep, file=filename, sep="", append=TRUE)
  } # Run through object along dimension 2
  if(def == TRUE) return(def.text)
}



getPermutations <- function(s) {
  # Internal function that gives all permutations of a string in terms of capitalization.
  #
  # Args:
  #   s:            String to be permutated in terms of capitalization.
  #
  # Returns: All permutations of string s in terms of capitalization.

  vec<-c(tolower(s))
  for(pos in 1:nchar(s)) {
    for(i in 1:length(vec)) {
      vec[length(vec)+1] <- vec[i]
      substr(vec[length(vec)],pos,pos) <- substr(toupper(vec[length(vec)]),pos,pos)
    }
  }
  return(vec)
}



getAttr <- function(node, attr.name, default) {
  # Internal function that reads an attribute from an XML element. getAttr() is effectively case-insensitive as it
  # looks for all permutations of the attribute's name in terms of capitalization.
  #
  # Args:
  #   node:         XML element to read an attribute from.
  #   attr.name:    Name of the attribute.
  #   default:      Default value returned if attribute attr.name is not found in element node.
  #
  # Returns: The value of the attribute.

  v <- getPermutations(attr.name)
  for(i in 1:length(v)) {
    res <- XML::xmlGetAttr(node, v[i], default=default)
    if(res != default) break
  }
  return(res)
}



cleanText <- function(text) {
  # Internal function that recodes characters which invalid in XML element texts (>,< and & in R code).
  #
  # Args:
  #   text:         XML element text that includes R code and potentially invalid characters.
  #
  # Returns:  The original XML element text with invalid characters recoded to valid HMTL characters.

  fnd <- c("&", ">", "<")
  rpl <- c("&amp;", "&gt;", "&lt;")
  for (i in 1:3) {
    pat <- paste0("!%%.*", fnd[i], ".*(?!(!%%))%%!")
    ind <- gregexpr(pat,text, perl=TRUE)
    if(ind[[1]] != -1) {
      mat <- regmatches(text, ind)
      mat2 <- gsub(fnd[i], rpl[i], mat)
      text <- gsub(mat[[1]], mat2, text, fixed=TRUE)
    }
  }
  return(text)
}



#' @export
xplain.getcall<-function(fun) {
  # This function can be called from an xplain wrapper function. It returns a string containing the call to the explained function (fun)
  # with all arguments provided to the wrapper function. With xplain.getcall() it is very easy to write xplain wrapper functions.
  #
  # Args:
  #   fun:          Name of the function
  #
  # Returns:  String representing the call of the wrapped function, i.e. the explained function, with all arguments provided to the wrapper function.
  #           This string can then be used as the call argument for xplain().

  # Get arguments of calling function
  li <- as.list(sys.call(-1))
  cl <- paste0(fun, "(", collapse = "")
  # Build argument list for explained function
  for(i in 2:length(li)) {
    if(names(li)[i] != "") {
      if(i != 2) cl <- paste0(cl, ", ", names(li)[i], "=", li[i])
      else cl <- paste0(cl, names(li)[i], "=", li[i])
    }
    else {
      if(i != 2) cl <- paste0(cl, ", ", li[i])
      else cl <- paste0(cl, li[i])
    }
  }
  cl<-paste0(cl, ")")
  return(cl)
}



#' @export
xplain.overview <- function(xml, show.text=FALSE, preserve.seq=FALSE) {
  # Provides an overview of the content of a xplain XML file.
  #
  # Args:
  #   xml:          Path to the xplain XML file to be analyzed. Can be either a local path or an URL.
  #   show.text:    Indicates if the full interpretation/explanation texts shall be included in the result (optional). Default: FALSE.
  #   preserve.seq: Indicates if the overview results for the interpretation/explanation texts shall be shown in the same sequence as they appear in the XML file (optional).
  #                 If FALSE, the results are sorted, e.g. by package, function, language and complexity level. Default: FALSE.
  #
  # Returns:  Dataframe with summary information on the xplain XML file.

  df <- data.frame()
  obj.name <- ""

  # Get path to XML file
  if(xml == "") {
    stop(paste0("\nNo xplain XML file provided.\n"))
  }
  else {
    # Read XML file
    if(!file.exists(xml) && !RCurl::url.exists(xml)) stop(paste0("xplain XML file '", basename(xml),"' does not exist."))
    doc <- XML::xmlInternalTreeParse(xml)

    # Check if XML file is a valid xplain file
    if(length(XML::getNodeSet(doc, "//xplain"))==0 && length(XML::getNodeSet(doc, "//XPLAIN"))==0) {
      stop(paste0("The file '", basename(xml), "' is not a valid xplain XML file (does not contain the <xplain> tag)."))
    }
    else {
      query <- paste0("//xplain/package")
      p <- XML::getNodeSet(doc, query)
      if(length(p)>0) {
        def.lang <-c()
        def.level <- c()

        for(i in 1:length(p)) {
          pack <- getAttr(p[[i]], "name", default="")
          def.lang[1] <- getAttr(p[[i]], "lang", default="default")
          def.level[1] <- as.integer(getAttr(p[[i]], "level", default=1000))
          lang <- tolower(def.lang[1])
          if(def.level[1] == 1000) level <- "all"
          else level <- paste0("<=", as.character(def.level[1]))

          f <- XML::xmlChildren(p[[i]])
          for(n in 1:length(f)) {
            if(tolower(XML::xmlName(f[[n]])) == "function") {
              fun <- getAttr(f[[n]], "name", "")
              def.lang[2] <- getAttr(f[[i]], "lang", default="default")
              def.level[2] <- as.integer(getAttr(f[[i]], "level", default=1000))
              lang <- tolower(def.lang[2])
              if(def.level[2] == 1000) level <- "all"
              else level <- paste0("<=", as.character(def.level[2]))

              r <- XML::xmlChildren(f[[n]])
              for(k in 1:length(r)) {
                if(tolower(XML::xmlName(r[[k]])) == "title" || tolower(XML::xmlName(r[[k]])) == "result" || tolower(XML::xmlName(r[[k]])) == "text") {
                  def.lang[3] <- getAttr(r[[k]], "lang", default="default")
                  def.level[3] <- as.integer(getAttr(r[[k]], "level", default=1000))
                  lang <- tolower(def.lang[3])
                  if(def.level[3] == 1000) level <- "all"
                  else level <- paste0("<=", as.character(def.level[3]))
                }

                if(tolower(XML::xmlName(r[[k]])) == "title" || tolower(XML::xmlName(r[[k]])) == "text") {
                  foreach <- getAttr(r[[k]], "foreach", default="")
                  type <- tolower(XML::xmlName(r[[k]]))
                  text <- XML::xmlValue(r[[k]], ignoreComments=TRUE)
                  if(regexpr("!%%", text) != -1) contains.r <- TRUE
                  else contains.r <- FALSE
                  if(regexpr("@", text) != -1 || regexpr("##", text) != -1) uses.retobj <- TRUE
                  else uses.retobj <- FALSE
                  if(show.text == TRUE) df.add <- data.frame(pack, fun, type, lang, level, obj.name, foreach, contains.r, uses.retobj, text)
                  else df.add <- data.frame(pack, fun, type, lang, level, obj.name, foreach, contains.r, uses.retobj)
                  df <- rbind(df,df.add)
                }
                if(tolower(XML::xmlName(r[[k]])) == "result") {
                  obj.name <- getAttr(r[[k]], "name", default="")
                  if(obj.name != "") {
                    t <- XML::xmlChildren(r[[k]])
                    for(z in 1:length(t)) {
                      if(tolower(XML::xmlName(t[[z]])) == "title" || tolower(XML::xmlName(t[[z]])) == "text") {
                        def.lang[4] <- getAttr(t[[z]], "lang", default="default")
                        def.level[4] <- as.integer(getAttr(t[[z]], "level", default=1000))
                        lang <- tolower(def.lang[4])
                        if(def.level[4] == 1000) level <- "all"
                        else level <- paste0("<=", as.character(def.level[4]))
                        if(tolower(XML::xmlName(t[[z]])) == "title") type <- "title"
                        else {
                          if(tolower(XML::xmlName(t[[z]])) == "text") {
                            type <- "text"
                            foreach <- getAttr(t[[z]], "foreach", default="")
                          } # current tag is a text tag
                        }
                      } # tag is 'title' or 'text' tag
                      text <- XML::xmlValue(t[[z]], ignoreComments=TRUE)
                      if(regexpr("!%%", text) != -1) contains.r <- TRUE
                      else contains.r <- FALSE
                      if(regexpr("@", text) != -1 || regexpr("##", text) != -1) uses.retobj <- TRUE
                      else uses.retobj <- FALSE
                      if(show.text == TRUE) df.add <- data.frame(pack, fun, type, lang, level, obj.name, foreach, contains.r, uses.retobj, text)
                      else df.add <- data.frame(pack, fun, type, lang, level, obj.name, foreach, contains.r, uses.retobj)
                      df <- rbind(df,df.add)
                      length(def.lang) <- 3
                      length(def.level) <- 3
                    } # for all tags below the current 'result' tag
                  } # object referenced by the 'result' tag does indeed exist
                } # tag 'results'
                length(def.lang) <- 2
                length(def.level) <- 2
              } # for all tags below the the function tag of the relevant function
            } # current tag is a function tag for the relevant function
            length(def.lang) <- 1
            length(def.level) <- 1
          } # for all function blocks below the relevant package
        } # for all blocks in XML related to the relevant package
      } # XML contains data for relevant package
      else {
        stop(paste0("\nNo xplain information found for any package.\n"))
      }
    } # XML file found
  }
  names(df) <- c("Package", "Function", "Type", "Language", "Level", "Result object", "Iteration", "Has R code", "Uses return obj.")
  if(show.text == TRUE) names(df)[length(names(df))] <- "Text"
  if(preserve.seq == FALSE) return(df[order(df[,1], df[,2], df[,3], df[,3], df[,5], df[,6], df[,7]),])
  else return(df)
}



#' @export
xplain <- function(call, xml="", lang = "", level = -1, filename="", sep="\n", title.char="-", before=TRUE, addfun="", addfun.args="", addfun.title="") {
  # Main function of the xplain package. Interprets/explains the results of a function call based on the interpretation/explanation
  # information given in a xplain XML file.
  #
  # Args:
  #   call:         Function call to be explained/interpreted.
  #   xml:          Path to the xplain XML file containing the interpretation/explanation information (optional). Can be either a local path or an URL.
  #                 If no path is provided or the provided file does not exist then xplain() searches for a suitable XML file in various locations:
  #                 (1) in the path of the package containing the function from which xplain() was called for a file of the name "package_of_the_calling_function.xml";
  #                 (2) in the same path for a file with the name "package_of_the_explained_function.xml" (the function given in the "call" argument);
  #                 (3) in the path of the package containing the explained function for a file with the name "package_of_the_explained_function.xml";
  #                 (4) in the current working directory for a file with the name "package_of_the_explained_function.xml"; and
  #                 (5) in the current working directory for a file with the name "explained_function.xml".
  #   lang:         ISO country code of the language of the interpretations/explanations that shall be shown (optional). Default: English (EN).
  #   level:        Integer number indicating the complexity level of the interpretations/explanations that shall be shown (optional). level is cumulative:
  #                 All interpretations/explanations with a level number up to the number provided are shown. Default: -1, i.e. all interpretations/explanations
  #                 are shown.
  #   filename:     File to write the xplain() output to (optional). If no filename is provided the xplain() output is shown in the console.
  #   sep:          Separator used to separate the outputs from consecutive XML text elements (<text>...</text>) (optional). Default: "\n".
  #   title.char:   Character used for underlining titles (optional). Default: "-".
  #   before:       Indicates if the results of the call of the explained function shall be shown before the interpretations/explanations (optional). Default: TRUE,
  #                 i.e. function output is shown before the interpretations/explanations.
  #   addfun:       Names of additional functions that shall be called (e.g. summary()), without brackets (optional). It is assumed that these functions take the return
  #                 object of the explained function as their first argument. Further arguments can be specified with addfun.args. Results of additional functions are
  #                 shown right after the output of the explained function.
  #   addfun.args:  Vector of arguments (beyond the return object of the explained function) for the additional functions (optional).
  #                 Example: addfun.args = "trim = 0, na.rm = FALSE". Argument must be of the same length as addfun; so addfun.args must be "" if the respective
  #                 additional function does not take any additional arguments.
  #   addfun.title: Vector of titles that will be shown as headers to the outputs of the addional functions (optional).Argument must be of the same length as addfun;
  #                 so addfun.args must be "" if the respective the output of the additional function shall have no title.

  tag.open <- "!%%"
  tag.close <- "%%!"

  # Identify function and package name of the function to be explained
  fun <- substr(call, 1, regexpr("\\(", call)-1)
  package.name <- utils::packageName(environment(get(fun)))

  # Identify function and package name of the function calling xplain
  if(sys.nframe()!=1) {
    calling.fun <- deparse(sys.calls()[[sys.nframe()-1]])
    if(regexpr(".*\\(", calling.fun)[[1]] != -1) {
      calling.fun <- regmatches(calling.fun, regexpr(".*\\(", calling.fun))[[1]]
      calling.fun <- strtrim(calling.fun, nchar(calling.fun) - 1)
    }
    package.calling <- utils::packageName(environment(get(calling.fun)))
    if(is.null(package.calling)) package.calling <- ""
  }
  else
  {
    calling.fun <- ""
    package.calling <- ""
  }

  # Get path to XML file
  xml <- getXMLFile(xml, fun, package.name, package.calling)

  if(xml == "" || xml == "-") {
    if(xml == "") stop(paste0("\nNo xplain XML file for function '", fun, "' found.\n"))
    if(xml == "-") stop(paste0("xplain XML file '", basename(xml),"' does not exist."))
  }
  else {

    # Identify language of user's work environment
    if(lang == "") {
      lang <- getLanguage()
    }

    # Read XML file

    text <- readr::read_file(xml)
    text <- cleanText(text)
    doc <- XML::xmlInternalTreeParse(text, asText=TRUE)

    # Process call of function to be explained
    res <- eval(parse(text=call))

    if(before == TRUE) {
      methods::show(res)
      for(i in 1:length(addfun)) {
        if(addfun[1] != "") {
          if(length(addfun.title) == length(addfun)) {
            if(addfun.title[i] != "") outputTitle(text=addfun.title[i], title.char = title.char)
          }
          if(length(addfun.args) == length(addfun)) {
            if(addfun.args[i] != "") methods::show(eval(parse(text=paste0(addfun[i],"(res, ", addfun.args[i],")"))))
            else methods::show(eval(parse(text=paste0(addfun[i],"(res)"))))
          }
          else methods::show(eval(parse(text=paste0(addfun[i],"(res)"))))
        }
      }
    }

    # Check if XML file is a valid xplain file
    if(length(XML::getNodeSet(doc, "//xplain"))==0 && length(XML::getNodeSet(doc, "//XPLAIN"))==0) {
      stop(paste0("The file '", basename(xml), "' is not a valid xplain XML file (does not contain the <xplain> tag)."))
      invisible(res)
    }
    else {
      query <- paste0("//xplain/package[@name='", package.name, "']")
      p <- XML::getNodeSet(doc, query)
      if(length(p)>0) {
        def.lang <-c()
        def.level <- c()

        def.frame <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(def.frame) <- c("name", "definition")

        for(i in 1:length(p)) {
          def.lang[1] <- getAttr(p[[i]], "lang", default="")
          def.level[1] <- as.integer(getAttr(p[[i]], "level", default=1000))

          f <- XML::xmlChildren(p[[i]])
          for(n in 1:length(f)) {
            if(tolower(XML::xmlName(f[[n]])) == "define") {
              if(nrow(def.frame[def.frame$name == getAttr(f[[n]], "name", ""),]) == 0) {
                df <- data.frame(getAttr(f[[n]], "name", ""), outputText(text=XML::xmlValue(f[[n]], ignoreComments=TRUE), res=res, def=TRUE, def.frame=def.frame))
                colnames(df) <- c("name", "definition")
                def.frame <- rbind(def.frame, df)
              }
            }
            if(tolower(XML::xmlName(f[[n]])) == "function" && getAttr(f[[n]], "name", "") == fun) {
              def.lang[2] <- getAttr(f[[i]], "lang", default="")
              def.level[2] <- as.integer(getAttr(f[[i]], "level", default=1000))

              r <- XML::xmlChildren(f[[n]])
              for(k in 1:length(r)) {
                if(tolower(XML::xmlName(r[[k]])) == "define") {
                  if(nrow(def.frame[def.frame$name == getAttr(r[[k]], "name", ""),]) == 0) {
                    df <- data.frame(getAttr(r[[k]], "name", ""), outputText(text=XML::xmlValue(r[[k]], ignoreComments=TRUE), res=res, def=TRUE, def.frame=def.frame))
                    colnames(df) <- c("name", "definition")
                    def.frame <- rbind(def.frame, df)
                  }
                }
                if(tolower(XML::xmlName(r[[k]])) == "title" || tolower(XML::xmlName(r[[k]])) == "result" || tolower(XML::xmlName(r[[k]])) == "text") {
                  def.lang[3] <- getAttr(r[[k]], "lang", default="")
                  def.level[3] <- as.integer(getAttr(r[[k]], "level", default=1000))
                }

                if(tolower(XML::xmlName(r[[k]])) == "title") {
                  if(tolower(getHierarchyLang(def.lang)) == tolower(lang) && (getHierarchyLevel(def.level) <= level || level==-1)) {
                    text <- XML::xmlValue(r[[k]], ignoreComments=TRUE)
                    outputTitle(filename, text, title.char)
                  }
                }
                if(tolower(XML::xmlName(r[[k]])) == "text") {
                  if(tolower(getHierarchyLang(def.lang)) == tolower(lang) && (getHierarchyLevel(def.level) <= level || level==-1)) {
                    text <- XML::xmlValue(r[[k]], ignoreComments=TRUE)
                    foreach <- getAttr(r[[k]], "foreach", default="")
                    outputText(filename, text, res, foreach=foreach, sep=sep, top=TRUE, def.frame=def.frame)
                  }
                }
                if(tolower(XML::xmlName(r[[k]])) == "result") {
                  obj.name <- getAttr(r[[k]], "name", default="")
                  if(obj.name != "" && obj.name %in% names(res)) {
                    t <- XML::xmlChildren(r[[k]])
                    for(z in 1:length(t)) {
                      if(tolower(XML::xmlName(t[[z]])) == "define") {
                        if(nrow(def.frame[def.frame$name == getAttr(z[[z]], "name", ""),]) == 0) {
                          df <- data.frame(getAttr(t[[z]], "name", ""), outputText(text=XML::xmlValue(t[[z]], ignoreComments=TRUE), res=res, def=TRUE, def.frame=def.frame))
                          colnames(df) <- c("name", "definition")
                          def.frame <- rbind(def.frame, df)
                        }
                      }
                      if(tolower(XML::xmlName(t[[z]])) == "title" || tolower(XML::xmlName(t[[z]])) == "text") {
                        def.lang[4] <- getAttr(t[[z]], "lang", default="")
                        def.level[4] <- as.integer(getAttr(t[[z]], "level", default=1000))
                        if(tolower(getHierarchyLang(def.lang)) == tolower(lang) && (getHierarchyLevel(def.level) <= level || level==-1)) {
                          if(tolower(XML::xmlName(t[[z]])) == "title") {
                            text <- XML::xmlValue(t[[z]], ignoreComments=TRUE)
                            outputTitle(filename, text, title.char)
                          }
                          else {
                            if(tolower(XML::xmlName(t[[z]])) == "text") {
                              text <- XML::xmlValue(t[[z]], ignoreComments=TRUE)
                              # Text validity check
                              if(length(regmatches(text, gregexpr(tag.open, text))[[1]]) > length(regmatches(text, gregexpr(tag.close, text))[[1]])) {
                                invisible(res)
                                stop(paste0("XML syntax error: You have more opening R code tags (", tag.open, ") than closing tags (", tag.close,")."))
                              }
                              else {
                                if(length(regmatches(text, gregexpr(tag.open, text))[[1]]) < length(regmatches(text, gregexpr(tag.close, text))[[1]])) {
                                  invisible(res)
                                  stop(paste0("XML syntax error: You have more closing R code tags (", tag.close, ") than opening tags (", tag.open,")."))
                                }
                                else {
                                  foreach <- getAttr(t[[z]], "foreach", default="")
                                  outputText(filename, text, res, obj.name, foreach, sep, def.frame=def.frame)
                                } # if R-code tags are syntactically correct
                              } # if R-code tags are syntactically correct
                            } # current tag is a text tag
                          } # current tag is no title tag
                        } # tag is 'title' or 'text' tag
                        length(def.lang) <- 3
                        length(def.level) <- 3
                      }
                    } # for all tags below the current 'result' tag
                  } # object referenced by the 'result' tag does indeed exist
                } # tag 'results'
                length(def.lang) <- 2
                length(def.level) <- 2
              } # for all tags below the the function tag of the relevant function
            } # current tag is a function tag for the relevant function
            length(def.lang) <- 1
            length(def.level) <- 1
          } # for all function blocks below the relevant package
        } # for all blocks in XML related to the relevant package
      } # XML contains data for relevant package
      else {
        stop(paste0("\nNo xplain information found for package '", package.name, "' in file '", xml, "'.\n"))
        invisible(res)
      }
      if(before == FALSE) {
        methods::show(res)
        for(i in 1:length(addfun)) {
          if(addfun[1] != "") {
            if(length(addfun.title) == length(addfun)) {
              if(addfun.title[i] != "") outputTitle(text=addfun.title[i], title.char = title.char)
            }
            if(length(addfun.args) == length(addfun)) {
              if(addfun.args[i] != "") methods::show(eval(parse(text=paste0(addfun[i],"(res, ", addfun.args[i],")"))))
              else methods::show(eval(parse(text=paste0(addfun[i],"(res)"))))
            }
            else methods::show(eval(parse(text=paste0(addfun[i],"(res)"))))
          }
        }
      }
      invisible(res)
    } # XML file found
  }
}
