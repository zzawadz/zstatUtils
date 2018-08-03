#' Scala engine for knitr.
#'
#' @param ... params passed to \code{\link{scala}} function from rscala package.
#'
#' @export
#'
make_scala_engine <- function(..., stderr = FALSE) {

  rscala::scala(assign.name = "engine", serialize.output = TRUE, stdout = "", stderr = stderr, ...)
  engine <- force(engine)
  function(options) {
    code <- paste(options$code, collapse = "\n")
    output <- capture.output(invisible(engine + code))

    if(!is.null(options$waitForResult) && options$waitForResult > 0) {
      Sys.sleep(options$waitForResult)
      output <- c(output, capture.output(invisible(engine + 'println("___RSCALAWAIT___")')))
      output <- output[output != "___RSCALAWAIT___"]
    }

    engine_output(options, options$code, output)
  }
}


#' An sbt engine which can be used to create scala engine in knitr.
#'
#' @param path the directory in which the sbt probject will be created.
#'
#' @export
#'
make_sbt_engine <- function(path, stderr = FALSE) {

  PATH <- force(path)
  dir.create(file.path(PATH, "project"), recursive = TRUE, showWarnings = FALSE)

  function(options) {

    code <- paste(options$code, collapse = "\n")
    if(any("// plugins.sbt" == options$code)) {
      outputPath <- file.path(PATH, "project", "plugins.sbt")
      cat(file = outputPath, code, "\n")
      output <- "plugins.sbt created"
    } else if(any("// build.sbt" == options$code)) {
      outputPath <- file.path(PATH, "build.sbt")
      cat(file = outputPath, code, "\n")

      if(!check_sbt_cache(PATH)) {
        system2(
          system.file("runSbt.sh", package = "zstatUtils"),
          PATH, stdout = TRUE, stderr = TRUE
        )
      }

      output <- "build.sbt created"

      jars <- get_jars_from_sbt_project(file.path(PATH, "target/pack/lib"))
      output <- c(output, "Some jars:", paste("-", basename(head(jars))))

      knitr::knit_engines$set(scala = make_scala_engine(JARs = jars, stderr = stderr))

    } else {
      stop("File is not supported.")
    }

    engine_output(options, options$code, output)
  }
}

check_sbt_cache <- function(path) {
  cacheInfoFile <- file.path(path, ".rscinfo")
  if(!file.exists(cacheInfoFile)) cat(file = cacheInfoFile, "\n")

  allPaths <- file.path(path, c("build.sbt", "project/plugins.sbt"))
  lines <- unlist(lapply(allPaths, readLines, warn = FALSE))
  oldHash <- readLines(cacheInfoFile, warn = FALSE)[1]
  newHash <- digest::digest(lines)
  cat(file = cacheInfoFile, newHash)

  return(oldHash == newHash)
}

get_jars_from_sbt_project <- function(path) {

  jars <- dir(path, full.names = TRUE)

  jarsToRemove <- c("scala-compiler-.*\\.jar$",
                    "scala-library-.*\\.jar$",
                    "scala-reflect-.*\\.jar$",
                    "scalap-.*\\.jar$",
                    "scala-parser-combinators_.*\\.jar$",
                    "scala-xml_.*\\.jar$")
  jars <- jars[!grepl(jars, pattern = paste(jarsToRemove, collapse = "|"))]
  jars
}
