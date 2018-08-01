make_scala_engine <- function(...) {

  rscala::scala(assign.name = "engine", serialize.output = TRUE, stdout = "", ...)
  engine <- force(engine)
  function(options) {
    code <- paste(options$code, collapse = "\n")
    output <- capture.output(invisible(engine + code))
    engine_output(options, options$code, output)
  }
}

make_sbt_engine <- function(path) {

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

      system2(system.file("runSbt.sh", package = "zstatUtils"), PATH, stdout = TRUE, stderr = TRUE)

      output <- "build.sbt created"

      jars <- get_jars_from_sbt_project(file.path(PATH, "target/pack/lib"))
      output <- c(output, "Some jars:", paste("-", basename(head(jars))))

      knitr::knit_engines$set(scala = make_scala_engine(JARs = jars))

    } else {
      stop("File is not supported.")
    }

    engine_output(options, options$code, output)
  }
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
