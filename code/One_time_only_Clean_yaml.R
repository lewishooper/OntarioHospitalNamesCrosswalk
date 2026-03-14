library(yaml)


#Purpose is to remove __path__: code/canonical_hospitals/.... line from yaml

suppressPackageStartupMessages({
  library(yaml)
})

YAML_DIR <- file.path("code", "canonical_hospitals")
files <- list.files(YAML_DIR, pattern = "\\.ya?ml$", full.names = TRUE)

stopifnot(length(files) > 0)

changed <- 0L

for (f in files) {
  obj <- yaml::read_yaml(f)
  
  # Remove the injected key if present
  if (".__path__" %in% names(obj)) {
    obj$`.__path__` <- NULL
    yaml::write_yaml(obj, f)
    changed <- changed + 1L
  }
}

cat("Removed .__path__ from", changed, "YAML files.\n")
