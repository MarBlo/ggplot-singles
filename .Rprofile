source("renv/activate.R")

if (interactive() && Sys.getenv("RSTUDIO") == "") {
  Sys.setenv(TERM_PROGRAM = "vscode")
  if ("httpgd" %in% .packages(all.available = TRUE)) {
    options(vsc.plot = FALSE)
    options(device = function(...) {
      httpgd::hgd(silent = TRUE)
      .vsc.browser(httpgd::hgd_url(history = FALSE), viewer = FALSE)
    })
  }
}

options(
  # activate RStudio Addins on command pallet
  vsc.rstudioapi = TRUE,
  # interactive plots with {httpgd}
  vsc.use_httpgd = FALSE,
  vsc.plot = FALSE,
  # radian highlight scheme (choose what suits you)
  radian.color_scheme = "native",
  # code completion triggers
  languageserver.server_capabilities = list(
    signatureHelpProvider = list(triggerCharacters = list("(", ",", "$")),
    completionProvider = list(
      resolveProvider = TRUE, triggerCharacters = list(".", ":", "$")
    )
  )
)
