#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(cli)
})

source("SCRIPTS/pipeline_runtime_helpers.R")

root <- find_project_root()
setwd(root)

render_audit <- safe_shell("quarto", c("render", "REPORTS/auditoria_integral_rcbpa_arequipa.qmd", "--to", "html"), wd = root)
if (render_audit$status != 0L) cli::cli_abort("Fallo el render del informe de auditoria integral.")

portal_src <- project_path(root, "REPORTS", "portal_rcbpa_src")
render_portal <- safe_shell("quarto", c("render", portal_src), wd = root)
if (render_portal$status != 0L) cli::cli_abort("Fallo el render del portal Quarto.")

cli::cli_alert_success("Auditoria integral y portal renderizados.")
