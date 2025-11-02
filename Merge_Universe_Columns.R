# R script (data.table)
library(data.table)

# adjust path if needed
files <- list.files(path = "~/Documents/data/universe/20251101/", pattern = "^Universe.*\\.csv$", full.names = TRUE)
#if (length(files) == 0) stop("No files found matching /mnt/data/Universe*.csv")
dts <- lapply(files, function(f) fread(f, colClasses = "character", na.strings = c("", "NA")))

# Trim column names
dts <- lapply(dts, function(dt) { setnames(dt, trimws(names(dt))); dt })

# Find columns common to all
common_all <- Reduce(intersect, lapply(dts, names))

if (length(common_all) > 0) {
  merged <- Reduce(function(x, y) {
    out <- merge(x, y, by = common_all, all = TRUE, sort = FALSE, suffixes = c("", ".dup"))
    
    # Identify duplicate-suffixed columns
    dup_cols <- grep("\\.dup$", names(out), value = TRUE)
    for (dc in dup_cols) {
      orig <- sub("\\.dup$", "", dc)
      if (orig %in% names(out)) {
        # Coalesce: prefer non-empty from original, else take dup
        out[, (orig) := fifelse(!is.na(get(orig)) & trimws(get(orig)) != "", get(orig), get(dc))]
        out[, (dc) := NULL]
      } else {
        # If no original, just rename
        setnames(out, dc, orig)
      }
    }
    out
  }, dts)
  
} else {
  merged <- rbindlist(dts, use.names = TRUE, fill = TRUE)
}

# Drop completely empty columns
is_empty_col <- function(x) all(is.na(x) | trimws(x) == "")
empty_cols <- names(merged)[vapply(merged, is_empty_col, logical(1))]
if (length(empty_cols) > 0) merged[, (empty_cols) := NULL]

# Save final clean file
fwrite(merged, "~/Documents/data/universe/20251101/Universe_merged.csv", na = "", quote = TRUE)

cat("✅ Merge completed.\n")
cat("Rows:", nrow(merged), "Cols:", ncol(merged), "\n")
cat("Dropped empty columns:", paste(empty_cols, collapse = ", "), "\n")
cat("Saved as /mnt/data/Universe_merged.csv\n")


old_univ_merged <- fread("data/Universe_merged.csv")
new_univ_merged <- fread("~/Documents/data/universe/20251101/Universe_merged.csv")

setdiff(colnames(new_univ_merged),colnames(old_univ_merged))
setdiff(colnames(old_univ_merged),colnames(new_univ_merged))

# compareDF::create_output_table(compare_df(old_univ_merged , new_univ_merged,group_col="Symbol"),output="html",
#                                file_name="comapre_Universe.html")

#library(compareDF)

#

