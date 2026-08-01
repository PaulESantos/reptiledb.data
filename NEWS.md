# reptiledb.data 0.0.1

* **New Dataset**: Added `reptiledb_062026` containing 14,719 rows corresponding to the June 2026 snapshot from The Reptile Database.
* **Online Data Retrieval**: Added `fetch_latest_reptile_data()` to dynamically download, clean, and cache the latest checklist from The Reptile Database server at runtime.
* **Cleaning Pipeline Export**: Exported `clean_reptile_data()` for cleaning raw checklist data frames.
* **Automation & Maintenance**: Added `data-raw/update_package_data.R` script for maintainer updates and `.github/workflows/update-data.yml` for scheduled monthly automatic checking.
