# Matching a Hospital Name to a FAC
*Ontario Hospital Name Crosswalk*

---

The two core tables support a straightforward lookup pattern. Given an unknown hospital
name string, the goal is to find the FAC it belongs to and confirm the match is reliable.

---

## The Core Pattern

```r
library(dplyr)
library(stringr)

# Load the tables once at the top of your script
name_universe <- readr::read_csv("outputs/derived/name_universe_safe.csv",
                                  show_col_types = FALSE)
fac_master    <- readRDS("outputs/fac_master.rds")

# Normalize the name you are trying to match
# (same logic as normalize_name() in code/utils/normalize.R)
normalize <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- toupper(x)
  x <- stringr::str_replace_all(x, "[^A-Z0-9]+", " ")
  x <- stringr::str_squish(x)
  x <- dplyr::na_if(x, "")
  x
}

query      <- "Humber River Regional Hospital"
query_norm <- normalize(query)

# Step 1: exact normalized match
hit <- name_universe |>
  filter(name_norm == query_norm)

hit
```

If `hit` returns one or more rows all pointing to the same FAC, the match is confirmed.
Join to `fac_master` on `fac` to get the canonical name, type, and SACtype:

```r
hit |>
  distinct(fac) |>
  left_join(fac_master, by = "fac") |>
  select(fac, canonical_name, moh_type, sactype_value)
```

---

## What the Results Mean

| Scenario | Meaning | Action |
|----------|---------|--------|
| One FAC, multiple safe aliases from different sources | Strong match | Accept |
| One FAC, single alias from one source | Weak but unambiguous | Accept with note |
| Zero rows returned | Name not in safe universe | Try fuzzy (see below) |
| Multiple FACs returned | Name collision — was marked unsafe and should not appear here | Investigate |

`name_universe_safe.csv` contains only safe aliases (`unsafe = FALSE`). A zero-row
result means the name was either never seen before or was flagged as a collision risk.

---

## Step 2: Fuzzy Match for Near-Misses

If exact match returns nothing, try Jaro-Winkler against the full safe name universe:

```r
library(stringdist)

universe_norms <- name_universe$name_norm

scores     <- stringdist::stringsim(query_norm, universe_norms, method = "jw")
best_score <- max(scores)
best_norm  <- universe_norms[which.max(scores)]

cat("Best match score:", round(best_score, 3), "\n")
cat("Best match name: ", best_norm, "\n")

# Retrieve the candidate row
name_universe |>
  filter(name_norm == best_norm) |>
  left_join(fac_master |> select(fac, canonical_name, moh_type), by = "fac")
```

**Interpreting the score:**

| Score | Interpretation |
|-------|----------------|
| >= 0.95 | Very likely the same hospital — name variant or minor typo |
| 0.88–0.94 | Probable match — review the canonical name to confirm |
| 0.75–0.87 | Possible match — manual review required |
| < 0.75 | No reliable match — treat as unknown |

The pipeline uses 0.88 as the auto-accept threshold. Below that, always verify by eye.

---

## Step 3: Verify the Match

Regardless of how the match was found, confirm it makes sense:

```r
# Pull the full alias picture for the candidate FAC
fac_of_interest <- "941"   # replace with your candidate

readRDS("outputs/fac_aliases.rds") |>
  filter(fac == fac_of_interest) |>
  select(source_system, name_raw, unsafe) |>
  arrange(source_system, name_raw) |>
  print(n = 50)
```

A good match will show the query name (or something close to it) appearing under the
same FAC across multiple source systems. If only one source shows the name and it is
a source known for network/corporate names (e.g., CIHI with `unsafe = TRUE`), treat
the match as unconfirmed.

---

## When There Is No Match

If fuzzy matching also fails, the name is genuinely unknown. Options:

1. **Check `fac_aliases.rds` including unsafe aliases** — the name may exist but have
   been flagged as a collision risk. If it matches exactly one FAC in the unsafe set,
   that FAC is a reasonable candidate but should be noted as unverified.

2. **Check `fac_master` directly** — search canonical names for partial string matches:
   ```r
   fac_master |>
     filter(stringr::str_detect(canonical_name, "HUMBER")) |>
     select(fac, canonical_name, moh_type)
   ```

3. **Add to the appropriate override file** once the correct FAC is confirmed, so future
   runs will match automatically.
