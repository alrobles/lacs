# Analysis: Download Functions and Reusable Logic in `alrobles/lacs`

## Executive Summary

The `lacs` repository is an **R package for Literature Abstract Classification System (LACS)** — specifically designed to classify ecological/parasite-host interaction research papers. It **does not contain any functions for downloading papers** from PLOS, arXiv, or similar sources. Instead, it processes pre-downloaded abstract data. However, there are several reusable components in the data ingestion and processing pipeline worth extracting.

---

## 1. Repository Structure

The package lives entirely in `R/` (25 files). Scientific paper data is stored in pre-computed `.rda` binary files under `data/`.

---

## 2. External Data Sources Referenced (But Not Implemented in Code)

The documentation for the bundled datasets names the original data sources:

**`R/hp3_abstracts.R` (lines 1–29):**
- 710 abstracts retrieved from **PubMed via the Entrez API** (by PMID)
- Also sourced from **ZOVER** and **GMPD** (parasite-host interaction databases)
- Curated versions archived on Zenodo: `https://doi.org/10.5281/zenodo.4435127`

**`R/lacsSample.R` (lines 1–16):**
- Positive class from **ZOVER** + **GMPD** databases
- Negative/unknown class from the **Crossref API** (random abstracts)
- 600 total abstracts, binary-labeled

> **There is no R code in this repository that calls PubMed, Crossref, arXiv, PLOS, or ZOVER APIs.** The data retrieval was done externally and the results stored as `.rda` files.

---

## 3. Reusable Data Ingestion & Validation Logic

### A. `validate_abstracts()` — Schema Validation

**File:** `R/validate_abstracts.R`, lines 11–32
**Signature:** `validate_abstracts(abstracts_df)`

Enforces a strict 4-column schema on any incoming data frame:
- Required columns: `doi`, `title`, `abstract`, `class`
- Allowed class values: `"possitive"` or `"unknown"` (note the typo in the package)
- Filters out rows with invalid class labels
- **Reusability:** This pattern (column-presence check + value allowlist + filter) is directly portable to any ingestion pipeline receiving paper metadata from an API.

### B. `new_abstracts()` — Data Object Constructor

**File:** `R/new_abstracts.R`, lines 19–30
**Signature:**

```r
new_abstracts(doi = character(), title = character(),
              abstract = character(), class = character())
```

Creates a typed `tibble` with S3 class `"abstracts"`. This is the canonical internal representation for all paper metadata. The four-field schema (`doi`, `title`, `abstract`, `class`) is the interface contract the rest of the package depends on.

### C. `get_abstracts()` — Unified Entry Point

**File:** `R/get_abstracts.R`, lines 10–18
**Signature:** `get_abstracts(x = NULL)`

Composes `validate_abstracts()` → `new_abstracts()` into a single call. This is the function to call when bringing in external data; any data frame matching the schema can be piped through here to produce a validated, typed abstracts object.

---

## 4. Text Parsing and Normalization

### D. `abstracts2text()` — UTF-8 Encoding

**File:** `R/abstracts2text.R`, lines 13–26
**Signature:** `abstracts2text(abstracts = NULL)`

Accepts either an `abstracts` object or a raw character vector, applies `utf8::utf8_encode()` and `utf8::utf8_format()` for normalization. This is the standard entry point for turning structured records into a clean character vector ready for NLP.

---

## 5. Feature Extraction and Organization

### E. `get_vocabulary()` — Vocabulary Construction

**File:** `R/get_vocabulary.R`, lines 15–44
**Signature:**

```r
get_vocabulary(abstracts, term_count_min = 2,
               doc_proportion_min = 0, doc_proportion_max = 1)
```

Uses `text2vec::itoken()` for tokenization, `stopwords::stopwords()` for filtering, and builds **1–5-gram vocabulary**. Pruning rules (lines 33–42):
- Removes terms matching `"__"` pattern
- Removes terms starting or ending with digits
- Requires term length > 3 characters
- Applies minimum document frequency (`term_count_min`) and document proportion bounds

Returns a `data.table` of terms — directly reusable in any text classification pipeline over scientific abstracts.

### F. `get_dtm()` — Document-Term Matrix

**File:** `R/get_dtm.R`, lines 18–42
**Signature:** `get_dtm(abstracts, vocabulary, tf_idf = TRUE)`

Tokenizes, vectorizes against a vocabulary, and optionally applies TF-IDF weighting (`text2vec::TfIdf$new()`, `fit_transform()`). Returns a sparse matrix. This is the bridge between raw text and a machine-learning-ready numeric representation.

### G. `get_y()` — Label Extraction

**File:** `R/get_y.R`, lines 12–19
**Signature:** `get_y(abstracts)`

Extracts the `class` column and converts `"possitive"` → `1`, everything else → `0`. Straightforward but reusable for any binary PU-learning setup.

---

## 6. Overall Data Flow

```
External API (PubMed/Crossref/ZOVER/GMPD) — NOT in repo
        ↓
  [User-supplied data.frame with doi, title, abstract, class]
        ↓
  validate_abstracts()   [schema + value enforcement]
        ↓
  new_abstracts()        [typed tibble construction]
        ↓
  get_abstracts()        [composed entry point]
        ↓
  abstracts2text()       [UTF-8 normalization]
        ↓
  get_vocabulary()       [n-gram vocab with pruning]
  get_y()                [binary label vector]
        ↓
  get_dtm()              [sparse TF-IDF matrix]
        ↓
  fit_plus() → lacs()    [PLUS classifier, stored as .rda]
```

---

## 7. Key Findings for Reuse

| Function | File | Lines | Reusable For |
|---|---|---|---|
| `validate_abstracts()` | `R/validate_abstracts.R` | 11–32 | Schema enforcement on any paper metadata ingestion |
| `new_abstracts()` | `R/new_abstracts.R` | 19–30 | Typed data object construction |
| `get_abstracts()` | `R/get_abstracts.R` | 10–18 | Drop-in validation + construction pipeline |
| `abstracts2text()` | `R/abstracts2text.R` | 13–26 | UTF-8 normalization of abstract text |
| `get_vocabulary()` | `R/get_vocabulary.R` | 15–44 | Domain-tuned n-gram vocabulary construction |
| `get_dtm()` | `R/get_dtm.R` | 18–42 | Text → sparse TF-IDF matrix |
| `get_y()` | `R/get_y.R` | 12–19 | Binary label extraction |
| `loadpkg()` | `R/loadpkg.R` | 5–11 | Graceful optional-dependency loading |

---

## 8. Gap Analysis & Recommendations

**Gap:** There is no code for fetching papers from PLOS, arXiv, or PubMed. If you want to add a download layer, you would integrate it before `get_abstracts()` — fetching from APIs and normalizing the result into the `(doi, title, abstract, class)` schema.

The `R/lacsSample.R` documentation (lines 1–16) indicates Crossref was used for the negative class, and `R/hp3_abstracts.R` (lines 1–29) references PubMed via Entrez — these would be natural targets for implementing a reusable fetch layer using packages like `rcrossref` or `rentrez`.
