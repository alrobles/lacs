# LACS Phase 1 — Task Board

## Column 1: Backlog
Tasks not yet started.

---

## Column 2: Ready for Implementation

### Task 1 — Standardize label terminology ✅ Done
**Owner:** Agent 1  
**Files:**
- R/validate_abstracts.R
- R/get_y.R
- R/get_abstracts.R
- R/new_abstracts.R
- man/*.Rd
- DESCRIPTION if wording needs updating

**Checklist**
- [x] Choose one canonical positive-class spelling
- [x] Remove possitive / unknow inconsistencies
- [x] Update validation logic to use canonical labels
- [x] Update label extraction logic
- [x] Update examples and docs

**Acceptance criteria**
- All label-related code uses one consistent spelling
- No remaining ambiguous class references in docs or examples

---

### Task 2 — Rewrite validation logic ✅ Done
**Owner:** Agent 1  
**Files:**
- R/validate_abstracts.R
- R/get_abstracts.R
- tests/testthat/test-validate-abstracts.R

**Checklist**
- [x] Replace stopifnot() with explicit validation
- [x] Validate required columns
- [x] Validate allowed class values
- [x] Emit helpful error messages
- [x] Preserve expected output column order

**Acceptance criteria**
- Invalid input fails clearly and predictably
- Valid input passes without side effects

---

### Task 3 — Clean up object construction
**Owner:** Agent 1  
**Files:**
- R/new_abstracts.R

**Checklist**
- [ ] Verify returned object structure
- [ ] Ensure class assignment is correct
- [ ] Check for any unnecessary assumptions
- [ ] Keep constructor simple and stable

**Acceptance criteria**
- new_abstracts() returns a valid abstracts object every time

---

### Task 4 — Review and fix vocabulary filtering
**Owner:** Agent 2  
**Files:**
- R/get_vocabulary.R

**Checklist**
- [ ] Review regex patterns
- [ ] Fix token filtering bugs
- [ ] Confirm stopword behavior
- [ ] Confirm n-gram range
- [ ] Validate on sample data

**Acceptance criteria**
- Vocabulary output behaves as intended
- Filtering rules are documented and reproducible

---

### Task 5 — Verify DTM generation
**Owner:** Agent 2  
**Files:**
- R/get_dtm.R
- R/abstracts2text.R

**Checklist**
- [ ] Confirm accepted input types
- [ ] Verify UTF-8 normalization behavior
- [ ] Confirm TF-IDF toggle works
- [ ] Ensure matrix dimensions match inputs

**Acceptance criteria**
- get_dtm() works for both abstract objects and character vectors
- Output is stable and documented

---

### Task 6 — Clean package metadata
**Owner:** Agent 3  
**Files:**
- DESCRIPTION
- NAMESPACE

**Checklist**
- [ ] Replace placeholder description text
- [ ] Review imports
- [ ] Verify exports
- [ ] Confirm package metadata matches reality

**Acceptance criteria**
- Package metadata is accurate and complete enough for maintenance

---

### Task 7 — Update documentation
**Owner:** Agent 3  
**Files:**
- man/*.Rd
- _pkgdown.yml
- README.md if needed

**Checklist**
- [ ] Regenerate docs after code changes
- [ ] Update examples to match canonical terminology
- [ ] Align parameter descriptions with behavior
- [ ] Confirm package site configuration is still valid

**Acceptance criteria**
- Documentation matches the implementation

---

### Task 8 — Add tests for Phase 1 behavior
**Owner:** Agent 4  
**Files:**
- tests/testthat/*.R
- tests/testthat.R
- tests/testthat/helper*.R if needed

**Checklist**
- [ ] Add validation tests
- [ ] Add constructor tests
- [ ] Add label extraction tests
- [ ] Add vocabulary tests
- [ ] Add DTM tests
- [ ] Add regression tests for terminology fixes

**Acceptance criteria**
- Core behavior is covered by tests
- Fixes cannot regress silently

---

## Column 3: In Progress
Move tasks here when an agent starts working.

---

## Column 4: Review
Tasks ready for human review after implementation.

---

## Column 5: Done
Tasks that passed implementation and review.

---

## Suggested execution order
1. Task 1 — Standardize label terminology
2. Task 2 — Rewrite validation logic
3. Task 3 — Clean up object construction
4. Task 4 — Review and fix vocabulary filtering
5. Task 5 — Verify DTM generation
6. Task 6 — Clean package metadata
7. Task 7 — Update documentation
8. Task 8 — Add tests for Phase 1 behavior

## Dependency notes
- Task 1 should happen before Tasks 2, 3, and 8.
- Task 2 should happen before Task 8.
- Task 4 and Task 5 depend on Task 1 being settled.
- Task 6 and Task 7 should follow the code changes.
- Task 8 should be done after the core cleanup so tests reflect the final behavior.