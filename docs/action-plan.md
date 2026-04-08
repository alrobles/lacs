# LACS Action Plan

## Goal
Stabilize the LACS package as a reliable research-oriented R package for literature abstract classification.

## Milestones

### Milestone 1 — Clean the foundation
Focus on correctness, consistency, and clearer package behavior.

#### Tasks
- Standardize label naming across the package.
  - Decide on one canonical spelling for the positive class.
  - Ensure `validate_abstracts()`, `get_y()`, examples, and documentation use the same terminology.
- Fix validation logic.
  - Replace brittle checks with explicit and descriptive validation.
  - Verify required columns: `doi`, `title`, `abstract`, `class`.
  - Ensure only allowed class values are accepted.
- Review text preprocessing.
  - Confirm the intended vocabulary filtering rules.
  - Correct regex patterns if they are not behaving as expected.
  - Validate the n-gram range and stopword handling.
- Improve package metadata.
  - Replace placeholder DESCRIPTION text with a real package description.
  - Review package fields for completeness and consistency.
- Add tests for the core workflow.
  - Validation of correct and incorrect input.
  - Constructor behavior.
  - Label extraction.
  - Vocabulary creation.
  - DTM generation.

#### Definition of done
- Package terminology is consistent.
- Core functions fail clearly and predictably on invalid input.
- Basic unit tests cover the main workflow.
- The package metadata is no longer placeholder-like.

### Milestone 2 — Stabilize the package API
Focus on predictable inputs and outputs.

#### Tasks
- Confirm object structure for `abstracts`.
- Document expected input types and return types for each exported function.
- Add examples that reflect the current canonical workflow.
- Verify that `get_abstracts()`, `abstracts2text()`, `get_vocabulary()`, `get_dtm()`, and `get_y()` compose cleanly.

#### Definition of done
- Exported functions behave consistently.
- Users can follow a single documented workflow from raw data to DTM and labels.

### Milestone 3 — Prepare for maintainability
Focus on long-term reliability.

#### Tasks
- Add or verify CI checks.
- Run package checks in a repeatable way.
- Improve documentation coverage.
- Add a short vignette or workflow guide.
- Introduce changelog or release notes if needed.

#### Definition of done
- Package checks are reproducible.
- Documentation is sufficient for new users.
- Future changes can be made with lower risk.

## Phase 1 priority order
The first phase should be executed in this order:
1. Standardize terminology.
2. Fix validation behavior.
3. Review and correct preprocessing logic.
4. Update documentation and examples.
5. Add tests for the cleaned-up behavior.

## Suggested working style
- Make one small fix at a time.
- Add tests before or alongside behavior changes.
- Prefer clarity over cleverness.
- Keep the package focused on its research use case.

## Outcome
After Phase 1, LACS should be internally consistent, easier to understand, and safer to extend.