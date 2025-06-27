# Gemini CLI Development Guide

This guide provides instructions for developing and contributing to `gemini-cli.el`.

## Quick Start

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/Kyure-A/gemini-cli.el.git
    cd gemini-cli.el
    ```
2.  **Run tests:**
    ```bash
    make test
    ```
3.  **Compile and lint:**
    ```bash
    make all
    ```

## Build and Test Commands

We use a `Makefile` to streamline common tasks.

-   **`make all`**: Run both `compile` and `checkdoc`. This is the recommended command before committing changes.
-   **`make compile`**: Byte-compile the Elisp files. This helps catch syntax errors and improves performance.
-   **`make test`**: Run unit tests using `ert`. Test files are located in the `tests/` directory.
-   **`make checkdoc`**: Lint the source code for documentation style.
-   **`make clean`**: Remove compiled (`.elc`) and backup files.
-   **`make autoloads`**: Generate the autoloads file.

For manual execution, you can use the following commands:

-   **Byte compile**: `emacs -Q --batch -f batch-byte-compile gemini-cli.el`
-   **Run tests**: `emacs -Q --batch -l ert -l tests/test-gemini-cli.el -f ert-run-tests-batch-and-exit`
-   **Check package**: `emacs -Q --batch -l package-lint.el -f package-lint-batch-and-exit gemini-cli.el`

## Code Style Guidelines

-   **Prefixing**: All public functions and variables must be prefixed with `gemini-cli-`. Private ones with `gemini-cli--`.
-   **Naming**: Follow standard Emacs Lisp kebab-case naming conventions.
-   **Binding**: Use lexical binding. Ensure `;; lexical-binding: t` is present in the file header.
-   **Headers**: Organize code with section headers: `;;;; Section Name`.
-   **Documentation**:
    -   Provide comprehensive docstrings for all functions and variables.
    -   The first line of a docstring must be a complete sentence ending with a period.
    -   For multi-line docstrings, add a blank line after the summary line.
-   **Autoloads**: Mark functions intended for interactive use with `;;;###autoload` comments.

## Dependencies and Package Headers

-   **Dependencies**: The package requires **Emacs 30.1+**, **transient 0.4.0+**, and **eat 0.8+**.
-   **Package-Requires**: Declare dependencies in the file header for `package.el`.
    ```emacs-lisp
    ;;; Package-Requires: ((emacs "30.1") (transient "0.4.0") (eat "0.8"))
    ```

## Error Handling

-   Use `if-let` or `when-let` for conditional execution on potentially nil values.
-   Use the `error` function to signal errors with clear, user-friendly messages.
-   Always check for the availability of external processes or APIs before making calls.

## Project Structure

-   **`gemini-cli.el`**: The main package file.
-   **`tests/`**: Directory for `ert` unit tests.
-   **`Makefile`**: Build and automation scripts.
-   **`README.md`**: User-facing documentation.
-   **`GEMINI.md`**: This development guide.
-   **`LICENSE`**: The license file (GPL-3.0).

## Git Workflow

-   **Commit Granularity**: Aim for small, focused commits. Each commit should ideally address a single logical change (e.g., a new feature, a bug fix, a refactoring). This makes the commit history easier to review, revert, and understand.
-   **Commit Messages**: Write clear, concise, and descriptive commit messages. Follow the conventional commits specification (e.g., `feat: add new feature`, `fix: resolve bug in X`). The subject line should be brief, followed by a blank line and a more detailed body if necessary.
-   **Branching**: Use feature branches for new features or bug fixes. Avoid committing directly to `master` (or `main`).

## Example User Configuration

To help users get started, provide a `use-package` example in the `README.md`.

```emacs-lisp
(use-package gemini-cli
  :ensure t
  :config
  (setq gemini-cli-api-key "YOUR_API_KEY"))
```
