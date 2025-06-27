# Gemini CLI Emacs Integration Requirements

This document outlines the requirements and design for `gemini-cli.el`, an Emacs integration for the Google Gemini CLI.

## 1. Core Goal

Create a powerful and user-friendly Emacs package to interact with the `gemini` command-line tool, inspired by modern packages like `claude-code.el` and `aidermacs`.

## 2. Core Functionality

-   **Asynchronous Process Management**:
    -   Start and stop the `gemini` CLI process (`npx https://github.com/google-gemini/gemini-cli`) from within Emacs.
    -   The process must run asynchronously without blocking the Emacs UI.
    -   Provide functions like `gemini-cli-start` and `gemini-cli-stop`.
-   **Dedicated Interaction Buffer**:
    -   All interaction with the CLI should happen in a dedicated buffer (e.g., `*gemini-cli*`).
    -   The buffer should display real-time output from the process.
    -   The buffer should correctly render ANSI color escape codes for a rich, terminal-like experience.
-   **Sending Input**:
    -   Provide a function (`gemini-cli-send-prompt`) to send user input from Emacs to the `gemini` process.

## 3. User Interface (UI)

-   **Transient Menu**:
    -   Implement a `transient`-based UI for a modern and efficient user experience.
    -   The main transient should be triggered by a command like `gemini-cli`.
-   **Command Completion**:
    -   When the user types `/` in the prompt, provide completion for available Gemini CLI commands (e.g., `/help`, `/tools`, `/memory`).
-   **Context-Aware Completion**:
    -   When the user types `@`, provide file path completion to easily add context to the prompt.

## 4. Configuration

-   Use `defcustom` to allow users to easily configure the package.
-   **Required settings**:
    -   `gemini-cli-api-key`: To store the user's Google AI API key (if needed, although the CLI might handle this).
-   **Optional settings**:
    -   `gemini-cli-model`: To specify the Gemini model to use (e.g., "gemini-pro").
    -   `gemini-cli-yolo-mode`: A boolean to automatically accept all actions (`--yolo` flag).
    -   `gemini-cli-start-command`: The command to launch the CLI, defaulting to `npx ...`.

## 5. Project Structure

-   **`gemini-cli.el`**: The main single-file package.
-   **`tests/`**: A directory for unit tests using the `ert` framework.
-   **`Makefile`**: For automating tasks like compiling, testing, and cleaning.
-   **`README.md`**: User-facing documentation with installation and usage instructions.
-   **`GEMINI.md`**: The internal development guide.
-   **`REQUIREMENTS.md`**: This file.
-   **`LICENSE`**: GPL-3.0.
