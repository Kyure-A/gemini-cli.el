# Project Progress: gemini-cli.el

This document outlines the current development status and planned features for `gemini-cli.el`.

## Implemented Features

We've made significant progress in building a robust Emacs integration for the Google Gemini CLI. The following features have been successfully implemented:

-   **Basic Process Management**:
    -   Start, stop, and restart the `gemini` CLI process (`gemini-cli-start`, `gemini-cli-stop`, `gemini-cli-restart`).
    -   Process output is displayed in a dedicated `*gemini-cli*` buffer.

-   **Interactive Buffer (`*gemini-cli*`)**:
    -   The buffer behaves like a `comint` buffer, allowing direct input and history navigation.
    -   ANSI color codes in the CLI output are correctly rendered.
    -   **Code Block Highlighting**: Markdown fenced code blocks (` ```lang `) are automatically highlighted with appropriate Emacs major modes.
    -   **Clickable URLs**: URLs in the output are made clickable.

-   **Transient Command Interface**:
    -   A `transient` menu (`M-x gemini-cli`) provides a central hub for all interactions.
    -   **Prompt Input**: Send free-form prompts to Gemini.
    -   **Slash Command Completion**: Easily input Gemini's built-in slash commands (e.g., `/help`, `/tools`) with completion.
    -   **File Context**: Attach file contents to prompts by selecting files (`@` command).
    -   **Buffer/Region Context**: Send the content of the current buffer or active region as context to Gemini.

-   **Customization Options**:
    -   Configurable `gemini-cli-start-command`, `gemini-cli-model`, and `gemini-cli-yolo-mode` via `customize`.
    -   **Customizable Keybindings**: Users can define their own keybindings for common actions.
    -   **Customizable Prompt Templates**: Define and use reusable prompt templates with placeholders (`@buffer`, `@region`, `{{prompt}}`).

## Next Planned Features

Our next focus will be on enhancing the interaction with Gemini's output:

-   **Extract Last Code Block**: Implement a function to easily extract the last generated code block from the `*gemini-cli*` buffer and insert it into the current buffer or copy it to the kill ring.

## Future Considerations

-   **Advanced Output Parsing**: Further enhance output parsing for JSON, tables, etc.
-   **Prompt Template Enhancements**: Allow more dynamic context (e.g., current file name, directory) in templates.
-   **Integration with Emacs Features**: Explore more seamless integrations with other Emacs features (e.g., `magit` for diffs, `org-mode` for notes).
