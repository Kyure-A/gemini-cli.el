# gemini-cli.el

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

An Emacs integration for the Google Gemini Command Line Interface (CLI).

`gemini-cli.el` provides a seamless and interactive way to communicate with the `gemini` CLI tool directly from your Emacs environment. Leverage the power of Gemini AI for coding, writing, and more, all without leaving your favorite editor.

## ✨ Features

*   **Interactive Process Management**: Start, stop, and restart the `gemini` CLI process within Emacs.
*   **Dedicated Interaction Buffer**: All interactions occur in a dedicated `*gemini-cli*` buffer, featuring ANSI color support for a rich terminal-like experience.
*   **Intuitive Input**: Type directly into the `*gemini-cli*` buffer and press `RET` to send your prompts, just like a regular terminal.
*   **Transient Menu**: Access common commands and actions via a powerful `transient` menu (`M-x gemini-cli`).
    *   **Prompt**: Send free-form prompts to Gemini.
    *   **Slash Commands**: Easily input Gemini's built-in slash commands (e.g., `/help`, `/tools`) with completion.
    *   **File Context**: Attach file contents to your prompts by selecting files with completion (`@` command).
*   **Customizable**: Configure the Gemini model, starting command, and other behaviors to suit your workflow.

## 🚀 Installation

### Manual Installation

1.  Clone this repository:
    ```bash
    git clone https://github.com/Kyure-A/gemini-cli.el.git
    ```
2.  Add the directory to your Emacs `load-path`:
    ```emacs-lisp
    (add-to-list 'load-path "/path/to/gemini-cli.el")
    (require 'gemini-cli)
    ```

### Using `use-package` (Recommended)

Add the following to your Emacs configuration (e.g., `init.el`):

```emacs-lisp
(use-package gemini-cli
  :ensure t ; Requires straight.el or package.el setup
  :commands (gemini-cli-start gemini-cli)
  :custom
  (gemini-cli-model "gemini-1.5-pro") ; Optional: Specify your preferred model
  (gemini-cli-yolo-mode nil) ; Optional: Set to t for automatic action approval
  :config
  ;; Optional: Add a global keybinding for the transient menu
  (global-set-key (kbd "C-c g") 'gemini-cli))
```

## 💡 Usage

1.  **Start the Gemini CLI**: 
    Execute `M-x gemini-cli-start` to launch the Gemini CLI process and open the interaction buffer (`*gemini-cli*`).

2.  **Interact Directly**: 
    Type your prompts directly into the `*gemini-cli*` buffer and press `RET` to send them to Gemini. The buffer behaves like a `comint` buffer, so you can use `M-p` and `M-n` for history.

3.  **Use the Transient Menu**: 
    Execute `M-x gemini-cli` (or your custom keybinding, e.g., `C-c g`) to bring up the `transient` menu:
    *   Press `p` to send a general prompt.
    *   Press `s` to send a slash command (e.g., `/help`, `/tools`).
    *   Press `@` to attach a file's content to your prompt.
    *   Press `r` to restart the Gemini CLI process.
    *   Press `q` to quit the Gemini CLI process.

## ⚙️ Configuration

Customize `gemini-cli.el` to fit your needs using Emacs's `customize` interface:

`M-x customize-group RET gemini-cli RET`

Here you can adjust:

*   `gemini-cli-start-command`: The command used to launch the Gemini CLI.
*   `gemini-cli-model`: Your preferred Gemini model (e.g., `"gemini-1.5-pro"`).
*   `gemini-cli-yolo-mode`: Enable/disable YOLO mode for automatic action approval.

## 🤝 Contributing

Contributions are welcome! Please refer to `GEMINI.md` for development guidelines and `REQUIREMENTS.md` for project requirements.

## 📄 License

This project is licensed under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.html).
