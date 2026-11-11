const sidebars = {
  sidebar: [
    {
      type: "doc",
      id: "index",
      label: "Introduction"
    },
    {
      type: "doc",
      id: "getting-started",
      label: "Getting Started"
    },
    {
      type: "category",
      label: "Reference",
      collapsed: false,
      items: [
        "args",
        "options",
        "commands",
        "cli-app",
        "helpdoc",
        "built-in-commands",
        "cli-config",
        "validation-and-errors",
        "wizard",
        "bash-and-zsh-completion",
        "figlet",
        "auth",
        "sbt-plugin",
        "testing"
      ]
    },
    {
      type: "category",
      label: "Tutorials",
      collapsed: false,
      items: ["examples"]
    },
    {
      type: "category",
      label: "How-to Guides",
      collapsed: false,
      items: [
        "how-to-optional-option-with-default",
        "how-to-add-shell-completion",
        "how-to-test-a-cli",
        "how-to-package-native-binary"
      ]
    }
  ]
};

module.exports = sidebars;
