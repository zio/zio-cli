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
      type: "doc",
      id: "parsing",
      label: "How Parsing Works"
    },
    {
      type: "category",
      label: "Reference",
      collapsed: false,
      items: [
        {
          type: "category",
          label: "Commands and Parameters",
          collapsed: false,
          items: ["commands", "options", "args", "value-types"]
        },
        {
          type: "category",
          label: "Running an App",
          collapsed: false,
          items: [
            "cli-app",
            "cli-config",
            "built-in-commands",
            "helpdoc",
            "validation-and-errors"
          ]
        },
        {
          type: "category",
          label: "Interactive Features",
          collapsed: false,
          items: ["wizard", "bash-and-zsh-completion", "figlet", "auth"]
        },
        {
          type: "category",
          label: "Testing and Tooling",
          collapsed: false,
          items: ["testing", "sbt-plugin"]
        }
      ]
    },
    {
      type: "doc",
      id: "examples",
      label: "Tutorial"
    },
    {
      type: "category",
      label: "How-to Guides",
      collapsed: true,
      items: [
        "how-to-test-a-cli",
        "how-to-optional-option-with-default",
        "how-to-validate-input",
        "how-to-map-subcommands",
        "how-to-add-shell-completion",
        "how-to-oauth2-login",
        "how-to-exit-codes",
        "how-to-package-native-binary"
      ]
    },
    {
      type: "doc",
      id: "resources",
      label: "Resources"
    }
  ]
};

module.exports = sidebars;
