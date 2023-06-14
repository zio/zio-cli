const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO CLI",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [
        "args",
        "options",
        "commands",
        "helpdoc",
        "built-in-commands",
        "cli-config",
        "auth",
        "bash-and-zsh-completion",
        "sbt-plugin",
        "examples"
      ]
    }
  ]
};

module.exports = sidebars;
