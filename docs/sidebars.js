const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO CLI",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [ 
        "bash-and-zsh-completion",
        "sbt-plugin"
      ]
    }
  ]
};

module.exports = sidebars;
