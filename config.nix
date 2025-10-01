{
  packageOverrides = pkgs: rec {
    all = pkgs.buildEnv {
      name = "all";
      paths = [
        pkgs.bat
        pkgs.emacs
        pkgs.gitAndTools.gitFull
        pkgs.graphviz
        pkgs.grpcurl
        pkgs.nodejs_24
        pkgs.pandoc
        pkgs.pprof
        pkgs.semgrep
        pkgs.tree
        pkgs.wget
        (pkgs.yarn.override {
          nodejs = null;
        })
      ];
    };
  };
}
