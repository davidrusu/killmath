{ pkgs ? import <nixpkgs> {} }:
let
in
  pkgs.mkShell {
    buildInputs = [
      (pkgs.rustChannelOf { channel = "nightly"; date = "2021-02-04"; }).rust
      pkgs.lld_11
      pkgs.clang_11
      pkgs.alsaLib
      pkgs.lutris
      pkgs.pkgconfig
      pkgs.vulkan-headers
      pkgs.vulkan-loader
      pkgs.vulkan-tools
      pkgs.vulkan-validation-layers
      pkgs.x11
      pkgs.xorg.libXcursor
      pkgs.xorg.libXi
      pkgs.xorg.libXrandr
      pkgs.udev
    ];

    LD_LIBRARY_PATH = "/run/opengl-driver/lib:${with pkgs; lib.makeLibraryPath [
      pkgs.x11
      pkgs.xorg.libXcursor
      pkgs.xorg.libXi
      pkgs.xorg.libXrandr
      pkgs.xorg.libxcb
      pkgs.vulkan-headers
      pkgs.vulkan-loader
      pkgs.vulkan-tools
      pkgs.vulkan-validation-layers
      udev
      alsaLib
    ]}";
}
