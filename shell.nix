{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = [
    rustup
    lld_13
    clang_13
    xorg.libxcb
    # https://github.com/bevyengine/bevy/blob/e3170586776b165f40e446780fc4f9a4ed04187d/docs/linux_dependencies.md#nixos
    pkgconfig udev alsaLib # lutris
    x11 xorg.libXcursor xorg.libXrandr xorg.libXi
    vulkan-tools vulkan-headers vulkan-loader vulkan-validation-layers
  ];
  LD_LIBRARY_PATH = "/run/opengl-driver/lib:${pkgs.lib.makeLibraryPath [
    rustup
    lld_13
    clang_13
    xorg.libxcb
    pkgconfig udev alsaLib # lutris
    x11 xorg.libXcursor xorg.libXrandr xorg.libXi
    vulkan-tools vulkan-headers vulkan-loader vulkan-validation-layers
  ]}";
}
