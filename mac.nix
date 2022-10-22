{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = [
    cmake
    shaderc
    libiconv
    darwin.apple_sdk.frameworks.Security
    darwin.apple_sdk.frameworks.CoreFoundation
    darwin.apple_sdk.frameworks.CoreServices
    darwin.apple_sdk.frameworks.AppKit
  ];
}
