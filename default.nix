{ pkgs ? import <nixpkgs> {} }:
(import ./stack_overlay.nix { inherit pkgs; }).hamtsolo
