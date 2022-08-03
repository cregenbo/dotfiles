{ pkgs, ... }: 

{
  home.packages = with pkgs; [
    wine64
    lutris
    steam
  ];
}
