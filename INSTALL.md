# Installation Instructions
1. `opam install ounit`
2. `opam install ounit2`
  - this project was designed using version 2.2.2
3. `opam install yojson`
  - this project was designed using version 1.7.0
4. Make sure that SDL2 is installed on your computer. 
  - On Ubuntu 18.04 this can be done via `apt install libsdl2-2.0-0`
5. Check what version of SDL2 you have with `sdl2-config --version`
6. If SDL2 version is at least 2.0.9: `opam install ocamlsdl2`
7. If SDL2 version is 2.0.8 or less:  `opam install ocamlsdl2.0.02`
  -this project was designed using SDL2 version 2.0.8; if problems arise,
  try switching to that version

# P.S.:
  - make sure that ocamlbuild and ocamlfind are installed
  - if opam prompts you to install something, please do
    - some of these installations might be OS dependent - for example, sdl2
      is installed differently depending on your OS
  - this compilation unit ***DOES NOT WORK*** on `WSL` - WSL was not built to
    handle graphics. Our recommended OS for this game is Ubuntu 18.04.