#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"async"
  [ oasis_lib "async"
  ; file "META" ~section:"lib"
  ]
