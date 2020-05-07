(** Run the following commands in your terminal in this exact order. Note that
the first two commands will differ depending on the machine you are using.
Additional instructions to install brew are attached. *)

For Mac users, to get brew:
xcode-select --install
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

LINUX:
sudo apt-get install pkg-config
sudo apt-get install libssl-dev
#sudo apt-get install libx11-dev

MAC:
brew install pkg-config
brew install openssl

All users:
opam install ssl lwt_ssl
opam install cohttp lwt js_of_ocaml
opam install cohttp-lwt-unix
#opam install graphics