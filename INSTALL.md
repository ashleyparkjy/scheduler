For Mac users, to get brew:
xcode-select --install
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

LINUX:
sudo apt-get install pkg-config
sudo apt-get install libssl-dev

MAC:
brew install pkg-config
brew install openssl

opam install ssl lwt_ssl
opam install cohttp lwt js_of_ocaml
opam install cohttp-lwt-unix