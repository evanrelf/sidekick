args:

let
  # master on 2021-06-26
  rev = "b99a500a045e73fcabfb141b33fe0b9021966040";
  sha256 = "1dpcky02hn3jcah9cjqwjp3b9b71w0jfa8qf3zqpq8dsawdfb8dh";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)
