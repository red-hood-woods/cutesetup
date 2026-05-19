module Languages
  ( Language (..)
  , LangPackage (..)
  , allLanguages
  , packagesFor
  , langNixInputs
  , langDescription
  , langIcon
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Supported languages
data Language
  = Python
  | Rust
  | Go
  | NodeJS
  | Haskell
  | Ruby
  | Zig
  | C
  | Java
  | Elixir
  | Gleam
  | Ocaml
  | Nix
  deriving (Eq, Ord, Show, Enum, Bounded)

-- A package selectable for a language
data LangPackage = LangPackage
  { pkgName    :: String   -- ^ nix attribute name
  , pkgLabel   :: String   -- ^ human-readable label
  , pkgDesc    :: String   -- ^ short description
  , pkgDefault :: Bool     -- ^ pre-selected by default?
  } deriving (Eq, Show)

allLanguages :: [Language]
allLanguages = [minBound .. maxBound]

langIcon :: Language -> String
langIcon Python  = "🐍"
langIcon Rust    = "🦀"
langIcon Go      = "🐹"
langIcon NodeJS  = "🟩"
langIcon Haskell = "𝞴 "
langIcon Ruby    = "💎"
langIcon Zig     = "⚡"
langIcon C       = "🔧"
langIcon Java    = "☕"
langIcon Elixir  = "💧"
langIcon Gleam   = "✨"
langIcon Ocaml   = "🐫"
langIcon Nix     = "❄️"

langDescription :: Language -> String
langDescription Python  = "Python 3 with pip / uv tooling"
langDescription Rust    = "Rust via rustup / cargo"
langDescription Go      = "Go toolchain"
langDescription NodeJS  = "Node.js via npm / pnpm / yarn"
langDescription Haskell = "GHC + cabal / stack"
langDescription Ruby    = "Ruby + bundler / gems"
langDescription Zig     = "Zig compiler"
langDescription C       = "GCC / Clang + cmake / meson"
langDescription Java    = "JDK + Maven / Gradle"
langDescription Elixir  = "Elixir + Mix + Erlang/OTP"
langDescription Gleam   = "Gleam language + Erlang runtime"
langDescription Ocaml   = "OCaml + opam / dune"
langDescription Nix     = "Nix tooling (nil, alejandra…)"

-- Default packages per language
packagesFor :: Language -> [LangPackage]
packagesFor Python =
  [ LangPackage "python3"         "python3"         "Python 3 interpreter"       True
  , LangPackage "python3Packages.pip" "pip"         "Package installer"          True
  , LangPackage "uv"              "uv"              "Fast Python pkg manager"    False
  , LangPackage "python3Packages.virtualenv" "virtualenv" "Virtual environments" False
  , LangPackage "ruff"            "ruff"            "Fast Python linter"         False
  , LangPackage "pyright"         "pyright"         "Python LSP"                 False
  , LangPackage "black"           "black"           "Code formatter"             False
  , LangPackage "poetry"          "poetry"          "Dependency manager"         False
  , LangPackage "python3Packages.pytest" "pytest"  "Testing framework"          False
  ]
packagesFor Rust =
  [ LangPackage "rustc"           "rustc"           "Rust compiler"              True
  , LangPackage "cargo"           "cargo"           "Rust build tool"            True
  , LangPackage "rustfmt"         "rustfmt"         "Code formatter"             True
  , LangPackage "clippy"          "clippy"          "Linter"                     True
  , LangPackage "rust-analyzer"   "rust-analyzer"   "Rust LSP"                   False
  , LangPackage "cargo-watch"     "cargo-watch"     "Auto-rebuild on change"     False
  , LangPackage "cargo-edit"      "cargo-edit"      "Add/rm deps from CLI"       False
  , LangPackage "cargo-nextest"   "cargo-nextest"   "Faster test runner"         False
  , LangPackage "sccache"         "sccache"         "Shared compilation cache"   False
  , LangPackage "mold"            "mold"            "Fast linker"                False
  ]
packagesFor Go =
  [ LangPackage "go"              "go"              "Go toolchain"               True
  , LangPackage "gopls"           "gopls"           "Go LSP"                     False
  , LangPackage "golangci-lint"   "golangci-lint"   "Linter suite"               False
  , LangPackage "gotools"         "gotools"         "Extra go tools"             False
  , LangPackage "delve"           "delve"           "Go debugger"                False
  , LangPackage "air"             "air"             "Live reload"                False
  , LangPackage "migrate"         "migrate"         "DB migrations"              False
  ]
packagesFor NodeJS =
  [ LangPackage "nodejs_22"       "nodejs 22"       "Node.js LTS"                True
  , LangPackage "nodePackages.npm" "npm"            "Package manager"            True
  , LangPackage "nodePackages.pnpm" "pnpm"         "Fast pkg manager"           False
  , LangPackage "yarn"            "yarn"            "Yarn pkg manager"           False
  , LangPackage "nodePackages.typescript" "typescript" "TypeScript compiler"     False
  , LangPackage "nodePackages.ts-node" "ts-node"   "TS execution"               False
  , LangPackage "biome"           "biome"           "Formatter + linter"         False
  , LangPackage "nodePackages.prettier" "prettier"  "Code formatter"             False
  , LangPackage "nodePackages.eslint" "eslint"      "Linter"                     False
  , LangPackage "bun"             "bun"             "All-in-one JS runtime"      False
  ]
packagesFor Haskell =
  [ LangPackage "ghc"             "ghc"             "Haskell compiler"           True
  , LangPackage "cabal-install"   "cabal"           "Build tool"                 True
  , LangPackage "haskell-language-server" "HLS"    "Haskell LSP"                False
  , LangPackage "stack"           "stack"           "Stack build tool"           False
  , LangPackage "hlint"           "hlint"           "Linter"                     False
  , LangPackage "ormolu"          "ormolu"          "Formatter"                  False
  , LangPackage "hoogle"          "hoogle"          "Package search"             False
  , LangPackage "ghcid"           "ghcid"           "Auto-reload GHCi"           False
  ]
packagesFor Ruby =
  [ LangPackage "ruby"            "ruby"            "Ruby interpreter"           True
  , LangPackage "bundler"         "bundler"         "Gem bundler"                True
  , LangPackage "rubyPackages.rubocop" "rubocop"   "Linter + formatter"         False
  , LangPackage "rubyPackages.solargraph" "solargraph" "Ruby LSP"               False
  , LangPackage "rubyPackages.rake" "rake"         "Build tool"                 False
  , LangPackage "rubyPackages.rspec" "rspec"       "Testing framework"          False
  ]
packagesFor Zig =
  [ LangPackage "zig"             "zig"             "Zig compiler"               True
  , LangPackage "zls"             "zls"             "Zig LSP"                    False
  , LangPackage "lldb"            "lldb"            "Debugger"                   False
  ]
packagesFor C =
  [ LangPackage "gcc"             "gcc"             "GNU C/C++ compiler"         True
  , LangPackage "clang"           "clang"           "Clang compiler"             False
  , LangPackage "cmake"           "cmake"           "Build system"               True
  , LangPackage "ninja"           "ninja"           "Fast build tool"            False
  , LangPackage "meson"           "meson"           "Meson build system"         False
  , LangPackage "gdb"             "gdb"             "GNU debugger"               False
  , LangPackage "clang-tools"     "clang-tools"     "clangd LSP + clang-format"  False
  , LangPackage "valgrind"        "valgrind"        "Memory error detector"      False
  , LangPackage "pkg-config"      "pkg-config"      "Library config tool"        True
  ]
packagesFor Java =
  [ LangPackage "jdk21"           "JDK 21"          "Java Development Kit"       True
  , LangPackage "maven"           "maven"           "Build + dep manager"        False
  , LangPackage "gradle"          "gradle"          "Gradle build tool"          False
  , LangPackage "jdt-language-server" "jdtls"      "Java LSP"                   False
  ]
packagesFor Elixir =
  [ LangPackage "elixir"          "elixir"          "Elixir + Erlang/OTP"        True
  , LangPackage "elixir-ls"       "elixir-ls"       "Elixir LSP"                 False
  , LangPackage "erlang"          "erlang"          "Erlang runtime"             False
  , LangPackage "rebar3"          "rebar3"          "Erlang build tool"          False
  ]
packagesFor Gleam =
  [ LangPackage "gleam"           "gleam"           "Gleam compiler"             True
  , LangPackage "erlang"          "erlang"          "Erlang runtime"             True
  , LangPackage "rebar3"          "rebar3"          "Erlang build tool"          True
  ]
packagesFor Ocaml =
  [ LangPackage "ocaml"           "ocaml"           "OCaml compiler"             True
  , LangPackage "opam"            "opam"            "OCaml pkg manager"          True
  , LangPackage "dune_3"          "dune"            "Build system"               True
  , LangPackage "ocamlPackages.ocaml-lsp" "ocaml-lsp" "OCaml LSP"               False
  , LangPackage "ocamlPackages.utop" "utop"         "Better REPL"                False
  ]
packagesFor Nix =
  [ LangPackage "nil"             "nil"             "Nix LSP"                    True
  , LangPackage "alejandra"       "alejandra"       "Nix formatter"              True
  , LangPackage "nix-prefetch-git" "nix-prefetch-git" "Prefetch git sources"    False
  , LangPackage "nix-tree"        "nix-tree"        "Nix store explorer"         False
  , LangPackage "statix"          "statix"          "Nix linter"                 False
  , LangPackage "deadnix"         "deadnix"         "Dead code finder"           False
  ]

-- Extra nix inputs needed for some languages (beyond nixpkgs)
langNixInputs :: Language -> Map String (String, String)
langNixInputs _ = Map.empty
-- Future: could add fenix for Rust nightly, etc.
