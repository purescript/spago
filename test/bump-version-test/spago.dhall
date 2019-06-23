{ name =
    "bump-version-test"
, dependencies =
    [ "effect", "console", "psci-support", "tortellini" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, license =
    "MIT"
, repository =
    "git://github.com/spago/not-a-real-repo.git"
}
