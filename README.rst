=====
Shit's discontinued.
=====

Dancer
================================

Dancer is the declarative package manager for Compass Linux. It handles package resolution, building, conflict solving, and incremental updates. Everything is configured in Haskell.

Features
--------

- **Declarative package definitions**: Write packages in Haskell with full type safety.
- **Dependency resolution**: Automatically resolves and fetches transitive dependencies.
- **Conflict detection**: Warns about incompatible package combinations (e.g., GNU tools + musl libc).
- **Incremental builds**: Only rebuild packages that have changed.
- **Full rebuilds**: Cold rebuild the entire system with `--from-scratch`.
- **Generations**: Automatically creates snapshots for easy rollback.
- **USE flags**: Fine-grained control over package features.
- **Build environment**: Respects MAKEFLAGS, CFLAGS, and other build variables from your config.

Building Dancer
---------------

Requirements
~~~~~~~~~~~~

- GHC 9.6+
- Stack
- Haskell libraries (automatically fetched by Stack)

Build
~~~~~

::

    cd dancer
    stack build

Usage
-----

Basic Commands
~~~~~~~~~~~~~~

**Full system rebuild from scratch**

::

    dancer rebuild --from-scratch

**Incremental rebuild**

::

    dancer rebuild

**Fetch latest package definitions**

::

    dancer fetch

**Rebuild user homes**

::

    dancer home rebuild

Architecture
------------

**Modules**

- `Dancer.Types`: Core data types (Package, SystemConfig, LibC, etc.)
- `Dancer.CLI`: Command-line interface and argument parsing.
- `Dancer.Config`: Loading and validating Haskell config files.
- `Dancer.Build`: Dependency resolution, conflict checking, and build orchestration.
- `Dancer.Logging`: Consistent logging utilities.

**Build Flow**

1. Parse CLI arguments
2. Load `/etc/dancer/system.hs` config
3. Resolve package dependencies
4. Check for conflicts
5. Build each package in order
6. Create a new generation for rollback
7. Print summary

Generations
-----------

Dancer automatically creates a new "generation" after each successful rebuild. These are snapshots of your system that you can rollback to.

Generations are stored in `/var/lib/dancer/generations/`.

Development
-----------

To add a new command:

1. Add a handler in `Dancer.CLI`
2. Add the pattern match to `runCLI`
3. Implement the logic

To modify the build process:

1. Edit `Dancer.Build`
2. Update the logging calls to match the style
3. Test with `stack build && stack exec dancer -- <command>`
