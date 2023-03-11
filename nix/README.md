# Update procedures

# sources.json

```bash
nix-shell -p niv --run "niv update"
# or
niv update nixpkgs -b master
```

## aspecs

```bash
# nix-env -i nix-prefetch-scripts
nix-prefetch-git https://github.com/zoranbosnjak/asterix-specs.git > aspecs.json
# or
nix-prefetch-git [--rev some-rev] https://github.com/zoranbosnjak/asterix-specs.git > aspecs.json
```

