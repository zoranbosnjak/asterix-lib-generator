# Website local rebuild

```bash
nix-shell
ghcid "--command=ghci -Wall site.hs"
runhaskell site.hs rebuild
firefox _site/index.html
```

