# Regex Strings Generator

 Random Strings Generator For Given Regex.

## Installation

Use the package manager [cabal] to install regex-streamer.
1. clone the repo.

```bash
cd  project_dir
cabal configure
cabal build
cabal install
```


## Features

```.``` Match any character except newline   
```[``` Start character class definition   
```]``` End character class definition   
```?``` 0 or 1 quantifier   
```*``` 0 or more quantifiers   
```+``` 1 or more quantifier   
```{``` Start min/max quantifier   
```}``` End min/max quantifier     
```\d``` ```\s``` ```\w``` Character class (Escape Seqeuences)

WIthin Character Class-  
```^``` Negate the class, but only if the first character   
```-``` Indicates character range




## Usage

```haskell
import Evaluate
import Text.RawString.QQ
:set -XQuasiQuotes
generate 20 [r|[-+]{1,2}[0-9]{2,2}|]
```

## References
[Randex](https://github.com/ananthakumaran/randex)

## License
[BSD3](https://raw.githubusercontent.com/sagarchamp007/regex_streamer/master/LICENSE)
