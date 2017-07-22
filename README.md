# dino

Chrome's no-wifi dino game lazily ported to the terminal

![terminal-gif](./docs/img/dino.gif)

## installation 

If you're installing on a Mac, then you can use the binaries attached to the GitHub release 
([instructions](#github-release-binaries)). For other operating systems, you can install
from source ([instructions](#install-from-source)).

#### github release binaries

Install on macOS with the following commands:

```bash
curl -L https://github.com/arcticmatt/dino-brick/releases/download/0.1.0/dino-`uname -s`-`uname -m` -o dino
chmod +x dino
sudo mv dino /usr/local/dino/ 
```

#### install from source

This method requires [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). After installing stack, run
the following commands:

```bash
git clone https://github.com/arcticmatt/dino-brick.git
cd dino-brick
stack install dino
```

## usage 

To play, use the `dino` command in your terminal.   
To get help, run `dino --help`.  
To see your high score, run `dino --high-score`.

## game instructions

If you're too lazy to run `dino --help`, here's how you play:

* **W/S** or 	**&uarr;/&darr;** to jump/duck
* **p** to pause 
* **r** to restart 
* **q** to quit
