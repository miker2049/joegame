# Gravis Ultrasound Classic Pach Set V 1.6
This is the pat files for the gravis ultrasound classic patch set, converted with [unsf](https://github.com/psi29a/unsf) from [this](https://archive.org/details/GravisUltrasoundClassicPachSetV1.6) sf2 file.

## Modifying this
Use something like [polyphone](https://github.com/davy7125/polyphone) to modify the included sf2 file.

Init submodules to get unsf, then:
``` sh
# in unsf directory
mkdir build && cd build
cmake ..
make
```

Use the compiled `unsf` binary on the sf2 file to generate new files
