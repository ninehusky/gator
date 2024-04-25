# gator 🐊

Gator is the next step in creating open-source hardware
compilation tools that are **correct** and **extensible**.

## What is Gator?

Getting hardware right is critical – correct hardware is the mechanism that ultimately enables the
correctness of software. While formally verified hardware compilers exist, these compilers are
too brittle to keep up with the ever-changing landscape of FPGA hardware primitives.
As a result, mainstream FPGA compilers make no formal correctness guarantees about the designs
they synthesize, leaving hardware designers to simply hope what they get is correct. Shoot!

As a first step toward a more correct FPGA compiler, we propose Gator:
a technology mapper which leverages the extensibility properties of tools like
[Lakeroad](https://github.com/uwsampl/lakeroad)
to provide these critical correctness guarantees to the hardware designer.

See my [slides](https://ninehusky.github.io/assets/02-14-2024-gator-talk.pdf)
for a high-level overview of the project.

## More
You'll need to set `$GATOR_DIR` to this. You can do this by running `export GATOR_DIR=$(pwd)` in the root directory of this repository.
yaa

## Acknowledgements

Gator is an offshoot project of the [Lakeroad](https://github.com/uwsampl/lakeroad) project
-- none of this would be possible without the hard work of my fellow Lakeroad teammates!
