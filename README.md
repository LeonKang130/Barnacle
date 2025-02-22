# Barnacle Renderer

## Overview

Barnacle is a toy renderer written in F# as part of UCSB 2025W CS263 course project. Here is a sample image rendered by Barnacle:

![Path-Traced GI](https://github.com/LeonKang130/Barnacle/blob/main/Sample%20-%20Path%20Tracing.png)

## Roadmap

Although this renderer is not designed for actual real-time rendering, we would still like to implement some real-time rendering techniques. Here are the features we plan to implement:
- [x] Support triangle mesh
- [x] Use BVH for acceleration
- [x] Support Lambertian, Mirror, Dielectric materials
- [ ] Support Microfacet material
- [x] Path-traced DI & GI
- [x] Primary Sample Space Metropolis Light Transport
- [ ] ReSTIR DI
- [x] Load scene from JSON file
- [x] Image output using [ImageSharp](https://github.com/SixLabors/ImageSharp)
- [ ] Animation output as GIF
