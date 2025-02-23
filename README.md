# Barnacle Renderer

## Overview

Barnacle is a toy renderer written in F# as part of UCSB 2025W CS263 course project. Here is a sample image rendered by Barnacle:

![Path-Traced GI](https://github.com/LeonKang130/Barnacle/blob/main/Sample%20-%20Path%20Tracing.png)

## Roadmap

Although this renderer is not designed for actual real-time rendering, we would still like to implement some real-time rendering techniques. Here are the features we plan to implement:
- [x] Support triangle mesh
- [x] Use BVH for acceleration
- [x] Support Lambertian, Mirror, Dielectric materials
- [x] Support Metallic-Roughness workflow materials (following [glTF specification](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#appendix-b-brdf-implementation))
- [x] Path-traced DI & GI
- [x] Primary Sample Space Metropolis Light Transport
- [ ] ReSTIR DI
- [x] Load scene from JSON file
- [x] Image output using [ImageSharp](https://github.com/SixLabors/ImageSharp)
- [ ] Animation output as GIF
