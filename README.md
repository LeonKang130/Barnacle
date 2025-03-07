﻿# Barnacle Renderer

## Overview

Barnacle is a toy renderer written in F# as part of UCSB 2025W CS263 course project. Here is a sample image rendered by Barnacle:

![Path-Traced GI](https://github.com/LeonKang130/Barnacle/blob/main/Sample%20-%20Path%20Tracing.png)

To run the project, clone the repository and run the following command:

```bash
dotnet run -i <scene.json> -o <output-image>
```

## Roadmap

Although this renderer is not designed for actual real-time rendering, we would still like to implement some common tricks in real-time rendering. Here are the features we plan to implement:
- [x] Support triangle mesh
- [x] Use SAH-based BVHs for acceleration (TLAS + BLAS)
- [x] Support Lambertian, Mirror, Dielectric materials
- [x] Support Metallic-Roughness workflow materials (following [glTF specification](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#appendix-b-brdf-implementation))
- [x] Path-traced DI & GI with MIS
- [x] Primary Sample Space Metropolis Light Transport
- [x] Load scene from JSON file
- [x] Image output using [ImageSharp](https://github.com/SixLabors/ImageSharp)
- [ ] Animation output as GIF
