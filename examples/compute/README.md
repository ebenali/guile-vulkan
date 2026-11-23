# Compute

A headless compute example: it squares 1000 floats on the GPU and checks the
result. It uses the plain Vulkan API, with no window and no extensions, so it
runs on any Vulkan device, including Mesa's software renderer.

## Running

From the repository root:

```sh
guile -L . examples/compute/compute.scm [device-index]
```

Output:

```
device: <the Vulkan device>
first values: (0.0 1.0 4.0 9.0 16.0 25.0 36.0 49.0)
last value:   998001.0 (expected 998001.0)
1000/1000 elements correct
```

It exits with status 0 when every element is correct, and 1 otherwise.

## What it shows

The whole path of a compute dispatch with the raw API:

- instance and device creation, picking a queue family with compute support;
- a storage buffer in host-visible memory, mapped and viewed as a bytevector
  with `vk-bytevector`;
- a shader module, a descriptor set layout, a pipeline layout with a push
  constant, and a compute pipeline;
- a descriptor set pointing at the buffer;
- a command buffer with bind, push-constant and dispatch commands, submitted
  with a fence.

Nested structs are built inline with `vk-new`. Pointer fields take the inner
bytevectors directly, and those stay alive as long as the struct holding
them.

For numerical work, `(vulkan compute)` does all of this for you: see "GPU
tensors" in the main README.

## Shader

`square.comp` squares each element of a storage buffer in place; the element
count comes in as a push constant. To rebuild `square.spv`:

```sh
glslangValidator -V examples/compute/square.comp -o examples/compute/square.spv
```
