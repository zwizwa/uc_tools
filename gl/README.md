https://claude.ai/chat/676a8d85-b626-4c8a-8508-5dfd5df3b309
Using claude to bootstrap this.
Basic ideas:
- Use the same shaders in WebGL for browser and SDL for native linux app
- All the data proessing code is C (emscripten in browser, native on linux)
- Use backticks in JS to include the shader code (e.g. in the emscripten shell.html)
- In C, use xxd -i to convert to C file

Build the C code first then reuse as much as possible in emscripten.
The GL stuff can just stay in C.



