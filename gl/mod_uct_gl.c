/* This is just the body of an example app split up and pasted here to
   serve as a glue wrapper.  Will need refactoring. */

#ifndef MOD_UCT_GL
#define MOD_UCT_GL

#include <SDL2/SDL.h>
#include <GLES2/gl2.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Generated from .vert and .frag shader sources using xxd -i
#include "mod_graph_vert.c"
#include "mod_graph_frag.c"

#include "macros.h"



static GLuint uct_gl_shader(GLenum type, const char *src) {
    GLuint s = glCreateShader(type);
    glShaderSource(s, 1, &src, NULL);
    glCompileShader(s);
    GLint ok; glGetShaderiv(s, GL_COMPILE_STATUS, &ok);
    if (!ok) {
        char log[1024]; glGetShaderInfoLog(s, sizeof log, NULL, log);
        fprintf(stderr, "shader: %s\n", log); exit(1);
    }
    return s;
}

struct coord2 {
    float x, y;
};

struct uct_gl_app {
    SDL_Window *win;
    SDL_GLContext ctx;

    int W, H;
    int running:1;

    // X,Y array
    struct coord2 data[512];
    
};

void uct_gl_open(struct uct_gl_app *s) {

    memset(s, 0, sizeof(*s));

    /* Initialize SDL with video graphics subsystem + event system */
    SDL_Init(SDL_INIT_VIDEO);

    /* Set up as GLES2 */
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0);

    /* Create window and graphics context object. */
    s->W = 800;
    s->H = 600;
    s->win = SDL_CreateWindow("graph",
        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, s->W, s->H,
        SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE);
    s->ctx = SDL_GL_CreateContext(s->win);

    /* Compile and load the shaders. */
    GLuint prog = glCreateProgram();
    glAttachShader(prog, uct_gl_shader(GL_VERTEX_SHADER,   (const char *)graph_vert));
    glAttachShader(prog, uct_gl_shader(GL_FRAGMENT_SHADER, (const char *)graph_frag));
    glLinkProgram(prog);
    glUseProgram(prog);

    /* Create a buffer to hold the data. Note that in GLES2 there is
       no client-side data.  Everything needs to be uploaded to the
       graphics context. */
    GLuint buf; glGenBuffers(1, &buf);
    glBindBuffer(GL_ARRAY_BUFFER, buf);
    GLint loc = glGetAttribLocation(prog, "p");
    glEnableVertexAttribArray(loc);
    glVertexAttribPointer(loc, 2, GL_FLOAT, GL_FALSE, 0, 0);

    // --- demo data (replace with your C-generated array) ---
    // Interleaved [x,y], already in clip space [-1,1]. This is exactly the
    // buffer your function generator should fill.
    int N = ARRAY_SIZE(s->data);
    for (int i = 0; i < N; i++) {
        float x = -1.0f + 2.0f * i / (N - 1);
        s->data[i].x = x;
        s->data[i].y = 0.8f * sinf(x * 2 * 6.2831853f);
    }

    /* Opload the data to the GPU. */ 
    glBufferData(GL_ARRAY_BUFFER, sizeof(s->data), s->data, GL_STATIC_DRAW);

}

void uct_gl_tick(struct uct_gl_app *s) {
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
        if (e.type == SDL_QUIT) s->running = 0;
        if (e.type == SDL_KEYDOWN && e.key.keysym.sym == SDLK_ESCAPE) s->running = 0;
        if (e.type == SDL_WINDOWEVENT &&
            e.window.event == SDL_WINDOWEVENT_SIZE_CHANGED) {
            SDL_GL_GetDrawableSize(s->win, &s->W, &s->H);
            glViewport(0, 0, s->W, s->H);
        }
    }
    glClearColor(0.07f, 0.07f, 0.09f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    glDrawArrays(GL_LINE_STRIP, 0, ARRAY_SIZE(s->data));
    SDL_GL_SwapWindow(s->win);
}

void uct_gl_close(struct uct_gl_app *s) {
    SDL_GL_DeleteContext(s->ctx);
    SDL_DestroyWindow(s->win);
    SDL_Quit();
}



#endif
