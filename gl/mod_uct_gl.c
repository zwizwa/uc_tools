/* Towards a stripped down TUI + plot interface.  Some constraints:
   - WebGL + Linux SDL (GLES2), i.e. no fixed pipeline
   - The GL stateful API just really sucks. I'll try not to complain.
*/

#ifndef MOD_UCT_GL
#define MOD_UCT_GL

#include <SDL2/SDL.h>
#include <GLES2/gl2.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* Generated from .vert and .frag shader sources using xxd -i */

/* Vertex and fragment shaders for the graph and text render programs. */
#include "mod_graph_vert.c"
#include "mod_graph_frag.c"
#include "mod_text_vert.c"
#include "mod_text_frag.c"

/* Text mode font */
#include "mod_terminus_font_8x16.c"


#include "macros.h"
#include "tui_vga.h"


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

    int tick;

    // There are two shader programs: one for drawing text and one for
    // drawing graphs.  Data associated to each of them is in these
    // structs.

    // _u uniform
    // _a attriub
    // _t texture
    // _b buffer

    struct {
        GLuint program;
        GLint  dims_u;
        GLint  point_a;
        GLuint quad_b;
        GLuint font_t;
        GLuint char_t;
        struct tui_vga tui_vga;
    } text;

    struct {
        GLuint program;
        GLuint points_b;
        GLint  point_a;
        struct coord2 data[512];
    } graph;


};

void uct_gl_update_text(struct uct_gl_app *s) {
    glUseProgram(s->text.program);
    glUniform2f(s->text.dims_u,
                s->text.tui_vga.nb_cols,
                s->text.tui_vga.nb_rows);

    glBindTexture(GL_TEXTURE_2D, s->text.char_t);
    GLsizei w = s->text.tui_vga.nb_cols * 2; // includes attribute byte
    GLsizei h = s->text.tui_vga.nb_rows;
    glTexImage2D( /* upload the texture image data */
        GL_TEXTURE_2D, 0, /* target, level */
        GL_LUMINANCE, w, h, 0, /* internalformat, width, height, border */
        GL_LUMINANCE, GL_UNSIGNED_BYTE, /* format, type */
        s->text.tui_vga.video);
}

void uct_gl_init_text(struct uct_gl_app *s) {
    LOG("init_text\n");
    /* allocate a new texture object */
    glGenTextures(1, &s->text.font_t);
    /* set texture as current 2D texture target for subsequent calls */
    glBindTexture(GL_TEXTURE_2D, s->text.font_t);
    /* subsequent glTexImage2D uses byte-aligned pixel data */
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glTexImage2D( /* upload the texture image data */
        GL_TEXTURE_2D, 0, /* target, level */
        GL_LUMINANCE, 8, 8, 0, /* internalformat, width, height, border */
        GL_LUMINANCE, GL_UNSIGNED_BYTE, /* format, type */
        terminus_bold_8x16);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glActiveTexture(GL_TEXTURE0); /* set current texture unit (sampler) */

    GLuint prog = s->text.program = glCreateProgram();
    glAttachShader(prog, uct_gl_shader(GL_VERTEX_SHADER,   (const char *)text_vert));
    glAttachShader(prog, uct_gl_shader(GL_FRAGMENT_SHADER, (const char *)text_frag));
    glLinkProgram(prog);

    /* Point the shader uniform to the texture unit, while the texture
       unit is linked to the texture. */
    GLint font_sampler_loc = glGetUniformLocation(prog, "font_sampler");
    glUniform1i(font_sampler_loc, 0);  // where 0 refers to the GL_TEXTURE0 texture unit / sampler


    /* Fragment shader needs to know the text frame buffer dimensions. */
    s->text.dims_u = glGetUniformLocation(prog, "uDims");

    /* allocate texture object for the text frame buffer.
       This will be updated frequently using glTexImage2D */
    glGenTextures(1, &s->text.char_t);

    /* Allocate vertex array for the text screen's quad. */
    GLfloat quad[] = {
        -0.5f, -0.5f,
         0.5f, -0.5f,
        -0.5f,  0.5f,
         0.5f,  0.5f,
    };
    GLuint buf; glGenBuffers(1, &buf);
    s->text.quad_b = buf;
    glBindBuffer(GL_ARRAY_BUFFER, buf);

    /* Note that only vertex shaders have attribute variables that can
       range over arrays.  In contrast, fragment shaders only have
       uniform and varying variables. */
    GLint point_a = s->graph.point_a = glGetAttribLocation(prog, "point");
    glEnableVertexAttribArray(point_a);

    /* Permanently set this attriubte as an array attribute and not a
       constant shared by all vertices like uDims above. */
    glEnableVertexAttribArray(point_a);

    uct_gl_update_text(s);
}
void uct_gl_load_graph(struct uct_gl_app *s) {
    glBindBuffer(GL_ARRAY_BUFFER, s->graph.points_b);
    glBufferData(GL_ARRAY_BUFFER, sizeof(s->graph.data),
                 s->graph.data, GL_DYNAMIC_DRAW);
}

void uct_gl_init_graph(struct uct_gl_app *s) {
    LOG("init_graph\n");

    GLuint prog = s->graph.program = glCreateProgram();
    glAttachShader(prog, uct_gl_shader(GL_VERTEX_SHADER,   (const char *)graph_vert));
    glAttachShader(prog, uct_gl_shader(GL_FRAGMENT_SHADER, (const char *)graph_frag));
    glLinkProgram(prog);

    /* Create a buffer to hold the data. Note that in GLES2 there is
       no client-side data.  Everything needs to be uploaded to the
       graphics context. */
    GLuint buf; glGenBuffers(1, &buf);
    s->graph.points_b = buf;
    glBindBuffer(GL_ARRAY_BUFFER, buf);
    GLint point_a = s->graph.point_a = glGetAttribLocation(prog, "point");
    glEnableVertexAttribArray(point_a);

    /* FIXME */
    int N = ARRAY_SIZE(s->graph.data);
    for (int i = 0; i < N; i++) {
        float x = -1.0f + 2.0f * i / (N - 1);
        s->graph.data[i].x = x;
        s->graph.data[i].y = 0.8f * sinf(x * 2 * 6.2831853f);
    }
}


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


    uct_gl_init_text(s);
    uct_gl_init_graph(s);

}

void uct_gl_tick(struct uct_gl_app *s) {
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
        if (e.type == SDL_QUIT) {
            s->running = 0;
        }
        if (e.type == SDL_KEYDOWN &&
            e.key.keysym.sym == SDLK_ESCAPE) {
            s->running = 0;
        }
        if (e.type == SDL_WINDOWEVENT &&
            e.window.event == SDL_WINDOWEVENT_SIZE_CHANGED) {
            SDL_GL_GetDrawableSize(s->win, &s->W, &s->H);
            glViewport(0, 0, s->W, s->H);
        }
    }

    glClearColor(0.07f, 0.07f, 0.09f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    if (1) {
        uct_gl_update_text(s);

        /* Vertex and fragment shader we use to process the quad (nop)
           and fill in the pixels (will perform text frame buffer and
           font map lookup in the fragment shader code).  The textures
           are bound to texture units at init and left there. */
        glUseProgram(s->text.program);

        /* The quad vertices live in an array buffer. */
        glBindBuffer(GL_ARRAY_BUFFER, s->text.quad_b);

        /* Bind the current buffer to

        /* According to Claude explanation, in GLES2 there is no way
           to permanently bind a shader input attribute to an array
           buffer (no VOAs), so we have to do this again when
           switching in the buffer so it can be used by glDrawArrays()
           below. */

        /* We tell the GPU how to read a vertex attribute out of that
           array buffer.  See other invocation for more information*/
        glVertexAttribPointer(s->text.point_a, 2, GL_FLOAT, GL_FALSE, 0, 0);


        /* Run the vertex shader for each element in the array, and
           the fragment shader for each pixel in each line. */
        glDrawArrays(GL_LINE_STRIP, 0, ARRAY_SIZE(s->graph.data) /* count */ );

    }

    if (1) {
        for (int i=0; i<ARRAY_SIZE(s->graph.data); i++) {
            s->graph.data[i].y *= 0.9;
        }
        uct_gl_load_graph(s);

        glUseProgram(s->graph.program);

        /* Run the vertex shader for each element in the array, and
           the fragment shader for each pixel in each line. */
        glBindBuffer(GL_ARRAY_BUFFER, s->graph.points_b);

        /* Define how bytes in the buffer map to the vertex program
           point attribute. */
        glVertexAttribPointer(
            s->graph.point_a,   /* Vertex shader atribute index */
            2,                  /* Components per vertex */
            GL_FLOAT,           /* Type of the data in the buffer. */
            GL_FALSE,           /* Normalized, ignored for float */
            0,                  /* Stride */
            0);                 /* Offset */

        glDrawArrays(GL_LINE_STRIP, 0, ARRAY_SIZE(s->graph.data) /* count */ );
    }

    SDL_GL_SwapWindow(s->win);
    LOG("%d\r", s->tick++);
}

void uct_gl_close(struct uct_gl_app *s) {
    SDL_GL_DeleteContext(s->ctx);
    SDL_DestroyWindow(s->win);
    SDL_Quit();
    LOG("\n");
}



#endif
