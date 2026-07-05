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
#define SHADER_SRC(x) (const char *)x, sizeof(x)


/* Text mode font */
#include "mod_terminus_font_8x16.c"


#include "macros.h"
#include "tui_vga.h"


static GLuint uct_gl_shader(GLenum type, const char *src, const GLint len) {
    GLuint s = glCreateShader(type);
    glShaderSource(s, 1, &src, &len);
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
        GLint  font_size_u;
        GLint  font_map_u;
        GLint  char_u;
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

#define GL_ERRORS(m) \
    m(GL_NO_ERROR) \
    m(GL_INVALID_ENUM) \
    m(GL_INVALID_VALUE) \
    m(GL_INVALID_OPERATION) \
    m(GL_INVALID_FRAMEBUFFER_OPERATION) \
    m(GL_OUT_OF_MEMORY) \

#define CASE_GL_ERROR(e) case e: return #e;
const char* gl_error_string(GLenum err) {
    switch (err) { GL_ERRORS(CASE_GL_ERROR); default: return "unknown"; }
}



/* Texture units are a limited resource so it seems best to
   re-allocate them per program switch. */
struct texture_data {
    int w, h;
    uint8_t *data;
};
struct prog_texture {
    GLint  uniform;
    GLuint texture;
    void *data; // in case data changes
};

/* Abstract the state change that is needed after program change,
   i.e. allocating texture units. */
void uct_gl_prog_with_textures(GLuint program,
                               struct prog_texture *pt,
                               int nb_pt) {
    glUseProgram(program);
    int tu = 0;
    for (int i=0; i<nb_pt; i++) {
        /* Select the current texture unit. */
        glActiveTexture(GL_TEXTURE0 + tu);
        /* Bind a texture to it. */
        glBindTexture(GL_TEXTURE_2D, pt[i].texture);
        /* Optionally update texture data. */
        struct texture_data *td = pt[i].data;
        if (td) {
            /* Format is currently hardcoded to just GL_LUMINANCE */
            glTexImage2D( /* upload the texture image data */
                GL_TEXTURE_2D, 0, /* target, level */
                GL_LUMINANCE, td->w, td->h, 0, /* internalformat, width, height, border */
                GL_LUMINANCE, GL_UNSIGNED_BYTE, /* format, type */
                td->data);
        }
        /* Set the uniform variable to point to the texture unit. */
        glUniform1i(pt[i].uniform, tu);
        /* Next texture unit. The limit is behind the
           GL_MAX_TEXTURE_IMAGE_UNITS enum. */
        tu++;
    }
}



void uct_gl_render_text(struct uct_gl_app *s) {

    /* Program switch, optional new texture data upload and binding to
       uniform variables is abstracted */

    struct tui_vga *fb = &s->text.tui_vga;
    int w = fb->nb_cols * 2;
    int h = fb->nb_rows;

    if (!fb->video) {
        /* If not initialized, use a test pattern. */
        fb->video = malloc(w*h);
        for(int i=0; i<w*h/2; i++) {
            fb->video[2*i + 0] = i;
            fb->video[2*i + 1] = 0x07;
        }
    }
    struct texture_data td = {
        .w = w,
        .h = h,
        .data = fb->video,
    };
    struct prog_texture pt[] = {
        // uniform            // texture       // texture_data
        {s->text.font_map_u,  s->text.font_t,  NULL},
        {s->text.char_u,      s->text.char_t,  &td},
    };
    uct_gl_prog_with_textures(
        s->text.program,
        pt, ARRAY_SIZE(pt));


    /* Additional uniform variable initialization before drawing. */
    glUniform2f(s->text.dims_u,      w, h);
    glUniform2f(s->text.font_size_u, 8, 16);

    /* FIXME: Do the same abastraction for array attribures? */

    /* The quad vertices live in an array buffer. */
    glBindBuffer(GL_ARRAY_BUFFER, s->text.quad_b);

    /* The shader program refers to the elements of the buffer by
       attribute variable. */
    glVertexAttribPointer(s->text.point_a, 2, GL_FLOAT, GL_FALSE, 0, 0);

    /* Run the vertex shader for each element in the array, and
       the fragment shader for each pixel in each line. */
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4 /* count */ );


}

GLint uct_gl_uniform(GLuint prog, const char *name) {
    GLint loc = glGetUniformLocation(prog, name);
    if (loc == -1) {
        GLenum err = glGetError();
        if (err == 0) {
            ERROR("uct_gl_uniform: '%s' not found\n", name);
        }
        else {
            ERROR("uct_gl_uniform: '%s': loc=%d, prog=%d, err=%d (%s)\n",
                  name, loc, prog, err, gl_error_string(err));
        }
    }
    return loc;
}



void uct_gl_init_text(struct uct_gl_app *s) {
    LOG("init_text\n");

    /* Do all texture init on the first texture unit.  This doesn't
       really matter.  Only during rendering it is important that
       different textures are bound to different texture units which
       then can be set to uniform sampler2D variables. */
    glActiveTexture(GL_TEXTURE0);

    { /* Font texture */

        /* Allocate a new texture object. */
        glGenTextures(1, &s->text.font_t);
        /* Set texture as current 2D texture target for subsequent calls. */
        glBindTexture(GL_TEXTURE_2D, s->text.font_t);
        /* Subsequent glTexImage2D uses byte-aligned pixel data. */
        glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

        /* Convert the bit pattern to individual 0/1 texels. */
        uint8_t font_tex[8*16*256] = {};
        for (int row=0; row<16*256; row++) {
            uint8_t *out_r = &font_tex[row * 8];
            uint8_t  in_r  =  terminus_bold_8x16[row];
            for (int col=0; col<8; col++) {
                out_r[col] = 255 * (1 & (in_r >> (7-col)));
                // LOG(" %02x", out_r[col]);
            }
        }
        /* Upload the texture image data. */
        glTexImage2D(
            GL_TEXTURE_2D, 0, /* target, level */
            GL_LUMINANCE, 8, 16*256, 0, /* internalformat, width, height, border */
            GL_LUMINANCE, GL_UNSIGNED_BYTE, /* format, type */
            font_tex);
        /* Interpolation settings are part of the texture object (not the
           texture unit). */
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    }

    { /* Text character + attribute buffer texture */

      /* Allocate texture object for the text frame buffer.
         This will be updated frequently using glTexImage2D */
        glGenTextures(1, &s->text.char_t);
        /* Set texture as current 2D texture target for subsequent calls. */
        glBindTexture(GL_TEXTURE_2D, s->text.char_t);
        /* Subsequent glTexImage2D uses byte-aligned pixel data. */
        glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

        /* Interpolation settings are part of the texture object (not the
           texture unit). */
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

        /* Texture image data is uploaded at every frame. */

    }


    GLuint prog = s->text.program = glCreateProgram();
    glAttachShader(prog, uct_gl_shader(GL_VERTEX_SHADER,   SHADER_SRC(text_vert)));
    glAttachShader(prog, uct_gl_shader(GL_FRAGMENT_SHADER, SHADER_SRC(text_frag)));
    glLinkProgram(prog);

    /* Fragment shader needs to know the text frame buffer dimensions
       and font size.  Those are set every time they change due to
       e.g. window size change. */
    s->text.dims_u      = uct_gl_uniform(prog, "text_dims");
    s->text.font_size_u = uct_gl_uniform(prog, "font_size");

    s->text.font_map_u = uct_gl_uniform(prog, "font_map");
    s->text.char_u     = uct_gl_uniform(prog, "char_buffer");


    /* Allocate vertex array for the text screen's quad. */
    float c = 1.0f;
    GLfloat quad[] = {
        -c,  c,
         c,  c,
         c, -c,
        -c, -c,
    };
    GLuint buf; glGenBuffers(1, &buf);
    s->text.quad_b = buf;
    glBindBuffer(GL_ARRAY_BUFFER, buf);
    glBufferData(GL_ARRAY_BUFFER, sizeof(quad), quad, GL_STATIC_DRAW);


    /* Note that only vertex shaders have attribute variables that can
       range over arrays.  In contrast, fragment shaders only have
       uniform and varying variables. */
    GLint point_a = s->text.point_a = glGetAttribLocation(prog, "point");

    /* Permanently set this attriubte as an array attribute and not a
       constant shared by all vertices like uDims above. */
    glEnableVertexAttribArray(point_a);

}
void uct_gl_load_graph(struct uct_gl_app *s) {
    glBindBuffer(GL_ARRAY_BUFFER, s->graph.points_b);
    glBufferData(GL_ARRAY_BUFFER, sizeof(s->graph.data),
                 s->graph.data, GL_DYNAMIC_DRAW);
}

void uct_gl_init_graph(struct uct_gl_app *s) {
    LOG("init_graph\n");

    GLuint prog = s->graph.program = glCreateProgram();
    glAttachShader(prog, uct_gl_shader(GL_VERTEX_SHADER,   SHADER_SRC(graph_vert)));
    glAttachShader(prog, uct_gl_shader(GL_FRAGMENT_SHADER, SHADER_SRC(graph_frag)));
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


void log_gl_enum(GLenum e, const char *name) {
    GLint val;
    glGetIntegerv(e, &val);
    LOG("%s = %d\n", name, val);
}
#define LOG_GL_ENUM(sym) log_gl_enum(sym, #sym)

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

    LOG_GL_ENUM(GL_MAX_TEXTURE_IMAGE_UNITS);

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
        uct_gl_render_text(s);

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
    LOG("\r%d", s->tick++);
}

void uct_gl_close(struct uct_gl_app *s) {
    SDL_GL_DeleteContext(s->ctx);
    SDL_DestroyWindow(s->win);
    SDL_Quit();
    LOG("\n");
}



#endif
