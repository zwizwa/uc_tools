// Don't use medium precision until effect is testable.  From what I
// gather online this is only relevant on old mobile hardware.
// precision mediump float;
precision highp float;

// Dimensions of current character grid. The dimensions are typically
// derived from the window/viewport dimensions.
uniform vec2 text_dims;    // e.g. (80, 25) for VGA

// Font size encoded in the font texture (glyphs stacked vertically)
uniform vec2 font_size;    // e.g. (8, 16) for VGA font

// Texture sampler for font glyphs
uniform sampler2D font_map;

// Texture sampler for character buffer
uniform sampler2D char_buffer;

void main() {

  // Textures are not arrays. They are interpolated samplers. We have
  // to take a bit of care to get discrete array lookup behavior using
  // float indices.

  // The _gc variables are "grid coordinates", i.e. float-encoded
  // integers that point at discrete pixels, 0 being the first one, 1
  // the next etc.  The _fgc are like _gc but represent fractional
  // grid coordinates.  We convert these to and from
  // center-of-fragment or center-of-texel normalized floating point
  // coordinates used in OpenGL.  Those are contained in _cp
  // coordinates.  The relation is _cp = _gc + 0.5

  // gl_FragCoord.xy is the coordinate of the pixel using
  // "center-of-fragment" coordinates.  Convert it to integer grid
  // coordinates.
  vec2 pixel_gc = floor(gl_FragCoord.xy);

  // Uncomment to zoom in, for glyp debugging.
  //pixel_gc = floor(pixel_gc / 2.0);

  // GLES2 does not have integer modulo so we implement it with
  // floor(), subtracting that from original to get fractional, and
  // then multiplying again to get integer glyph coordinates.

  vec2 char_fgc = pixel_gc / font_size;             // for 80x25 and 8x16 grids:
  vec2 char_gc  = floor(char_fgc);                  // (0,0) - (79,24)
  vec2 glyph_gc = font_size * (char_fgc - char_gc); // (0,0) - (7,15)

  // Stride = 2 to skip attribute byte.
  char_gc *= 2.0;

  // The texture sampling uses (0,1) range. We need to aim at the
  // center of the texels to get proper discrete "array element
  // lookup", which means ( (x+0.5)/n_x , (y+0.5)/ny )
  //
  // Together with nearest neighbor sampling this is correct if floats
  // have enough precision.
  //
  // Also note that y axis points differently in opengl (up) so we
  // compensate for that as well where needed.

  // The _cp are center-of-texel texture coordinates in 0.0 - 1.0
  // inclusive float range.
  vec2 char_cp = (char_gc + 0.5) / text_dims;
  // The texture lookup returns normalized 0.0 - 1.0 range data.
  float fchar_code = texture2D(char_buffer, char_cp).x;
  // Recover the integer value.
  float char_code = floor(0.5 + 255.0 * fchar_code);

  // The exact font atlas grid coordinates are
  vec2 font_gc = vec2(glyph_gc.x,
                      font_size.y - 1.0 - glyph_gc.y
                      + char_code * font_size.y);
  vec2 map_dims = vec2(font_size.x,
                       256.0 * font_size.y);

  // Converted to texel centers for lookup.
  vec2 font_cp = (font_gc + 0.5) / map_dims;
  float p = texture2D(font_map, font_cp).x;
 
  gl_FragColor = vec4(p,p,p,1.0);


}


