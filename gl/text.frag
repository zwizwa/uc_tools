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

// As I understand, all shader code is "scalarized" and then mapped to
// SIMD slots and multiple processing units in parallel, so there is
// no reason to operate on vectors.  Use scalars for clarity where
// appropriate.


void main() {
  // gl_FragCoord.xy is the coordinate of the pixel.
  vec2 pixel_gc = gl_FragCoord.xy;

  // Uncomment to zoom in, for glyp debugging.
  pixel_gc /= 16.0;

  // GLES2 does not have integer modulo so we implement it with
  // floor(), subtracting that from original to get fractional, and
  // then multiplying again to get integer glyph coordinates.

  // The _gc are "grid coordinates", e.g. like C array indices.
  // Annotated with ranges for 80x25 text grid and 8x16 font.
  vec2 fchar_gc = pixel_gc / font_size;
  vec2 char_gc  = floor(fchar_gc);                  // (0,0) - (79,24)
  vec2 glyph_gc = font_size * (fchar_gc - char_gc); // (0,0) - (7,15)

  // The texture sampling uses (0,1) range. We need to aim at the
  // center of the texels to get proper "array lookup", which means
  // ( (x+0.5)/n_x , (y+0.5)/ny )
  //
  // Together with nearest neighbor sampling this is correct if floats
  // have enough precision.
  //
  // Also note that y axis points differently in opengl (up) so we
  // compensate for that as well where needed.

  // The _uv are texture coordinates in 0.0 - 1.0 inclusive float range.
  vec2 char_uv = (char_gc + 0.5) / text_dims;
  // The texture lookup returns normalized 0.0 - 1.0 range data.
  float fchar_code = texture2D(char_buffer, char_uv).x;
  // Recover the integer value.
  float char_code = floor(0.5 + 255.0 * fchar_code);

  // The exact font atlas grid coordinates are
  vec2 font_gc = vec2(glyph_gc.x,
                      font_size.y - 1.0 - glyph_gc.y + (255.0 - char_code) * font_size.y);
  vec2 map_dims = vec2(font_size.x,
                       256.0 * font_size.y);

  vec2 font_uv = (font_gc + 0.5) / map_dims;
  float p = texture2D(font_map, font_uv).x;
 
  gl_FragColor = vec4(p,p,p,1.0);


}


