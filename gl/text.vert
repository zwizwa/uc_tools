// -*- glsl -*-
attribute vec2 point;  // point.x in [-1,1], point.y = value
void main(){
  // This produces normalized device coordinates in clip space (1,1)
  // top right to (-1,-1) bottom left.
  gl_Position = vec4(point /*x,y*/, 0.0 /*z*/, 1.0/*a*/);
}

