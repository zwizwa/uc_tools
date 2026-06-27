// -*- glsl -*-
attribute vec2 p;  // p.x in [-1,1], p.y = value
void main(){
     gl_Position = vec4(p,0.0,1.0);
}

