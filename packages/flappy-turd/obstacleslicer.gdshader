shader_type canvas_item;
uniform vec2 pos;
uniform vec2 rectSize;
uniform float tilesize;

void vertex() {
	switch(VERTEX_ID){
		case 0: 
			VERTEX = pos + rectSize;
			break;
		case 1:
			VERTEX = vec2(pos.x,pos.y+rectSize.y);
			break;
		case 2:
			VERTEX = pos;
			break;
		case 3:
			VERTEX = vec2(pos.x+rectSize.x,pos.y);
			break;
	}
}

void fragment() {
  //vec2 newUV = UV * (rectSize / vec2(tilesize,tilesize));
  //newUV *= vec2(tilesize/20.0,1);
  //float newY = mod(2,newUV.y);
  //COLOR = texture(TEXTURE, vec2(newUV.x,newY)); // Read from texture
  vec2 newUV = fract(UV * vec2(1,rectSize.y/tilesize));
  COLOR = texture(TEXTURE, newUV); // Read from texture
}

//void light() {
	// Called for every pixel for every light affecting the CanvasItem.
	// Uncomment to replace the default light processing function with this one.
//}
