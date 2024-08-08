shader_type canvas_item;
uniform vec2 scaleTransform;

void vertex() {
	// Called for every vertex the material is visible on.
}

void fragment() {
  vec2 newUV = UV * scaleTransform;
  float tuv = mod(newUV.y,1);
  COLOR = texture(TEXTURE, vec2(UV.x*1.0,tuv)); // Read from texture

	// Called for every pixel the material is visible on.
}

//void light() {
	// Called for every pixel for every light affecting the CanvasItem.
	// Uncomment to replace the default light processing function with this one.
//}
