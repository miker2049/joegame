shader_type canvas_item;

void vertex() {
	// Called for every vertex the material is visible on.
}

void fragment() {
	// Called for every pixel the material is visible on.
	float dist = distance(UV, vec2(0.5));
    float ripple = sin(10.9 * dist - TIME * 2.0) * 0.5 + 0.5;
	vec2 distortedUV = UV + vec2(ripple * UV.x, 0);
    COLOR = texture(TEXTURE,(distortedUV+UV+UV+UV)/4.0);
}

//void light() {
	// Called for every pixel for every light affecting the CanvasItem.
	// Uncomment to replace the default light processing function with this one.
//}
