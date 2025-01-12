shader_type canvas_item;
uniform vec2 mouse;

void vertex() {
	// Called for every vertex the material is visible on.
}

void fragment() {

        // Calculate the distance from the center
    vec2 d = UV - mouse;
    float r = length(d);
    
    // Apply fisheye distortion
    float theta = atan(d.y, d.x);
    float radius = pow(r,2.1);
    
    // Map the distorted coordinates back to the texture
    vec2 distortedUV = mouse + vec2(radius * cos(theta), radius * sin(theta));
    
    // Sample the texture with the distorted coordinates
    COLOR = texture(TEXTURE, distortedUV);
    

	// Called for every pixel the material is visible on.
}

//void light() {
	// Called for every pixel for every light affecting the CanvasItem.
	// Uncomment to replace the default light processing function with this one.
//}
