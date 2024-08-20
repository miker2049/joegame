shader_type canvas_item;

uniform vec4 hcolor : source_color = vec4(1);

void vertex() {
	// Called for every vertex the material is visible on.
}

void fragment() {
	vec4 c = texture(TEXTURE, UV);
	if (c.a == 0.0){
		float t = texture(TEXTURE, UV+vec2(0.0,-TEXTURE_PIXEL_SIZE.y)).a;
		float b = texture(TEXTURE, UV+vec2(0.0, TEXTURE_PIXEL_SIZE.y)).a;
		float l = texture(TEXTURE, UV+vec2(-TEXTURE_PIXEL_SIZE.x,0.0)).a;
		float r = texture(TEXTURE, UV+vec2(TEXTURE_PIXEL_SIZE.x,0.0)).a;
		float sum = t + b + l + r;
		if (sum > 0.0){
			COLOR = vec4(hcolor);
		}
	}
}