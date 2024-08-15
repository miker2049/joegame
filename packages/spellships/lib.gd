class_name LibClass
extends Node

func ease_in_speed(current_pos: Vector2, target_pos: Vector2, max_speed: float, deceleration_radius: float) -> float:
	var distance = current_pos.distance_to(target_pos)
	
	if distance > deceleration_radius:
		# Outside the deceleration radius, return max speed
		return max_speed
	else:
		# Inside the deceleration radius, ease in the speed
		# This uses a simple linear interpolation, but you can use other easing functions for different effects
		var t = distance / deceleration_radius
		return max_speed * t
