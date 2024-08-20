# utilities, for autoloading
extends Node

func get_random_viewport_point()->Vector2:
	var s = get_viewport().get_visible_rect().size
	return s * Vector2(randf(),randf())
	

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

func array_to_string(arr: Array) -> String:
	var s = ""
	for i in arr:
		s += String(i)
	return s

func string_to_array(s: String) -> Array:
	var arr = []
	for l in s:
		arr.push_back(l)
	return arr


# https://natureofcode.com/autonomous-agents/#path-following
func get_normal_point(pos: Vector2, segA: Vector2, segB: Vector2)-> Vector2:
	var vA = pos - segA
	var vB = segB -segA
	vB = vB.normalized()
	var p = vB * (vA.dot(vB))
	return segA + p
