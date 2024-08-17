class_name LibClass
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

var ship_class = preload("res://ship.tscn")
func random_ship(team: Ship.Team)->Ship:
	var ss = preload("res://ship.tscn")
	var r = ShipConfig.new().duplicate(true)
	r.vehicle_max_speed = 30.0 + (30.0*randf())
	r.vehicle_accel = 0.21 + 0.2*randf()
	r.ship_height = randf_range(48.0,96.0)
	r.ship_width = randf_range(24.0,48.0)
	r.attack = 10.0 * randf() + 7.0
	if team == Ship.Team.RED_TEAM:
		r.texture = preload("res://assets/ship_red.png")
	else: 
		r.texture = preload("res://assets/ship_blue.png")
	var ship =  ship_class.instantiate()
	ship.conf = r
	ship.team = team
	return ship
	
func randomize_ship_conf(r: ShipConfig, team: Ship.Team)->ShipConfig:
	r.vehicle_max_speed = 30.0 + (30.0*randf())
	r.vehicle_accel = 0.21 + 0.2*randf()
	r.ship_height = randf_range(48.0,96.0)
	r.ship_width = randf_range(24.0,48.0)
	r.attack = 10.0 * randf() + 7.0
	if team == Ship.Team.RED_TEAM:
		r.texture = preload("res://assets/ship_red.png")
	else: 
		r.texture = preload("res://assets/ship_blue.png")
	return r
	
	
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
