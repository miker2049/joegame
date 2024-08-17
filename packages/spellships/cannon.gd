class_name Cannon
extends Sprite2D
var team: Ship.Team
var bullet_scene = preload("res://bullet.tscn")

var dmg: float

signal hit_target(Ship)


func _ready():
	var p = get_parent()
	dmg = p.conf.attack
	team = p.team
	rotate(PI/2)

func fire()-> void:
	var b: Bullet = bullet_scene.instantiate()
	get_tree().root.add_child(b)
	b.rotation = global_rotation 
	b.position =  global_position
	b.team = team
	b.hit_enemy.connect(_emit_hit)

	
func _emit_hit(s:Ship):
	s.take_damage(dmg)
	hit_target.emit(s)
