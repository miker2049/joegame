class_name Bullet
extends Area2D

var speed: float = 100.0
var angle: float = 0.0
@onready var offscreen = $VisibleOnScreenEnabler2D

var team: Ship.Team = Ship.Team.RED_TEAM
signal hit_enemy

func _ready():
	offscreen.screen_exited.connect(queue_free)
	body_entered.connect(_body_enter)

func _process(delta):
	global_position +=Vector2.from_angle(rotation-PI/2)  * speed * delta

func _body_enter(a: Ship) -> void:
	if "team" in a:
		if a.team != team:
			hit_enemy.emit(a)
			queue_free()
