extends RigidBody2D

@onready var splat = $Splats
@onready var uhh = $Uhh
@onready var wheeze = $Wheezing
@onready var spr = $AnimatedSprite2D
var isTurd = true

const splat_offsets = [
		1.900884,
		2.819252,
		10.281406,
	]
# Called when the node enters the scene tree for the first time.
func _ready():
	spr.play()


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta):
	if Input.is_action_just_pressed("jump-turd"):
		uhh.play()
		wheeze.playing = false
		spr.speed_scale = 4
		apply_central_impulse(Vector2(0,-1000))

func _play_splat():
	splat.play(splat_offsets.pick_random())
	await get_tree().create_timer(0.8).timeout
	splat.stop()
	wheeze.playing = true
	

func _on_body_entered(body):
	_play_splat()
	spr.speed_scale = 1
	pass # Replace with function body.


func _on_wheezing_finished():
	wheeze.play()
	pass # Replace with function body.
