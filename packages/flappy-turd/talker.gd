extends Control

var sscript = ''
var buf = ''
var idx = 0

# Called when the node enters the scene tree for the first time.
func _ready():
	var f = FileAccess.open('res://seinfeld.txt',FileAccess.READ)
	sscript = f.get_as_text()
	var tween = get_tree().create_tween()
	tween.set_loops(1)
	tween.tween_property($phone, "scale", $phone.scale*1.5, 18).set_trans(Tween.TRANS_BOUNCE)
	_next_word()
	

func _next_word():
	$speech.append_text(sscript[idx])
	idx+=1
	await get_tree().create_timer(randf_range(0.01,0.04)).timeout
	_next_word()
	

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	pass
