class_name EatPersonState
extends DestinationState
var this_person: Person
var eat_timer: Timer
@export var eat_tick_amt = 40

signal finished_eating

func _init_timer():
	if eat_timer:
		remove_child(eat_timer)
		eat_timer.timeout.disconnect(_eat_tick)
	eat_timer = Timer.new()
	eat_timer.wait_time = 2.5
	eat_timer.timeout.connect(_eat_tick)
	add_child(eat_timer)


func _enter_state(p:Person):
	print("eat")
	p.mover_sprite.enabled = false
	p.spr.play("eat")
	_init_timer()
	this_person =  p
	curr_target = p.pantry.get_child(0).global_position/p.mover.tilesize
	__enter_state(p)
	
func _exit_state(p):
	p.mover_sprite.enabled = true
	eat_timer.stop()
	__exit_state(p)
	
func _eat_tick():
	this_person.hunger.value -= eat_tick_amt
	if this_person.hunger.value < 10:
		finished_eating.emit()  


func _on_enter_destination():
	eat_timer.start()


