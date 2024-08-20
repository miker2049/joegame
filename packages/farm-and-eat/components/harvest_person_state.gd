class_name HarvestPersonState
extends DestinationState

var currd = Vector2.ZERO

var this_person: Person
var progress = 0.0
@export var work_dur = 10

var work_timer: Timer

signal finished_work

func _init_timer():
	if work_timer:
		remove_child(work_timer)
		work_timer.timeout.disconnect(_work_tick)
	work_timer = Timer.new()
	work_timer.wait_time = work_dur/4
	work_timer.timeout.connect(_work_tick)
	add_child(work_timer)

func _enter_state(p: Person):
	_init_timer()
	this_person = p
	this_person.wprogress.frame = 0
	assert(p.inventory.size()<p.carrying_capacity, "Can't be harvesting if you can't carry it.")
	assert(p.curr_job != null, "Person has actually received the job.")
	print("harvest")
	curr_target = p.curr_job.global_position/p.mover.tilesize
	__enter_state(p)

func _on_enter_destination():
	_start_work()

func _start_work():
	this_person.mover_sprite.enabled = false 
	this_person.spr.play("eat")
	this_person.wprogress.visible = true
	work_timer.start()

func _work_tick():
	progress += work_dur/4
	this_person.wprogress.frame = floor(remap(progress,0,10,0,4))
	if progress > work_dur:
		this_person.inventory.push_back(this_person.curr_job.plant_name) 
		this_person.curr_job.harvest()
		work_timer.stop()
		finished_work.emit()

func _exit_state(p: Person):
	p.mover_sprite.enabled = true
	__exit_state(p)
	this_person.wprogress.visible = false
	progress = 0.0


