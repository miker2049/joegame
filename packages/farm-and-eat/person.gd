class_name Person
extends CharacterBody2D


@onready var mover_sprite = $TileMoverSprite


@onready var mover = $TileMover
@onready var hunger = $Hunger
@onready var hunger_bar = $MtnProgressBar
@onready var spr = $Spr

@export var pantry: Area2D
@export var idle_area: Area2D
@export var mappath: MapPath 

@onready var mach = $PersonMachine
@onready var idle_state = $PersonMachine/IdlePersonState
@onready var harvest_state = $PersonMachine/HarvestPersonState
@onready var die_state = $PersonMachine/DiePersonState
@onready var stock_state = $PersonMachine/StockPersonState
@onready var eat_state = $PersonMachine/EatPersonState


@onready var wprogress = $WorkProgress

var curr_job: Plant 
var inventory: Array[String] = []

@export var carrying_capacity: int = 1

func _tick_hunger(): 
	hunger_bar.set_value(1-(hunger.value/100))
	if hunger.value > 100 and mach.state != die_state:
		mover_sprite.enabled = false
		mach.change_state(die_state)

func _ready():
	idle_state.reached_target.connect(_idle_reached_target)
	harvest_state.finished_work.connect(_harvest_finished)
	stock_state.reached_target.connect(_idle_reached_target)
	eat_state.finished_eating.connect(_idle_reached_target)
	hunger.timeout.connect(_tick_hunger)
	Jobq.new_job.connect(_jobq_new_job)

func _harvest_finished():
	if inventory.size() < carrying_capacity:
		if not Jobq.jobs.is_empty():
			mach.change_state(stock_state)
			#_update_current_job()
		else:
			mach.change_state(stock_state)
	else:
		mach.change_state(stock_state)
		

func _idle_reached_target():
	if hunger.value > 50:
		mach.change_state(eat_state)
	elif not Jobq.jobs.is_empty():
		_update_current_job()
	else:
		print("idel reach target")
		mach.change_state(idle_state)


func _update_current_job():
	var job = Jobq.jobs.pop_back()
	curr_job = job
	if curr_job.grown:
		mach.change_state(harvest_state)

func _jobq_new_job():
	if mach.state == idle_state:
		pass
		#_update_current_job()
