class_name Ship
extends CharacterBody2D

@onready var fsm = $FSM as FSM
@onready var ship_wander_state = $FSM/ShipWanderState as ShipWanderState
@onready var ship_pursue_state = $FSM/ShipPursueState as ShipPursueState
@onready var ship_attacking_state = $FSM/ShipAttackingState as ShipAttackingState
@onready var ship_dead_state = $FSM/ShipDeadState
@onready var ship_nothing_state = $FSM/ShipNothingState

@onready var mesh = $MeshInstance2D
@onready var vehicle = $Vehicle as Vehicle
@onready var purview_shape = $Purview/CollisionShape2D
@onready var attack_shape = $AttackZone/CollisionShape2D
@onready var timer = $Timer as Timer
@onready var body = $Body
@onready var personal_space = $PersonalSpace
@onready var cannon = $Cannon
@onready var word_label = $WordLabel
@onready var word_label_ctl = $WordLabel/WordLabelCtl

@onready var waves_2 = $Waves2
@onready var waves_1 = $Waves1

enum Team {RED_TEAM, BLUE_TEAM}

@export var team: Team = Team.RED_TEAM
@export var conf: ShipConfig

@export var word: String = ""

var curr_target: Ship

var full_health: float = 100
var health: float = full_health

var is_slow = true

func _ready() -> void:
	mesh.texture = conf.texture
	mesh.mesh = QuadMesh.new()
	mesh.mesh.size.y = conf.ship_height
	mesh.mesh.size.x = conf.ship_width
	
	body.shape = RectangleShape2D.new()
	body.shape.size.x = conf.ship_width
	body.shape.size.y = conf.ship_height
	body.rotation_degrees = 90
	
	health = conf.health
	full_health = conf.health
	
	#vehicle.max_speed = 70
	#vehicle.turn_speed = 0.7
	#vehicle.mass = 1
	#vehicle.desired_space = 32
	#vehicle.boundaries_offset = 120
	# needs to be set same or lower as attackzone
	#vehicle.arrive_offset = 64
	#vehicle.desired_space = .0
	
	#purview_shape.shape.radius = conf.purview_radius
	#attack_shape.shape.radius = conf.attack_radius
	
	timer.wait_time = conf.shoot_speed
	timer.timeout.connect(cannon.fire)
	_set_waves(100)
	_init_fsm()
	word_label_ctl.text = word
	
	
func _init_fsm():
	ship_wander_state.see_enemy.connect(_set_new_target)
	ship_pursue_state.reached_target.connect(fsm.change_state.bind(ship_attacking_state))
	ship_pursue_state.lost_target.connect(_reset_target)
	ship_attacking_state.lost_target.connect(_reset_target)
	

func _reset_target():
	print("reset target")
	curr_target=null
	fsm.change_state(ship_wander_state)

func _set_new_target(a: Ship):
	curr_target = a
	#curr_target.tree_exiting.connect(_reset_target)
	fsm.change_state(ship_pursue_state)

func take_damage(dmg:float):
	health -= dmg * randf()
	if health <= 0:
		fsm.change_state(ship_dead_state)

func _set_waves(amt:float):
	var lt = clampf(remap(amt,15.0,vehicle.max_speed,0.1,2.0),0.01,2.0)
	waves_1.lifetime = lt
	waves_2.lifetime = lt
	var ac = remap(amt,0,vehicle.max_speed,0.0,20.0)
	waves_1.linear_accel_max = ac
	waves_2.linear_accel_max = ac
	

func _physics_process(delta):
	_set_waves(velocity.length())
	word_label.global_rotation = 0
	if curr_target:
		if not is_instance_valid(curr_target):
			_reset_target()
		else:
			cannon.global_rotation = global_position.angle_to_point(curr_target.global_position) + PI/2
