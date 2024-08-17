class_name WordUI
extends Control

@onready var i_1 = $GridContainer/I1
@onready var i_2 = $GridContainer/I2
@onready var i_3 = $GridContainer/I3
@onready var i_4 = $GridContainer/I4
@onready var i_5 = $GridContainer/I5
@onready var i_6 = $GridContainer/I6
@onready var i_7 = $GridContainer/I7
@onready var p_1 = $GridContainer/P1
@onready var p_2 = $GridContainer/P2
@onready var p_3 = $GridContainer/P3
@onready var p_4 = $GridContainer/P4
@onready var p_5 = $GridContainer/P5
@onready var p_6 = $GridContainer/P6
@onready var p_7 = $GridContainer/P7

@onready var trash_btn = $GridContainer2/Trash
@onready var shuffle_btn = $GridContainer2/Shuffle
@onready var enter_btn = $Enter

const POOL_SIZE = 7

@onready var pool: Array[Button] = [p_1,p_2,p_3,p_4,p_5,p_6,p_7]
@onready var inp: Array[Button] = [i_1,i_2,i_3,i_4,i_5,i_6,i_7]

signal entered_word(String)

var alphabet: String = "abcdefghijklmnopqrstuvwxyz"

var pool_string: String = ""
var inp_string: String = ""

func _random_letter()->String:
	return alphabet[randi_range(0,25)]

func _random_string(n=7)->String:
	var out = ""
	for idx in n:
		out += _random_letter()
	return out

func _set_button_array(arr: Array[Button],i: String)->void:
	var s_size = i.length()
	print(s_size)
	for idx in arr.size():
		var b = arr[idx]
		var l = i[idx] if idx < s_size else ""
		b.text = l

func _update_buttons()->void:
	_set_button_array(pool, pool_string)
	_set_button_array(inp, inp_string)

func _refresh_pool()->void:
	var diff = POOL_SIZE-pool_string.length()
	var ns = _random_string(diff)
	pool_string += ns

func _refresh_pool_buttons()->void:
	var unused = pool.filter( func(btn): return not btn.button_pressed)
	var unused_string = Lib.array_to_string( unused.map(func(btn): return btn.text[0]))
	pool_string = unused_string
	_refresh_pool()
	_clear_pool_selection()

func _pool_button_pressed(idx: int)->void:
	var pressed = pool[idx].button_pressed
	if not pressed:
		pool[idx].button_pressed = true
	else:
		inp_string += pool[idx].text
		_update_buttons()

func _clear_pool_selection()->void:
	for btn in pool:
		btn.button_pressed = false
	inp_string = ""
	_update_buttons()


func _setup_pool_buttons()->void:
	for btni in pool.size():
		pool[btni].pressed.connect(_pool_button_pressed.bind(btni))
	pass

func _enter_word():
	entered_word.emit(inp_string)
	inp_string = ""
	_refresh_pool_buttons()

func _shuffle_pool()->void:
	var larr = Lib.string_to_array(pool_string)
	larr.shuffle()
	pool_string = Lib.array_to_string(larr)
	_clear_pool_selection()

func _ready():
	_refresh_pool()
	inp_string = ""
	_update_buttons()
	_setup_pool_buttons()
	trash_btn.pressed.connect(_clear_pool_selection)
	enter_btn.pressed.connect(_enter_word)
	shuffle_btn.pressed.connect(_shuffle_pool)
