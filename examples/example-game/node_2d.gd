extends Node2D

func _ready():
    prints("start")

func _process(d):
    visible = !visible
