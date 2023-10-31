@tool
extends EditorScript

func _run():
	prints(ClassDB.get_parent_class(&"CanvasItem"))
