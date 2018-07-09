
proc InitGIDProject {dir} {
      set materials  [GiD_Info materials] 
      set conditions [GiD_Info conditions ovpnt] 
      CreateWindow $dir $materials $conditions
}

proc CreateWindow {dir mat cond} {  
    if { [GidUtils::AreWindowsDisabled] } {
	return
    }  
    set w .gid.win_example
    InitWindow $w [= "CMAS2D.TCL - Example tcl file"] ExampleCMAS "" "" 1
    if { ![winfo exists $w] } return ;# windows disabled || usemorewindows == 0

    ttk::frame $w.top
    ttk::label $w.top.title_text -text [= "TCL window example for CMAS2D problem type"]
   
    ttk::frame $w.information -relief ridge
    ttk::label $w.information.path -text [= "Problem Type path: %s" $dir]
    ttk::label $w.information.materials -text [= "Available materials: %s" $mat]
    ttk::label $w.information.conditions -text [= "Available conditions: %s" $cond]
 
    ttk::frame $w.bottom
    ttk::button $w.bottom.start -text [= "Continue"] -command "destroy $w"
    ttk::button $w.bottom.random -text [= "Random surface"] -command "CreateRandomSurface $w"

    grid $w.top.title_text -sticky ew
    grid $w.top -sticky new
    grid $w.information.path -sticky w -padx 6 -pady 6
    grid $w.information.materials -sticky w -padx 6 -pady 6
    grid $w.information.conditions -sticky w -padx 6 -pady 6
    grid $w.information -sticky nsew
    
    grid $w.bottom.start $w.bottom.random -padx 6
    grid $w.bottom -sticky sew -padx 6 -pady 6
    if { $::tcl_version >= 8.5 } { grid anchor $w.bottom center }
    grid rowconfigure $w 1 -weight 1
    grid columnconfigure $w 0 -weight 1    
}

proc CreateRandomSurface {w} {

    set ret [tk_dialogRAM $w.dialog [= "Warning!!"] \
		 [= "Warning: this will create a nurbs surface in your current project"] "" 1 [= "Ok"] [= "Cancel"]]

    if {$ret ==0} {
	Create_surface
	destroy $w
    }
}

proc Create_surface {} {
    set a_x [expr rand()*10]
    set a_y [expr rand()*10]    
    set b_x [expr $a_x + rand()*10]
    set b_y [expr $a_y + rand()*10]    
    set c_x [expr $b_x + rand()*10]
    set c_y [expr $b_y - rand()*10]    
    if {$a_y < $c_y} {
	set d_y [expr $a_y - rand()*10]
	set d_x [expr $a_x + rand()*10]
    } else {
	set d_y [expr $c_y - rand()*10] 
	set d_x [expr $c_x - rand()*10]
    }    
    set next_line_id [GiD_Info Geometry MaxNumLines]
    GiD_Process MEscape Geometry Create Line $a_x,$a_y,0.000000 $b_x,$b_y,0.000000 $c_x,$c_y,0.000000 $d_x,$d_y,0.000000 close escape
    GiD_Process Mescape Geometry Create NurbsSurface [expr $next_line_id+1]:[expr $next_line_id+4] escape escape
    GiD_Process 'Zoom Frame
}
