JOYPAD1 = $4016
DIRECTIONAL_BUTTONS = %00001111
BUTTON_RIGHT = %00000001
BUTTON_LEFT  = %00000010
BUTTON_DOWN  = %00000100
BUTTON_UP    = %00001000
enum $0000
	bg_ptr_lo dsb 1
	bg_ptr_hi dsb 1
        player_x_sub dsb 1
        player_x  dsb 1
        player_y_sub dsb 1
        player_y  dsb 1
        nmi_counter dsb 1
        buttons dsb 1
        moving? dsb 1
        direction dsb 1
        tmp dsb 1
ende
