open Graphics

let graph = open_graph "";
moveto 50 50;
draw_string "hi"

let cut = let status = wait_next_event [Button_down ] in 
if status.button then 
  let x = status.mouse_x in 
  let y = status.mouse_y in 
  get_image x y 20 20 
else get_image 0 0 20 20 


let paste capture = let status = wait_next_event [Button_down] in 
if status.button then 
  let x = status.mouse_x in 
  let y = status.mouse_y in 
  draw_image capture x y 

let cut_paste = let capture = cut in paste capture
