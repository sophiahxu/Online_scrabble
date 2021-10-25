open Graphics

(*[random int] generates a random number 0 <= x <= 25*)
let random_int = Random.int 26

(*[random_letter int] takes in an int and outputs a string of the corresponding
letter in the alphabet*)
let random_letter int = match int with
| 0 -> "A"
| 1 -> "B"
| 2 -> "C"
| 3 -> "D"
| 4 -> "E"
| 5 -> "F"
| 6 -> "G"
| 7 -> "H"
| 8 -> "I"
| 9 -> "J"
| 10 -> "K"
| 11 -> "L"
| 12 -> "M"
| 13 -> "N"
| 14 -> "O"
| 15 -> "P"
| 16 -> "Q"
| 17 -> "R"
| 18 -> "S"
| 19 -> "T"
| 20 -> "U"
| 21 -> "V"
| 22 -> "W"
| 23 -> "X"
| 24 -> "Y"
| 25 -> "Z"
| _ -> "hi"

let graph = open_graph "";
moveto 50 50;
draw_string (random_letter(random_int))

(*[paste capture] takes in an image [capture] and 
places the image wherever a mouse is clicked*)
let paste capture = let status = wait_next_event [Button_down] in 
if status.button then 
  let x = status.mouse_x in 
  let y = status.mouse_y in 
  draw_image capture x y 

(*[cut_paste] copies an image and places it wherever a mouse is clicked, and 
draws a white rectangle over the original image's location*)
let cut_paste = let status = wait_next_event [Button_down] in 
let x = status.mouse_x in 
let y = status.mouse_y in 
if status.button then 
  let capture = get_image x y 20 20 in paste capture;
set_color white;
fill_rect x y 20 20

