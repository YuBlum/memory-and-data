package course

import "core:fmt"
import "core:math"
import "core:strconv"
import rl "vendor:raylib"

is_key_repeat :: #force_inline proc "contextless" (key: rl.KeyboardKey) -> bool { return rl.IsKeyPressed(key) || rl.IsKeyPressedRepeat(key) }

smoothlerp :: #force_inline proc "contextless" (e0, e1, t: $T) -> T { return math.lerp(e0, e1, math.smoothstep(T(0), T(1), t)) }

str_buf: [32]u8

DisplayMode :: enum {
  DECIMAL,
  BINARY,
  HEXADECIMAL,
}

InstructionType :: enum u8 {
  NOP,
  MOV,
  ADD,
  SUB,
  AND,
  XOR,
  AMOV,
  NAND,
  OR,
  JMP,
  JZR,
}

SelectedAddress :: struct {
  is_selected: bool,
  address: u8,
}

instruction_to_string :: #force_inline proc "contextless" (inst: InstructionType) -> cstring {
  switch inst {
  case .NOP:  return "NOP"
  case .MOV:  return "MOV"
  case .ADD:  return "ADD"
  case .SUB:  return "SUB"
  case .AND:  return "AND"
  case .XOR:  return "XOR"
  case .AMOV: return "AMOV"
  case .NAND: return "NAND"
  case .OR:   return "OR"
  case .JMP:  return "JMP"
  case .JZR:  return "JZR"
  case:       return "???"
  }
}

int_to_cstring :: proc(#any_int i: int) -> cstring {
  length := len(strconv.itoa(str_buf[:], i))
  str_buf[length] = 0
  return cstring(raw_data(str_buf[:]))
}

byte_bin_to_cstring :: proc "contextless" (byte: u8) -> cstring {
  for i in u8(0)..<8 do str_buf[i] = (byte >> (7 - i) & 1) + '0'
  str_buf[8] = 0
  return cstring(raw_data(str_buf[:]))
}

byte_hex_to_cstring :: proc "contextless" (byte: u8) -> cstring {
  for i in u8(0)..<2 {
    str_buf[i] = (byte >> ((1 - i) * 4) & 0xf)
    str_buf[i] = (u8(str_buf[i] < 0xa) * (str_buf[i] + '0')) + (u8(str_buf[i] > 0x9) * (str_buf[i] - 0xa + 'A'))
  }
  str_buf[2] = 0
  return cstring(raw_data(str_buf[:]))
}

byte_dec_to_cstring :: proc(byte: u8) -> cstring {
  length := len(strconv.itoa(str_buf[:], int(byte)))
  switch length {
  case 1:
    str_buf[3] = 0
    str_buf[2] = str_buf[0]
    str_buf[1] = '0'
    str_buf[0] = '0'
  case 2:
    str_buf[3] = 0
    str_buf[2] = str_buf[1]
    str_buf[1] = str_buf[0]
    str_buf[0] = '0'
  case:
    str_buf[length] = 0
  }
  return cstring(raw_data(str_buf[:]))
}

pow :: proc "contextless" (#any_int x, e: uint) -> uint {
  result: uint = 1
  for i in 0..<e do result *= x 
  return result
}

input_byte_decimal :: proc "contextless" (cursor: ^uint, byte: ^u8) {
  if cursor^ < 3 {
    nxt_num: uint
    if is_key_repeat(.ZERO)  do cursor^ += 1
    if is_key_repeat(.ONE)   do nxt_num = 1
    if is_key_repeat(.TWO)   do nxt_num = 2
    if is_key_repeat(.THREE) do nxt_num = 3
    if is_key_repeat(.FOUR)  do nxt_num = 4
    if is_key_repeat(.FIVE)  do nxt_num = 5
    if is_key_repeat(.SIX)   do nxt_num = 6
    if is_key_repeat(.SEVEN) do nxt_num = 7
    if is_key_repeat(.EIGHT) do nxt_num = 8
    if is_key_repeat(.NINE)  do nxt_num = 9
    if nxt_num > 0 {
      switch cursor^ {
      case 0:
        if (nxt_num * 100 + uint(byte^) <= 0xff) {
          byte^ += u8(nxt_num) * 100
          cursor^ += 1
        }
      case 1:
        if (nxt_num * 10 + uint(byte^) <= 0xff) {
          byte^ += u8(nxt_num) * 10
          cursor^ += 1
        }
      case 2:
        if (nxt_num + uint(byte^) <= 0xff) {
          byte^ += u8(nxt_num)
          cursor^ += 1
        }
      }
    }
  }
  if cursor^ > 0 && is_key_repeat(.BACKSPACE) {
    hundreds := byte^ / 100
    if cursor^ == 1 {
      byte^ -= hundreds * 100
    } else {
      tens := byte^ / 10 - (hundreds * 10)
      if cursor^ == 2 {
        byte^ -= tens * 10
      } else {
        ones := byte^ - (tens * 10) - (hundreds * 100)
        byte^ -= ones
      }
    }
    cursor^ -= 1
  }
}

main :: proc() {
  VIGNETTE_SHADER :: `
#version 330
in vec2 fragTexCoord;
out vec4 f_color;
uniform sampler2D tex0;
void main() { f_color = texture(tex0, fragTexCoord) * min((1.0 - length(fragTexCoord - 0.5)) * 1.3, 1.0); } `
  SCALE :: 0.5
  WINDOW_W :: 1280 * SCALE
  WINDOW_H :: 1280 * SCALE

  rl.InitWindow(WINDOW_W, WINDOW_H, "Course")
  defer rl.CloseWindow()

  vignette_sh := rl.LoadShaderFromMemory(nil, VIGNETTE_SHADER)
  rendertex := rl.LoadRenderTexture(WINDOW_W, WINDOW_H)

  BG_RECT_SIZE  :: 32 * SCALE
  BG_SPEED      :: 80
  bg_rect_color := [?]rl.Color{{0x40, 0x28, 0x3c, 0xff}, {0x22, 0x20, 0x34, 0xff}}
  bg_offset: [2]f32

  BYTE_BOX_SIZE_SELECTED  :: 200 * SCALE
  BYTE_BOX_SIZE           :: 150 * SCALE
  BYTE_BOX_GAP            :: 50  * SCALE
  BYTE_BOX_POS            :: [2]f32{WINDOW_W * 0.5, WINDOW_H * 0.5}
  BYTE_BOX_CHANGE_SPEED   :: 6
  memory: [0x100]u8
  current_byte: int
  previous_byte: int
  change_byte_timer: f32
  memory_is_instruction: [len(memory)]bool

  selected_memory: [len(memory)]u8
  memory_display_mode: DisplayMode

  insert_mode: bool
  insert_cursor: uint

  instruction_mode: bool
  current_instruction_mode := InstructionType(1)

  register_display_mode: DisplayMode
  registers: [3]u8
  selected_address: SelectedAddress

  execution_mode: bool
  current_instruction_exec: InstructionType
  execution_step: int
  execution_register: int
  execution_auto: bool
  execution_auto_timer: f32

  jump_mode: bool
  jump_to_memory_address: u8

  monocraft_fnt := rl.LoadFontEx("Monocraft.ttf", 64 * SCALE, nil, 0)

  for !rl.WindowShouldClose() {
    dt := rl.GetFrameTime()
    bg_offset += dt * BG_SPEED
    if bg_offset.x >= BG_RECT_SIZE do bg_offset = {}
    if !execution_mode {
      if !jump_mode {
        if !insert_mode {
          if rl.IsKeyPressed(.TAB) do execution_mode = true
          if rl.IsKeyPressed(.I) do instruction_mode = !instruction_mode
          if rl.IsKeyPressed(.J) {
            jump_mode = true
            insert_cursor = 0
            jump_to_memory_address = 0
          }
          if rl.IsKeyDown(.K) {
            if rl.IsKeyPressed(.R) do for &r in registers do r = 0
            if rl.IsKeyPressed(.M) do for &m in memory do m = 0
            if rl.IsKeyPressed(.L) do for &i in memory_is_instruction do i = false
          }
          if instruction_mode {
            if is_key_repeat(.ONE)   do current_instruction_mode = .MOV
            if is_key_repeat(.TWO)   do current_instruction_mode = .ADD
            if is_key_repeat(.THREE) do current_instruction_mode = .SUB
            if is_key_repeat(.FOUR)  do current_instruction_mode = .AND
            if is_key_repeat(.FIVE)  do current_instruction_mode = .XOR
            if is_key_repeat(.SIX)   do current_instruction_mode = .AMOV
            if is_key_repeat(.SEVEN) do current_instruction_mode = .NAND
            if is_key_repeat(.EIGHT) do current_instruction_mode = .OR
            to_register := -1
            if      rl.IsKeyPressed(.Z) do to_register = 0
            else if rl.IsKeyPressed(.X) do to_register = 1
            else if rl.IsKeyPressed(.C) do to_register = 2
            if to_register > -1 && selected_address.is_selected {
              switch current_instruction_mode {
              case .MOV:
                registers[to_register] = memory[selected_address.address]
                selected_address.is_selected = false
              case .ADD:
                registers[to_register] += memory[selected_address.address]
                selected_address.is_selected = false
              case .SUB:
                registers[to_register] -= memory[selected_address.address]
                selected_address.is_selected = false
              case .AND:
                registers[to_register] &= memory[selected_address.address]
                selected_address.is_selected = false
              case .XOR:
                registers[to_register] ~= memory[selected_address.address]
                selected_address.is_selected = false
              case .AMOV:
                if memory[selected_address.address] <= len(memory) - 1 do memory[memory[selected_address.address]] = registers[to_register]
                selected_address.is_selected = false
              case .NAND:
                registers[to_register] = ~(registers[to_register] & memory[selected_address.address])
                selected_address.is_selected = false
              case .OR:
                registers[to_register] |= memory[selected_address.address]
                selected_address.is_selected = false
              case .NOP:
              case .JMP:
              case .JZR:
              }
            }
          } else {
            selected_address.is_selected = false
          }
          if current_byte == previous_byte {
            current_byte += int(is_key_repeat(.DOWN)) - int(is_key_repeat(.UP))
            if current_byte > len(memory) - 1 do current_byte = len(memory) - 1
            if current_byte < 0               do current_byte = 0
            if current_byte != previous_byte do change_byte_timer = 1
          }
          if      rl.IsKeyPressed(.Q) do memory_display_mode = .BINARY
          else if rl.IsKeyPressed(.W) do memory_display_mode = .HEXADECIMAL
          else if rl.IsKeyPressed(.E) do memory_display_mode = .DECIMAL
          if      rl.IsKeyPressed(.A) do register_display_mode = .BINARY
          else if rl.IsKeyPressed(.S) do register_display_mode = .HEXADECIMAL
          else if rl.IsKeyPressed(.D) do register_display_mode = .DECIMAL
          if rl.IsKeyPressed(.LEFT_CONTROL) do memory_is_instruction[current_byte] = !memory_is_instruction[current_byte]
          if rl.IsKeyPressed(.ENTER) {
            insert_mode = true
            insert_cursor = 0
            if !memory_is_instruction[current_byte] {
              switch memory_display_mode {
              case .BINARY:
                if      memory[current_byte] & 0x01 != 0 do insert_cursor = 8
                else if memory[current_byte] & 0x02 != 0 do insert_cursor = 7
                else if memory[current_byte] & 0x04 != 0 do insert_cursor = 6
                else if memory[current_byte] & 0x08 != 0 do insert_cursor = 5
                else if memory[current_byte] & 0x10 != 0 do insert_cursor = 4
                else if memory[current_byte] & 0x20 != 0 do insert_cursor = 3
                else if memory[current_byte] & 0x40 != 0 do insert_cursor = 2
                else if memory[current_byte] & 0x80 != 0 do insert_cursor = 1
                else                                     do insert_cursor = 0
              case .HEXADECIMAL:
                if      memory[current_byte] & 0x0f != 0 do insert_cursor = 2
                else if memory[current_byte] & 0xf0 != 0 do insert_cursor = 1
                else                                     do insert_cursor = 0
              case .DECIMAL:
                hundreds := memory[current_byte] / 100
                tens := memory[current_byte] / 10 - (hundreds * 10)
                ones := memory[current_byte] - (tens * 10) - (hundreds * 100)
                if      ones     != 0 do insert_cursor = 3
                else if tens     != 0 do insert_cursor = 2
                else if hundreds != 0 do insert_cursor = 1
                else                  do insert_cursor = 0
              }
            }
          }
        } else {
          if rl.IsKeyPressed(.ENTER) do insert_mode = false
          if memory_is_instruction[current_byte] {
            if is_key_repeat(.UP) {
              memory[current_byte] -= 1
              if memory[current_byte] >= len(InstructionType) do memory[current_byte] = len(InstructionType) - 1
            }
            if is_key_repeat(.DOWN) {
              memory[current_byte] += 1
              if memory[current_byte] > len(InstructionType) - 1 do memory[current_byte] = 0
            }
          } else {
            switch memory_display_mode {
            case .BINARY:
              if insert_cursor < 8 {
                if is_key_repeat(.ONE) {
                  memory[current_byte] |= (0x80 >> insert_cursor)
                  insert_cursor += 1
                }
                if is_key_repeat(.ZERO) {
                  insert_cursor += 1
                }
              }
              if insert_cursor > 0 && is_key_repeat(.BACKSPACE) {
                memory[current_byte] &= ~(0x80 >> (insert_cursor - 1))
                insert_cursor -= 1
              }
            case .HEXADECIMAL:
              if insert_cursor < 2 {
                nxt_num: u8
                if is_key_repeat(.ZERO)  do insert_cursor += 1
                if is_key_repeat(.ONE)   do nxt_num = 0x1
                if is_key_repeat(.TWO)   do nxt_num = 0x2
                if is_key_repeat(.THREE) do nxt_num = 0x3
                if is_key_repeat(.FOUR)  do nxt_num = 0x4
                if is_key_repeat(.FIVE)  do nxt_num = 0x5
                if is_key_repeat(.SIX)   do nxt_num = 0x6
                if is_key_repeat(.SEVEN) do nxt_num = 0x7
                if is_key_repeat(.EIGHT) do nxt_num = 0x8
                if is_key_repeat(.NINE)  do nxt_num = 0x9
                if is_key_repeat(.A)     do nxt_num = 0xA
                if is_key_repeat(.B)     do nxt_num = 0xB
                if is_key_repeat(.C)     do nxt_num = 0xC
                if is_key_repeat(.D)     do nxt_num = 0xD
                if is_key_repeat(.E)     do nxt_num = 0xE
                if is_key_repeat(.F)     do nxt_num = 0xF
                if nxt_num > 0 {
                  switch insert_cursor {
                  case 0:
                    memory[current_byte] |= nxt_num << 4
                    insert_cursor += 1
                  case 1:
                    memory[current_byte] |= nxt_num
                    insert_cursor += 1
                  }
                }
              }
              if insert_cursor > 0 && is_key_repeat(.BACKSPACE) {
                memory[current_byte] &= ~(0xf0 >> u8(4 * (insert_cursor - 1)))
                insert_cursor -= 1
              }
            case .DECIMAL:
              input_byte_decimal(&insert_cursor, &memory[current_byte])
            }
          }
        }
      } else {
        if rl.IsKeyPressed(.J) do jump_mode = false
        if rl.IsKeyPressed(.ENTER) {
          jump_mode = false
          if int(jump_to_memory_address) != current_byte {
            current_byte = int(jump_to_memory_address)
            change_byte_timer = 1
          }
        }
        input_byte_decimal(&insert_cursor, &jump_to_memory_address)
      }
    } else {
      end_execution: bool
      if rl.IsKeyPressed(.TAB) do end_execution = true
      if rl.IsKeyPressed(.A) do execution_auto = !execution_auto
      if (is_key_repeat(.N) ||  (execution_auto && execution_auto_timer <= 0)) && change_byte_timer <= 0 {
        if current_byte >= (len(memory) - 1) do end_execution = true
        if current_instruction_exec != .NOP {
          first_step :: #force_inline proc "contextless" (execution_step, execution_register: ^int, byte: u8) -> bool {
            if execution_step^ > 0 {
              execution_step^ = 0
              return false
            }
            execution_register^ = int(byte)
            execution_step^ = 1
            return true
          }
          switch current_instruction_exec {
          case .MOV:
            if !first_step(&execution_step, &execution_register, memory[current_byte]) {
              registers[execution_register] = memory[current_byte]
              current_instruction_exec = .NOP
            } else if execution_register >= len(registers) {
              end_execution = true
              fmt.println("error: unknown register")
            }
          case .ADD:
            if !first_step(&execution_step, &execution_register, memory[current_byte]) {
              registers[execution_register] += memory[current_byte]
              current_instruction_exec = .NOP
            } else if execution_register >= len(registers) {
              end_execution = true
              fmt.println("error: unknown register")
            }
          case .SUB:
            if !first_step(&execution_step, &execution_register, memory[current_byte]) {
              registers[execution_register] -= memory[current_byte]
              current_instruction_exec = .NOP
            } else if execution_register >= len(registers) {
              end_execution = true
              fmt.println("error: unknown register")
            }
          case .AND:
            if !first_step(&execution_step, &execution_register, memory[current_byte]) {
              registers[execution_register] &= memory[current_byte]
              current_instruction_exec = .NOP
            } else if execution_register >= len(registers) {
              end_execution = true
              fmt.println("error: unknown register")
            }
          case .XOR:
            if !first_step(&execution_step, &execution_register, memory[current_byte]) {
              registers[execution_register] ~= memory[current_byte]
              current_instruction_exec = .NOP
            } else if execution_register >= len(registers) {
              end_execution = true
              fmt.println("error: unknown register")
            }
          case .AMOV:
            if !first_step(&execution_step, &execution_register, memory[current_byte]) {
              memory[memory[current_byte]] = registers[execution_register]
              current_instruction_exec = .NOP
            } else if execution_register >= len(registers) {
              end_execution = true
              fmt.println("error: unknown register")
            }
          case .NAND:
            if !first_step(&execution_step, &execution_register, memory[current_byte]) {
              registers[execution_register] = ~(registers[execution_register] & memory[current_byte])
              current_instruction_exec = .NOP
            } else if execution_register >= len(registers) {
              end_execution = true
              fmt.println("error: unknown register")
            }
          case .OR:
            if !first_step(&execution_step, &execution_register, memory[current_byte]) {
              registers[execution_register] |= memory[current_byte]
              current_instruction_exec = .NOP
            } else if execution_register >= len(registers) {
              end_execution = true
              fmt.println("error: unknown register")
            }
          case .JMP:
            current_byte = int(memory[current_byte]) - 1
            change_byte_timer = 1
            current_instruction_exec = .NOP
          case .JZR:
            if !first_step(&execution_step, &execution_register, memory[current_byte]) {
              if registers[execution_register] == 0 {
                current_byte = int(memory[current_byte]) - 1
                change_byte_timer = 1
              }
              current_instruction_exec = .NOP
            } else if execution_register >= len(registers) {
              end_execution = true
              fmt.println("error: unknown register")
            }
          case .NOP:
          }
        } else {
          current_instruction_exec = InstructionType(memory[current_byte])
        }
        current_byte += 1
        change_byte_timer = 1
      }
      if execution_auto {
        if execution_auto_timer > 0 {
          execution_auto_timer -= dt * 20
        } else {
          execution_auto_timer = 1
        }
      }
      if end_execution {
        execution_mode = false
        execution_step = 0
        execution_auto = false
        execution_auto_timer = 0
      }
    }
    if previous_byte != current_byte && change_byte_timer > 0 {
      change_byte_timer -= dt * BYTE_BOX_CHANGE_SPEED
    } else {
      previous_byte = current_byte
    }

    rl.BeginDrawing()
      rl.BeginTextureMode(rendertex)
        for y in f32(-1)..<WINDOW_H/BG_RECT_SIZE {
          for x in f32(-1)..<WINDOW_W/BG_RECT_SIZE {
            rl.DrawRectangleV([2]f32{x, y} * BG_RECT_SIZE + bg_offset, [2]f32{BG_RECT_SIZE, BG_RECT_SIZE}, bg_rect_color[int(x+y) & 1])
          }
        }
      rl.EndTextureMode()
      rl.BeginShaderMode(vignette_sh)
        rl.DrawTextureRec(rendertex.texture, {0, 0, WINDOW_W, -WINDOW_H}, {}, {0xff, 0xff, 0xff, 0xff})
      rl.EndShaderMode()
      BOX_COLOR :: rl.Color{0xd9, 0x57, 0x63, 0xff}
      BOX_COLOR_SHADOW :: rl.Color{0xac, 0x32, 0x32, 0xff}
      BOX_COLOR_SELECTED :: rl.Color{0xd7, 0x7b, 0xba, 0xff}
      BOX_COLOR_SELECTED_SHADOW :: rl.Color{0x76, 0x42, 0x8a, 0xff}
      pos := BYTE_BOX_POS + {0, -smoothlerp(f32(current_byte), f32(previous_byte), change_byte_timer) * (BYTE_BOX_SIZE + BYTE_BOX_GAP)}
      for byte, i in memory {
        size := f32(BYTE_BOX_SIZE)
        if previous_byte != current_byte && i == previous_byte do size = smoothlerp(f32(BYTE_BOX_SIZE), f32(BYTE_BOX_SIZE_SELECTED), change_byte_timer)
        box_color := BOX_COLOR
        box_color_shadow := BOX_COLOR_SHADOW
        selected := selected_address.is_selected && selected_address.address == u8(i)
        if selected {
          box_color = BOX_COLOR_SELECTED
          box_color_shadow = BOX_COLOR_SELECTED_SHADOW
        }
        if i == current_byte {
          size = smoothlerp(f32(BYTE_BOX_SIZE_SELECTED), f32(BYTE_BOX_SIZE), change_byte_timer)
          if instruction_mode && rl.IsKeyPressed(.SPACE) {
            if selected {
              box_color = BOX_COLOR
              box_color_shadow = BOX_COLOR_SHADOW
              selected_address.is_selected = false
            } else {
              box_color = BOX_COLOR_SELECTED
              box_color_shadow = BOX_COLOR_SELECTED_SHADOW
              selected_address.is_selected = true
              selected_address.address = u8(i)
            }
          }
        }
        sizev := [2]f32{size, size}
        rl.DrawRectangleV(pos + 16 - sizev * 0.5, sizev, box_color_shadow)
        rl.DrawRectangleV(pos - sizev * 0.5, sizev, box_color)
        address_txt := int_to_cstring(i)
        address_size: f32 = 64 * SCALE
        address_dim := rl.MeasureTextEx(monocraft_fnt, address_txt, address_size, 0)
        rl.DrawTextEx(monocraft_fnt, address_txt, pos - {size * 0.5 + address_dim.x, address_dim.y * 0.5}, address_size, 0, rl.WHITE)
        byte_txt: cstring
        byte_size_min: f32
        byte_size_max: f32
        byte_size_min = 48 * SCALE
        byte_size_max = 64 * SCALE
        if memory_is_instruction[i] {
          byte_txt = instruction_to_string(InstructionType(byte))
        } else {
          switch memory_display_mode {
          case .BINARY:
            byte_txt = byte_bin_to_cstring(byte)
            byte_size_min = 32 * SCALE
            byte_size_max = 40 * SCALE
          case .HEXADECIMAL:
            byte_txt = byte_hex_to_cstring(byte)
          case .DECIMAL:
            byte_txt = byte_dec_to_cstring(byte)
          }
        }
        byte_size := byte_size_min
        if previous_byte != current_byte && i == previous_byte do byte_size = smoothlerp(byte_size_min, byte_size_max, change_byte_timer)
        if i == current_byte do byte_size = smoothlerp(byte_size_max, byte_size_min, change_byte_timer)
        byte_dim := rl.MeasureTextEx(monocraft_fnt, byte_txt, byte_size, 0)
        if insert_mode && i == current_byte && !memory_is_instruction[i] {
          w := byte_size/2
          dim := [2]f32{w, 8.0/6.0 * w}
          rl.DrawRectangleV(pos + {-byte_dim.x * 0.5 + (w * f32(insert_cursor)), -dim.y * 0.5}, dim, {0x00, 0x00, 0x00, 0x66})
        }
        rl.DrawTextEx(monocraft_fnt, byte_txt, pos - byte_dim * 0.5, byte_size, 0, rl.WHITE)
        pos.y += size + BYTE_BOX_GAP
      }
      if insert_mode do rl.DrawTextEx(monocraft_fnt, "INSIRA", {0, WINDOW_H-(64 * SCALE)}, 64 * SCALE, 0, rl.WHITE)
      if execution_mode do rl.DrawTextEx(monocraft_fnt, "EXECUTANDO", {0, WINDOW_H-(64 * SCALE)}, 64 * SCALE, 0, rl.WHITE)
      if instruction_mode {
        str_buf[len(fmt.bprintf(str_buf[:], "INSTRUCAO:%s", instruction_to_string(current_instruction_mode)))] = 0
        rl.DrawTextEx(monocraft_fnt, cstring(raw_data(str_buf[:])), {}, 64 * SCALE, 0, rl.WHITE)
      }
      if jump_mode {
        jump_address_txt := byte_dec_to_cstring(jump_to_memory_address)
        jump_address_pos := [2]f32{0, WINDOW_H-(64 * SCALE)}
        jump_address_size: f32 = 64 * SCALE
        jump_address_dim := rl.MeasureTextEx(monocraft_fnt, jump_address_txt, jump_address_size, 0)
        w := jump_address_size/2
        dim := [2]f32{w, 8.0/6.0 * w}
        rl.DrawRectangleV(jump_address_pos + {(w * f32(insert_cursor)), dim.y * 0.5 - 5}, dim, {0xff, 0xff, 0xff, 0x33})
        rl.DrawTextEx(monocraft_fnt, jump_address_txt, jump_address_pos, jump_address_size, 0, rl.WHITE)
      }
      REG_BOX :: [2]f32{272 * SCALE, 64 * SCALE}
      for reg, i in registers {
        pos := [2]f32{WINDOW_W - REG_BOX.x - 32, 32 + f32(i) * (REG_BOX.y + 32)}
        rl.DrawRectangleV(pos + 10, REG_BOX, BOX_COLOR_SHADOW)
        rl.DrawRectangleV(pos, REG_BOX, BOX_COLOR)
        str_buf[0] = 'R'
        str_buf[1] = '0' + u8(i)
        str_buf[2] = 0
        txt := cstring(raw_data(str_buf[:]))
        dim := rl.MeasureTextEx(monocraft_fnt, txt, REG_BOX.y, 0)
        rl.DrawTextEx(monocraft_fnt, txt, pos - {dim.x + 10, 0}, REG_BOX.y, 0, rl.WHITE)
        reg_txt: cstring
        switch register_display_mode {
        case .BINARY:
          reg_txt = byte_bin_to_cstring(reg)
        case .HEXADECIMAL:
          reg_txt = byte_hex_to_cstring(reg)
        case .DECIMAL:
          reg_txt = byte_dec_to_cstring(reg)
        }
        rl.DrawTextEx(monocraft_fnt, txt, pos + {(REG_BOX.x - rl.MeasureTextEx(monocraft_fnt, reg_txt, REG_BOX.y, 0).x) * 0.5, 0}, REG_BOX.y, 0, rl.WHITE)
      }
    rl.EndDrawing()
  }
}
