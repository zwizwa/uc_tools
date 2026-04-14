; Loaded at 0x7C00 by PXE firmware in 16-bit real mode
; Or just the first sector loaded at 0x7C00 by BIOS floppy boot.  

; I don't think I need BIOS for anything other than initial message
; display, floppy/disk load, then switch to protected mode and use raw
; access.  If network is needed I'll have to write a driver using
; direct I/O.
;
; Leave the console I/O in there for debugging or interaction with the
; loader.
;
; I think I just want the loader to jump into C at 0x7E00.


[BITS 16]                       ; 16-bit real mode
[ORG 0x7C00]                    ; PXE loads NBP at 0x7C00

; ----- Entry Point -----
start:
    
    cli                         ; Disable interrupts during setup
    xor ax, ax
    mov ds, ax                  ; Data segment = 0
    mov es, ax                  ; Extra segment = 0
    mov ss, ax                  ; Stack segment = 0
    mov sp, 0x7C00              ; Stack grows down from load address
    sti                         ; Re-enable interrupts

    mov si, msg_banner
    call print_string

    mov si, msg_newline
    call print_string

    mov si, msg_prompt
    call print_string
                                                                                           ; First enable the A20 line
    ; https://claude.ai/chat/f4693772-1425-4e1f-a287-f974e0cff4ac
    mov ax, 0x2401
    int 0x15
    jnc .a20_ok
    ret
    mov si, msg_a20_failed
    call print_string
.a20_ok:

    ;; In case anything goes wrong start the console input.
    ; jmp input_loop

%ifdef FLOPPY
   jmp load
%else
   jmp load_done
%endif



;;; This currently only works for the first track.  I don't really
;;; need it yet: all hosts I want to run this on support netboot or
;;; can be made to netboot using an ipxe floppy, including qemu.

;;; It seems simpler to use extended read call ah=0x42
;;; https://claude.ai/chat/4229f038-c4b4-4c99-89c8-506a306f3b0a

load:
    ; reset floppy
    xor ax, ax
    int 0x13

    mov bx, 0x7E0           ; after bootsector
    mov cl, 2               ; start at sector 2 (sector after boot sector)
    mov al, 17              ; first read sector count (others are 18)
    mov ch, 0               ; cylinder 0
    mov dh, 0               ; head 0
    call load_track
    call load_track
    call load_track
      
    jmp 0x0000:load_done    ; jump to loaded code

load_track:
    push bx
    mov es, bx
    xor bx, bx              ; es:bx = destination
    mov dl, 0               ; drive 0 (floppy A:)
    mov ah, 0x02            ; BIOS read sectors
    int 0x13
    jc error
    pop bx

;;; advance segment in bx
    push ax
    push cx
    push dx
    mov cx, 0x20
    mov ah, 0
    mul cx                      
    add bx, ax                  ; bx = bx + 0x20 * nb_sec
    pop dx
    pop cx
    pop ax

    mov al, 18                  ; next track nb_sec always the same
    mov cl,  1                  ; start sector always 1
    xor dh,  1                  ; flip head
    jz .no_inc_cyl              ; if 1->0 inc cyl
    inc ch
.no_inc_cyl:
    ret

error:
    mov si, msg_disk_error
    call print_string
.hang:
    hlt
    jmp .hang

; print_string: Print a null-terminated string pointed to by DS:SI
print_string:
    pusha
.loop:
    lodsb                       ; Load byte at DS:SI into AL, increment SI
    or al, al                   ; Check for null terminator
    jz .done
    call print_char_inner
    jmp .loop
.done:
    popa
    ret

; print_char: Print a single character in AL using BIOS teletype output
print_char:
    pusha
    call print_char_inner
    popa
    ret

print_char_inner:
    mov ah, 0x0E                ; INT 10h, AH=0Eh: Teletype output
    mov bh, 0x00                ; Page number 0
    mov bl, 0x07                ; Light gray on black (attribute for graphics modes)
    push ds
    push ax
    xor ax,ax
    mov ds, ax
    pop ax
    int 0x10
    pop ds
    ret


; ----- Main Input Loop -----
input_loop:

    ; Use hlt to avoid the busyloop.  This makes qemu more efficient.
    sti
    hlt                ; sleep until any interrupt fires
    mov ah, 0x01       ; int 0x16 ah=1: check if key available (non-blocking)
    int 0x16
    jz input_loop

    ; Wait for a keypress (blocking)
    ; INT 16h, AH=00h: Wait for key
    ;   Returns: AH = scan code, AL = ASCII character
    xor ah, ah
    int 0x16

    ; Check for ESC key (ASCII 0x1B)
    cmp al, 0x1B
    je do_reboot

    ; Check for Enter key (ASCII 0x0D)
    cmp al, 0x0D
    je do_enter

    ; Check for Backspace (ASCII 0x08)
    cmp al, 0x08
    je do_backspace

    ; Check for DEL (ASCII 0x7F)
    cmp al, 0x7f
    je do_backspace

    ; Ignore non-printable characters (below space)
    cmp al, 0x20
    jb input_loop

    ; Echo the printable character to the screen
    call print_char

    jmp input_loop

; ----- Handle Enter Key -----
do_enter:
    ; Print newline (CR+LF)
    mov si, msg_newline
    call print_string

    ; Print a new prompt
    mov si, msg_prompt
    call print_string

    jmp input_loop

; ----- Handle Backspace -----
do_backspace:
    ; Move cursor back, print space (erase), move cursor back again
    mov al, 0x08
    call print_char
    mov al, ' '
    call print_char
    mov al, 0x08
    call print_char

    jmp input_loop

; ----- Reboot -----
do_reboot:
    mov si, msg_newline
    call print_string

    ; Reboot via BIOS: jump to reset vector
    jmp 0xFFFF:0x0000

msg_banner:
    db 'zpxe!', 0
msg_prompt:
    db '> ', 0
msg_newline:
    db 0x0D, 0x0A, 0    
msg_a20_failed:
    db "A20 enable failed", 0x0D, 0x0A, 0
msg_disk_error:
    db "disk error", 0


load_done:

    mov ah, 02h       ; Set cursor position
    mov bh, 00h       ; Page 0
    mov dh, 25        ; Row 25 (off-screen in 80x25 mode)
    mov dl, 00h       ; Column 0
    int 10h

    ; Load the GDT
    lgdt [gdt_descriptor]

    ; Enter protected mode
    cli                     ; Disable interrupts
    mov eax, cr0
    or eax, 1               ; Set PE (Protection Enable) bit
    mov cr0, eax

    ; Load 32-bit segment limits
    mov ax, 0x10            ; Selector for the data segment in the GDT
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax

    jmp 0x08:protected_mode

[BITS 32]
protected_mode:

    ; https://claude.ai/chat/c083247d-58de-4198-88e8-d762f99823a3

    ; The 8259 PIC maps IRQ 0–7 to interrupts 0x08–0x0F by default,
    ; which collides with CPU exceptions in protected mode (e.g., IRQ
    ; 0 = timer hits the same vector as Double Fault). They need to be
    ; remapped.


    ; Remap PIC1 (master) to 0x20-0x27, PIC2 (slave) to 0x28-0x2F
    mov al, 0x11       ; ICW1: init + ICW4 needed
    out 0x20, al       ; master PIC command
    out 0xA0, al       ; slave PIC command

    mov al, 0x20       ; ICW2: master starts at 0x20
    out 0x21, al
    mov al, 0x28       ; ICW2: slave starts at 0x28
    out 0xA1, al

    mov al, 0x04       ; ICW3: master has slave on IRQ2
    out 0x21, al
    mov al, 0x02       ; ICW3: slave cascade identity
    out 0xA1, al

    mov al, 0x01       ; ICW4: 8086 mode
    out 0x21, al
    out 0xA1, al

    ; Mask all IRQs except IRQ1 (keyboard)
;    mov al, 0xFD       ; 1111 1101 — only IRQ1 unmasked
    mov al, 0xFF        ; mask all on master
    out 0x21, al

    mov al, 0xFF       ; mask all on slave
    out 0xA1, al


; FIXME: set up IDT

    jmp kernel



; Global Descriptor Table (GDT)
gdt:
    ; Null descriptor
    dw 0x0000, 0x0000, 0x0000, 0x0000
    ; Code segment descriptor (base=0, limit=4GB, code segment, read/execute)
    dw 0xFFFF, 0x0000, 0x9A00, 0x00CF
    ; Data segment descriptor (base=0, limit=4GB, data segment, read/write)
    dw 0xFFFF, 0x0000, 0x9200, 0x00CF

gdt_descriptor:
    dw gdt_end - gdt - 1    ; Limit of GDT
    dd gdt                  ; Base address of GDT

gdt_end:


    times 510 - ($ - $$) db 0  ; Pad to floppy bootsector size
    dw 0xaa55                  ; Bootable marker


; Dummy kernel just for testing.


kernel:
%ifdef  DUMMY_KERNEL
    mov ebx, 0xB8000
    mov ecx, 80*25
.fill_screen:
    mov byte [ebx], '.'
    inc ebx
    mov byte [ebx], 0x0F
    inc ebx
    dec ecx
    jnz .fill_screen
busy:
    sti
    jmp busy
%endif



