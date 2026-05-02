; Loaded at 0x7C00 by PXE firmware in 16-bit real mode
; Or just the first sector loaded at 0x7C00 by BIOS floppy boot.  


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

    ; https://claude.ai/chat/22c77dcf-e200-42e0-8b70-f9fabd27b804
    ; different text mode
    ;mov ax, 0x4F02
    ;mov bx, 0x010C   ; 0109, 010A, 010B, 010C
    ;int 0x10

    ;mov ax, 0x0003   ; set 80x25 color text first
    ;int 0x10
    ;mov ax, 0x1112   ; load 8x8 ROM font, recalc CRTC for 50 rows
    ;mov bl, 0x00
    ;int 0x10

    mov si, msg_banner
    call print_string_nl

    ; First enable the A20 line
    ; https://claude.ai/chat/f4693772-1425-4e1f-a287-f974e0cff4ac
    ; FIXME: This works on qemu but not on my 2 real machines.
    mov ax, 0x2401
    int 0x15
    jnc .a20_ok
    mov si, msg_a20_failed
    call print_string_nl
    ; It fails on hardware.  Not an issue for now since kernel is still small.
.a20_ok:


%ifndef FLOPPY
    jmp load_done

%else
    ; reset floppy
    xor ax, ax
    int 0x13

    ; read a number of tracks
    mov ax, 0
    mov cx, 10   ; nb tracks
next_track:
    call load_track
    add ax, 1
    sub cx, 1
    jnz next_track

    mov si, msg_newline
    call print_string

    ; Turn off the floppy motor
    ; Bit 2 = controller enable, bit 3 = DMA enable
    ; Bits 4-7 = motor enable for drives 3-0 (all off)
    mov dx, 3F2h
    mov al, 0Ch     
    out dx, al
    ; Claude had some more notes about making sure there are no spurious interrupts.
    ; https://claude.ai/chat/936c13d4-8692-4c76-a0fe-96919f65f6ec

    jmp load_done
 


    ; FIXME: Can't cross DMA boundary, but can't easily keep the overwrite either.
    ; I did see this work on the X230 BIOS.
    ; Not an issue for now since kernel is still small.

    ; So maybe use an explicit destination for the data:
    ; load track 0 at 0x7C00
    ; load other tracks at e.g. 0x500 and copy to destination


    ; Use track addressing
    ; This re-loads 0x7C00 to keep code simpler.
load_track:
    push cx
    push bx
    push ax
    push ax
    mov dh, al
    and dh, 1      ; head
    shr ax, 1
    mov ch, al     ; cylinder number
    pop ax
    mov bx, 32*17  ; segments per sector * nb_sectors
    mul bx
    add ax, 0x7C0  ; base segment

    call print_hex_word

    mov es, ax     ; es:dx is destination
    xor bx, bx
    mov cl, 1      ; start at sector 1
    mov al, 17     ; read 17 sectors
    mov dl, 0      ; drive A
    mov ah, 0x02   ; BIOS read sectors
    int 0x13
    call print_hex_word
    ; jc disk_error
    mov si, msg_newline
    call print_string
    pop ax
    pop bx
    pop cx
    ret

disk_error:
    push ax
    mov si, msg_newline
    call print_string
    pop ax
    call print_hex_byte
    mov si, msg_disk_error   
    call print_string_nl
    call spin

spin:   
    hlt
    jmp spin

%endif

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

print_nibble:
    and ax, 0xF
    push si
    mov si, hex_table
    add si, ax
    lodsb
    pop si
    jmp print_char

print_hex_byte:
    push ax
    shr ax, 4
    call print_nibble
    pop ax
    call print_nibble
    ret

hex_table:
    db "0123456789ABCDEF"

print_hex_word:
    push ax
    push ax
    shr ax, 8
    call print_hex_byte
    pop ax
    call print_hex_byte
    pop ax
    ret

print_string_nl:        
    call print_string
    mov si, msg_newline
    call print_string
    ret



msg_newline:
    db 0x0D, 0x0A, 0    
msg_banner:
    db 'booting', 0
msg_a20_failed:
    db "!A20", 0
msg_disk_error:
    db "!disk", 0






load_done:

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

    ; Jump into 32bit code segment.
    jmp 0x08:protected_mode


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

[BITS 32]
protected_mode: 
    mov ebx, 0xB8000 + (2 * 79)
    mov byte [ebx], '?'
    jmp kernel

    times 510 - ($ - $$) db 0  ; Pad to floppy bootsector size
    dw 0xaa55                  ; Bootable marker


kernel:

%ifdef  TESTKERNEL

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



