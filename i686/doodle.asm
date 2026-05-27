
        ;;  This doesn't work for floppy
read_hdd:

    mov cx, 100                      ; number of sectors 

.read_loop:
    ; Compute how many sectors fit before next 64K boundary
    ;mov al, '.'
    ;call print_char
    mov ax, cx
    cmp ax, 127
    jbe .ok
    mov ax, 127
.ok:
    mov [dap_count], ax
    sub cx, ax

    mov si, dap
    mov ah, 0x42
    mov dl, 0x80                     ; first hdd
    int 0x13
    jc  error

    ; Advance LBA
    movzx eax, word [dap_count]
    add [dap_lba], eax

    ; Advance segment (each sector = 512 bytes = 0x20 paragraphs)
    shl ax, 5                ; sectors * 32 paragraphs
    add [dap_segment], ax

    test cx, cx
    jnz .read_loop

    jmp load_done

error:
    call print_hex
    mov si, msg_disk_error
    call print_string_nl
    call spin

spin:
    hlt
    jmp spin

; ...

dap:          db 0x10          ; DAP size
              db 0             ; reserved
dap_count:    dw 0             ; number of sectors
              dw 0x0000        ; offset (always 0, we advance via segment)
dap_segment:  dw 0x07E0
              dw 0             ; padding for qword alignment
dap_lba:      dq 1             ; LBA

