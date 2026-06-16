; A 'kexec' loader.  An running kernel will load an image into a
; buffer (e.g. by host tether app via 3if protocol over udp) and jump
; to the start of it.  The code will then copy the kernel into the
; requested start location (i.e. 0x7E00) and jump to it.

[BITS 32]

start:
    cli              ; fa
    call get_eip     ; e8 00 00 00 00
get_eip:
    pop esi          ; start + 6
    add esi, 512-6   ; esi now points at boot_config

    mov edx, [esi+0] ; entry
    mov ecx, [esi+4] ; endx
    mov edi, [esi+8] ; start
    sub ecx, edi     ; ecx = length

    cld
    rep movsb

    ; Write ? to top right corner of video memory before jumping.
    mov ebx, 0xB8000 + (2 * 79)
    mov byte [ebx], '?'

    ; Reset the stack then jump to the kernel entry point
    mov eax, 0x7C00
    mov esp, eax
    jmp edx


; Use the same marker as floppy bootsector.  This also pads the bin
; output file to 512 bytes.
    times 510 - ($ - $$) db 0
    dw 0xaa55
