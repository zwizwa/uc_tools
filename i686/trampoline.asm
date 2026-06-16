; A 'kexec' loader.  An existing kernel will load an image into a
; buffer and jump to the start of it.  The code will then copy the
; kernel to 0x7C00 and jump to it.
[BITS 32]

start:
    cli
    mov ebx, 0xB8000 + (2 * 79)
    mov byte [ebx], '?'

busy:
    jmp busy

    mov esi, [esp+4]    ; start of the kernel (boot block)
    mov ecx, [esp+8]    ; length
    add esi, 512        ; skip the boot block
    sub ecx, 512
    mov edi, 0x7E00
    cld
    rep movsb
    mov edx, [0x7E00]
    jmp edx


; Use the same marker as floppy bootsector.  This also pads the bin
; output file to 512 bytes.
    times 510 - ($ - $$) db 0
    dw 0xaa55
