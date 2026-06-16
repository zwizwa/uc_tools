; A 'kexec' loader.  An existing kernel will load an image into a
; buffer and jump to the start of it.  The code will then copy the
; kernel to 0x7C00 and jump to it.  It seems simplest to make this
; code self-locating so we can just jump into the image.


[BITS 32]

start:
    cli           ; fa
    call get_eip  ; e8 00 00 00 00
get_eip:
    pop esi

    mov ebx, 0xB8000 + (2 * 79)
    mov byte [ebx], '?'

    add esi, 512-6      ; esi is now the first address of the kernel
    mov ecx, 14734-512
    mov edi, 0x7E00
    cld
    rep movsb
    mov edx, [0x7E00]
    jmp edx


; Use the same marker as floppy bootsector.  This also pads the bin
; output file to 512 bytes.
    times 510 - ($ - $$) db 0
    dw 0xaa55
