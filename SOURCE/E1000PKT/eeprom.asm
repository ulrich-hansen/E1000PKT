;

; EEPROM API

include 82544.inc
include def.inc
include segms.inc
include eeprom.inc 

.386

_TEXT   segment para public USE16 'CODE'

extrn   IOBase:word
extrn   mac_type:byte
extrn   Wait_10:near
_TEXT   ends

_INIT   segment para public USE16 'CODE'

if DEBUG
extrn print_str:near
endif

; Procedure:    init_eeprom
; Description:  check if eeprom present and store eeprom IA in buffer
; Enter:        di - buffer address
; Exit:         carry set if any error

        public  init_eeprom
init_eeprom proc near
        DMESSAGE 2, "EEPROM init called"
        
        cmp   mac_type, i82541
        jae   no_size_needed
        call  get_ee_size
        mov   address_bitmask, ax
no_size_needed:    
        mov   bx, EE_INIT1
        call  read_ee_word
        jc    bad_eeprom
        shr   ax, EE_SIGN_SHIFT
        cmp   ax, EE_SIGN                ; Check if EEPROM valid
        jne   short bad_eeprom 

        mov   cx, EEPROM_SIZE_64         ; Read first 64 words for
                                         ; eeprom eeprom checksum calculation
        xor   bx, bx                     ; bx contane a eeprom address
Read_next_word:        
        push  cx
        push  bx
        call  read_ee_word
        jc    bad_eeprom
        add   checksum, ax
        pop   bx
        pop   cx
        inc   bx
        loop  Read_next_word
        cmp   checksum, EE_CHECK_SUM_VALUE
        jne   short bad_eeprom

        mov   cx, EADDR_LEN/2
        xor   bx, bx
Read_next_IA:        
        push  cx
        push  bx
        call  read_ee_word
        jc    bad_eeprom
        mov   [di], ax
        pop   bx
        pop   cx
        inc   bx
        add   di,2
        loop  Read_next_IA
        clc
        ret 
bad_eeprom:
        DMESSAGE 2,"EEPROM init FAILED!!!"
        stc
        ret
init_eeprom endp



; Procedure:    get_ee_size
; Description:  return address mask for eeprom operations
; Enter:        
; Exit:         ax - address bitmask
        public  get_ee_size
get_ee_size proc  near
        DMESSAGE 3, "Get eeprom size"
        
        LOAD_REG E1000_EECD
        in    eax,dx
        mov   bx, ax
        mov   ax, EEPROM_SIZE_64
        test  bx, E1000_EE_SIZE
        jz    size64
        mov   ax, EEPROM_SIZE_256
size64:        
        ret        
get_ee_size endp

; Procedure:    read_ee_word
; Description:  read eeprom word
; Enter:        bx - address to read
; Exit:         ax - eeprom value
        public  read_ee_word
read_ee_word proc  near
        DMESSAGE 3,"Read EEPROM word"
        
        cmp     mac_type, i82541
        jae     read_word
        call    grant_access            ; request access to eeprom
        jc      cant_access             ; exit with carry if not
        
        call    write_read_opcode
        mov     cx, address_bitmask     ; load eeprom address bitmask
next_add_bit:
        shr     cx,1
        and     al, not E1000_EEDI
        test    bl, cl
        je      short next_add_bit_1
        or      al,E1000_EEDI           ; if the bit was a one, bring it in.
next_add_bit_1:
        call    write_eeprom_bit        ; store the address bit
        cmp     cx,1
        ja      next_add_bit            

eeprom_address_done:                    ; Now ready to read the EEPROM data
        xor     si, si                  ; si will be temporaly storage for data
        mov     cx, 16                  ; Number of bits to fetch
get_next_data_bit:
        call    read_eeprom_bit         
        and     bx, E1000_EEDO          ; get bit value
        or      si, bx                  ; Add the bit to the assembled byte
        rol     si, 1                   ; Advance byte ready for next bit
        loop    get_next_data_bit       ; Read next data eeprom bit
        ror     si, 4                   ; Get real eeprom value
        call    end_access
        mov     ax, si
        clc
cant_access:
        ret

read_word:
        DMESSAGE 3,"Read EEPROM through EEPROM Read register"
        LOAD_REG E1000_EERD
        mov     ax, bx
        shl     ax, E1000_EERD_ADDR_SHIFT
        or      ax, E1000_EERD_START
        out     dx, eax

        mov     cx, 10
wait_ee_read:
        UsecWait 10
        in      eax,dx
        test    ax, E1000_EERD_DONE
        jnz     ee_read_done
        loop    wait_ee_read
        DMESSAGE 3, "Cannot complete EEPROM read"
        stc
        ret

ee_read_done:
        shr     eax, E1000_EERD_DATA_SHIFT
        clc
        ret
read_ee_word endp


; Procedure:    grant_access
; Description:  request access to EEPROM
; Enter:        
; Exit:         carry clear if granted
grant_access proc near
        DMESSAGE 4, "Request EEPROM access"
        
        LOAD_REG E1000_EECD
        in      eax, dx

        cmp     mac_type, i82544
        jbe     g_a_ok

        or      al, E1000_EE_REQ
        out     dx, eax
        mov     cx, 10000
Wait_access:
        UsecWait 10
        in      eax, dx
        test    al, E1000_EE_GNT
        jnz     g_a_ok
        loop    Wait_access
        DMESSAGE 4,"EEPROM access not granted"
        stc
        ret
g_a_ok:
        and     al, 0f0h
        out     dx, eax
        UsecWait 40
        or      al, E1000_EECS
        out     dx, eax
        UsecWait 40
        clc
        ret
grant_access endp

; Procedure:    end_access
; Description:  complete acccess to EEPROM
; Enter:
; Exit:
end_access proc near
        DMESSAGE 4, "Complete EEPROM access"
        cmp     mac_type, i82544
        jbe     end_a_ok
        and     al, not E1000_EE_REQ
end_a_ok:
        and     al, 0f0h
        out     dx, eax
        UsecWait 40
        ret
end_access endp


; Procedure:    write_read_opcode
; Description:  send read opcode to eeprom
; Enter:        al - data bits
; Exit:
        public  write_read_opcode
write_read_opcode proc near
        or      al, E1000_EEDI                      ; Set a 1 in the data bit
        call    write_eeprom_bit                    ; Write a 1
        call    write_eeprom_bit                    ; Write a 1
        and     al,not E1000_EEDI
        call    write_eeprom_bit                    ; Write a 0
        ret

write_read_opcode endp


; Procedure:    write_eeprom_bit
; Description:  write bit to eeprom
; Enter:        al - data bits
; Exit:         
        public  write_eeprom_bit
write_eeprom_bit proc near

        out     dx,eax
        or      al, E1000_EESK                      ; Set the eeprom strobe
        out     dx, eax
        UsecWait 40                                 ; wait clock up
        and     al, not E1000_EESK                  ; Clear the eeprom strobe
        out     dx, eax
        UsecWait 40                                 ; wait clock down
        ret
write_eeprom_bit endp



; Procedure:    read_eeprom_bit
; Description:  read data bit from eeprom
; Enter:        
; Exit:         bl - data bit
        public  read_eeprom_bit
read_eeprom_bit proc near
        and     al, 0f0h
        or      al, E1000_EECS or E1000_EESK
        out     dx, eax                 ; Write rising edge of strobe to eeprom
        UsecWait 40                     ; wait clock up
       
        in      eax, dx                 ; Get the data bit
        mov     bl, al                  ; Place the result in bl
        and     al, not (E1000_EEDI or E1000_EESK or E1000_EEDO)    ; Clear the eeprom strobe
        out     dx, eax                 ; Write falling edge of strobe to eeprom
        
        UsecWait 40                     ; wait clock down
        ret
read_eeprom_bit endp

checksum        dw 0
address_bitmask dw 0
_INIT   ends
        end
