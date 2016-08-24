;
; Packet Driver API

include segms.inc
include def.inc
include 82544.inc
include eeprom.inc

.386

_TEXT   segment para public USE16 'CODE'
        org    100h

extrn   enable_rx:near
extrn   enable_tx:near
extrn   disable_rx:near
extrn   init_rx_addrs:near
extrn   enable_board_int:near
extrn   disable_board_int:near

start:
        jmp     pkt_init
even

dw STACK_SIZE dup(?)
         public our_stack
our_stack label word
align  4
old_stack label dword
        oldsp   dw ?
        oldss   dw ?
        
even
functions       label   word
        dw      f_not_implemented       ;0
        dw      f_driver_info           ;1
        dw      f_access_type           ;2
        dw      f_release_type          ;3
        dw      f_send_pkt              ;4
        dw      f_terminate             ;5
        dw      f_get_address           ;6
        dw      f_reset_interface       ;7
        dw      f_not_implemented       ;8
        dw      f_not_implemented       ;9
        dw      f_get_parameters        ;10
        dw      f_as_send_pkt           ;11
        dw      f_not_implemented       ;12
        dw      f_not_implemented       ;13
        dw      f_not_implemented       ;14
        dw      f_not_implemented       ;15
        dw      f_not_implemented       ;16
        dw      f_not_implemented       ;17
        dw      f_not_implemented       ;18
        dw      f_not_implemented       ;19
        dw      f_set_rcv_mode          ;20
        dw      f_get_rcv_mode          ;21
        dw      f_set_multicast_list    ;22
        dw      f_get_multicast_list    ;23
        dw      f_get_statistics        ;24
        dw      f_set_address           ;25


extrn reset_chip:near
extrn Wait_10:near

include print.asm

        public  __pktdrv

__pktdrv proc far
        jmp     short StartDriver        
        nop
        db      'PKT DRVR',0              ;the required signature.

StartDriver:

        PUSH_16_32      ax
        PUSH_16_32      bx
        PUSH_16_32      cx
        PUSH_16_32      dx
        PUSH_16_32      si
        PUSH_16_32      di
        PUSH_16_32      bp
        push            ds
        push            es

        cld
        mov     bp,sp                     ;we use bp to access the original regs.
        and     (regs ptr [bp])._F,not CY ;start by clearing the carry flag.

        mov     bl,ah                     ;jump to the correct function.
        xor     bh,bh
        cmp     bx,25                     ;only twenty five functions right now.
        ja      f_bad_command
        shl     bx,1                      

        mov     ax,cs
        mov     ds,ax

        call    functions[bx]
        jc      pktdrv_error

if DEBUG
        to_scr  ' '
endif

exit_driver:
        pop             es
        pop             ds
        POP_16_32       bp
        POP_16_32       di
        POP_16_32       si
        POP_16_32       dx
        POP_16_32       cx
        POP_16_32       bx
        POP_16_32       ax

        iret


f_bad_command:
        mov     dh, BAD_COMMAND

pktdrv_error:

if DEBUG
        to_scr  'X'
endif

        mov     (bytes ptr [bp])._DH,dh
        or      (regs ptr[bp])._F,CY               
        jmp     exit_driver

__pktdrv endp



f_not_implemented:
        mov     dh,BAD_COMMAND
        stc
        ret

f_driver_info proc near

        cmp     (bytes ptr[bp])._AL,0ffh        ; check calling convention
        jne     bad_info_call                 

        mov     al, driver_class
        mov     (bytes ptr [bp])._CH,al

        mov     (regs ptr [bp])._BX,VERSION      ;version
        mov     ax, driver_type
        mov     (regs ptr [bp])._DX,ax
        mov     al, driver_number
        mov     (bytes ptr [bp])._CL,al          ;number
        mov     (regs ptr [bp])._DS,ds           ;pointer to our name in their ds:si
        mov     (regs ptr [bp])._SI,offset driver_name
        mov     al, driver_function
        mov     (bytes ptr [bp])._AL,al
        clc
        ret
bad_info_call:
        stc
        ret
f_driver_info endp


access_type_class:
        mov     dh,NO_CLASS
        stc
        ret

access_type_type:
        mov     dh,NO_TYPE
        stc
        ret

access_type_number:
        mov     dh,NO_NUMBER
        stc
        ret

access_type_bad:
        mov     dh,BAD_TYPE
        stc
        ret

access_type_space:
        mov     dh,NO_SPACE
        stc
        ret


;register caller of pkt TYPE
f_access_type proc near
        mov     bx, offset CGROUP:driver_class
access_type_9:
        mov     al, [bx]                ;get the next class.
        inc     bx
        or      al,al                   ;end of the list?
        je      access_type_class       ;class failed (story of my life)
        cmp     (bytes ptr[bp])._AL,al  ;our class?
        jne     access_type_9           ;no, try again
access_type_1:
        cmp     (regs ptr[bp])._BX,-1   ;generic type?
        je      access_type_2           ;yes.
        mov     ax,driver_type
        cmp     (regs ptr[bp])._BX,ax              ;our type?
        jne     access_type_type        ;no.
access_type_2:
        cmp     (bytes ptr[bp])._DL,0               ;generic number?
        je      access_type_3
        cmp     (bytes ptr[bp])._DL,1               ;our number?
        jne     access_type_number
access_type_3:
        cmp     (regs ptr[bp])._CX,MAX_P_LEN       ;is the type length too long?
        ja      access_type_bad         ;yes - can't be ours.

; now we do two things--look for an open handle, and check the existing
; handles to see if they're replicating a packet type.

        mov     free_handle,0           ;remember no free handle yet.
        mov     bx,offset handles
access_type_4:
        cmp     (per_handle ptr[bx]).in_use,0           ;is this handle in use?
        je      access_type_5           ;no - don't check the type.
        mov     al, (bytes ptr[bp])._AL             ;is this handle the same class as
        cmp     al, (per_handle ptr[bx]).class          ;  they're want?
        jne     short access_type_6
        mov     es,(regs ptr[bp])._DS              ;get a pointer to their type
        mov     di,(regs ptr[bp])._SI              ;  from their ds:si to our es:di
        mov     cx,(regs ptr[bp])._CX              ;get the minimum of their length
                                        ;  and our length.  As currently
                                        ;  implemented, only one receiver
                                        ;  gets the packets, so we have to
                                        ;  ensure that the shortest prefix
                                        ;  is unique.
        cmp     cx,(per_handle ptr[bx]).packet_type_len ;Are we less specific than they are?
        jb      access_type_8           ;no.
        mov     cx,(per_handle ptr[bx]).packet_type_len ;yes - use their count.
access_type_8:
        lea     si,(per_handle ptr[bx]).packet_type
        or      cx,cx                   ; pass-all TYPE? (zero TYPE length)
        jne     access_type_7           ; ne = no
        mov     bx,offset CGROUP:handles+(MAX_HANDLE-1)*(size per_handle)
        jmp     short access_type_5     ; put pass-all last
access_type_7:
        repe    cmpsb
        jne     short access_type_6     ;go look at the next one.
access_type_inuse:
        mov     dh,TYPE_INUSE           ;a handle has been assigned for TYPE
        stc                             ;and we can't assign another
        ret
access_type_5:                          ;handle is not in use
        cmp     free_handle,0           ;found a free handle yet?
        jne     access_type_6           ;yes.
        mov     free_handle,bx          ;remember a free handle
access_type_6:
        add     bx,(size per_handle)    ;go to the next handle.
        cmp     bx,offset CGROUP:end_handles    ;examined all handles?
        jb      access_type_4           ;no, continue.

        mov     bx,free_handle          ;did we find a free handle?
        or      bx,bx
        je      access_type_space       ;no - return error.

        mov     ax,(regs ptr[bp])._DI              ;remember the receiver type.
        mov     (segmoffs ptr (per_handle ptr[bx]).receiver).offs,ax
        mov     ax,(regs ptr[bp])._ES
        mov     (segmoffs ptr(per_handle ptr[bx]).receiver).segm,ax

        push    ds
        mov     ax,ds
        mov     es,ax
        mov     ds,(regs ptr[bp])._DS              ;remember their type.
        mov     si,(regs ptr[bp])._SI
        mov     cx,(regs ptr[bp])._CX
        mov     es:(per_handle ptr[bx]).packet_type_len,cx      ; remember the TYPE length
        lea     di,(per_handle ptr[bx]).packet_type
        rep     movsb
        pop     ds

        mov     al, (bytes ptr[bp])._AL
        mov     (per_handle ptr[bx]).class, al

        mov     (per_handle ptr[bx]).in_use,1      ;remember that we're using it.

        mov     (regs ptr[bp])._AX,bx              ;return the handle to them.

        clc
        ret
f_access_type endp


f_release_type proc near
        call    verify_handle                      ; check that it is our handle
        jc      f_release_bad_handle
        mov     (per_handle ptr[bx]).in_use,0      ;mark this handle as being unused.
        ret
f_release_type endp

f_release_bad_handle:
        mov      dh, BAD_HANDLE
        ret

align 4
f_send_pkt proc near
;ds:si -> buffer, cx = length

        add2    packets_out,1
        add2    bytes_out,cx            ;add up the transmit bytes.
        mov     al, cmd_opt
        test    al, N_OPTION
        jnz     novell_pro
        call    send_pkt
        ret
novell_pro:
        call    check_novell
        call    send_pkt
        ret
f_send_pkt endp

check_novell proc near
        mov     bx, ds
        mov     ax, 3781h               ; Novell protocol number

        mov     ds, (regs ptr[bp])._DS  ; si already point on packet
	cmp	[si](EADDR_LEN*2), ax   ; if not Novell
	jne	not_novell_pro		;  don't tread on it

	mov	ax, [si](EADDR_LEN*2 + 4) ; get IPX length field
	xchg	ah, al
	inc	ax			; make even (rounding up)
	and	al, 0feh
	xchg	ah, al
	mov	[si](EADDR_LEN*2), ax	; save in type/length field
not_novell_pro:
        mov     ds, bx                  ; restore our data segment
        ret
check_novell endp

align 4
        public  send_pkt
send_pkt proc near

;enter with es:di->upcall routine, (0:0) if no upcall is desired.
;  (only if the high-performance bit is set in driver_function)
;enter with ds:si -> packet, cx = packet length.
;if we're a high-performance driver, es:di -> upcall.
;exit with nc if ok, or else cy if error, dh set to error number.

        cmp     cx, GIANT                       ; Is this packet too large?
        ja      f_send_cant_send
        cmp     cx, RUNT
        jb      f_send_cant_send

if DEBUG
        to_scr  'S'
endif

        call    get_buffer
        jc      f_send_cant_send

;       copy data which need to be send in our buffer 
        mov     ds,(regs ptr[bp])._DS

        mov     ax, cx                          ; Save byte count
        shr     cx, 2                           ; Convert to a DWORD count
        rep     movsd                           ; Copy DWORDS
        mov     cx, ax                          ; Recover byte count
        and     cx, 0003h                       ; Obtain count mod 4
        jz      no_more
        rep     movsb                           ; Copy remaining bytes

no_more:
        call    issue_cmd
if DEBUG
        to_scr  'T'
endif
        clc
        ret

f_send_cant_send:
        add2    errors_out,1
        mov     dh, CANT_SEND 
        stc
        ret
send_pkt endp   

; Procedure:    get_buffer
; Description:  find empty tx buffer
; Enter:        
; Exit:         carry set if no buffers available
;               ds:si point on buffer

align 4
        public get_buffer
get_buffer proc near
        mov   ax, cs
        mov   es, ax
        
        mov   ax, tx_buffer_no
        mov   bx, ax
        inc   ax
        cmp   ax, TX_BUFFER_NUMBER
        jl    BufferOk
        xor   ax, ax

BufferOk:
        mov   tx_buffer_no,ax
        shl   bx, 1
        mov   di, tx_buffer_ptrs[bx]
        shl   bx, 3
        push  cx
        mov   cx, 2000
WaitDescriptorDone:
; chech descriptor done
        mov   al, (Xmit_Descr_Type  ptr [bx+offset TxRing]).tstatus
        test  al, XD_STAT_DD
        jnz   DescriptorDone
        in    al, 61h
        in    al, 61h
        in    al, 61h
        sub   cx, 1
        jnz   WaitDescriptorDone

if TX_HOOK       
        pop   cx
        shr   bx, 4
        mov   tx_buffer_no, bx
        stc
        ret
endif        
        
DescriptorDone:
        pop   cx
        xor   ax, ax
        mov   (Xmit_Descr_Type  ptr [bx+offset TxRing]).tstatus, al
        mov   (Xmit_Descr_Type  ptr [bx+offset TxRing]).t_length,cx
        clc
        ret

get_buffer endp

; Procedure:    issue_cmd
; Description:  Initiate packet transmition
; Enter:
; Exit:

align 4
        public issue_cmd
issue_cmd proc near
        mov   ax, cs
        mov   ds, ax
        pushf
        cli
        LOAD_REG  E1000_TDT
        mov   ax, tx_buffer_no
        out   dx, eax
        popf
        
        ret

issue_cmd endp



f_terminate proc near
        call   verify_handle                     ; check that it is our handle
        jc     f_term_bad_handle

        mov    (per_handle ptr[bx]).in_use,0
        call   count_handles
        or     cl,cl
        jnz    f_term_cant_term

        call   reset_chip
        call   remove_device_isr
        jc     f_term_cant_term

        mov    ah, 25h
        mov    al, com_int
        lds    dx, their_int
        int    21h
        
        push   cs
        pop    es
        mov    ah, 49h
	int    21h
        clc
        ret

f_term_bad_handle:
        mov    dh, BAD_HANDLE
        stc
        ret

f_term_cant_term:
        mov    dh, CANT_TERMINATE
        stc
        ret
        
f_terminate endp


        public remove_device_isr
remove_device_isr proc near
        mov    al, dev_isr
        mov    ah, 35h
        int    21h

        mov    di, bx
        mov    si, offset our_isr_signature

        mov    cx, INT_SIGN_WORDS
        repe   cmpsw
        jnz    remove_err 

        push   ds
        lds    dx, their_recv_isr
        mov    ah, 25h
        int    21h
        pop    ds
        clc
        ret

remove_err:
        stc
        ret

remove_device_isr endp


             
f_get_address proc near
        call    verify_handle                   ;check that it is our handle
        jc      get_address_bad_handle
        
        cmp     cx, EADDR_LEN                   ;is there enough room for our address?
        jb      get_address_no_space            ;no.
        
        mov     cx, EADDR_LEN                   ;yes - get our address length.
        mov     (regs ptr[bp])._CX,cx           ;Tell them how long our address is.
        mov     si,offset  my_eth_addr          ;copy it into their area.
        rep     movsb
        clc
        ret

get_address_bad_handle:
        mov     dh, BAD_HANDLE
        stc
        ret

get_address_no_space:
        mov     dh, NO_SPACE
        stc
        ret
f_get_address   endp


f_reset_interface proc near
        call    verify_handle                    ; check that it is our handle
        jc      f_reset_interface_bad_handle
        call    count_handles
        cmp     cl, 1
        ja      f_reset_interface_cant_reset     ; more that 1 handle open
        call    reset_interface
        clc
        ret

f_reset_interface_bad_handle:
        mov     dh, BAD_HANDLE
        stc
        ret

f_reset_interface_cant_reset:
        mov     dh, CANT_RESET
        stc
        ret
f_reset_interface endp


reset_interface proc near
        mov     ax, ds                           ; first restore our net 
        mov     es, ax                           ; address to eeprom value
        mov     si, offset rom_addr
        mov     di, offset my_eth_addr
        mov     cx, EADDR_LEN
        rep     movsb
        
        sub     si, EADDR_LEN                    ; setup IA
        xor     di, di
        call    set_address
        
        mov     di, offset multicast_table       ; clear on host memory
        xor     ax, ax                           ; multicast table
        mov     multicast_inuse, al                
        mov     cx, MULTICAST_SIZE
        rep     stosb

        mov     bx, BRDCAST                      ; set broadcast receive mode
        mov     rcv_mode, bx
        shl     bx,1
        call    recv_setup[bx]
        ret
reset_interface endp 


f_get_parameters proc near
        mov     (regs ptr[bp])._ES, ds
        mov     (regs ptr[bp])._DI, offset parameter_list
        clc
        ret
f_get_parameters endp


f_as_send_pkt proc near
        add2    packets_out,1
        add2    bytes_out,cx            ;add up the transmit bytes.

        mov     al, cmd_opt
        test    al, N_OPTION
        jz      no_novell     
        call    check_novell
no_novell:

        cmp     cx, GIANT
        ja      f_as_send_pkt_cant_send
        cmp     cx, RUNT
        jb      f_as_send_pkt_cant_send

        mov     (segmoffs ptr upcall_routine).offs, di
        mov     (segmoffs ptr upcall_routine).segm, es
        
        call    get_buffer
        mov     ax, 1
        jc      copy_failed

        mov     ds,(regs ptr[bp])._DS
        mov     ax, cx                          ; Save byte count
        shr     cx, 2                           ; Convert to a DWORD count
        rep     movsd                           ; Copy DWORDS
        mov     cx, ax                          ; Recover byte count
        and     cx, 0003h                       ; Obtain count mod 4
        jz      no_more_copy
        rep     movsb                           ; Copy remaining bytes
no_more_copy:
        xor     ax, ax
copy_failed:
        mov     di, (regs ptr[bp])._SI
        mov     es, (regs ptr[bp])._DS
        or      al,al
        jnz     error_call
        
        call    upcall_routine
        call    issue_cmd
        clc
        ret

error_call:
        add2    errors_out,1
        call    upcall_routine
        clc
        ret

f_as_send_pkt_cant_send:
        add2    errors_out,1
        mov     dh, CANT_SEND
        stc
        ret

f_as_send_pkt  endp


extrn   recv_setup:word
f_set_rcv_mode proc near
        call    verify_handle
        jc      f_set_rcv_mode_bad_handle
        or      cx,cx
        je      f_set_rcv_mode_bad_mode
        mov     bx, RECEIVE_ALL
        test    cmd_opt, P_OPTION
        jz      promiscuous_enable
        dec     bx
promiscuous_enable:
        cmp     cx, bx
        ja      f_set_rcv_mode_bad_mode

        mov     ax, cx                             ; save receive mode
        call    count_handles
        cmp     cl,1
        ja      f_set_rcv_mode_bad_mode

        cmp     ax, rcv_mode
        je      mode_already_set
        mov     rcv_mode, ax
        mov     bx, ax
        shl     bx, 1
        call    recv_setup[bx]
mode_already_set:        
        clc
        ret

f_set_rcv_mode_bad_handle:
        mov     dh, BAD_HANDLE
        stc
        ret

f_set_rcv_mode_bad_mode:
        mov     dh, BAD_MODE
        stc
        ret
f_set_rcv_mode endp

f_get_rcv_mode proc near
        call    verify_handle
        jc      f_get_rcv_mode_bad_handle
        mov     ax, rcv_mode
        mov     (regs ptr[bp])._AX, ax
        clc
        ret
        
f_get_rcv_mode_bad_handle:
        mov     dh, BAD_HANDLE
        stc
        ret
f_get_rcv_mode endp        


f_set_multicast_list proc near
        cmp     cx, MULTICAST_SIZE
        ja      f_set_multicast_no_space
        jcxz    f_set_multicast_bad_address

        mov     ax, cx
        xor     dx, dx
        mov     bx, EADDR_LEN
        div     bx
        or      dx, dx
        jnz     f_set_multicast_bad_address
        mov     multicast_inuse, al

; copy multicast table
        push    ds
        push    es
        pop     ds
        pop     es
        mov     si, di
        mov     di, offset multicast_table
        rep     movsb
        
        push    cs
        pop     ds
        call    set_multicast
        clc
        ret

f_set_multicast_no_space:
        mov     dh, NO_SPACE
        stc
        ret

f_set_multicast_bad_address:
        mov     dh, BAD_ADDRESS
        stc
        ret

f_set_multicast_list endp


f_get_multicast_list proc near
        mov    (regs ptr[bp])._ES, ds
        mov    (regs ptr[bp])._DI, offset multicast_table
        mov    al, EADDR_LEN
        mul    multicast_inuse
        mov    (regs ptr[bp])._CX, ax
        clc
        ret
f_get_multicast_list endp

f_get_statistics proc near
        call   verify_handle
        jc     f_get_statistics_bad_handle
        mov    (regs ptr[bp])._DS, ds
        mov    (regs ptr [bp])._SI, offset statistics
        clc
        ret

f_get_statistics_bad_handle:
        mov    dh, BAD_HANDLE
        stc
        ret
f_get_statistics endp

f_set_address proc near
        cmp    cx, EADDR_LEN
        jne    f_set_address_bad_address
        call   count_handles
        cmp    cl, 1
        ja     f_set_address_cant_set

        push   es
        push   ds
        pop    es
        pop    ds

        mov    si, di
        mov    di, offset my_eth_addr          ; copy desired address
        mov    cx, EADDR_LEN                   ; in our IA storage
        rep    movsb

        push   cs                              ; restore our data segment
        pop    ds
        
        mov    si, offset my_eth_addr
        xor    di,di                           ; Setup RAR0
        call   set_address
        clc
        ret
        
f_set_address_cant_set:
        mov    dh, CANT_SET
        stc
        ret

f_set_address_bad_address:
        mov    dh, BAD_ADDRESS
        stc
        ret
f_set_address endp        


; Procedure:    set_address
; Description:  Set IA address in RAR array
; Enter:        di - array index
; Exit:         

set_address proc near
        push   si
        push   di
        pushf

        cli
        shl    di, 3
        xor    eax,eax
        mov    dx, IOBase
        mov    ax, E1000_RA
        add    ax, di
        out    dx, eax
        add    dx, 4

        push   eax
        mov    eax,[si]
        out    dx, eax
        pop    eax

        sub    dx, 4
        add    si, 4
        add    ax, 4
        out    dx, eax
        add    dx, 4

        mov    ax, 8000h
        shl    eax,16
        mov    ax, [si]
        out    dx, eax

        popf
        pop    di
        pop    si
        ret
set_address endp

; Procedure:    set_multicast
; Description:  setup multicast adresses
; Enter:        
; Exit:         

        public set_multicast
set_multicast proc near
        call   init_rx_addrs
        xor    cx, cx
        mov    cl, multicast_inuse
        or     cl, cl
        jz     no_multicast_used

        mov    bx, rcv_mode
        cmp    bx, MULTI_LIMITED
        jb     no_multicast_used
        cmp    bx, ALL_MULTI
        ja     no_multicast_used

        mov    si, offset multicast_table
        mov    di, 1
next_address:        
        call   set_address
        inc    di
        add    si, EADDR_LEN
        loop   next_address

no_multicast_used:
        ret
set_multicast endp


        public verify_handle
verify_handle  proc near

;Ensure that their handle is real.  If it isn't, we pop off our return
;address, and return to *their* return address with cy set.

        mov     bx,(regs ptr[bp])._BX   ;get the handle they gave us
        cmp     bx,offset handles
        jb      verify_handle_bad       ;no - must be bad.
        cmp     bx,offset end_handles
        jae     verify_handle_bad       ;no - must be bad.
        cmp     (per_handle ptr[bx]).in_use,1           ;if it's not in use, it's bad.
        jne     verify_handle_bad
        clc
        ret

verify_handle_bad:
        stc
        ret

verify_handle  endp

        public  count_handles
count_handles proc near

;exit with cl = number of handles currently in use.
        mov     bx,offset handles
        xor     cl,cl                                   ;number of handles in use.

count_handles_1:
        add     cl,(per_handle ptr[bx]).in_use          ;is this handle in use?
        add     bx,(size per_handle)                    ;go to the next handle.
        cmp     bx,offset end_handles
        jb      count_handles_1
        ret

count_handles endp


; Procedure:    eth_open
; Description:  Setup packet driver interface
; Enter:
; Exit:         carry set if any error

        public eth_open
eth_open proc near
        DMESSAGE 1, "Initialize driver interface"
        
        call  find_nic                 ; find device 
        jc    error_eth_open


; This will perform a chip reset of all functions to
; their equivalent power on state, except that the state
; of PCI configuration space

        call   reset_chip              
        jc     error_eth_open

        mov    di,offset rom_addr
        call   init_eeprom
        jc     error_eth_open

        call   config_chip
        jc     error_eth_open
        
        cmp    mac_type, i82541
        jb     no_setup
        LOAD_REG E1000_MANC
        in     eax,dx
        or     eax, E1000_MANC_DRV_LOAD    ; this bit should be set
        out    dx,eax                      ; by the driver after it was loaded
no_setup:

        push   cs
        pop    es
        mov    si, offset rom_addr         ; save our EEPROM IA
        mov    di, offset my_eth_addr
        mov    cx, EADDR_LEN
        rep    movsb

        call   set_pkt_int
        call   set_device_isr
        call   rx_tx_on
        clc
        ret
        
error_eth_open:
        stc
        ret
eth_open endp



; Procedure:    device_isr
; Description:  device interrupt handler
; Enter:
; Exit:         
        public device_isr
device_isr proc far

if DEBUG
        to_scr 'I'
endif
        push   ds
        push   cs
        pop    ds

        push   eax
        push   dx

        call   check_our_interrupt
        jc     not_our_interrupt

; relocate stack         
        mov    oldss,ss
        mov    oldsp,sp

        mov    ax,cs
        mov    ss,ax
        mov    sp,offset our_stack
        
        cld
        pusha
        push   es

; block other interrupts
        mov    al,irq_number
        call   mask_interrupt

; EOI  signal
        mov    al,irq_number
        cmp    al,7
        jg     second_controller
        or     al,60h
        out    20h,al
        jmp    short Eoi

second_controller:
        add    al,(60h - 8)
        out    0a0h,al
        mov    al,62h
        out    20h,al

Eoi:
        test   Interrupt_status,E1000_IMC_RXT0
        jz     not_timer_int

        call   receive_timer_int

not_timer_int:
        mov    al,irq_number
        call   unmask_interrupt

        pop    es
        popa 

; restore stack
        lss    sp, old_stack
        
if DEBUG 
        to_scr   'E'
endif
        pop    dx
        pop    eax
        pop    ds
        iret
        
not_our_interrupt:

        pop    dx
        pop    eax

already_in_isr:
        pop    ds
        jmp    cs:their_recv_isr

align  4
their_recv_isr  dd 0

device_isr endp

; Procedure:    check_our_interrupt
; Description:  check interrupt status
; Enter:
; Exit:         carry set if not our

align  16
        public  check_our_interrupt
check_our_interrupt  proc  near

        LOAD_REG  E1000_ICR
        in    eax,dx
        and   eax,E1000_INTERRUPT_MASK
        jz    not_our
        mov   Interrupt_status,eax

        clc
        ret

not_our:
        stc
        ret

check_our_interrupt  endp


; Procedure:    receive_timer_int
; Description:  receive timer interrupt routine
; Enter:
; Exit:         
align  16
        public receive_timer_int
receive_timer_int proc near

        mov   ax,rx_head

check_next_frame:
        mov   bx,ax
        shl   bx,4

        test  (recv_descr_type ptr[bx+offset RxRing]).rstatus,RD_STAT_DD
        jz    short receive_timer_int_fin

        push  bx
        call  receive_frame
        pop   bx

if DEBUG
        to_scr 'R'
endif
        xor   al,al
        mov   (recv_descr_type ptr[bx+offset RxRing]).rstatus,al

        mov   ax,rx_head
        inc   ax
        cmp   ax,RX_BUFFER_NUMBER

        jl    not_last_descriptor

        LOAD_REG  E1000_RDT
        xor   ax,ax
        out   dx,eax

        mov   ax,RX_BUFFER_NUMBER
        out   dx,eax

        xor   ax,ax

not_last_descriptor:

        mov   rx_head,ax

        jmp   check_next_frame


receive_timer_int_fin:

        ret
receive_timer_int endp

; Procedure:    receive_frame
; Description:  receive function
; Enter:        bx - point on current descriptor 
; Exit:         

align  16
        public receive_frame
receive_frame proc near
   
        test  BYTE PTR (recv_descr_type ptr[bx + offset RxRing]).errors, RX_ERROR_MASK
        jnz   receive_frame_exit

        mov   ax,ds
        mov   es,ax

        mov   si,bx
        shr   si,3

        mov   si,[si + offset rx_buffer_ptrs]
        mov   di,si

        add   di,ETHERNET_ADDRESS_LENGTH*2           ; skip addresses field

        mov   cx,(recv_descr_type ptr [bx+ offset RxRing]).r_length    ; get receive frame length

        push  cx                           ; We will want the count and pointer
        push  bx                           ; to hand to client after copying,
        push  si                           ; so save them at this point

        mov   dl, BLUEBOOK                 ; assume bluebook Ethernet.
        mov   ax, [di]                     ; Get the packet type
        xchg  ah, al
        cmp   ax, MAXIMUM_ETHERNET_PACKET_SIZE
        ja    short BlueBookPacket
        inc   di                           ; set di to 802.3 header
        inc   di
        mov   dl, IEEE8023

BlueBookPacket:

        call  recv_find                    ; See if type and size are wanted

        pop   si
        pop   bx
        pop   cx

        mov   ax,es
        or    ax,di
        jz    receive_frame_exit

        push  cx                         ; We will want the count and pointer
        push  es                         ; to hand to client after copying,
        push  di                         ; so save them at this point


; copy receive data
        mov   ax,cx                        ; save length count
        shr   cx,2
        rep   movsd
        mov   cx,ax                        ; restore frame length
        and   cx,3
        jz    no_more_copy
        rep   movsb
no_more_copy:

        pop   si                 ; Recover pointer to destination
        pop   ds                 ; Tell client it's his source
        pop   cx                 ; And it's this long

        call  recv_copy

        mov   ax, cs
        mov   ds, ax

receive_frame_exit:

        ret
receive_frame endp


align  16
        public recv_find
recv_find proc near

        push    cx                      ;preserve packet length.
        call    recv_locate             ;search for the packet type.
        pop     cx
        jc      handle_not_found        ;we didn't find it -- discard it.

        add2    packets_in,1
        add2    bytes_in,cx             ;add up the received bytes.

        mov     bx,found_handle

;remember the receiver upcall.

        les     di,(per_handle ptr[bx]).receiver  

        mov     (segmoffs ptr receive_ptr).offs,di
        mov     (segmoffs ptr receive_ptr).segm,es

        mov     ax,0                    ; allocate request.
        stc                             ; with stc, flags must be an odd number
        push    ax                      ; save a number that cant be flags
        pushf                           ; save flags in case iret used.
        call    receive_ptr             ; ask the client for a buffer.

; on return, flags should be at top of stack. if an IRET has been used,
; then 0 will be at the top of the stack

        pop     ax
        cmp     ax,0                    ; 0 is at top of stack
        jz      stack_already_clean
        add     sp,2

stack_already_clean:
        ret

handle_not_found:
        xor     di,di                   ; "return" a null pointer.
        mov     es,di
        ret

recv_find endp


align  16
        public recv_locate
recv_locate proc near

;called when we want to determine what to do with a received packet.
;enter with es:di -> packet type, dl = packet class.
;exit with cy if the packet is not desired, or nc if we know its type.

        mov     bx,offset handles

next_handle:
        cmp     (per_handle ptr[bx]).in_use,0           ;is this handle in use?
        jz      not_our_handle                              ;no - don't check the type.

; check if the handle has receiver
        mov     ax,(segmoffs ptr(per_handle ptr[bx]).receiver).offs
        or      ax,(segmoffs ptr(per_handle ptr[bx]).receiver).segm
        jz      not_our_handle             


;per request by the UK people, we match on IEEE 802.3 classes, then types.
;for all others, we match on type, then class.  This lets their software work
;without breaking BLUEBOOK type length=0 clients.

        cmp     (per_handle ptr[bx]).class,IEEE8023     ;is this an IEEE 802.3 handle
        jne     not_IEEE8023            ;no.
        cmp     dl,IEEE8023             ;is the packet also IEEE 802.3?
        jne     not_our_handle          ;no, give up on it now.


not_IEEE8023:
        mov     cx,(per_handle ptr[bx]).packet_type_len 
        lea     si,(per_handle ptr[bx]).packet_type

        jcxz    found_our_handle        ;if cx is zero, they want them all.

        cmp     (per_handle ptr[bx]).class, dl          ;is this the right class?
        jne     not_our_handle              ;no- don't bother

        push    di
        repe    cmpsb
        pop     di
        je      found_our_handle        ; we've got it!

not_our_handle:

        add     bx,(size per_handle)    ; increase handle pointer
        cmp     bx,offset end_handles   ; is it a last handle
        jb      next_handle

        add2    packets_dropped,1       ; count it as dropped.
        stc
        ret

found_our_handle:

        mov     found_handle,bx         ;remember what our handle was.
        clc
        ret

recv_locate endp


align  16
        public recv_copy
recv_copy proc near
;called after we have copied the packet into the buffer.
;enter with ds:si ->the packet, cx = length of the packet.
;preserve bx.

        push    es
        push    bx

        mov     ax, cs
        mov     es, ax
        mov     bx,es:found_handle
        mov     ax,1                    ;store request.
        clc                             ;with clc, flags must be an even number
        push    ax                      ;save a number that can't be flags
        pushf                           ;save flags incase iret used.
        call    es:receive_ptr          ;ask the client for a buffer.
        pop     ax
        cmp     ax,1                    ;if this is a 1, IRET was used.
        jz      stack_recovery
        add     sp,2
stack_recovery:

        pop     bx
        pop     es

        ret

recv_copy endp

; Procedure:    mask_interrupt
; Description:  mask interrupt on interrupt controller
; Enter:        al - interrupt number
; Exit:

align  4
        public  mask_interrupt
mask_interrupt proc near
        mov     dx,21h
        cmp     al,8
        jb      mask_interrupt07
        mov     dx,0a1h
        sub     al,8
mask_interrupt07:
        mov     cl,al
        in      al,dx
        mov     ah,1
        shl     ah,cl
        or      al,ah
        out     dx,al
        ret
mask_interrupt endp

; Procedure:    unmask_interrupt
; Description:  unmask interrupt on interrupt controller
; Enter:        al - interrupt number
; Exit:
align  4
        public  unmask_interrupt
unmask_interrupt proc near

        mov     dx,21h                 
        mov     cl,al
        cmp     cl,8                   
        jb      unmask_interrupt_07        
        in      al,dx                  

        and     al,not BIT_2        ; enable second interrupt controller
        out     dx,al                   

        mov     dx,0a1h                 
        sub     cl,8

unmask_interrupt_07:

        in      al,dx
        mov     ah,1                    
        shl     ah,cl

        not     ah
        and     al,ah
        out     dx,al

        ret
unmask_interrupt endp

extrn   TransmitBuffers:byte
extrn   ReceiveBuffers:byte
extrn   TxRing:byte
extrn   RxRing:byte

even
	public  handles
handles         per_handle MAX_HANDLE dup(<>)
end_handles     label   byte

align 4
        public  tx_buffer_ptrs, rx_buffer_ptrs
tx_buffer_ptrs  dw TX_BUFFER_NUMBER dup (?)
rx_buffer_ptrs  dw RX_BUFFER_NUMBER dup (?)
        public  multicast_table
multicast_table	db MULTICAST_SIZE   dup (?)
multicast_inuse db 0

align 4
                public  statistics
statistics      label   dword
        public  packets_in, packets_out, bytes_in, bytes_out
packets_in      dw      0,0
packets_out     dw      0,0
bytes_in        dw      0,0
bytes_out       dw      0,0
errors_in       dw      0,0
errors_out      dw      0,0
packets_dropped dw      0,0             ; dropped due to no type handler.

upcall_routine  dd      0
        public  free_handle, found_handle, receive_ptr
free_handle     dw      0               ; temp, a handle not in use
found_handle    dw      0               ; temp, handle for our packet
receive_ptr     dd      0               ; the pkt receive service routine

tx_buffer_no    dw      0               ; current transmit buffer
rx_head         dw      0               ; current receive buffer
rcv_mode        dw      3               ; current receive mode number

our_isr_signature dw  INT_SIGN_WORDS dup (?)
align 4
Interrupt_status  dd    ?

        public IOBase, their_int
IOBase  	  dw    ?
their_int         dd    0

        public  driver_class, driver_type, driver_name, driver_number
driver_type     dw      0FFFFh                  ; from the packet spec
driver_class    db      BLUEBOOK,IEEE8023,0     ; possible driver classes
driver_number   db      0
driver_name     db      DRVNAME, 0              ; driver name
        public  driver_function, irq_number
driver_function db      BASIC_HIGH_EXT_F        ; Implemented basic,
                                                ; high-perfomance,extended functions
irq_number      db      ?  			; Storage for device IRQ number

        public  parameter_list
parameter_list	label	byte
	db	1				; major rev of packet driver
	db	9				; minor rev of packet driver
	db	14				; length of parameter list
	db	EADDR_LEN			; length of MAC-layer address
	dw	GIANT				; MTU, including MAC headers
	dw	MULTICAST_SIZE	                ; buffer size of multicast addrs
	dw	0				;(# of back-to-back MTU rcvs) - 1
	dw	0				;(# of successive xmits) - 1
dev_isr db	0,0                             ; Interrupt # to hook for post-EOI
						; processing, 0 == none,
	public	com_int
com_int db	0,0                             ; packet driver interrupt number                             

        public  cmd_opt, mac_type						
cmd_opt         db      0                     	; command line options
mac_type        db      ?                       ; device type
_TEXT   ends

_INIT   segment para public USE16 'CODE'
my_eth_addr    db EADDR_LEN   dup (?)
        public rom_addr
rom_addr       dw EADDR_LEN/2 dup (?)
resident_part label byte

extrn config_chip:near
extrn read_phy_reg:near
extrn write_phy_reg:near
extrn init_eeprom:near

extrn find_nic:near
extrn cmd_line_par:near

pkt_init: 
        mov    ah, 9
        mov    dx, offset driver_msg
        int    21h

        mov    dx, offset copyright
        int    21h

        mov    dx, offset pci_msg
        int    21h

        call   cmd_line_par

        call   eth_open
        jc     init_error


        mov    ah,9
        mov    dx, offset success_msg
        int    21h

; free our environment
	mov    ah, 49h
	mov    es, cs:[2ch]
	int    21h
; terminate and stay resident        
        mov    ax, 3100h
        mov    dx, offset resident_part 
        shr    dx,4
        inc    dx
        int    21h

init_error:

        mov    ah, 9
        mov    dx, offset skip_str
        int    21h

        mov    dx, offset error_msg
        int    21h
        
        mov    ax,4c01h
        int    21h

; Procedure:    set_pkt_int
; Description:  set packet driver interrup
; Enter:
; Exit:

set_pkt_int proc near
        DMESSAGE 1, "Set packet driver interrupt"
        mov    ah, 35h
        mov    al, com_int
        int    21h

        mov    (segmoffs ptr their_int).offs, bx
        mov    (segmoffs ptr their_int).segm, es
        
        mov    ah, 25h
        mov    dx, offset CGROUP:__pktdrv
        int    21h
        ret        
set_pkt_int endp

; Procedure:    set_device_isr
; Description:  sets our interrupt handler
; Enter:
; Exit:

        public set_device_isr
set_device_isr proc near
        DMESSAGE 1, "Set irq handler"
        mov     al,irq_number
        cmp     al,15
        ja      error_irq_number

        add     al,8
        cmp     al,8+8                  ;is it a slave interrupt controller?
        jb      first_controller        
        add     al,70h - 8 - 8          ;map it to the real interrupt.
first_controller:
        mov     dev_isr, al
        mov     ah, 35h 
        int     21h
        mov     (segmoffs ptr their_recv_isr).offs,bx  
        mov     (segmoffs ptr their_recv_isr).segm,es

        mov     ah, 25h        
        mov     dx, offset device_isr
        int     21h

        mov     si, dx                   ; si points on device_isr
        mov     di, offset our_isr_signature
        
        push    ds
        pop     es

        mov     cx, INT_SIGN_WORDS
        cld
        rep     movsw

        mov     al,irq_number
        call    unmask_interrupt
error_irq_number:

        ret
set_device_isr endp


; Procedure:    rx_tx_on
; Description:  enable chip receive & transmit logic
; Enter:
; Exit:
        public rx_tx_on
rx_tx_on  proc near

        call   enable_tx
        call   enable_rx

        LOAD_REG E1000_RDT
        mov    ax,RX_BUFFER_NUMBER
        out    dx,eax
        call   enable_board_int

        ret
rx_tx_on  endp

even
        public MediaType, speed, duplex
MediaType       dw      ?
speed           dw      SPEED_10
duplex          dw      HALF_DUPLEX

include message.inc
_INIT ends
        end   start
