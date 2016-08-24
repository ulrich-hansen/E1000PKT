;

; Definitions


TRUE            equ  0FFFFh
FALSE           equ   0000h

ifndef  DEBUG
DEBUG           equ   TRUE
endif

ifndef  DLEVEL
DLEVEL          equ   0
endif

DRVNAME         equ   <"e1000pkt">
DRV_VER         equ   <"v0.50">
DRV_NAME_LENGTH equ   8+1
N_OPTION        equ   1                 ; Novell protocol conversion
W_OPTION        equ   2                 ; Windows upcall checking.
P_OPTION        equ   4                 ; Promiscuous mode disable (6 mode)
U_OPTION        equ   8                 ; Terminate the driver.
MSPD_OPTION     equ   16                ; Manually speed configuration
MDPX_OPTION     equ   32                ; Manually duplex configuration

WORK_INT_NO     equ   60h
TX_HOOK         equ   TRUE
STACK_SIZE      equ   80

; Load needed register address in address port
; and update dx to IODATA register

LOAD_REG macro REG_ADDRESS

        mov  dx,IOBase

if      REG_ADDRESS eq  0
        xor  eax,eax
else
        mov  eax,REG_ADDRESS
endif
        out  dx,eax
        add  dx,4
        endm


; Wait for specific number of microseconds
UsecWait macro  USEC_NUMBER

        push cx
        mov  cx,USEC_NUMBER/10
        call Wait_10
        pop  cx

        endm


; DMESSAGE
; prints constant debug message when debug level
; is less or equal than value of DEBUG macro
; Usage: DMESSAGE   <debug level>,<message>

DMESSAGE    macro   level:req, text:req
        local   ctext,done
if      DEBUG
if      level le DLEVEL

        pushf            
        push    ds
        push    si
           
        push    cs
        pop     ds

        lea     si,cs:ctext
        call    print_str

        jmp     done
ctext   db      "DBG: ",text,0DH,0AH,0

done:   pop     si
        pop     ds
        popf

endif
endif
endm


; Bits masks

BIT_0      equ  0001h
BIT_1      equ  0002h
BIT_2      equ  0004h
BIT_3      equ  0008h
BIT_4      equ  0010h
BIT_5      equ  0020h
BIT_6      equ  0040h
BIT_7      equ  0080h
BIT_8      equ  0100h
BIT_9      equ  0200h
BIT_10     equ  0400h
BIT_11     equ  0800h
BIT_12     equ  1000h
BIT_13     equ  2000h
BIT_14     equ  4000h
BIT_15     equ  8000h    

BIT_16     equ  00010000h
BIT_17     equ  00020000h
BIT_18     equ  00040000h
BIT_19     equ  00080000h
BIT_20     equ  00100000h
BIT_21     equ  00200000h
BIT_22     equ  00400000h
Bit_23     equ  00800000h
BIT_24     equ  01000000h
BIT_25     equ  02000000h
BIT_26     equ  04000000h
BIT_27     equ  08000000h
BIT_28     equ  10000000h
BIT_29     equ  20000000h
BIT_30     equ  40000000h
BIT_31     equ  80000000h

i82544           equ     0
i82540           equ     i82544 + 1
i82545           equ     i82540 + 1
i82541           equ     i82545 + 1
i82547           equ     i82541 + 1

VERSION          equ     01             ;version number of the infrastructure.
MAX_ADDR_LEN     equ     16             ;maximum number of bytes in our address.
MAX_HANDLE       equ     10             ;maximum number of handles.
MAX_P_LEN        equ     8              ;maximum type length


; Packet driver functionality
BASIC_F          equ     1               ;Basic functions only present
BASIC_EXT_F      equ     2               ;Basic and extended functions present
BASIC_HIGH_F     equ     5               ;Basic and high-perfomance functions present
BASIC_HIGH_EXT_F equ     6               ;Basic, high-perfomance, extended functions present
NOT_INSTALLED    equ     255             ;Driver not installed 


; Packet Driver Error numbers
NO_ERROR        equ     0               ;no error at all.
BAD_HANDLE      equ     1               ;invalid handle number
NO_CLASS        equ     2               ;no interfaces of specified class found
NO_TYPE         equ     3               ;no interfaces of specified type found
NO_NUMBER       equ     4               ;no interfaces of specified number found
BAD_TYPE        equ     5               ;bad packet type specified
NO_MULTICAST    equ     6               ;this interface does not support
                                        ;multicast
CANT_TERMINATE  equ     7               ;this packet driver cannot terminate
BAD_MODE        equ     8               ;an invalid receiver mode was specified
NO_SPACE        equ     9               ;operation failed because of
                                        ;insufficient space
TYPE_INUSE      equ     10              ;the type had previously been accessed,
                                        ;and not released.
BAD_COMMAND     equ     11              ;the command was out of range, or not
                                        ;implemented
CANT_SEND       equ     12              ;the packet couldn't be sent (usually
                                        ;hardware error)
CANT_SET        equ     13              ;hardware address couldn't be changed
                                        ;(more than 1 handle open)
BAD_ADDRESS     equ     14              ;hardware address has bad length or
                                        ;format
CANT_RESET      equ     15              ;Couldn't reset interface (more than
                                        ;1 handle open).
BAD_IOCB        equ     16              ;an invalid iocb was specified

; receive mode defenition
RECEIVE_OFF     equ     1               ; turn off receiver
IA_ONLY         equ     2               ; receive only packets sent to this interface
BRDCAST         equ     3               ; receeve IA and broadcast packets
MULTI_LIMITED   equ     4               ; BRDCAST and some multicast packets
ALL_MULTI       equ     5               ; BRDCAST and all multicast packets
RECEIVE_ALL     equ     6               ; receive all packets


;a few useful Ethernet definitions.
RUNT            equ     60              ;smallest legal size packet, no fcs
GIANT           equ     1514            ;largest legal size packet, no fcs
EADDR_LEN       equ     6               ;Ethernet address length.
MULTICAST_MAX   equ     6

BLUEBOOK        equ     1
IEEE8023        equ     11

MULTICAST_SIZE  equ     MULTICAST_MAX*EADDR_LEN



;if DEBUG

to_scr  MACRO  ch

        push   ax
        mov    al,ch
        call   print_sym
        pop    ax

        endm

;endif


REGS_R_32_BIT  equ  TRUE

ifdef REGS_R_32_BIT
        REG_WORD_SIZE equ       2
else
        REG_WORD_SIZE equ       1
endif

REG_16  MACRO   param
param   dw      ?
ifdef REGS_R_32_BIT
                dw      ?
endif
ENDM

REG_8   MACRO   param1, param2
        param1  db      ?
        param2  db      ?
ifdef REGS_R_32_BIT
                dw      ?
endif
ENDM


PUSH_16_32 macro    param
if      REGS_R_32_BIT
        push    e&param
        else
        push    param
endif
          endm

POP_16_32  macro    param
ifdef   REGS_R_32_BIT
        pop             e&param
else
        pop             param
endif
ENDM



; The following structure is used to access the registers pushed by the
; packet driver interrupt handler.  Don't change this structure without also
; changing the "bytes" structure given below.
;
regs    struc                           ; stack offsets of incoming regs
_ES             dw      ?
_DS             dw      ?
REG_16 _BP
REG_16 _DI
REG_16 _SI
REG_16 _DX
REG_16 _CX
REG_16 _BX
REG_16 _AX
_IP     dw      ?
_CS     dw      ?

_F      dw      ?                       ; flags, Carry flag is bit 0
regs    ends

;
; bits in the _F register.
;
CY      equ     0001h
EI      equ     0200h



; This structure is a bytewise version of the "regs" structure above.
;
bytes   struc                           ; stack offsets of incoming regs
        dw      ?                       ; es, ds, are 16 bits
        dw      ?
        dw      REG_WORD_SIZE dup (?)   ; bp, di, si are 16 (or 32) bits
        dw      REG_WORD_SIZE dup (?)
        dw      REG_WORD_SIZE dup (?)
REG_8   _DL, _DH
REG_8   _CL, _CH
REG_8   _BL, _BH
REG_8   _AL, _AH
bytes   ends


segmoffs        struc                   ; defines offs as 0, segm as 2
offs            dw      ?
segm            dw      ?
segmoffs        ends

;add a word to a dword.
add2    macro   n,a                                   ; inc a 32 bit integer
        add     (segmoffs ptr n).offs,a               ;increment the low word
        adc     (segmoffs ptr n).segm,0               ;increment the high word
        endm


per_handle      struc
in_use          db      0               ;non-zero if this handle is in use.
class           db      ?               ;interface class
packet_type     db      MAX_P_LEN dup(0);associated packet type.
receiver        dd      0               ;receiver handler.
packet_type_len dw      0               ;associated packet type length.
;receiver_sig    db      8 dup(?)        ;signature at the receiver handler.
per_handle      ends

INT_SIGN_WORDS  equ  3
