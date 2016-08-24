;

; timer API

; Ticks number for time values
ONE_MIL         equ 2387
TEN_MICRO       equ 24

include segms.inc

_TEXT   segment  para public USE16 'CODE'

; Procedure:    latch-timer
; Description:  read timer value
; Enter: 
; Exit:         ax - timer value 
        public latch_timer
latch_timer proc near
        mov     al,0                    ;latch counter zero.
        out     43h,al
        in      al,40h                  ;read counter zero.
        mov     ah,al
        in      al,40h
        xchg    ah,al
        ret
latch_timer endp


; Procedure:    Wait_10
; Description:  Do delay 
; Enter:        cx - delay value  in 10 microseconds
; Exit:         
        public Wait_10
Wait_10 proc near

        jcxz short no_wait

        push ax

        push bp
        mov  bp,sp
        sub  sp,2

next_10:

        call latch_timer
        mov  [bp-2],ax

do_next:
        call latch_timer
        add  ax,TEN_MICRO
        cmp  ax,[bp-2]
        ja   do_next

        loop next_10

        mov  sp,bp
        pop  bp

        pop  ax

no_wait:
        ret
Wait_10 endp


_TEXT   ends
        end
