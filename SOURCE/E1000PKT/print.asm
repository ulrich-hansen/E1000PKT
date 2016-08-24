;if DEBUG
; Procedure:    print_sym
; Description:  print symbol 
; Enter:        al - symbol to print
; Exit:

          public  print_sym
print_sym proc  near

          push es
          push di

          push 0b800h
          pop  es
          mov  di,79*2
          
          mov  ah,04h
          stosw

          pop  di
          pop  es

          ret

print_sym endp

; Procedure:    print_str
; Description:  print ASCIIZ string
; Enter:        si points on string
; Exit:
          public print_str
print_str proc    near
 
          push    ax
          mov     ah,0eh
@@Rep:
          lodsb   
          cmp     al,00h

          je      @@FinProg
          int     10h
          jmp     short @@Rep  

@@FinProg:
          pop     ax
          ret
print_str endp
;endif
