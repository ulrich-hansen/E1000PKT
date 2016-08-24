;

; Link setup routines

include def.inc
include phy.inc
include segms.inc
include 82544.inc

.386

_TEXT   segment para public USE16 'CODE'

extrn   Wait_10:near
extrn   cmd_opt:byte
extrn   mac_type:byte
extrn   IOBase:word

_TEXT   ends

_INIT   segment para public USE16 'CODE'

if DEBUG
extrn print_str:near
endif

; Procedure:    read_phy_reg
; Description:  read phy register
; Enter:        bx - register number
; Exit:         ax - register value
        public read_phy_reg
read_phy_reg proc near
        DMESSAGE 6, "Read PHY register"
        
        push  bx
        push  cx
        
        LOAD_REG E1000_MDIC        

        mov   ax,PHY_ADDR
        shl   ax,5
        or    ax,bx
        or    ax,E1000_MDIC_OP_READ
        shl   eax,16
        out   dx,eax

        mov   cx,10
@@1:
        in    eax,dx
        test  eax,E1000_MDIC_READY
        jnz   read_complete
        UsecWait 10
        loop  @@1

error_r_ph_r:
        DMESSAGE 3,"Cannot read PHY register"
        stc
        jc    exit_r_ph_r
read_complete:
        test  eax,E1000_MDIC_ERROR
        jnz   error_r_ph_r
        clc
exit_r_ph_r:
        pop   cx
        pop   bx                
        ret 
             
read_phy_reg endp

; Procedure:    write_phy_reg
; Description:  write phy register
; Enter:        bx - register number  ax - register value
; Exit:         
        public  write_phy_reg
write_phy_reg proc near
        DMESSAGE 6, "Write PHY register"

        push  bx
        push  cx
        
        mov   cx, ax
        LOAD_REG E1000_MDIC
        mov   ax,PHY_ADDR
        shl   ax,5
        or    ax,E1000_MDIC_OP_WRITE
        or    ax,bx
        shl   eax,16
        or    ax,cx
        out   dx,eax

        mov   cx,10
@@2:
        in    eax,dx
        test  eax,E1000_MDIC_READY
        jnz   write_complete
        UsecWait 10
        loop  @@2
error_w_ph_r:
        DMESSAGE 3,"Cannot write PHY register"
        stc
        jc    exit_w_ph_r
write_complete:        
        test  eax,E1000_MDIC_ERROR
        jnz   error_w_ph_r
        clc
exit_w_ph_r:        
        pop   cx
        pop   bx
        ret
write_phy_reg endp

; Procedure:    setup_copper
; Description:  set up link for copper adapters
; Enter:        
; Exit:         carry set if any error
        public setup_copper
setup_copper proc  near
        DMESSAGE 3, "Setup copper link"
        
        cmp   mac_type, i82541
        jae   no_opt

        mov   bx, PHY_SCTRL
        call  read_phy_reg
        jc    error_s_c
        or    ax, PHY_SCTRL_ACRS                  ; need only for half-duplex operation
        call  write_phy_reg
        jc    error_s_c
       
        call  phy_soft_reset
        jc    error_s_c
no_opt:        
        test  cmd_opt, MDPX_OPTION
        jnz   force_link
negot_duplex:
; Enable automatically speed detection
; and Set Link Up

        LOAD_REG E1000_CTRL
        in    eax,dx
        or    ax, E1000_CTRL_ASDE or E1000_CTRL_SLU
        out   dx,eax

; Check if PHY is eable to perfom Auto-Negotiation
; if so Restart it
        
        mov   bx,PHY_STATUS
        call  read_phy_reg
        jc    error_s_c

        test  ax,PHY_ST_ANA
        jz    error_s_c
        call  auto_config
        jc    error_s_c
        jmp   check_link     
force_link:
        cmp   speed, SPEED_1000                  ; always negotiate Gig link
        je    negot_duplex

        call  manual_config
        jc    error_s_c

check_link:
        call  check_link_up
        ret

error_s_c:
        stc
        ret

setup_copper endp

; Procedure:    auto_config
; Description:  set up desired negotiation speed if specified  
;               and restart auto-negotiation
; Enter:        
; Exit:         carry set if any error
        public auto_config
auto_config proc near
        DMESSAGE 3, "Negotiate speed and duplex"
        test  cmd_opt, MSPD_OPTION
        jz    no_speed_specified
        call  set_adv_speed
        jc    ErrorAN

no_speed_specified:
        cmp   mac_type, i82544
        jbe   no_hdx_disable
        cmp   mac_type, i82541
        jae   no_hdx_disable

        mov   bx, PHY_SCTRL
        call  read_phy_reg
        jc    ErrorAN
        and   ax, not PHY_MSCTRL_1000THD
        call  write_phy_reg
        jc    ErrorAN
no_hdx_disable:

        mov   bx, PHY_ANA
        call  read_phy_reg
        jc    ErrorAN
        and   ax, not (PHY_ANA_AP or PHY_ANA_PAUSE)     ; disable flow control capabilites
        call  write_phy_reg
        jc    ErrorAN
        call  phy_soft_reset                            ; Set-up our changes
        jc    ErrorAN

        mov   bx, PHY_CTRL
        call  read_phy_reg
        jc    ErrorAN

        or    ax, PHY_CTRL_ANE or PHY_CTRL_RAN

        call  write_phy_reg
        jc    ErrorAN

; Wait 4.5 second to complete

        mov   cx, 45
        mov   bx,PHY_STATUS
@@Rep:
        UsecWait 100000
        call  read_phy_reg
        jc    ErrorAN
; check every 0.1 seconds
; if  Auto-Negotiation completed
        and   ax,PHY_ST_ANC
        jnz   completed
        loop  @@Rep
        DMESSAGE 3, "Auto-Negotiation FAILED!!!"
ErrorAN:
        stc
        ret

completed:
        clc
        ret
auto_config endp


; Procedure:    manual_config
; Description:  force link
; Enter:        
; Exit:         carry set if any error
        public manual_config
manual_config proc near
        DMESSAGE  3, "Force link"
        
        mov   bx, PHY_CTRL
        call  read_phy_reg
        jc    error_m_c
        and   ax, not PHY_CTRL_ANE
        and   ax, not PHY_CTRL_SP_MASK
        or    ax, PHY_CTRL_DM
        mov   bx, ax

        LOAD_REG E1000_CTRL
        in    eax, dx
        or    ax, (E1000_CTRL_FRCDPX or E1000_CTRL_FRCSPD)
        and   ax, not E1000_CTRL_SPD_MASK
        or    ax, E1000_CTRL_FD

        cmp   duplex, HALF_DUPLEX
        jne   use_fd
        and   al, not E1000_CTRL_FD
        and   bx, not PHY_CTRL_DM
use_fd:
        xor   cx, cx
        cmp   speed, SPEED_10
        je    setup_speed
        inc   cx
        or    bx, PHY_CTRL_SP100
        cmp   speed, SPEED_100
        je    setup_speed
        jmp   short error_m_c
setup_speed:
        shl   cx, E1000_CTRL_SPD_SHIFT
        or    ax, cx
        out   dx, eax

        mov   ax, bx
        mov   bx, PHY_CTRL
        call  write_phy_reg
        jc    error_m_c

        cmp   mac_type, i82541
        jae   config_mdi
        mov   bx, PHY_SCTRL
        call  read_phy_reg
        jc    error_m_c
        and   ax, not PHY_SCTRL_MDIM_MASK
        call  write_phy_reg
        jc    error_m_c
        jmp   short setup_mdi
config_mdi:
        mov   bx, PHY_PCTRL
        call  read_phy_reg
        jc    error_m_c
        and   ax, not (PHY_PCTRL_AMDIX or PHY_PCTRL_MDIXM)
        call  write_phy_reg
        jc    error_m_c

setup_mdi:
        call  phy_soft_reset
        jc    error_m_c

        LOAD_REG E1000_CTRL                     ; After MAC and PHY layers configurate
        in    eax,dx                            ; to the same speed and duplex set Link Up
        or    ax, E1000_CTRL_SLU
        out   dx, eax

        clc
        ret

error_m_c:       
        stc
        ret
manual_config endp


; Procedure:    phy_soft_reset
; Description:  reset PHY - apply MII registers changes
; Enter:        
; Exit:         carry set if any error
        public phy_soft_reset
phy_soft_reset proc near
        DMESSAGE 3, "Do PHY soft reset"

        cmp   mac_type, i82541
        jae   ResetComplete
        mov   bx, PHY_CTRL
        call  read_phy_reg
        jc    CantReset
        or    ax, PHY_CTRL_RST
        call  write_phy_reg
        mov   cx, 100
@@3:
        call  read_phy_reg
        jc    CantReset
        test  ax, PHY_CTRL_RST
        jz    ResetComplete
        UsecWait 10
        loop  @@3
CantReset:
        stc
        ret
        
ResetComplete:
        clc
        ret
phy_soft_reset endp


; Procedure:    set_adv_speed
; Description:  set up negotiation speed
; Enter:        
; Exit:         carry set if any error
set_adv_speed proc near
        DMESSAGE 3, "Setup negotiation speed"
        
        mov    bx, PHY_ANA
        call   read_phy_reg
        jc     err_s_adv_sp
        and    ax, not (PHY_ANA_100TXFD or PHY_ANA_100TXHD)
        and    ax, not (PHY_ANA_10TXFD  or PHY_ANA_10TXHD)

        cmp    speed, SPEED_1000
        jne    setup10_100
        call   write_phy_reg
        jc     err_s_adv_sp
        jmp    short exit_s_adv_sp

setup10_100:
        cmp    speed, SPEED_100
        je     setup100
        or     ax, (PHY_ANA_10TXFD  or PHY_ANA_10TXHD)
        jmp    short save_sp    
setup100:
        or     ax, (PHY_ANA_100TXFD or PHY_ANA_100TXHD)
save_sp:
        call   write_phy_reg
        jc     err_s_adv_sp

        mov    bx, PHY_MSCTRL
        call   read_phy_reg
        jc     err_s_adv_sp
        and    ax, not (PHY_MSCTRL_1000TFD or PHY_MSCTRL_1000THD)
        call   write_phy_reg
exit_s_adv_sp:
        ret
        
err_s_adv_sp:
        stc
        ret
set_adv_speed endp


; Procedure:    check_link_up
; Description:  wait 2 second for link establishing
; Enter:        
; Exit:         carry set if no link detected
check_link_up proc near
        DMESSAGE 3, "Check link state"

        mov   cx, 2001
        mov   bx, PHY_STATUS

check_again:
        call  read_phy_reg
        jc    link_down
        call  read_phy_reg
        jc    link_down
        test  ax, PHY_ST_LS
        jnz   link_up
                
        UsecWait 1000
        loop  check_again
link_down:
        DMESSAGE 3, "Cannot establish link"
        stc
        ret
link_up:
        clc
        ret
check_link_up endp


extrn   speed:word
extrn   duplex:word

_INIT   ends

        end
