;

; Board initialization routines

include def.inc
include segms.inc
include 82544.inc
include pci.inc


.386

_TEXT   segment para public USE16 'CODE'

extrn   __pktdrv:far
if DEBUG
extrn   print_str:near
endif

extrn   Wait_10:near
extrn   set_multicast:near

extrn cmd_opt:byte
extrn driver_class:byte
extrn com_int:byte

; Procedure:    reset_chip
; Description:  does chip soft reset and PHY reset
; Enter:
; Exit:

        public  reset_chip
reset_chip proc near
        DMESSAGE 2, "Reset chip"

        call  disable_board_int

;  First disable receive unit      
        call  disable_rx

;  Now disable transmit unit 
        call  disable_tx

; Wait for all PCI or PCI-X transactions complete

        UsecWait  10000 

; Do device reset 
        cmp   mac_type, i82541
        jb    do_reset
        LOAD_REG E1000_WUC
        in    eax,dx
        and   eax, not E1000_WUC_SPM       ; SPM bit must be cleared before Soft Reset
        out   dx,eax
do_reset:
        LOAD_REG  E1000_CTRL
        in    eax,dx
        or    eax,E1000_CTRL_RST
        out   dx,eax

        cmp   mac_type, i82540
        jb    ee_reread
        cmp   mac_type, i82547
        jb    auto_ee_reread

ee_reread:
        UsecWait  10

; Reread EEPROM context
        LOAD_REG E1000_CTRL_EXT
        in    eax,dx
        or    eax,E1000_EXCTRL_EE_RST
        out   dx,eax
        jmp   exit_r_ch
auto_ee_reread:
        UsecWait 10000                           ; wait about 10ms for eeprom reload

exit_r_ch:

; we should disable device interrupts after chip reset
        call  disable_board_int

        LOAD_REG E1000_CTRL
        in    eax,dx
        test  eax,E1000_CTRL_RST                 ; check reset status
        jnz   error_r_ch

; Reset PHY to H/W default values
        or    eax, E1000_CTRL_PHYRST
        out   dx, eax
        and   eax, not E1000_CTRL_PHYRST
        out   dx, eax
        UsecWait 10000                           ; additional wait for phy restoring
        clc
        ret

error_r_ch:
        stc
        ret

reset_chip endp


; Procedure:    disable_board_int
; Description:  disable chip interrupt generation
; Enter:
; Exit:

        public  disable_board_int
disable_board_int proc near

        pushf
        cli
        push  ax
        LOAD_REG E1000_IMC
        mov   eax,0FFFFFFFFh
        out   dx, eax
        pop   ax

        LOAD_REG E1000_ICR                ; stop pending interrupts
        in    eax,dx
        popf
        ret

disable_board_int endp


; Procedure:    enable_board_int
; Description:  setup board interrupts
; Enter:
; Exit:

        public  enable_board_int
enable_board_int proc near

        pushf
        cli
        push  ax
        LOAD_REG E1000_IMS
        mov   eax,E1000_INTERRUPT_MASK
        out   dx, eax
        pop   ax
        popf
        ret

enable_board_int endp




; Procedure:    init_rx_addrs
; Description:  clear RAR1-RAR15 registers
; Enter:
; Exit:

        public init_rx_addrs
init_rx_addrs proc near

; if EEPROM  present  RAR0 will be loaded from it
; Address Select field will be 00b
; and Address Valid will be 1b

; zero other recieve addresses
; Receive Array contane 15 addresses
        pushf
        cli
        mov   cx, UNICAST_RX_ADDR*2  ; Two registers for one address
        mov   dx, IOBase
        mov   eax,E1000_RA + 8       ; eax contane RAR1 address

NextReceiveAddr:
        push  ax
        out   dx,eax                 ; Set IOADDR register
        add   dx,4
        xor   ax,ax                  ; Store Address Value
        out   dx,eax
        sub   dx,4                   ; update dx to IOADDR
        pop   ax
        add   ax,4
        loop  NextReceiveAddr
        popf
        ret

init_rx_addrs endp




; Procedure:    get_linear_addr
; Description:  Return memory linear address
; Enter:        ds:si - memory address
; Exit:         eax - 32bit memory linear address

        public get_linear_addr
get_linear_addr proc near

        push  si
        xor   eax,eax
        mov   esi,eax
        pop   si
        mov   ax,ds
        shl   eax,4
        add   eax,esi

        ret
get_linear_addr endp


; Procedure:    enable_tx
; Description:  enable transmit unit
; Enter:
; Exit:

        public enable_tx
enable_tx proc near

        pushf
        cli
        LOAD_REG E1000_TCTL
        in    eax,dx
        or    ax,E1000_TCTL_EN
        out   dx,eax
        popf
        ret

enable_tx  endp

; Procedure:    enable_rx
; Description:  enable receive unit
; Enter:
; Exit:
        public enable_rx
enable_rx proc near

        pushf
        cli
        LOAD_REG E1000_RCTL
        in    eax,dx
        or    ax,E1000_RCTL_EN
        out   dx,eax
        popf
        ret

enable_rx   endp

; Procedure:    disable_tx
; Description:  disable transmit unit
; Enter:
; Exit:

        public disable_tx
disable_tx proc near

        pushf
        cli
        LOAD_REG E1000_TCTL
        in    eax,dx
        and   ax,not E1000_TCTL_EN
        out   dx,eax
        popf
        ret

disable_tx   endp

; Procedure:    disable_rx
; Description:  disable receive unit
; Enter:
; Exit:

        public disable_rx
disable_rx proc near
        pushf
        cli
        LOAD_REG E1000_RCTL
        in    eax,dx
        and   ax,not E1000_RCTL_EN
        out   dx,eax
        popf
        ret

disable_rx    endp


align   16
        public  TransmitBuffers, ReceiveBuffers

TransmitBuffers         db      (TX_BUFFER_NUMBER * TX_BUFFER_SIZE) dup (?)
ReceiveBuffers          db      (RX_BUFFER_NUMBER * RX_BUFFER_SIZE) dup (?)

; align 16

        public  TxRing, RxRing
TxRing  db (SIZE TxDescTable_type) dup (0)
RxRing  db (SIZE RxDescTable_type) dup (0)

even   
        public  recv_setup
recv_setup label word
        dw   0
        dw   r_mode_1
        dw   r_mode_2
        dw   r_mode_3
        dw   r_mode_4
        dw   r_mode_5
        dw   r_mode_6

extrn   tx_buffer_ptrs:word
extrn   rx_buffer_ptrs:word

extrn   IOBase:word
extrn   MediaType:word
extrn   speed:word
extrn   duplex:word
extrn   irq_number:byte
extrn   their_int:dword
extrn   driver_name:byte
extrn   mac_type:byte
even
r_mode_1:
        call  disable_rx
        ret

r_mode_2:
        call  init_rx_addrs
        LOAD_REG  E1000_RCTL
        in    eax,dx
        and   ax, not (E1000_RCTL_UPE or E1000_RCTL_BAM or E1000_RCTL_MPE)
        or    ax, E1000_RCTL_EN
        out   dx, eax
        ret

r_mode_3:
        call  init_rx_addrs
r_mode_3_RCTL:
        LOAD_REG  E1000_RCTL
        in    eax,dx
        and   ax, not (E1000_RCTL_UPE or E1000_RCTL_MPE) 
        or    ax, E1000_RCTL_EN or E1000_RCTL_BAM
        out   dx, eax
        ret

r_mode_4:
        call  set_multicast
        jmp   short r_mode_3_RCTL

r_mode_5:
	call  set_multicast
        LOAD_REG  E1000_RCTL
        in    eax, dx
        and   ax, (not E1000_RCTL_UPE)
        or    ax, E1000_RCTL_MPE or E1000_RCTL_EN or E1000_RCTL_BAM
        out   dx, eax
        ret

r_mode_6:
        LOAD_REG  E1000_RCTL
        in    eax,dx
        or    ax, E1000_RCTL_UPE or E1000_RCTL_MPE or E1000_RCTL_EN or E1000_RCTL_BAM
        out   dx, eax
        ret
_TEXT   ends


_INIT   segment para public USE16 'CODE'

extrn   setup_copper:near
extrn   read_phy_reg:near
extrn   write_phy_reg:near

; Procedure:    config_chip
; Description:  Performs chip configuration
; Enter:
; Exit:

        public config_chip
config_chip proc near
        DMESSAGE 2, "Config chip called"

; Zero the multicast table
        mov   cx, MULTICAST_TABLE_SIZE
        mov   dx, IOBase                    ; get IO port
        mov   eax,E1000_MTA                 ; set MTA address

NextMultiEntry:
        push  ax                            ; Save MTA entry address
        out   dx,eax                        ; set IOADDR register
        add   dx,4                          ; update dx to IODATA
        xor   ax,ax
        out   dx,eax                        ; Write new register value
        sub   dx,4                          ; dx point to IOADDR
        pop   ax                            ; Restore MTA entry address
        add   ax,4                          ; Update Entry address
        loop  NextMultiEntry

; Zero VLAN Filter table array
        mov   cx, VLAN_FILTER_TABLE_SIZE
        mov   dx, IOBase
        mov   eax,E1000_VFTA

NextVFEntry:
        push  ax
        out   dx,eax
        add   dx,4
        xor   ax,ax
        out   dx,eax
        sub   dx,4
        pop   ax
        add   ax,4
        loop  NextVFEntry

        call  init_rx_addrs               ; initialize Receive addresses

        LOAD_REG E1000_WUC
        in    eax, dx
        and   al, not (E1000_WUC_APME or E1000_WUC_PME_En)      ; disable APM Wake Up and ACPI Wake Up
        out   dx, eax

        cmp   mac_type, i82540
        jb    no_management

        LOAD_REG  E1000_MANC
        and   eax, not (E1000_MANC_RMCP_EN or E1000_MANC_ARP_EN or E1000_MANC_0298_EN or E1000_MANC_NEIGHBOR_EN)
        out   dx, eax

no_management:

        call  setup_fc_link
        jc    config_error

        call  get_sp_dx
        call  config_cold

        call  setup_tx

        call  setup_rx

        clc
config_error:
        ret

config_chip endp


; Procedure:    setup_fc_link
; Description:  Setup flow control and link
; Enter:
; Exit:

        public setup_fc_link
setup_fc_link  proc near
        DMESSAGE 2, "Setup flow control and link"

        call  setup_link
        jc    error_s_fc_link

        call  setup_fc
        clc
        ret

error_s_fc_link:
        stc
        ret

setup_fc_link  endp


; Procedure:    setup_link
; Description:  Setup link for copper and fiber adapters
; Enter:
; Exit:

        public setup_link
setup_link proc near
        DMESSAGE 2, "Setup link"

        call  setup_led
        LOAD_REG E1000_CTRL
        in    eax, dx
        and   ax, not (E1000_CTRL_FRCDPX or E1000_CTRL_FRCSPD)
        and   ax, not (E1000_CTRL_ASDE or E1000_CTRL_SLU)
        and   ax, not  E1000_CTRL_ILOS
        out   dx, eax

; Get media type 
        LOAD_REG E1000_STATUS
        in    eax,dx  
        and   ax, E1000_STATUS_TBIMODE
        mov   MediaType,ax
        jz    copper_setup
        call  setup_fiber
        ret

copper_setup:
        call  setup_copper
        ret

setup_link endp

; Procedure:    setup_led
; Description:  Setup Cordova similar led configuration
; Enter:
; Exit:

setup_led proc near
        cmp   mac_type, i82544
        jbe   no_led_setup
        LOAD_REG E1000_LEDCTL
        mov   eax, LED_CTRL_DAFAULT
        out   dx, eax
no_led_setup:
        clc
        ret 
setup_led endp


; Procedure:    setup_fiber
; Description:  Setup link for fiber adapters
; Enter:
; Exit:

        public setup_fiber
setup_fiber proc near
        DMESSAGE 3, "Setup Fiber Link"

        LOAD_REG E1000_RXCW               ; clear rx configuration word
        xor   eax,eax
        out   dx,eax
        
        LOAD_REG E1000_TXCW
        in    eax,dx
        and   ax, not (E1000_TXCW_ASM_DIR or E1000_TXCW_PAUSE) ;disable fc
        or    ax, E1000_TXCW_FD           ; Setup our duplex resolution
        and   ax, not E1000_TXCW_HD
        out   dx, eax      
        test  cmd_opt, MDPX_OPTION
        jnz   manual_s_f
        test  eax,E1000_TXCW_ANE          ; 0-1 transition ANE bit in TXCW register
        jz    restart_a_n                 ; restart AN 
        and   eax, not E1000_TXCW_ANE
        out   dx, eax
restart_a_n:
        or    eax,E1000_TXCW_ANE
        out   dx, eax                     ; restart hardware auto-negotiation process

        mov   cx, 50000
        LOAD_REG E1000_STATUS
wait_an_complete:
        UsecWait 10
        in    eax,dx
        test  al, E1000_STATUS_LU
        jnz   exit_s_f
        loop  wait_an_complete        
        LOAD_REG E1000_RXCW
        in    eax, dx
        test  eax,E1000_RXCW_C
        jz    manual_s_f                   ; Connected to non-AN link partner
error_s_f:        
        stc
        ret

manual_s_f:
        DMESSAGE 3, "Force Fiber Link"
        
        LOAD_REG  E1000_TXCW
        in    eax, dx
        and   eax, not E1000_TXCW_ANE
        out   dx, eax
        
        LOAD_REG E1000_CTRL
        or    ax, (E1000_CTRL_SLU or E1000_CTRL_FD)
        out   dx, eax

exit_s_f:
        clc
        ret
        
setup_fiber endp


; Procedure:    setup_fc
; Description:  Configurate flow control settings
; Enter:
; Exit:
        public setup_fc
setup_fc proc near
        DMESSAGE 2, "Setup flow control"
        
        LOAD_REG  E1000_FCAL
        mov   eax, E1000_FCAL_VALUE
        out   dx,eax

        LOAD_REG  E1000_FCAH
        mov   eax,E1000_FCAH_VALUE
        out   dx,eax

        LOAD_REG  E1000_FCT
        mov   eax,E1000_FCT_VALUE
        out   dx,eax

; Disable ability to transmit PAUSE frames

        LOAD_REG  E1000_FCRTL
        xor   ax,ax
        out   dx,eax

        LOAD_REG  E1000_FCRTH
        xor   ax,ax
        out   dx,eax

        ret

setup_fc endp


; Procedure:    get_sp_dx
; Description:  return speed and duplex settings
;               from device STATUS register
; Enter:
; Exit:

        public get_sp_dx
get_sp_dx  proc near
        DMESSAGE 3, "Get speed and duplex"

        LOAD_REG E1000_STATUS
        in    eax,dx

        mov   bx, HALF_DUPLEX
        test  ax, E1000_STATUS_FD
        jz    Duplex_H
        mov   bx, FULL_DUPLEX
Duplex_H:
        mov   duplex,bx                      ; store duplex value 

        mov   bx, SPEED_10
        and   ax,(E1000_STATUS_SPEED_100 or E1000_STATUS_SPEED_1000) ; mask speed value
        jz    exit_get_s_d                   ; if zero - speed 10Mb
        test  ax, E1000_STATUS_SPEED_100     
        mov   bx, SPEED_100
        jnz   exit_get_s_d
        mov   bx, SPEED_1000
exit_get_s_d:
        mov   speed,bx                       ; store speed value
        ret

get_sp_dx  endp


; Procedure:    config_cold
; Description:  Sets collision distance. 
;               chips check this value even in full duplex operation
; Enter:
; Exit:

        public config_cold
config_cold proc near
        DMESSAGE 3, "Setup collision distance"
        
        LOAD_REG E1000_TCTL
        in    eax,dx
        and   eax, not E1000_TCTL_COLD

        cmp   speed,SPEED_1000
        jne   hdx10_100

        cmp   duplex, FULL_DUPLEX
        jnz   gig_hdx
hdx10_100:
        or    eax,FD_COLLISION_DISTANCE
        jmp   save_value
gig_hdx:
        or    eax,HD_COLLISION_DISTANCE
save_value:
        out   dx,eax
        ret
config_cold endp


; Procedure:    setup_tx
; Description:  Initialize transmit unit
; Enter:
; Exit:         
        public  setup_tx
setup_tx proc near
        DMESSAGE 2, "Setup transmit unit"

        push  si
        push  di

        mov   di,offset TxRing

        push  di

        mov   si,offset TransmitBuffers

        call  get_linear_addr

        mov   cx,TX_BUFFER_NUMBER

        xor   ebx,ebx
        mov   edx,ebx
        mov   dx,TX_BUFFER_SIZE

NextTXDesc:

        mov   [bx+offset tx_buffer_ptrs],si

        mov   DWORD PTR (Xmit_Descr_Type PTR [di]).taddr_lo,eax
        mov   BYTE PTR (Xmit_Descr_Type PTR[di]).command,(XD_CMD_EOP or XD_CMD_RPS or XD_CMD_IFCS)
        mov   BYTE PTR (Xmit_Descr_Type PTR[di]).tstatus, XD_STAT_DD

        add   bx, 2
        add   eax,edx
        add   si,dx
        add   di,size Xmit_Descr_Type

        loop  NextTXDesc

; Setup Transmit Registers

; Setup 64bit Base Address

        LOAD_REG  E1000_TDBAL
        pop   si                          ; get Descriptor Table offset

        call  get_linear_addr
        out   dx,eax

        LOAD_REG  E1000_TDBAH
        xor   ax,ax
        out   dx,eax

; Setup Length Register
        LOAD_REG  E1000_TDLEN
        mov   eax,(TX_DESCRIPTOR_NUMBER*SIZE Xmit_Descr_Type)
        out   dx,eax

; Setup Head and Tail registers

        LOAD_REG  E1000_TDH
        xor   ax,ax
        out   dx,eax

        LOAD_REG  E1000_TDT
        xor   ax,ax
        out   dx,eax

; Setup Transmit IPG Register
        LOAD_REG  E1000_TIPG
        mov   eax,00602006h
        out   dx,eax

; Setup Transmit Control Register
        LOAD_REG  E1000_TCTL
        in    eax,dx
        and   ax, not E1000_TCTL_CT
        or    ax, E1000_TCTL_CT_VALUE

; Enable  short packets padding
        or    ax, E1000_TCTL_PSP
        out   dx,eax

        pop   di
        pop   si

        ret
setup_tx endp


; Procedure:    setup_rx
; Description:  Initialize receive unit
; Enter:
; Exit:

        public  setup_rx
setup_rx proc near
        DMESSAGE 2, "Setup receive unit"

        push  si
        push  di


        mov   di,offset RxRing

        push  di

        mov   si,offset ReceiveBuffers
        call  get_linear_addr

        mov   cx,RX_BUFFER_NUMBER

        xor   ebx,ebx
        mov   edx,ebx
        mov   dx,RX_BUFFER_SIZE

NextRXDesc:
        mov   [bx+offset rx_buffer_ptrs],si
        mov   DWORD PTR (recv_descr_type PTR [di]).raddr_lo,eax
        add   bx, 2
        add   eax,edx
        add   si,dx

        add   di,size recv_descr_type

        loop  NextRXDesc

; Setup Receive Registers

; Setup Receive Descriptors Base Address

        LOAD_REG E1000_RDBAL

        pop   si
        call  get_linear_addr
        out   dx,eax

        LOAD_REG E1000_RDBAH
        xor   ax,ax
        out   dx,eax

; Setup Receive Descriptor Table Length

        LOAD_REG E1000_RDLEN
        mov   eax,(RX_DESCRIPTOR_NUMBER*SIZE recv_descr_type)
        out   dx,eax

; Setup Head & Tail Receive Descriptors

        LOAD_REG E1000_RDH
        xor   ax,ax
        out   dx,eax

        LOAD_REG E1000_RDT
        xor   ax,ax
        out   dx,eax

; Setup Receive Control Register
        LOAD_REG E1000_RCTL
        in    eax,dx
        or    eax, (E1000_RCTL_BAM or E1000_RCTL_SECRC or E1000_RCTL_DPF)
        out   dx,eax

; Setup rx interrupt delay
        cmp   mac_type, i82544
        jbe   no_int_delay

        LOAD_REG E1000_RDTR            ; this registers setup
        mov   ax, RX_INT_DELAY         ; will cause receive timer inerrupt
        out   dx, eax                  ; wheh RADT count reaches 0
        LOAD_REG E1000_RADV
        mov   ax, RX_INT_DELAY
        out   dx, eax

; Enable Small Packet Detection
;        LOAD_REG E1000_RSRPD
;        mov   ax, SMALL_PKT_SIZE
;        out   dx, eax
no_int_delay:

        pop   di
        pop   si

        ret
setup_rx endp

; Procedure:    find_nic
; Description:  search device and setup PCI cmd and PCI-X cmd registers
; Enter:
; Exit:         carry set if any error

        public  find_nic
find_nic proc near
        DMESSAGE 2, "Find device called"

        mov     ah, PCI_FUNCTION_ID
        mov     al, PCI_BIOS_PRESENT
        int     PCI_BIOS_INTERRUPT
        jnc     check_return_values
no_bios:
        mov     dx, offset no_pci_bios
        jmp     error
check_return_values:
        cmp     ah, PCI_SUCCESSFULL
        jne     no_bios
        cmp     edx, ' ICP'
        jne     no_bios

        call    get_pci_info                    ; Go and search the card
        jc      short f_n_failed                ; Jump if card not found

        call    check_opt                       ; check speed duplex settings
        jnc     options_ok
        jmp     error
options_ok:

; Get command register
        mov     ah, PCI_FUNCTION_ID             ; Read PCI Command Register
        mov     al, READ_PCI_CONFIG_WORD
        mov     bx, pci_bus_dev_no              ; Recover the bus / device number
        mov     di, COMMAND_REGISTER
        int     PCI_BIOS_INTERRUPT
        cmp     ah, PCI_SUCCESSFULL
        jne     short f_n_failed

        cmp     mac_type, i82547                ; setup command register
        je      not_mwi
        or      cl, CMD_MWI_ENABLE
not_mwi:
        or      cl, CMD_BUS_MASTER + CMD_IO_SPACE + CMD_MEMORY_SPACE
        mov     ah, PCI_FUNCTION_ID
        mov     al, WRITE_PCI_CONFIG_WORD
        int     PCI_BIOS_INTERRUPT
        cmp     ah, PCI_SUCCESSFULL
        jne     short f_n_failed

        LOAD_REG E1000_STATUS
        in      eax,dx
        test    ax, E1000_STATUS_PCIX
        jz      pci_mode

; Setup correct MMRBC value in PCI-X command register
        mov     ah, PCI_FUNCTION_ID             ; Read PCI-X status register
        mov     al, READ_PCI_CONFIG_WORD
        mov     di, PCIX_STATUS_REG_HW
        int     PCI_BIOS_INTERRUPT
        cmp     ah, PCI_SUCCESSFULL
        jne     short f_n_failed

        mov     dx, cx                          ; Save PCI-X status value
        and     dx, PCIX_ST_DMMRBC_MASK         ; Mask Designed Maximum Memory
                                                ; Read Byte Count value

        mov     ah, PCI_FUNCTION_ID             ; Read PCI-X command register
        mov     al, READ_PCI_CONFIG_WORD
        mov     di, PCIX_COMMAND_REG
        int     PCI_BIOS_INTERRUPT
        cmp     ah, PCI_SUCCESSFULL
        jne     short f_n_failed

        and     cx, not PCIX_CMD_MMRBC_MASK     ; Zero DMMRBC bits in cmd register
        shr     dx, (PCIX_ST_DMMRBC_SHIFT - PCIX_CMD_MMRBC_SHIFT)
        or      cx, dx                          ; Set MMRBC bits to DMMRBC value

        mov     ah, PCI_FUNCTION_ID
        mov     al, WRITE_PCI_CONFIG_WORD
        int     PCI_BIOS_INTERRUPT
        cmp     ah, PCI_SUCCESSFULL
        jne     short f_n_failed

pci_mode:
        clc
        ret

f_n_failed:
        stc
        ret

find_nic  endp

; Procedure:    get_pci_info
; Description:  Returns chip PCI information
; Enter:
; Exit:         carry set if any error
        public  get_pci_info
get_pci_info proc near
        DMESSAGE 3,"Get device information"

        xor   si, si                            ; device index
next_chip:
        mov   ah, PCI_FUNCTION_ID               ; find ethernet controller
        mov   al, FIND_PCI_CLASS_CODE
        mov   ecx,020000h
        int   PCI_BIOS_INTERRUPT
        cmp   ah, PCI_SUCCESSFULL
        jne   last_device
        call  check_our_nic                     ; is it known controller
        jc    not_our_nic
        inc   nic_found
        mov   ax, nic_found
        cmp   ax, nic_number                    ; should we setup it
        je    found_desired
not_our_nic:
        inc   si
        jmp   short next_chip
last_device:
        mov   dx, offset no_nics
        test  nic_found, 0FFFFh
        jz    no_nic_found
        mov   dx, offset nic_num_err
no_nic_found:
        jmp   error

found_desired:
        call  get_mac
        mov   pci_bus_dev_no, bx        ; Save bus/device number

        mov   ah, PCI_FUNCTION_ID       ; Get Memory BAR
        mov   al, READ_PCI_CONFIG_DWORD
        mov   di, 10h                   
        int   PCI_BIOS_INTERRUPT
        cmp   ah, PCI_SUCCESSFULL
        jne   short error_get_pci_info

        mov   si, 4
        and   cl, BIT_2 or BIT_1
        jz    bit32_BAR
        cmp   cl, BIT_2
        jne   short error_get_pci_info
        mov   si, 8
bit32_BAR:
        add   di, si
        mov   ah, PCI_FUNCTION_ID       ; Read next BAR
        mov   al, READ_PCI_CONFIG_DWORD
        int   PCI_BIOS_INTERRUPT
        cmp   ah, PCI_SUCCESSFULL
        jne   short error_get_pci_info
        test  cl, BIT_0                 ; Is it IO BAR
        jnz   found_io_bar

        add   di, si
        mov   ah, PCI_FUNCTION_ID       ; Check other BAR
        mov   al, READ_PCI_CONFIG_DWORD
        int   PCI_BIOS_INTERRUPT
        cmp   ah, PCI_SUCCESSFULL
        jne   short error_get_pci_info

found_io_bar:
        mov   dl, cl                    ; test BAR for correct IO info
        and   dl, 3
        cmp   dl, 1
        jne   error_get_pci_info  
        and   cl, 0FCh                  ; Mask not meaning bits

        mov   IOBase, cx
        mov   ah, PCI_FUNCTION_ID       ; Get IRQ number
        mov   al, READ_PCI_CONFIG_BYTE
        mov   di, 3ch
        int   PCI_BIOS_INTERRUPT
        cmp   ah, PCI_SUCCESSFULL

        jne   short error_get_pci_info

        mov   irq_number, cl

        clc
        ret

error_get_pci_info:
        stc
        ret

get_pci_info    ENDP

; Procedure:    check_our_nic
; Description:  Check PCI VENDOR and DEVICE ID
; Enter:
; Exit:         carry set if device not supported
;               dx - chip number in DEVICE ID list

check_our_nic proc near
        DMESSAGE 4, "Check nic called"

        push   si
        mov    ah, PCI_FUNCTION_ID
        mov    al, READ_PCI_CONFIG_DWORD
        xor    di, di
        int    PCI_BIOS_INTERRUPT
        cmp    cx, VENDOR_ID
        jne    Bad_DeviceId
        shr    ecx,16
        mov    si, offset DeviceId_list
        mov    dx, [si]
next_device_entry:
        add    si, 2                
        cmp    cx, [si]
        je     found_our
        dec    dx
        jnz    next_device_entry

Bad_DeviceId:
        stc
exit_ch_o_n:
        pop    si
        ret
found_our:
        clc
        jnc    exit_ch_o_n
check_our_nic endp

; Procedure:    get_mac
; Description:  set device membership
; Enter:        dx - chip number in DEVICE ID list
; Exit:         mac_type set to chip detection

get_mac proc near
        DMESSAGE 4, "Get mac called"

        mov    al, (offset DeviceId_end-DeviceId_list)/2
        sub    al, dl
        
        mov    dl, i82544
        cmp    al, (offset e1000_82540 - offset e1000_82544)/2
        jbe    exit_g_m
        inc    dl
        cmp    al, (offset e1000_82545 - offset e1000_82544)/2
        jbe    exit_g_m
        inc    dl
        cmp    al, (offset e1000_82541 - offset e1000_82544)/2
        jbe    exit_g_m
        inc    dl
        cmp    al, (offset e1000_82547 - offset e1000_82544)/2
        jbe    exit_g_m
        inc    dl
exit_g_m:        
        mov    mac_type, dl
        ret
get_mac endp

usage_error:
	mov     dx, offset usage_msg
	jmp     error

; Procedure:    get_options
; Description:  analyze cmd line options
; Enter:
; Exit:         cmd_opt set 

get_options proc near
        call    skip_space
        cmp     al,13
        jz      usage_error
        mov     bl, cmd_opt
        mov     dx,si
        dec     dx
next_opt:
	call	skip_space
	cmp	al,'-'			; check if options
	jne	no_more_opt
	inc	si			
	lodsb
	mov     dx,si
	cmp	al,'i'
	jne	not_i_opt
	mov	ax, word ptr driver_class
        xchg    ah, al
        mov     word ptr driver_class,ax
	jne	next_opt
not_i_opt:
	cmp	al,'n'
	jne	not_n_opt
	or	bl,N_OPTION
	jne	next_opt
not_n_opt:
	cmp	al,'p'
	jne	not_p_opt
	or      bl,P_OPTION
	jne	next_opt
not_p_opt:
	cmp	al,'u'
	jne	not_u_opt
	or	bl,U_OPTION
	jne	next_opt
not_u_opt:
	jmp	usage_error
no_more_opt:
        cmp     dx,si
        jne     save_opt
        cmp     al, 13
        jne     not_u_opt
save_opt:        
        mov     cmd_opt, bl
        ret
get_options endp

cmd_line_error:
        jmp     error


; Procedure:    cmd_line_par
; Description:  parses cmd line settings
; Enter:          0822404302
; Exit:

	public  cmd_line_par
cmd_line_par proc near
        DMESSAGE 1,"Parsing cammand line parameters"
        
        cld
        mov     si,81h
        call    to_low_case
        call    get_options
        
        call    skip_space
        cmp     al, 13
        je      no_int

        call    get_byte
        jnc     int_ok
        mov     dx, offset packet_int_msg
        jmp     error
int_ok:
        mov     com_int,al
no_int:
	test	cmd_opt,U_OPTION	;should we terminate the driver?
	jne	terminate
	jmp	not_terminate
terminate:
        cmp     com_int,0               ; check if user specified driver interrupt
        jnz     int_specified
        call    find_pktdrv             ; find driver
        jc      cmd_line_error
int_specified:
        call    verify_int
        jc      cmd_line_error
        mov     dx, offset no_pkt_msg
        jnz     cmd_line_error

        mov     (segmoffs ptr their_int).offs, bx
        mov     (segmoffs ptr their_int).segm, es

        mov     ax, 1ffh                ; get driver information
        pushf    
        cli 
        call    their_int
        jnc     driver_info_ok
        mov     dx, offset bad_driver_info
        jmp     error
driver_info_ok:
        push    cx
        push    cs
        pop     es
        mov     di, offset driver_name   ; check driver name
        mov     cx, DRV_NAME_LENGTH
        repe    cmpsb

        pop     cx
        push    cs
        pop     ds
       
        jz      our_name
        
        mov     dx, offset bad_driver_name
        jmp     error
our_name:
	mov	ah,2			;access_type
	mov	al,ch			;their class from driver_info().
	mov	bx,dx			;their type from driver_info().
	mov	dl,cl			;their number from driver_info().
	mov	cx,2			;use type length 2.
	mov	si,offset not_recv_type
 	mov	di,offset not_recv
	pushf
	cli
	call	their_int
        jnc     access_type_ok
        mov     dx, offset skip_str
        jmp     error
access_type_ok:

        mov     bx, ax
        mov     ah, 5
        pushf
        cli 
        call    their_int               ; terminate driver
        jnc     terminate_ok
        mov     dx, offset cannot_exit
        jmp     error

terminate_ok:
	mov	dx,offset terminated_msg
	mov	ah,9
	int	21h
	int	20h

not_terminate:
        call    skip_space
        cmp     al, 13
        je      parsing_complete

        call    get_byte
        jnc     nic_correct
        mov     dx, offset nic_num_err
        jmp     error
nic_correct:        
        mov     nic_number, ax
        call    skip_space
        cmp     al, 13
        je      parsing_complete

        mov     bx, offset speed_list 
        call    str_present
        jnc     speed_str_ok        
        mov     dx, offset bad_keyword
        jmp     error
speed_str_ok:
        or      cmd_opt, MSPD_OPTION
        mov     speed, ax
        call    skip_space
        cmp     al, 13
        je      parsing_complete
        
        mov     bx, offset duplex_list 
        call    str_present
        jnc     duplex_str_ok
        mov     dx, offset bad_keyword
        jmp     error
duplex_str_ok:
        or      cmd_opt, MDPX_OPTION
        mov     duplex, ax
        call    skip_space
        cmp     al, 13
        je      parsing_complete
        mov     dx, offset too_many_par
        jmp     error
        
parsing_complete:
        call    verify_int
        jnc     packet_int_in_range
        jmp     short bad_install
packet_int_in_range:
        jne     int_free
        mov     dx, offset already_pkt_msg
bad_install:        
        jmp     error
int_free:
	ret

cmd_line_par endp


; Procedure:    check_opt
; Description:  check fiber speed & duplex settings
; Enter:
; Exit:         carry set if any error

check_opt proc near
        LOAD_REG E1000_STATUS
        in      eax, dx
        test    al, E1000_STATUS_TBIMODE
        jz      exit_ch_o
        test    cmd_opt,MSPD_OPTION
        jz      exit_ch_o
        cmp     speed, SPEED_1000
        je      check_duplex
        mov     dx, offset tbi_speed
        jmp     error_ch_o
check_duplex:
        test    cmd_opt,MDPX_OPTION
        jz      exit_ch_o
        cmp     duplex, FULL_DUPLEX
        je      exit_ch_o
        mov     dx, offset tbi_duplex
        jmp     error_ch_o
exit_ch_o:
        clc
        ret
        
error_ch_o:
        stc
        ret
check_opt endp


; Procedure:    str_present
; Description:  
; Enter:        bx - point on strings array
;               si - point on search string
; Exit:         carry set if not present
;               ax - found string array index

str_present proc near
        cld
        xor     ax, ax
        mov     cx, [bx]

next_str:
        push    cx
        push    si
        inc     ax
        add     bx, 2
        mov     di, [bx]
        mov     cx, 0ffffh
        repe    cmpsb
        dec     di
        xor     dl, dl
        cmp     dl, [di]
        jne     not_last
        dec     si
        mov     dl, [si]
        cmp     dl, 20h
        je      matche_one
        cmp     dl, 13
        je      matche_one
        cmp     dl, 9
        je      matche_one
        
not_last:
        pop     si
        pop     cx
        dec     cx
        jnz     next_str
        stc
        ret
        
matche_one:
        pop     cx
        pop     cx
        clc
        ret
str_present endp

; Procedure:    find_pktdrv
; Description:  tryes to find installed packet driver
; Enter:
; Exit:         carry set if any error
;               com_int - driver interrupt
find_pktdrv  proc near
        mov     com_int, 60h
next_pkt_int:
        call    verify_int
        jc      bad_int
        jnz     bad_signature
        cmp     packet_int_fd, 0
        jnz     more_that_one
        mov     al, com_int
        mov     packet_int_fd, al
bad_int:
bad_signature:
        inc     com_int
        cmp     com_int, 80h
        jbe     next_pkt_int
        mov     al, packet_int_fd
        or      al, al
        jnz     found_one_drv
        mov     dx, offset no_pktdrv_found
        jmp     short find_pktdrv_error
found_one_drv:
        mov     com_int, al
        clc
        ret

more_that_one:
        mov     dx, offset more_one_msg
find_pktdrv_error:        
        stc
        ret
find_pktdrv  endp


; Procedure:    verify_int
; Description:  Checks driver interrupt setting
; Enter:
; Exit:         carry set if int number without range
;               zero set if ok


verify_int proc near
        mov     al, com_int
	cmp	al,60h	                ;make sure that the packet interrupt
	jb	verify_packet_int_bad	;  number is in range.
	cmp	al,67h                  ;make sure that the packet interrupt
	je	verify_packet_int_bad	;  number is in range.
	cmp	al,70h                  ;make sure that the packet interrupt
	jb	verify_packet_int_ok	;  number is in range.
	cmp	al,7bh                  ;make sure that the packet interrupt
	jb	verify_packet_int_bad	;  number is in range.
	cmp	al,7eh
	jbe	verify_packet_int_ok
verify_packet_int_bad:
	mov	dx,offset packet_int_msg
	stc
	ret
	
verify_packet_int_ok:
	mov	ah,35h			;get their packet interrupt.
	int	21h

	lea	di,3[bx]		;see if there is already a signature
	mov	si,offset CGROUP:__pktdrv+3	;  there.
	mov	cx,9
	repe	cmpsb
	clc
	ret
        
verify_int endp

; Procedure:    skip_space
; Description:  skip blank symbols
; Enter:        si points on string
; Exit:         si point on first non blank symbol
skip_space proc near
next_space:
        lodsb
        cmp   al, ' '
        je    next_space
        cmp   al, 09
        je    next_space
	dec   si
        ret
skip_space endp


; Procedure:    to_low_case
; Description:  Convert symbols to low case
; Enter:        si points on string
; Exit:
to_low_case proc near
        push  si
next_ch:
        lodsb
        cmp   al, 13
        je    last_sym
        cmp   al, 'A'
        jb    next_ch
        cmp   al, 'Z'
        ja    next_ch
        or    al, 20h
        dec   si
        mov   [si], al
        inc   si
        jnz   next_ch
last_sym:        
        pop   si
        ret
to_low_case endp

extrn skip_str:byte
error:  mov   bx, dx
        mov   ah, 9
        mov   dx, offset skip_str
        int   21h
        mov   dx, bx
        int   21h
        
        mov   ax,4c01h
        int   21h

not_recv proc  far
	xor	di,di
	mov	es,di
	ret
not_recv endp


; Procedure:    get_byte
; Description:  Get byte from string
; Enter:        si points on string
; Exit:         carry set if not a byte value
;               ax - retrieved byte
      public get_byte
get_byte proc near
      call  skip_space
      call  get_digit
      jnc   number_present
      jmp   short exit_g_b
number_present:
      mov   bx, 10
      xor   dx, dx
      
      cmp   al, bl
      jae   bad_number 
      or    al, al
      jnz   use_decimal
      inc   si
      mov   bl, 16
      lodsb 
      or    al, 20h
      cmp   al, 'x'
      je    hex
      mov   bl, 8
      dec   si

hex:
use_decimal:
      xor   cx,cx

next_digit:
      lodsb
      call  get_digit
      jc    last_digit
      inc   cl
      xchg  al, dl
      mul   bl
      add   dx, ax
      test  dx, 0ff00h
      jnz   bad_number
      jmp   next_digit
last_digit:
      or    cl,cl
      je    bad_number
      dec   si
      mov   ax, dx
      clc
     
exit_g_b:
      ret

bad_number:
      stc
      ret
get_byte endp


; Procedure:    get_digit
; Description:  check digit value
; Enter:        al - symbol to check
; Exit:         carry set if not a number

get_digit proc near
      cmp   al, '0'            ; check decimal
      jb    not_digit
      cmp   al, '9'
      jbe   decimal
      or    al, 20h
      cmp   al, 'a'
      jb    not_digit
      cmp   al, 'f'
      ja    not_digit
      sub   al, 'a' - '0' - 10
decimal:
      sub   al, '0'
      clc
      ret
not_digit:
      stc
      ret
get_digit endp


extrn   usage_msg:byte

packet_int_fd   db      0
even
        public pci_bus_dev_no
pci_bus_dev_no  dw      ?  ; Storage for PCI bus / device number
not_recv_type   dw      0
nic_number      dw      1
nic_found       dw      0

DeviceId_list label word
                dw      (DeviceId_end-2 - $)/2
e1000_82544     dw      i82544EI_COPPER
                dw      i82544EI_FIBER
                dw      i82544GC_COPPER
                dw      i82544GC_LOM
e1000_82540     dw      i82540EM
                dw      i82540EM_LOM
                dw      i82540EP_LOM1
                dw      i82540EP_LOM2
                dw      i82540EP
e1000_82545     dw      i82545EM_COPPER
                dw      i82545EM_FIBER
e1000_82541     dw      i82541EI
                dw      i82541ER
                dw      i82541EI_MOBILE 
                dw      i82541PI
e1000_82547     dw      i82547EI
                dw      i82547EI_MOBILE
DeviceId_end:

speed_list    label word
                dw      (speed_end-2 - $)/2
                dw      offset speed10
                dw      offset speed100
                dw      offset speed1000
speed_end:

duplex_list   label word
                dw      (duplex_end-2 - $)/2
                dw      offset duplex_half
                dw      offset duplex_full
duplex_end:

already_pkt_msg db      "There is already packet driver installed. Choose another interrupt",13,10,
                        "or uninstall exist driver if possible",13,10,'$'
no_pktdrv_found db      "No packet driver found between 0x60 and 0x80",13,10,'$'
no_pkt_msg	db	"There is no packet driver",13,10,'$'
more_one_msg    db      "There are more that one packet driver found",13,10,'$'
terminated_msg	db	13,10,"Uninstall completed",13,10,'$'
packet_int_msg	db	13,10
		db	"Error: <packet_int_no> should be 0x60->0x66, 0x68->0x6f, or 0x7b->0x7e",13,10
		db	"       0x67 is the EMS interrupt, and 0x70 through 0x77 are used by second 8259,"
		db	"       and 0x7a is used by NetWare's IPX"
		db	'$'
bad_driver_info db      "Cannot get driver information",13,10,'$'
bad_driver_name db      "There is not our driver",13,10,'$'
cannot_exit	db	"This packet driver cannot terminate",13,10,'$'
no_pci_bios     db      "No PCI BIOS present",13,10,'$'
no_nics         db      "Cannot find supported network card",13,10,'$'
nic_num_err     db      "Incorrect nic specified",13,10,'$'
bad_keyword     db      "Bad command line keyword",13,10,'$'
too_many_par    db      "Too many parameters",13,10,'$'
tbi_speed       db      "Only 1000Mb/s speed valid for fiber adapters",13,10,'$'
tbi_duplex      db      "Only full-duplex operation support in TBI mode",13,10,'$'

speed10         db      "10",0
speed100        db      "100",0
speed1000       db      "1000",0

duplex_half     db      "half",0
duplex_full     db      "full",0
_INIT   ends
        end
