;

; PHY registers definitions


; PHY address definition 
PHY_ADDR           equ  0001h

; MII managment register set
PHY_CTRL           equ  00   ; Control Register
PHY_STATUS         equ  01   ; Status Register
PHY_ID0            equ  02   ; PHY Identifier byte 0
PHY_ID1            equ  03   ; PHY Identifier byte 1
PHY_ANA            equ  04   ; Auto-Negotiation Advertisement Register
PHY_ANLPBPA        equ  05   ; Auto-Negotiation Link Partner
                             ; base Page Ability Register
PHY_ANE            equ  06   ; Auto-Negotiation Expansion Register
PHY_ANNPT          equ  07   ; Auto-Negotiation Next Page Transmit Register
PHY_ANLPRNP        equ  08   ; Auto-Negotiation Link Partner 
                             ; Received Next Page Register

                
PHY_MSCTRL         equ  09   ; Master-Slave Control Register
PHY_MSST           equ  10   ; Master-Slave Status Register
PHY_ESTATUS        equ  15   ; Extended Status Register
PHY_SCTRL          equ  16   ; PHY Specific Control Register
PHY_PC             equ  16   ; Port Configuration Register
PHY_SST            equ  17   ; PHY Specific Status Register
PHY_PS             equ  17   ; Port Status Register
PHY_IE             equ  18   ; Interrupt Enable Register
PHY_PCTRL          equ  18   ; Port Control Register
PHY_IST            equ  19   ; Interrupt Status Register
PHY_LH             equ  19   ; Link Health Register
PHY_ESCTRL         equ  20   ; Extended PHY Specific Control Register



; Main PHY Registers bit definitions 

; RW  - bit is read/write able
; SC  - bit is self-clearing
; RO  - bit is read only
; LL  - bit is Lathing Low
; LH  - bit is Latching High 


; Control Register bit definitions
PHY_CTRL_RST        equ BIT_15   ; RW  SC ; 1=PHY reset
                                 ;        ; 0=normal operation
PHY_CTRL_LB         equ BIT_14   ; RW     ; 1=enable loopback mode
                                 ;        ; 0=disable loopback mode
PHY_CTRL_LSB        equ BIT_13   ; RW     ; This bits is used for
PHY_CTRL_MSB        equ BIT_6    ; RW     ; manual speed selection
PHY_CTRL_ANE        equ BIT_12   ; RW     ; 1=Enable Auto-Negotiation Process
                                 ;        ; 0=Disable Auto-Negotiayion Process
PHY_CTRL_PD         equ BIT_11   ; RW     ; 1=Power down
                                 ;        ; 0=normal operation
PHY_CTRL_IS         equ BIT_10   ; RW     ; 1=Electrically Isolate PHY from MII or GII
                                 ;        ; 0=normal operation
PHY_CTRL_RAN        equ BIT_9    ; RW SC  ; 1=Restart Auto_Negotiation Process
                                 ;        ; 0=normal operation
PHY_CTRL_DM         equ BIT_8    ; RW     ; 1=Full Duplex
                                 ;        ; 0=Half Duplex
PHY_CTRL_CT         equ BIT_7    ; RW     ; 1=Enable COL signal test
                                 ;        ; 0=disable COL signal test

; Manual speed selection values
PHY_CTRL_SP_MASK    equ PHY_CTRL_MSB + PHY_CTRL_LSB
PHY_CTRL_SP1000     equ PHY_CTRL_MSB
PHY_CTRL_SP100      equ PHY_CTRL_LSB
PHY_CTRL_SP10       equ 0


; Status Register bit definitions
PHY_ST_100T4        equ BIT_15   ; RO     ; 1=PHY able to perfom 100BASE-T4  
                                 ;        ; 0=PHY not able to perfom 100BASE-T4
PHY_ST_100XFD       equ BIT_14   ; RO     ; 1=PHY able to perfom full duplex 100BASE-X
                                 ;        ; 0=PHY not able to perfom full duplex 100BASE-X
PHY_ST_100XHD       equ BIT_13   ; RO     ; 1=PHY able to perfom half duplex 100BASE-X
                                 ;        ; 0=PHY not able to perfom half duplex 100BASE-X
PHY_ST_10FD         equ BIT_12   ; RO     ; 1=PHY able to operate at 10 Mb/s in full duplex mode
                                 ;        ; 0=PHY not able to operate at 10 Mb/s in full duplex mode
PHY_ST_10HD         equ BIT_11   ; RO     ; 1=PHY able to operate at 10 Mb/s in half duplex mode
                                 ;        ; 0=PHY not able to operate at 10 Mb/s in half duplex mode
PHY_ST_100T2F       equ BIT_10   ; RO     ; 1=PHY able to perfom full duplex 100BASE-T2
                                 ;        ; 0=PHY not able to perfom full duplex 100BASE-T2
PHY_ST_100T2H       equ BIT_9    ; RO     ; 1=PHY able to perfom half duplex 100BASE-T2
                                 ;        ; 0=PHY not able to perfom half duplex 100BASE-T2
PHY_ST_ES           equ BIT_8    ; RO     ; 1=Extended Status information in reg 15
                                 ;        ; 0=no extended Status information in reg 15
PHY_ST_MFPS         equ BIT_6    ; RO     ; 1=PHY will accept managment frames with preamble suppressed
                                 ;        ; 0=PHY will not accept managment frames with preamble suppressed
PHY_ST_ANC          equ BIT_5    ; RO     ; 1=Auto-Negotiation process completed
                                 ;        ; 0=Auto-Negotiation process not completed
PHY_ST_RF           equ BIT_4    ; RO  LH ; 1=remote fault condition detected
                                 ;        ; 0=no remote fault condition detected
PHY_ST_ANA          equ BIT_3    ; RO     ; 1=PHY is able to perfom Auto-Negotiation 
                                 ;        ; 0=PHY is not able to perfom Auto-Negotiation 
PHY_ST_LS           equ BIT_2    ; RO  LL ; 1=link is up
                                 ;        ; 0=link is down
PHY_ST_JD           equ BIT_1    ; RO  LH ; 1=jabber condition detected
                                 ;        ; 0=no jabber condition detected
PHY_ST_EC           equ BIT_0    ; RO     ; 1=extended register capabilites
                                 ;        ; 0=basic register set capabilities only

; Auto-Negotiation Advertisement Register bit definitions
PHY_ANA_NP          equ BIT_15   ; RW     ; 1=Next Page Advertise
                                 ;        ; 0=Don't advertise
PHY_ANA_RF          equ BIT_13   ; RW     ; 1=Set Remote Fault bit
                                 ;        ; 0=Do not set Remote Fault bit
PHY_ANA_AP          equ BIT_11   ; RW     ; 1=Asymmetric pause
                                 ;        ; 0=No asymmetric pause
PHY_ANA_PAUSE       equ BIT_10   ; RW     ; 1=MAC PAUSE implemented
                                 ;        ; 0=MAC PAUSE not implemented
PHY_ANA_100T4       equ BIT_9    ; RO     ; 0=Not capable of 100BASE-T4
PHY_ANA_100TXFD     equ BIT_8    ; RW     ; 1=Advertise 100BASE-TX Full Duplex
                                 ;        ; 0=Don't Advertise 100BASE-TX Full Duplex
PHY_ANA_100TXHD     equ BIT_7    ; RW     ; 1=Advertise 100BASE-TX Half Duplex
                                 ;        ; 0=Don't Advertise 100BASE-TX Half Duplex
PHY_ANA_10TXFD      equ BIT_6    ; RW     ; 1=Advertise 10BASE-TX Full Duplex
                                 ;        ; 0=Don't Advertise 10BASE-TX Full Duplex
PHY_ANA_10TXHD      equ BIT_5    ; RW     ; 1=Advertise 10BASE-TX Half Duplex
                                 ;        ; 0=Don't Advertise 10BASE-TX Half Duplex
PHY_ANA_SF          equ 00001b   ;        ; value of IEEE 802.3 Selector Field                                 

; Auto-Negotiation Expansion Status Register
PHY_ANEST_PDF       equ BIT_4    ; RO LH  ; 1=Parallel Detection Fault has been detected
                                 ;        ; 0=Parallel Detection Fault has not been detected
PHY_ANEST_LPNPA     equ BIT_3    ; RO     ; 1=Link Partner is Next Page Able
                                 ;        ; 0=Link Partner is not Next Page Able
PHY_ANEST_LNPE      equ BIT_2    ; RO     ; 1=Local Device is Next Page Able
                                 ;        ; 0=Local Device is not Next Page Able
PHY_ANEST_NPR       equ BIT_1    ; RO LH  ; 1=A New Page has been received
                                 ;        ; 0=A New Page has not been received
PHY_ANEST_LPANA     equ BIT_0    ; RO     ; 1=Link Partner Auto-Negotiation Able
                                 ;        ; 0=Link Partner not AutoNeg Able

; Master-Slave Control Register bit definitions
PHY_MSCTRL_MSCE     equ BIT_12   ; RW     ; 1=Manual MASTER/SLAVE configuration
                                 ;        ; 0=Automatic MASTER/SLAVE configuration
PHY_MSCTRL_MSCV     equ BIT_11   ; RW     ; 1=Manual configuration as MASTER
                                 ;        ; 0=Manual configuration as SLAVE
PHY_MSCTRL_PT       equ BIT_10   ; RW     ; 1=Prefer multiport device (MASTER)
                                 ;        ; 0=Prefer single port device (SLAVE)
PHY_MSCTRL_1000TFD  equ BIT_9    ; RW     ; 1=Advertise 1000BASE-T Full Duplex
                                 ;        ; 0=Not Advertise 1000BASE-T Full Duplex
PHY_MSCTRL_1000THD  equ BIT_8    ; RW     ; 1=Advertise 1000BASE-T Half Duplex
                                 ;        ; 0=Not Advertise 1000BASE-T Half Duplex
                                 
; Master-Slave Status Register bit definitions
PHY_MSST_MSCF       equ BIT_15   ; RO  LH ; 1=MASTER/SLAVE configuration fault detected
                                 ; SC     ; 0=No MASTER/SLAVE configuration fault detected
PHY_MSST_MSCR       equ BIT_14   ; RO     ; 1=Local PHY configuration resolved to MASTER
                                 ;        ; 0=Local PHY configuration resolved to SLAVE
PHY_MSST_LRS        equ BIT_13   ; RO     ; 1=Local Receiver OK
                                 ;        ; 0=Local Receiver not OK
PHY_MSST_RRS        equ BIT_12   ; RO     ; 1=Remote Receiver OK
                                 ;        ; 0=Remote Receiver not OK
PHY_MSST_LP1000TFD  equ BIT_11   ; RO     ; 1=Link Partner is capable of 1000BASE-T full duplex
                                 ;        ; 0=Link Partner is not capable of 1000BASE-T full duplex
PHY_MSST_LP1000THD  equ BIT_10   ; RO     ; 1=Link Partner is capable of 1000BASE-T half duplex
                                 ;        ; 0=Link Partner is not capable of 1000BASE-T half duplex
                                 
; Extended Status Register bit definitions
PHY_EST_1000XFD     equ BIT_15   ; RO     ; 1=PHY able to perfom full duplex 1000BASE-X
                                 ;        ; 0=PHY not able to perfom full duplex 1000BASE-X
PHY_EST_1000XHD     equ BIT_14   ; RO     ; 1=PHY able to perfom half duplex 1000BASE-X
                                 ;        ; 0=PHY not able to perfom full duplex 1000BASE-X
PHY_EST_1000TFD     equ BIT_13   ; RO     ; 1=PHY able to perfom full duplex 1000BASE-T
                                 ;        ; 0=PHY not able to perfom full duplex 1000BASE-T
PHY_EST_1000THD     equ BIT_12   ; RO     ; 1=PHY able to perfom half duplex 1000BASE-T
                                 ;        ; 0=PHY not able to perfom half duplex 1000BASE-T

; 82541/82547 PHY Specific Register Set

; Port Configuration Register bit definitions
PHY_PC_LD           equ BIT_14   ; RW     ; 1=Force Link Pass
                                 ;        ; 0=Normal operation
PHY_PC_TD           equ BIT_13   ; RW     ; 1=Disable twisted-pair transmitter
                                 ;        ; 0=Normal Operation
PHY_PC_BS           equ BIT_12   ; RW     ; 1=Bypass Scrambler and Descrambler
                                 ;        ; 0=Normal Operation
PHY_PC_B4B5B        equ BIT_11   ; RW     ; 1=Bypass 4B5B encoder and decoder
                                 ;        ; 0=Normal Operation
PHY_PC_JB           equ BIT_10   ; RW     ; 1=Disable Jabber
                                 ;        ; 0=Normal operation
PHY_PC_TPL          equ BIT_8    ; RW     ; 1=Disable TP loopback during half-duplex operation
                                 ;        ; 0=Normal operation
PHY_PC_SS           equ BIT_7    ; RW     ; 1=Smart Speed Selection enabled
                                 ;        ; 0-Smart Speed Selection disabled
PHY_PC_PE           equ BIT_5    ; RW     ; 1=Preamble Enable
                                 ;        ; 0=Preamble Disnable
PHY_PC_PDB          equ BIT_4    ; RW     ; 1=Strict 802.3 Auto-MDIX algorithm.
                                 ;        ; 0=Auto-MDIX algorithm
PHY_PC_ANP          equ BIT_1    ; RW     ; 1=Enable alternate of auto negotiate next page feature
                                 ;        ; 0=Disable alternate of auto negotiate next page feature

; Port Status Register bit definitions
PHY_PS_DR_MASK      equ BIT_15 + BIT_14   ; Bits mask for PHY Data Rate
PHY_PS_TS           equ BIT_13   ; RO     ; 1=The PHY is currently Transmitting a packet
                                 ;        ; 0=The PHY transmitter is IDLE
PHY_PS_RS           equ BIT_12   ; RO     ; 1=The PHY is currently Receiving a packet
                                 ;        ; 0=The PHY receiver is IDLE
PHY_PS_MDIX         equ BIT_11   ; RO     ; 1=PHY has selected MDIX
                                 ;        ; 0=PHY has selected MDI
PHY_PS_LINK         equ BIT_10   ; RO     ; 1=Link is currently up
                                 ;        ; 0=Link is currently down
PHY_PS_DX           equ BIT_9    ; RO     ; 1=Full Duplex
                                 ;        ; 0=Half Duplex
PHY_PS_LL_MASK      equ 1111100b          ; Line Length Estimation
PHY_PS_PS           equ BIT_1    ; RO     ; 1=10BaseT polarity is reversed
                                 ;        ; 0=10BaseT polarity is normal
PHY_PS_LFITI        equ BIT_0    ; RO  LH ; 1=Autonegotiation has aborted Link establishment following normal page exchange
                                 ; SC     ; Autonegotiation has either completed normally, or is still in progress
PHY_PS_DR10         equ 1                 ; The PHY is operating in 10BASE-T MII Mode
PHY_PS_DR100        equ 2                 ; The PHY is operating in 100BASE-TX Mode
PHY_PS_DR1000       equ 3                 ; The PHY is operating in 1000BASE-T Mode

; Port Control Register bit definitions
PHY_PCTRL_MDIXM     equ BIT_13   ; RW     ; Force MDIX mode 1=MDIX 0=MDI
PHY_PCTRL_AMDIX     equ BIT_12   ; RW     ; 1=Enable Auto-MDIX mode
                                 ;        ; 0=Disable Auto-MDIX mode (Manual mode)
PHY_PCTRL_FC        equ BIT_11   ; RW     ; This forces all MDI/MDIX and polarity issues to be correct
PHY_PCTRL_TEN_CSR   equ BIT_10   ; RW     ; 1=Extend CRS to cover GMII latency and RX_DV
                                 ;        ; 0=Do NOT extend CRS (RX_DV may continue past CRS)
PHY_PCTRL_NCSC      equ BIT_9    ; RW     ; 1=Detect and correct for non-compliant scrambler
                                 ;        ; 0=Detect and report non-compliant scrambler
PHY_PCTRL_TE        equ BIT_5    ; RW     ; 1=Allow test modes to operate as programmed in indirect registers
                                 ;        ; 0=Not allow test modes to operate as programmed in indirect registers
PHY_PCTRL_TPL       equ BIT_4    ; RW     ; 1=Allow Gig Loopback on twisted pairs
                                 ;        ; 0=Not allow Gig Loopback on twisted pairs

; Link Health Register bit definitions
PHY_LH_SSDOWN       equ BIT_15   ; RO  LH ; Smart Speed has downgraded the link speed from the maximum advertised
PHY_LH_GSE          equ BIT_14   ; RO     ; Indicates Gig scrambler error
PHY_LH_GMF          equ BIT_13   ; RO     ; This is a copy of bit PHY.REG.10.15
PHY_LH_GMR          equ BIT_12   ; RO     ; This is a duplicate of bit PHY.REG.10.14
PHY_LH_GRRE         equ BIT_11   ; RO  LH ; This is a latched high version of PHY.REG.10.12
PHY_LH_CO           equ BIT_10   ; RO  LH ; The idle error counter has overflowed
PHY_LH_ANF          equ BIT_6    ; RO     ; This is the logical OR of (PHY.REG 1.4, PHY.REG 6.4, PHY.REG 10.15)



; 82540/4/5  PHY Specific Register Set

; PHY Specific Control Register bit defiitions
PHY_SCTRL_ACRS      equ BIT_11   ; RW     ; 1=Assert CRS on transmit
                                 ;        ; 0=Never assert CRS on transmit
PHY_SCTRL_FLG       equ BIT_10   ; RW     ; 1=Force Link good
                                 ;        ; 0=Normal operation
PHY_SCTRL_ED_MASK   equ BIT_9 + BIT_8     ; Energy Detect Mask
PHY_SCTRL_EED       equ BIT_7    ; RW     ; 1=Lower 10BASE-T receive threshold
                                 ;        ; 0=Normal 10BASE-T receive threshold
PHY_SCTRL_MDIM_MASK equ BIT_6 + BIT_5     ; MDI Cross over Mode Mask
PHY_SCTRL_SQE       equ BIT_2    ; RW     ; 1=SQE test enabled
                                 ;        ; 0=SQE test disabled
PHY_SCTRL_PR        equ BIT_1    ; RW     ; 1=Polarity Reversal Disabled
                                 ;        ; 0=Polarity Reversal Enabled
PHY_SCTRL_DJB       equ BIT_0    ; RW     ; 1=Disable jabber function
                                 ;        ; 0=Enable jabber function


; PHY Specific Status Register bit definitions
PHY_SST_SP_MASK     equ BIT_15 + BIT_14   ; PHY Speed Mask
PHY_SST_DX          equ BIT_13   ; RO     ; 1=Full duplex
                                 ;        ; 0=Half duplex
PHY_SST_PR          equ BIT_12   ; RO  LH ; 1=Page Received
                                 ;        ; 0=Page not received
PHY_SST_SPDXR       equ BIT_11   ; RO     ; 1=Speed and Duplex Resolved
                                 ;        ; 0=Speed and Duplex Not Resolved
PHY_SST_LINK        equ BIT_10   ; RO     ; Indicates Link Real State
PHY_SST_CL_MASK     equ BIT_9+BIT_8+BIT_7 ; Cable  Length Mask (100/1000 Modes Only)
PHY_SST_MDIXST      equ BIT_6    ; RO     ; MDIX Crossover Status 1=MDIX 0=MDI
PHY_SST_DOWNST      equ BIT_5    ; RO     ; Downshift Status 1=Downshift 0=No downshift
PHY_SST_EDST        equ BIT_4    ; RO     ; Energy Detect Status 1=Sleep 0=Active
PHY_SST_TPE         equ BIT_3    ; RO     ; 1=Transmit Pause enabled
                                 ;        ; 0=Transmit Pause disabled
PHY_SST_RPE         equ BIT_2    ; RO     ; 1=Receive Pause enabled
                                 ;        ; 0=Receive Pause disabled
PHY_SST_POL         equ BIT_1    ; RO     ; Polarity (Real Time) 1=Reversed 0=Normal
PHY_SST_JB          equ BIT_0    ; RO     ; Jabber (Real Time) 1=Jabber 0=No Jabber


; Interrupt Enable/Status Register bit definitions
; Interrupt Enable  register controls the conditions which may generate a PHY interrupt,
; reflected in the MAC CTRL_EXT.PHY_INT bit, and the MAC Interrupt Cause (ICR) register.
; Set any bit to 1 enable corresponding interrupt
; Interrupt Status register reflect the cause(s) of a PHY interrupt that may be 
; visible in the MAC CTRL_EXT.PHY_INT status bit
PHY_IE_ANE          equ BIT_15   ; Auto-Negotiation Error Interrupt
PHY_IE_SP_CH        equ BIT_14   ; Speed Changed Interrupt
PHY_IE_DX_CH        equ BIT_13   ; Duplex Changed Interrupt
PHY_IE_PR           equ BIT_12   ; Page Received Interrupt
PHY_IE_ANC          equ BIT_11   ; Auto-Negotiation Completed Interrupt
PHY_IE_LINK_CH      equ BIT_10   ; Link Status Changed Interrupt
PHY_IE_SYM_ERR      equ BIT_9    ; Symbol Error Interrupt
PHY_IE_FC           equ BIT_8    ; False Carrier Interrupt 
PHY_IE_FIFO_OU      equ BIT_7    ; FIFO Over/Underflow Interrupt
PHY_IE_MDI_CH       equ BIT_6    ; MDI Crossover ChangedInterruptt
PHY_IE_DOWN         equ BIT_5    ; Downshift Interrupt
PHY_IE_ED           equ BIT_4    ; Energy DetectInterrup
PHY_IE_POL_CH       equ BIT_1    ; Polarity Changed Interrupt
PHY_IE_JB           equ BIT_0    ; Jabber Interrupt


; Extended PHY Specific Control Register bit definitions
PHY_ESCTRL_MDC_MASK equ BIT_11 + BIT_10   ; Master Downshift Counter Mask
PHY_ESCTRL_SDC_MASK equ BIT_9  + BIT_8    ; Slave Downshift Counter Mask



comment |
; PHY specific register
PHY_SPSTATUS   equ  17   ; PHY Specific Status register


; Speed mask 
PHY_SPST_SPD_MASK  equ BIT_15 + BIT_14 
PHY_SPST_CL_MASK   equ BIT_9+BIT_9+BIT_7

PHY_SPST_FD    equ BIT_13  ; RO     ; 1=Full Duplex
                           ;        ; 0=Half Duplex
PHY_SPST_PR    equ BIT_12  ; RO  LH ; 1=Page received
                           ;        ; 0=Page not received
PHY_SPST_SDR   equ BIT_11  ; RO     ; 1=Speed and Duplex Resolved
                           ;        ; 0=Speed and Duplex Not Resolved
PHY_SPST_LINK  equ BIT_10  ; RO     ; 1=Link (real time) Up
                           ;        ; 0=Link (real time) Down
PHY_SPST_MDICS equ BIT_6   ; RO     ; 1=MDIX
                           ;        ; 0=MDI
PHY_SPST_DS    equ BIT_5   ; RO     ; 1=Downshift
                           ;        ; 0=No Downshift
PHY_SPST_EDS   equ BIT_4   ; RO     ; 1=Energy Detect Sleep
                           ;        ; 0=Energy Detect Active
PHY_SPST_TPE   equ BIT_3   ; RO     ; 1=Transmit Pause Enabled
                           ;        ; 0=Transmit Pause Disabled
PHY_SPST_RPE   equ BIT_2   ; RO     ; 1=Receive Pause Enabled
                           ;        ; 0=Receive Pause Disabled
PHY_SPST_POL   equ BIT_1   ; RO     ; 0=Polarity (real) normal
PHY_SPST_JABB  equ BIT_0   ; RO     ; 1=Jabber
                           ;        ; 0=No jabber
|                           
