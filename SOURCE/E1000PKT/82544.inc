;

; Registers set for 8254x adapters
 
; Registers are defined to be 32 bits and  should be accessed as 32 bit values.
; These registers are physically located on the NIC, but are mapped into the 
; host memory address space.
 
; RW - register is both readable and writable
; RO - register is read only
; WO - register is write only
; R/clr - register is read only and is cleared when read
; A - register array
 
 
E1000_CTRL     equ  00000h  ; Device Control - RW 
E1000_STATUS   equ  00008h  ; Device Status - RO 
E1000_EECD     equ  00010h  ; EEPROM/Flash Control - RW 
E1000_EERD     equ  00014h  ; EEPROM Read - RW 
E1000_CTRL_EXT equ  00018h  ; Extended Device Control - RW
E1000_FLA      equ  0001Ch  ; Flash Access Register - RW
E1000_MDIC     equ  00020h  ; MDI Control - RW 
E1000_FCAL     equ  00028h  ; Flow Control Address Low - RW 
E1000_FCAH     equ  0002Ch  ; Flow Control Address High - RW 
E1000_FCT      equ  00030h  ; Flow Control Type - RW 
E1000_VET      equ  00038h  ; VLAN Ether Type - RW 
E1000_ICR      equ  000C0h  ; Interrupt Cause Read - R/clr 
E1000_ITR      equ  000C4h  ; Interrupt Throttling Rate - RW 
E1000_ICS      equ  000C8h  ; Interrupt Cause Set - WO 
E1000_IMS      equ  000D0h  ; Interrupt Mask Set - RW 
E1000_IMC      equ  000D8h  ; Interrupt Mask Clear - WO 
E1000_RCTL     equ  00100h  ; RX Control - RW 
E1000_FCTTV    equ  00170h  ; Flow Control Transmit Timer Value - RW 
E1000_TXCW     equ  00178h  ; TX Configuration Word - RW 
E1000_RXCW     equ  00180h  ; RX Configuration Word - RO 
E1000_TCTL     equ  00400h  ; TX Control - RW 
E1000_TIPG     equ  00410h  ; TX Inter-packet gap -RW 
E1000_TBT      equ  00448h  ; TX Burst Timer - RW 
E1000_AIT      equ  00458h  ; Adaptive Interframe Spacing Throttle - RW 
E1000_LEDCTL   equ  00E00h  ; LED Control - RW 
E1000_PBA      equ  01000h  ; Packet Buffer Allocation - RW 
E1000_FCRTL    equ  02160h  ; Flow Control Receive Threshold Low - RW 
E1000_FCRTH    equ  02168h  ; Flow Control Receive Threshold High - RW 
E1000_RDBAL    equ  02800h  ; RX Descriptor Base Address Low - RW 
E1000_RDBAH    equ  02804h  ; RX Descriptor Base Address High - RW 
E1000_RDLEN    equ  02808h  ; RX Descriptor Length - RW 
E1000_RDH      equ  02810h  ; RX Descriptor Head - RW 
E1000_RDT      equ  02818h  ; RX Descriptor Tail - RW 
E1000_RDTR     equ  02820h  ; RX Delay Timer - RW 
E1000_RXDCTL   equ  02828h  ; RX Descriptor Control - RW 
E1000_RADV     equ  0282Ch  ; RX Interrupt Absolute Delay Timer - RW 
E1000_RSRPD    equ  02C00h  ; RX Small Packet Detect - RW 
E1000_TXDMAC   equ  03000h  ; TX DMA Control - RW 
E1000_TDBAL    equ  03800h  ; TX Descriptor Base Address Low - RW 
E1000_TDBAH    equ  03804h  ; TX Descriptor Base Address High - RW 
E1000_TDLEN    equ  03808h  ; TX Descriptor Length - RW 
E1000_TDH      equ  03810h  ; TX Descriptor Head - RW 
E1000_TDT      equ  03818h  ; TX Descripotr Tail - RW 
E1000_TIDV     equ  03820h  ; TX Interrupt Delay Value - RW 
E1000_TXDCTL   equ  03828h  ; TX Descriptor Control - RW 
E1000_TADV     equ  0382Ch  ; TX Interrupt Absolute Delay Val - RW 
E1000_TSPMT    equ  03830h  ; TCP Segmentation PAD & Min Threshold - RW 
E1000_CRCERRS  equ  04000h  ; CRC Error Count - R/clr 
E1000_ALGNERRC equ  04004h  ; Alignment Error Count - R/clr 
E1000_SYMERRS  equ  04008h  ; Symbol Error Count - R/clr 
E1000_RXERRC   equ  0400Ch  ; Receive Error Count - R/clr 
E1000_MPC      equ  04010h  ; Missed Packet Count - R/clr 
E1000_SCC      equ  04014h  ; Single Collision Count - R/clr 
E1000_ECOL     equ  04018h  ; Excessive Collision Count - R/clr 
E1000_MCC      equ  0401Ch  ; Multiple Collision Count - R/clr 
E1000_LATECOL  equ  04020h  ; Late Collision Count - R/clr 
E1000_COLC     equ  04028h  ; Collision Count - R/clr 
E1000_DC       equ  04030h  ; Defer Count - R/clr 
E1000_TNCRS    equ  04034h  ; TX-No CRS - R/clr 
E1000_SEC      equ  04038h  ; Sequence Error Count - R/clr 
E1000_CEXTERR  equ  0403Ch  ; Carrier Extension Error Count - R/clr 
E1000_RLEC     equ  04040h  ; Receive Length Error Count - R/clr 
E1000_XONRXC   equ  04048h  ; XON RX Count - R/clr 
E1000_XONTXC   equ  0404Ch  ; XON TX Count - R/clr 
E1000_XOFFRXC  equ  04050h  ; XOFF RX Count - R/clr 
E1000_XOFFTXC  equ  04054h  ; XOFF TX Count - R/clr
E1000_FCRUC    equ  04058h  ; Flow Control RX Unsupported Count- R/clr 
E1000_PRC64    equ  0405Ch  ; Packets RX (64 bytes) - R/clr 
E1000_PRC127   equ  04060h  ; Packets RX (65-127 bytes) - R/clr 
E1000_PRC255   equ  04064h  ; Packets RX (128-255 bytes) - R/clr 
E1000_PRC511   equ  04068h  ; Packets RX (255-511 bytes) - R/clr 
E1000_PRC1023  equ  0406Ch  ; Packets RX (512-1023 bytes) - R/clr 
E1000_PRC1522  equ  04070h  ; Packets RX (1024-1522 bytes) - R/clr 
E1000_GPRC     equ  04074h  ; Good Packets RX Count - R/clr 
E1000_BPRC     equ  04078h  ; Broadcast Packets RX Count - R/clr 
E1000_MPRC     equ  0407Ch  ; Multicast Packets RX Count - R/clr 
E1000_GPTC     equ  04080h  ; Good Packets TX Count - R/clr
E1000_GORCL    equ  04088h  ; Good Octets RX Count Low - R/clr 
E1000_GORCH    equ  0408Ch  ; Good Octets RX Count High - R/clr 
E1000_GOTCL    equ  04090h  ; Good Octets TX Count Low - R/clr
E1000_GOTCH    equ  04094h  ; Good Octets TX Count High - R/clr 
E1000_RNBC     equ  040A0h  ; RX No Buffers Count - R/clr 
E1000_RUC      equ  040A4h  ; RX Undersize Count - R/clr 
E1000_RFC      equ  040A8h  ; RX Fragment Count - R/clr
E1000_ROC      equ  040ACh  ; RX Oversize Count - R/clr 
E1000_RJC      equ  040B0h  ; RX Jabber Count - R/clr 
E1000_MGTPRC   equ  040B4h  ; Management Packets RX Count - R/clr 
E1000_MGTPDC   equ  040B8h  ; Management Packets Dropped Count - R/clr 
E1000_MGTPTC   equ  040BCh  ; Management Packets TX Count - R/clr
E1000_TORL     equ  040C0h  ; Total Octets RX Low - R/clr
E1000_TORH     equ  040C4h  ; Total Octets RX High - R/clr
E1000_TOTL     equ  040C8h  ; Total Octets TX Low - R/clr
E1000_TOTH     equ  040CCh  ; Total Octets TX High - R/clr
E1000_TPR      equ  040D0h  ; Total Packets RX - R/clr
E1000_TPT      equ  040D4h  ; Total Packets TX - R/clr 
E1000_PTC64    equ  040D8h  ; Packets TX (64 bytes) - R/clr
E1000_PTC127   equ  040DCh  ; Packets TX (65-127 bytes) - R/clr 
E1000_PTC255   equ  040E0h  ; Packets TX (128-255 bytes) - R/clr 
E1000_PTC511   equ  040E4h  ; Packets TX (256-511 bytes) - R/clr 
E1000_PTC1023  equ  040E8h  ; Packets TX (512-1023 bytes) - R/clr 
E1000_PTC1522  equ  040ECh  ; Packets TX (1024-1522 Bytes) - R/clr 
E1000_MPTC     equ  040F0h  ; Multicast Packets TX Count - R/clr 
E1000_BPTC     equ  040F4h  ; Broadcast Packets TX Count - R/clr 
E1000_TSCTC    equ  040F8h  ; TCP Segmentation Context TX - R/clr 
E1000_TSCTFC   equ  040FCh  ; TCP Segmentation Context TX Fail - R/clr 
E1000_RXCSUM   equ  05000h  ; RX Checksum Control - RW 
E1000_MTA      equ  05200h  ; Multicast Table Array - RW Array
E1000_RA       equ  05400h  ; Receive Address - RW Array 
E1000_VFTA     equ  05600h  ; VLAN Filter Table Array - RW Array 
E1000_WUC      equ  05800h  ; Wakeup Control - RW 
E1000_WUFC     equ  05808h  ; Wakeup Filter Control - RW 
E1000_WUS      equ  05810h  ; Wakeup Status - RO 
E1000_MANC     equ  05820h  ; Management Control - RW 
E1000_IPAV     equ  05838h  ; IP Address Valid - RW 
E1000_IP4AT    equ  05840h  ; IPv4 Address Table - RW Array 
E1000_IP6AT    equ  05880h  ; IPv6 Address Table - RW Array 
E1000_WUPL     equ  05900h  ; Wakeup Packet Length - RW 
E1000_WUPM     equ  05A00h  ; Wakeup Packet Memory - RO A
E1000_FFLT     equ  05F00h  ; Flexible Filter Length Table - RW Array
E1000_HLCTRL   equ  08700h  ; Hublink Control Register - RW
E1000_FFMT     equ  09000h  ; Flexible Filter Mask Table - RW Array
E1000_FFVT     equ  09800h  ; Flexible Filter Value Table - RW Array


;--------------------------------------------------------------------------
;- Register Bit Masks.
;--------------------------------------------------------------------------

;- Device Control Register bit masks 

E1000_CTRL_FD              equ  00000001h    ;- Full duplex.0=half; 1=full
E1000_CTRL_BEM             equ  00000002h    ;- Endian Mode.0=little,1=big 
E1000_CTRL_PRIOR           equ  00000004h    ;- Priority on PCI. 0=rx,1=fair 
E1000_CTRL_LRST            equ  00000008h    ;- Link reset. 0=normal,1=reset 
E1000_CTRL_TME             equ  00000010h    ;- Test mode. 0=normal,1=test 
E1000_CTRL_SLE             equ  00000020h    ;- Serial Link on Internal SerDes.
                                             ;- 0=disabled,1=enabled WS 
E1000_CTRL_ASDE            equ  00000020h    ;- Auto-speed detect enable
E1000_CTRL_SLU             equ  00000040h    ;- Set link up Force Link 
E1000_CTRL_ILOS            equ  00000080h    ;- Invert Loss-Of_Signal 0=normal 1=inverted 
E1000_CTRL_SPD_MASK        equ  00000300h    ;- Speed Select Mask
E1000_CTRL_SPD_SHIFT       equ  00000008h    ;- Speed shift
E1000_CTRL_SPD_10          equ  00000000h    ;- Force 10Mb
E1000_CTRL_SPD_100         equ  00000100h    ;- Force 100Mb
E1000_CTRL_SPD_1000        equ  00000200h    ;- Force 1Gb
E1000_CTRL_BEM32           equ  00000400h    ;- Big Endian 32 mode LG
E1000_CTRL_FRCSPD          equ  00000800h    ;- Force Speed
E1000_CTRL_FRCDPX          equ  00001000h    ;- Force Duplex
E1000_CTRL_SWDPIN0         equ  00040000h    ;- SWDPIN 0 value 
E1000_CTRL_SWDPIN1         equ  00080000h    ;- SWDPIN 1 value 
E1000_CTRL_SWDPIN2         equ  00100000h    ;- SWDPIN 2 value 
E1000_CTRL_SWDPIN3         equ  00200000h    ;- SWDPIN 3 value 
E1000_CTRL_SWDPIO0         equ  00400000h    ;- SWDPIN 0 Input or output 
E1000_CTRL_SWDPIO1         equ  00800000h    ;- SWDPIN 1 input or output 
E1000_CTRL_SWDPIO2         equ  01000000h    ;- SWDPIN 2 input or output 
E1000_CTRL_SWDPIO3         equ  02000000h    ;- SWDPIN 3 input or output 
E1000_CTRL_RST             equ  04000000h    ;- Global reset 
E1000_CTRL_RFCE            equ  08000000h    ;- Receive Flow Control enable 
E1000_CTRL_TFCE            equ  10000000h    ;- Transmit flow control enable 
E1000_CTRL_RTE             equ  20000000h    ;- Routing tag enable WS 
E1000_CTRL_VME             equ  40000000h    ;- IEEE VLAN mode enable 
E1000_CTRL_PHYRST          equ  80000000h    ;- 0=normal 1=assert PHY reset


; Device status Register bit mask

E1000_STATUS_FD            equ  00000001h    ;- Full duplex.0=half,1=full 
E1000_STATUS_LU            equ  00000002h    ;- Link up.0=no,1=link 
E1000_STATUS_TCKOK         equ  00000004h    ;- Transmit Clock Running 
E1000_STATUS_RBCOK         equ  00000008h    ;- Receive Byte Clock Running
E1000_STATUS_TXOFF         equ  00000010h    ;- transmission paused 
E1000_STATUS_TBIMODE       equ  00000020h    ;- TBI mode LG 
E1000_STATUS_SPEED_10      equ  00000000h    ;- Speed 10Mb/s
E1000_STATUS_SPEED_100     equ  00000040h    ;- Speed 100Mb/s
E1000_STATUS_SPEED_1000    equ  00000080h    ;- Speed 1000Mb/s
E1000_STATUS_ASDV          equ  00000300h    ;- Auto speed detection value
E1000_STATUS_MTXCKOK       equ  00000400h    ;- MTX clock running OK
E1000_STATUS_PCI66         equ  00000800h    ;- In 66Mhz slot
E1000_STATUS_BUS64         equ  00001000h    ;- In 64 bit slot
E1000_STATUS_PCIX          equ  00002000h    ;- 1=PCIX mode 0= PCI mode
E1000_STATUS_PCIXSPEED     equ  0000C000h


; EEPROM/FLASH control Register bit masks
E1000_EESK                 equ  00000001h    ;- EEPROM SK bit 
E1000_EECS                 equ  00000002h    ;- EEPROM CS bit 
E1000_EEDI                 equ  00000004h    ;- EEPROM DI bit 
E1000_EEDO                 equ  00000008h    ;- EEPROM DO bit 
E1000_FLASH_WRITE_DIS      equ  00000010h    ;- FLASH write disable LG 
E1000_FLASH_WRITE_EN       equ  00000020h    ;- FLASH write enable LG
; Additional fields
E1000_EE_REQ               equ  00000040h    ;- Request EEPROM Access
E1000_EE_GNT               equ  00000080h    ;- Grant EEPROM Access
E1000_EE_PRES              equ  00000100h    ;- EEPROM present
E1000_EE_SIZE              equ  00000200h    ;- 1=256w 0=64w EEPROM

; EEPROM Read Register bit masks (use for Tabor/Tanacross only)
E1000_EERD_START           equ  00000001h    ;- writing 1 causes EEPROM to read a word
E1000_EERD_DONE            equ  00000002h    ;- 1=Read complete
E1000_EERD_ADDR_SHIFT      equ  00000002h
E1000_EERD_ADDR_MASK       equ  0000FFFCh    ;- EEPROM address field
E1000_EERD_DATA_SHIFT      equ  00000010h
E1000_EERD_DATA_MASK       equ 0ffff0000h    ;- EEprom data field

; Extended Device Control Register bit masks
E1000_EXCTRL_GPI_EN0       equ  00000001h    ;- GP Int enable 0 LG 
E1000_EXCTRL_GPI_EN1       equ  00000002h    ;- GP Int enable 1 LG 
E1000_EXCTRL_GPI_EN2       equ  00000004h    ;- GP Int enable 2 LG 
E1000_EXCTRL_GPI_EN3       equ  00000008h    ;- GP Int enable 3 LG 
E1000_EXCTRL_SWDPIN4       equ  00000010h    ;- SWDPIN 4 value LG 
E1000_EXCTRL_SWDPIN5       equ  00000020h    ;- SWDPIN 5 value LG 
E1000_EXCTRL_SWDPIN6       equ  00000040h    ;- SWDPIN 6 value LG 
E1000_EXCTRL_SWDPIN7       equ  00000080h    ;- SWDPIN 7 value LG 
E1000_EXCTRL_SWDPIO4       equ  00000100h    ;- SWDPIN 4 Input or output LG 
E1000_EXCTRL_SWDPIO5       equ  00000200h    ;- SWDPIN 5 input or output LG 
E1000_EXCTRL_SWDPIO6       equ  00000400h    ;- SWDPIN 6 input or output LG 
E1000_EXCTRL_SWDPIO7       equ  00000800h    ;- SWDPIN 7 input or output LG 
E1000_EXCTRL_ASDCHK        equ  00001000h    ;- ASD check LG 
E1000_EXCTRL_EE_RST        equ  00002000h    ;- EEPROM Reset LG 
E1000_EXCTRL_IPS           equ  00004000h    ;- Invert power state LG 
E1000_EXCTRL_SPD_BYPS      equ  00008000h    ;- Speed select bypass LG 


; MDI Control Register bit masks

E1000_MDIC_DATA_MASK       equ  0000FFFFh
E1000_MDIC_REG_MASK        equ  001F0000h
E1000_MDIC_REG_SHIFT       equ  16
E1000_MDIC_PHY_MASK        equ  03E00000h
E1000_MDIC_PHY_SHIFT       equ  21
E1000_MDIC_OP_WRITE        equ  0400h
E1000_MDIC_OP_READ         equ  0800h
E1000_MDIC_READY           equ  10000000h
E1000_MDIC_INT_EN          equ  20000000h
E1000_MDIC_ERROR           equ  40000000h

; This work when CTRL.RFCE is set 
; FLow Control constants
E1000_FCAL_VALUE           equ  00C28001h
E1000_FCAH_VALUE           equ  00000100h
E1000_FCT_VALUE            equ  00008808h



; Interrupt Cause Read/Set Registers bit masks
E1000_ICR_TXDW             equ  00000001h    ;- Transmit desc written back
E1000_ICR_TXQE             equ  00000002h    ;- Transmit Queue empty 
E1000_ICR_LSC              equ  00000004h    ;- Link Status Change 
E1000_ICR_RXSEQ            equ  00000008h    ;- rx sequence error 
E1000_ICR_RXDMT0           equ  00000010h    ;- rx desc min. threshold 0
E1000_ICR_RXDMT1           equ  00000020h    ;- rx desc min. threshold 1 WS 
E1000_ICR_RX0              equ  00000040h    ;- rx overrun 
E1000_ICR_RXT0             equ  00000080h    ;- rx timer intr ring 0 
E1000_ICR_RXT1             equ  00000100h    ;- rx timer intr ring 1 WS 
E1000_ICR_PCIE             equ  00000200h    ;- PCI error WS 
E1000_ICR_MDIAC            equ  00000200h    ;- MDIO access complete LG 
E1000_ICR_RXCFG            equ  00000400h    ;- RX /c/ ordered set LG 
E1000_ICR_GPI_EN0          equ  00000800h    ;- GP Int 0 LG 
E1000_ICR_GPI_EN1          equ  00001000h    ;- GP Int 1 LG 
E1000_ICR_GPI_EN2          equ  00002000h    ;- GP Int 2 LG 
E1000_ICR_GPI_EN3          equ  00004000h    ;- GP Int 3 LG
E1000_ICR_SRPD             equ  00008000h    ;- Small Receive Packet Detected

E1000_ICS_TXDW             equ  E1000_ICR_TXDW     ;- Transmit desc written back
E1000_ICS_TXQE             equ  E1000_ICR_TXQE     ;- Transmit Queue empty 
E1000_ICS_LSC              equ  E1000_ICR_LSC      ;- Link Status Change 
E1000_ICS_RXSEQ            equ  E1000_ICR_RXSEQ    ;- rx sequence error 
E1000_ICS_RXDMT0           equ  E1000_ICR_RXDMT0   ;- rx desc min. threshold 0
E1000_ICS_RXDMT1           equ  E1000_ICR_RXDMT1   ;- rx desc min. threshold 1 WS 
E1000_ICS_RXO              equ  E1000_ICR_RX0      ;- rx overrun 
E1000_ICS_RXT0             equ  E1000_ICR_RXT0     ;- rx timer intr ring 0 
E1000_ICS_RXT1             equ  E1000_ICR_RXT1     ;- rx timer intr ring WS 1 
E1000_ICS_PCIE             equ  E1000_ICR_PCIE     ;- PCI error WS 
E1000_ICS_MDIAC            equ  E1000_ICR_MDIAC    ;- MDIO access complete LG 
E1000_ICS_RXCFG            equ  E1000_ICR_RXCFG    ;- RX /c/ ordered set LG 
E1000_ICS_GPI_EN0          equ  E1000_ICR_GPI_EN0  ;- GP Int 0 LG 
E1000_ICS_GPI_EN1          equ  E1000_ICR_GPI_EN1  ;- GP Int 1 LG 
E1000_ICS_GPI_EN2          equ  E1000_ICR_GPI_EN2  ;- GP Int 2 LG 
E1000_ICS_GPI_EN3          equ  E1000_ICR_GPI_EN3  ;- GP Int 3 LG 
E1000_ICS_SRPD             equ  E1000_ICR_SRPD     ;- Small Receive Packet Detected


; Interrupt  MaskSet/MaskRead Registers bit masks

E1000_IMS_TXDW             equ  E1000_ICR_TXDW     ;- Transmit desc written back
E1000_IMS_TXQE             equ  E1000_ICR_TXQE     ;- Transmit Queue empty 
E1000_IMS_LSC              equ  E1000_ICR_LSC      ;- Link Status Change 
E1000_IMS_RXSEQ            equ  E1000_ICR_RXSEQ    ;- rx sequence error 
E1000_IMS_RXDMT0           equ  E1000_ICR_RXDMT0   ;- rx desc min. threshold 0
E1000_IMS_RXDMT1           equ  E1000_ICR_RXDMT1   ;- rx desc min. threshold 1 WS
E1000_IMS_RXO              equ  E1000_ICR_RX0      ;- rx overrun 
E1000_IMS_RXT0             equ  E1000_ICR_RXT0     ;- rx timer intr ring 0 
E1000_IMS_RXT1             equ  E1000_ICR_RXT1     ;- rx timer intr ring 1 WS 
E1000_IMS_PCIE             equ  E1000_ICR_PCIE     ;- PCI error WS 
E1000_IMS_MDIAC            equ  E1000_ICR_MDIAC    ;- MDIO access complete LG 
E1000_IMS_RXCFG            equ  E1000_ICR_RXCFG    ;- RX /c/ ordered set LG 
E1000_IMS_GPI_EN0          equ  E1000_ICR_GPI_EN0  ;- GP Int 0 LG 
E1000_IMS_GPI_EN1          equ  E1000_ICR_GPI_EN1  ;- GP Int 1 LG 
E1000_IMS_GPI_EN2          equ  E1000_ICR_GPI_EN2  ;- GP Int 2 LG 
E1000_IMS_GPI_EN3          equ  E1000_ICR_GPI_EN3  ;- GP Int 3 LG 
E1000_IMS_SRPD             equ  E1000_ICR_SRPD     ;- Small Receive Packet Detected


E1000_IMC_TXDW             equ  E1000_ICR_TXDW     ;- Transmit desc written back
E1000_IMC_TXQE             equ  E1000_ICR_TXQE     ;- Transmit Queue empty 
E1000_IMC_LSC              equ  E1000_ICR_LSC      ;- Link Status Change 
E1000_IMC_RXSEQ            equ  E1000_ICR_RXSEQ    ;- rx sequence error 
E1000_IMC_RXDMT0           equ  E1000_ICR_RXDMT0   ;- rx desc min. threshold 0
E1000_IMC_RXDMT1           equ  E1000_ICR_RXDMT1   ;- rx desc min. threshold 1 WS
E1000_IMC_RXO              equ  E1000_ICR_RX0      ;- rx overrun 
E1000_IMC_RXT0             equ  E1000_ICR_RXT0     ;- rx timer intr ring 0 
E1000_IMC_RXT1             equ  E1000_ICR_RXT1     ;- rx timer intr ring 1 WS 
E1000_IMC_PCIE             equ  E1000_ICR_PCIE     ;- PCI error WS 
E1000_IMC_MDIAC            equ  E1000_ICR_MDIAC    ;- MDIO access complete LG 
E1000_IMC_RXCFG            equ  E1000_ICR_RXCFG    ;- RX /c/ ordered set LG 
E1000_IMC_GPI_EN0          equ  E1000_ICR_GPI_EN0  ;- GP Int 0 LG 
E1000_IMC_GPI_EN1          equ  E1000_ICR_GPI_EN1  ;- GP Int 1 LG 
E1000_IMC_GPI_EN2          equ  E1000_ICR_GPI_EN2  ;- GP Int 2 LG 
E1000_IMC_GPI_EN3          equ  E1000_ICR_GPI_EN3  ;- GP Int 3 LG
E1000_IMC_SRPD             equ  E1000_ICR_SRPD     ;- Small Receive Packet Detected


; Interrup Enable Mask
E1000_INTERRUPT_MASK       equ  E1000_ICR_RXT0

; Receive Control Registr bit masks

E1000_RCTL_EN              equ  00000002h    ;- enable 
E1000_RCTL_SBP             equ  00000004h    ;- store bad packet 
E1000_RCTL_UPE             equ  00000008h    ;- unicast promiscuous enable
E1000_RCTL_MPE             equ  00000010h    ;- multicast promiscuous enab
E1000_RCTL_LPE             equ  00000020h    ;- long packet enable 
E1000_RCTL_LBM_NO          equ  00000000h    ;- no loopback mode 
E1000_RCTL_LBM_MAC         equ  00000040h    ;- MAC loopback mode 
E1000_RCTL_LBM_SLP         equ  00000080h    ;- serial link loopback mode 
E1000_RCTL_LBM_TCVR        equ  000000c0h    ;- tcvr loopback mode 
E1000_RCTL_RDMTS0_HALF     equ  00000000h    ;- rx desc 0 threshold size 
E1000_RCTL_RDMTS0_QUAT     equ  00000100h    ;- rx desc 0 threshold size 
E1000_RCTL_RDMTS0_EIGTH    equ  00000200h    ;- rx desc 0 threshold size 
E1000_RCTL_RDMTS1_HALF     equ  00000000h    ;- rx desc 1 threshold size 
E1000_RCTL_RDMTS1_QUAT     equ  00000400h    ;- rx desc 1 threshold size 
E1000_RCTL_RDMTS1_EIGTH    equ  00000800h    ;- rx desc 1 threshold size 
E1000_RCTL_MO_0            equ  00000000h    ;- multicast offset 47:36   
E1000_RCTL_MO_1            equ  00001000h    ;- multicast offset 46:35   
E1000_RCTL_MO_2            equ  00002000h    ;- multicast offset 45:34   
E1000_RCTL_MO_3            equ  00003000h    ;- multicast offset 43:32   
E1000_RCTL_BAM             equ  00008000h    ;- broadcast enable 
E1000_RCTL_SZ_2048         equ  00000000h    ;- rx buffer size 2048 
E1000_RCTL_SZ_1024         equ  00010000h    ;- rx buffer size 1024 
E1000_RCTL_SZ_512          equ  00020000h    ;- rx buffer size 512 
E1000_RCTL_SZ_256          equ  00030000h    ;- rx buffer size 256 
E1000_RCTL_VFE             equ  00040000h    ;- vlan filter enable 
E1000_RCTL_CFIEN           equ  00080000h    ;- canonical form enable 
E1000_RCTL_CFI             equ  00100000h    ;- canonical form indicator 
E1000_RCTL_ISLE            equ  00200000h    ;- ISL enable 
E1000_RCTL_DPF             equ  00400000h    ;- discard pause frames 
E1000_RCTL_PMCF            equ  00800000h    ;- pass MAC control frames 
E1000_RCTL_SISLH           equ  01000000h    ;- Strip ISL header 
E1000_RCTL_BSEX            equ  02000000h    ;- When set SZ are multiplied by 16
E1000_RCTL_SECRC           equ  04000000h    ;- Strip Ethernet CRC 

; Receive Delay Timer Register bit masks
E1000_RDT0_DELAY           equ  0000ffffh    ;- Delay timer 1=1024us 
E1000_RDT0_FPDB            equ  80000000h    ;- Flush descriptor block 
E1000_RDT1_DELAY           equ  0000ffffh    ;- Delay timer 1=1024us 
E1000_RDT1_FPDB            equ  80000000h    ;- Flush descriptor block 


; Receive Descriptor Length bit masks
E1000_RDLEN0_LEN           equ  0007ff80h    ;- descriptor length 
E1000_RDLEN1_LEN           equ  0007ff80h    ;- descriptor length 


E1000_RDH0_RDH             equ  0000ffffh    ;- receive descriptor head 
E1000_RDH1_RDH             equ  0000ffffh    ;- receive descriptor head 
E1000_RDT0_RDT             equ  0000ffffh    ;- receive descriptor tail 
E1000_RDT1_RDT             equ  0000ffffh    ;- receive descriptor tail 

; Transmit/Receive Configuration Word Register bit masks
E1000_TXCW_FD              equ  00000020h    ;- TXCW full duplex 
E1000_TXCW_HD              equ  00000040h    ;- TXCW half duplex 
E1000_TXCW_PAUSE           equ  00000080h    ;- TXCW sym pause request 
E1000_TXCW_ASM_DIR         equ  00000100h    ;- TXCW astm pause direction 
E1000_TXCW_PAUSE_MASK      equ  00000180h    ;- TXCW pause request mask
E1000_TXCW_RF              equ  00003000h    ;- TXCW remote fault 
E1000_TXCW_NP              equ  00008000h    ;- TXCW next page 
E1000_TXCW_CW              equ  0000ffffh    ;- TxConfigWord mask 
E1000_TXCW_TXC             equ  40000000h    ;- Transmit Config control 
E1000_TXCW_ANE             equ  80000000h    ;- Auto-neg enable 

E1000_RXCW_CW              equ  0000ffffh    ;- RxConfigWord mask 
E1000_RXCW_NC              equ  04000000h    ;- Receive config no carrier 
E1000_RXCW_IV              equ  08000000h    ;- Receive config invalid 
E1000_RXCW_CC              equ  10000000h    ;- Receive config change 
E1000_RXCW_C               equ  20000000h    ;- Receive config 
E1000_RXCW_SYNCH           equ  40000000h    ;- Receive config synch 
E1000_RXCW_ANC             equ  80000000h    ;- Auto-neg complete 


; Transmit Control Register bit masks
E1000_TCTL_EN              equ  00000002h    ;- enable tx 
E1000_TCTL_PSP             equ  00000008h    ;- pad short packets 
E1000_TCTL_CT              equ  00000ff0h    ;- collision threshold 
E1000_TCTL_COLD            equ  003ff000h    ;- collision distance 
E1000_TCTL_SWXOFF          equ  00400000h    ;- SW Xoff transmission 
E1000_TCTL_PBE             equ  00800000h    ;- Packet Burst Enable 
E1000_TCTL_RTLC            equ  01000000h    ;- Re-transmit on late collision 

E1000_TCTL_CT_VALUE        equ  000000f0h    ;- Collision Threshold value
FD_COLLISION_DISTANCE      equ  00040000h    ;- Collision distance for full duplex mode
HD_COLLISION_DISTANCE      equ  00200000h


E1000_TDBAL_TDBAL          equ  0fffffff0h    ;- tx desc base addr low 
E1000_TDBAH_TDBAH          equ  0ffffffffh    ;- tx desc base addr high 

E1000_TDL_LEN              equ  0007ff80h     ;- tx desc length 
E1000_TDH_TDH              equ  0000ffffh     ;- tx desc head 
E1000_TDT_TDT              equ  0000ffffh     ;- tx desc tail

; Wake Up Control Register bit masks
E1000_WUC_APME             equ  00000001h     ;- 1=Advance Power Management Enable
E1000_WUC_PME_En           equ  00000002h     ;- PME_En bit in the Power Management Control / Status Register 
E1000_WUC_PME_Status       equ  00000004h     ;- Set when receives a wakeup event
E1000_WUC_APMPME           equ  00000008h     ;- Asssert PME on AMP Wakeup
E1000_WUC_SPM              equ  80000000h     ;- Enables MAC Smart Power Down

; Management Control Register bit masks
E1000_MANC_SMBUS_EN        equ  00000001h     ;- SMBus Enabled. 1=enable 0=disable - RO
E1000_MANC_ASF_EN          equ  00000002h     ;- ASF Mode. 1=ASF mode 0=82559 compatible mode - RO
E1000_MANC_R_ON_FORCE      equ  00000004h     ;- Reset on Force TCO. - RO
E1000_MANC_RMCP_EN         equ  00000100h     ;- Enable RMCP 026Fh Filtering.
E1000_MANC_0298_EN         equ  00000200h     ;- Enable RMCP 0298h Filtering.
E1000_MANC_ARP_EN          equ  00002000h     ;- Enable ARP Request Filtering.
E1000_MANC_NEIGHBOR_EN     equ  00004000h     ;- Enable Neighbor Discovery Filtering.
E1000_MANC_TCO_RESET       equ  00010000h     ;- TCO Reset occurred.
E1000_MANC_RCV_TCO_EN      equ  00020000h     ;- Receive TCO Packets Enabled. - RO
E1000_MANC_REPORT_STATUS   equ  00040000h     ;- Status Reporting Enabled. - RO
E1000_MANC_SMB_REQ         equ  01000000h     ;- SMBus Request. 1=Request 0=Release
E1000_MANC_SMB_GNT         equ  02000000h     ;- 1=SMB Grant.
E1000_MANC_SMB_CLKIN       equ  04000000h     ;- SMB ClockIn.
E1000_MANC_SMB_DATAIN      equ  08000000h     ;- SMB DataIn.
E1000_MANC_SMB_DATAOUT     equ  10000000h     ;- SMB DataOut.
E1000_MANC_SMB_CLKOUT      equ  20000000h     ;- SMB ClockOut.
E1000_MANC_DRV_LOAD        equ  80000000h     ;- Driver Loaded.

UNICAST_RX_ADDR            equ  15
MULTICAST_TABLE_SIZE       equ  128
VLAN_FILTER_TABLE_SIZE     equ  128

RX_INT_DELAY               equ  0             ;- Receive Interrupt Delay
SMALL_PKT_SIZE             equ  80
LED_CTRL_DAFAULT           equ  07068302h     ;- This is Cordova default value

; Receive Checksum Control bit masks
E1000_RXCSUM_PCSS_MASK     equ  000000ffh     ;- Packet Checksum Start
E1000_RXCSUM_IPOFL         equ  00000100h     ;- IPv4 checksum offload
E1000_RXCSUM_TUOFL         equ  00000200h     ;- TCP / UDP checksum offload

; Hublink Control Register bit masks
E1000_HLCTRL_INTA          equ  00000100h     ;- Enable INTA pin - RW
E1000_HLCTRL_PCI           equ  00000200h     ;- Enable DO_PCI_INT message interrupt - RW


MediaCopper                equ  0000h
MediaFiber                 equ  E1000_STATUS_TBIMODE


SPEED_1000                 equ  3
SPEED_100                  equ  2
SPEED_10                   equ  1

FULL_DUPLEX                equ  2
HALF_DUPLEX                equ  1



; Ethernet Frame Sizes
ETHERNET_ADDRESS_LENGTH            equ     6
ETHERNET_HEADER_SIZE               equ     14
MINIMUM_ETHERNET_PACKET_SIZE       equ     60
MAXIMUM_ETHERNET_PACKET_SIZE       equ     1500 ; just data, not media header

RX_BUFFER_SIZE                     equ     2048
TX_BUFFER_SIZE                     equ     1536

RX_BUFFER_NUMBER                   equ     2
TX_BUFFER_NUMBER                   equ     2


;- Ethernet 14-byte Header
ETH_HEADER_STRUC     struc
    Destination      db    ETHERNET_ADDRESS_LENGTH   dup (0)
    Source           db    ETHERNET_ADDRESS_LENGTH   dup (0)
    TypeLength       dw    ?
ETH_HEADER_STRUC     ends

ETH_RX_BUFFER_STRUC     struc
    RxMacHeader         db   TYPE ETH_HEADER_STRUC   dup (0)
    RxBufferData        db   (RX_BUFFER_SIZE - SIZE ETH_HEADER_STRUC) dup (0)
ETH_RX_BUFFER_STRUC     ends

;- Ethernet Buffer (Including Ethernet Header) for Transmits
ETH_TX_BUFFER_STRUC     struc
    TxMacHeader         db   TYPE ETH_HEADER_STRUC   dup (0)
    TxBufferData        db   (TX_BUFFER_SIZE - SIZE ETH_HEADER_STRUC) dup (0)
ETH_TX_BUFFER_STRUC     ends



;- Structures

;- Receive descriptor format

recv_descr_type struc
   raddr_lo             dd      0
   raddr_hi             dd      0
   r_length             dw      0
   tcp_checksum         dw      0
   rstatus              db      0
   errors               db      0
   special              dw      0
recv_descr_type ends

; Receive Descriptor number must be  devide by 8

RX_DESCRIPTOR_NUMBER    equ     ((RX_BUFFER_NUMBER-1)/8 +1)*8

RxDescTable_type        struc
  recv_descr_type       RX_DESCRIPTOR_NUMBER   dup ({0,0,0,0,0,0,0})
RxDescTable_type        ends

; Receive Descriptor bit definition

RD_STAT_DD         equ      BIT_0            ;- Descriptor Done
RD_STAT_EOP        equ      BIT_1            ;- End of Packet
RD_STAT_IXSM       equ      BIT_2            ;- Ignore Checksum indication
RD_STAT_VP         equ      BIT_3            ;- Packet is 802.1q (matched VET)
RD_STAT_TCPCS      equ      BIT_5            ;- TCP Checksum Calculated on Packet
RD_STAT_IPCS       equ      BIT_6            ;- IP Checksum Calculated on Packet
RD_STAT_PIF        equ      BIT_7            ;- Passed in-exact filter

RD_ERR_CE          equ      BIT_0            ;- CRC Error or Alignment Error
RD_ERR_SE          equ      BIT_1            ;- Symbol Error
RD_ERR_SEQ         equ      BIT_2            ;- Sequence Error
RD_ERR_CXE         equ      BIT_4            ;- Carrier Extension Error
RD_ERR_TCPE        equ      BIT_5            ;- TCP/UDP Checksum Error
RD_ERR_IPE         equ      BIT_6            ;- IP Checksum Error
RD_ERR_RXE         equ      BIT_7            ;- RX Data Error


RX_ERROR_MASK      equ     RD_ERR_CE or RD_ERR_SE or RD_ERR_SEQ or RD_ERR_CXE or RD_ERR_RXE



;- Legacy Transmit descriptor  format
Xmit_Descr_Type struc
   taddr_lo             dd      0
   taddr_hi             dd      0
   t_length             dw      0
   cs_offset            db      0
   command              db      0
   tstatus              db      0
   cs_start             db      0
   special              dw      0
Xmit_Descr_Type ends


; Tranmsit Descriptor bit definitions

XD_CMD_EOP         equ      BIT_0            ;- End of Packet
XD_CMD_IFCS        equ      BIT_1            ;- Insert FCS
XD_CMD_IC          equ      BIT_2            ;- Insert Checksum
XD_CMD_RS          equ      BIT_3            ;- Report Status
XD_CMD_RPS         equ      BIT_4            ;- Report Packet Sent (reserved bit in chip newer that Cordova)
XD_CMD_DEXT        equ      BIT_5            ;- Descriptor Extension
XD_CMD_VLE         equ      BIT_6            ;- VLAN Packet Enable
XD_CMD_IDE         equ      BIT_7            ;- Interrupt Delay Enable 

XD_STAT_DD         equ      BIT_0            ;- Descriptor Done
XD_STAT_EC         equ      BIT_1            ;- Excess Collisions
XD_STAT_LC         equ      BIT_2            ;- Late Collision


; Transmit Descriptor number must be  devide by 8
TX_DESCRIPTOR_NUMBER     equ     ((TX_BUFFER_NUMBER-1)/8+1)*8

TxDescTable_type        struc
   Xmit_Descr_Type      TX_DESCRIPTOR_NUMBER dup ({0,0,0,0,0,0,0,0})
TxDescTable_type        ends
