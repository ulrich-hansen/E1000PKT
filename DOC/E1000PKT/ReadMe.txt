Generic Packet Driver for the Intel(R) PRO/1000 Family of Adapters -- NO SUPPORT
================================================================================

February 1, 2007


Contents
========

- In This Release
- Building and Installation
- Additional Configurations
- No Support
- License


In This Release
===============

This README file offers a brief description of the generic Gigabit packet driver. This driver
is for use on the Intel(R) PRO/1000 Family of Desktop and Server adapters, specifically 
the Intel 82544, 82540, 82545, 82541, and 82547 based Ethernet controllers.


Building and Installation
=========================

A packet driver is a DOS network driver. PCI BIOS 2.1 is required for this driver.
To compile this packet driver, MASM 6.13 or higher should be installed on the system.

Type "nmake" to build packet driver.

To install driver type:

  e1000pkt.com int_no

in the command line, Where int_no is the software interrupt associated with the driver.
The valid range is 0x60 to 0x80. The "0x" prefix specifies a hexadecimal number. 
The interrupt must not conflict with another system component. 

After proper interrupt configuration, the packet driver locates the first supported adapter
on the PCI bus.  The default link setting is for auto-negotiation of speed and duplex.  This 
requires a link partner that is also set to auto-negotiate. If no errors are detected
during the installation process, a "Driver succesfully installed" message should appear.

If you have more than one adapter, you may specify the target adapter following the packet 
interrupt, as follows:

  e1000pkt.com 0x60 1
  e1000pkt.com 0x60 4

The driver also supports forced speed and duplex for 10/100 Mb/s.

For example:

  e1000pkt.com 0x60 1 100      - this set 100Mb/s link for first adapter
  e1000pkt.com 0x61 4 10 full  - this force adapter to 10Mb/s full duplex operation 

The driver may also be forced to 1000Mb/s; in this case the duplex mode is auto-negotiated.


Additional Configurations
=========================

To experiment with driver performance, try changing the number of driver receive and
transmit buffers (RX_BUFFER_NUMBER and TX_BUFFER_NUMBER in i82544.inc file)
and rebuild the driver.


Test Application
================

A test application that can be used to exercise this packet driver can be found at:
 http://www.amsat.org/amsat/instanttrack/wattcp/

Unit testing done on the following Device_ids.

82545EM
84541ER
82541PI
82540EM
82544EI


No Support
==========

This packet driver is offered "AS IS" and without warranty or support of any kind.
Intel makes no representations as to the accuracy, completeness, quality, functionality or
supportability of this packet driver.  Additionally, Intel does not and will not provide
support for this packet driver via telephone, e-mail or in any other form, nor will Intel
provide any future software updates to support new operating systems or to improve
compatibility with third party devices or software products.


License
=======

Intel PRO/1000 packet driver.
Copyright(c) 2006 - 2007 Intel Corporation.

This program is free software; you can redistribute it and/or modify it
under the terms and conditions of the GNU General Public License,
version 2, as published by the Free Software Foundation.

This program is distributed in the hope it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.

The full GNU General Public License is included in this distribution in
the file called "COPYING".


Trademarks
==========

Intel, Itanium, and Pentium are trademarks or registered trademarks of
Intel Corporation or its subsidiaries in the United States and other
countries.

* Other names and brands may be claimed as the property of others.

