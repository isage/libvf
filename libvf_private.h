/*
        libvf
        Copyright (C) 2022 Cat (Ivan Epifanov)

        This program is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, either version 3 of the License, or
        (at your option) any later version.

        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.

        You should have received a copy of the GNU General Public License
        along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#ifndef __LIBVF_PRIVATE_H__
#define __LIBVF_PRIVATE_H__

#include "libvf.h"

#include <psp2/types.h>
#include <stdint.h>

#define FTDI_DEVICE_OUT_REQTYPE ((0x02 << 5) | 0x00)
#define FTDI_DEVICE_IN_REQTYPE ((0x02 << 5) | 0x80)

#define SIO_RESET 0         /* Reset the port */
#define SIO_MODEM_CTRL 1    /* Set the modem control register */
#define SIO_SET_FLOW_CTRL 2 /* Set flow control register */
#define SIO_SET_BAUD_RATE 3 /* Set baud rate */
#define SIO_SET_DATA 4      /* Set the data characteristics of the port */

/* Requests */
#define SIO_RESET_REQUEST SIO_RESET
#define SIO_SET_BAUDRATE_REQUEST SIO_SET_BAUD_RATE
#define SIO_SET_DATA_REQUEST SIO_SET_DATA
#define SIO_SET_FLOW_CTRL_REQUEST SIO_SET_FLOW_CTRL
#define SIO_SET_MODEM_CTRL_REQUEST SIO_MODEM_CTRL
#define SIO_POLL_MODEM_STATUS_REQUEST 0x05
#define SIO_SET_EVENT_CHAR_REQUEST 0x06
#define SIO_SET_ERROR_CHAR_REQUEST 0x07
#define SIO_SET_LATENCY_TIMER_REQUEST 0x09
#define SIO_GET_LATENCY_TIMER_REQUEST 0x0A
#define SIO_SET_BITMODE_REQUEST 0x0B
#define SIO_READ_PINS_REQUEST 0x0C
#define SIO_READ_EEPROM_REQUEST 0x90
#define SIO_WRITE_EEPROM_REQUEST 0x91
#define SIO_ERASE_EEPROM_REQUEST 0x92

#define SIO_RESET_SIO 0

#define SIO_TCIFLUSH 2
#define SIO_TCOFLUSH 1

#define SIO_DISABLE_FLOW_CTRL 0x0
#define SIO_RTS_CTS_HS (0x1 << 8)
#define SIO_DTR_DSR_HS (0x2 << 8)
#define SIO_XON_XOFF_HS (0x4 << 8)

#define SIO_SET_DTR_MASK 0x1
#define SIO_SET_DTR_HIGH (1 | (SIO_SET_DTR_MASK << 8))
#define SIO_SET_DTR_LOW (0 | (SIO_SET_DTR_MASK << 8))
#define SIO_SET_RTS_MASK 0x2
#define SIO_SET_RTS_HIGH (2 | (SIO_SET_RTS_MASK << 8))
#define SIO_SET_RTS_LOW (0 | (SIO_SET_RTS_MASK << 8))

#define SIO_RTS_CTS_HS (0x1 << 8)

/** FTDI chip type */
enum chip_type
{
  TYPE_AM    = 0,
  TYPE_BM    = 1,
  TYPE_2232C = 2,
  TYPE_R     = 3,
  TYPE_2232H = 4,
  TYPE_4232H = 5,
  TYPE_232H  = 6,
  TYPE_230X  = 7,
};

struct ftdi_context
{
  /* USB specific */
  SceUID device_id;

  /* Endpoints */
  SceUID out_pipe_id;
  SceUID in_pipe_id;
  SceUID control_pipe_id;

  /* FTDI specific */
  /** FTDI chip type */
  enum chip_type type;
  /** baudrate */
  int baudrate;
  /** pointer to read buffer for ftdi_read_data */
  unsigned char readbuffer[4096] __attribute__((aligned(64)));
  /** read buffer offset */
  unsigned int readbuffer_offset;
  /** number of remaining data in internal read buffer */
  unsigned int readbuffer_remaining;
  /** read buffer chunk size */
  unsigned int readbuffer_chunksize;

  unsigned char writebuffer[4096] __attribute__((aligned(64)));
  /** write buffer chunk size */
  unsigned int writebuffer_chunksize;
  /** maximum packet size. Needed for filtering modem status bytes every n packets. */
  unsigned int max_packet_size;

};

#endif // __LIBVF_PRIVATE_H__
