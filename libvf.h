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

#ifndef __LIBVF_H__
#define __LIBVF_H__

#include <psp2/types.h>
#include <stdint.h>

/** Parity mode for ftdi_set_line_property() */
enum parity_type
{
  PARITY_NONE  = 0,
  PARITY_ODD   = 1,
  PARITY_EVEN  = 2,
  PARITY_MARK  = 3,
  PARITY_SPACE = 4
};
/** Number of stop bits for ftdi_set_line_property() */
enum stopbits_type
{
  STOP_BIT_1  = 0,
  STOP_BIT_15 = 1,
  STOP_BIT_2  = 2
};
/** Number of bits for ftdi_set_line_property() */
enum bits_type
{
  BITS_7 = 7,
  BITS_8 = 8
};
/** Break type for ftdi_set_line_property2() */
enum break_type
{
  BREAK_OFF = 0,
  BREAK_ON  = 1
};

#ifdef __cplusplus
extern "C"
{
#endif

  int libvf_start();
  int libvf_stop();

  int libvf_has_ftdi();

  int libvf_set_baudrate(int baudrate);
  int libvf_set_line_property(enum bits_type bits, enum stopbits_type sbit, enum parity_type parity,
                              enum break_type break_type);
  int libvf_write_data(const unsigned char *buf, int size);
  int libvf_read_data(unsigned char *buf, int size);

  int libvf_tciflush();
  int libvf_tcoflush();
  int libvf_tcioflush();

  /* flow control */
  int libvf_setflowctrl(int flowctrl);
  int libvf_setflowctrl_xonxoff(unsigned char xon, unsigned char xoff);
  int libvf_setdtr_rts(int dtr, int rts);
  int libvf_setdtr(int state);
  int libvf_setrts(int state);

#ifdef __cplusplus
}
#endif

#endif // __LIBVF_H__
