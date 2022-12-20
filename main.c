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

#include "libvf_private.h"

#include <psp2kern/kernel/cpu.h>
#include <psp2kern/kernel/debug.h>
#include <psp2kern/kernel/modulemgr.h>
#include <psp2kern/kernel/suspend.h>
#include <psp2kern/kernel/sysclib.h>
#include <psp2kern/kernel/sysmem/data_transfers.h>
#include <psp2kern/kernel/threadmgr/event_flags.h>
#include <psp2kern/usbd.h>
#include <psp2kern/usbserv.h>
#include <string.h>

#define USB_ID_VENDOR_FTDI 0x0403

#define USB_ENDPOINT_OUT 0x02
#define USB_ENDPOINT_IN 0x81

#define EVF_SEND 1
#define EVF_RECV 2
#define EVF_CTRL 4

SceUID transfer_ev;

static struct ftdi_context ctx;

static uint8_t started = 0;
static uint8_t plugged = 0;
static int transferred;

int libvf_probe(int device_id);
int libvf_attach(int device_id);
int libvf_detach(int device_id);

static const SceUsbdDriver libvfDriver = {
    .name   = "libvf",
    .probe  = libvf_probe,
    .attach = libvf_attach,
    .detach = libvf_detach,
};

#ifndef NDEBUG
#define _error_return(code, str)                                                                                       \
  do                                                                                                                   \
  {                                                                                                                    \
    ksceDebugPrintf("%s\n", str);                                                                                      \
    EXIT_SYSCALL(state);                                                                                               \
    return code;                                                                                                       \
  } while (0);

#define trace(...) ksceDebugPrintf(__VA_ARGS__)

#else
#define _error_return(code, str)                                                                                       \
  do                                                                                                                   \
  {                                                                                                                    \
    EXIT_SYSCALL(state);                                                                                               \
    return code;                                                                                                       \
  } while (0);

#define trace(...)

#endif

static int _init_ctx()
{
  ctx.in_pipe_id        = 0;
  ctx.out_pipe_id       = 0;
  ctx.control_pipe_id   = 0;

  ctx.type     = TYPE_BM; /* chip type */
  ctx.baudrate = 9600;

  ctx.readbuffer_chunksize  = 4096;
  ctx.readbuffer_offset     = 0;
  ctx.readbuffer_remaining  = 0;
  ctx.writebuffer_chunksize = 4096;
  ctx.max_packet_size       = 64;

  return 0;
}

static int libvf_sysevent_handler(int resume, int eventid, void *args, void *opt)
{
  if (resume && started)
  {
    ksceUsbServMacSelect(2, 0); // re-set host mode
  }
  return 0;
}

static void _callback_control(int32_t result, int32_t count, void *arg)
{
  trace("config cb result: %08x, count: %d\n", result, count);
  ksceKernelSetEventFlag(transfer_ev, EVF_CTRL);
}

static void _callback_send(int32_t result, int32_t count, void *arg)
{
  trace("send cb result: %08x, count: %d\n", result, count);
  if (result == 0)
    *(int *)arg = count;
  ksceKernelSetEventFlag(transfer_ev, EVF_SEND);
}

static void _callback_recv(int32_t result, int32_t count, void *arg)
{
  trace("recv cb result: %08x, count: %d\n", result, count);
  *(int *)arg = count;
  ksceKernelSetEventFlag(transfer_ev, EVF_RECV);
}

int _control_transfer(int rtype, int req, int val, int idx, void *data, int len)
{
  SceUsbdDeviceRequest _dr;
  _dr.bmRequestType = rtype; // (0x02 << 5)
  _dr.bRequest      = req;
  _dr.wValue        = val;
  _dr.wIndex        = idx;
  _dr.wLength       = len;

  int ret = ksceUsbdControlTransfer(ctx.control_pipe_id, &_dr, data, _callback_control, NULL);
  if (ret < 0)
    return ret;
  trace("waiting ef (cfg)\n");
  ksceKernelWaitEventFlag(transfer_ev, EVF_CTRL, SCE_EVENT_WAITCLEAR_PAT | SCE_EVENT_WAITAND, NULL, 0);
  return 0;
}

int _send(unsigned char *request, unsigned int length)
{
  transferred = 0;
  // transfer
  trace("sending 0x%08x\n", request);
  int ret = ksceUsbdBulkTransfer(ctx.out_pipe_id, request, length, _callback_send, &transferred);
  trace("send 0x%08x\n", ret);
  if (ret < 0)
    return ret;
  // wait for eventflag
  trace("waiting ef (send)\n");
  ksceKernelWaitEventFlag(transfer_ev, EVF_SEND, SCE_EVENT_WAITCLEAR_PAT | SCE_EVENT_WAITAND, NULL, 0);
  return transferred;
}

int _recv(unsigned char *result, unsigned int length)
{
  transferred = 0;
  // transfer
  trace("sending (recv) 0x%08x, len 64\n", result);
  int ret = ksceUsbdBulkTransfer(ctx.in_pipe_id, result, length, _callback_recv, &transferred);
  trace("send (recv) 0x%08x\n", ret);
  if (ret < 0)
    return ret;
  // wait for eventflag
  trace("waiting ef (recv)\n");
  ksceKernelWaitEventFlag(transfer_ev, EVF_RECV, SCE_EVENT_WAITCLEAR_PAT | SCE_EVENT_WAITAND, NULL, 0);
  return transferred;
}

int _ftdi_reset()
{
  SceUsbdDeviceRequest _dr;
  _dr.bmRequestType = FTDI_DEVICE_OUT_REQTYPE; // (0x02 << 5)
  _dr.bRequest      = SIO_RESET_REQUEST;
  _dr.wValue        = SIO_RESET_SIO;
  _dr.wIndex        = 0;
  _dr.wLength       = 0;
  ksceUsbdControlTransfer(ctx.control_pipe_id, &_dr, NULL, _callback_control, NULL);
  trace("waiting ef (reset)\n");
  ksceKernelWaitEventFlag(transfer_ev, EVF_CTRL, SCE_EVENT_WAITCLEAR_PAT | SCE_EVENT_WAITAND, NULL, 0);

  // Invalidate data in the readbuffer
  ctx.readbuffer_offset    = 0;
  ctx.readbuffer_remaining = 0;
  return 0;
}

static unsigned int _ftdi_determine_max_packet_size()
{
  unsigned int packet_size;

  // Determine maximum packet size.
  // New hi-speed devices from FTDI use a packet size of 512 bytes
  if (ctx.type == TYPE_2232H || ctx.type == TYPE_4232H || ctx.type == TYPE_232H)
    packet_size = 512;
  else
    packet_size = 64;

  return packet_size;
}

/*  ftdi_to_clkbits_AM For the AM device, convert a requested baudrate
                    to encoded divisor and the achievable baudrate
    Function is only used internally
    \internal

    See AN120
   clk/1   -> 0
   clk/1.5 -> 1
   clk/2   -> 2
   From /2, 0.125/ 0.25 and 0.5 steps may be taken
   The fractional part has frac_code encoding
*/
static int _ftdi_to_clkbits_AM(int baudrate, unsigned long *encoded_divisor)

{
  static const char frac_code[8]    = {0, 3, 2, 4, 1, 5, 6, 7};
  static const char am_adjust_up[8] = {0, 0, 0, 1, 0, 3, 2, 1};
  static const char am_adjust_dn[8] = {0, 0, 0, 1, 0, 1, 2, 3};
  int divisor, best_divisor, best_baud, best_baud_diff;
  int i;
  divisor = 24000000 / baudrate;

  // Round down to supported fraction (AM only)
  divisor -= am_adjust_dn[divisor & 7];

  // Try this divisor and the one above it (because division rounds down)
  best_divisor   = 0;
  best_baud      = 0;
  best_baud_diff = 0;
  for (i = 0; i < 2; i++)
  {
    int try_divisor = divisor + i;
    int baud_estimate;
    int baud_diff;

    // Round up to supported divisor value
    if (try_divisor <= 8)
    {
      // Round up to minimum supported divisor
      try_divisor = 8;
    }
    else if (divisor < 16)
    {
      // AM doesn't support divisors 9 through 15 inclusive
      try_divisor = 16;
    }
    else
    {
      // Round up to supported fraction (AM only)
      try_divisor += am_adjust_up[try_divisor & 7];
      if (try_divisor > 0x1FFF8)
      {
        // Round down to maximum supported divisor value (for AM)
        try_divisor = 0x1FFF8;
      }
    }
    // Get estimated baud rate (to nearest integer)
    baud_estimate = (24000000 + (try_divisor / 2)) / try_divisor;
    // Get absolute difference from requested baud rate
    if (baud_estimate < baudrate)
    {
      baud_diff = baudrate - baud_estimate;
    }
    else
    {
      baud_diff = baud_estimate - baudrate;
    }
    if (i == 0 || baud_diff < best_baud_diff)
    {
      // Closest to requested baud rate so far
      best_divisor   = try_divisor;
      best_baud      = baud_estimate;
      best_baud_diff = baud_diff;
      if (baud_diff == 0)
      {
        // Spot on! No point trying
        break;
      }
    }
  }
  // Encode the best divisor value
  *encoded_divisor = (best_divisor >> 3) | (frac_code[best_divisor & 7] << 14);
  // Deal with special cases for encoded value
  if (*encoded_divisor == 1)
  {
    *encoded_divisor = 0; // 3000000 baud
  }
  else if (*encoded_divisor == 0x4001)
  {
    *encoded_divisor = 1; // 2000000 baud (BM only)
  }
  return best_baud;
}

/*  ftdi_to_clkbits Convert a requested baudrate for a given system clock  and predivisor
                    to encoded divisor and the achievable baudrate
    Function is only used internally
    \internal

    See AN120
   clk/1   -> 0
   clk/1.5 -> 1
   clk/2   -> 2
   From /2, 0.125 steps may be taken.
   The fractional part has frac_code encoding

   value[13:0] of value is the divisor
   index[9] mean 12 MHz Base(120 MHz/10) rate versus 3 MHz (48 MHz/16) else

   H Type have all features above with
   {index[8],value[15:14]} is the encoded subdivisor

   FT232R, FT2232 and FT232BM have no option for 12 MHz and with
   {index[0],value[15:14]} is the encoded subdivisor

   AM Type chips have only four fractional subdivisors at value[15:14]
   for subdivisors 0, 0.5, 0.25, 0.125
*/
static int _ftdi_to_clkbits(int baudrate, int clk, int clk_div, unsigned long *encoded_divisor)
{
  static const char frac_code[8] = {0, 3, 2, 4, 1, 5, 6, 7};
  int best_baud                  = 0;
  int divisor, best_divisor;
  if (baudrate >= clk / clk_div)
  {
    *encoded_divisor = 0;
    best_baud        = clk / clk_div;
  }
  else if (baudrate >= clk / (clk_div + clk_div / 2))
  {
    *encoded_divisor = 1;
    best_baud        = clk / (clk_div + clk_div / 2);
  }
  else if (baudrate >= clk / (2 * clk_div))
  {
    *encoded_divisor = 2;
    best_baud        = clk / (2 * clk_div);
  }
  else
  {
    /* We divide by 16 to have 3 fractional bits and one bit for rounding */
    divisor = clk * 16 / clk_div / baudrate;
    if (divisor & 1) /* Decide if to round up or down*/
      best_divisor = divisor / 2 + 1;
    else
      best_divisor = divisor / 2;
    if (best_divisor > 0x20000)
      best_divisor = 0x1ffff;
    best_baud = clk * 16 / clk_div / best_divisor;
    if (best_baud & 1) /* Decide if to round up or down*/
      best_baud = best_baud / 2 + 1;
    else
      best_baud = best_baud / 2;
    *encoded_divisor = (best_divisor >> 3) | (frac_code[best_divisor & 0x7] << 14);
  }
  return best_baud;
}
/**
    ftdi_convert_baudrate returns nearest supported baud rate to that requested.
    Function is only used internally
    \internal
*/
static int _ftdi_convert_baudrate(int baudrate, unsigned short *value, unsigned short *index)
{
  int best_baud;
  unsigned long encoded_divisor;

  if (baudrate <= 0)
  {
    // Return error
    return -1;
  }

#define H_CLK 120000000
#define C_CLK 48000000
  if ((ctx.type == TYPE_2232H) || (ctx.type == TYPE_4232H) || (ctx.type == TYPE_232H))
  {
    if (baudrate * 10 > H_CLK / 0x3fff)
    {
      /* On H Devices, use 12 000 000 Baudrate when possible
         We have a 14 bit divisor, a 1 bit divisor switch (10 or 16)
         three fractional bits and a 120 MHz clock
         Assume AN_120 "Sub-integer divisors between 0 and 2 are not allowed" holds for
         DIV/10 CLK too, so /1, /1.5 and /2 can be handled the same*/
      best_baud = _ftdi_to_clkbits(baudrate, H_CLK, 10, &encoded_divisor);
      encoded_divisor |= 0x20000; /* switch on CLK/10*/
    }
    else
      best_baud = _ftdi_to_clkbits(baudrate, C_CLK, 16, &encoded_divisor);
  }
  else if ((ctx.type == TYPE_BM) || (ctx.type == TYPE_2232C) || (ctx.type == TYPE_R) || (ctx.type == TYPE_230X))
  {
    best_baud = _ftdi_to_clkbits(baudrate, C_CLK, 16, &encoded_divisor);
  }
  else
  {
    best_baud = _ftdi_to_clkbits_AM(baudrate, &encoded_divisor);
  }
  // Split into "value" and "index" values
  *value = (unsigned short)(encoded_divisor & 0xFFFF);
  if (ctx.type == TYPE_2232H || ctx.type == TYPE_4232H || ctx.type == TYPE_232H)
  {
    *index = (unsigned short)(encoded_divisor >> 8);
    *index &= 0xFF00;
    *index |= 0;
  }
  else
    *index = (unsigned short)(encoded_divisor >> 16);

  // Return the nearest baud rate
  return best_baud;
}

int _set_baudrate(int baudrate)
{
  unsigned short value, index;
  int actual_baudrate;

  trace("setting baudrate %d\n", baudrate);

  actual_baudrate = _ftdi_convert_baudrate(baudrate, &value, &index);

  if (actual_baudrate <= 0)
  {
    return -1;
  }

  // Check within tolerance (about 5%)
  if ((actual_baudrate * 2 < baudrate /* Catch overflows */)
      || ((actual_baudrate < baudrate) ? (actual_baudrate * 21 < baudrate * 20)
                                       : (baudrate * 21 < actual_baudrate * 20)))
  {
    return -1;
  }

  if (_control_transfer(FTDI_DEVICE_OUT_REQTYPE, SIO_SET_BAUDRATE_REQUEST, value, index, NULL, 0) < 0)
  {
    return -2;
  }

  ctx.baudrate = baudrate;
  return 0;
}

/*
 *  Driver
 */

int libvf_probe(int device_id)
{
  SceUsbdDeviceDescriptor *device;
  trace("probing device: %x\n", device_id);
  device = (SceUsbdDeviceDescriptor *)ksceUsbdScanStaticDescriptor(device_id, 0, SCE_USBD_DESCRIPTOR_DEVICE);
  if (device)
  {
    trace("vendor: %04x\n", device->idVendor);
    trace("product: %04x\n", device->idProduct);
    if (device->idVendor == USB_ID_VENDOR_FTDI
        && (device->idProduct == 0x6001 || device->idProduct == 0x6010 || device->idProduct == 0x6011
            || device->idProduct == 0x6014 || device->idProduct == 0x6015))
    {
      trace("found ftdi\n");
      return SCE_USBD_PROBE_SUCCEEDED;
    }
  }
  return SCE_USBD_PROBE_FAILED;
}

int libvf_attach(int device_id)
{
  trace("attaching device: %x\n", device_id);
  SceUsbdDeviceDescriptor *device;
  device = (SceUsbdDeviceDescriptor *)ksceUsbdScanStaticDescriptor(device_id, 0, SCE_USBD_DESCRIPTOR_DEVICE);
  if (device && device->idVendor == USB_ID_VENDOR_FTDI
      && (device->idProduct == 0x6001 || device->idProduct == 0x6010 || device->idProduct == 0x6011
          || device->idProduct == 0x6014 || device->idProduct == 0x6015))
  {
    SceUsbdConfigurationDescriptor *cdesc;
    if ((cdesc = (SceUsbdConfigurationDescriptor *)ksceUsbdScanStaticDescriptor(device_id, NULL,
                                                                                SCE_USBD_DESCRIPTOR_CONFIGURATION))
        == NULL)
      return SCE_USBD_ATTACH_FAILED;

    if (cdesc->bNumInterfaces != 1)
      return SCE_USBD_ATTACH_FAILED;

    if (device->bcdDevice == 0x400 || (device->bcdDevice == 0x200 && device->iSerialNumber == 0))
      ctx.type = TYPE_BM;
    else if (device->bcdDevice == 0x200)
      ctx.type = TYPE_AM;
    else if (device->bcdDevice == 0x500)
      ctx.type = TYPE_2232C;
    else if (device->bcdDevice == 0x600)
      ctx.type = TYPE_R;
    else if (device->bcdDevice == 0x700)
      ctx.type = TYPE_2232H;
    else if (device->bcdDevice == 0x800)
      ctx.type = TYPE_4232H;
    else if (device->bcdDevice == 0x900)
      ctx.type = TYPE_232H;
    else if (device->bcdDevice == 0x1000)
      ctx.type = TYPE_230X;

    trace("type = %d\n", ctx.type);

    // Determine maximum packet size
    ctx.max_packet_size = _ftdi_determine_max_packet_size();

    trace("max_packet_size = %d\n", ctx.max_packet_size);

    SceUsbdEndpointDescriptor *endpoint;
    trace("scanning endpoints\n");
    endpoint
        = (SceUsbdEndpointDescriptor *)ksceUsbdScanStaticDescriptor(device_id, device, SCE_USBD_DESCRIPTOR_ENDPOINT);
    while (endpoint)
    {
      trace("got EP: %02x\n", endpoint->bEndpointAddress);
      if (endpoint->bEndpointAddress == USB_ENDPOINT_IN)
      {
        trace("opening in pipe\n");
        ctx.in_pipe_id = ksceUsbdOpenPipe(device_id, endpoint);
        trace("= 0x%08x\n", ctx.in_pipe_id);
      }
      else if (endpoint->bEndpointAddress == USB_ENDPOINT_OUT)
      {
        trace("opening out pipe\n");
        ctx.out_pipe_id = ksceUsbdOpenPipe(device_id, endpoint);
        trace("= 0x%08x\n", ctx.out_pipe_id);
      }
      endpoint = (SceUsbdEndpointDescriptor *)ksceUsbdScanStaticDescriptor(device_id, endpoint,
                                                                           SCE_USBD_DESCRIPTOR_ENDPOINT);
    }

    ctx.control_pipe_id = ksceUsbdOpenPipe(device_id, NULL);
    // set default config
    int r = ksceUsbdSetConfiguration(ctx.control_pipe_id, cdesc->bConfigurationValue, _callback_control, NULL);
#ifdef NDEBUG
    (void)r;
#endif
    trace("ksceUsbdSetConfiguration = 0x%08x\n", r);
    trace("waiting ef (cfg)\n");
    ksceKernelWaitEventFlag(transfer_ev, EVF_CTRL, SCE_EVENT_WAITCLEAR_PAT | SCE_EVENT_WAITAND, NULL, 0);

    _ftdi_reset();


    if (_set_baudrate(9600) != 0)
    {
      trace("can't set baudrate\n");
      return SCE_USBD_ATTACH_FAILED;
    }

    if (ctx.out_pipe_id > 0 && ctx.in_pipe_id > 0 && ctx.control_pipe_id)
    {
      plugged = 1;
      return SCE_USBD_ATTACH_SUCCEEDED;
    }
  }
  return SCE_USBD_ATTACH_FAILED;
}

int libvf_detach(int device_id)
{
  ctx.in_pipe_id  = 0;
  ctx.out_pipe_id = 0;
  plugged         = 0;
  return -1;
}

/*
 *  PUBLIC
 */

int libvf_start()
{
  uint32_t state;
  ENTER_SYSCALL(state);

  trace("starting libvf\n");
  if (started)
  {
    _error_return(-1, "Already started");
  }

  // reset ctx
  _init_ctx();

  started = 1;
  int ret = ksceUsbServMacSelect(2, 0);
#ifdef NDEBUG
  (void)ret;
#endif
  trace("MAC select = 0x%08x\n", ret);
  ret = ksceUsbdRegisterDriver(&libvfDriver);
  trace("ksceUsbdRegisterDriver = 0x%08x\n", ret);
  EXIT_SYSCALL(state);
  return 1;
}

int libvf_stop()
{
  uint32_t state;
  ENTER_SYSCALL(state);
  if (!started)
  {
    _error_return(-1, "Not started");
  }

  started = 0;
  plugged = 0;
  if (ctx.in_pipe_id)
    ksceUsbdClosePipe(ctx.in_pipe_id);
  if (ctx.out_pipe_id)
    ksceUsbdClosePipe(ctx.out_pipe_id);
  if (ctx.control_pipe_id)
    ksceUsbdClosePipe(ctx.control_pipe_id);
  ksceUsbdUnregisterDriver(&libvfDriver);
  ksceUsbServMacSelect(2, 1);

  EXIT_SYSCALL(state);

  return 1;
  // TODO: restore udcd?
}

int libvf_has_ftdi()
{
  return (started && plugged);
}

int libvf_set_baudrate(int baudrate)
{
  int ret;
  uint32_t state;
  ENTER_SYSCALL(state);

  if (!started || !plugged)
    _error_return(-2, "USB device unavailable");

  ret = _set_baudrate(baudrate);
  if (ret < 0)
  {
    EXIT_SYSCALL(state);
    return ret;
  }

  EXIT_SYSCALL(state);
  return 0;
}

int libvf_set_line_property(enum bits_type bits, enum stopbits_type sbit, enum parity_type parity,
                            enum break_type break_type)
{
  uint32_t state;
  ENTER_SYSCALL(state);

  if (!started || !plugged)
    _error_return(-2, "USB device unavailable");

  trace("libvf_set_line_property(%d,%d,%d,%d)\n", bits, sbit, parity, break_type);

  unsigned short value = bits;

  switch (parity)
  {
    case PARITY_NONE:
      value |= (0x00 << 8);
      break;
    case PARITY_ODD:
      value |= (0x01 << 8);
      break;
    case PARITY_EVEN:
      value |= (0x02 << 8);
      break;
    case PARITY_MARK:
      value |= (0x03 << 8);
      break;
    case PARITY_SPACE:
      value |= (0x04 << 8);
      break;
  }

  switch (sbit)
  {
    case STOP_BIT_1:
      value |= (0x00 << 11);
      break;
    case STOP_BIT_15:
      value |= (0x01 << 11);
      break;
    case STOP_BIT_2:
      value |= (0x02 << 11);
      break;
  }

  switch (break_type)
  {
    case BREAK_OFF:
      value |= (0x00 << 14);
      break;
    case BREAK_ON:
      value |= (0x01 << 14);
      break;
  }

  if (_control_transfer(FTDI_DEVICE_OUT_REQTYPE, SIO_SET_DATA_REQUEST, value, 0, NULL, 0) < 0)
  {
    EXIT_SYSCALL(state);
    return -1;
  }
  EXIT_SYSCALL(state);

  return 0;
}

int libvf_write_data(const unsigned char *buf, int size)
{
  int offset = 0;
  int actual_length;
  uint32_t state;
  ENTER_SYSCALL(state);

  if (!started || !plugged)
    _error_return(-2, "USB device unavailable");

  trace("size: %d\n", size);

  while (offset < size)
  {
    int write_size = ctx.writebuffer_chunksize;

    if (offset + write_size > size)
      write_size = size - offset;

    trace("write size: %d\n", write_size);

    ksceKernelMemcpyUserToKernel(ctx.writebuffer, buf + offset, write_size);

    actual_length = _send((unsigned char *)ctx.writebuffer, write_size);
    if (actual_length < 0)
      return -1;

    offset += actual_length;
  }

  EXIT_SYSCALL(state);

  return offset;
}

int libvf_read_data(unsigned char *buf, int size)
{
  int offset = 0, ret, i;
  unsigned int num_of_chunks, chunk_remains;
  unsigned int packet_size;
  int actual_length = 1;

  uint32_t state;
  ENTER_SYSCALL(state);

  if (!started || !plugged)
    _error_return(-3, "USB device unavailable");

  // Packet size sanity check (avoid division by zero)
  packet_size = ctx.max_packet_size;
  if (packet_size == 0)
  {
    _error_return(-1, "bad packet size");
    return -1;
  }

  // everything we want is still in the readbuffer?
  if (size <= (int)ctx.readbuffer_remaining)
  {
    ksceKernelMemcpyKernelToUser(buf, ctx.readbuffer + ctx.readbuffer_offset, size);

    // Fix offsets
    ctx.readbuffer_remaining -= size;
    ctx.readbuffer_offset += size;

    EXIT_SYSCALL(state);

    return size;
  }
  // something still in the readbuffer, but not enough to satisfy 'size'?
  if (ctx.readbuffer_remaining != 0)
  {
    ksceKernelMemcpyKernelToUser(buf, ctx.readbuffer + ctx.readbuffer_offset, ctx.readbuffer_remaining);

    // Fix offset
    offset += ctx.readbuffer_remaining;
  }
  // do the actual USB read
  while (offset < size && actual_length > 0)
  {
    ctx.readbuffer_remaining = 0;
    ctx.readbuffer_offset    = 0;

    /* returns how much received */
    ret = _recv(ctx.readbuffer, ctx.readbuffer_chunksize);

    if (ret < 0)
    {
      trace("recv failed: 0x%08x\n", ret);

      EXIT_SYSCALL(state);
      return ret;
    }

    actual_length = ret;

    if (actual_length > 2)
    {
      // skip FTDI status bytes.
      // Maybe stored in the future to enable modem use
      num_of_chunks = actual_length / packet_size;
      chunk_remains = (unsigned int)actual_length % packet_size;

      ctx.readbuffer_offset += 2;
      actual_length -= 2;

      if (actual_length > packet_size - 2)
      {
        for (i = 1; i < num_of_chunks; i++)
          memmove(ctx.readbuffer + ctx.readbuffer_offset + (packet_size - 2) * i,
                  ctx.readbuffer + ctx.readbuffer_offset + packet_size * i, packet_size - 2);
        if (chunk_remains > 2)
        {
          memmove(ctx.readbuffer + ctx.readbuffer_offset + (packet_size - 2) * i,
                  ctx.readbuffer + ctx.readbuffer_offset + packet_size * i, chunk_remains - 2);
          actual_length -= 2 * num_of_chunks;
        }
        else
          actual_length -= 2 * (num_of_chunks - 1) + chunk_remains;
      }
    }
    else if (actual_length <= 2)
    {
      // no more data to read?
      EXIT_SYSCALL(state);
      return offset;
    }

    if (actual_length > 0)
    {
      // data still fits in buf?
      if (offset + actual_length <= size)
      {
        ksceKernelMemcpyKernelToUser(buf + offset, ctx.readbuffer + ctx.readbuffer_offset, actual_length);
        // printf("buf[0] = %X, buf[1] = %X\n", buf[0], buf[1]);
        offset += actual_length;

        /* Did we read exactly the right amount of bytes? */
        if (offset == size)
        {
          EXIT_SYSCALL(state);
          return offset;
        }
      }
      else
      {
        // only copy part of the data or size <= readbuffer_chunksize
        int part_size = size - offset;
        ksceKernelMemcpyKernelToUser(buf + offset, ctx.readbuffer + ctx.readbuffer_offset, part_size);

        ctx.readbuffer_offset += part_size;
        ctx.readbuffer_remaining = actual_length - part_size;
        offset += part_size;

        EXIT_SYSCALL(state);
        return offset;
      }
    }
  }
  // never reached

  EXIT_SYSCALL(state);
  return -127;
}

int _tciflush()
{
  if (_control_transfer(FTDI_DEVICE_OUT_REQTYPE, SIO_RESET_REQUEST, SIO_TCIFLUSH, 0, NULL, 0) < 0)
    return -1;

  // Invalidate data in the readbuffer
  ctx.readbuffer_offset    = 0;
  ctx.readbuffer_remaining = 0;

  return 0;
}
int libvf_tciflush()
{
  uint32_t state;
  ENTER_SYSCALL(state);

  if (!started || !plugged)
    _error_return(-2, "USB device unavailable");

  if (_tciflush() < 0)
    _error_return(-1, "FTDI purge of RX buffer failed");

  // Invalidate data in the readbuffer
  ctx.readbuffer_offset    = 0;
  ctx.readbuffer_remaining = 0;

  EXIT_SYSCALL(state);
  return 0;
}

int _tcoflush()
{
  if (_control_transfer(FTDI_DEVICE_OUT_REQTYPE, SIO_RESET_REQUEST, SIO_TCOFLUSH, 0, NULL, 0) < 0)
    return -1;

  return 0;
}

int libvf_tcoflush()
{
  uint32_t state;
  ENTER_SYSCALL(state);

  if (!started || !plugged)
    _error_return(-2, "USB device unavailable");

  if (_tcoflush() < 0)
    _error_return(-1, "FTDI purge of TX buffer failed");

  EXIT_SYSCALL(state);
  return 0;
}

int libvf_tcioflush()
{
  int result;
  uint32_t state;
  ENTER_SYSCALL(state);

  if (!started || !plugged)
    _error_return(-3, "USB device unavailable");

  result = _tcoflush();
  if (result < 0)
    _error_return(-1, "FTDI purge of TX buffer failed");

  result = _tciflush();
  if (result < 0)
    _error_return(-2, "FTDI purge of RX buffer failed");

  EXIT_SYSCALL(state);
  return 0;
}

int libvf_setflowctrl(int flowctrl)
{
  uint32_t state;
  ENTER_SYSCALL(state);
  if (!started || !plugged)
    _error_return(-2, "USB device unavailable");

  if (_control_transfer(FTDI_DEVICE_OUT_REQTYPE, SIO_SET_FLOW_CTRL_REQUEST, 0, (flowctrl | 0), NULL, 0) < 0)
    _error_return(-1, "set flow control failed");

  EXIT_SYSCALL(state);
  return 0;
}

int libvf_setflowctrl_xonxoff(unsigned char xon, unsigned char xoff)
{
  uint32_t state;
  ENTER_SYSCALL(state);

  if (!started || !plugged)
    _error_return(-2, "USB device unavailable");

  uint16_t xonxoff = xon | (xoff << 8);
  if (_control_transfer(FTDI_DEVICE_OUT_REQTYPE, SIO_SET_FLOW_CTRL_REQUEST, xonxoff, (SIO_XON_XOFF_HS | 0), NULL, 0)
      < 0)
    _error_return(-1, "set flow control failed");

  EXIT_SYSCALL(state);
  return 0;
}

int libvf_setdtr_rts(int dtr, int rts)
{
  unsigned short usb_val;

  uint32_t state;
  ENTER_SYSCALL(state);

  if (!started || !plugged)
    _error_return(-2, "USB device unavailable");

  if (dtr)
    usb_val = SIO_SET_DTR_HIGH;
  else
    usb_val = SIO_SET_DTR_LOW;

  if (rts)
    usb_val |= SIO_SET_RTS_HIGH;
  else
    usb_val |= SIO_SET_RTS_LOW;

  if (_control_transfer(FTDI_DEVICE_OUT_REQTYPE, SIO_SET_MODEM_CTRL_REQUEST, usb_val, 0, NULL, 0) < 0)
    _error_return(-1, "set of rts/dtr failed");

  EXIT_SYSCALL(state);
  return 0;
}

int libvf_setdtr(int dtrstate)
{
  unsigned short usb_val;
  uint32_t state;
  ENTER_SYSCALL(state);

  if (!started || !plugged)
    _error_return(-2, "USB device unavailable");

  if (dtrstate)
    usb_val = SIO_SET_DTR_HIGH;
  else
    usb_val = SIO_SET_DTR_LOW;

  if (_control_transfer(FTDI_DEVICE_OUT_REQTYPE, SIO_SET_MODEM_CTRL_REQUEST, usb_val, 0, NULL, 0) < 0)
    _error_return(-1, "set dtr failed");

  EXIT_SYSCALL(state);
  return 0;
}

int libvf_setrts(int rtsstate)
{
  unsigned short usb_val;

  uint32_t state;
  ENTER_SYSCALL(state);

  if (!started || !plugged)
    _error_return(-2, "USB device unavailable");

  if (rtsstate)
    usb_val = SIO_SET_RTS_HIGH;
  else
    usb_val = SIO_SET_RTS_LOW;

  if (_control_transfer(FTDI_DEVICE_OUT_REQTYPE, SIO_SET_MODEM_CTRL_REQUEST, usb_val, 0, NULL, 0) < 0)
    _error_return(-1, "set of rts failed");

  EXIT_SYSCALL(state);
  return 0;
}

void _start() __attribute__((weak, alias("module_start")));

int module_start(SceSize args, void *argp)
{
  trace("liblibvf starting\n");
  ksceKernelRegisterSysEventHandler("zlibvf_sysevent", libvf_sysevent_handler, NULL);
  transfer_ev = ksceKernelCreateEventFlag("libvf_transfer", 0, 0, NULL);
  trace("ef: 0x%08x\n", transfer_ev);
  return SCE_KERNEL_START_SUCCESS;
}

int module_stop(SceSize args, void *argp)
{
  libvf_stop();
  return SCE_KERNEL_STOP_SUCCESS;
}
