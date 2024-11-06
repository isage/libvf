# libvf - PSVita FTDI driver.

PSVita kernel module and sample for communicating via serial protocol through FTDI adapters

## Building

* Install [vitasdk](https://vitasdk.org)
* `mkdir build && cmake -DCMAKE_BUILD_TYPE=Release .. && make install`
* Add libvf.skprk under `*KERNEL` in tai config

## Devices supported

* FT230X
* FT4232H / FT2232H
* FT232R  / FT245R
* FT2232L / FT2232D / FT2232C
* FT232BM / FT245BM (and the BL/BQ variants)
* FT8U232AM / FT8U245AM

## Notes

* Only first interface on dual/quad-interface chips are supported
* Only FT232R tested
* Bitbang and eeprom reading/writing isn't implemented

## Documntation
 See libvf.h, api is pretty self-explanatory.

## License

GPLv3, see LICENSE.md

## Credits

* [libFTDI](https://www.intra2net.com/en/developer/libftdi/)