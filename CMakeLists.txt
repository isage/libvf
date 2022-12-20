#        libvf
#        Copyright (C) 2022 Cat (Ivan Epifanov)
#
#        This program is free software: you can redistribute it and/or modify
#        it under the terms of the GNU General Public License as published by
#        the Free Software Foundation, either version 3 of the License, or
#        (at your option) any later version.
#
#        This program is distributed in the hope that it will be useful,
#        but WITHOUT ANY WARRANTY; without even the implied warranty of
#        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#        GNU General Public License for more details.
#
#        You should have received a copy of the GNU General Public License
#        along with this program.  If not, see <https://www.gnu.org/licenses/>.

cmake_minimum_required(VERSION 3.2)

if(NOT DEFINED CMAKE_TOOLCHAIN_FILE)
  if(DEFINED ENV{VITASDK})
    set(CMAKE_TOOLCHAIN_FILE "$ENV{VITASDK}/share/vita.toolchain.cmake" CACHE PATH "toolchain file")
  else()
    message(FATAL_ERROR "Please define VITASDK to point to your SDK path!")
  endif()
endif()

project(libvf)
include("${VITASDK}/share/vita.cmake" REQUIRED)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wl,-q -Wall -O3 -nostdlib")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti -fno-exceptions")

add_executable(libvf
  main.c
)

target_link_libraries(libvf
  SceSysclibForDriver_stub
  SceSysmemForDriver_stub
  SceSysmemForKernel_stub
  SceThreadmgrForDriver_stub
  SceDebugForDriver_stub
  SceUsbdForDriver_stub
  SceUsbServForDriver_stub
  SceKernelSuspendForDriver_stub
)

vita_create_self(libvf.skprx libvf CONFIG exports.yml UNSAFE)

vita_create_stubs(stubs libvf ${CMAKE_CURRENT_SOURCE_DIR}/exports.yml KERNEL)

add_custom_target(libvf_all
  ALL
  DEPENDS liblibvf_stub.a
  DEPENDS liblibvf_stub_weak.a
)

install(DIRECTORY ${CMAKE_BINARY_DIR}/stubs/
  DESTINATION lib
  FILES_MATCHING PATTERN "*.a"
)

install(FILES libvf.h
  DESTINATION include
)
