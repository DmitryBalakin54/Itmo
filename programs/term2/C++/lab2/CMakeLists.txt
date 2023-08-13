cmake_minimum_required(VERSION 3.24)
project(Lab2 C)
set(CMAKE_C_STANDARD 11)

#include_directories(.)
#find_package(ZLIB )
#ADD_DEFINITIONS(-D ISAL )

find_package(ZLIB REQUIRED)
include_directories(${ZLIB_INCLUDE_DIRS})
link_libraries(${ZLIB_LIBRARIES})

add_executable(Lab2 main.c
        return_codes.h)

#target_link_libraries( Lab2  ZLIB::ZLIB )
#target_link_libraries( Lab2  ${CMAKE_SOURCE_DIR}/libdeflate.a )
#target_link_libraries( Lab2  ${CMAKE_SOURCE_DIR}/libisal.so )