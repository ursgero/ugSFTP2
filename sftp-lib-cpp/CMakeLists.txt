cmake_minimum_required(VERSION 3.10)
project(UgSFTPClientLibrary)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

find_package(LibSSH REQUIRED)

add_library(ugsftpclient SHARED UgSFTPClient.cpp)
target_include_directories(ugsftpclient PRIVATE ${LIBSSH_INCLUDE_DIRS})
target_link_libraries(ugsftpclient ${LIBSSH_LIBRARIES})

