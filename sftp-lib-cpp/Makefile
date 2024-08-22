CC=g++-9
CFLAGS=-c -Wall -fPIC
LDFLAGS=-lssh
SOURCES=UgSFTPClient.cpp UgSFTPClientWrapper.cpp
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=libugsftpclient.so

all:	$(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(LDFLAGS) -shared -o $@ $(OBJECTS)

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)
